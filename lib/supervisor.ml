(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Not mutated: this module judges mutants (see lib/dune). *)
[@@@mutate exclude_file]

module Coverage = Windtrap_runtime.Coverage
module Mutate = Windtrap_runtime.Mutate

let strf = Printf.sprintf
let ( let* ) = Result.bind

(* Frames *)

(* What a worker sends: a report of [Run.serve], the mutants each test
   evaluated, and as it ends the coverage counts it hands over and the
   evaluations of its armed site. *)
type to_parent =
  | Report of Run.report
  | Reached of { test : string list option; sites : (Mutate.id * int) list }
  | Counts of Coverage.t
  | Hits of int

(* A frame is a 4-byte length and a [Marshal]led value. Nothing that crosses
   holds a closure, which [Marshal] would refuse at once. *)
exception Closed

let rec write_all fd buf off len =
  if len > 0 then
    match Unix.write fd buf off len with
    | n -> write_all fd buf (off + n) (len - n)
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> write_all fd buf off len
    | exception Unix.Unix_error ((Unix.EPIPE | Unix.EBADF), _, _) ->
        raise Closed

let frame value =
  let payload = Marshal.to_bytes value [] in
  let n = Bytes.length payload in
  let frame = Bytes.create (4 + n) in
  Bytes.set_int32_be frame 0 (Int32.of_int n);
  Bytes.blit payload 0 frame 4 n;
  frame

let rec read_exact ~interrupted fd buf off len =
  if len > 0 then
    match Unix.read fd buf off len with
    | 0 -> raise Closed
    | n -> read_exact ~interrupted fd buf (off + n) (len - n)
    | exception Unix.Unix_error (Unix.EINTR, _, _) ->
        interrupted ();
        read_exact ~interrupted fd buf off len
    | exception Unix.Unix_error ((Unix.ECONNRESET | Unix.EBADF), _, _) ->
        raise Closed

let receive ?(interrupted = ignore) fd =
  let header = Bytes.create 4 in
  read_exact ~interrupted fd header 0 4;
  let n = Int32.to_int (Bytes.get_int32_be header 0) in
  let payload = Bytes.create n in
  read_exact ~interrupted fd payload 0 n;
  Marshal.from_bytes payload 0

(* Workers *)

let mutating (config : Run.config) =
  match config.mutation with
  | Run.No_mutation -> false
  | Run.Loop _ | Run.Armed _ -> true

(* Under mutation a test's window opens at its start and closes at its end,
   so what module initialization and a fixture release evaluate falls in a
   window of no test. *)
let window ~send test =
  (match Mutate.drain () with
  | [] -> ()
  | reached ->
      send test
        (List.map (fun (r : Mutate.reached) -> (r.mutant.id, r.hits)) reached));
  Mutate.next_epoch ()

let reach_windows ~send (event : Run.event) =
  match event with
  | Test_started _ -> window ~send None
  | Test_finished result -> window ~send (Some result.path)
  | Run_started _ | Shrinking _ | Shrunk _ | Fixture_release _ | Interrupted _
    ->
      ()

(* The worker is a session of its own, so a terminal's signal reaches the
   parent alone, which forwards it, and the parent kills the group. *)
let serve (config : Run.config) ~suite tests ~temp_dir ~input ~output =
  ignore (Unix.setsid ());
  (* What the parent counted before the fork is the parent's to dump. *)
  ignore (Coverage.take ());
  (* The parent validated an [--arm] identifier, which a re-executed worker
     arms anew. Evaluations count from here. *)
  (match config.mutation with
  | Run.Armed spec ->
      ignore (Result.bind (Mutate.id_of_string spec) (fun id -> Mutate.arm id))
  | Run.No_mutation | Run.Loop _ -> ());
  if mutating config then Mutate.reset_reach ();
  (* A domain the test left running can call exit while the test's domain
     sends, and two frames must not interleave. *)
  let lock = Mutex.create () in
  let send value =
    let frame = frame value in
    Mutex.protect lock (fun () -> write_all output frame 0 (Bytes.length frame))
  in
  let reached test sites = send (Reached { test; sites }) in
  let last () =
    (if mutating config then
       try
         window ~send:reached None;
         send (Hits (Mutate.armed_hits ()))
       with _ -> ());
    let counts = Coverage.take () in
    if Coverage.files counts <> [] then try send (Counts counts) with _ -> ()
  in
  let report = function
    | Run.Event event as report ->
        if mutating config then reach_windows ~send:reached event;
        send (Report report)
    | report -> send (Report report)
  in
  Run.serve config ~suite tests ~temp_dir ~send:report
    ~receive:(fun ~interrupted -> (receive ~interrupted input : Run.request))
    ~last

type worker = {
  pid : int;
  requests : Unix.file_descr;
  reports : Unix.file_descr;
  temp_dir : string;
  mutable job : case_job option;
  mutable releasing : string option; (* the fixture it announced last *)
  mutable exited : bool; (* it said that code under test called exit *)
  mutable signalled : bool; (* the parent sent it the run's signal *)
  mutable status : Unix.process_status option; (* once it is reaped *)
}

and case_job = {
  index : int;
  case : Test_tree.case;
  started : Os.counter;
  limit : float option;
  mutable deadline : float option; (* seconds after [started] *)
  mutable asked : (Run.attempt * Os.counter) option; (* a verdict it awaits *)
  mutable rev_events : Run.event list; (* held until the case is reached *)
  mutable shown : bool; (* its [Test_started] reached the observer *)
}

(* A worker's temporary root: the parent removes it whatever the worker
   left there. *)
let next_temp_dir = ref 0

let make_temp_dir () =
  let base = Filename.get_temp_dir_name () in
  let rec make attempts =
    incr next_temp_dir;
    let dir =
      Filename.concat base
        (strf "windtrap-%d-w%d" (Unix.getpid ()) !next_temp_dir)
    in
    match Unix.mkdir dir 0o700 with
    | () -> dir
    | exception Unix.Unix_error (Unix.EEXIST, _, _) when attempts < 64 ->
        make (attempts + 1)
  in
  make 1

let pipes () =
  let requests_r, requests_w = Unix.pipe ~cloexec:true () in
  let reports_r, reports_w = Unix.pipe ~cloexec:true () in
  ((requests_r, reports_w), (requests_w, reports_r))

let worker pid ~temp_dir (requests, reports) =
  {
    pid;
    requests;
    reports;
    temp_dir;
    job = None;
    releasing = None;
    exited = false;
    signalled = false;
    status = None;
  }

(* A fork of the parent before it ran any test, so the worker starts warm and
   clean. The buffers are drained first, or the worker would write them a
   second time. *)
let fork ?(prepare = fun ~temp_dir:_ config -> config) config ~suite tests =
  let temp_dir = make_temp_dir () in
  let (input, output), ends = pipes () in
  Capture.drain ();
  match Unix.fork () with
  | 0 -> (
      Unix.close (fst ends);
      Unix.close (snd ends);
      try
        let config = prepare ~temp_dir config in
        serve config ~suite tests ~temp_dir ~input ~output
      with _ -> Unix._exit 2)
  | pid ->
      Unix.close input;
      Unix.close output;
      worker pid ~temp_dir ends

(* A process that spawned a domain cannot fork, so it executes itself again,
   and the [run] call of the same rank serves (see [enter]). *)
let worker_variable = "WINDTRAP_WORKER"
let rank = ref 0

(* On POSIX systems a descriptor is its number. *)
let fd_to_int : Unix.file_descr -> int = Obj.magic
let fd_of_int : int -> Unix.file_descr = Obj.magic

let exec config =
  let temp_dir = make_temp_dir () in
  let (input, output), ends = pipes () in
  Unix.clear_close_on_exec input;
  Unix.clear_close_on_exec output;
  let spec =
    strf "%s=%d:%d:%d:%s" worker_variable !rank (fd_to_int input)
      (fd_to_int output) temp_dir
  in
  let env = Array.append [| spec |] (Unix.environment ()) in
  Capture.drain ();
  let pid =
    Unix.create_process_env Sys.executable_name Sys.argv env Unix.stdin
      Unix.stdout Unix.stderr
  in
  Unix.close input;
  Unix.close output;
  let w = worker pid ~temp_dir ends in
  let frame = frame config in
  write_all w.requests frame 0 (Bytes.length frame);
  w

(* Read as the library initialises, before a test can change the
   environment, which the worker's tests then do not inherit. *)
let re_executed =
  match Sys.getenv_opt worker_variable with
  | None -> None
  | Some spec -> (
      Os.setenv worker_variable None;
      match String.split_on_char ':' spec with
      | rank :: input :: output :: dir -> (
          match List.map int_of_string_opt [ rank; input; output ] with
          | [ Some rank; Some input; Some output ] ->
              Some
                (rank, fd_of_int input, fd_of_int output, String.concat ":" dir)
          | _ -> None)
      | _ -> None)

(* A re-executed worker skips the calls before its own, which its parent
   made, and its own never returns. *)
let enter ~suite tests =
  incr rank;
  match re_executed with
  | None -> `Parent
  | Some (target, _, _, _) when !rank < target -> `Skip
  | Some (_, input, output, temp_dir) -> (
      match (receive input : Run.config) with
      | config -> serve config ~suite tests ~temp_dir ~input ~output
      | exception Closed -> Unix._exit 2)

let domain_spawned () =
  match Domain.before_first_spawn ignore with
  | () -> false
  | exception Invalid_argument _ -> true

(* The parent *)

let interrupting = [ Sys.sigint; Sys.sigterm; Sys.sighup ]

(* A worker that died leaves its read end closed, and the write must fail
   rather than kill the parent. *)
let request w (request : Run.request) =
  let frame = frame request in
  let previous = Sys.signal Sys.sigpipe Sys.Signal_ignore in
  Fun.protect
    ~finally:(fun () -> Sys.set_signal Sys.sigpipe previous)
    (fun () ->
      try write_all w.requests frame 0 (Bytes.length frame) with Closed -> ())

(* The worker is killed with its process group: it may not have made the
   group yet. *)
let kill w =
  (try Unix.kill (-w.pid) Sys.sigkill with Unix.Unix_error _ -> ());
  try Unix.kill w.pid Sys.sigkill with Unix.Unix_error _ -> ()

(* Once the worker is reaped, its process group goes: what the test started
   there, and what it left in the temporary root. *)
let bury w =
  (try Unix.kill (-w.pid) Sys.sigkill with Unix.Unix_error _ -> ());
  (try Unix.close w.requests with Unix.Unix_error _ -> ());
  (try Unix.close w.reports with Unix.Unix_error _ -> ());
  Run.remove_tree w.temp_dir

let rec reap ?(flags = []) w =
  match w.status with
  | Some _ as status -> status
  | None -> (
      match Unix.waitpid flags w.pid with
      | 0, _ -> None
      | _, status ->
          w.status <- Some status;
          w.status
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> reap ~flags w
      | exception Unix.Unix_error (Unix.ECHILD, _, _) ->
          w.status <- Some (Unix.WEXITED 255);
          w.status)

let signal_name signal =
  List.assoc_opt signal
    [
      (Sys.sigsegv, "SIGSEGV");
      (Sys.sigbus, "SIGBUS");
      (Sys.sigfpe, "SIGFPE");
      (Sys.sigill, "SIGILL");
      (Sys.sigabrt, "SIGABRT");
      (Sys.sigkill, "SIGKILL");
      (Sys.sigterm, "SIGTERM");
      (Sys.sigint, "SIGINT");
      (Sys.sighup, "SIGHUP");
      (Sys.sigpipe, "SIGPIPE");
    ]
  |> Option.value ~default:(strf "signal %d" signal)

let ended = function
  | Unix.WEXITED n -> strf "exited with code %d" n
  | Unix.WSIGNALED s -> "was killed by " ^ signal_name s
  | Unix.WSTOPPED s -> "was stopped by " ^ signal_name s

(* The soft limit bounds setup and body, then the teardown with what is left
   or a fresh window. The kill comes one window after that, for each
   attempt. *)
let deadline (case : Test_tree.case) limit =
  Option.map (fun l -> float_of_int (case.retries + 1) *. 3. *. l) limit

type settled =
  | Served of { served : Run.served; stopped : bool }
  | Lost of Run.result

let supervise ?(on_event = ignore) ~on_reach (config : Run.config) ~suite tests
    ~spawn =
  let started = Os.counter () in
  let* selection = Run.select config ~suite tests in
  let run = Run.supervising config ~suite in
  Run.in_run run @@ fun () ->
  let workers = ref [] and signal = ref None in
  (* The signal reaches every worker at once, whatever the parent does. *)
  let forward () =
    match !signal with
    | None -> ()
    | Some s ->
        List.iter
          (fun w ->
            if not w.signalled then begin
              w.signalled <- true;
              try Unix.kill w.pid s with Unix.Unix_error _ -> ()
            end)
          !workers
  in
  let interrupt s =
    if Option.is_none !signal then begin
      signal := Some s;
      forward ()
    end
  in
  Os.with_signals interrupting interrupt @@ fun () ->
  on_event (Run.run_started ~suite selection);
  let cases = Array.of_list selection.cases in
  let n = Array.length cases in
  (* Under --stream the tests write to the real descriptors, one at a time. *)
  let jobs = if config.stream then 1 else config.jobs in
  let settled = Array.make n None and running = Array.make n None in
  let next = ref 0 and reached = ref 0 in
  let stopping = ref false and uncorrected = ref false in
  let rev_release_failures = ref [] in
  let interrupted = ref None in
  let fail_outside failure =
    rev_release_failures := failure :: !rev_release_failures
  in
  let give event =
    on_event event;
    Capture.drain ()
  in
  let drop w = workers := List.filter (fun x -> x != w) !workers in
  let lose (job : case_job) failure =
    settled.(job.index) <-
      Some
        (Lost (Run.lost run job.case failure ~duration:(Os.count_s job.started)))
  in
  (* A worker that ended on its own: its test fails saying how, and what it
     was releasing fails the run. A worker that a signal interrupted ends the
     run as the signal would have ended an in-process one, whoever sent
     it. *)
  let died w status =
    drop w;
    bury w;
    match (w.job, w.releasing, status) with
    | _, _, Unix.WSIGNALED s
      when Option.is_some !interrupted && List.mem s interrupting ->
        interrupt s
    | Some job, _, _ ->
        w.job <- None;
        let loc = job.case.loc in
        lose job
          (if w.exited then
             Failure.message ?loc
               ("the test called exit, which ended its worker; " ^ Run.exit_text)
           else
             Failure.message ?loc
               (strf "the test ended its worker process, which %s"
                  (ended status)))
    | None, Some name, _ when status <> Unix.WEXITED 0 ->
        fail_outside
          (Failure.with_phase Failure.Release
             (Failure.message
                (strf "%s: its worker process %s while releasing it" name
                   (ended status))))
    | None, _, _ -> ()
  in
  let expire w (job : case_job) limit =
    drop w;
    kill w;
    ignore (reap w);
    bury w;
    w.job <- None;
    lose job (Failure.timeout ?loc:job.case.loc limit)
  in
  (* An event of the case that the observer is at is given at once, and one
     of a later case when that case is reached. The parent gives each row's
     own [Test_finished], and the worker waits for none after
     [Interrupted]. *)
  let observe w (event : Run.event) =
    (match (event, w.job) with
    | Interrupted { running; releasing; _ }, _ ->
        interrupted := Some (running, releasing)
    | Fixture_release { name }, _ ->
        w.releasing <- Some name;
        give event
    | (Test_started _ | Shrinking _ | Shrunk _), Some job
      when !reached = job.index ->
        job.shown <- true;
        give event
    | (Test_started _ | Shrinking _ | Shrunk _), Some job ->
        job.rev_events <- event :: job.rev_events
    | (Test_started _ | Shrinking _ | Shrunk _), None
    | (Run_started _ | Test_finished _), _ ->
        ());
    match event with
    | Interrupted _ -> ()
    | _ -> if config.stream then request w Ack
  in
  (* The reports a worker sends whatever it runs. *)
  let common w : Run.report -> unit = function
    | Event event -> observe w event
    | Released failures ->
        w.releasing <- None;
        rev_release_failures := List.rev_append failures !rev_release_failures
    | Outside failure -> fail_outside failure
    | Exited -> w.exited <- true
    | Judge _ | Done _ -> ()
  in
  let receive_from w : to_parent =
    try receive w.reports with Failure _ | End_of_file -> raise Closed
  in
  (* A worker releases its fixtures, runs what its tests registered with
     [at_exit], and goes. *)
  let rec drain w =
    match receive_from w with
    | Report report ->
        common w report;
        drain w
    | Counts counts ->
        Coverage.absorb counts;
        drain w
    | Reached { test; sites } ->
        on_reach test sites;
        drain w
    | Hits _ -> drain w
    | exception Closed -> ()
  in
  let finish w =
    request w Finish;
    drain w;
    drop w;
    match reap w with
    | Some status when Option.is_none !interrupted -> died w status
    | Some _ | None -> bury w
  in
  let answer (job : case_job) =
    match job.asked with
    | None -> ()
    | Some (attempt, since) -> (
        job.asked <- None;
        (* The wait is the parent's, and does not count toward the kill. *)
        job.deadline <- Option.map (fun d -> d +. Os.count_s since) job.deadline;
        let verdict = Run.verdict run job.case attempt in
        let holds w = match w.job with Some j -> j == job | None -> false in
        match List.find_opt holds !workers with
        | Some w -> request w (Verdict verdict)
        | None -> ())
  in
  (* The rows reach the observer in the order of the selection, and the
     parent makes the checks of each when it is reached, after every earlier
     one: the order in which an in-process run made them. *)
  let rec reach () =
    if (not !stopping) && !reached < n then
      match settled.(!reached) with
      | None -> Option.iter answer running.(!reached)
      | Some settled ->
          let job = Option.get running.(!reached) in
          let result, corrected, stopped =
            match settled with
            | Lost result -> (result, false, false)
            | Served { served; stopped } ->
                let result, corrected = Run.adjudicate run job.case served in
                (result, corrected, stopped)
          in
          if not job.shown then give (Test_started { path = job.case.path });
          List.iter give (List.rev job.rev_events);
          Run.add_result run result;
          give (Test_finished result);
          uncorrected := !uncorrected || (result.counted && not corrected);
          if (config.bail && result.counted) || stopped then stopping := true;
          incr reached;
          reach ()
  in
  let assign () =
    let idle () = List.find_opt (fun w -> Option.is_none w.job) !workers in
    let rec go () =
      if (not !stopping) && !next < n && Option.is_none !signal then
        let w =
          match idle () with
          | Some w -> Some w
          | None when List.length !workers < jobs ->
              let w = spawn () in
              workers := !workers @ [ w ];
              Some w
          | None -> None
        in
        match w with
        | None -> ()
        | Some w ->
            let case = cases.(!next) in
            let limit =
              match case.timeout with None -> config.timeout | some -> some
            in
            let job =
              {
                index = !next;
                case;
                started = Os.counter ();
                limit;
                deadline = deadline case limit;
                asked = None;
                rev_events = [];
                shown = false;
              }
            in
            incr next;
            w.job <- Some job;
            running.(job.index) <- Some job;
            request w (Case (Test_tree.path_to_string case.path));
            go ()
    in
    go ()
  in
  let handle w =
    match (receive_from w, w.job) with
    | exception Closed -> (
        match reap w with Some status -> died w status | None -> ())
    | Counts counts, _ -> Coverage.absorb counts
    | Reached { test; sites }, _ -> on_reach test sites
    | Hits _, _ -> ()
    | Report (Judge attempt), Some job ->
        job.asked <- Some (attempt, Os.counter ())
    | Report (Done { served; stopped; retire }), Some job ->
        w.job <- None;
        settled.(job.index) <- Some (Served { served; stopped });
        (* A domain the test spawned may still run, and would run into the
           next test. *)
        if retire then finish w
    | Report report, _ -> common w report
  in
  (* A worker found dead may have left reports in its pipe, which say more
     than its status: they are read first. *)
  let rec dead w status =
    if List.memq w !workers then
      match Unix.select [ w.reports ] [] [] 0. with
      | _ :: _, _, _ ->
          handle w;
          dead w status
      | [], _, _ -> died w status
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> dead w status
  in
  let busy () = List.filter (fun w -> Option.is_some w.job) !workers in
  let remaining (job : case_job) =
    if Option.is_some job.asked then None
    else Option.map (fun d -> d -. Os.count_s job.started) job.deadline
  in
  (* The rows already settled reach the observer before the next case
     starts, so a signal it raises falls between two tests. A worker whose
     pipe another process holds open sends no end of file, so a wait never
     outlasts a second without asking whether it lives. *)
  let rec loop () =
    reach ();
    assign ();
    (* After a stop no verdict comes: a worker that waits for one releases
       its fixtures and goes, and its case is not reported. *)
    if !stopping then
      List.iter
        (fun w ->
          match w.job with
          | Some job when Option.is_some job.asked ->
              w.job <- None;
              finish w
          | Some _ | None -> ())
        !workers;
    forward ();
    match busy () with
    | [] -> ()
    | _ when Option.is_some !signal -> ()
    | busy ->
        let wait =
          List.fold_left
            (fun acc w ->
              match Option.bind w.job remaining with
              | Some r -> Float.min acc (Float.max 0. r)
              | None -> acc)
            1. busy
        in
        let ready =
          match Unix.select (List.map (fun w -> w.reports) busy) [] [] wait with
          | ready, _, _ -> ready
          | exception Unix.Unix_error (Unix.EINTR, _, _) -> []
        in
        (* Once a signal came, what the workers report is read as they end
           by it. *)
        List.iter
          (fun w ->
            match w.job with
            | _ when Option.is_some !signal -> ()
            | None -> ()
            | Some _ when List.mem w.reports ready -> handle w
            | Some job -> (
                match (remaining job, job.limit) with
                | Some r, Some limit when r <= 0. -> expire w job limit
                | _ when ready = [] -> (
                    match reap ~flags:[ Unix.WNOHANG ] w with
                    | Some status -> dead w status
                    | None -> ())
                | _ -> ()))
          busy;
        loop ()
  in
  let die_by s =
    let running, releasing = Option.value !interrupted ~default:(None, None) in
    (try
       on_event
         (Interrupted
            {
              running;
              releasing;
              results = Run.results run;
              duration = Os.count_s started;
            })
     with _ -> ());
    (try Capture.drain () with _ -> ());
    Os.die_by s
  in
  (* Once a signal came, the workers end by it, and the parent then does. *)
  let end_by_signal () =
    match !signal with
    | None -> ()
    | Some s ->
        forward ();
        List.iter
          (fun w ->
            drain w;
            ignore (reap w);
            bury w)
          !workers;
        die_by s
  in
  match
    loop ();
    reach ();
    end_by_signal ();
    List.iter finish !workers;
    end_by_signal ()
  with
  | () ->
      let release_failures = List.rev !rev_release_failures in
      let outcome =
        Run.conclude run selection ~suite ~started ~uncorrected:!uncorrected
          ~release_failures
      in
      end_by_signal ();
      Ok outcome
  | exception exn ->
      (* An observer that raised: the workers go, and so does the run. *)
      let backtrace = Printexc.get_raw_backtrace () in
      List.iter
        (fun w ->
          kill w;
          ignore (reap w);
          bury w)
        !workers;
      Printexc.raise_with_backtrace exn backtrace

(* The parent's own evaluations, at module initialization, are outside
   every test. *)
let execute ?(on_reach = fun _ _ -> ()) ?(on_event = ignore)
    (config : Run.config) ~suite tests =
  if mutating config then window ~send:on_reach None;
  if config.in_process || Sys.win32 then (
    let on_event event =
      if mutating config then reach_windows ~send:on_reach event;
      on_event event
    in
    let ran = Run.execute ~on_event config ~suite tests in
    if mutating config then window ~send:on_reach None;
    ran)
  else
    let spawn =
      if domain_spawned () then fun () -> exec config
      else fun () -> fork config ~suite tests
    in
    supervise ~on_event ~on_reach config ~suite tests ~spawn

(* Fresh workers *)

let can_fork () = not (Sys.win32 || domain_spawned ())

type job = {
  cases : string list;
  deadline : float;
  bail : bool;
  arm : unit -> unit;
}

type ended = Ran | Lost of string | Expired

type ran = {
  results : Run.result list;
  release_failures : Failure.t list;
  armed_hits : int;
  ended : ended;
}

let counted_failure (r : Run.result) =
  r.counted
  && match r.outcome with Failure.Fail _ -> true | Pass | Skip _ -> false

(* A job's state while its workers run it. A worker that retires leaves the
   rest of the job's cases to a fresh one. *)
type state = {
  index : int;
  job : job;
  started : Os.counter;
  mutable w : worker;
  mutable pending : string list; (* not yet sent *)
  mutable finishing : bool; (* Finish was sent *)
  mutable rev_results : Run.result list;
  mutable rev_failures : Failure.t list;
  mutable hits : int;
}

(* Nothing a mutated program prints reaches the parent's descriptors, and
   its capture logs live and die with the worker. *)
let quiet ~temp_dir (config : Run.config) =
  (match Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 with
  | fd ->
      Unix.dup2 fd Unix.stdout;
      Unix.dup2 fd Unix.stderr;
      Unix.close fd
  | exception Unix.Unix_error _ -> ());
  {
    config with
    stream = false;
    baseline = Baseline.Check;
    junit = None;
    log_dir = Filename.concat temp_dir "logs";
  }

let fresh (config : Run.config) ~suite tests jobs ~on_ran ~on_signal =
  let jobs = Array.of_list jobs in
  let n = Array.length jobs in
  let settled = Array.make n None in
  let running = ref [] and next = ref 0 and delivered = ref 0 in
  let signal = ref None in
  let spawn job =
    let prepare ~temp_dir config =
      let config = quiet ~temp_dir config in
      job.arm ();
      config
    in
    fork ~prepare config ~suite tests
  in
  let send_next st =
    match st.pending with
    | path :: rest ->
        st.pending <- rest;
        request st.w (Case path)
    | [] ->
        st.finishing <- true;
        request st.w Finish
  in
  let settle st ended =
    running := List.filter (fun x -> x != st) !running;
    bury st.w;
    settled.(st.index) <-
      Some
        {
          results = List.rev st.rev_results;
          release_failures = List.rev st.rev_failures;
          armed_hits = st.hits;
          ended;
        }
  in
  let start () =
    while
      !next < n && List.length !running < config.jobs && Option.is_none !signal
    do
      let job = jobs.(!next) in
      let st =
        {
          index = !next;
          job;
          started = Os.counter ();
          w = spawn job;
          pending = job.cases;
          finishing = false;
          rev_results = [];
          rev_failures = [];
          hits = 0;
        }
      in
      incr next;
      running := !running @ [ st ];
      send_next st
    done
  in
  (* A worker that ended after its Finish ran the job; one that retired
     mid-job hands the rest to a fresh fork; any other end loses the job. *)
  let closed st =
    let status = Option.value (reap st.w) ~default:(Unix.WEXITED 255) in
    match status with
    | Unix.WEXITED 0 when st.finishing && st.pending = [] -> settle st Ran
    | Unix.WEXITED 0 when st.finishing ->
        bury st.w;
        st.w <- spawn st.job;
        st.finishing <- false;
        send_next st
    | status ->
        settle st
          (Lost
             (if st.w.exited then "ended when code under test called exit"
              else ended status))
  in
  let handle st =
    match receive st.w.reports with
    | exception (Closed | Failure _ | End_of_file) -> closed st
    | Report (Done { served; retire; _ }) ->
        st.rev_results <- served.result :: st.rev_results;
        if st.job.bail && counted_failure served.result then st.pending <- [];
        if retire then begin
          st.finishing <- true;
          request st.w Finish
        end
        else send_next st
    | Report (Released failures) ->
        st.rev_failures <- List.rev_append failures st.rev_failures
    | Report (Outside failure) -> st.rev_failures <- failure :: st.rev_failures
    | Report Exited -> st.w.exited <- true
    | Report (Event _ | Judge _) | Reached _ | Counts _ -> ()
    | Hits hits -> st.hits <- st.hits + hits
  in
  let expired st =
    kill st.w;
    ignore (reap st.w);
    settle st Expired
  in
  let rec deliver () =
    if !delivered < n then
      match settled.(!delivered) with
      | None -> ()
      | Some ran ->
          let index = !delivered in
          incr delivered;
          on_ran index ran;
          deliver ()
  in
  let remaining st = st.job.deadline -. Os.count_s st.started in
  (* A worker found dead may have left reports in its pipe, which say more
     than its status: they are read first. *)
  let rec dead st =
    if List.memq st !running then
      match Unix.select [ st.w.reports ] [] [] 0. with
      | _ :: _, _, _ ->
          handle st;
          dead st
      | [], _, _ -> closed st
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> dead st
  in
  let rec loop () =
    deliver ();
    start ();
    match !running with
    | [] -> ()
    | _ when Option.is_some !signal -> ()
    | running' ->
        let wait =
          List.fold_left
            (fun acc st -> Float.min acc (Float.max 0. (remaining st)))
            1. running'
        in
        let ready =
          match
            Unix.select (List.map (fun st -> st.w.reports) running') [] [] wait
          with
          | ready, _, _ -> ready
          | exception Unix.Unix_error (Unix.EINTR, _, _) -> []
        in
        List.iter
          (fun st ->
            if Option.is_none !signal then
              if remaining st <= 0. then expired st
              else if List.mem st.w.reports ready then handle st
              else if ready = [] then
                match reap ~flags:[ Unix.WNOHANG ] st.w with
                | Some _ -> dead st
                | None -> ())
          running';
        loop ()
  in
  let stop_all () =
    List.iter
      (fun st ->
        kill st.w;
        ignore (reap st.w);
        bury st.w)
      !running;
    running := []
  in
  (* A reader that went away stops the jobs as an interrupt does. *)
  Os.with_signals (Sys.sigpipe :: interrupting) (fun s ->
      if Option.is_none !signal then signal := Some s;
      on_signal s)
  @@ fun () ->
  match loop () with
  | () -> stop_all ()
  | exception exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      stop_all ();
      Printexc.raise_with_backtrace exn backtrace
