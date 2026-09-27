(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type ending = Done | Raised of exn * Printexc.raw_backtrace

exception Stuck of exn * Printexc.raw_backtrace

(* The caller publishes a run under [mutex], as a new [generation] and its
   [jobs]. Worker [i] marks the generation it took in [arrived.(i)] at the
   start barrier, and in [ended.(i)] once it has written its [ending], so
   the ending and every write of the job are visible to the caller that
   reads the mark. A mark is an atomic store: it allocates nothing, so no
   asynchronous exception lands between a worker and its mark, as one can
   in [Atomic.incr], which allocates from OCaml 5.4; and marking twice is
   harmless. *)
type t = {
  mutex : Mutex.t;
  wake : Condition.t;
  mutable generation : int;
  mutable jobs : (unit -> unit) array;
  mutable quit : bool;
  arrived : int Atomic.t array; (* the start barrier *)
  ended : int Atomic.t array;
  endings : ending array;
  mutable domains : unit Domain.t list;
  mutable stuck : bool;
}

let size t = Array.length t.endings

let all_marked marks generation =
  let rec from i =
    i = Array.length marks || (Atomic.get marks.(i) = generation && from (i + 1))
  in
  from 0

(* The next generation after [seen], or [None] to exit. An exception out of
   the blocking wait, as a handler of the user's may raise, leaves the mutex
   released. *)
let take t seen =
  Mutex.lock t.mutex;
  match
    while t.generation = seen && not t.quit do
      Condition.wait t.wake t.mutex
    done;
    if t.quit then None else Some (t.generation, t.jobs)
  with
  | next ->
      Mutex.unlock t.mutex;
      next
  | exception exn ->
      Mutex.unlock t.mutex;
      raise exn

let flush_formatters () =
  Format.pp_print_flush Format.std_formatter ();
  Format.pp_print_flush Format.err_formatter ()

(* Whatever the job raises is its ending. *)
let run_job t i generation jobs =
  let ending =
    match
      Atomic.set t.arrived.(i) generation;
      while not (all_marked t.arrived generation) do
        Domain.cpu_relax ()
      done;
      jobs.(i) ()
    with
    | () -> Done
    | exception exn -> Raised (exn, Printexc.get_raw_backtrace ())
  in
  (try flush_formatters () with _ -> ());
  ending

(* Nothing leaves the loop but an exit. An exception that escapes [serve],
   as an asynchronous one may between its handlers, ends the job taken and
   not yet ended, if any: the worker arrives, so the other jobs start, and
   the exception is the job's ending, so no run waits for this worker. An
   exception in that recovery starts it again. The loop then takes the next
   job. *)
let work t i =
  let taken = ref 0 in
  let publish ending =
    t.endings.(i) <- ending;
    Atomic.set t.ended.(i) !taken
  in
  let rec serve () =
    match take t (Atomic.get t.ended.(i)) with
    | None -> ()
    | Some (generation, jobs) ->
        taken := generation;
        publish (run_job t i generation jobs);
        serve ()
  in
  let rec guard () =
    match serve () with () -> () | exception exn -> recover exn
  and recover exn =
    match
      if Atomic.get t.ended.(i) <> !taken then begin
        Atomic.set t.arrived.(i) !taken;
        publish (Raised (exn, Printexc.get_raw_backtrace ()))
      end
    with
    | () -> guard ()
    | exception exn -> recover exn
  in
  guard ()

let tell_quit t =
  Mutex.lock t.mutex;
  t.quit <- true;
  Condition.broadcast t.wake;
  Mutex.unlock t.mutex

(* The signals a test runner handles. A domain is born with its creator's
   mask, so a worker spawned while the calling domain blocks them never runs
   the runner's handlers, not even in its first instructions. *)
let runner_signals = [ Sys.sigalrm; Sys.sigint; Sys.sigterm; Sys.sighup ]

(* [masked fn] is [fn ()] with [runner_signals] blocked on the calling
   domain, whose mask is put back however [fn] ends. The mask is read before
   it changes, so a handler that raises in the change still has it put back.
   A signal that arrived meanwhile is handled when the mask is put back; over
   an exception of [fn], what its handler raises is dropped for [fn]'s. *)
let masked fn =
  if Sys.win32 then fn ()
  else
    let previous = Unix.sigprocmask Unix.SIG_BLOCK [] in
    let restore () =
      ignore (Unix.sigprocmask Unix.SIG_SETMASK previous : int list)
    in
    match
      ignore (Unix.sigprocmask Unix.SIG_BLOCK runner_signals : int list);
      fn ()
    with
    | () -> restore ()
    | exception exn ->
        let backtrace = Printexc.get_raw_backtrace () in
        (try restore () with _ -> ());
        Printexc.raise_with_backtrace exn backtrace

let spawn n =
  if n < 1 then invalid_arg "Workers.spawn: no worker";
  let t =
    {
      mutex = Mutex.create ();
      wake = Condition.create ();
      generation = 0;
      jobs = [||];
      quit = false;
      arrived = Array.init n (fun _ -> Atomic.make 0);
      ended = Array.init n (fun _ -> Atomic.make 0);
      endings = Array.make n Done;
      domains = [];
      stuck = false;
    }
  in
  let backtraces = Printexc.backtrace_status () in
  let start i =
    Domain.spawn (fun () ->
        Printexc.record_backtrace backtraces;
        work t i)
  in
  let start_all () =
    for i = 0 to n - 1 do
      t.domains <- start i :: t.domains
    done
  in
  (* A worker whose domain was lost to a raise still exits on [quit]. *)
  match masked start_all with
  | () -> t
  | exception exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      tell_quit t;
      List.iter Domain.join t.domains;
      Printexc.raise_with_backtrace exn backtrace

(* Spinning answers a short run at once; a long one is waited for in
   sleeps, which leave the workers the processors. A signal handler runs at
   the poll points of either. *)
let spins = 20_000
let pause = 0.000_2

let wait done_ =
  let rec step spun =
    if not (done_ ()) then
      if spun < spins then begin
        Domain.cpu_relax ();
        step (spun + 1)
      end
      else begin
        Unix.sleepf pause;
        step spun
      end
  in
  step 0

(* Nothing a handler raises during the grace ends it early. *)
let rec wait_until ~started ~grace done_ =
  match
    while (not (done_ ())) && Os.count_s started < grace do
      Unix.sleepf pause
    done
  with
  | () -> ()
  | exception _ -> wait_until ~started ~grace done_

(* Nothing between the publication and the wait can take a signal but the
   lock, which leaves nothing published when it raises. *)
let run t ~grace jobs =
  if Array.length jobs <> size t then
    invalid_arg "Workers.run: one job per worker";
  if t.stuck then invalid_arg "Workers.run: a worker is stuck";
  let generation = t.generation + 1 in
  let done_ () = all_marked t.ended generation in
  let published = ref false in
  match
    Mutex.lock t.mutex;
    t.jobs <- jobs;
    t.generation <- generation;
    published := true;
    Condition.broadcast t.wake;
    Mutex.unlock t.mutex;
    wait done_
  with
  | () ->
      let raised = function
        | Done -> ()
        | Raised (exn, backtrace) -> Printexc.raise_with_backtrace exn backtrace
      in
      Array.iter raised t.endings
  | exception exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      if !published then
        wait_until ~started:(Os.counter ()) ~grace:(grace exn) done_;
      if (not !published) || done_ () then
        Printexc.raise_with_backtrace exn backtrace
      else begin
        t.stuck <- true;
        raise (Stuck (exn, backtrace))
      end

let join t =
  tell_quit t;
  if not t.stuck then List.iter Domain.join t.domains;
  t.domains <- []
