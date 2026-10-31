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
  gate : Mutex.t;
  opened : Condition.t;
  asleep : int Atomic.t;
  ended : int Atomic.t array;
  endings : ending array;
  mutable domains : unit Domain.t list;
  mutable stuck : bool;
}

let size t = Array.length t.endings

(* A local closure over [marks] would allocate in every call, and a worker
   and the caller spin on this. *)
let rec marked_from marks generation i =
  i = Array.length marks
  || (Atomic.get marks.(i) = generation && marked_from marks generation (i + 1))

let all_marked marks generation = marked_from marks generation 0

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

(* [Format.std_formatter] is the main domain's: a worker's printing goes to
   its own domain's formatters. *)
let flush_formatters () =
  Format.pp_print_flush (Format.get_std_formatter ()) ();
  Format.pp_print_flush (Format.get_err_formatter ()) ()

(* The start barrier. A worker that arrives before the others spins, so
   that on a processor of its own it starts its job within a spin of the
   last to arrive. Past [spin_ns] it sleeps on [opened] and leaves its
   processor to the workers it waits for, which may share it; the last to
   arrive wakes it. A sleeper counts itself in [asleep], under [gate],
   before it reads the marks, and an arriver reads [asleep] after its mark,
   so either the sleeper sees the mark or the arriver sees the sleeper. A
   sleeper's generation stays in every mark until it has passed, since the
   next is published only once every worker has ended.

   An asynchronous exception can land at a poll point between a mark and
   the wake, as a signal handler or a finaliser raises: in the loop of
   [all_marked], or as a contended lock of [gate] leaves its blocking
   section. The worker then opens the gate again as it records the
   exception as its job's ending, and [recover] opens it once more whatever
   else lands.

   Two workers woken together for a run arrive within 3 us of each other
   when each has a processor; one that waits for a processor arrives tens
   of microseconds to milliseconds after the other. 20 us of spinning takes
   the first and leaves the processor in the second, where spinning on
   would cost a worker that shares it a time slice. The bound is a time,
   since a spin's cost varies tenfold between processors. Yielding before
   sleeping hands the processor to whatever else waits for it, for its
   slice: on a loaded machine that cost each run 2 ms and most of the
   jobs' overlap. The last to arrive wakes a sleeper at once, where sleeping
   in steps, as [wait] does, would start it a step late. *)
let spin_ns = 20_000

(* Wakes the sleepers once every worker has arrived. A spurious wake is
   harmless: a sleeper reads the marks again. *)
let open_gate t generation =
  if Atomic.get t.asleep > 0 && all_marked t.arrived generation then begin
    Mutex.lock t.gate;
    match Condition.broadcast t.opened with
    | () -> Mutex.unlock t.gate
    | exception exn ->
        Mutex.unlock t.gate;
        raise exn
  end

(* Only a sleeper writes [asleep], under [gate]. *)
let count_asleep t n = Atomic.set t.asleep (Atomic.get t.asleep + n)

(* An exception out of the blocking wait leaves [asleep] as it found it and
   [gate] released. *)
let sleep t generation =
  Mutex.lock t.gate;
  count_asleep t 1;
  match
    while not (all_marked t.arrived generation) do
      Condition.wait t.opened t.gate
    done
  with
  | () ->
      count_asleep t (-1);
      Mutex.unlock t.gate
  | exception exn ->
      count_asleep t (-1);
      Mutex.unlock t.gate;
      raise exn

(* A clock that fails reads 0, and the worker then sleeps at once. *)
let rec pass t generation start =
  if not (all_marked t.arrived generation) then begin
    let now = Os.spin_ns () in
    if now <= 0 || now - start >= spin_ns then sleep t generation
    else begin
      Domain.cpu_relax ();
      pass t generation start
    end
  end

(* Whatever the job raises is its ending, and opens the gate. *)
let run_job t i generation jobs =
  let ending =
    match
      Atomic.set t.arrived.(i) generation;
      open_gate t generation;
      pass t generation (Os.spin_ns ());
      jobs.(i) ()
    with
    | () -> Done
    | exception exn ->
        let backtrace = Printexc.get_raw_backtrace () in
        open_gate t generation;
        Raised (exn, backtrace)
  in
  (try flush_formatters () with _ -> ());
  ending

(* Nothing leaves the loop but an exit. An exception that escapes [serve],
   as an asynchronous one may between its handlers, ends the job taken and
   not yet ended, if any: the worker arrives and opens the gate, so the
   other jobs start, and the exception is the job's ending, so no run waits
   for this worker. An exception in that recovery starts it again. The loop
   then takes the next job. *)
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
      end;
      open_gate t !taken
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
      gate = Mutex.create ();
      opened = Condition.create ();
      asleep = Atomic.make 0;
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
  match Os.with_blocked runner_signals start_all with
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
