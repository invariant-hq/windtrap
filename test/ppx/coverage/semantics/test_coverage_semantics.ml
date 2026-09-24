(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Semantics preservation of the coverage instrumentation (guarantee 10),
   checked by running the instrumented Covsem_fixtures library. This is the
   mandatory suite of the frozen expression-grade scope: the out-edge
   machinery - post-visit wrapping under tail-position analysis - is
   exactly what could break tail calls, laziness, or evaluation order if
   mishandled, so deep self-, mutual-, CPS-, pipeline-, and condition-arm
   recursion must not overflow, evaluation order (branches, arguments,
   sequences) must match an uninstrumented baseline, lazy must stay lazy
   and trivial lazy already forced, raising calls must raise as before -
   with their out-edge NOT counted - and every result must be the one an
   uninstrumented build computes. This executable is itself
   uninstrumented, so its own definitions are the uninstrumented
   baselines. The final tests read the in-process runtime through
   [Windtrap_runtime.Coverage.snapshot] to prove the generated registration and
   visit calls actually count.

   A windtrap suite ([run] executes tests sequentially in declaration
   order); the last test reads what the others visited in the shared
   in-process registry, so run the suite whole, not filtered. *)

open Windtrap
module F = Covsem_fixtures

(* Every claim below is about the fixture library, and the registry is the
   whole process: under `--instrument-with` this executable links an
   instrumented windtrap core, whose thousands of points would drown the
   fixture's twenty. Scope once, here, and the suite says the same thing
   instrumented or not. *)
let fixture_snapshot () =
  Windtrap_runtime.Coverage.filter
    (fun file -> Filename.basename file = "covsem_fixtures.ml")
    (Windtrap_runtime.Coverage.snapshot ())

(* The fixture's hit count per point, read off the collection's
   serialization — the v3 format test/instr/coverage pins byte for byte:
   the magic line, the file count, [len name], the point count, then one
   [start end count] line per point. Counts, not the summary's tally of
   visited points, because a test here may run more than once in one
   process — the mutation loop's probe and its children are forks of the
   process that ran the dry run — and a point an earlier run visited is
   visited still. What a call hits is the points whose counts it raised. *)
let counts () =
  match
    String.split_on_char '\n'
      (Windtrap_runtime.Coverage.to_string (fixture_snapshot ()))
  with
  | _magic :: "1" :: _file :: _points :: lines ->
      List.filter_map
        (fun line ->
          match String.split_on_char ' ' line with
          | [ start; stop; count ] ->
              Some
                ((int_of_string start, int_of_string stop), int_of_string count)
          | _ -> None)
        lines
  | _ -> failf "the fixture snapshot does not serialize as one file"

(* [hits f] is [f ()] with the points it raised, each with its increment. *)
let hits f =
  let before = counts () in
  let value = f () in
  let after = counts () in
  ( value,
    List.filter_map
      (fun ((point, before), (_, after)) ->
        if after > before then Some (point, after - before) else None)
      (List.combine before after) )

let hit_points hits = List.map fst hits

(* What registration did before any call is observable only before
   anything else ran, and the test that reports it may run after an
   earlier run of itself. Captured once, at module load, before the first
   test. *)
let at_load = fixture_snapshot ()

let tests =
  [
    test "registration happens at module load, before any call" (fun () ->
        is_true ~msg:"fixtures registered at load"
          (not (Windtrap_runtime.Coverage.is_empty at_load));
        let summary = Windtrap_runtime.Coverage.summary at_load in
        is_true ~msg:"no point visited before any call" (summary.visited = 0);
        is_true ~msg:"the fixture points are all registered"
          (summary.total >= 20));
    (* Tail calls survive entry sequencing and out-edge wrapping *)
    test "deep tail recursion survives instrumentation" ~tags:[ "slow" ]
      (fun () ->
        is_true ~msg:"deep tail recursion through a match arm"
          (String.equal (F.countdown 100_000_000) "done");
        is_true ~msg:"deep mutual tail recursion through if branches"
          (F.even 50_000_000 = true);
        equal ~msg:"deep CPS recursion: closures unwind through tail calls" int
          1_000_000
          (F.cps_count 1_000_000 (fun x -> x));
        equal ~msg:"deep tail recursion through a pipeline" int 0
          (F.pipe_down 50_000_000);
        is_true ~msg:"deep tail recursion through a || right arm"
          (F.any_odd 20_000_000 = false);
        is_true ~msg:"the || arm still answers" (F.any_odd 7 = true);
        is_true ~msg:"deep tail recursion through a && right arm"
          (F.all_even 20_000_000 = true);
        is_true ~msg:"the && arm still answers" (F.all_even 3 = false));
    test "|| right arms that are not applications still compute" (fun () ->
        (* What this proves: each arm still computes the uninstrumented
           result. What it does NOT prove: that the arm kept its tail call —
           these return [true] whether or not the call was post-wrapped,
           because OCaml 5 grows the main fibre's stack on demand and a lost
           tail call does not reliably overflow at these depths. The
           tail-call property is pinned byte-wise on the expansion, in
           test/ppx/coverage/fixture_cond.expected, which carries one
           function per shape the instrumenter's tail guard lists (let,
           match, if, try, sequence, open, letmodule, letexception, letop,
           constraint, coerce). Only the four with a fixture here are also
           run. *)
        is_true ~msg:"let arm" (F.or_let 3_000_000 = true);
        is_true ~msg:"match arm" (F.or_match 3_000_000 = true);
        is_true ~msg:"if arm" (F.or_if 3_000_000 = true);
        is_true ~msg:"try arm" (F.or_try 1_000_000 = true));
    test "tail_mod_cons survives instrumentation" (fun () ->
        (* If the attribute had been stripped this file would not compile;
           if it were honoured but the call wrapped, this would overflow. *)
        let n = 2_000_000 in
        let xs = List.init n (fun i -> i) in
        equal ~msg:"the TMC map is constant-stack and correct" int n
          (List.length (F.tmc_map succ xs)));
    (* Evaluation order is untouched *)
    test "branch and guard evaluation order is untouched" (fun () ->
        let y, trace = F.order_witness true in
        equal ~msg:"order witness true takes arm1" int 10 y;
        is_true ~msg:"order witness true trace"
          (trace = [ "cond"; "then"; "scrutinee"; "guard"; "arm1" ]);
        let y, trace = F.order_witness false in
        equal ~msg:"order witness false takes arm2" int 20 y;
        is_true ~msg:"order witness false trace"
          (trace = [ "cond"; "else"; "scrutinee"; "arm2" ]));
    (* Short-circuit order: the desugared [||]/[&&] must evaluate left
       first, right only when left did not decide, and never eagerly. *)
    test "|| and && short-circuit as uninstrumented" (fun () ->
        is_true ~msg:"|| short-circuits: a true left arm skips the right"
          (F.or_trace true true = (true, [ "left" ]));
        is_true ~msg:"|| evaluates left before right"
          (F.or_trace false true = (true, [ "left"; "right" ]));
        is_true ~msg:"|| is false only after both arms"
          (F.or_trace false false = (false, [ "left"; "right" ]));
        is_true ~msg:"&& short-circuits: a false left arm skips the right"
          (F.and_trace false true = (false, [ "left" ]));
        is_true ~msg:"&& evaluates left before right"
          (F.and_trace true false = (false, [ "left"; "right" ]));
        is_true ~msg:"&& is true only after both arms"
          (F.and_trace true true = (true, [ "left"; "right" ])));
    (* Argument order: this executable is uninstrumented, so the same shape
       computed locally is the baseline the instrumented fixture must
       match. *)
    test "argument evaluation order matches the uninstrumented baseline"
      (fun () ->
        let baseline =
          let log = ref [] in
          let note tag value =
            log := tag :: !log;
            value
          in
          let two_args a b = (a, b) in
          let r = two_args (note "first" 1) (note "second" 2) in
          (r, List.rev !log)
        in
        let actual = F.arg_order () in
        is_true
          ~msg:"argument evaluation order matches the uninstrumented baseline"
          (actual = baseline);
        is_true ~msg:"argument values are untouched" (fst actual = (1, 2)));
    test "sequence statements run left to right, once" (fun () ->
        is_true ~msg:"sequence statements run left to right, once"
          (F.seq_order () = [ "one"; "two" ]));
    (* Lazy stays lazy; trivial lazy stays a value *)
    test "lazy stays lazy; trivial lazy stays a value" (fun () ->
        let thunk, forced, force_count = F.make_thunk () in
        is_true ~msg:"lazy body not run at construction" (!forced = false);
        let v, force = hits (fun () -> Lazy.force thunk) in
        equal ~msg:"forcing the thunk" int 42 v;
        is_true ~msg:"lazy body ran on force" (!forced = true);
        is_true ~msg:"forcing visited exactly the lazy-body point, once"
          (match force with [ (_, 1) ] -> true | _ -> false);
        let v, again = hits (fun () -> Lazy.force thunk) in
        equal ~msg:"forcing again computes the same value" int 42 v;
        equal ~msg:"forcing twice runs the body once: the count stays 1" int 1
          !force_count;
        is_true ~msg:"forcing again visits nothing" (again = []);
        is_true ~msg:"trivial lazy compiles as in an uninstrumented build"
          (Lazy.is_val (F.trivial ()) = Lazy.is_val (lazy 42)));
    (* Raising applications: the out-edge is NOT counted

       [tap_ok] and [tap_raise] are shape-identical: leaf-body entry
       point, out-edge point on [f ()], and the argument's own leaf
       entry. The ok path visits all three; the raising path must visit
       only two - its [f ()] out-edge point never fires. This delta IS
       the re-grade: under block grade both paths looked identical. *)
    test "a raising application's out-edge is not counted" (fun () ->
        let returned, ok = hits (fun () -> F.tap_ok F.ret_unit) in
        is_true ~msg:"the ok path returns" (returned = true);
        equal ~msg:"the ok path visits its out-edge (3 points)" int 3
          (List.length ok);
        let raised, raise =
          hits (fun () ->
              match F.tap_raise F.raise_unit with
              | _ -> false
              | exception Exit -> true)
        in
        is_true ~msg:"the raising path still raises Exit" raised;
        equal
          ~msg:
            "the raising path visits one point fewer: the out-edge is not \
             counted"
          int 2 (List.length raise));
    (* Pipelines and method calls *)
    test "pipelines and method calls compute uninstrumented results" (fun () ->
        equal ~msg:"pipeline in tail position" int 12 (F.pipeline 3);
        equal ~msg:"pipeline bound in a let" int 7 (F.pipeline_bound 3);
        equal ~msg:"method calls through an object" int 6
          (F.sum_object [ 1; 2; 3 ]);
        let poked, send = hits (fun () -> F.poke (new F.adder)) in
        equal ~msg:"a send with a successor" int 1 poked;
        is_true ~msg:"the send's out-edge counted" (send <> []));
    (* An arm that is itself a function: two points, two moments *)
    test "an arm that is itself a function: two points, two moments" (fun () ->
        let add, select = hits (fun () -> F.dispatch `Add) in
        equal ~msg:"selecting the arm visits exactly the arm point" int 1
          (List.length select);
        let sum, apply = hits (fun () -> add 2 3) in
        equal ~msg:"applying the closure" int 5 sum;
        equal ~msg:"applying visits exactly the leaf-body point" int 1
          (List.length apply);
        is_true ~msg:"a point of its own, not the arm's"
          (hit_points apply <> hit_points select);
        let sum, again = hits (fun () -> add 4 5) in
        equal ~msg:"re-applying" int 9 sum;
        is_true
          ~msg:"re-applying visits no new point: points are counted, not calls"
          (hit_points again = hit_points apply));
    (* Instrumented forms compute the uninstrumented results *)
    test "instrumented forms compute the uninstrumented results" (fun () ->
        equal ~msg:"while loop" int 55 (F.sum_while 10);
        equal ~msg:"while loop, zero iterations" int 0 (F.sum_while 0);
        equal ~msg:"for loop" int 55 (F.sum_for 10);
        equal ~msg:"letop bodies" int 42 (F.letop_sum 40 2);
        is_true ~msg:"guards choose arms in order"
          (String.equal (F.bucket 5) "small"
          && String.equal (F.bucket 50) "medium"
          && String.equal (F.bucket 500) "large");
        equal ~msg:"try arm catches" int 0 (F.safe_div 7 0);
        equal ~msg:"try body result" int 3 (F.safe_div 7 2));
    (* The visit calls counted; a raising path lowers the % *)
    test "the visit calls counted; a raising path lowers the percentage"
      (fun () ->
        let s = fixture_snapshot () in
        let summary = Windtrap_runtime.Coverage.summary s in
        is_true ~msg:"points were visited" (summary.visited > 0);
        is_true ~msg:"visited never exceeds total"
          (summary.visited <= summary.total);
        (* [tap_raise]'s out-edge can never fire, so the file can never
           reach 100%: raising paths lower the percentage (the point of
           the expression-grade re-grade). *)
        is_true ~msg:"the raise out-edge keeps the file below 100%"
          (summary.visited < summary.total);
        is_true ~msg:"the percentage reflects the unvisited out-edge"
          (Windtrap_runtime.Coverage.percentage summary < 100.);
        match Windtrap_runtime.Coverage.file_reports s with
        | [ report ] ->
            is_true ~msg:"the registered file is the fixture module"
              (Filename.basename report.file = "covsem_fixtures.ml")
        | reports ->
            failf "exactly one instrumented file, got %d" (List.length reports));
  ]

let () = exit @@ run "coverage semantics" tests
