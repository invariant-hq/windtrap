(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Semantics preservation of the coverage instrumentation (Law 13), checked
   by running the instrumented Covsem_fixtures library. This is the
   mandatory suite of Law 14 (as amended): the expression-grade out-edge
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
   [Windtrap_coverage.snapshot] to prove the generated registration and
   visit calls actually count.

   A windtrap suite ([run] executes tests sequentially in declaration
   order); the visited-count deltas observe the shared in-process
   registry, so the tests are order-dependent — run the suite whole, not
   filtered. The dune action sets WINDTRAP_COVERAGE=off: the inline
   coverage line would otherwise report the fixture's (deliberately
   incomplete) instrumentation on every green run. *)

open Windtrap
module F = Covsem_fixtures

let check name cond = is_true ~msg:name cond
let check_int name ~expected ~actual = equal ~msg:name int expected actual

let visited () =
  (Windtrap_coverage.summary (Windtrap_coverage.snapshot ())).visited

let tests =
  [
    test "registration happens at module load, before any call" (fun () ->
        let s = Windtrap_coverage.snapshot () in
        check "fixtures registered at load" (not (Windtrap_coverage.is_empty s));
        let summary = Windtrap_coverage.summary s in
        check "no point visited before any call" (summary.visited = 0);
        check "the fixture points are all registered" (summary.total >= 20));
    (* Tail calls survive entry sequencing and out-edge wrapping *)
    test "deep tail recursion survives instrumentation" ~tags:[ "slow" ]
      (fun () ->
        check "deep tail recursion through a match arm"
          (String.equal (F.countdown 100_000_000) "done");
        check "deep mutual tail recursion through if branches"
          (F.even 50_000_000 = true);
        check_int "deep CPS recursion: closures unwind through tail calls"
          ~expected:1_000_000
          ~actual:(F.cps_count 1_000_000 (fun x -> x));
        check_int "deep tail recursion through a pipeline" ~expected:0
          ~actual:(F.pipe_down 50_000_000);
        check "deep tail recursion through a || right arm"
          (F.any_odd 20_000_000 = false);
        check "the || arm still answers" (F.any_odd 7 = true);
        check "deep tail recursion through a && right arm"
          (F.all_even 20_000_000 = true);
        check "the && arm still answers" (F.all_even 3 = false));
    (* Evaluation order is untouched *)
    test "branch and guard evaluation order is untouched" (fun () ->
        let y, trace = F.order_witness true in
        check_int "order witness true takes arm1" ~expected:10 ~actual:y;
        check "order witness true trace"
          (trace = [ "cond"; "then"; "scrutinee"; "guard"; "arm1" ]);
        let y, trace = F.order_witness false in
        check_int "order witness false takes arm2" ~expected:20 ~actual:y;
        check "order witness false trace"
          (trace = [ "cond"; "else"; "scrutinee"; "arm2" ]));
    (* Short-circuit order: the desugared [||]/[&&] must evaluate left
       first, right only when left did not decide, and never eagerly. *)
    test "|| and && short-circuit as uninstrumented" (fun () ->
        check "|| short-circuits: a true left arm skips the right"
          (F.or_trace true true = (true, [ "left" ]));
        check "|| evaluates left before right"
          (F.or_trace false true = (true, [ "left"; "right" ]));
        check "|| is false only after both arms"
          (F.or_trace false false = (false, [ "left"; "right" ]));
        check "&& short-circuits: a false left arm skips the right"
          (F.and_trace false true = (false, [ "left" ]));
        check "&& evaluates left before right"
          (F.and_trace true false = (false, [ "left"; "right" ]));
        check "&& is true only after both arms"
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
        check "argument evaluation order matches the uninstrumented baseline"
          (actual = baseline);
        check "argument values are untouched" (fst actual = (1, 2)));
    test "sequence statements run left to right, once" (fun () ->
        check "sequence statements run left to right, once"
          (F.seq_order () = [ "one"; "two" ]));
    (* Lazy stays lazy; trivial lazy stays a value *)
    test "lazy stays lazy; trivial lazy stays a value" (fun () ->
        check "lazy body not run at module load" (!F.forced = false);
        let before = visited () in
        check_int "forcing the thunk" ~expected:42 ~actual:(Lazy.force F.thunk);
        let after = visited () in
        check "lazy body ran on force" (!F.forced = true);
        check_int "forcing visited exactly the lazy-body point" ~expected:1
          ~actual:(after - before);
        check_int "forcing again computes the same value" ~expected:42
          ~actual:(Lazy.force F.thunk);
        check_int "forcing twice runs the body once: the count stays 1"
          ~expected:1 ~actual:!F.force_count;
        check_int "forcing again visits nothing" ~expected:0
          ~actual:(visited () - after);
        check "trivial lazy compiles as in an uninstrumented build"
          (Lazy.is_val F.trivial = Lazy.is_val (lazy 42)));
    (* Raising applications: the out-edge is NOT counted

       [tap_ok] and [tap_raise] are shape-identical: leaf-body entry
       point, out-edge point on [f ()], and the argument's own leaf
       entry. The ok path visits all three; the raising path must visit
       only two - its [f ()] out-edge point never fires. This delta IS
       the re-grade: under block grade both paths looked identical. *)
    test "a raising application's out-edge is not counted" (fun () ->
        let before = visited () in
        check "the ok path returns" (F.tap_ok F.ret_unit = true);
        let after_ok = visited () in
        check_int "the ok path visits its out-edge (3 points)" ~expected:3
          ~actual:(after_ok - before);
        let raised =
          match F.tap_raise F.raise_unit with
          | _ -> false
          | exception Exit -> true
        in
        check "the raising path still raises Exit" raised;
        let after_raise = visited () in
        check_int
          "the raising path visits one point fewer: the out-edge is not counted"
          ~expected:2 ~actual:(after_raise - after_ok));
    (* Pipelines and method calls *)
    test "pipelines and method calls compute uninstrumented results" (fun () ->
        check_int "pipeline in tail position" ~expected:12
          ~actual:(F.pipeline 3);
        check_int "pipeline bound in a let" ~expected:7
          ~actual:(F.pipeline_bound 3);
        check_int "method calls through an object" ~expected:6
          ~actual:(F.sum_object [ 1; 2; 3 ]);
        let before = visited () in
        check_int "a send with a successor" ~expected:1
          ~actual:(F.poke (new F.adder));
        check "the send's out-edge counted" (visited () - before > 0));
    (* An arm that is itself a function: two points, two moments *)
    test "an arm that is itself a function: two points, two moments" (fun () ->
        let before = visited () in
        let add = F.dispatch `Add in
        let after_select = visited () in
        check_int "selecting the arm visits exactly the arm point" ~expected:1
          ~actual:(after_select - before);
        check_int "applying the closure" ~expected:5 ~actual:(add 2 3);
        let after_apply = visited () in
        check_int "applying visits exactly the leaf-body point" ~expected:1
          ~actual:(after_apply - after_select);
        check_int "re-applying visits no new point" ~expected:9
          ~actual:(add 4 5);
        check_int "visited counts points, not calls" ~expected:0
          ~actual:(visited () - after_apply));
    (* Instrumented forms compute the uninstrumented results *)
    test "instrumented forms compute the uninstrumented results" (fun () ->
        check_int "while loop" ~expected:55 ~actual:(F.sum_while 10);
        check_int "while loop, zero iterations" ~expected:0
          ~actual:(F.sum_while 0);
        check_int "for loop" ~expected:55 ~actual:(F.sum_for 10);
        check_int "letop bodies" ~expected:42 ~actual:(F.letop_sum 40 2);
        check "guards choose arms in order"
          (String.equal (F.bucket 5) "small"
          && String.equal (F.bucket 50) "medium"
          && String.equal (F.bucket 500) "large");
        check_int "try arm catches" ~expected:0 ~actual:(F.safe_div 7 0);
        check_int "try body result" ~expected:3 ~actual:(F.safe_div 7 2));
    (* The visit calls counted; a raising path lowers the % *)
    test "the visit calls counted; a raising path lowers the percentage"
      (fun () ->
        let s = Windtrap_coverage.snapshot () in
        let summary = Windtrap_coverage.summary s in
        check "points were visited" (summary.visited > 0);
        check "visited never exceeds total" (summary.visited <= summary.total);
        (* [tap_raise]'s out-edge can never fire, so the file can never
           reach 100%: raising paths lower the percentage (the point of
           the expression-grade re-grade). *)
        check "the raise out-edge keeps the file below 100%"
          (summary.visited < summary.total);
        check "the percentage reflects the unvisited out-edge"
          (Windtrap_coverage.percentage summary < 100.);
        match Windtrap_coverage.file_reports s with
        | [ report ] ->
            check "the registered file is the fixture module"
              (Filename.basename report.file = "covsem_fixtures.ml")
        | reports ->
            failf "exactly one instrumented file, got %d" (List.length reports));
  ]

let () = run "semantics" tests
