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
   baselines. The tests read the counts of this process through the
   at_exit dump of a forked child, to prove the generated registration and
   visit calls actually count. Each reads what its own calls visited, and
   registration is captured at module load, so each passes alone. *)

open Windtrap
module F = Covsem_fixtures
module C = Windtrap_runtime.Coverage
module I = Windtrap_runtime.Instr

(* A lost tail call overflows only a bounded stack, and OCaml 5's default
   bound is 1 GiB, which no depth a test can afford reaches. The dune
   action runs this suite under OCAMLRUNPARAM=l=1M, an 8 MiB stack, where
   a recursion of [depth] frames overflows whatever its frame size, and a
   tail recursion of any depth does not. [non_tail] is the positive
   control: deliberately not a tail call, so the suite shows the bound
   bites before trusting what the tail tests read under it. *)
let depth = 2_000_000
let rec non_tail n = if n = 0 then 0 else 1 + non_tail (n - 1)

let rec non_tail_map f = function
  | [] -> []
  | x :: xs -> f x :: non_tail_map f xs

(* [dump_after f] forks a child that runs [f] and exits, and waits for its
   at_exit dump: the counts of this process as they stood, plus what [f]
   visited. WINDTRAP_COVERAGE_FILE names one path, fixed at the first
   registration, so each dump is read before the next child writes it.
   The buffers are flushed first, or the child's exit would print them
   again. *)
let dump = Sys.getenv "WINDTRAP_COVERAGE_FILE"

let dump_after f =
  Stdlib.flush_all ();
  Format.pp_print_flush Format.std_formatter ();
  Format.pp_print_flush Format.err_formatter ();
  match Unix.fork () with
  | 0 ->
      (try ignore (f ()) with _ -> ());
      exit 0
  | pid -> (
      match Unix.waitpid [] pid with
      | _, Unix.WEXITED 0 -> ()
      | _ -> failf "the dumping child did not exit 0")

(* Every claim below is about the fixture library, and the dump is the
   whole process: under `--instrument-with` this executable links an
   instrumented windtrap core, whose thousands of points would drown the
   fixture's twenty. Scope once, here, and the suite says the same thing
   instrumented or not. *)
let is_fixture file = Filename.basename file = "covsem_fixtures.ml"

(* The fixture's count per point, read with the runtime's scanner in the
   v3 format that test/instr/coverage pins: the magic line, the writer's
   identity if any, the file count, then per file [len name], the point
   count and one [start end count] per point. Counts, not a tally of
   visited points, because a test here may run more than once in one
   process — the mutation loop's probe and its children are forks of the
   process that ran the dry run — and a point an earlier run visited is
   visited still. *)
let counts () =
  let text = In_channel.with_open_bin dump In_channel.input_all in
  let c =
    match I.start C.format ~path:"dump" text with
    | Ok c -> c
    | Error e -> failf "the dump does not start: %a" (I.pp_error C.format) e
  in
  ignore (I.read_identity c);
  let fixture = ref None in
  for _ = 1 to I.read_count c "files" do
    let file = I.read_name c "file" in
    let rows = ref [] in
    for _ = 1 to I.read_count c "points" do
      let start = I.read_nat c "start" in
      let stop = I.read_nat c "end" in
      rows := ((start, stop), I.read_nat c "count") :: !rows
    done;
    if is_fixture file then fixture := Some (List.rev !rows)
  done;
  match !fixture with
  | Some rows -> rows
  | None -> failf "the dump holds no fixture file"

(* [hits f] is [f ()] with the points it raised, each with its increment:
   a child that exits at once dumps the counts before, a child that runs
   [f] dumps them after, and this process then runs [f] for its value. *)
let hits f =
  dump_after ignore;
  let before = counts () in
  dump_after f;
  let after = counts () in
  let value = f () in
  ( value,
    List.filter_map
      (fun ((point, before), (_, after)) ->
        if after > before then Some (point, after - before) else None)
      (List.combine before after) )

let hit_points hits = List.map fst hits

(* The fixture's report in the dump of a child forked now. *)
let fixture_report () =
  dump_after ignore;
  match C.load dump with
  | Error e -> failf "the dump does not load: %a" C.pp_error e
  | Ok (t, _) -> (
      match
        List.filter
          (fun (r : C.file_report) -> is_fixture r.C.file)
          (C.file_reports t)
      with
      | [ report ] -> report
      | reports ->
          failf "exactly one fixture report, got %d" (List.length reports))

(* What registration did before any call is observable only before
   anything else ran, and the test that reports it may run after an
   earlier run of itself. Captured once, at module load, before the first
   test. *)
let at_load = fixture_report ()

let tests =
  [
    test "registration happens at module load, before any call" (fun () ->
        let summary = at_load.C.summary in
        is_true ~msg:"no point visited before any call" (summary.visited = 0);
        is_true ~msg:"the fixture points are all registered"
          (summary.total >= 20));
    (* Tail calls survive entry sequencing and out-edge wrapping *)
    test "a non-tail recursion as deep as the tail tests overflows" (fun () ->
        match non_tail depth with
        | _ ->
            failf
              "a non-tail recursion %d frames deep returned: the stack is not \
               bounded, so no tail test below can fail (the dune action runs \
               this suite under OCAMLRUNPARAM=l=1M)"
              depth
        | exception Stack_overflow -> ());
    test "deep tail recursion survives instrumentation" (fun () ->
        is_true ~msg:"deep tail recursion through a match arm"
          (String.equal (F.countdown depth) "done");
        is_true ~msg:"deep mutual tail recursion through if branches"
          (F.even depth = true);
        equal ~msg:"deep CPS recursion: closures unwind through tail calls" int
          depth
          (F.cps_count depth (fun x -> x));
        equal ~msg:"deep tail recursion through a pipeline" int 0
          (F.pipe_down depth);
        is_true ~msg:"deep tail recursion through a || right arm"
          (F.any_odd depth = false);
        is_true ~msg:"the || arm still answers" (F.any_odd 7 = true);
        is_true ~msg:"deep tail recursion through a && right arm"
          (F.all_even depth = true);
        is_true ~msg:"the && arm still answers" (F.all_even 3 = false));
    test "|| right arms that are not applications keep their tail calls"
      (fun () ->
        is_true ~msg:"let arm" (F.or_let depth = true);
        is_true ~msg:"match arm" (F.or_match depth = true);
        is_true ~msg:"if arm" (F.or_if depth = true);
        (* The recursive call of [or_try] is in the body of its [try],
           which is never a tail position, instrumented or not: the arm
           computes, and its depth stays within the bound. The expansion
           golden pins that the [try] arm keeps its position and that its
           handler's call stays bare. *)
        is_true ~msg:"try arm" (F.or_try 1_000 = true));
    test "tail_mod_cons survives instrumentation" (fun () ->
        (* A lost TMC call is warning 71, which the dev profile's
           [-warn-error +a] makes a build failure of the fixture library;
           under a profile that does not, the map silently consumes stack,
           and the bound makes that an overflow here. The control shows a
           map that is not TMC overflows at this length. *)
        let xs = List.init depth (fun i -> i) in
        (match non_tail_map succ xs with
        | _ -> failf "a non-TMC map over %d elements returned" depth
        | exception Stack_overflow -> ());
        equal ~msg:"the TMC map is constant-stack and correct" int depth
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
        (* [poke]'s body entry, the body of the method [total], and the
           out-edge of [a#total]: without the out-edge, two. *)
        equal ~msg:"the send's out-edge counted, with the two entries" int 3
          (List.length send));
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
    (* The visits reach the runtime's summary and its file report *)
    test "the visits reach the summary and the fixture's one file report"
      (fun () ->
        equal ~msg:"a call into the fixture" int 6 (F.sum_while 3);
        let summary = (fixture_report ()).C.summary in
        is_true ~msg:"points were visited" (summary.visited > 0);
        is_true ~msg:"visited never exceeds total"
          (summary.visited <= summary.total));
  ]

let () = exit @@ run "coverage semantics" tests
