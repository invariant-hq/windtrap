(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Covsem_fixtures is instrumented by ppx_windtrap.coverage and this suite is
   not, so a value computed here is the uninstrumented answer. The counts are
   read from the at_exit dump of a forked child, since the dump is the only
   public reading of them. *)

open Windtrap
module Coverage = Windtrap_runtime.Coverage
module Instr = Windtrap_runtime.Instr

let strf = Printf.sprintf

(* The stack bound *)

(* A lost tail call overflows only a bounded stack, and OCaml 5 bounds it at
   1 GiB by default. The dune action sets OCAMLRUNPARAM=l=1M, an 8 MiB stack,
   where a recursion [depth] frames deep overflows and a tail recursion of any
   depth does not. *)
let depth = 2_000_000
let unbounded = "the stack is not bounded: the dune action sets l=1M"
let rec non_tail n = if n = 0 then 0 else 1 + non_tail (n - 1)

let rec non_tail_map f = function
  | [] -> []
  | x :: xs -> f x :: non_tail_map f xs

(* Dumps *)

(* WINDTRAP_COVERAGE_FILE names one path, fixed at the first registration, so
   each dump is read before the next child writes it. *)
let dump = Sys.getenv "WINDTRAP_COVERAGE_FILE"

(* [dump_after f] forks a child that runs [f] and exits, which writes the
   counts of this process plus what [f] visited. The buffers are flushed
   first, or the child would print them again. *)
let dump_after f =
  Stdlib.flush_all ();
  Format.pp_print_flush Format.std_formatter ();
  Format.pp_print_flush Format.err_formatter ();
  match Unix.fork () with
  | 0 ->
      (try ignore (f ()) with _ -> ());
      exit 0
  | pid -> (
      match snd (Unix.waitpid [] pid) with
      | Unix.WEXITED code ->
          equal ~msg:"the dumping child's exit code" int 0 code
      | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
          failf "the dumping child was stopped by signal %d" signal)

(* Under --instrument-with the dump also holds the points of the windtrap core
   this suite links; every claim is about the fixture's file. *)
let is_fixture file = String.equal (Filename.basename file) "covsem_fixtures.ml"

(* [counts ()] is the fixture's [(extent, count)] per point, in the order of
   its point table. Counts rather than visited points, because a test may run
   twice in one process: the mutation loop's children are forks of the
   process that ran the dry run. *)
let counts () =
  let text = In_channel.with_open_bin dump In_channel.input_all in
  let c =
    require_ok
      ~pp:(Instr.pp_error Coverage.format)
      (Instr.start Coverage.format ~path:dump text)
  in
  ignore (Instr.read_identity c : Instr.identity option);
  let fixture = ref None in
  for _ = 1 to Instr.read_count c "files" do
    let file = Instr.read_name c "file" in
    let rows = ref [] in
    for _ = 1 to Instr.read_count c "points" do
      let start = Instr.read_nat c "start" in
      let stop = Instr.read_nat c "end" in
      rows := ((start, stop), Instr.read_nat c "count") :: !rows
    done;
    if is_fixture file then fixture := Some (List.rev !rows)
  done;
  require_some ~msg:"the dump holds the fixture's file" !fixture

(* [hits f] is [f ()] and the increment of each point [f] visited: a child
   that exits at once dumps the counts before [f], a child that runs [f]
   dumps them after, and this process then runs [f] for its value. *)
let hits f =
  dump_after ignore;
  let before = counts () in
  dump_after f;
  let after = counts () in
  let increment ((extent, before), (_, after)) =
    if after > before then Some (extent, after - before) else None
  in
  let increments = List.filter_map increment (List.combine before after) in
  let value = f () in
  (value, increments)

let extents increments = List.map fst increments
let extent = pair int int

let fixture_report () : Coverage.file_report =
  dump_after ignore;
  let t, _ = require_ok ~pp:Coverage.pp_error (Coverage.load dump) in
  let reports =
    List.filter
      (fun (r : Coverage.file_report) -> is_fixture r.file)
      (Coverage.file_reports t)
  in
  require_match ~msg:"the dump holds one report of the fixture"
    (function [ r ] -> Some r | _ -> None)
    reports

(* Before the first test, since any test may call into the fixture. *)
let at_load : Coverage.summary = (fixture_report ()).summary

(* [row (name, expected, actual)] checks [actual ()] against [expected]. *)
let row (_, expected, actual) = equal string expected (actual ())
let row_name (name, _, _) = name

(* Registration *)

let registration =
  group "Registration"
    [
      test "no point is visited before the first call" (fun () ->
          equal int 0 at_load.visited);
      test "the fixture's points register at module load" (fun () ->
          at_least int ~than:20 at_load.total);
    ]

(* Tail calls *)

let deep_recursions =
  [
    ("through a match arm", "done", fun () -> Covsem_fixtures.countdown depth);
    ( "mutually, through if branches",
      "true",
      fun () -> string_of_bool (Covsem_fixtures.even depth) );
    ( "through continuations",
      string_of_int depth,
      fun () -> string_of_int (Covsem_fixtures.cps_count depth Fun.id) );
    ( "through a pipeline",
      "0",
      fun () -> string_of_int (Covsem_fixtures.pipe_down depth) );
    ( "as the right arm of ||",
      "false",
      fun () -> string_of_bool (Covsem_fixtures.any_odd depth) );
    ( "as the right arm of &&",
      "true",
      fun () -> string_of_bool (Covsem_fixtures.all_even depth) );
    ( "in a let that is the right arm of ||",
      "true",
      fun () -> string_of_bool (Covsem_fixtures.or_let depth) );
    ( "in a match that is the right arm of ||",
      "true",
      fun () -> string_of_bool (Covsem_fixtures.or_match depth) );
    ( "in an if that is the right arm of ||",
      "true",
      fun () -> string_of_bool (Covsem_fixtures.or_if depth) );
  ]

let tail_mod_cons () =
  let xs = List.init depth Fun.id in
  raises ~msg:unbounded Stack_overflow (fun () -> non_tail_map succ xs);
  equal int depth (List.length (Covsem_fixtures.tmc_map succ xs))

let tail_calls =
  group "Tail calls"
    [
      test "a non-tail recursion as deep overflows" (fun () ->
          raises ~msg:unbounded Stack_overflow (fun () -> non_tail depth));
      cases "a tail call 2,000,000 deep stays a tail call" ~name:row_name
        deep_recursions row;
      test "a tail_mod_cons map runs in constant stack" tail_mod_cons;
    ]

(* Evaluation order *)

let branches =
  [
    (true, (10, [ "cond"; "then"; "scrutinee"; "guard"; "arm1" ]));
    (false, (20, [ "cond"; "else"; "scrutinee"; "arm2" ]));
  ]

let connectives =
  [
    ( "true || _",
      (true, [ "left" ]),
      fun () -> Covsem_fixtures.or_trace true true );
    ( "false || true",
      (true, [ "left"; "right" ]),
      fun () -> Covsem_fixtures.or_trace false true );
    ( "false || false",
      (false, [ "left"; "right" ]),
      fun () -> Covsem_fixtures.or_trace false false );
    ( "false && _",
      (false, [ "left" ]),
      fun () -> Covsem_fixtures.and_trace false true );
    ( "true && false",
      (false, [ "left"; "right" ]),
      fun () -> Covsem_fixtures.and_trace true false );
    ( "true && true",
      (true, [ "left"; "right" ]),
      fun () -> Covsem_fixtures.and_trace true true );
  ]

(* The same shape as [Covsem_fixtures.arg_order], compiled here without the
   rewriter. *)
let uninstrumented_arg_order () =
  let log = ref [] in
  let note tag value =
    log := tag :: !log;
    value
  in
  let two_args a b = (a, b) in
  let r = two_args (note "first" 1) (note "second" 2) in
  (r, List.rev !log)

let evaluation_order =
  group "Evaluation order"
    [
      cases "a branch runs its condition, one arm, its scrutinee and its guard"
        ~name:(fun (b, _) -> strf "order_witness %b" b)
        branches
        (fun (b, expected) ->
          equal
            (pair int (list string))
            expected
            (Covsem_fixtures.order_witness b));
      cases "a connective runs its right arm only when the left does not decide"
        ~name:row_name connectives (fun (_, expected, actual) ->
          equal (pair bool (list string)) expected (actual ()));
      test "arguments run in the uninstrumented order" (fun () ->
          equal
            (pair (pair int int) (list string))
            (uninstrumented_arg_order ())
            (Covsem_fixtures.arg_order ()));
      test "a sequence runs its statements left to right, once" (fun () ->
          equal (list string) [ "one"; "two" ] (Covsem_fixtures.seq_order ()));
    ]

(* Laziness *)

let lazy_body () =
  let thunk, forced, runs = Covsem_fixtures.make_thunk () in
  is_false ~msg:"forced before the first force" !forced;
  equal int 42 (Lazy.force thunk);
  equal int 42 (Lazy.force thunk);
  equal ~msg:"runs of the body" int 1 !runs

let laziness =
  group "Laziness"
    [
      test "a lazy body runs at the first force, once" lazy_body;
      test "a trivial lazy is a value, as uninstrumented" (fun () ->
          equal bool
            (Lazy.is_val (lazy 42))
            (Lazy.is_val (Covsem_fixtures.trivial ())));
    ]

(* Points *)

let lazy_points () =
  let thunk, _, _ = Covsem_fixtures.make_thunk () in
  let _, first = hits (fun () -> Lazy.force thunk) in
  let _, again = hits (fun () -> Lazy.force thunk) in
  equal ~msg:"the first force" (list int) [ 1 ] (List.map snd first);
  equal ~msg:"the second force" (list int) [] (List.map snd again)

(* [tap_ok] and [tap_raise] have one shape: a body entry, the out-edge of
   [f ()] and the entry of [f]. *)
let raising_out_edge () =
  let returned, ok =
    hits (fun () -> Covsem_fixtures.tap_ok Covsem_fixtures.ret_unit)
  in
  let outcome, raising =
    hits (fun () ->
        match Covsem_fixtures.tap_raise Covsem_fixtures.raise_unit with
        | _ -> "returned"
        | exception Exit -> "raised Exit")
  in
  equal (pair bool int) (true, 3) (returned, List.length ok);
  equal (pair string int) ("raised Exit", 2) (outcome, List.length raising)

(* [unvisited_inside f] is the extents of the points left unvisited, once [f]
   ran, inside the widest extent [f] visits: the body of the fixture function
   [f] calls. *)
let unvisited_inside f =
  let (), visited = hits f in
  let width (start, stop) = stop - start in
  let wider x y = if width y > width x then y else x in
  let start, stop =
    match extents visited with
    | [] -> fail "the fixture function visited no point"
    | x :: xs -> List.fold_left wider x xs
  in
  let inside (p : Coverage.point) =
    if start <= p.start_ofs && p.end_ofs <= stop then
      Some (p.start_ofs, p.end_ofs)
    else None
  in
  List.filter_map inside (fixture_report ()).uncovered_extents

(* Each row runs both paths of a check that raises from a call of a function
   that never returns. *)
let never_returning =
  [
    ( "failwith as the right operand of ||",
      fun () ->
        is_true (Covsem_fixtures.positive 1);
        raises (Failure "positive") (fun () -> Covsem_fixtures.positive 0) );
    ( "failwith through |> as the right operand of || out of tail position",
      fun () ->
        is_true (Covsem_fixtures.bound 1);
        raises (Failure "bound") (fun () -> Covsem_fixtures.bound 0) );
    ( "failwith through @@ in a branch out of tail position",
      fun () ->
        equal int 1 (Covsem_fixtures.applied 1);
        raises (Failure "applied") (fun () -> Covsem_fixtures.applied 0) );
  ]

(* [poke]'s body entry, the body of the method [total] and the out-edge of
   [a#total]. *)
let send_out_edge () =
  let poked, visited =
    hits (fun () -> Covsem_fixtures.poke (new Covsem_fixtures.adder))
  in
  equal (pair int int) (1, 3) (poked, List.length visited)

let function_arm () =
  let add, select = hits (fun () -> Covsem_fixtures.dispatch `Add) in
  let sum, apply = hits (fun () -> add 2 3) in
  let sum', again = hits (fun () -> add 4 5) in
  equal ~msg:"points visited by selecting the arm" int 1 (List.length select);
  equal (pair int int) (5, 1) (sum, List.length apply);
  not_equal ~msg:"the applied point is the arm's" (list extent) (extents select)
    (extents apply);
  equal (pair int (list extent)) (9, extents apply) (sum', extents again)

let points =
  group "Points"
    [
      test "forcing a lazy visits its body's point once" lazy_points;
      test "a raising application leaves its out-edge unvisited"
        raising_out_edge;
      cases "a call that never returns leaves no point of its check unvisited"
        ~name:fst never_returning (fun (_, run) ->
          equal (list extent) [] (unvisited_inside run));
      test "a send with a successor visits its out-edge" send_out_edge;
      test "an arm that is a function has a point visited when it is applied"
        function_arm;
    ]

(* Results *)

let forms =
  [
    ( "a while loop",
      "55",
      fun () -> string_of_int (Covsem_fixtures.sum_while 10) );
    ( "a while loop of no iteration",
      "0",
      fun () -> string_of_int (Covsem_fixtures.sum_while 0) );
    ("a for loop", "55", fun () -> string_of_int (Covsem_fixtures.sum_for 10));
    ( "letop bodies",
      "42",
      fun () -> string_of_int (Covsem_fixtures.letop_sum 40 2) );
    ("the first guard that holds", "small", fun () -> Covsem_fixtures.bucket 5);
    ("a later guard", "medium", fun () -> Covsem_fixtures.bucket 50);
    ("the arm after the guards", "large", fun () -> Covsem_fixtures.bucket 500);
    ( "a try handler",
      "0",
      fun () -> string_of_int (Covsem_fixtures.safe_div 7 0) );
    ("a try body", "3", fun () -> string_of_int (Covsem_fixtures.safe_div 7 2));
    ( "a pipeline in tail position",
      "12",
      fun () -> string_of_int (Covsem_fixtures.pipeline 3) );
    ( "a pipeline bound by a let",
      "7",
      fun () -> string_of_int (Covsem_fixtures.pipeline_bound 3) );
    ( "method calls",
      "6",
      fun () -> string_of_int (Covsem_fixtures.sum_object [ 1; 2; 3 ]) );
    ( "the right arm of || on an odd number",
      "true",
      fun () -> string_of_bool (Covsem_fixtures.any_odd 7) );
    ( "the right arm of && on an odd number",
      "false",
      fun () -> string_of_bool (Covsem_fixtures.all_even 3) );
    (* The call is in the body of the [try], never a tail position. *)
    ( "a try that is the right arm of ||, 1,000 deep",
      "true",
      fun () -> string_of_bool (Covsem_fixtures.or_try 1_000) );
  ]

let results =
  group "Results"
    [
      cases "an instrumented form computes the uninstrumented result"
        ~name:row_name forms row;
    ]

(* Reports *)

let report_visits () =
  equal int 6 (Covsem_fixtures.sum_while 3);
  let summary = (fixture_report ()).summary in
  greater int ~than:0 summary.visited;
  at_most int ~than:summary.total summary.visited

let reports =
  group "Reports"
    [ test "the visits reach the fixture's file report" report_visits ]

let () =
  exit
    (run "coverage semantics"
       [
         registration;
         tail_calls;
         evaluation_order;
         laziness;
         points;
         results;
         reports;
       ])
