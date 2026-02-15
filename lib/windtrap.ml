(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* ───── Submodules ───── *)

module Testable = Testable
module Tag = Tag
module Pp = Pp
module Ppx_runtime = Ppx_runtime

(* ───── Testable Constructors ───── *)

type 'a testable = 'a Testable.t

let unit = Testable.unit
let bool = Testable.bool
let int = Testable.int
let small_int = Testable.small_int
let int32 = Testable.int32
let int64 = Testable.int64
let nativeint = Testable.nativeint
let float = Testable.float
let float_rel = Testable.float_rel
let char = Testable.char
let string = Testable.string
let bytes = Testable.bytes
let option = Testable.option
let result = Testable.result
let either = Testable.either
let list = Testable.list
let array = Testable.array
let pair = Testable.pair
let triple = Testable.triple
let quad = Testable.quad
let pass = Testable.pass
let slist = Testable.slist
let of_equal = Testable.of_equal
let contramap = Testable.contramap
let seq = Testable.seq
let lazy_t = Testable.lazy_t

(* ───── Types ───── *)

type test = Test.t
type pos = string * int * int * int
type here = Lexing.position

(* ───── Test Creation ───── *)

let test = Test.test
let ftest = Test.ftest
let group = Test.group
let fgroup = Test.fgroup
let slow = Test.slow
let bracket = Test.bracket

let cases testable cases name fn =
  let pp = Testable.pp testable in
  let tests =
    List.map
      (fun case ->
        let case_name = Pp.str "%s (%a)" name pp case in
        Test.test case_name (fun () -> fn case))
      cases
  in
  Test.group name tests

let fixture create =
  let v = lazy (create ()) in
  fun () -> Lazy.force v

(* ───── Runner ───── *)

type format = Progress.mode = Compact | Verbose | Tap | Junit

let run ?quick ?bail ?fail_fast ?output_dir ?stream ?update ?snapshot_dir
    ?filter ?exclude ?failed ?list_only ?format ?junit ?seed ?timeout
    ?prop_count ?tags ?exclude_tags ?argv name tests =
  let cli = Cli.parse (Option.value ~default:Sys.argv argv) in

  (* Resolve list_only with priority: programmatic arg > CLI flag > default *)
  let list_only =
    Option.fold ~none:cli.list_only ~some:Option.some list_only
    |> Option.value ~default:false
  in
  if list_only then begin
    Runner.list_tests name tests;
    exit 0
  end;

  let config =
    Cli.resolve_config ?quick ?bail ?fail_fast ?output_dir ?stream ?update
      ?snapshot_dir ?filter ?exclude ?failed ?format ?junit ?seed ?timeout
      ?prop_count ?tags ?exclude_tags cli
  in
  let result = Runner.run ~config name tests in
  if result.failed > 0 then exit 1
  else if result.passed = 0 && result.skipped = 0 then begin
    Pp.epr "%a No tests matched the current filter.@."
      (Pp.styled `Yellow Pp.string)
      "[WARNING]";
    exit 2
  end
  else exit 0

(* ───── Assertions ───── *)

let equal = Check.equal
let not_equal = Check.not_equal
let is_true = Check.is_true
let is_false = Check.is_false
let is_some = Check.is_some
let is_none = Check.is_none
let some = Check.some
let is_ok = Check.is_ok
let is_error = Check.is_error
let ok = Check.ok
let error = Check.error
let raises = Check.raises
let raises_match = Check.raises_match
let no_raise = Check.no_raise
let fail = Check.fail
let failf = Check.failf
let skip = Failure.skip

(* ───── Snapshot Testing ───── *)

let snapshot = Snapshot.snapshot
let snapshot_pp = Snapshot.snapshot_pp
let snapshotf = Snapshot.snapshotf

(* ───── Output Testing ───── *)

let output = Expect.output
let expect = Expect.expect
let expect_exact = Expect.expect_exact
let capture = Expect.capture
let capture_exact = Expect.capture_exact

(* ───── Property-Based Testing ───── *)

module Gen = Windtrap_prop.Gen

let assume = Windtrap_prop.assume
let reject = Windtrap_prop.reject
let collect = Windtrap_prop.collect
let classify = Windtrap_prop.classify
let cover = Windtrap_prop.cover

(* Bridge between Testable (assertion-oriented) and Arbitrary (generation-
   oriented). Extracts the generator from a testable, failing early if absent. *)
let testable_to_arbitrary name testable =
  let gen =
    match Testable.gen testable with
    | Some g -> g
    | None -> invalid_arg (Pp.str "Testable for '%s' has no generator" name)
  in
  Windtrap_prop.Arbitrary.make ~gen ~print:(Pp.to_string (Testable.pp testable))

let prop ?(config = Windtrap_prop.Prop.default_config) ?pos ?tags ?timeout name
    testable law =
  let arb = testable_to_arbitrary name testable in
  Test.test ?pos ?tags ?timeout name (fun () ->
      match Windtrap_prop.Prop.check ~config arb law with
      | Windtrap_prop.Prop.Success _ -> ()
      | Windtrap_prop.Prop.Failed
          { count; seed; shrunk_counterexample; shrink_steps; _ } ->
          (* count is 0-indexed; convert to 1-indexed for human-readable output *)
          let shrink_info =
            if shrink_steps > 0 then
              Printf.sprintf ", shrunk in %d steps" shrink_steps
            else ""
          in
          let test_num = count + 1 in
          let plural = if test_num = 1 then "" else "s" in
          let msg =
            Printf.sprintf
              "Property failed after %d test%s%s (seed=%d)\n\
               Counterexample: %s\n\
               %s"
              test_num plural shrink_info seed
              (Pp.styled_string `Red shrunk_counterexample)
              (Pp.styled_string `Faint
                 (Printf.sprintf "Replay with: WINDTRAP_SEED=%d" seed))
          in
          Failure.raise_failure ?pos msg
      | Windtrap_prop.Prop.Error
          { count; seed; counterexample; exn; backtrace; _ } ->
          let test_num = count + 1 in
          let plural = if test_num = 1 then "" else "s" in
          let backtrace_str = String.trim backtrace in
          let backtrace_part =
            if backtrace_str = "" then "" else "\n" ^ backtrace_str
          in
          let msg =
            Printf.sprintf
              "Property raised exception after %d test%s (seed=%d)\n\
               Exception: %s\n\
               Counterexample: %s\n\
               %s%s"
              test_num plural seed
              (Pp.styled_string `Red (Printexc.to_string exn))
              (Pp.styled_string `Red counterexample)
              (Pp.styled_string `Faint
                 (Printf.sprintf "Replay with: WINDTRAP_SEED=%d" seed))
              backtrace_part
          in
          Failure.raise_failure ?pos msg
      | Windtrap_prop.Prop.Coverage_failed
          { count; discarded; seed; missing; collected } ->
          let coverage_lines =
            missing
            |> List.map (fun issue ->
                Printf.sprintf "- %s: required >= %.1f%%, got %.1f%% (%d/%d)"
                  issue.Windtrap_prop.Prop.label
                  issue.Windtrap_prop.Prop.required
                  issue.Windtrap_prop.Prop.actual issue.Windtrap_prop.Prop.hits
                  count)
          in
          let collected_lines =
            match collected with
            | [] -> [ "- (none)" ]
            | _ ->
                List.map
                  (fun (label, hits) ->
                    let pct =
                      if count <= 0 then 0.0
                      else float_of_int hits *. 100.0 /. float_of_int count
                    in
                    Printf.sprintf "- %s: %.1f%% (%d/%d)" label pct hits count)
                  collected
          in
          let msg =
            Printf.sprintf
              "Property coverage failed after %d successful tests (%d \
               discarded, seed=%d)\n\
               Missing coverage:\n\
               %s\n\
               Collected buckets:\n\
               %s\n\
               %s"
              count discarded seed
              (String.concat "\n" coverage_lines)
              (String.concat "\n" collected_lines)
              (Pp.styled_string `Faint
                 (Printf.sprintf "Replay with: WINDTRAP_SEED=%d" seed))
          in
          Failure.raise_failure ?pos msg
      | Windtrap_prop.Prop.Gave_up { count; discarded; seed } ->
          let msg =
            Printf.sprintf
              "Gave up after %d successful tests (%d discarded, seed=%d). Too \
               many cases discarded."
              count discarded seed
          in
          Failure.raise_failure ?pos msg)

(* Wraps an assertion-style function (unit-returning, raises on failure) into a
   boolean law for [prop]. The law returns true unless an assertion raises. *)
let prop' ?config ?pos ?tags ?timeout name testable fn =
  let law x =
    fn x;
    true
  in
  prop ?config ?pos ?tags ?timeout name testable law

let prop2 ?config ?pos ?tags ?timeout name a b law =
  prop ?config ?pos ?tags ?timeout name (Testable.pair a b) (fun (x, y) ->
      law x y)

let prop3 ?config ?pos ?tags ?timeout name a b c law =
  prop ?config ?pos ?tags ?timeout name (Testable.triple a b c)
    (fun (x, y, z) -> law x y z)
