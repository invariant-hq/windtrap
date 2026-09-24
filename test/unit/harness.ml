(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Minimal hand-rolled harness for the meta suites (test_run,
   test_ppx_runtime, test_windtrap). Those suites drive the ambient slot
   and Run.execute in-process with synthetic configs — the sanctioned
   way to test runner behavior with windtrap itself — and [execute]
   refuses to nest inside an active run, so they cannot host their own
   assertions under the windtrap runner. Everything else lives in the
   per-module suites beside them, one executable each. *)

(* Every line printed carries the suite name (set by [init]): the meta
   suites interleave with the windtrap suites under `dune runtest`, and
   an unattributed line is undebuggable there. Styling and the duration
   format come from windtrap's own machinery (Pp, Os, Report through
   the facade's Private) — one output dialect tree-wide, never a second
   implementation of it. *)
let suite = ref ""
let started = ref 0.
let failures = ref 0
let count = ref 0
let skipped = ref 0

(* The ANSI decision, captured by [init] before it clears the
   environment — the same resolution the windtrap suites make
   (WINDTRAP_COLOR, terminal status, INSIDE_DUNE). Composed here, from
   [Os.resolve_color] and the three inputs, exactly as [Report.terminal]
   composes it: nothing in windtrap resolves colour for a sink it did not
   name, and a harness that took a shortcut would be the one place where
   the decision could drift from the renderer's. *)
let ansi = ref false

let resolve_ansi () =
  let module Os = Windtrap.Private.Os in
  let module Cli = Windtrap.Private.Cli in
  (* WINDTRAP_COLOR is read as the runner reads it — through [--color]'s
     parser — so a bad value is refused here as it is everywhere. *)
  let mode =
    match Cli.color_mode () with
    | Ok mode -> mode
    | Error error ->
        prerr_endline ("harness: " ^ Cli.error_message error);
        exit 2
  in
  Os.resolve_color mode ~tty:(Os.is_tty_stdout ())
    ~inside_dune:(Os.inside_dune ()) ~term_dumb:(Os.term_dumb ())

(* The check lines' FAIL tag, in the renderer's own style. *)
let fail_tag ~ansi =
  Windtrap.Private.Report_sections.(render ~ansi [ styled `Red "FAIL" ])

let check name cond =
  incr count;
  if not cond then begin
    incr failures;
    Printf.printf "%s: %s: %s\n%!" !suite (fail_tag ~ansi:!ansi) name
  end

let check_int name ~expected ~actual =
  incr count;
  if expected <> actual then begin
    incr failures;
    Printf.printf "%s: %s: %s\n  expected: %d\n  actual:   %d\n%!" !suite
      (fail_tag ~ansi:!ansi) name expected actual
  end

let check_string name ~expected ~actual =
  incr count;
  if not (String.equal expected actual) then begin
    incr failures;
    Printf.printf "%s: %s: %s\n  expected: %S\n  actual:   %S\n%!" !suite
      (fail_tag ~ansi:!ansi) name expected actual
  end

(* A scenario that cannot run on this platform says so, and the summary
   counts it: a guard that checked nothing would pass it in silence. The
   scenario is named by its position, as in [skip_scenario ~reason __POS__]. *)
let skip_scenario ~reason (file, line, _, _) =
  incr skipped;
  Printf.printf "%s: SKIP: %s:%d (%s)\n%!" !suite (Filename.basename file) line
    reason

let expect_invalid_arg name fn =
  incr count;
  match fn () with
  | _ ->
      incr failures;
      Printf.printf "%s: %s: %s (no Invalid_argument)\n%!" !suite
        (fail_tag ~ansi:!ansi) name
  | exception Invalid_argument _ -> ()

let contains needle haystack =
  Windtrap.Private.Text.contains_substring ~pattern:needle haystack

let check_contains name ~sub haystack =
  incr count;
  if not (contains sub haystack) then begin
    incr failures;
    Printf.printf "%s: %s: %s\n  missing: %s\n  in:\n%s\n%!" !suite
      (fail_tag ~ansi:!ansi) name sub haystack
  end

(* Environment hygiene

   Empty means unset for every windtrap variable (Env's contract): the
   suites' scripted runs must not inherit ambient configuration. The list
   must name every variable the runner reads — one missing entry is one
   setting the suites silently take from whoever is running them. The
   Cli suite holds it equal to the variables of `--help` and the few
   named below. *)

let windtrap_vars =
  [
    "CI";
    "GITHUB_ACTIONS";
    "INSIDE_DUNE";
    "WINDTRAP_FILTER";
    "WINDTRAP_EXCLUDE";
    "WINDTRAP_TAG";
    "WINDTRAP_EXCLUDE_TAG";
    "WINDTRAP_SEED";
    "WINDTRAP_TIMEOUT";
    "WINDTRAP_PROP_COUNT";
    "WINDTRAP_JUNIT";
    "WINDTRAP_OUTPUT";
    "WINDTRAP_COVERAGE_FILE";
    "WINDTRAP_STREAM";
    "WINDTRAP_COLOR";
    "WINDTRAP_SHARD";
    "WINDTRAP_SLOW_THRESHOLD";
    "WINDTRAP_VERBOSE";
    "WINDTRAP_PROJECT_ROOT";
    (* The mutation mirrors are read by every windtrap run, instrumented
       or not, and one of them is meant to be set for a WHOLE project at
       once: [WINDTRAP_MUTATE_ARM=<id>] before the suite command is the
       remedy the aggregate report prints. A suite that spawns a child
       and pins its transcript byte for byte inherits that variable
       unless it is named here. *)
    "WINDTRAP_MUTATE";
    "WINDTRAP_MUTATE_ARM";
    (* Not a windtrap variable, but it turns styling off in Auto mode,
       so a developer's shell setting would reshape a pinned transcript. *)
    "NO_COLOR";
    (* Not variables of the runner at all: the Cli suite binds them to
       prove that a flag without a mirror reads no variable, and that the
       optional-value grammar reads the one its test row names. *)
    "WINDTRAP_UPDATE";
    "WINDTRAP_BAIL";
    "WINDTRAP_FAILED";
    "WINDTRAP_LIST";
    "WINDTRAP_PROBE";
  ]

(* Unset is neutral for every variable above. Cleared here rather than
   by the dune action because the suites' re-exec'd children call this
   directly, before [init]. *)
let clear_env () = List.iter (fun var -> Unix.putenv var "") windtrap_vars

let init name =
  suite := name;
  started := Unix.gettimeofday ();
  (* Resolve color before [clear_env] wipes WINDTRAP_COLOR and
     INSIDE_DUNE: the decision must match what a windtrap suite in the
     same runtest invocation decides. *)
  ansi := resolve_ansi ();
  clear_env ();
  Printexc.record_backtrace true

(* Temp roots: a scratch directory for one scenario, removed when the
   scenario ends however it ends. *)
let with_temp_root ?(prefix = "windtrap-meta-") f =
  let module Scratch = Windtrap_test_support.Scratch in
  let path = Scratch.dir prefix in
  Fun.protect ~finally:(fun () -> Scratch.remove_tree path) (fun () -> f path)

(* Summary

   One output dialect for the whole tree: the one-liner is the windtrap
   summary line with "checks" inserted — the harness counts assertions,
   a windtrap suite counts tests, and the word keeps the two countable
   ("run: 115 checks" is not 115 tests). Styling and the duration bytes
   are the renderer's own: green wraps the passed segment of a green
   run, red wraps the failed segment, exactly as Report.finish styles
   them. *)

let summary_line ?(skipped = 0) ~ansi ~suite ~failures ~count ~duration () =
  let st style s =
    Windtrap.Private.Report_sections.(render ~ansi [ styled style s ])
  in
  if count = 0 then Printf.sprintf "%s: no checks ran." suite
  else
    let counts =
      if failures = 0 then st `Green (Printf.sprintf "%d checks passed" count)
      else
        Printf.sprintf "%d checks passed, %s" (count - failures)
          (st `Red (Printf.sprintf "%d failed" failures))
    in
    (* This harness's own copy of the renderer's duration format: a meta
       harness imitating windtrap's summary line is not a reason for the
       library to publish its formatter. *)
    let ms = duration *. 1000. in
    let duration =
      if Float.round (ms *. 10.) < 100. then Printf.sprintf "%.1fms" ms
      else if Float.round ms < 1000. then Printf.sprintf "%.0fms" ms
      else Printf.sprintf "%.1fs" duration
    in
    let skipped =
      if skipped = 0 then ""
      else Printf.sprintf ", %d scenarios skipped" skipped
    in
    Printf.sprintf "%s: %s%s in %s." suite counts skipped duration

let finish () =
  let duration = Unix.gettimeofday () -. !started in
  print_endline
    (summary_line ~skipped:!skipped ~ansi:!ansi ~suite:!suite
       ~failures:!failures ~count:!count ~duration ());
  exit (if !failures = 0 then 0 else 1)
