(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Minimal hand-rolled harness for the meta suites (test_run, test_runner,
   test_ppx_runtime, test_windtrap). Those suites drive the ambient slot
   and Runner.execute in-process with synthetic configs — the sanctioned
   way to test runner behavior with windtrap itself — and [execute]
   refuses to nest inside an active run, so they cannot host their own
   assertions under the windtrap runner. Everything else lives in the
   aggregated main.exe suite. *)

(* Every line printed carries the suite name (set by [init]): the meta
   suites interleave with the windtrap suites under `dune runtest`, and
   an unattributed line is undebuggable there. Styling and the duration
   format come from windtrap's own machinery (Pp, Env, Render through
   the facade's Private) — one output dialect tree-wide, never a second
   implementation of it. *)
let suite = ref ""
let started = ref 0.
let failures = ref 0
let count = ref 0

(* The ANSI decision, captured by [init] before it clears the
   environment — the same resolution the windtrap suites make
   (WINDTRAP_COLOR, terminal status, INSIDE_DUNE). *)
let ansi = ref false

(* The check lines' FAIL tag, ansi-explicit (like [summary_line]) so
   test_render's dialect test can pin its bytes against the renderer's
   own FAIL header. *)
let fail_tag ~ansi = Windtrap.Private.Pp.styled_string ~ansi `Red "FAIL"

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

(* ───── Environment hygiene ─────

   Empty means unset for every windtrap variable (Env's contract): the
   suites' scripted runs must not inherit ambient configuration. *)

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
    "WINDTRAP_STREAM";
    "WINDTRAP_UPDATE";
    "WINDTRAP_PRUNE";
    "WINDTRAP_ALLOW_FOCUS";
    "WINDTRAP_COLUMNS";
    "WINDTRAP_TAIL_ERRORS";
    "WINDTRAP_COLOR";
    "WINDTRAP_SHARD";
    "WINDTRAP_SLOW_THRESHOLD";
    "WINDTRAP_QUIET";
    "WINDTRAP_VERBOSE";
    "WINDTRAP_PROJECT_ROOT";
  ]

let clear_env () = List.iter (fun var -> Unix.putenv var "") windtrap_vars

let init name =
  suite := name;
  started := Unix.gettimeofday ();
  (* Resolve color before [clear_env] wipes WINDTRAP_COLOR and
     INSIDE_DUNE: the decision must match what a windtrap suite in the
     same runtest invocation decides. *)
  ansi := Windtrap.Private.Env.use_color_stdout ();
  clear_env ();
  Printexc.record_backtrace true

(* ───── Temp roots ───── *)

let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Array.iter
        (fun name -> remove_tree (Filename.concat path name))
        (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path

let with_temp_root ?(prefix = "windtrap-meta-") f =
  let path = Filename.temp_file prefix ".dir" in
  Unix.unlink path;
  Unix.mkdir path 0o700;
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> f path)

(* ───── Summary ─────

   One output dialect for the whole tree: the one-liner is the windtrap
   summary line with "checks" inserted — the harness counts assertions,
   a windtrap suite counts tests, and the word keeps the two countable
   ("run: 115 checks" is not 115 tests). Styling and the duration bytes
   are the renderer's own: green wraps the passed segment of a green
   run, red wraps the failed segment, exactly as Render.finish styles
   them. Pinned against the renderer by test_render's dialect test. *)

let summary_line ~ansi ~suite ~failures ~count ~duration =
  let st style s = Windtrap.Private.Pp.styled_string ~ansi style s in
  if count = 0 then Printf.sprintf "%s: no checks ran." suite
  else
    let counts =
      if failures = 0 then st `Green (Printf.sprintf "%d checks passed" count)
      else
        Printf.sprintf "%d checks passed, %s" (count - failures)
          (st `Red (Printf.sprintf "%d failed" failures))
    in
    Printf.sprintf "%s: %s in %ss." suite counts
      (Windtrap.Private.Render.pp_run_duration duration)

let finish () =
  let duration = Unix.gettimeofday () -. !started in
  print_endline
    (summary_line ~ansi:!ansi ~suite:!suite ~failures:!failures ~count:!count
       ~duration);
  exit (if !failures = 0 then 0 else 1)
