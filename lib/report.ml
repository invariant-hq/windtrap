(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   The transcript layout (status lines, end-of-run failure blocks, slowest
   list) adapts windtrap v1's progress.ml, rebuilt over typed Failure
   payloads and Diff data — the report projects, never alters, run data.
   The workflow-command emission adapts v1's emit_github_annotation.
  ---------------------------------------------------------------------------*)

(* Not mutated. This module is part of the machinery a mutation run uses
   to judge mutants — the executor, the ambient run state, the reporting
   spine, the loop itself — so a mutant here is armed inside the process
   that is supposed to detect it. The failure mode is not a false
   survivor but a hang or a corrupted verdict. Coverage still measures
   these files; only mutation is off. Everything below — the verbs, the
   generators, the diffing, the blocks — is mutated. *)
[@@@mutate exclude_file]

module Sections = Report_sections

let spf = Printf.sprintf

(* Layout constants — illustrative, not contract. The transcript is a
   report, not a canvas: one width, so a pipe and a wide terminal are
   byte-identical, and one captured-output tail — the last [tail_lines]
   lines of the [Failure.tail_bytes] the capture kept, with the full
   log's path beside them. Neither is configurable. *)
let duration_column = 51
let columns = 80
let tail_lines = 10
let slowest_count = 5
let slowest_threshold = 5.0 (* seconds *)
let indent = "    "

(* Failure projections, re-exported *)

let headline = Sections.headline
let is_subtest_failure = Sections.is_subtest_failure
let labeled_msg = Sections.labeled_msg
let pp_failure = Sections.pp_failure
let sanitize_name = Sections.sanitize_name

let pp_duration secs =
  if secs >= 60. then
    (* Round to whole seconds first, or 119.6s prints as "1m60s". *)
    let total = int_of_float (Float.round secs) in
    spf "%dm%ds" (total / 60) (total mod 60)
  else if secs >= 1. then spf "%.2fs" secs
  else
    let ms = secs *. 1000. in
    if ms >= 10. then spf "%.0fms" ms else spf "%.1fms" ms

(* Wall-clock seconds for the summary line: three significant digits, but
   never scientific notation — [%.3g] alone prints [1e+03] from 999.5s up
   and [1e-05] below 0.1ms. *)
let pp_run_duration secs =
  if secs >= 999.5 then spf "%.0f" secs
  else if secs < 0.0001 then "0"
  else spf "%.3g" secs

(* Renderer state *)

type t = {
  out : Format.formatter;
  ansi : bool;
  verbose : bool;
  live : bool;
  slow_threshold : float; (* seconds; 0. disables the slow machinery *)
  invocation : Run.invocation;
      (* the hint context: every acceptance, replay and rerun line derives
         from the one value the facade computed at startup. *)
  mutable total_tests : int;
  mutable seen : int;
  mutable live_pending : bool;
  mutable declared : int option;
      (* tests the suite declares, before selection — the denominator the
         empty-selection message needs; [total_tests] is what survived.
         [None] until [header] runs: an embedder that renders results
         without one gets the bare wording rather than a guess. *)
  mutable selection : string option;
      (* the active selection, described by the caller (which owns the
         config), used only to say why nothing ran. *)
  mutable suite : string option;
      (* recorded by [header] so that a compact run can name itself — in
         the header it prints at the end, or in its one-line summary. *)
  mutable seed : Seed.seed option;
      (* recorded by [header] for the header line and the compact
         one-liner's seed suffix. *)
}

let create ~out ~ansi ?(live = false) (config : Run.config) =
  if
    not
      (Float.is_finite config.Run.slow_threshold
      && config.Run.slow_threshold >= 0.)
  then invalid_arg "Report.create: slow_threshold not finite and non-negative";
  {
    out;
    ansi;
    verbose = config.Run.verbose;
    live = live && ansi;
    slow_threshold = config.Run.slow_threshold;
    invocation = config.Run.invocation;
    total_tests = 0;
    seen = 0;
    live_pending = false;
    declared = None;
    selection = None;
    suite = None;
    seed = None;
  }

(* The level decides what prints; the sink only decides color ([ansi])
   and the erasable live tail ([live], TTY only) — no sink changes shape.
   Under GITHUB_ACTIONS the same transcript sits inside the ::group::
   envelope, and the live tail is explicitly off even if stdout is a TTY:
   its erase/redraw control sequences would land verbatim in the CI
   log. *)
let terminal (config : Run.config) =
  let inside_dune = Env.inside_dune () in
  let tty = Env.is_tty_stdout () in
  let ansi =
    Env.resolve_color config.Run.color ~tty ~inside_dune
      ~term_dumb:(Env.term_dumb ())
  in
  create ~out:Format.std_formatter ~ansi
    ~live:(tty && not (Env.in_github_actions ()))
    config

(* As the blocks' sink: with [ansi:false] escape codes arriving in test
   names or captured output are stripped, keeping the transcript clean. *)
let put t line =
  Pp.pf t.out "%s@\n" (if t.ansi then line else Text.strip_ansi line)

let st t style s = Pp.styled_string ~ansi:t.ansi style s

(* Erase the live tail: the whole line is cleared, and nothing is
   re-printed — compact commits nothing per test, and verbose commits
   whole lines. *)
let clear_live t =
  if t.live_pending then begin
    Pp.pf t.out "\r\027[2K";
    t.live_pending <- false
  end

let sections t l =
  clear_live t;
  Sections.print ~out:t.out ~ansi:t.ansi l

(* The transcript *)

(* The header line, printed by [header] under verbose and by [finish]
   under compact — from the recorded fields either way, so the two paths
   cannot drift. *)
let header_line t =
  match t.suite with
  | None -> ()
  | Some suite ->
      let seed_part =
        match t.seed with
        | None -> ""
        | Some s -> spf " (seed %s)" (Seed.to_string s)
      in
      put t
        (spf "%s: %d test%s%s" (sanitize_name suite) t.total_tests
           (if t.total_tests = 1 then "" else "s")
           seed_part)

let header t ~suite ~tests ?declared ?selection ~seed () =
  t.total_tests <- tests;
  t.declared <- Some (Option.value declared ~default:tests);
  t.selection <- selection;
  t.suite <- Some suite;
  t.seed <- seed;
  if t.verbose then begin
    header_line t;
    Pp.flush t.out ()
  end

let begin_test t ~path =
  if t.live then begin
    clear_live t;
    let name = sanitize_name (Test_tree.path_to_string path) in
    let counter = spf "[%d/%d]" (t.seen + 1) (max t.total_tests (t.seen + 1)) in
    let text =
      if t.verbose then spf "Running %s %s\u{2026}" counter name
      else spf "%s %s\u{2026}" counter name
    in
    let text = Text.truncate_utf8 (columns - 4) text in
    Pp.pf t.out "\r\027[2K%s" (st t `Faint ("  " ^ text));
    Pp.flush t.out ();
    t.live_pending <- true
  end

let has_missing_baseline failures =
  List.exists
    (fun (f : Failure.t) ->
      match f.kind with
      | Failure.Baseline { state = Failure.Missing _; _ } -> true
      | _ -> false)
    failures

(* "  TAG  <name><suffix>" padded so [timing] starts at a fixed column. *)
let test_line t ~tag ~style ~name ~suffix ~timing =
  let line = "  " ^ st t style tag ^ "  " ^ name ^ st t `Faint suffix in
  if timing = "" then line
  else
    let width = 4 + String.length tag + Text.length_utf8 (name ^ suffix) in
    let pad = max 2 (duration_column - width) in
    line ^ String.make pad ' ' ^ st t `Faint timing

(* The label-distribution table (one producer, two placements): the failure
   blocks always show it; a passing property's prints under verbose — the
   calibration view for collect/classify. *)
let pp_prop_stats t (s : Property.stats) =
  if s.collected <> [] then begin
    put t
      (indent
      ^ st t `Faint
          (spf "labels (%d passing case%s):" s.cases
             (if s.cases = 1 then "" else "s")));
    List.iter
      (fun (label, count) ->
        let line =
          if s.cases > 0 then
            spf "  %5.1f%%  %s"
              (100. *. float_of_int count /. float_of_int s.cases)
              label
          else spf "  %d  %s" count label
        in
        put t (indent ^ st t `Faint line))
      s.collected
  end;
  (* The failure headline already names every label that was never covered,
     so this list earns its place only by showing the ones that were —
     which is the question a reader asks next. *)
  if
    List.length s.coverage > 1
    && List.exists (fun c -> not c.Property.satisfied) s.coverage
  then begin
    put t (indent ^ "covered labels:");
    List.iter
      (fun (c : Property.cover_status) ->
        put t
          (indent
          ^ spf "  %s  %d%s" c.label c.hits
              (if c.satisfied then "" else " \u{2014} never covered")))
      s.coverage
  end

(* Record-driven classification: a failing result that did not count is an
   excused expected failure — the executor's unexpected-pass synthesis
   arrives counted, so no failure message is ever inspected. *)
let counted_failure (r : Run.result) =
  match r.outcome with
  | Failure.Fail _ -> r.counted
  | Failure.Pass | Failure.Skip _ -> false

(* Over the slow threshold and not exempt: skips never count (their
   durations are not run time), tests tagged ["slow"] are exempt
   everywhere, and a zero threshold disables the machinery entirely. *)
let over_threshold t (r : Run.result) =
  t.slow_threshold > 0. && (not r.slow_tagged)
  && (match r.outcome with Failure.Skip _ -> false | _ -> true)
  && r.duration >= t.slow_threshold

(* A pass that needed a retry: the test failed and then passed, and a
   pass on retry is never silent. *)
let flaky (r : Run.result) =
  r.attempts > 1 && match r.outcome with Failure.Pass -> true | _ -> false

let verbose_result t (r : Run.result) =
  let name = sanitize_name (Test_tree.path_to_string r.path) in
  let timing =
    pp_duration r.duration
    ^ if r.attempts > 1 then spf " (%d attempts)" r.attempts else ""
  in
  match r.outcome with
  | Failure.Pass -> (
      put t (test_line t ~tag:"PASS" ~style:`Green ~name ~suffix:"" ~timing);
      (* A passing property with collected labels prints its distribution —
         the same [pp_prop_stats] projection as the failure blocks, so the
         bytes cannot drift. XFAIL and SKIP lines print no table. *)
      match r.prop_stats with
      | Some s when s.Property.collected <> [] -> pp_prop_stats t s
      | _ -> ())
  | Failure.Fail _ when not r.counted ->
      (* An expected failure: informational and dim. *)
      let suffix =
        match r.xfail with
        | Some { Test_tree.reason = Some reason } ->
            spf " (expected failure: %s)" reason
        | Some { Test_tree.reason = None } | None -> " (expected failure)"
      in
      put t (test_line t ~tag:"XFAIL" ~style:`Faint ~name ~suffix ~timing)
  | Failure.Fail failures ->
      let suffix =
        if has_missing_baseline failures then " \u{2014} no baseline" else ""
      in
      put t (test_line t ~tag:"FAIL" ~style:`Red ~name ~suffix ~timing)
  | Failure.Skip reason ->
      let suffix = match reason with Some r -> spf " (%s)" r | None -> "" in
      put t (test_line t ~tag:"SKIP" ~style:`Yellow ~name ~suffix ~timing:"")

let result t (r : Run.result) =
  t.seen <- t.seen + 1;
  clear_live t;
  if t.verbose then begin
    verbose_result t r;
    Pp.flush t.out ()
  end

(* Run-scoped notices arrive between results (fixture releases fire after
   the last test, before [finish]). Verbose prints them as lines; compact
   prints nothing per test, so the notice is an erasable live line — a
   hanging fixture release still names itself on a terminal. *)
let note t line =
  let line = sanitize_name line in
  clear_live t;
  if t.verbose then begin
    put t line;
    Pp.flush t.out ()
  end
  else if t.live then begin
    Pp.pf t.out "%s" (st t `Faint (Text.truncate_utf8 (columns - 1) line));
    Pp.flush t.out ();
    t.live_pending <- true
  end

let observe t ~seed ~selection = function
  | Run.Run_started { suite; total; selected } ->
      header t ~suite ~tests:selected ~declared:total ?selection ~seed ()
  | Run.Test_started { path } -> begin_test t ~path
  | Run.Test_finished r -> result t r
  | Run.Fixture_release { name } -> note t ("releasing " ^ name)

(* The selection, described *)

(* Quoted for the reader, not for OCaml: [%S] would escape the [\u{203a}]
   of a test path into decimal bytes, and this string is meant to be read
   and retyped. Control characters are escaped because a raw newline in a
   filter would break the report's layout. *)
let quote s =
  let escaped =
    String.concat ""
      (List.map
         (fun c ->
           match c with
           | '"' -> "\\\""
           | '\\' -> "\\\\"
           | '\n' -> "\\n"
           | '\t' -> "\\t"
           | '\r' -> "\\r"
           | c when c < ' ' || c = '\127' -> Pp.str "\\x%02x" (Char.code c)
           | c -> String.make 1 c)
         (List.init (String.length s) (String.get s)))
  in
  "\"" ^ escaped ^ "\""

(* What narrowed the run, in the words the reader typed. Used only to
   explain an empty selection: a bare "no tests ran." names neither the
   filter that matched nothing nor how many tests there were to match. *)
let selection_description (config : Run.config) =
  let quoted values = String.concat ", " (List.map quote values) in
  let parts =
    List.concat
      [
        (match config.Run.filter with
        | Some f -> [ Pp.str "filter %s" (quote f) ]
        | None -> []);
        (match config.Run.exclude with
        | Some e -> [ Pp.str "exclusion %s" (quote e) ]
        | None -> []);
        (match config.Run.tags with
        | [] -> []
        | ts -> [ Pp.str "tag %s" (quoted ts) ]);
        (match config.Run.exclude_tags with
        | [] -> []
        | ts -> [ Pp.str "excluded tag %s" (quoted ts) ]);
        (if config.Run.failed_only then [ "--failed" ] else []);
        (match config.Run.shard with
        | Some (k, n) -> [ Pp.str "shard %d/%d" k n ]
        | None -> []);
      ]
  in
  match parts with
  | [] -> None
  | [ one ] -> Some one
  | many ->
      let last = List.nth many (List.length many - 1) in
      let rest = List.filteri (fun i _ -> i < List.length many - 1) many in
      Some (String.concat ", " rest ^ " and " ^ last)

(* Why a selection is empty, in one sentence — exit 2 either way, but
   the two causes call for different words: a suite with nothing in it is
   not a mistyped filter, and neither is a shard that legitimately drew an
   empty bucket. Naming the selection and the denominator is what turns a
   dead end into a next step. [None] when nothing narrowed a non-empty
   suite, which is a case with nothing to explain. *)
let empty_selection_reason ~declared ~selection =
  match (declared, selection) with
  | 0, _ -> Some "the suite declares none"
  | declared, Some selection ->
      Some
        (spf "%s matched none of %d test%s" selection declared
           (if declared = 1 then "" else "s"))
  | _, None -> None

(* End of run *)

let pp_tail t (tail : Failure.tail) =
  if not (tail.text = "" && tail.omitted_bytes = 0) then begin
    let lines = Text.split_lines tail.text in
    let total = List.length lines in
    let shown_count = min tail_lines total in
    let shown = List.filteri (fun i _ -> i >= total - shown_count) lines in
    let head =
      if tail.omitted_bytes > 0 then
        spf
          "\u{2500}\u{2500} captured output (last %d line%s, %d earlier bytes \
           omitted) \u{2500}\u{2500}"
          shown_count
          (if shown_count = 1 then "" else "s")
          tail.omitted_bytes
      else if shown_count < total then
        spf
          "\u{2500}\u{2500} captured output (last %d of %d lines) \
           \u{2500}\u{2500}"
          shown_count total
      else
        spf "\u{2500}\u{2500} captured output (%d line%s) \u{2500}\u{2500}"
          total
          (if total = 1 then "" else "s")
    in
    put t (indent ^ st t `Faint head);
    List.iter (fun l -> put t (indent ^ l)) shown;
    match tail.log_path with
    | Some p -> put t (indent ^ "full log: " ^ Path_ops.display_artifact p)
    | None -> ()
  end

let pp_block t (r : Run.result) =
  match r.outcome with
  | Failure.Pass | Failure.Skip _ -> ()
  | Failure.Fail failures -> (
      let name = Test_tree.path_to_string r.path in
      (* One spelling of the count, the verbose status line's: a block
         only prints for a test that failed on its last attempt, so
         "attempt N of N" was always N of N — the declared total is not
         recorded, and a number that can only equal itself says nothing
         the plain count does not. *)
      let attempts =
        if r.attempts > 1 then spf " (%d attempts)" r.attempts else ""
      in
      put t
        ("  " ^ st t `Red "FAIL" ^ "  "
        ^ st t `Bold (sanitize_name name)
        ^ st t `Faint attempts);
      (* One test can fail more than once — sibling subtests, or a body and
         its teardown, which report independently. Separate them, for the
         same reason blocks are separated: without it the only break inside
         a block falls between a failure's location and its detail, which
         reads as a boundary where there is none and hides the one that is
         actually there. *)
      List.iteri
        (fun i f ->
          if i > 0 then put t "";
          pp_failure ~ansi:t.ansi ~excerpt:true ~filter:name
            ~invocation:t.invocation t.out f)
        failures;
      (match r.prop_stats with Some s -> pp_prop_stats t s | None -> ());
      match List.find_map (fun (f : Failure.t) -> f.output_tail) failures with
      | Some tail -> pp_tail t tail
      | None -> ())

(* The summary counts REPORTED RESULTS, which is not the header's count of
   selected tests: a failing fixture release is recorded as a verdict row
   after the header printed (Run.Fixture_release), so a one-test suite
   whose release raises reads "1 test" above and "1 passed, 1 failed" below.
   The two are answering different questions — what will run, what came
   back — and the extra row names itself in the block directly above, under
   a [release] phase tag. Dropping such a row from [failed] to make the
   arithmetic close would be the real defect: the run failed, and the
   summary would then disagree with the exit code. *)
let summary_line t ~named ~passed ~failed ~skipped ~excused ~subtests ~duration
    =
  (* A compact run with no block to print printed no header: the summary
     carries the suite name — nothing may print without a name — and
     appends the root seed the header would have shown, so property runs
     stay replayable from one line. *)
  let prefix =
    match t.suite with
    | Some suite when named -> sanitize_name suite ^ ": "
    | _ -> ""
  in
  if passed + failed + skipped + excused = 0 then begin
    let reason =
      match t.declared with
      | Some declared -> empty_selection_reason ~declared ~selection:t.selection
      | None -> None
    in
    match reason with
    | None -> put t (prefix ^ "no tests ran.")
    | Some reason ->
        put t (spf "%sno tests ran: %s." prefix reason);
        if t.declared <> Some 0 then
          put t (st t `Faint "(list the suite's tests with -l)")
  end
  else begin
    let passed_part =
      if passed > 0 || (failed = 0 && skipped = 0 && excused = 0) then
        [
          (if failed = 0 then st t `Green (spf "%d passed" passed)
           else spf "%d passed" passed);
        ]
      else []
    in
    (* One convention across the transcript: green pass, red fail, yellow
       skip, faint excused. *)
    let skipped_part =
      if skipped > 0 then [ st t `Yellow (spf "%d skipped" skipped) ] else []
    in
    let excused_part =
      if excused > 0 then
        [
          st t `Faint
            (spf "%d expected failure%s" excused
               (if excused = 1 then "" else "s"));
        ]
      else []
    in
    let failed_part =
      if failed > 0 then
        let subtest_part =
          if subtests > 0 then
            spf " (%d subtest failure%s)" subtests
              (if subtests = 1 then "" else "s")
          else ""
        in
        [ st t `Red (spf "%d failed%s" failed subtest_part) ]
      else []
    in
    let seed_part =
      match t.seed with
      | Some s when named -> spf " (seed %s)" (Seed.to_string s)
      | _ -> ""
    in
    put t
      (prefix
      ^ String.concat ", "
          (passed_part @ skipped_part @ excused_part @ failed_part)
      ^ spf " in %ss%s." (pp_run_duration duration) seed_part)
  end

(* The slow warnings: a labelled block in the shape of the failures block
   and the slowest list, rather than bare lines at column zero — a heading
   carrying the count, then one indented entry per test with the duration
   in a right-aligned leading column, so the paths line up and the
   durations can be read down. Slowest first: with several over the
   threshold, the top one is the one worth acting on, and execution order
   says nothing a reader wants here. The hint names the interface the run
   actually has, like every other hint. *)
let slow_warnings t slow_results =
  let rendered =
    List.map
      (fun (r : Run.result) ->
        (pp_duration r.duration, sanitize_name (Test_tree.path_to_string r.path)))
      (List.sort
         (fun (a : Run.result) (b : Run.result) ->
           Float.compare b.duration a.duration)
         slow_results)
  in
  (* [pp_duration] is ASCII, so byte length is display width. *)
  let width =
    List.fold_left (fun w (d, _) -> max w (String.length d)) 0 rendered
  in
  let warn s = put t (st t `Faint (st t `Yellow s)) in
  warn (spf "slow tests (%d):" (List.length rendered));
  List.iter (fun (d, path) -> warn (spf "  %*s  %s" width d path)) rendered;
  put t
    (st t `Faint
       (match t.invocation with
       | `Exe _ ->
           "(exempt with the \"slow\" tag, or raise --slow-threshold SECONDS)"
       | `Mirrors ->
           "(exempt with the \"slow\" tag, or raise WINDTRAP_SLOW_THRESHOLD)"))

(* The flaky block, in the slow block's shape: a test that failed and then
   passed is a defect observed, and the tool surfaces what it saw. Run
   order, one line per test. *)
let flaky_block t flaky_results =
  let warn s = put t (st t `Faint (st t `Yellow s)) in
  warn (spf "flaky tests (%d):" (List.length flaky_results));
  List.iter
    (fun (r : Run.result) ->
      warn
        (spf "  passed on attempt %d  %s" r.attempts
           (sanitize_name (Test_tree.path_to_string r.path))))
    flaky_results

let slowest t results =
  let timed =
    List.filter
      (fun (r : Run.result) ->
        match r.outcome with Failure.Skip _ -> false | _ -> true)
      results
  in
  let total =
    List.fold_left (fun acc (r : Run.result) -> acc +. r.duration) 0. timed
  in
  if total >= slowest_threshold && List.length timed >= slowest_count then begin
    let rendered =
      List.map
        (fun (r : Run.result) ->
          ( pp_duration r.duration,
            sanitize_name (Test_tree.path_to_string r.path) ))
        (List.filteri
           (fun i _ -> i < slowest_count)
           (List.sort
              (fun (a : Run.result) (b : Run.result) ->
                Float.compare b.duration a.duration)
              timed))
    in
    (* Same duration column as the slow-tests block, which prints a few
       lines above this one in a verbose run: two lists of (duration, path)
       that align differently read as a mistake. *)
    let width =
      List.fold_left (fun w (d, _) -> max w (String.length d)) 0 rendered
    in
    put t "";
    put t (st t `Faint "slowest tests:");
    List.iter
      (fun (d, path) -> put t (st t `Faint (spf "  %*s  %s" width d path)))
      rendered
  end

type coverage_summary = { visited : int; total : int }

let finish t ?coverage ~results ~duration () =
  clear_live t;
  let failed_results, excused_results =
    List.partition counted_failure
      (List.filter
         (fun (r : Run.result) ->
           match r.outcome with Failure.Fail _ -> true | _ -> false)
         results)
  in
  let count p = List.length (List.filter p results) in
  let passed =
    count (fun (r : Run.result) ->
        match r.outcome with Failure.Pass -> true | _ -> false)
  in
  let skipped =
    count (fun (r : Run.result) ->
        match r.outcome with Failure.Skip _ -> true | _ -> false)
  in
  let failed = List.length failed_results in
  let subtests =
    List.fold_left
      (fun acc (r : Run.result) ->
        match r.outcome with
        | Failure.Fail fs ->
            acc + List.length (List.filter is_subtest_failure fs)
        | _ -> acc)
      0 failed_results
  in
  let slow_results = List.filter (over_threshold t) results in
  let flaky_results = List.filter flaky results in
  let excused = List.length excused_results in
  (* The noteworthy rule, a pure function of the results: the header
     prints iff there is a block to print. *)
  let noteworthy = failed > 0 || slow_results <> [] || flaky_results <> [] in
  if (not t.verbose) && not noteworthy then
    (* Green and healthy: the whole transcript is the one named summary
       line. *)
    summary_line t ~named:true ~passed ~failed ~skipped ~excused ~subtests
      ~duration
  else begin
    if not t.verbose then header_line t;
    if failed > 0 then begin
      sections t [ Sections.Rule (Some (spf "failures (%d)" failed)) ];
      (* One blank line between blocks, none inside the run: a block is the
         unit a reader scans for, and the only other break in this region —
         between a block's location and its detail — must not read as loud
         as the boundary between two failures. *)
      List.iteri
        (fun i r ->
          if i > 0 then put t "";
          pp_block t r)
        failed_results;
      sections t [ Sections.Rule None ];
      put t ""
    end;
    if slow_results <> [] then begin
      slow_warnings t slow_results;
      put t ""
    end;
    if flaky_results <> [] then begin
      flaky_block t flaky_results;
      put t ""
    end;
    summary_line t ~named:false ~passed ~failed ~skipped ~excused ~subtests
      ~duration;
    (* No rerun hint. [--failed] is an optimization, not a step a reader has
       to take, and a suite is meant to be fast enough that rerunning all of
       it costs nothing — so advertising the flag under every failing run is
       an ad, not a report. The acceptance commands stay: those name a verb
       nobody can guess (Law 3), which is a different thing entirely. *)
    (* Diagnosis, not signal: the slowest list is verbose-only. *)
    if t.verbose then slowest t results
  end;
  (* An in-process number is always one executable's view of the code it
     links; the project number is the merge, so the line points at the
     aggregate rather than posing as the total. *)
  (match coverage with
  | Some { visited; total } ->
      sections t
        [
          Sections.Line
            (Sections.coverage_line ~hint:"project: windtrap coverage" ~visited
               ~total ());
        ]
  | None -> ());
  Pp.flush t.out ()

(* The baseline report *)

(* Printed after [finish], so the transcript is settled: plain lines on
   the sink. *)
let report_baselines t run =
  let baselines = Run.baselines run in
  let verb =
    match Baseline.mode baselines with
    | Baseline.Update -> "accepted"
    | Baseline.Corrected | Baseline.Check -> "wrote"
  in
  List.iter
    (fun { Baseline.path; literals } ->
      let count =
        match literals with
        | 0 -> ""
        | 1 -> " (1 expectation)"
        | n -> spf " (%d expectations)" n
      in
      Format.fprintf t.out "%s %s%s@." verb (Path_ops.display path) count)
    (Baseline.writes baselines);
  List.iter
    (fun (path, reason) ->
      Format.fprintf t.out "could not write %s: %s@." (Path_ops.display path)
        reason)
    (Baseline.refusals baselines)

(* The GitHub Actions envelope *)

(* Workflow-command data encoding: '%' first, then CR and LF. *)
let escape_data s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '%' -> Buffer.add_string buf "%25"
      | '\r' -> Buffer.add_string buf "%0D"
      | '\n' -> Buffer.add_string buf "%0A"
      | c -> Buffer.add_char buf c)
    (Text.strip_ansi s);
  Buffer.contents buf

(* Property values additionally encode the command delimiters. *)
let escape_property s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | ':' -> Buffer.add_string buf "%3A"
      | ',' -> Buffer.add_string buf "%2C"
      | c -> Buffer.add_char buf c)
    (escape_data s);
  Buffer.contents buf

let group_start name = spf "::group::%s\n" (escape_data name)
let group_end = "::endgroup::\n"

let rec drop_trailing_newlines s =
  let len = String.length s in
  if len > 0 && s.[len - 1] = '\n' then
    drop_trailing_newlines (String.sub s 0 (len - 1))
  else s

let annotation ?(invocation = `Mirrors) ~path (f : Failure.t) =
  let path_string = Test_tree.path_to_string path in
  let location =
    match f.loc with
    | Some { Loc.file; line; _ } ->
        spf "file=%s,line=%d," (escape_property file) line
    | None -> ""
  in
  let title = escape_property (spf "Test failure: %s" path_string) in
  let message =
    drop_trailing_newlines
      (Pp.str "%a"
         (fun ppf f ->
           pp_failure ~ansi:false ~filter:path_string ~invocation ppf f)
         f)
  in
  spf "::error %stitle=%s::%s\n" location title (escape_data message)

let annotations ?invocation results =
  let buf = Buffer.create 256 in
  List.iter
    (fun (r : Run.result) ->
      (* Counted failures only (the record's bit): an excused expected
         failure did not fail the run and annotates nothing. *)
      match r.outcome with
      | Failure.Fail fs when r.counted ->
          List.iter
            (fun f ->
              Buffer.add_string buf (annotation ?invocation ~path:r.path f))
            fs
      | Failure.Fail _ | Failure.Pass | Failure.Skip _ -> ())
    results;
  Buffer.contents buf

(* The coverage seam *)

(* WINDTRAP_COVERAGE_ONLY: the source prefixes this run's number is about.
   Applied here, at the one seam, and not to the .coverage dump, which
   the runtime writes whole because it is what `windtrap coverage`
   merges. Prefix matching, not globbing: the registry's file names are
   the paths the instrumenter recorded, and a prefix is the one predicate
   a reader can apply by eye. *)
let coverage_scope () =
  match Env.coverage_only () with
  | [] -> Fun.id
  | prefixes ->
      Windtrap_runtime.Coverage.filter (fun file ->
          List.exists (fun prefix -> String.starts_with ~prefix file) prefixes)

let snapshot_coverage () =
  let collection = coverage_scope () (Windtrap_runtime.Coverage.snapshot ()) in
  if Windtrap_runtime.Coverage.is_empty collection then None
  else
    let s = Windtrap_runtime.Coverage.summary collection in
    Some { visited = s.visited; total = s.total }

(* Mutation lines *)

(* The lines an armed run is owed. The announcement prints
   unconditionally, because Law 16(b) makes it the guarantee that a run
   whose output does not say so has no mutant armed. *)

let mutation_armed t ~id ~before ~after =
  clear_live t;
  put t (spf "mutant %s armed: %s \u{2192} %s" (st t `Bold id) before after)

let mutation_killed t =
  clear_live t;
  put t (st t `Green "mutant killed.")

let mutation_survived t ~hits =
  clear_live t;
  put t
    (st t `Red
       (spf
          "mutant survived: the armed site was evaluated %d time(s) and no \
           test failed."
          hits))

let mutation_not_evaluated t =
  clear_live t;
  put t (st t `Yellow "mutant not evaluated: no selected test ran the site.")

let mutation_not_saved t =
  clear_live t;
  put t
    (st t `Yellow
       "verdicts not saved: this run's selection narrows the suite, and a \
        partial run's verdicts would stand in the project merge as the whole.")

let mutation_report t m =
  sections t (Sections.mutation_report ~invocation:t.invocation m)

(* Running, reported *)

(* The one composition: build the renderer and the observer, open the
   GitHub envelope, run the suite, and project the run into every sink.
   The [Error] arm prints the startup message here and hands the error
   back; everything after the report — the focus warning, the exit code —
   is the facade's own. *)
let run ?(on_event = fun (_ : Run.event) -> ()) ~suite (config : Run.config)
    tests =
  let renderer = terminal config in
  (* Header-seed policy: the root seed iff the suite declares property
     tests — selection never changes it, so the token stays stable across
     filtered runs. *)
  let seed =
    if
      List.exists
        (fun case -> Tag.mem Tag.prop case.Test_tree.tags)
        (Test_tree.flatten tests)
    then Some config.Run.seed
    else None
  in
  (* [Run.execute]'s [?on_event] has one slot and the transcript owns it.
     A second subscriber composes here rather than replacing it, in a
     fixed order — transcript first — so no caller can drop the run's own
     output by subscribing, and none can reorder it. *)
  let transcript =
    observe renderer ~seed ~selection:(selection_description config)
  in
  let on_event event =
    transcript event;
    on_event event
  in
  let github = config.Run.github in
  if github then print_string (group_start suite);
  match Run.execute ~on_event config ~suite tests with
  | Error error ->
      if github then print_string group_end;
      prerr_endline (Run.startup_message error);
      Error error
  | Ok outcome ->
      (* The one recorded list: test rows plus the executor's verdict rows
         (fixture-release failures). Every sink projects it, so a verdict
         that sets the exit code is always visible in the report. *)
      let results = Run.results outcome.Run.run in
      let coverage =
        if config.Run.coverage then snapshot_coverage () else None
      in
      finish renderer ?coverage ~results ~duration:outcome.Run.duration ();
      report_baselines renderer outcome.Run.run;
      if github then begin
        print_string group_end;
        (* After the envelope closes, deliberately: an ::error:: block
           written inside it folds away with the transcript, and the
           annotations are the part a reviewer must see without
           unfolding anything. *)
        print_string (annotations ~invocation:config.Run.invocation results)
      end;
      (* Last, so a report is written from the rows the terminal has
         already shown. *)
      Option.iter
        (Report_junit.write ~invocation:config.Run.invocation ~suite
           ~duration:outcome.Run.duration ~results)
        config.Run.junit;
      Format.pp_print_flush Format.std_formatter ();
      Format.pp_print_flush Format.err_formatter ();
      Ok outcome
