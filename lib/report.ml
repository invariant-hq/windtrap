(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   The transcript layout (status lines, failure blocks) adapts windtrap
   v1's progress.ml, rebuilt over typed Failure payloads and Diff data:
   the report projects, never alters, run data.
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

(* Layout constants. [columns] is a cap that [report.mli] states. The
   transcript is a report, not a canvas: one width, so a pipe and a wide
   terminal are byte-identical, and one captured-output tail, the last
   [Sections.max_lines] lines of the [Failure.tail_bytes] the capture kept,
   with the full log's path beside them. Neither is configurable. *)
let duration_column = 51
let columns = 80
let rule_width = 58 (* of the rules around a compact run's failures *)
let indent = "    "

(* Failure projections, re-exported *)

let headline = Sections.headline
let is_subtest_failure = Sections.is_subtest_failure
let labeled_msg = Sections.labeled_msg
let pp_failure = Sections.pp_failure
let sanitize_name = Sections.sanitize_name

(* A measured duration, the one format of every slot. Rounded to the
   precision it prints at before its unit is chosen, so 9.96ms is [10ms]
   and never [10.0ms]. *)
let pp_duration secs =
  let ms = secs *. 1000. in
  if Float.round (ms *. 10.) < 100. then spf "%.1fms" ms
  else if Float.round ms < 1000. then spf "%.0fms" ms
  else spf "%.1fs" secs

(* A configured number of seconds, as its user wrote it: the shortest
   decimal that reads back the same, never an exponent. *)
let pp_configured secs =
  let rec shortest digits =
    let s = spf "%.*f" digits secs in
    if digits >= 17 || float_of_string s = secs then s else shortest (digits + 1)
  in
  shortest 0

(* Renderer state *)

type t = {
  out : Format.formatter;
  ansi : bool;
  verbose : bool;
  stream : bool;
  live : bool;
  slow_threshold : float; (* seconds; 0. disables the slow machinery *)
  invocation : Run.invocation;
      (* the hint context: every [accept:] and [replay:] line derives from
         the one value the facade computed at startup. *)
  armed : string option; (* the armed mutant's identifier *)
  config : Run.config;
      (* the selection a mutation loop's [reproduce:] command restates *)
  mutable total_tests : int;
  mutable seen : int;
  mutable live_pending : bool;
  mutable spaced : bool;
      (* the last line committed is blank: a verbose block has just closed *)
  mutable header_printed : bool;
  mutable blocks : int;
      (* failure blocks committed: the first [blocks] counted failures of
         the results [finish] is given, in their order. *)
  mutable survivors : int; (* survivor blocks a mutation loop committed *)
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
         the header it prints before its first section, or in its one-line
         summary. *)
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
    stream = config.Run.stream;
    (* A streamed test's bytes would land on the tail before its erasure. *)
    live = live && ansi && not config.Run.stream;
    slow_threshold = config.Run.slow_threshold;
    invocation = config.Run.invocation;
    armed =
      (match config.Run.mutation with
      | Run.Armed id -> Some id
      | Run.No_mutation | Run.Loop _ -> None);
    config;
    total_tests = 0;
    seen = 0;
    live_pending = false;
    spaced = false;
    header_printed = false;
    blocks = 0;
    survivors = 0;
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
  let inside_dune = Os.inside_dune () in
  let tty = Os.is_tty_stdout () in
  let ansi =
    Os.resolve_color config.Run.color ~tty ~inside_dune
      ~term_dumb:(Os.term_dumb ())
  in
  create ~out:Format.std_formatter ~ansi
    ~live:(tty && not (Os.in_github_actions ()))
    config

(* As the blocks' sink: with [ansi:false] escape codes arriving in test
   names or captured output are stripped, keeping the transcript clean. *)
let put t line =
  Pp.pf t.out "%s@\n" (if t.ansi then line else Text.strip_ansi line)

let st t style s = Pp.styled_string ~ansi:t.ansi style s

(* A streamed test writes past [t.out]: through C stdio, or straight to
   descriptor 1. What it wrote is forced out before the report writes on. *)
let sync t = if t.stream then Capture.drain ()

(* Erases the live tail. Nothing is re-printed: every committed write is
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

(* The header line, committed once: by [header] under verbose, before the
   first block or section under compact. *)
let commit_header t =
  match t.suite with
  | Some suite when not t.header_printed ->
      t.header_printed <- true;
      put t
        (spf "%s: %d test%s%s" (sanitize_name suite) t.total_tests
           (if t.total_tests = 1 then "" else "s")
           (match t.seed with
           | None -> ""
           | Some s -> spf " (seed %s)" (Seed.to_string s)))
  | Some _ | None -> ()

let header t ~suite ~tests ?declared ?selection ~seed () =
  t.total_tests <- tests;
  t.declared <- Some (Option.value declared ~default:tests);
  t.selection <- selection;
  t.suite <- Some suite;
  t.seed <- seed;
  if t.verbose then begin
    commit_header t;
    Pp.flush t.out ()
  end

(* Draws the live tail over the previous one. *)
let draw_live t text =
  if t.live then begin
    clear_live t;
    let text = Text.truncate_utf8 (columns - 4) text in
    Pp.pf t.out "\r\027[2K%s" (st t `Faint ("  " ^ text));
    Pp.flush t.out ();
    t.live_pending <- true
  end

(* The denominator follows the count when more results arrive than [header]
   announced, so the counter never reads [5/4]. *)
let begin_test t ~path =
  if t.live then begin
    let name = sanitize_name (Test_tree.path_to_string path) in
    let counter = spf "[%d/%d]" (t.seen + 1) (max t.total_tests (t.seen + 1)) in
    draw_live t
      (if t.verbose then spf "Running %s %s\u{2026}" counter name
       else spf "%s %s\u{2026}" counter name)
  end

let has_missing_baseline failures =
  List.exists
    (fun (f : Failure.t) ->
      match f.kind with
      | Failure.Baseline { state = Failure.Missing _; _ } -> true
      | _ -> false)
    failures

(* "  TAG  <name> (<qualifiers>)" padded so [timing] starts at a fixed
   column. [title] is set on the row that is a block's title. *)
let test_line ?(title = false) t ~tag ~style ~name ~qualifiers ~timing =
  let qualifier =
    match qualifiers with
    | [] -> ""
    | parts -> spf "(%s)" (String.concat ", " parts)
  in
  let line =
    "  " ^ st t style tag ^ "  "
    ^ (if title then st t `Bold name else name)
    ^ if qualifier = "" then "" else " " ^ st t `Faint qualifier
  in
  if timing = "" then line
  else
    let width =
      4 + String.length tag + Text.length_utf8 name
      + if qualifier = "" then 0 else 1 + Text.length_utf8 qualifier
    in
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
              (if c.satisfied then "" else "  never covered")))
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

(* Failure blocks *)

let pp_tail t (tail : Failure.tail) =
  if not (tail.text = "" && tail.omitted_bytes = 0) then begin
    let lines = Text.split_lines tail.text in
    let total = List.length lines in
    let shown_count = min Sections.max_lines total in
    let dropped = List.filteri (fun i _ -> i < total - shown_count) lines in
    let shown = List.filteri (fun i _ -> i >= total - shown_count) lines in
    let head =
      if tail.omitted_bytes > 0 then
        (* Every byte before the first line shown: what the capture cut and
           the kept lines the cap drops, each with its newline. *)
        spf "captured output (last %d line%s, %d earlier bytes omitted):"
          shown_count
          (if shown_count = 1 then "" else "s")
          (List.fold_left
             (fun bytes l -> bytes + String.length l + 1)
             tail.omitted_bytes dropped)
      else if shown_count < total then
        spf "captured output (last %d of %d lines):" shown_count total
      else
        spf "captured output (%d line%s):" total (if total = 1 then "" else "s")
    in
    put t (indent ^ st t `Faint head);
    List.iter (fun l -> put t (indent ^ "  " ^ l)) shown;
    match tail.log_path with
    | Some p ->
        put t (indent ^ st t `Faint ("full log: " ^ Os.display_artifact p))
    | None -> ()
  end

(* A block's lines under its title: an entry per failure (sibling
   subtests, or a body and its teardown, fail independently) with a blank
   line between two, the label table, the captured tail, and last the
   hints, once for the whole test. *)
let pp_body t (r : Run.result) failures =
  List.iteri
    (fun i f ->
      if i > 0 then put t "";
      pp_failure ~ansi:t.ansi ~excerpt:true ~hints:false t.out f)
    failures;
  (match r.prop_stats with Some s -> pp_prop_stats t s | None -> ());
  Option.iter (pp_tail t)
    (List.find_map (fun (f : Failure.t) -> f.output_tail) failures);
  (* A fixture release belongs to no test: no filter selects it. *)
  let filter =
    match r.subject with
    | Run.Test -> Some (Test_tree.path_to_string r.path)
    | Run.Fixture_release -> None
  in
  List.iter
    (fun hint -> put t (indent ^ hint))
    (Sections.hints ?armed:t.armed ~invocation:t.invocation ~filter failures)

let armed_qualifier t =
  match t.armed with Some _ -> [ "mutant armed" ] | None -> []

let pp_block t (r : Run.result) =
  match r.outcome with
  | Failure.Pass | Failure.Skip _ -> ()
  | Failure.Fail failures ->
      let attempts =
        if r.attempts > 1 then [ spf "%d attempts" r.attempts ] else []
      in
      put t
        (test_line ~title:true t ~tag:"FAIL" ~style:`Red
           ~name:(sanitize_name (Test_tree.path_to_string r.path))
           ~qualifiers:(attempts @ armed_qualifier t)
           ~timing:"");
      pp_body t r failures

(* A verbose row. A counted failure's row is its block's title: the
   block's lines follow it, and a blank line closes it. *)
let verbose_result t (r : Run.result) =
  let name = sanitize_name (Test_tree.path_to_string r.path) in
  let timing =
    pp_duration r.duration
    ^ if r.attempts > 1 then spf " (%d attempts)" r.attempts else ""
  in
  match r.outcome with
  | Failure.Pass -> (
      put t (test_line t ~tag:"PASS" ~style:`Green ~name ~qualifiers:[] ~timing);
      (* A passing property with collected labels prints its distribution —
         the same [pp_prop_stats] projection as the failure blocks, so the
         bytes cannot drift. XFAIL and SKIP lines print no table. *)
      match r.prop_stats with
      | Some s when s.Property.collected <> [] -> pp_prop_stats t s
      | _ -> ())
  | Failure.Fail _ when not r.counted ->
      (* An expected failure: informational and dim. *)
      let expected =
        match r.xfail with
        | Some { Test_tree.reason = Some reason } ->
            "expected failure: " ^ sanitize_name reason
        | Some { Test_tree.reason = None } | None -> "expected failure"
      in
      put t
        (test_line t ~tag:"XFAIL" ~style:`Faint ~name ~qualifiers:[ expected ]
           ~timing)
  | Failure.Fail failures ->
      let qualifiers =
        (if has_missing_baseline failures then [ "no baseline" ] else [])
        @ armed_qualifier t
      in
      put t
        (test_line ~title:true t ~tag:"FAIL" ~style:`Red ~name ~qualifiers
           ~timing);
      pp_body t r failures;
      put t ""
  | Failure.Skip reason ->
      put t
        (test_line t ~tag:"SKIP" ~style:`Yellow ~name
           ~qualifiers:(Option.to_list (Option.map sanitize_name reason))
           ~timing:"")

(* A block is committed when its test finishes, so a run that dies has
   printed what it knew. Compact precedes the first by the header and the
   section's opening rule, and [finish] closes the section; verbose has no
   section, its blocks sit under their rows. *)
let commit_block t r =
  clear_live t;
  commit_header t;
  if t.verbose then begin
    verbose_result t r;
    t.spaced <- true
  end
  else begin
    put t
      (if t.blocks = 0 then
         st t `Faint (Sections.rule ~width:rule_width (Some "failures"))
       else "");
    pp_block t r
  end;
  t.blocks <- t.blocks + 1;
  Pp.flush t.out ()

let result t (r : Run.result) =
  sync t;
  t.seen <- t.seen + 1;
  if counted_failure r then commit_block t r
  else if t.verbose then begin
    clear_live t;
    verbose_result t r;
    t.spaced <- false;
    Pp.flush t.out ()
  end
  else clear_live t

(* Run-scoped notices arrive between results (fixture releases fire after
   the last test, before [finish]). Verbose prints them as lines; compact
   prints nothing per test, so the notice is an erasable live line — a
   hanging fixture release still names itself on a terminal. *)
let note t line =
  sync t;
  let line = sanitize_name line in
  clear_live t;
  if t.verbose then begin
    put t line;
    t.spaced <- false;
    Pp.flush t.out ()
  end
  else if t.live then begin
    Pp.pf t.out "%s" (st t `Faint (Text.truncate_utf8 (columns - 1) line));
    Pp.flush t.out ();
    t.live_pending <- true
  end

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

(* The summary's terms. They count the results reported, not the header's
   selected tests: a fixture release that raised is a verdict row recorded
   after the header ([Run.Fixture_release]), and it counts as failed so that
   the summary agrees with the exit code. *)
type summary = {
  passed : int;
  flaky : int;
  skipped : int;
  excused : int;
  failed : int;
  subtests : int;
  not_run : int; (* selected tests the run stopped before *)
  corrections : int; (* files *)
  accepted : bool; (* whether the corrections replaced their files *)
}

let plural n = if n = 1 then "" else "s"

(* A run that printed no header names itself here and carries the seed the
   header would have shown: a green property run stays replayable from its
   one line. *)
let summary_line t (c : summary) ~duration =
  let named = not t.header_printed in
  let prefix =
    match t.suite with
    | Some suite when named -> sanitize_name suite ^ ": "
    | Some _ | None -> ""
  in
  if c.passed + c.failed + c.skipped + c.excused + c.not_run = 0 then begin
    let reason =
      match t.declared with
      | Some declared -> empty_selection_reason ~declared ~selection:t.selection
      | None -> None
    in
    match reason with
    | None -> put t (prefix ^ "no tests ran.")
    | Some reason -> (
        put t (spf "%sno tests ran: %s." prefix reason);
        (* A build action has no launcher to restate: it names the flag. A
           suite that declares nothing has nothing to list. *)
        if t.declared <> Some 0 then
          match t.invocation with
          | `Exe launcher -> put t (spf "list: %s -l" launcher)
          | `Mirrors -> put t (st t `Faint "(list the suite's tests with -l)"))
  end
  else begin
    let term n text = if n > 0 then [ text ] else [] in
    let terms =
      List.concat
        [
          term c.passed
            ((if c.failed = 0 then st t `Green else Fun.id)
               (spf "%d passed" c.passed)
            ^
            if c.flaky > 0 then " " ^ st t `Yellow (spf "(%d flaky)" c.flaky)
            else "");
          term c.skipped (st t `Yellow (spf "%d skipped" c.skipped));
          term c.excused
            (st t `Faint
               (spf "%d expected failure%s" c.excused (plural c.excused)));
          term c.failed
            (st t `Red
               (spf "%d failed%s" c.failed
                  (if c.subtests > 0 then
                     spf " (%d subtest failure%s)" c.subtests
                       (plural c.subtests)
                   else "")));
          term c.not_run (spf "%d not run" c.not_run);
          term c.corrections
            (spf "%d correction%s %s" c.corrections (plural c.corrections)
               (if c.accepted then "accepted" else "written"));
        ]
    in
    let seed =
      match t.seed with
      | Some s when named -> spf " (seed %s)" (Seed.to_string s)
      | Some _ | None -> ""
    in
    put t
      (spf "%s%s in %s%s." prefix (String.concat ", " terms)
         (pp_duration duration) seed)
  end

(* End-of-run sections *)

(* Slowest first: with several over the threshold, the top one is the one
   worth acting on. The duration column is right-aligned so the paths line
   up; the threshold prints as configured. *)
let slow_section t slow_results =
  let rows =
    List.map
      (fun (r : Run.result) ->
        (pp_duration r.duration, sanitize_name (Test_tree.path_to_string r.path)))
      (List.sort
         (fun (a : Run.result) (b : Run.result) ->
           Float.compare b.duration a.duration)
         slow_results)
  in
  (* [pp_duration] is ASCII, so byte length is display width. *)
  let width = List.fold_left (fun w (d, _) -> max w (String.length d)) 0 rows in
  let caution s = put t (st t `Yellow s) in
  caution
    (spf "slow tests (%d, over %ss):" (List.length rows)
       (pp_configured t.slow_threshold));
  List.iter (fun (d, path) -> caution (spf "  %*s  %s" width d path)) rows

(* Run order, one row per test that failed and then passed. *)
let flaky_section t flaky_results =
  let caution s = put t (st t `Yellow s) in
  caution (spf "flaky tests (%d):" (List.length flaky_results));
  List.iter
    (fun (r : Run.result) ->
      caution
        (spf "  passed on attempt %d  %s" r.attempts
           (sanitize_name (Test_tree.path_to_string r.path))))
    flaky_results

(* The files the run wrote for its baselines, sorted by the path that
   prints; a source file's row counts its expectations. *)
let corrections baselines =
  List.sort
    (fun (a, _) (b, _) -> String.compare a b)
    (List.map
       (fun { Baseline.path; literals } -> (Os.display_path path, literals))
       (Baseline.writes baselines))

let accepts baselines =
  match Baseline.mode baselines with
  | Baseline.Update -> true
  | Baseline.Corrected | Baseline.Check -> false

let corrections_section t ~accepted rows =
  put t (spf "corrections (%d):" (List.length rows));
  List.iter
    (fun (path, literals) ->
      put t
        (spf "  %s %s%s"
           (if accepted then "accepted" else "wrote")
           path
           (if literals = 0 then ""
            else spf " (%d expectation%s)" literals (plural literals))))
    rows

let refusals baselines =
  List.map
    (fun (path, reason) ->
      spf "could not write %s: %s" (Os.display_path path) reason)
    (Baseline.refusals baselines)

let rec drop n = function _ :: rest when n > 0 -> drop (n - 1) rest | l -> l

let finish t ~results ~duration ?baselines ?(before_summary = ignore) () =
  sync t;
  clear_live t;
  let count p = List.length (List.filter p results) in
  let failed_results = List.filter counted_failure results in
  let slow_results = List.filter (over_threshold t) results in
  let flaky_results = List.filter flaky results in
  let written = match baselines with Some b -> corrections b | None -> [] in
  let ran = count (fun (r : Run.result) -> r.subject = Run.Test) in
  let summary =
    {
      passed =
        count (fun (r : Run.result) ->
            match r.outcome with
            | Failure.Pass -> true
            | Failure.Fail _ | Failure.Skip _ -> false);
      flaky = List.length flaky_results;
      skipped =
        count (fun (r : Run.result) ->
            match r.outcome with
            | Failure.Skip _ -> true
            | Failure.Pass | Failure.Fail _ -> false);
      excused =
        count (fun (r : Run.result) ->
            match r.outcome with
            | Failure.Fail _ -> not r.counted
            | Failure.Pass | Failure.Skip _ -> false);
      failed = List.length failed_results;
      subtests =
        List.fold_left
          (fun acc (r : Run.result) ->
            match r.outcome with
            | Failure.Fail fs ->
                acc + List.length (List.filter is_subtest_failure fs)
            | Failure.Pass | Failure.Skip _ -> acc)
          0 failed_results;
      not_run =
        (match t.declared with
        | Some _ -> max 0 (t.total_tests - ran)
        | None -> 0);
      corrections = List.length written;
      accepted = (match baselines with Some b -> accepts b | None -> false);
    }
  in
  (* What is left of the blocks are the verdict rows, which the executor
     records after the last test with no event. *)
  List.iter (commit_block t) (drop t.blocks failed_results);
  let closed = t.blocks > 0 && not t.verbose in
  if closed then put t (st t `Faint (Sections.rule ~width:rule_width None));
  (* A green run with nothing to show is its summary line alone. *)
  if slow_results <> [] || flaky_results <> [] || written <> [] then
    commit_header t;
  (* One blank line after the closing rule and after a section, owed until
     the next line of the transcript: what [before_summary] writes sits
     against the sections. Verbose rows owe the first section one too,
     unless a failed row's block has just closed on its own. *)
  let owed = ref closed in
  let section print =
    if !owed || (t.verbose && not t.spaced) then put t "";
    print ();
    owed := true
  in
  if slow_results <> [] then section (fun () -> slow_section t slow_results);
  if flaky_results <> [] then section (fun () -> flaky_section t flaky_results);
  if written <> [] then
    section (fun () -> corrections_section t ~accepted:summary.accepted written);
  Pp.flush t.out ();
  before_summary ();
  if !owed then put t "";
  summary_line t summary ~duration;
  Pp.flush t.out ()

(* A signal is ending the run: what the run knows, committed. The summary
   counts the stopped test among the [not run]. *)
let interrupted t ?before_summary ?releasing ~running ~results ~duration () =
  sync t;
  clear_live t;
  Pp.flush t.out ();
  Os.say
    (match (running, releasing) with
    | Some path, _ ->
        "interrupted in " ^ sanitize_name (Test_tree.path_to_string path)
    | None, Some fixture ->
        "interrupted while releasing " ^ sanitize_name fixture
    | None, None -> "interrupted between tests");
  finish t ~results ~duration ?before_summary ()

let observe t ~seed ~selection = function
  | Run.Run_started { suite; total; selected; properties } ->
      header t ~suite ~tests:selected ~declared:total ?selection
        ~seed:(if properties then Some seed else None)
        ()
  | Run.Test_started { path } -> begin_test t ~path
  | Run.Test_finished r -> result t r
  | Run.Fixture_release { name } -> note t ("releasing " ^ name)
  | Run.Interrupted { running; releasing; results; duration } ->
      interrupted t ?releasing ~running ~results ~duration ()

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

(* The filter is the row's path for a fixture release too, where [pp_body]
   passes none: such a row holds no baseline or property failure, so no hint
   reads it. *)
let annotation ?(invocation = `Mirrors) ?armed ~path (f : Failure.t) =
  let path_string = Test_tree.path_to_string path in
  let location =
    match f.loc with
    | Some { Loc.file; line; _ } ->
        spf "file=%s,line=%d," (escape_property file) line
    | None -> ""
  in
  let title =
    escape_property (spf "Test failure: %s" (sanitize_name path_string))
  in
  let message =
    drop_trailing_newlines
      (Pp.str "%a"
         (fun ppf f ->
           pp_failure ~ansi:false ~filter:path_string ~invocation ?armed ppf f)
         f)
  in
  spf "::error %stitle=%s::%s\n" location title (escape_data message)

let annotations ?invocation ?armed results =
  let buf = Buffer.create 256 in
  List.iter
    (fun (r : Run.result) ->
      (* Counted failures only (the record's bit): an excused expected
         failure did not fail the run and annotates nothing. *)
      match r.outcome with
      | Failure.Fail fs when r.counted ->
          List.iter
            (fun f ->
              Buffer.add_string buf
                (annotation ?invocation ?armed ~path:r.path f))
            fs
      | Failure.Fail _ | Failure.Pass | Failure.Skip _ -> ())
    results;
  Buffer.contents buf

(* Mutation lines *)

(* The lines an armed run is owed. The announcement prints
   unconditionally, because guarantee 12 makes it the promise that a run
   whose output does not say so has no mutant armed. The four write one
   line and do not flush: [Mutate_loop] flushes the descriptors after the
   announcement and after the verdict. *)

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
          "mutant survived: the armed site was evaluated %d time%s and no test \
           failed."
          hits
          (if hits = 1 then "" else "s")))

let mutation_not_evaluated t =
  clear_live t;
  put t (st t `Yellow "mutant not evaluated: no selected test ran the site.")

(* The loop's report *)

let mutation_testing t ~index ~total ~id =
  draw_live t (spf "[%d/%d] %s\u{2026}" index total (sanitize_name id))

(* A survivor's block is committed when its child ends, so a loop that
   dies has printed what it found. The rule that opens the blocks prints
   before the first, and [mutation_finish] closes them. *)
let mutation_survivor t survivor =
  sections t
    ((if t.survivors = 0 then
        [ Sections.Line []; Sections.Rule (Some "survivors") ]
      else [ Sections.Line [] ])
    @ Sections.survivor_block ~exe_width:None survivor);
  t.survivors <- t.survivors + 1

let mutation_finish t m =
  sections t (Sections.mutation_closing ~config:t.config m)

(* Windtrap's own word goes to standard error, past [t.out]: the live
   display is erased before it. *)
let mutation_refused t message =
  clear_live t;
  Pp.flush t.out ();
  Os.say message

let mutation_interrupted t ~testing m =
  mutation_refused t
    (match testing with
    | Some id -> "interrupted while testing " ^ id
    | None -> "interrupted during the determinism probe");
  mutation_finish t m

(* Running, reported *)

(* The one composition: build the renderer and the observer, open the
   GitHub envelope, run the suite, and project the run into every sink.
   The [Error] arm prints the startup message here and hands the error
   back; everything after the report — the focus warning, the exit code —
   is the facade's own. *)
let run ?(on_event = fun (_ : Run.event) -> ()) ~suite (config : Run.config)
    tests =
  let renderer = terminal config in
  (* [Run.execute]'s [?on_event] has one slot and the transcript owns it.
     A second subscriber composes here rather than replacing it, in a
     fixed order — transcript first — so no caller can drop the run's own
     output by subscribing, and none can reorder it. *)
  let transcript =
    observe renderer ~seed:config.Run.seed
      ~selection:(selection_description config)
  in
  let github = config.Run.github in
  (* The annotations follow the envelope's close: an [::error] written
     inside it folds away with the transcript. The summary stays the
     report's last line. *)
  let close_envelope results () =
    if github then begin
      print_string group_end;
      print_string
        (annotations ~invocation:config.Run.invocation ?armed:renderer.armed
           results)
    end
  in
  let on_event event =
    (match event with
    | Run.Interrupted { running; releasing; results; duration } ->
        interrupted renderer ?releasing ~running ~results ~duration
          ~before_summary:(close_envelope results) ()
    | Run.Run_started _ | Run.Test_started _ | Run.Test_finished _
    | Run.Fixture_release _ ->
        transcript event);
    on_event event
  in
  if github then print_string (group_start suite);
  match Run.execute ~on_event config ~suite tests with
  | Error error ->
      if github then print_string group_end;
      Os.say (Run.startup_message error);
      Error error
  | Ok outcome ->
      (* The one recorded list: test rows plus the executor's verdict rows
         (fixture-release failures). Every sink projects it, so a verdict
         that sets the exit code is always visible in the report. *)
      let results = Run.results outcome.Run.run in
      let baselines = Run.baselines outcome.Run.run in
      finish renderer ~results ~duration:outcome.Run.duration ~baselines
        ~before_summary:(close_envelope results) ();
      List.iter Os.say (refusals baselines);
      (* Last, so a report is written from the rows the terminal has
         already shown. *)
      Option.iter
        (Report_junit.write ~invocation:config.Run.invocation
           ?armed:renderer.armed ~suite ~duration:outcome.Run.duration ~results)
        config.Run.junit;
      Format.pp_print_flush Format.std_formatter ();
      Format.pp_print_flush Format.err_formatter ();
      Ok outcome
