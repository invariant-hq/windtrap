(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Not mutated: see [Mutate_loop]. *)
[@@@mutate exclude_file]

module Sections = Report_sections

let strf = Printf.sprintf
let plain = Sections.plain
let styled = Sections.styled
let plural n = if n = 1 then "" else "s"

(* The transcript has one width, so a pipe and a wide terminal print the
   same bytes. *)
let columns = 80
let duration_column = 51
let indent = "    "

(* Rounded to the precision it prints at before its unit is chosen, so
   9.96ms is [10ms] and never [10.0ms]. *)
let duration_to_string secs =
  let ms = secs *. 1000. in
  if Float.round (ms *. 10.) < 100. then strf "%.1fms" ms
  else if Float.round ms < 1000. then strf "%.0fms" ms
  else strf "%.1fs" secs

let seed_suffix = function
  | Some seed -> strf " (seed %s)" (Seed.to_string seed)
  | None -> ""

(* The renderer *)

type header = {
  suite : string;
  tests : int; (* selected *)
  declared : int; (* before selection *)
  selection : string option;
  seed : Seed.seed option;
}

(* A renderer draws at most one live line, which the next write erases, and
   commits every other write as whole lines. *)
type t = {
  out : Format.formatter;
  ansi : bool;
  terminal : bool; (* a reader watches [out]: colour alone marks a change *)
  live : bool;
  config : Run.config;
  armed : string option;
  mutable header : header option; (* [None] until [header] runs *)
  mutable header_printed : bool;
  mutable seen : int; (* results received *)
  mutable live_pending : bool;
  mutable spaced : bool; (* the last line [put] wrote is blank *)
  mutable blocks : int;
      (* failure blocks committed: those of the first [blocks] counted
         failures of the results [finish] is given *)
  mutable survivors : int; (* survivor blocks committed *)
}

let create ~out ~ansi ?(terminal = false) (config : Run.config) =
  if not (Float.is_finite config.slow_threshold && config.slow_threshold >= 0.)
  then invalid_arg "Report.create: slow_threshold not finite and non-negative";
  {
    out;
    ansi;
    terminal;
    (* A streamed test's bytes would land on the live line before its erase. *)
    live = terminal && ansi && not config.stream;
    config;
    armed =
      (match config.mutation with
      | Run.Armed id -> Some id
      | Run.No_mutation | Run.Loop _ -> None);
    header = None;
    header_printed = false;
    seen = 0;
    live_pending = false;
    spaced = false;
    blocks = 0;
    survivors = 0;
  }

(* Under GitHub Actions the live line is off even on a terminal: its erase
   sequences would land verbatim in the log. *)
let terminal (config : Run.config) =
  let tty = Os.is_tty_stdout () in
  let ansi =
    Os.resolve_color config.color ~tty ~inside_dune:(Os.inside_dune ())
      ~term_dumb:(Os.term_dumb ())
  in
  create ~out:Format.std_formatter ~ansi
    ~terminal:(tty && not (Os.in_github_actions ()))
    config

(* The transcript *)

let put t spans =
  Pp.pf t.out "%s@\n" (Sections.render ~ansi:t.ansi spans);
  t.spaced <- (match spans with [] -> true | _ :: _ -> false)

(* A streamed test writes past [t.out], through C stdio or straight to
   descriptor 1; its bytes are forced out before the report writes on. *)
let sync t = if t.config.stream then Capture.drain ()

let clear_live t =
  if t.live_pending then begin
    Pp.pf t.out "\r\027[2K";
    t.live_pending <- false
  end

let print t sections =
  clear_live t;
  Sections.print ~out:t.out ~ansi:t.ansi sections

(* Printed once: by [header] under verbose, before the first block or
   section of a compact run. *)
let commit_header t =
  match t.header with
  | Some h when not t.header_printed ->
      t.header_printed <- true;
      put t
        [
          plain
            (strf "%s: %d test%s%s" h.suite h.tests (plural h.tests)
               (seed_suffix h.seed));
        ]
  | Some _ | None -> ()

let header t ~suite ~tests ?(declared = tests) ?selection ~seed () =
  t.header <- Some { suite; tests; declared; selection; seed };
  if t.config.verbose then begin
    commit_header t;
    Pp.flush t.out ()
  end

let selected t = match t.header with Some h -> h.tests | None -> 0

(* The live line is cut as it prints, after escaping, so the cut never
   splits an escape. *)
let draw_live t ~width text =
  let text = Text.truncate_utf8 width (Text.escape_controls text) in
  Pp.pf t.out "%s" (Sections.render ~ansi:t.ansi [ styled `Faint text ]);
  Pp.flush t.out ();
  t.live_pending <- true

(* The progress of a test or of a mutant, over the previous one. *)
let progress t text =
  if t.live then begin
    clear_live t;
    Pp.pf t.out "\r\027[2K";
    draw_live t ~width:(columns - 2) ("  " ^ text)
  end

(* The denominator follows the count when more results arrive than [header]
   announced, so the counter never reads [5/4]. *)
let begin_test t ~path =
  let n = t.seen + 1 in
  progress t
    (strf "%s[%d/%d] %s\u{2026}"
       (if t.config.verbose then "Running " else "")
       n
       (max (selected t) n)
       (Test_tree.path_to_string path))

(* A result is classified by its record, never by a failure message: a
   failure that did not count is an excused expected failure. *)
let status (r : Run.result) =
  match r.outcome with
  | Failure.Pass -> `Passed
  | Failure.Skip _ -> `Skipped
  | Failure.Fail _ -> if r.counted then `Failed else `Excused

let failures (r : Run.result) =
  match r.outcome with
  | Failure.Fail failures -> failures
  | Failure.Pass | Failure.Skip _ -> []

let attempts (r : Run.result) =
  if r.attempts > 1 then [ strf "%d attempts" r.attempts ] else []

let armed_qualifier t =
  match t.armed with Some _ -> [ "mutant armed" ] | None -> []

(* "  TAG  <name>", then [timing] at [duration_column], or two spaces after
   a name that reaches it, then "(<qualifiers>)". The qualifiers follow the
   timing, so only a name moves the column. *)
let test_line ?(title = false) ~tag ~style ~name ~qualifiers ~timing () =
  let line =
    [
      plain "  ";
      styled style tag;
      plain "  ";
      (if title then styled `Bold name else plain name);
    ]
  in
  let timing =
    if timing = "" then []
    else
      let pad = max 2 (duration_column - Sections.width line) in
      [ plain (String.make pad ' '); styled `Faint timing ]
  in
  let qualifiers =
    match qualifiers with
    | [] -> []
    | parts ->
        [ plain " "; styled `Faint (strf "(%s)" (String.concat ", " parts)) ]
  in
  line @ timing @ qualifiers

(* The headline of a failure names every label never covered, so the
   coverage rows earn their place by the labels that were. *)
let label_table t (s : Property.stats) =
  let faint line = put t [ plain indent; styled `Faint line ] in
  if s.collected <> [] then begin
    faint (strf "labels (%d passing case%s):" s.cases (plural s.cases));
    List.iter
      (fun (label, count) ->
        faint
          (if s.cases > 0 then
             strf "  %5.1f%%  %s"
               (100. *. float_of_int count /. float_of_int s.cases)
               label
           else strf "  %d  %s" count label))
      s.collected
  end;
  if
    List.length s.coverage > 1
    && List.exists
         (fun (c : Property.cover_status) -> not c.satisfied)
         s.coverage
  then begin
    put t [ plain (indent ^ "covered labels:") ];
    List.iter
      (fun (c : Property.cover_status) ->
        put t
          [
            plain
              (strf "%s  %s  %d%s" indent c.label c.hits
                 (if c.satisfied then "" else "  never covered"));
          ])
      s.coverage
  end

(* The heading counts every byte before the first line shown: what the
   capture cut and the lines the cap drops. *)
let captured_output t (tail : Failure.tail) =
  if not (tail.text = "" && tail.omitted_bytes = 0) then begin
    let total = List.length (Text.split_lines tail.text) in
    let cut, kept =
      Text.window ~lines:Sections.max_lines ~bytes:max_int Tail tail.text
    in
    let shown = Text.split_lines kept in
    let n = List.length shown in
    let heading =
      if tail.omitted_bytes > 0 then
        strf "captured output (last %d line%s, %d earlier bytes omitted):" n
          (plural n) (tail.omitted_bytes + cut)
      else if n < total then
        strf "captured output (last %d of %d lines):" n total
      else strf "captured output (%d line%s):" total (plural total)
    in
    put t [ plain indent; styled `Faint heading ];
    List.iter (fun line -> put t [ plain (indent ^ "  " ^ line) ]) shown;
    Option.iter
      (fun path ->
        put t
          [
            plain indent; styled `Faint ("full log: " ^ Os.display_artifact path);
          ])
      tail.log_path
  end

(* Sibling subtests, or a body and its teardown, fail independently: each
   failure has its entry, and the hints are the whole test's. *)
let block_body t ~hints (r : Run.result) failures =
  List.iteri
    (fun i f ->
      if i > 0 then put t [];
      Sections.pp_failure ~ansi:t.ansi ~terminal:t.terminal ~excerpt:true
        ~hints:false t.out f)
    failures;
  Option.iter (label_table t) r.prop_stats;
  Option.iter (captured_output t)
    (List.find_map (fun (f : Failure.t) -> f.output_tail) failures);
  if hints then
    List.iter
      (fun hint -> put t [ plain (indent ^ hint) ])
      (Sections.hints ?armed:t.armed ~invocation:t.config.invocation failures)

(* A counted failure's block. Under verbose its title is the test's row. *)
let failure_block t (r : Run.result) =
  let failures = failures r and verbose = t.config.verbose in
  let missing_baseline =
    List.exists
      (fun (f : Failure.t) ->
        match f.kind with
        | Failure.Baseline { state = Failure.Missing _; _ } -> true
        | Failure.Baseline
            { state = Failure.Mismatch _ | Failure.Unresolvable _; _ }
        | Failure.Equality _ | Failure.Containment _ | Failure.Raise _
        | Failure.Property _ | Failure.Timeout _ | Failure.Message _ ->
            false)
      failures
  in
  put t
    (test_line ~title:true ~tag:"FAIL" ~style:`Red
       ~name:(Test_tree.path_to_string r.path)
       ~qualifiers:
         (attempts r
         @ (if verbose && missing_baseline then [ "no baseline" ] else [])
         @ armed_qualifier t)
       ~timing:(if verbose then duration_to_string r.duration else "")
       ());
  block_body t ~hints:true r failures

(* An expected failure's block is a counted one's, dim past each line's
   indent, so it reads as evidence. It has no hints: an [accept:] would act
   on a failure the test expects. The text is escaped already, and escaping
   is idempotent. *)
let excused_block t (r : Run.result) =
  let buffer = Buffer.create 256 in
  let out = Format.formatter_of_buffer buffer in
  block_body { t with out; ansi = false } ~hints:false r (failures r);
  Pp.flush out ();
  let dim line =
    let rec text_start i =
      if i < String.length line && line.[i] = ' ' then text_start (i + 1) else i
    in
    let i = text_start 0 in
    put t
      [
        plain (String.sub line 0 i);
        styled `Faint (String.sub line i (String.length line - i));
      ]
  in
  List.iter dim (Text.split_lines (Buffer.contents buffer));
  put t []

(* A failed fixture release belongs to no test: its title has no attempts
   and no duration, and its block no hint. *)
let release_block t f =
  put t
    (test_line ~title:true ~tag:"FAIL" ~style:`Red ~name:Sections.release_title
       ~qualifiers:(armed_qualifier t) ~timing:"" ());
  Sections.pp_failure ~ansi:t.ansi ~terminal:t.terminal ~excerpt:true
    ~hints:false t.out f

(* A block is committed when its test finishes, so a run that dies has
   printed what it knew. A compact run opens its blocks with the header and
   a rule, which [finish] closes; under verbose a block closes on a blank
   line. *)
let commit t block x =
  clear_live t;
  commit_header t;
  if not t.config.verbose then
    print t
      [
        (if t.blocks = 0 then Sections.Rule (Some "failures")
         else Sections.Line []);
      ];
  block t x;
  if t.config.verbose then put t [];
  t.blocks <- t.blocks + 1;
  Pp.flush t.out ()

(* The verbose row of a result that is not a counted failure. *)
let row t (r : Run.result) =
  let name = Test_tree.path_to_string r.path in
  let timing = duration_to_string r.duration in
  match r.outcome with
  | Failure.Pass -> (
      put t
        (test_line ~tag:"PASS" ~style:`Green ~name ~qualifiers:(attempts r)
           ~timing ());
      match r.prop_stats with
      | Some s when s.collected <> [] -> label_table t s
      | Some _ | None -> ())
  | Failure.Skip reason ->
      put t
        (test_line ~tag:"SKIP" ~style:`Yellow ~name
           ~qualifiers:(Option.to_list reason) ~timing:"" ())
  | Failure.Fail _ ->
      let expected =
        match r.xfail with
        | Some { reason = Some reason } -> "expected failure: " ^ reason
        | Some { reason = None } | None -> "expected failure"
      in
      put t
        (test_line ~tag:"XFAIL" ~style:`Faint ~name
           ~qualifiers:(attempts r @ [ expected ])
           ~timing ());
      excused_block t r

let result t (r : Run.result) =
  sync t;
  t.seen <- t.seen + 1;
  match status r with
  | `Failed -> commit t failure_block r
  | `Passed | `Skipped | `Excused ->
      clear_live t;
      if t.config.verbose then begin
        row t r;
        Pp.flush t.out ()
      end

(* Fixture releases fire after the last test, before [finish]. A compact
   run prints nothing per test, so its notice is the live line, and a
   hanging release still names itself. *)
let note t line =
  sync t;
  clear_live t;
  if t.config.verbose then begin
    put t [ plain line ];
    Pp.flush t.out ()
  end
  else if t.live then draw_live t ~width:(columns - 1) line

(* The selection *)

(* Quoted to be read and retyped: [%S] would escape the [›] of a test path
   into decimal bytes. A raw newline would break the report's layout. *)
let quote s =
  let escape = function
    | '"' -> "\\\""
    | '\\' -> "\\\\"
    | '\n' -> "\\n"
    | '\t' -> "\\t"
    | '\r' -> "\\r"
    | c when c < ' ' || c = '\127' -> strf "\\x%02x" (Char.code c)
    | c -> String.make 1 c
  in
  "\""
  ^ String.concat "" (List.map escape (List.of_seq (String.to_seq s)))
  ^ "\""

(* A test is kept by any one filter and dropped by any one exclusion. A
   focus narrows from the source, so it is named first. *)
let selection_description ~focused (config : Run.config) =
  let named what ~sep = function
    | [] -> []
    | values -> [ what ^ " " ^ String.concat sep (List.map quote values) ]
  in
  let parts =
    List.concat
      [
        (if focused then [ "focus" ] else []);
        named "filter" ~sep:" or " config.filter;
        named "exclusion" ~sep:" or " config.exclude;
        named "tag" ~sep:", " config.tags;
        named "excluded tag" ~sep:", " config.exclude_tags;
        (if config.failed_only then [ "--failed" ] else []);
        (match config.shard with
        | Some (k, n) -> [ strf "shard %d/%d" k n ]
        | None -> []);
      ]
  in
  match List.rev parts with
  | [] -> None
  | [ one ] -> Some one
  | last :: rest -> Some (String.concat ", " (List.rev rest) ^ " and " ^ last)

let empty_selection_reason ~declared ~selection =
  match (declared, selection) with
  | 0, _ -> Some "the suite declares none"
  | declared, Some selection ->
      Some
        (strf "%s matched none of %d test%s" selection declared
           (plural declared))
  | _, None -> None

(* The end of the run *)

(* The terms count the results reported, not the header's tests, and a
   failed fixture release counts as failed, so the summary agrees with the
   exit code. *)
type summary = {
  passed : int;
  flaky : int;
  skipped : int;
  excused : int;
  failed : int;
  subtests : int;
  not_run : int;
  corrections : int; (* files written *)
  accepted : bool; (* the corrections replaced their files *)
  not_written : int; (* files refused *)
}

(* A run that printed no header names itself here, with the seed the header
   would have shown: a green property run stays replayable from one line. *)
let summary_line t (c : summary) ~duration =
  let named = if t.header_printed then None else t.header in
  let prefix = match named with Some h -> h.suite ^ ": " | None -> "" in
  if c.passed + c.failed + c.skipped + c.excused + c.not_run = 0 then begin
    let reason =
      match t.header with
      | Some h ->
          empty_selection_reason ~declared:h.declared ~selection:h.selection
      | None -> None
    in
    put t
      [
        plain
          (match reason with
          | Some reason -> strf "%sno tests ran: %s." prefix reason
          | None -> prefix ^ "no tests ran.");
      ];
    (* A build action has no launcher to restate, so it names the flag. A
       selection that the environment broadcast gets no hint: the variable
       reached every stanza, so an emptied one holds no mistake, and an
       inline runner takes no [-l]. *)
    match t.header with
    | Some { declared; selection = Some _; _ }
      when declared <> 0 && not t.config.broadcast.selection -> (
        match t.config.invocation with
        | `Exe launcher -> put t [ plain (strf "list: %s -l" launcher) ]
        | `Mirrors -> put t [ styled `Faint "(list the suite's tests with -l)" ]
        )
    | Some _ | None -> ()
  end
  else begin
    let term n spans = if n > 0 then [ spans ] else [] in
    let terms =
      List.concat
        [
          term c.passed
            ((if c.failed = 0 && c.not_written = 0 then styled `Green else plain)
               (strf "%d passed" c.passed)
            ::
            (if c.flaky > 0 then
               [ plain " "; styled `Yellow (strf "(%d flaky)" c.flaky) ]
             else []));
          term c.skipped [ styled `Yellow (strf "%d skipped" c.skipped) ];
          term c.excused
            [
              styled `Faint
                (strf "%d expected failure%s" c.excused (plural c.excused));
            ];
          term c.failed
            [
              styled `Red
                (strf "%d failed%s" c.failed
                   (if c.subtests > 0 then
                      strf " (%d subtest failure%s)" c.subtests
                        (plural c.subtests)
                    else ""));
            ];
          term c.not_run [ plain (strf "%d not run" c.not_run) ];
          term c.corrections
            [
              plain
                (strf "%d correction%s %s" c.corrections (plural c.corrections)
                   (if c.accepted then "accepted" else "written"));
            ];
          term c.not_written
            [ styled `Red (strf "%d not written" c.not_written) ];
        ]
    in
    let seed = match named with Some h -> seed_suffix h.seed | None -> "" in
    put t
      (plain prefix
       :: List.concat
            (List.mapi
               (fun i term -> if i > 0 then plain ", " :: term else term)
               terms)
      @ [ plain (strf " in %s%s." (duration_to_string duration) seed) ])
  end

(* Skips never count, their durations are not run time, and a test tagged
   [slow] is exempt everywhere. *)
let over_threshold t (r : Run.result) =
  t.config.slow_threshold > 0.
  && (not r.slow_tagged)
  && status r <> `Skipped
  && r.duration >= t.config.slow_threshold

let caution t line = put t [ styled `Yellow line ]

(* Slowest first: the top row is the one worth acting on. A duration is
   ASCII, so its bytes are its width. *)
let slow_section t results =
  let rows =
    List.map
      (fun (r : Run.result) ->
        (duration_to_string r.duration, Test_tree.path_to_string r.path))
      (List.sort
         (fun (a : Run.result) (b : Run.result) ->
           Float.compare b.duration a.duration)
         results)
  in
  let width = List.fold_left (fun w (d, _) -> max w (String.length d)) 0 rows in
  caution t
    (strf "slow tests (%d, over %ss):" (List.length rows)
       (Pp.to_string Pp.decimal t.config.slow_threshold));
  List.iter (fun (d, path) -> caution t (strf "  %*s  %s" width d path)) rows

let flaky_section t results =
  caution t (strf "flaky tests (%d):" (List.length results));
  List.iter
    (fun (r : Run.result) ->
      caution t
        (strf "  passed on attempt %d  %s" r.attempts
           (Test_tree.path_to_string r.path)))
    results

(* The files the run wrote or could not write, by the path that prints. *)
let corrections baselines =
  List.sort
    (fun (a, _) (b, _) -> String.compare a b)
    (List.map
       (fun write ->
         match write with
         | Baseline.Written { path; _ } | Baseline.Refused { path; _ } ->
             (Os.display_path path, write))
       (Baseline.writes baselines))

(* An accepted literal is compiled into the executable, so the tests see it
   only after a build; the row says so where the reader looks after
   accepting. *)
let corrections_section ~accepted t rows =
  put t [ plain (strf "corrections (%d):" (List.length rows)) ];
  List.iter
    (fun (path, write) ->
      match write with
      | Baseline.Written { literals; _ } ->
          put t
            [
              plain
                (strf "  %s %s%s"
                   (if accepted then "accepted" else "wrote")
                   path
                   (if literals = 0 then ""
                    else
                      strf " (%d expectation%s%s)" literals (plural literals)
                        (if accepted then
                           strf "; rebuild before the tests see %s"
                             (if literals = 1 then "it" else "them")
                         else "")));
            ]
      | Baseline.Refused { reason; _ } ->
          put t [ styled `Red (strf "  could not write %s: %s" path reason) ])
    rows

let rec drop n = function _ :: rest when n > 0 -> drop (n - 1) rest | l -> l

(* The commands that act on the whole run sit on the summary, as a loop's
   [reproduce:] sits on its outcome, so the last line still says how the run
   ended. An interrupted run's would run the tests the signal kept from
   running, and [-u] would accept baselines the report does not show. *)
let close t ~commands ~results ~release_failures ~duration ?baselines ?stopped
    ?(before_summary = ignore) () =
  sync t;
  clear_live t;
  let count s = List.length (List.filter (fun r -> status r = s) results) in
  let failed = List.filter (fun r -> status r = `Failed) results in
  let slow = List.filter (over_threshold t) results in
  let flaky =
    List.filter
      (fun (r : Run.result) -> r.attempts > 1 && status r = `Passed)
      results
  in
  let rows = match baselines with Some b -> corrections b | None -> [] in
  let accepted =
    match baselines with
    | Some b -> Baseline.mode b = Baseline.Update
    | None -> false
  in
  let written, refused =
    List.partition
      (function
        | _, Baseline.Written _ -> true | _, Baseline.Refused _ -> false)
      rows
  in
  let summary =
    {
      passed = count `Passed;
      flaky = List.length flaky;
      skipped = count `Skipped;
      excused = count `Excused;
      failed = List.length failed + List.length release_failures;
      subtests =
        List.length
          (List.filter Sections.is_subtest_failure
             (List.concat_map failures failed));
      not_run = max 0 (selected t - List.length results);
      corrections = List.length written;
      accepted;
      not_written = List.length refused;
    }
  in
  List.iter (commit t failure_block) (drop t.blocks failed);
  List.iter (commit t release_block) release_failures;
  let closed = t.blocks > 0 && not t.config.verbose in
  if closed then print t [ Sections.Rule None ];
  let section rows render =
    match rows with [] -> [] | _ :: _ -> [ (fun () -> render t rows) ]
  in
  let sections =
    section slow slow_section
    @ section flaky flaky_section
    @ section rows (corrections_section ~accepted)
  in
  (* A green run with nothing to show is its summary line alone. *)
  (match sections with [] -> () | _ :: _ -> commit_header t);
  (* A blank line after the closing rule and after each section is owed
     until the next line, so what [before_summary] writes sits against the
     sections. Under verbose the first section is owed one unless a block
     has just closed on its own. *)
  let owed =
    List.fold_left
      (fun owed section ->
        if owed || (t.config.verbose && not t.spaced) then put t [];
        section ();
        true)
      closed sections
  in
  Pp.flush t.out ();
  before_summary ();
  if owed then put t [];
  if commands then begin
    let armed = t.armed and invocation = t.config.invocation in
    let run = `Run t.config and failures = List.concat_map failures failed in
    (* [-x] stops a run on its one counted failure, and [-u] passes an
       accepted test: over the selection it would accept baselines of tests
       this run did not reach. *)
    let accepted =
      match failed with
      | [ r ] when t.config.bail ->
          `Filter (Some (Test_tree.path_to_string r.path))
      | _ -> run
    in
    List.iter
      (fun line -> put t [ plain line ])
      (Option.to_list
         (Sections.accept ?armed ~invocation ~tests:accepted failures)
      @ Option.to_list (Sections.replay ?armed ~invocation ~tests:run failures)
      )
  end;
  (* The summary counts the tests a stop kept from running; this names it. *)
  Option.iter
    (fun path ->
      put t
        [
          plain
            (strf
               "run stopped after %s: a call on another domain outlived the \
                test's limit"
               (Text.escape_controls (Test_tree.path_to_string path)));
        ])
    stopped;
  summary_line t summary ~duration;
  Pp.flush t.out ()

let finish = close ~commands:true

(* A path is one line of the diagnostic, whatever it holds. *)
let interrupted t ?before_summary ?releasing ~running ~results ~duration () =
  sync t;
  clear_live t;
  Pp.flush t.out ();
  Os.say
    (match (running, releasing) with
    | Some path, _ ->
        "interrupted in " ^ Text.escape_controls (Test_tree.path_to_string path)
    | None, Some fixture ->
        "interrupted while releasing " ^ Text.escape_controls fixture
    | None, None -> "interrupted between tests");
  close t ~commands:false ~results ~release_failures:[] ~duration
    ?before_summary ()

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

(* Over a text whose control bytes are escaped, LF is the one left. *)
let encode ~property s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '%' -> Buffer.add_string b "%25"
      | '\n' -> Buffer.add_string b "%0A"
      | ':' when property -> Buffer.add_string b "%3A"
      | ',' when property -> Buffer.add_string b "%2C"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let escape_data s =
  encode ~property:false
    (String.concat "\n"
       (List.map Text.escape_controls (String.split_on_char '\n' s)))

let escape_property s = encode ~property:true (Text.escape_controls s)
let group_start name = strf "::group::%s\n" (escape_data name)
let group_end = "::endgroup::\n"

let annotation ?(invocation = `Mirrors) ?armed ~path (f : Failure.t) =
  let path = Test_tree.path_to_string path in
  let location =
    match f.loc with
    | Some { Loc.file; line; _ } ->
        strf "file=%s,line=%d," (escape_property file) line
    | None -> ""
  in
  let entry =
    Pp.str "%a"
      (fun ppf f ->
        Sections.pp_failure ~ansi:false ~filter:path ~invocation ?armed ppf f)
      f
  in
  let rec stop i = if i > 0 && entry.[i - 1] = '\n' then stop (i - 1) else i in
  strf "::error %stitle=%s::%s\n" location
    (escape_property ("Test failure: " ^ path))
    (escape_data (String.sub entry 0 (stop (String.length entry))))

let annotations ?invocation ?armed ~release_failures results =
  let annotate path f = annotation ?invocation ?armed ~path f in
  let counted (r : Run.result) =
    if status r = `Failed then List.map (annotate r.path) (failures r) else []
  in
  String.concat ""
    (List.concat_map counted results
    @ List.map (annotate [ Sections.release_title ]) release_failures)

(* Mutation lines *)

(* The announcement prints whatever the mode: a run whose output does not
   announce a mutant has none armed. *)
let mutation_armed t ~id ~before ~after =
  clear_live t;
  put t
    [
      plain "mutant ";
      styled `Bold id;
      plain (strf " armed: %s \u{2192} %s" before after);
    ]

let mutation_killed t =
  clear_live t;
  put t [ styled `Green "mutant killed." ]

let mutation_survived t ~hits ~xfail_failed =
  clear_live t;
  let line =
    if xfail_failed then
      strf
        "mutant survived: the site was evaluated %d time%s and only xfail \
         tests failed."
        hits (plural hits)
    else
      strf
        "mutant survived: the armed site was evaluated %d time%s and no test \
         failed."
        hits (plural hits)
  in
  put t [ styled `Red line ]

let mutation_not_evaluated t =
  clear_live t;
  put t
    [ styled `Yellow "mutant not evaluated: no selected test ran the site." ]

let mutation_not_reached t =
  clear_live t;
  put t [ styled `Yellow "mutant not reached: only xfail tests ran the site." ]

let mutation_testing t ~index ~total ~id =
  progress t (strf "[%d/%d] %s\u{2026}" index total id)

(* A survivor's block is committed when its child ends, so a loop that dies
   has printed what it found. *)
let mutation_survivor t survivor =
  print t
    (Sections.Line []
     :: (if t.survivors = 0 then [ Sections.Rule (Some "survivors") ] else [])
    @ Sections.survivor_block ~exe_width:None survivor);
  t.survivors <- t.survivors + 1

let mutation_finish ?note t m =
  let closing = Sections.mutation_closing ~config:t.config m in
  match (note, List.rev closing) with
  | Some note, outcome :: rest ->
      print t (List.rev rest);
      Os.say note;
      print t [ outcome ]
  | None, _ | Some _, [] -> print t closing

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

(* Failure projections *)

let headline = Sections.headline
let is_subtest_failure = Sections.is_subtest_failure
let labeled_msg = Sections.labeled_msg
let pp_failure = Sections.pp_failure

(* Running *)

let run ?(on_event = fun (_ : Run.event) -> ()) ~suite (config : Run.config)
    tests =
  let renderer = terminal config in
  let transcript =
    observe renderer ~seed:config.seed
      ~selection:
        (selection_description
           ~focused:(Test_tree.focus_sites tests <> [])
           config)
  in
  (* An [::error] written inside the envelope folds away with the
     transcript, so the annotations follow its close. *)
  let close_envelope ~release_failures results () =
    if config.github then begin
      print_string group_end;
      print_string
        (annotations ~invocation:config.invocation ?armed:renderer.armed
           ~release_failures results)
    end
  in
  (* The transcript observes first, so a subscriber can neither drop nor
     reorder the run's own output. *)
  let on_event event =
    (match event with
    | Run.Interrupted { running; releasing; results; duration } ->
        interrupted renderer ?releasing ~running ~results ~duration
          ~before_summary:(close_envelope ~release_failures:[] results)
          ()
    | Run.Run_started _ | Run.Test_started _ | Run.Test_finished _
    | Run.Fixture_release _ ->
        transcript event);
    on_event event
  in
  if config.github then print_string (group_start suite);
  match Run.execute ~on_event config ~suite tests with
  | Error error ->
      if config.github then print_string group_end;
      Os.say (Run.startup_message error);
      Error error
  | Ok outcome ->
      let results = Run.results outcome.run in
      let release_failures = outcome.release_failures in
      finish renderer ~results ~release_failures ~duration:outcome.duration
        ~baselines:(Run.baselines outcome.run)
        ?stopped:(Run.stopped outcome.run)
        ~before_summary:(close_envelope ~release_failures results)
        ();
      (* Last, so the file is written from the rows the terminal has shown. *)
      Option.iter
        (Report_junit.write ~invocation:config.invocation ?armed:renderer.armed
           ~suite ~duration:outcome.duration ~results ~release_failures)
        config.junit;
      Pp.flush Format.std_formatter ();
      Pp.flush Format.err_formatter ();
      Ok outcome
