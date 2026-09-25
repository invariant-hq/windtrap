(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Report and Report_sections: golden transcripts over a
   synthetic run covering every failure kind (equality with diff, raise,
   baseline missing/mismatch, property with inner failure, body + teardown
   pair, captured tail with a drop count) at both levels, compact (nothing
   per test, the header iff a block follows) and verbose (a line per test),
   when a compact run prints more than its summary, the slow and flaky
   blocks, ANSI styling and diff highlighting,
   ANSI hygiene (payload-borne escapes shown, never
   obeyed), the live displays, the failure projections (headline,
   pp_failure), degenerate equalities, diff and proposed-content display
   bounds, duration forms, replay-line quoting and root-token consistency,
   captured-tail bounding, the source excerpt, the GitHub envelope, the
   event observer, and the coverage and mutation sections. Drives [Report] directly over synthetic [Run] results;
   detection goes through string equality and containment, so a broken
   renderer cannot hide its own failure. *)

open Windtrap
open Windtrap.Private
module Fixtures = Render_fixtures
module Sections = Report_sections

let has ~sub s = Text.contains_substring ~pattern:sub s

(* A styled transcript without its styling: every CSI sequence, from ESC
   [\[] to its final byte, the one kind the renderer writes, since a
   payload's own ESC prints escaped. *)
let strip_ansi s =
  let n = String.length s in
  let b = Buffer.create n in
  let rec final i =
    if i >= n then i
    else if s.[i] >= '\x40' && s.[i] <= '\x7e' then i + 1
    else final (i + 1)
  in
  let rec go i =
    if i < n then
      if s.[i] = '\027' && i + 1 < n && s.[i + 1] = '[' then go (final (i + 2))
      else begin
        Buffer.add_char b s.[i];
        go (i + 1)
      end
  in
  go 0;
  Buffer.contents b

let occurrences_of ~sub s =
  let n = String.length sub in
  let rec go i acc =
    if i + n > String.length s then acc
    else if String.sub s i n = sub then go (i + 1) (acc + 1)
    else go (i + 1) acc
  in
  go 0 0

(* A plain block without its [~] lines: what the coloured block, which
   colours the span instead, reads as once its styling is stripped. *)
let without_marks block =
  String.concat "\n"
    (List.filter
       (fun line ->
         not
           (String.contains line '~'
           && String.for_all (fun c -> c = ' ' || c = '~') line))
       (String.split_on_char '\n' block))

(* The rules around a compact run's failures, spelled out: 58 columns. *)
let failures_rule = "──────────────────────── failures ────────────────────────"

let closing_rule = "──────────────────────────────────────────────────────────"

(* Drivers *)

(* The renderer reads its presentation knobs off the one configuration
   record; the tests name only the knobs they vary. *)
let config ?(mode = `Compact) ?(slow_threshold = 1.0) ?(invocation = `Mirrors)
    ?armed () =
  {
    (Run.default_config ()) with
    Run.verbose = mode = `Verbose;
    slow_threshold;
    invocation;
    mutation =
      (match armed with Some id -> Run.Armed id | None -> Run.No_mutation);
  }

let with_renderer ?(ansi = false) ?mode ?terminal ?slow_threshold ?invocation
    ?armed fn =
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  let r =
    Report.create ~out:ppf ~ansi ?terminal
      (config ?mode ?slow_threshold ?invocation ?armed ())
  in
  fn r;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* The section vocabulary's own sink, for the coverage and mutation
   reports the binary draws without a renderer. *)
let sections ?(ansi = false) l =
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  Sections.print ~out:ppf ~ansi l;
  Buffer.contents buf

(* As the executor drives it: each test row through [begin_test] and
   [result], the failed release only in [finish]. *)
let transcript ?ansi ?mode ?terminal ?invocation ?(seed = Some Fixtures.root) ()
    =
  let tests = Fixtures.results in
  with_renderer ?ansi ?mode ?terminal ?invocation (fun r ->
      Report.header r ~suite:"mylib" ~tests:(List.length tests) ~seed ();
      List.iter
        (fun (res : Run.result) ->
          Report.begin_test r ~path:res.path;
          Report.result r res)
        tests;
      Report.finish r ~results:tests
        ~release_failures:[ Fixtures.release_failure ]
        ~duration:Fixtures.duration ())

(* A block as it prints; a coloured one lands on a terminal unless the
   caller says otherwise. *)
let failure_block ?(ansi = false) ?(terminal = ansi) ?excerpt ?filter
    ?invocation ?armed f =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Report.pp_failure ~ansi ~terminal ?excerpt ?filter ?invocation ?armed ppf f;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* Colour roles

   A coloured text is written with its roles marked [«role|text»]: [r] red,
   [g] green, [y] yellow, [d] faint, [b] bold, [R] bold red, [G] bold
   green, [c] cyan, [w] white, and [»] the reset. [roles] is that text with
   the escapes the marks stand for, and [marks] turns the escapes back into
   marks, one for one, so a coloured transcript reads and diffs as text. *)

let role_escapes =
  [
    ("\u{ab}r|", "\027[31m");
    ("\u{ab}g|", "\027[32m");
    ("\u{ab}y|", "\027[33m");
    ("\u{ab}d|", "\027[2m");
    ("\u{ab}b|", "\027[1m");
    ("\u{ab}R|", "\027[1;31m");
    ("\u{ab}G|", "\027[1;32m");
    ("\u{ab}c|", "\027[36m");
    ("\u{ab}w|", "\027[37m");
    ("\u{bb}", "\027[0m");
  ]

(* [s] with each [from] of [table] replaced by its [into], left to right. *)
let replace_all table s =
  let buf = Buffer.create (String.length s) in
  let rec go i =
    if i < String.length s then
      match
        List.find_opt
          (fun (from, _) ->
            i + String.length from <= String.length s
            && String.sub s i (String.length from) = from)
          table
      with
      | Some (from, into) ->
          Buffer.add_string buf into;
          go (i + String.length from)
      | None ->
          Buffer.add_char buf s.[i];
          go (i + 1)
  in
  go 0;
  Buffer.contents buf

let roles marked = replace_all role_escapes marked

let marks coloured =
  replace_all
    (List.map (fun (mark, escape) -> (escape, mark)) role_escapes)
    coloured

(* The golden transcripts

   A fixture run with failures of every kind, two slow tests and a flaky
   one, at both levels: compact opens with the header and goes straight to
   the blocks; verbose prints a row per test, a failed test's block under
   its row.

   These are file baselines, not string literals in this file. A
   transcript is an artifact (column alignment, ANSI runs) and the reason
   to keep one is to read the diff when it changes. As a literal it could
   only be reviewed by retyping it; as a file under expected/ the review
   is `git diff` and the acceptance is
   `dune exec test/unit/test_report.exe -- -u`. Read every accepted diff:
   this is the whole of what a windtrap run prints. *)

let golden name actual =
  expect_file actual ("test/unit/expected/test_report/" ^ name ^ ".expected")

let golden_exe = "dune exec test/main.exe --"
let golden_invocation = `Exe golden_exe

let test_golden_compact () =
  let actual = transcript ~invocation:golden_invocation () in
  golden "compact" actual;
  not_contains ~msg:"plain transcript has no escape codes" ~sub:"\027" actual

let test_golden_verbose () =
  let actual = transcript ~mode:`Verbose ~invocation:golden_invocation () in
  golden "verbose" actual;
  not_contains ~msg:"plain transcript has no escape codes" ~sub:"\027" actual

(* The coloured transcript, which had no golden at all: [test_ansi] pins
   nine substrings, so every escape run BETWEEN them was unpinned, and a
   colour bug is exactly a wrong byte next to a right one. A baseline of
   the whole thing costs one file and pins every escape, each spelled as
   its role, so a colour change is a diff a reader can review. The golden
   holds the marks, never a stripped text: the marks turn back into the
   exact bytes, and an escape without a role fails here. *)
let golden_coloured name actual =
  let marked = marks actual in
  equal ~msg:"the marks turn back into the exact bytes" string actual
    (roles marked);
  not_contains ~msg:"every escape has a role" ~sub:"\027" marked;
  golden name marked

let test_golden_ansi () =
  let actual =
    transcript ~ansi:true ~mode:`Verbose ~invocation:golden_invocation ()
  in
  golden_coloured "verbose-ansi" actual;
  contains ~msg:"the ansi golden really is coloured" ~sub:"\027[" actual

(* The compact one too: the rules around the failures are its own, and
   dim. *)
let test_golden_compact_ansi () =
  let actual = transcript ~ansi:true ~invocation:golden_invocation () in
  golden_coloured "compact-ansi" actual;
  contains ~msg:"the opening rule is dim"
    ~sub:("\n\027[2m" ^ failures_rule ^ "\027[0m\n  \027[31mFAIL\027[0m")
    actual;
  contains ~msg:"the closing rule is dim, a blank line after it"
    ~sub:("\n\027[2m" ^ closing_rule ^ "\027[0m\n\n")
    actual

let test_ansi () =
  let t = transcript ~ansi:true ~mode:`Verbose () in
  contains ~msg:"ansi: FAIL tag is red" ~sub:"\027[31mFAIL\027[0m" t;
  contains ~msg:"ansi: PASS tag is green" ~sub:"\027[32mPASS\027[0m" t;
  contains
    ~msg:
      "ansi: the inserted span is bold red inside a plain value, and off a \
       terminal a red mark repeats it"
    ~sub:
      ("\027[2mexpected\027[0m  [(\"alice\", [1; 2; 3]); (\"bob\", [4])]\n\
       \    \027[2mactual\027[0m    [(\"alice\", [1; 2; 3]); (\"bob\", \
        [4\027[1;31m; 5]); (\"carol\", [\027[0m])]\n" ^ String.make 47 ' '
     ^ "\027[1;31m~~~~~~~~~~~~~~~~~~\027[0m\n    \027[2mcaptured output")
    t;
  contains ~msg:"ansi: slow entry is caution yellow, one style"
    ~sub:"\n\027[33m  2.5s  slow › big sort\027[0m\n" t;
  contains ~msg:"ansi: slow heading is caution yellow, one style"
    ~sub:"\n\027[33mslow tests (2, over 1s):\027[0m\n" t;
  not_contains ~msg:"ansi: the slow section carries no advice line"
    ~sub:"exempt with the" t;
  let c = transcript ~ansi:true () in
  (* The summary counts wear the block palette, one convention for the
     whole transcript, not two for the same run. *)
  contains ~msg:"ansi: summary skip count is yellow"
    ~sub:"\027[33m1 skipped\027[0m" c;
  contains ~msg:"ansi: summary fail count is red" ~sub:"\027[31m7 failed\027[0m"
    c;
  (* The transcript fixture carries no excused result, so the faint count
     gets its own run. *)
  let x =
    with_renderer ~ansi:true (fun r ->
        Report.finish r ~release_failures:[]
          ~results:[ Fixtures.excused_result ]
          ~duration:0.1 ())
  in
  contains ~msg:"ansi: summary excused count is faint"
    ~sub:"\027[2m1 expected failure\027[0m" x;
  (* A pair that refines: ["true"]/["false"] marks 80% of a side, which the
     noise rule declines. The styling has to be shown on a real span. *)
  let b =
    failure_block ~ansi:true
      (Failure.equality ~expected:"the quick brown fox"
         ~actual:"the quick brawn fox" ())
  in
  contains ~msg:"ansi: the expected side's changed span is bold green"
    ~sub:"\027[2mexpected\027[0m  the quick br\027[1;32mo\027[0mwn fox\n" b;
  contains ~msg:"ansi: the actual side's changed span is bold red"
    ~sub:"\027[2mactual\027[0m    the quick br\027[1;31ma\027[0mwn fox\n" b;
  not_contains ~msg:"ansi: a refined pair prints no [~] line under colour"
    ~sub:"~" b;
  let plain_refined =
    failure_block
      (Failure.equality ~expected:"the quick brown fox"
         ~actual:"the quick brawn fox" ())
  in
  equal ~msg:"plain: a [~] line under each changed side" string
    "    expected  the quick brown fox\n\
    \                          ~\n\
    \    actual    the quick brawn fox\n\
    \                          ~\n"
    plain_refined;
  (* A span of spaces has no glyph to colour: its [~] line prints under
     colour too, on the side that holds it. *)
  let spaces =
    failure_block ~ansi:true
      (Failure.equality ~expected:"a long enough  value"
         ~actual:"a long enough value" ())
  in
  equal ~msg:"ansi: a changed span of spaces keeps its mark" string
    "    \027[2mexpected\027[0m  a long enough \027[1;32m \027[0mvalue\n\
    \                            \027[1;32m~\027[0m\n\
    \    \027[2mactual\027[0m    a long enough value\n"
    spaces;
  (* Refinement declined: each side is colored whole rather than losing its
     color, so an equality failure reads the same way either way. *)
  let d =
    failure_block ~ansi:true
      (Failure.equality ~expected:"true" ~actual:"false" ())
  in
  contains ~msg:"ansi: a declined pair colors expected whole"
    ~sub:"\027[32mtrue\027[0m" d;
  contains ~msg:"ansi: a declined pair colors actual whole"
    ~sub:"\027[31mfalse\027[0m" d;
  let plain =
    failure_block (Failure.equality ~expected:"true" ~actual:"false" ())
  in
  not_contains ~msg:"plain: a declined pair gets no marker line" ~sub:"~" plain;
  contains ~msg:"plain: a declined pair still shows both values"
    ~sub:"expected  true\n    actual    false" plain

let test_live () =
  let t =
    with_renderer ~ansi:true ~mode:`Verbose ~terminal:true (fun r ->
        Report.header r ~suite:"mylib" ~tests:2 ~seed:None ();
        Report.begin_test r ~path:[ "math"; "addition" ];
        Report.result r (List.hd Fixtures.results))
  in
  contains ~msg:"live: progress line drawn" ~sub:"Running [1/2] math › addition"
    t;
  contains ~msg:"live: cursor clear emitted" ~sub:"\r\027[2K" t;
  let plain =
    with_renderer ~ansi:false ~mode:`Verbose ~terminal:true (fun r ->
        Report.header r ~suite:"mylib" ~tests:2 ~seed:None ();
        Report.begin_test r ~path:[ "math"; "addition" ])
  in
  not_contains ~msg:"live: off without ansi" ~sub:"Running" plain

let test_live_compact_tail () =
  (* The compact erasable tail is the only thing a green compact run
     prints while it runs: it draws from column zero, never brings the
     header out, and its erasure re-prints nothing. A green run's screen
     stays blank, and what a pipe sees is exactly the committed
     transcript. *)
  let t =
    with_renderer ~ansi:true ~terminal:true (fun r ->
        Report.header r ~suite:"mylib" ~tests:2 ~seed:None ();
        Report.begin_test r ~path:[ "math"; "addition" ];
        Report.result r (List.hd Fixtures.results);
        Report.begin_test r ~path:[ "users"; "sessions after login" ])
  in
  contains ~msg:"compact tail: counter and name drawn"
    ~sub:"[1/2] math › addition" t;
  not_contains ~msg:"compact tail: the tail never brings the header out"
    ~sub:"mylib: 2 tests" t;
  contains ~msg:"compact tail: the erase re-prints nothing"
    ~sub:"\027[0m\r\027[2K\r\027[2K\027[2m  [2/2]" t;
  contains ~msg:"compact tail: next tail follows from column zero"
    ~sub:"[2/2] users › sessions after login" t;
  (* A failure commits its block when the test finishes: the tail is
     erased before the first committed byte, and the next tail draws from
     column zero under the block. *)
  let after_failure =
    with_renderer ~ansi:true ~terminal:true (fun r ->
        Report.header r ~suite:"mylib" ~tests:2 ~seed:None ();
        Report.begin_test r ~path:[ "bad" ];
        Report.result r
          (Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "b" ]));
        Report.begin_test r ~path:[ "math"; "addition" ];
        Report.result r (List.hd Fixtures.results))
  in
  equal ~msg:"compact tail: erased before the block, redrawn under it" string
    ("\r\027[2K\027[2m  [1/2] bad…\027[0m\r\027[2Kmylib: 2 tests\n\027[2m"
   ^ failures_rule
   ^ "\027[0m\n\
     \  \027[31mFAIL\027[0m  \027[1mbad\027[0m\n\
     \    b\n\
      \r\027[2K\027[2m  [2/2] math › addition…\027[0m\r\027[2K")
    after_failure;
  let plain =
    with_renderer ~ansi:false ~terminal:true (fun r ->
        Report.header r ~suite:"mylib" ~tests:2 ~seed:None ();
        Report.begin_test r ~path:[ "math"; "addition" ])
  in
  not_contains ~msg:"compact tail: off without ansi" ~sub:"[1/2]" plain

let test_header_forms () =
  let one =
    with_renderer ~mode:`Verbose (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ())
  in
  equal ~msg:"header: singular, no seed" string "s: 1 test\n" one;
  let zero =
    with_renderer ~mode:`Verbose (fun r ->
        Report.header r ~suite:"s" ~tests:0 ~seed:None ())
  in
  equal ~msg:"header: zero tests" string "s: 0 tests\n" zero;
  (* Compact prints the header before its first block or section, from
     the recorded fields (the golden transcripts pin it). *)
  let compact =
    with_renderer (fun r -> Report.header r ~suite:"s" ~tests:1 ~seed:None ())
  in
  equal ~msg:"header: compact prints nothing at the start" string "" compact

let test_seed_token_consistency () =
  (* Guarantee 7: the replay line prints exactly the token the header printed. *)
  let token = Seed.to_string Fixtures.root in
  let t = transcript () in
  contains ~msg:"header carries the root token"
    ~sub:(Printf.sprintf "(seed %s)" token)
    t;
  contains ~msg:"replay line carries exactly the header token"
    ~sub:(Printf.sprintf "replay: WINDTRAP_SEED=%s " token)
    t

let test_duration_forms () =
  let line duration =
    with_renderer ~mode:`Verbose (fun r ->
        Report.result r (Fixtures.result [ "t" ] Failure.Pass ~duration))
  in
  let summary duration =
    with_renderer (fun r ->
        Report.finish r ~release_failures:[]
          ~results:[ Fixtures.result [ "t" ] Failure.Pass ]
          ~duration ())
  in
  (* One format in every measured slot: a verbose row and the summary
     print the same bytes for the same duration, rounded before the unit
     is chosen. *)
  List.iter
    (fun (secs, form) ->
      contains
        ~msg:(Printf.sprintf "a row prints %gs as %s" secs form)
        ~sub:("  " ^ form ^ "\n")
        (line secs);
      contains
        ~msg:(Printf.sprintf "the summary prints %gs as %s" secs form)
        ~sub:(" in " ^ form ^ ".\n")
        (summary secs))
    [
      (2e-05, "0.0ms");
      (0.00046, "0.5ms");
      (0.00994, "9.9ms");
      (0.00996, "10ms");
      (0.06, "60ms");
      (0.9994, "999ms");
      (0.9996, "1.0s");
      (6.5, "6.5s");
      (119.6, "119.6s");
      (5400.0, "5400.0s");
      (0., "0.0ms");
      (0.00995, "10ms");
      (0.9995, "1.0s");
    ];
  (* Around both bucket edges, microsecond by microsecond: the unit is
     chosen after rounding, so no duration prints as [10.0ms] or [1000ms]. *)
  let edge first last =
    List.for_all
      (fun us ->
        let line = summary (float_of_int us /. 1e6) in
        not (has ~sub:" in 10.0ms." line || has ~sub:" in 1000ms." line))
      (List.init (last - first + 1) (fun i -> first + i))
  in
  is_true ~msg:"no 10.0ms around 9.95 ms" (edge 9_900 10_050);
  is_true ~msg:"no 1000ms around 999.5 ms" (edge 999_000 1_000_600)

let test_create_validation () =
  let raises fn =
    match fn () with
    | (_ : Report.t) -> false
    | exception Invalid_argument _ -> true
  in
  let ppf = Format.formatter_of_buffer (Buffer.create 8) in
  is_true ~msg:"create: negative slow_threshold rejected"
    (raises (fun () ->
         Report.create ~out:ppf ~ansi:false (config ~slow_threshold:(-1.0) ())));
  is_true ~msg:"create: non-finite slow_threshold rejected"
    (raises (fun () ->
         Report.create ~out:ppf ~ansi:false
           (config ~slow_threshold:Float.nan ())))

(* [--stream] changes no line of the transcript: it has the compact shape,
   and rows are [-v]'s. The order against a streamed test's bytes is the
   cram's to pin (test/cli/report.t), where they leave a real process. *)
let test_stream_shape () =
  let streamed ?(verbose = false) results =
    let buf = Buffer.create 256 in
    let ppf = Format.formatter_of_buffer buf in
    let r =
      Report.create ~out:ppf ~ansi:false
        { (config ()) with Run.stream = true; verbose }
    in
    Report.header r ~suite:"s" ~tests:(List.length results) ~seed:None ();
    List.iter (Report.result r) results;
    Report.finish r ~release_failures:[] ~results ~duration:0.5 ();
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  let pass = Fixtures.result [ "ok" ] Failure.Pass in
  let bad = Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "b" ]) in
  let tail =
    let buf = Buffer.create 64 in
    let ppf = Format.formatter_of_buffer buf in
    let r =
      Report.create ~out:ppf ~ansi:true ~terminal:true
        { (config ()) with Run.stream = true }
    in
    Report.begin_test r ~path:[ "ok" ];
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  equal ~msg:"no live tail for a streamed test's bytes to land on" string ""
    tail;
  equal ~msg:"a green streamed run is the one line" string
    "s: 1 passed in 500ms.\n" (streamed [ pass ]);
  equal ~msg:"a streamed run's failures are the compact section" string
    ("s: 2 tests\n" ^ failures_rule ^ "\n  FAIL  bad\n    b\n" ^ closing_rule
   ^ "\n\n1 passed, 1 failed in 500ms.\n")
    (streamed [ pass; bad ]);
  equal ~msg:"rows are -v's, under --stream too" string
    "s: 1 test\n\
    \  PASS  ok                                         0.2ms\n\
     1 passed in 500ms.\n"
    (streamed ~verbose:true [ pass ])

(* A signal ends the transcript on what the run knows: the line on stderr
   (this test's capture holds it), the summary on the renderer. *)
let test_interrupted () =
  let pass = Fixtures.result [ "g"; "ok" ] Failure.Pass in
  let transcript ?mode running =
    with_renderer ?mode (fun r ->
        Report.header r ~suite:"s" ~tests:4 ~seed:None ();
        Report.result r pass;
        Report.interrupted r ~running ~results:[ pass ] ~duration:0.5 ())
  in
  equal ~msg:"the summary counts the stopped test among the not run" string
    "s: 1 passed, 3 not run in 500ms.\n"
    (transcript (Some [ "g"; "sleeps\n" ]));
  equal ~msg:"stderr names the stopped test, its control byte escaped" string
    "windtrap: interrupted in g \u{203a} sleeps\\x0a\n" (output ());
  equal ~msg:"a verbose run keeps its rows above the summary" string
    "s: 4 tests\n\
    \  PASS  g \u{203a} ok                                     0.2ms\n\
     1 passed, 3 not run in 500ms.\n"
    (transcript ~mode:`Verbose None);
  equal ~msg:"with no test running it says so" string
    "windtrap: interrupted between tests\n" (output ());
  let stopped_first =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:4 ~seed:None ();
        Report.interrupted r ~releasing:"fixture (db.ml:3)\027[31m"
          ~running:None ~results:[] ~duration:0.5 ())
  in
  equal ~msg:"a run stopped before its first result is its not-run count" string
    "s: 4 not run in 500ms.\n" stopped_first;
  equal ~msg:"a stopped release is named, its control byte escaped" string
    "windtrap: interrupted while releasing fixture (db.ml:3)\\x1b[31m\n"
    (output ())

(* The selection in words that hold whichever layer set it, a flag or a
   mirror: the empty run's sentence is read, and its values retyped. *)
let test_selection_description () =
  let describe config = Report.selection_description ~focused:false config in
  let base = Run.default_config () in
  is_true ~msg:"nothing narrows a default run" (describe base = None);
  equal ~msg:"every part named, the last joined with and" string
    "filter \"pars er\", exclusion \"it's\", tag \"a\", \"b c\", excluded tag \
     \"d\", --failed and shard 1/3"
    (Option.get
       (describe
          {
            base with
            Run.filter = [ "pars er" ];
            exclude = [ "it's" ];
            tags = [ "a"; "b c" ];
            exclude_tags = [ "d" ];
            failed_only = true;
            shard = Some (1, 3);
          }));
  equal ~msg:"a control byte is escaped, so the line stays one" string
    "filter \"a\\nb\""
    (Option.get (describe { base with Run.filter = [ "a\nb" ] }));
  (* Patterns widen: a test is kept by any one, and the sentence says
     "or" where the tags, which a test must all carry, are listed. *)
  equal ~msg:"several patterns, each named, joined with or" (option string)
    (Some {|filter "a" or "b" and exclusion "c" or "d"|})
    (describe { base with Run.filter = [ "a"; "b" ]; exclude = [ "c"; "d" ] });
  equal ~msg:"the empty selection names every pattern" (option string)
    (Some {|filter "a" or "b" matched none of 5 tests|})
    (Report.empty_selection_reason ~declared:5
       ~selection:(describe { base with Run.filter = [ "a"; "b" ] }));
  (* A focus narrows from the source, so the empty run names it too, first,
     and a focus alone is a selection. *)
  equal ~msg:"a focus is named first" (option string)
    (Some {|focus and filter "a"|})
    (Report.selection_description ~focused:true
       { base with Run.filter = [ "a" ] });
  equal ~msg:"a focus alone is a selection" (option string) (Some "focus")
    (Report.selection_description ~focused:true base)

let test_no_tests () =
  (* No header, so no selection and no declared count: nothing to say
     beyond the fact. *)
  let t =
    with_renderer (fun r ->
        Report.finish r ~release_failures:[] ~results:[] ~duration:0.01 ())
  in
  equal ~msg:"finish: empty run" string "no tests ran.\n" t;
  (* A suite that declares nothing is not a mistyped filter. *)
  let declares_none =
    with_renderer (fun r ->
        Report.header r ~suite:"mylib" ~tests:0 ~declared:0 ~seed:None ();
        Report.finish r ~release_failures:[] ~results:[] ~duration:0.01 ())
  in
  equal ~msg:"empty suite names itself as the cause" string
    "mylib: no tests ran: the suite declares none.\n" declares_none;
  (* A selection that matched nothing names itself and the denominator,
     and points at the way to see what there was: the one line allowed
     after an outcome. A build action has no launcher to restate and names
     the flag; a suite that declares nothing has nothing to list. *)
  let filtered invocation =
    with_renderer ~invocation (fun r ->
        Report.header r ~suite:"mylib" ~tests:0 ~declared:48
          ~selection:{|filter "parsr"|} ~seed:None ();
        Report.finish r ~release_failures:[] ~results:[] ~duration:0.01 ())
  in
  equal ~msg:"empty selection names the selection, the total and the way out"
    string
    "mylib: no tests ran: filter \"parsr\" matched none of 48 tests.\n\
     list: ./t.exe -l\n"
    (filtered (`Exe "./t.exe"));
  equal ~msg:"a build action's empty selection names the flag" string
    "mylib: no tests ran: filter \"parsr\" matched none of 48 tests.\n\
     (list the suite's tests with -l)\n"
    (filtered `Mirrors);
  equal ~msg:"nor has a suite that declares nothing" string
    "mylib: no tests ran: the suite declares none.\n"
    (with_renderer ~invocation:(`Exe "./t.exe") (fun r ->
         Report.header r ~suite:"mylib" ~tests:0 ~declared:0 ~seed:None ();
         Report.finish r ~release_failures:[] ~results:[] ~duration:0.01 ()));
  let singular =
    with_renderer (fun r ->
        Report.header r ~suite:"mylib" ~tests:0 ~declared:1
          ~selection:"tag \"slow\"" ~seed:None ();
        Report.finish r ~release_failures:[] ~results:[] ~duration:0.01 ())
  in
  contains ~msg:"one declared test is not \"1 tests\""
    ~sub:"matched none of 1 test." singular

(* What compact commits as the run happens

   Nothing for a result that did not count as failed; for one that did,
   its block, when the test finishes: a run that dies has printed what it
   knew. Driven through the event interface, and read off the sink without
   flushing it here, so the order of the bytes and the flush are both
   pinned. *)

let test_compact_is_silent_per_test () =
  let silent results =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:(List.length results) ~seed:None ();
        List.iter (Report.result r) results)
  in
  equal ~msg:"compact: a pass prints nothing" string ""
    (silent [ Fixtures.result [ "t" ] Failure.Pass ]);
  equal ~msg:"compact: a skip prints nothing" string ""
    (silent [ Fixtures.result [ "t" ] (Failure.Skip None) ]);
  equal ~msg:"compact: an excused failure prints nothing" string ""
    (silent [ Fixtures.excused_result ])

let test_compact_commits_blocks () =
  let buf = Buffer.create 256 in
  let r =
    Report.create ~out:(Format.formatter_of_buffer buf) ~ansi:false (config ())
  in
  let observe = Report.observe r ~seed:Fixtures.root ~selection:None in
  let committed () = Buffer.contents buf in
  let pass = Fixtures.result [ "ok" ] Failure.Pass in
  let bad name =
    Fixtures.result [ name ] (Failure.Fail [ Failure.message "boom" ])
  in
  let block name = Printf.sprintf "  FAIL  %s\n    boom\n" name in
  observe
    (Run.Run_started
       { suite = "s"; total = 4; selected = 4; properties = false });
  observe (Run.Test_started { path = [ "ok" ] });
  observe (Run.Test_finished pass);
  equal ~msg:"the start of the run and a pass commit nothing" string ""
    (committed ());
  observe (Run.Test_started { path = [ "first" ] });
  observe (Run.Test_finished (bad "first"));
  let first = "s: 4 tests\n" ^ failures_rule ^ "\n" ^ block "first" in
  equal
    ~msg:
      "a failure commits the header, the opening rule and its block when its \
       test finishes, flushed"
    string first (committed ());
  observe (Run.Test_finished (bad "second"));
  let second = first ^ "\n" ^ block "second" in
  equal
    ~msg:"the next block follows one blank line; the opening rule prints once"
    string second (committed ());
  is_true ~msg:"the opening rule prints once, before the first block"
    (occurrences_of ~sub:failures_rule (committed ()) = 1);
  not_contains ~msg:"the closing rule is the end of the run's, not a block's"
    ~sub:closing_rule (committed ());
  (* A fixture release that raised reaches [finish] alone, after the last
     test and with no event. *)
  let release =
    Failure.with_phase Failure.Release
      (Failure.message
         ~loc:(Fixtures.loc "test/t.ml" 4)
         "db: release raised Exit")
  in
  Report.finish r
    ~results:[ pass; bad "first"; bad "second" ]
    ~release_failures:[ release ] ~duration:0.0042 ();
  equal
    ~msg:
      "finish adds the release's block, the closing rule, then the summary: \
       one test of the four never ran"
    string
    (second
   ^ "\n\
     \  FAIL  fixture release\n\
     \    [release] test/t.ml:4\n\
     \    db: release raised Exit\n" ^ closing_rule
   ^ "\n\n1 passed, 3 failed, 1 not run in 4.2ms.\n")
    (committed ());
  let exe =
    with_renderer ~invocation:(`Exe "./t.exe") (fun r ->
        Report.finish r ~results:[] ~release_failures:[ release ]
          ~duration:0.001 ())
  in
  contains ~msg:"a release block ends on its facts: no command closes it"
    ~sub:("\n    db: release raised Exit\n" ^ closing_rule ^ "\n")
    exe;
  not_contains ~msg:"no block carries a rerun hint" ~sub:"rerun" exe

(* Under [-v] a failed test's row is its block's title: the block's lines
   are committed under it when the test finishes, and no section repeats
   them at the end. *)
let test_verbose_commits_blocks () =
  let buf = Buffer.create 256 in
  let r =
    Report.create
      ~out:(Format.formatter_of_buffer buf)
      ~ansi:false (config ~mode:`Verbose ())
  in
  let observe = Report.observe r ~seed:Fixtures.root ~selection:None in
  let committed () = Buffer.contents buf in
  let pass = Fixtures.result [ "ok" ] Failure.Pass in
  let bad name =
    Fixtures.result [ name ] (Failure.Fail [ Failure.message "boom" ])
  in
  let row tag name = Printf.sprintf "  %s  %-41s  0.2ms\n" tag name in
  let block name = row "FAIL" name ^ "    boom\n\n" in
  observe
    (Run.Run_started
       { suite = "s"; total = 4; selected = 4; properties = false });
  equal ~msg:"the header is committed at the start, flushed" string
    "s: 4 tests\n" (committed ());
  observe (Run.Test_finished pass);
  let passed = "s: 4 tests\n" ^ row "PASS" "ok" in
  equal ~msg:"a pass commits its row" string passed (committed ());
  observe (Run.Test_finished (bad "first"));
  let first = passed ^ block "first" in
  equal
    ~msg:
      "a failure commits its row and, under it, its block's lines when its \
       test finishes, flushed"
    string first (committed ());
  observe (Run.Test_finished (bad "second"));
  let second = first ^ block "second" in
  equal
    ~msg:
      "a blank line closes each block; no heading and no rule separates two \
       rows"
    string second (committed ());
  let release =
    Failure.with_phase Failure.Release
      (Failure.message
         ~loc:(Fixtures.loc "test/t.ml" 4)
         "db: release raised Exit")
  in
  Report.finish r
    ~results:[ pass; bad "first"; bad "second" ]
    ~release_failures:[ release ] ~duration:0.0042 ();
  equal
    ~msg:
      "finish adds the release's title with its block and no duration, then \
       the summary: no failures section repeats the blocks"
    string
    (second ^ "  FAIL  fixture release\n"
   ^ "    [release] test/t.ml:4\n\
     \    db: release raised Exit\n\n\
      1 passed, 3 failed, 1 not run in 4.2ms.\n")
    (committed ());
  not_contains ~msg:"verbose draws no rule" ~sub:"\u{2500}" (committed ());
  let armed =
    with_renderer ~mode:`Verbose ~ansi:true ~invocation:(`Exe "./t.exe")
      ~armed:"lib/calc.ml:9:12:add" (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r
          (Fixtures.result [ "first" ] (Failure.Fail [ Fixtures.prop_failure ])))
  in
  contains
    ~msg:
      "the row is a title: its path bold, and (mutant armed) after its \
       duration in an armed run"
    ~sub:
      ("  \027[31mFAIL\027[0m  \027[1mfirst\027[0m" ^ String.make 38 ' '
     ^ "\027[2m0.2ms\027[0m \027[2m(mutant armed)\027[0m")
    armed;
  contains ~msg:"its hints are the block's, and the blank line closes it"
    ~sub:
      "    replay: ./t.exe --arm lib/calc.ml:9:12:add --seed \
       s1:7be1d2c904aa31f5 -f 'first'\n\n"
    armed;
  (* A missing baseline is the row's qualifier, sharing the slot. *)
  let missing ?armed () =
    with_renderer ~mode:`Verbose ?armed (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r
          (Fixtures.result [ "help" ] (Failure.Fail [ Fixtures.snap_missing ])))
  in
  contains
    ~msg:
      "a missing baseline qualifies the row, in parentheses after its duration"
    ~sub:"0.2ms (no baseline)\n" (missing ());
  not_contains ~msg:"and no dash does" ~sub:"\u{2014}" (missing ());
  contains ~msg:"the armed qualifier shares the slot"
    ~sub:"0.2ms (no baseline, mutant armed)\n"
    (missing ~armed:"lib/calc.ml:9:12:add" ())

let test_note () =
  (* Run-scoped notices (fixture releases) land between results. Compact
     prints nothing per test, so the notice is an erasable live line and
     never part of the transcript. A green run keeps its one line and a
     noteworthy one its blocks; verbose prints it in position. *)
  let green =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:2 ~seed:None ();
        Report.result r (Fixtures.result [ "a" ] Failure.Pass);
        Report.result r (Fixtures.result [ "b" ] Failure.Pass);
        Report.note r "releasing db";
        Report.finish r ~release_failures:[]
          ~results:
            [
              Fixtures.result [ "a" ] Failure.Pass;
              Fixtures.result [ "b" ] Failure.Pass;
            ]
          ~duration:0.01 ())
  in
  equal ~msg:"note: a green compact run stays one line" string
    "s: 2 passed in 10ms.\n" green;
  let noteworthy =
    let results =
      [
        Fixtures.result [ "a" ] Failure.Pass;
        Fixtures.result [ "b" ] (Failure.Fail [ Failure.message "boom" ]);
      ]
    in
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:2 ~seed:None ();
        Report.result r (List.nth results 0);
        Report.note r "releasing db";
        Report.result r (List.nth results 1);
        Report.finish r ~release_failures:[] ~results ~duration:0.01 ())
  in
  not_contains ~msg:"note: a compact transcript never carries the notice"
    ~sub:"releasing" noteworthy;
  is_true ~msg:"note: the compact transcript opens with the header"
    (String.starts_with
       ~prefix:("s: 2 tests\n" ^ failures_rule ^ "\n  FAIL  b\n")
       noteworthy);
  let verbose =
    with_renderer ~mode:`Verbose (fun r -> Report.note r "releasing db")
  in
  equal ~msg:"note: verbose prints the plain line" string "releasing db\n"
    verbose;
  let live =
    with_renderer ~ansi:true ~terminal:true (fun r ->
        Report.header r ~suite:"s" ~tests:2 ~seed:None ();
        Report.result r (Fixtures.result [ "a" ] Failure.Pass);
        Report.begin_test r ~path:[ "b" ];
        Report.note r "releasing db";
        Report.finish r ~release_failures:[]
          ~results:[ Fixtures.result [ "a" ] Failure.Pass ]
          ~duration:0.01 ())
  in
  contains ~msg:"note: the live tail is erased, the notice drawn erasable"
    ~sub:"\r\027[2K\027[2mreleasing db\027[0m" live;
  contains ~msg:"note: the erasable notice is erased before the one-liner"
    ~sub:"releasing db\027[0m\r\027[2Ks: \027[32m1 passed" live

(* The summary's terms, in their order, zero terms omitted. [not run] is
   what a stopped run ([-x]) selected and never reached. *)

let test_summary_terms () =
  let bad = Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "b" ]) in
  let stopped =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:5 ~seed:None ();
        Report.result r bad;
        Report.finish r ~release_failures:[] ~results:[ bad ] ~duration:0.0004
          ())
  in
  is_true ~msg:"a stopped run counts what it never reached"
    (String.ends_with ~suffix:"\n\n1 failed, 4 not run in 0.4ms.\n" stopped);
  let complete =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r bad;
        Report.finish r ~release_failures:[] ~results:[ bad ] ~duration:0.0004
          ())
  in
  not_contains ~msg:"a run that reached every selected test omits the term"
    ~sub:"not run" complete;
  let root = temp_dir () in
  let baselines = Baseline.create ~root ~cwd:root ~mode:Baseline.Corrected () in
  (try Baseline.check baselines (Baseline.File "help.expected") "hello\n"
   with Failure.Check_failure _ -> ());
  ignore (Baseline.settle baselines ~keep:true);
  Baseline.write baselines;
  let results =
    [
      Fixtures.result [ "ok" ] Failure.Pass;
      Fixtures.result [ "flaky" ] Failure.Pass ~attempts:2;
      Fixtures.result [ "skipped" ] (Failure.Skip None);
      Fixtures.excused_result;
      Fixtures.result [ "sub" ]
        (Failure.Fail [ Fixtures.subtest_failure "shape [0]" ]);
    ]
  in
  let every =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:7 ~seed:None ();
        List.iter (Report.result r) results;
        Report.finish r ~release_failures:[] ~results ~duration:6.5 ~baselines
          ())
  in
  is_true ~msg:"every term, in order, the summary last"
    (String.ends_with
       ~suffix:
         "\n\n\
          2 passed (1 flaky), 1 skipped, 1 expected failure, 1 failed (1 \
          subtest failure), 2 not run, 1 correction written in 6.5s.\n"
       every);
  let excused =
    [
      Fixtures.excused_result;
      { Fixtures.excused_result with Run.path = [ "also excused" ] };
    ]
  in
  equal ~msg:"zero terms are omitted, [passed] included; a plural term" string
    "s: 2 expected failures in 1.0ms.\n"
    (with_renderer (fun r ->
         Report.header r ~suite:"s" ~tests:2 ~seed:None ();
         Report.finish r ~release_failures:[] ~results:excused ~duration:0.001
           ()))

(* When a compact run prints more than its summary line *)

let test_compact_green_one_liner () =
  let passes = [ Fixtures.result [ "a" ] Failure.Pass ] in
  let named =
    with_renderer (fun r ->
        Report.header r ~suite:"mylib" ~tests:1 ~seed:None ();
        List.iter (Report.result r) passes;
        Report.finish r ~release_failures:[] ~results:passes ~duration:1.2 ())
  in
  equal ~msg:"green compact run: exactly one named line" string
    "mylib: 1 passed in 1.2s.\n" named;
  let seeded =
    with_renderer (fun r ->
        Report.header r ~suite:"mylib" ~tests:1 ~seed:(Some Fixtures.root) ();
        List.iter (Report.result r) passes;
        Report.finish r ~release_failures:[] ~results:passes ~duration:1.2 ())
  in
  equal ~msg:"green compact run: the seed the header carried is appended" string
    "mylib: 1 passed in 1.2s (seed s1:7be1d2c904aa31f5).\n" seeded;
  let segments =
    let results =
      [
        Fixtures.result [ "a" ] Failure.Pass;
        Fixtures.result [ "s" ] (Failure.Skip None);
        Fixtures.excused_result;
      ]
    in
    with_renderer (fun r ->
        Report.header r ~suite:"mylib" ~tests:3 ~seed:None ();
        Report.result r (List.nth results 0);
        Report.result r (List.nth results 1);
        Report.result r (List.nth results 2);
        Report.finish r ~release_failures:[] ~results ~duration:0.2 ())
  in
  equal
    ~msg:
      "green compact run: skip and expected-failure segments stay on the line"
    string "mylib: 1 passed, 1 skipped, 1 expected failure in 200ms.\n" segments;
  let empty =
    with_renderer (fun r ->
        Report.header r ~suite:"mylib" ~tests:0 ~seed:None ();
        Report.finish r ~release_failures:[] ~results:[] ~duration:0.01 ())
  in
  (* [~declared] defaults to [~tests], which is 0 here: the suite really
     does declare nothing. *)
  equal ~msg:"empty compact selection: one named line, no header" string
    "mylib: no tests ran: the suite declares none.\n" empty

let test_compact_slow_trigger () =
  let slow_pass = Fixtures.result [ "t" ] Failure.Pass ~duration:1.2 in
  let t =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r slow_pass;
        Report.finish r ~release_failures:[] ~results:[ slow_pass ]
          ~duration:1.2 ())
  in
  equal
    ~msg:
      "an untagged over-threshold pass is noteworthy: header, then the section \
       with its threshold, and no advice line"
    string
    "s: 1 test\nslow tests (1, over 1s):\n  1.2s  t\n\n1 passed in 1.2s.\n" t;
  let at_threshold =
    let r1 = Fixtures.result [ "t" ] Failure.Pass ~duration:1.0 in
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r r1;
        Report.finish r ~release_failures:[] ~results:[ r1 ] ~duration:1.0 ())
  in
  is_true ~msg:"the threshold is inclusive (duration >= threshold)"
    (String.starts_with
       ~prefix:"s: 1 test\nslow tests (1, over 1s):\n  1.0s  t\n" at_threshold);
  (* The threshold is configured, not measured: it prints as it was
     written, in seconds, never in the measured format or an exponent. *)
  List.iter
    (fun (slow_threshold, written) ->
      contains
        ~msg:
          (Printf.sprintf "a threshold of %s seconds prints as given" written)
        ~sub:(Printf.sprintf "slow tests (1, over %ss):\n" written)
        (with_renderer ~slow_threshold (fun r ->
             Report.finish r ~release_failures:[] ~results:[ slow_pass ]
               ~duration:1.2 ())))
    [ (0.01, "0.01"); (0.5, "0.5"); (1e-9, "0.000000001") ];
  let tagged_pass = { slow_pass with Run.slow_tagged = true } in
  let tagged =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r tagged_pass;
        Report.finish r ~release_failures:[] ~results:[ tagged_pass ]
          ~duration:1.2 ())
  in
  equal ~msg:"a slow-tagged test is exempt everywhere: one line, no warning"
    string "s: 1 passed in 1.2s.\n" tagged;
  let skip = Fixtures.result [ "t" ] (Failure.Skip None) ~duration:2.0 in
  let skipped =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r skip;
        Report.finish r ~release_failures:[] ~results:[ skip ] ~duration:2.0 ())
  in
  equal ~msg:"a skip never triggers the threshold" string
    "s: 1 skipped in 2.0s.\n" skipped;
  (* An excused expected failure is not a counted failure, but its
     duration still counts against the threshold when untagged. *)
  let excused_fast =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r Fixtures.excused_result;
        Report.finish r ~release_failures:[]
          ~results:[ Fixtures.excused_result ]
          ~duration:0.1 ())
  in
  equal ~msg:"an excused failure alone is not noteworthy" string
    "s: 1 expected failure in 100ms.\n" excused_fast

let test_slow_duration_semantics () =
  (* The compared duration is [Run.result.duration], the attempts summed
     (run.mli), so a retried test whose attempts together cross the
     threshold is slow even when its final attempt was fast. *)
  let retried =
    Fixtures.result [ "flaky" ] Failure.Pass ~duration:1.2 ~attempts:3
  in
  let t =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r retried;
        Report.finish r ~release_failures:[] ~results:[ retried ] ~duration:1.2
          ())
  in
  is_true ~msg:"a retried test is noteworthy on its summed duration"
    (String.starts_with ~prefix:"s: 1 test\n" t);
  contains ~msg:"the warning shows the summed duration"
    ~sub:"slow tests (1, over 1s):\n  1.2s  flaky\n" t;
  (* A slow test that also fails: one block and one warning (they report
     different things) and the summary counts the failure once. *)
  let slow_fail =
    Fixtures.result [ "boom" ]
      (Failure.Fail [ Failure.message "b" ])
      ~duration:2.0
  in
  let t =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r slow_fail;
        Report.finish r ~release_failures:[] ~results:[ slow_fail ]
          ~duration:2.0 ())
  in
  contains ~msg:"a slow failing test keeps its failure block"
    ~sub:(failures_rule ^ "\n  FAIL  boom\n")
    t;
  contains
    ~msg:
      "the section follows the closing rule and a blank line, before the \
       summary"
    ~sub:
      ("    b\n" ^ closing_rule
     ^ "\n\nslow tests (1, over 1s):\n  2.0s  boom\n\n1 failed in 2.0s.\n")
    t;
  contains ~msg:"the failure is counted once" ~sub:"\n1 failed in 2.0s.\n" t;
  is_true ~msg:"exactly one warning line for the slow failure"
    (occurrences_of ~sub:"  2.0s  boom" t = 1)

let test_slow_threshold_zero () =
  let slow_pass = Fixtures.result [ "t" ] Failure.Pass ~duration:5.0 in
  let t =
    with_renderer ~slow_threshold:0.0 (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r slow_pass;
        Report.finish r ~release_failures:[] ~results:[ slow_pass ]
          ~duration:5.0 ())
  in
  equal ~msg:"threshold 0 disables the trigger and the warnings" string
    "s: 1 passed in 5.0s.\n" t;
  let still_noteworthy =
    let fail = Fixtures.result [ "t" ] (Failure.Fail [ Failure.message "x" ]) in
    with_renderer ~slow_threshold:0.0 (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r fail;
        Report.finish r ~release_failures:[] ~results:[ fail ] ~duration:0.1 ())
  in
  is_true ~msg:"threshold 0 still makes a counted failure noteworthy"
    (String.starts_with
       ~prefix:("s: 1 test\n" ^ failures_rule ^ "\n")
       still_noteworthy)

let test_verbose_slow_warnings () =
  (* Verbose gains the section (before the summary, which stays the last
     line); a green verbose run still streams everything. *)
  let slow_pass = Fixtures.result [ "t" ] Failure.Pass ~duration:1.5 in
  let t =
    with_renderer ~mode:`Verbose (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r slow_pass;
        Report.finish r ~release_failures:[] ~results:[ slow_pass ]
          ~duration:1.5 ())
  in
  contains ~msg:"verbose: header and status line stream as always"
    ~sub:"s: 1 test\n  PASS  t" t;
  contains ~msg:"verbose: the slow section before the summary"
    ~sub:"\nslow tests (1, over 1s):\n  1.5s  t\n\n" t;
  is_true ~msg:"verbose: the summary is the last line"
    (String.ends_with ~suffix:"  1.5s  t\n\n1 passed in 1.5s.\n" t);
  let tagged_pass = { slow_pass with Run.slow_tagged = true } in
  let tagged =
    with_renderer ~mode:`Verbose (fun r ->
        Report.result r tagged_pass;
        Report.finish r ~release_failures:[] ~results:[ tagged_pass ]
          ~duration:1.5 ())
  in
  not_contains ~msg:"verbose: slow-tagged tests warn nowhere"
    ~sub:"slow tests (" tagged

(* The flaky block *)

let test_flaky_block () =
  (* A pass on retry is never silent: the run is noteworthy, the section
     names the test with the attempt it passed on, between the slow section
     and the summary, and the summary counts it as passed and says so. *)
  let flaky =
    Fixtures.result
      [ "network"; "fetches the manifest" ]
      Failure.Pass ~attempts:2
  in
  let steady = Fixtures.result [ "steady" ] Failure.Pass in
  let t =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:2 ~seed:None ();
        Report.result r steady;
        Report.result r flaky;
        Report.finish r ~release_failures:[] ~results:[ steady; flaky ]
          ~duration:0.3 ())
  in
  equal ~msg:"a flaky pass is noteworthy: header, section, summary term" string
    "s: 2 tests\n\
     flaky tests (1):\n\
    \  passed on attempt 2  network › fetches the manifest\n\n\
     2 passed (1 flaky) in 300ms.\n"
    t;
  (* Between the slow block and the summary, after the failure section. *)
  let slow = Fixtures.result [ "slow one" ] Failure.Pass ~duration:1.5 in
  let bad = Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "b" ]) in
  let ordered =
    with_renderer (fun r ->
        Report.finish r ~release_failures:[] ~results:[ bad; slow; flaky ]
          ~duration:2.0 ())
  in
  contains ~msg:"the flaky section follows the slow section"
    ~sub:
      "slow tests (1, over 1s):\n\
      \  1.5s  slow one\n\n\
       flaky tests (1):\n\
      \  passed on attempt 2  network › fetches the manifest\n\n\
       2 passed (1 flaky), 1 failed in 2.0s.\n"
    ordered;
  is_true ~msg:"the failure section precedes it, closed by its rule"
    (occurrences_of ~sub:(failures_rule ^ "\n") ordered = 1
    && occurrences_of ~sub:(closing_rule ^ "\n\n") ordered = 1
    && Text.first_occurrence ~pattern:(closing_rule ^ "\n") ordered
       < Text.first_occurrence ~pattern:"slow tests" ordered);
  (* A test that failed on every attempt is a failure, not a flake; a
     first-attempt pass is not one either. *)
  let hopeless =
    Fixtures.result [ "hopeless" ]
      (Failure.Fail [ Failure.message "still" ])
      ~attempts:3
  in
  let not_flaky =
    with_renderer (fun r ->
        Report.finish r ~release_failures:[] ~results:[ hopeless; steady ]
          ~duration:0.1 ())
  in
  not_contains ~msg:"a retried failure is not flaky" ~sub:"flaky tests"
    not_flaky;
  contains ~msg:"a retried failure keeps its attempt count in the block"
    ~sub:"  FAIL  hopeless (3 attempts)" not_flaky;
  (* Verbose keeps the block, and its status line already carried the
     count. *)
  let verbose =
    with_renderer ~mode:`Verbose (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r flaky;
        Report.finish r ~release_failures:[] ~results:[ flaky ] ~duration:0.3 ())
  in
  contains ~msg:"verbose: the status line carries the count"
    ~sub:"  PASS  network › fetches the manifest" verbose;
  contains ~msg:"verbose: the status line names the attempts"
    ~sub:"(2 attempts)" verbose;
  contains ~msg:"verbose: the block prints too, one blank line after the rows"
    ~sub:
      "0.2ms (2 attempts)\n\n\
       flaky tests (1):\n\
      \  passed on attempt 2  network › fetches the manifest\n\n\
       1 passed (1 flaky) in 300ms.\n"
    verbose;
  let colored =
    with_renderer ~ansi:true (fun r ->
        Report.finish r ~release_failures:[] ~results:[ flaky ] ~duration:0.3 ())
  in
  contains ~msg:"ansi: the flaky section wears the slow section's caution"
    ~sub:"\027[33mflaky tests (1):\027[0m\n" colored;
  contains ~msg:"ansi: the summary's flaky term is caution, beside the pass"
    ~sub:"\027[32m1 passed\027[0m \027[33m(1 flaky)\027[0m in 300ms." colored

(* Failure projections *)

let test_headline () =
  let h f = Report.headline f in
  is_true ~msg:"headline: equality"
    (h (Failure.equality ~expected:"true" ~actual:"false" ())
    = "expected true, got false");
  is_true ~msg:"headline: negated equality"
    (h (Failure.equality ~not_:true ~expected:"3" ~actual:"3" ())
    = "both sides equal: 3");
  is_true ~msg:"headline: raise both sides"
    (h (Failure.raised ~expected:"A" ~actual:"B" ())
    = "expected exception A, raised B");
  is_true ~msg:"headline: raise nothing raised"
    (h (Failure.raised ~expected:"A" ()) = "expected exception A, none raised");
  is_true ~msg:"headline: predicate miss"
    (h (Failure.raised ~actual:"B" ~predicate:true ())
    = "exception did not satisfy the predicate: B");
  is_true ~msg:"headline: a predicate that saw nothing raised"
    (h (Failure.raised ~predicate:true ())
    = "expected an exception, none raised");
  is_true ~msg:"headline: uncaught exception"
    (h (Failure.raised ~actual:"Not_found" ()) = "uncaught exception: Not_found");
  is_true ~msg:"headline: raise wanted any"
    (h (Failure.raised ()) = "expected an exception, none raised");
  is_true ~msg:"headline: a predicate's claim is the expected side"
    (h (Failure.predicate ~claim:"a power of two" "12")
    = "expected a power of two, got 12");
  is_true ~msg:"headline: equal renderings"
    (h (Failure.equality ~expected:"nan" ~actual:"nan" ())
    = "both sides render as: nan");
  is_true
    ~msg:"headline: a diff is a sentence counting the lines the block prints"
    (h (Failure.equality ~expected:"a\nb\nc" ~actual:"a\nB\nc" ())
    = "expected and actual differ (5 diff lines)");
  is_true ~msg:"headline: a newline-only difference is the block's sentence"
    (h (Failure.equality ~expected:"a\nb" ~actual:"a\nb\n" ())
    = "values differ only by a trailing newline (on the actual side)");
  is_true ~msg:"headline: file baseline missing"
    (h Fixtures.snap_missing = {|expect_file "test/help.expected": no baseline|});
  is_true ~msg:"headline: literal mismatch"
    (h Fixtures.snap_mismatch = "expect: mismatch");
  is_true ~msg:"headline: unresolvable"
    (h
       (Failure.baseline (Failure.File "../x")
          (Failure.Unresolvable { candidate = "/tmp/x" }))
    = {|expect_file "../x": cannot resolve the path under the project root|});
  is_true ~msg:"headline: property"
    (h Fixtures.prop_failure
   = "property failed (case 12, shrunk 4 steps): Rect (2, 0)");
  let one_step =
    Failure.property ~rendered:"0" ~case_index:0 ~shrink_steps:1
      ~root:Fixtures.root ~examples:false ()
  in
  is_true ~msg:"headline: one shrink step is singular"
    (h one_step = "property failed (case 0, shrunk 1 step): 0");
  contains ~msg:"and so it is in the block"
    ~sub:"    counterexample (case 0, shrunk 1 step): 0\n"
    (failure_block one_step);
  is_true ~msg:"headline: a pre-image is marked as the block marks it"
    (h
       (Failure.property ~rendered:"20" ~case_index:0 ~shrink_steps:0
          ~root:Fixtures.root ~examples:false ~rendering:Failure.Pre_image ())
    = "property failed (case 0): computed from 20");
  is_true
    ~msg:"headline: a summarized counterexample is its summary, not its table"
    (h
       (Failure.property ~summary:"2 calls, last: get"
          ~rendered:" #  call\n 1  inc\n 2  get" ~case_index:0 ~shrink_steps:2
          ~root:Fixtures.root ~examples:false ())
    = "property failed (case 0, shrunk 2 steps): 2 calls, last: get");
  is_true ~msg:"headline: message" (h (Failure.message "boom") = "boom");
  is_true ~msg:"headline: the user message leads, then a colon"
    (h
       {
         (Failure.equality ~expected:"1" ~actual:"2" ()) with
         Failure.msg = Some (Failure.text "deliberate");
       }
    = "deliberate: expected 1, got 2");
  contains ~msg:"headline: msg annotation prefixed" ~sub:"context: boom"
    (h
       {
         (Failure.message "boom") with
         Failure.msg = Some (Failure.text "context");
       });
  (* Two failing subtests of one test must not read the same. *)
  let labelled label =
    {
      (Failure.equality ~expected:"[1; 2]" ~actual:"[1; 3]" ()) with
      Failure.subtest = [ "contract"; label ];
    }
  in
  is_true ~msg:"headline: the subtest label leads"
    (h (labelled "shape [0]")
    = "contract \u{203a} shape [0]: expected [1; 2], got [1; 3]");
  is_true ~msg:"headline: sibling subtests differ by their label"
    (h (labelled "shape [0]") <> h (labelled "shape [2]"));
  is_true ~msg:"headline: the label, the user message, then the sentence"
    (h
       {
         (labelled "shape [0]") with
         Failure.msg = Some (Failure.text "deliberate");
       }
    = "contract \u{203a} shape [0]: deliberate: expected [1; 2], got [1; 3]");
  (* 80 code points, not bytes, then the ellipsis. *)
  let long = String.concat "" (List.init 300 (fun _ -> "\u{00e9}")) in
  let hl = h (Failure.equality ~expected:long ~actual:"y" ()) in
  is_true ~msg:"headline: cut at 80 code points, then an ellipsis"
    (hl
    = "expected "
      ^ String.concat "" (List.init 71 (fun _ -> "\u{00e9}"))
      ^ "\u{2026}");
  not_contains ~msg:"headline: no em dash" ~sub:"\u{2014}"
    (h
       {
         (Failure.message "boom") with
         Failure.msg = Some (Failure.text "context");
       });
  let multi = h (Failure.message "line one\nline two") in
  is_true ~msg:"headline: never multi-line" (not (String.contains multi '\n'));
  let esc = h (Failure.message "\027[31mred\027[0m alert") in
  is_true ~msg:"headline: a payload escape is left to the field's escaping"
    (esc = "\027[31mred\027[0m alert");
  is_true ~msg:"headline: empty message named"
    (h (Failure.message "") = "(empty failure message)");
  contains ~msg:"block: empty message named" ~sub:"(empty failure message)"
    (failure_block (Failure.message ""))

let test_property_projections () =
  let example =
    Failure.property ~rendered:"Rect (2, 0)" ~case_index:0 ~shrink_steps:0
      ~root:Fixtures.root ~examples:true ()
  in
  let b = failure_block example in
  contains ~msg:"example: numbered from one"
    ~sub:"counterexample (example 1): Rect (2, 0)" b;
  not_contains ~msg:"example: no replay line (examples always replay)"
    ~sub:"replay:" b;
  not_contains ~msg:"example: no seed token" ~sub:"WINDTRAP_SEED" b;
  (* A pre-image is marked in the slot, [computed from], and explained in
     the aside under it; a printing generator draws no aside at all. *)
  let pre_image =
    Failure.property ~rendered:"[2; 3] -> ([1.; 2.], [0.; 0.])" ~case_index:19
      ~shrink_steps:9 ~root:Fixtures.root ~examples:false
      ~rendering:Failure.Pre_image ()
  in
  contains ~msg:"pre-image: marked in the slot"
    ~sub:
      "counterexample (case 19, shrunk 9 steps): computed from [2; 3] -> ([1.; \
       2.], [0.; 0.])"
    (failure_block pre_image);
  contains
    ~msg:
      "pre-image: explained once under the counterexample, the remedy in the \
       aside, indented two"
    ~sub:
      "    counterexample (case 19, shrunk 9 steps): computed from [2; 3] -> \
       ([1.; 2.], [0.; 0.])\n\
      \      (the value has no printer, so this is the input that map and bind\n\
      \       computed it from; attach a printer with Gen.with_pp to see the \
       value)\n\
      \    replay: "
    (failure_block pre_image);
  contains ~msg:"pre-image: the aside is faint, line by line"
    ~sub:
      "\n\
      \      \027[2m(the value has no printer, so this is the input that map \
       and bind\027[0m\n\
      \      \027[2m computed it from; attach a printer with Gen.with_pp to \
       see the value)\027[0m\n"
    (failure_block ~ansi:true pre_image);
  not_contains ~msg:"pre-image: no em dash in the explanation" ~sub:"\u{2014}"
    (failure_block pre_image);
  not_contains ~msg:"a printing generator draws no note" ~sub:"no printer"
    (failure_block Fixtures.prop_failure);
  not_contains ~msg:"nor the remedy" ~sub:"Gen.with_pp"
    (failure_block Fixtures.prop_failure);
  let multi_pre_image =
    failure_block
      (Failure.property ~rendered:"1 ->\n  [2; 3]" ~case_index:3 ~shrink_steps:0
         ~root:Fixtures.root ~examples:false ~rendering:Failure.Pre_image ())
  in
  contains ~msg:"multi-line pre-image: marked head, block form"
    ~sub:
      "    counterexample (case 3): computed from\n\
      \      1 ->\n\
      \        [2; 3]\n\
      \      (the value has no printer, so this is the input that map and bind\n\
      \       computed it from; attach a printer with Gen.with_pp to see the \
       value)\n"
    multi_pre_image;
  let no_filter = failure_block Fixtures.prop_failure in
  contains ~msg:"replay without filter: seed only"
    ~sub:"replay: WINDTRAP_SEED=s1:7be1d2c904aa31f5 dune runtest" no_filter;
  let quoted = failure_block ~filter:"it's › tricky" Fixtures.prop_failure in
  contains ~msg:"replay filter is shell-quoted"
    ~sub:{|WINDTRAP_FILTER='it'\''s › tricky'|} quoted;
  (* A config-sourced count rides the payload and the replay line restates
     it. Replaying a late case needs at least as many cases as the failing
     run. A payload without a count (the declaration-site form) is pinned
     flagless just above. *)
  let counted =
    Failure.property ~count:1000 ~rendered:"0" ~case_index:499 ~shrink_steps:1
      ~root:Fixtures.root ~examples:false ()
  in
  contains ~msg:"config-sourced count: Mirrors replay restates the mirror"
    ~sub:
      "replay: WINDTRAP_SEED=s1:7be1d2c904aa31f5 WINDTRAP_PROP_COUNT=1000 dune \
       runtest"
    (failure_block counted);
  contains ~msg:"config-sourced count: Exe replay restates --prop-count"
    ~sub:
      "replay: ./t.exe --seed s1:7be1d2c904aa31f5 --prop-count 1000 -f 'late'"
    (failure_block ~invocation:(`Exe "./t.exe") ~filter:"late" counted);
  (* The seed and the count are the whole replay line: the shrink budget
     is fixed, so no clause restates it, even for a search that spent it. *)
  let spent =
    Failure.property ~count:1000 ~rendered:"0" ~case_index:499
      ~shrink_steps:10_000 ~shrink_end:Failure.Budget_spent ~root:Fixtures.root
      ~examples:false ()
  in
  contains ~msg:"a spent budget: the Mirrors replay line ends at the count"
    ~sub:
      "replay: WINDTRAP_SEED=s1:7be1d2c904aa31f5 WINDTRAP_PROP_COUNT=1000 dune \
       runtest\n"
    (failure_block spent);
  contains ~msg:"a spent budget: the Exe replay line ends at the filter"
    ~sub:
      "replay: ./t.exe --seed s1:7be1d2c904aa31f5 --prop-count 1000 -f 'late'\n"
    (failure_block ~invocation:(`Exe "./t.exe") ~filter:"late" spent);
  let multi =
    failure_block
      (Failure.property ~rendered:"Rect\n  (2, 0)" ~case_index:3 ~shrink_steps:0
         ~root:Fixtures.root ~examples:false ())
  in
  contains ~msg:"multi-line counterexample: block form"
    ~sub:"counterexample (case 3):\n      Rect\n        (2, 0)" multi;
  (* A summarized counterexample is a table: the summary on the head line,
     the table under it, its header row faint. *)
  let program =
    Failure.property ~summary:"2 calls, last: get"
      ~rendered:
        " #  model before  call\n 1  0             inc 3\n 2  3             get"
      ~case_index:0 ~shrink_steps:2 ~root:Fixtures.root ~examples:false ()
  in
  contains ~msg:"a summarized counterexample: summary on the head, then rows"
    ~sub:
      "    counterexample (case 0, shrunk 2 steps): 2 calls, last: get\n\
      \       #  model before  call\n\
      \       1  0             inc 3\n\
      \       2  3             get\n\
      \    replay: "
    (failure_block program);
  contains ~msg:"a summarized counterexample: only the header row is faint"
    ~sub:
      ": 2 calls, last: get\n\
      \      \027[2m #  model before  call\027[0m\n\
      \       1  0             inc 3\n\
      \       2  3             get\n"
    (failure_block ~ansi:true program)

let test_kind_details () =
  let b =
    failure_block (Failure.with_phase Failure.Teardown (Failure.message "x"))
  in
  contains ~msg:"a phase with nothing located is its tag, alone on the line"
    ~sub:"    [teardown]\n    x\n" b;
  let b =
    failure_block
      (Failure.equality ~msg:"context note" ~expected:"1" ~actual:"2" ())
  in
  contains ~msg:"msg annotation printed" ~sub:"    context note\n" b;
  let b =
    failure_block
      (Failure.baseline (Failure.File "../n.expected")
         (Failure.Unresolvable { candidate = "some/candidate" }))
  in
  equal
    ~msg:
      "unresolvable: the subject and the rule, then the candidate path and the \
       remedy naming the variable, and no command after it"
    string
    "    expect_file \"../n.expected\": the path cannot be proven to lie under \
     the project root\n\
    \    unverified path: some/candidate\n\
    \    (set WINDTRAP_PROJECT_ROOT to the directory the path is relative to)\n"
    b;
  not_contains
    ~msg:
      "unresolvable: never a claim about where the path lies, which can be \
       false"
    ~sub:"outside the project root" b;
  (* What the block composes on that line fits the width; the path may not. *)
  is_true
    ~msg:"unresolvable: the composed text of the remedy line fits 80 columns"
    (List.exists
       (fun line ->
         has ~sub:"unverified path:" line
         && String.length line - String.length "some/candidate" <= 80)
       (String.split_on_char '\n' b));
  not_contains ~msg:"unresolvable: no acceptance line" ~sub:"accept:" b;
  contains ~msg:"unresolvable: the remedy is a line of its own"
    ~sub:
      "\n\
      \    (set WINDTRAP_PROJECT_ROOT to the directory the path is relative to)\n"
    b;
  let b =
    failure_block
      (Failure.baseline
         (Failure.Literal { exact = false })
         (Failure.Unresolvable { candidate = "/elsewhere/t.ml" }))
  in
  contains ~msg:"unresolvable literal: the subject is expect"
    ~sub:"    expect: the path cannot be proven to lie under the project root\n"
    b;
  (* The first fact line names the verb that read the literal. *)
  let verb exact =
    failure_block
      (Failure.baseline
         (Failure.Literal { exact })
         (Failure.Mismatch
            { expected = Failure.text "a\n"; actual = Failure.text "b\n" }))
  in
  contains ~msg:"expect: the flexible verb, then the hunks with no head"
    ~sub:"    expect: mismatch\n    @@ -1,1 +1,1 @@\n    - a\n    + b\n"
    (verb false);
  contains ~msg:"expect_exact: the exact verb, then the hunks with no head"
    ~sub:"    expect_exact: mismatch\n    @@ -1,1 +1,1 @@\n    - a\n    + b\n"
    (verb true);
  not_contains ~msg:"a correction has no ---/+++ head" ~sub:"--- expected"
    (verb true);
  (* A line diff cannot show a trailing newline, the one difference that
     leaves [expect_exact] no hunk: the block says it in words. *)
  contains ~msg:"expect_exact: a newline-only difference is said in words"
    ~sub:
      "    expect_exact: mismatch\n\
      \    values differ only by a trailing newline (on the actual side)\n\
      \    accept: "
    (failure_block
       (Failure.baseline
          (Failure.Literal { exact = true })
          (Failure.Mismatch
             {
               expected = Failure.text "exact";
               actual = Failure.text "exact\n";
             })));
  is_true ~msg:"the headline is that first fact line"
    (Report.headline
       (Failure.baseline
          (Failure.Literal { exact = true })
          (Failure.Mismatch
             { expected = Failure.text "a\n"; actual = Failure.text "b\n" }))
    = "expect_exact: mismatch");
  let b =
    failure_block (Failure.equality ~expected:"a\nb\nc" ~actual:"a\nB\nc" ())
  in
  contains ~msg:"multi-line equality: unified diff header"
    ~sub:"--- expected\n    +++ actual\n" b;
  contains ~msg:"multi-line equality: hunk" ~sub:"@@ -1,3 +1,3 @@" b;
  contains ~msg:"multi-line equality: delete line" ~sub:"- b" b;
  contains ~msg:"multi-line equality: insert line" ~sub:"+ B" b

let test_degenerate_equalities () =
  (* Renderings line-equal but byte-different: the only such difference is a
     trailing newline, which a line diff cannot show; say so instead of
     printing an empty diff. *)
  let b =
    failure_block (Failure.equality ~expected:"a\nb" ~actual:"a\nb\n" ())
  in
  contains ~msg:"trailing-newline-only difference is stated, its side named"
    ~sub:"    values differ only by a trailing newline (on the actual side)\n" b;
  contains ~msg:"the other side is named when it holds the newline"
    ~sub:"    values differ only by a trailing newline (on the expected side)\n"
    (failure_block (Failure.equality ~expected:"a\nb\n" ~actual:"a\nb" ()));
  not_contains ~msg:"trailing-newline case prints no empty diff"
    ~sub:"--- expected" b;
  (* Renderings byte-equal while the equality distinguishes (lossy pp, e.g.
     [equal float nan nan]): two identical lines need an explanation. *)
  let b = failure_block (Failure.equality ~expected:"nan" ~actual:"nan" ()) in
  contains ~msg:"identical renderings print once, then say why"
    ~sub:
      "    both sides render as: nan\n\
      \    the printer shows less than the equality compares\n"
    b;
  not_contains ~msg:"identical renderings print no expected/actual pair"
    ~sub:"expected" b;
  (* Identical and multi-line: printed once, in block form. Inlining after
     an [expected] label would put continuation lines at column zero. *)
  let b =
    failure_block
      (Failure.equality ~expected:"line a\nline b" ~actual:"line a\nline b" ())
  in
  contains ~msg:"identical multi-line renderings print once, indented"
    ~sub:"    both sides render as:\n      line a\n      line b\n" b;
  contains ~msg:"identical multi-line explanation retained"
    ~sub:"      line b\n    the printer shows less than the equality compares\n"
    b;
  not_contains ~msg:"identical multi-line has no column-zero payload line"
    ~sub:"\nline b" b

let test_ansi_hygiene () =
  (* User pp output may carry raw escapes. The sink escapes every span, so
     under both [ansi] settings the transcript shows them and never obeys
     them, keeping every byte the value had: a comparison, a message, a
     test name and a captured tail alike. *)
  let esc = "\027[31mred\027[0m" in
  let f =
    Failure.equality ~expected:(esc ^ " one") ~actual:"\027]0;title\007 two" ()
  in
  let plain = failure_block f in
  not_contains ~msg:"ansi:false: no payload escape reaches a block" ~sub:"\027"
    plain;
  contains ~msg:"ansi:false: the payload's own bytes survive, escaped"
    ~sub:{|\x1b[31mred\x1b[0m one|} plain;
  contains ~msg:"ansi:false: an OSC payload survives the same way"
    ~sub:{|\x1b]0;title\x07 two|} plain;
  let colored = failure_block ~ansi:true (Failure.message (esc ^ " boom")) in
  not_contains ~msg:"ansi:true: payload escapes are not obeyed" ~sub:esc colored;
  contains ~msg:"ansi:true: payload escapes are shown"
    ~sub:{|\x1b[31mred\x1b[0m boom|} colored;
  let hostile_line =
    with_renderer ~mode:`Verbose (fun r ->
        Report.result r
          (Fixtures.result
             [ "suite"; esc ^ " name" ]
             (Failure.Fail [ Failure.message "boom" ])))
  in
  not_contains ~msg:"ansi:false: no escape of a name reaches its row"
    ~sub:"\027" hostile_line;
  contains ~msg:"ansi:false: the name's bytes survive, escaped"
    ~sub:{|\x1b[31mred\x1b[0m name|} hostile_line;
  let hostile_tail =
    let tail = Failure.tail ~log_path:"log" (esc ^ " captured\n") in
    let result =
      Fixtures.result [ "t" ]
        (Failure.Fail [ Failure.with_output_tail tail (Failure.message "boom") ])
    in
    with_renderer (fun r ->
        Report.finish r ~release_failures:[] ~results:[ result ] ~duration:0.01
          ())
  in
  not_contains ~msg:"ansi:false: no escape of a captured tail reaches it"
    ~sub:"\027" hostile_tail;
  contains ~msg:"ansi:false: the tail's bytes survive, escaped"
    ~sub:{|\x1b[31mred\x1b[0m captured|} hostile_tail

(* Control bytes on comparison surfaces

   A value carrying ESC drove the terminal instead of appearing in the
   report, and a grep for the reported bytes found nothing. Comparison
   surfaces escape C0 and DEL at render time; comparison and storage stay
   byte-raw. The three surfaces the escape has to reach are the short-value
   refinement path, the multi-line hunk path, and the containment excerpt;
   on all three the marks are computed against the raw value and drawn
   against the escaped one, so the columns are what these tests are really
   pinning. *)

(* The one failure a check verb raised: the end-to-end payload, not a
   hand-built one. *)
let caught name f =
  match f () with
  | () -> fail (name ^ ": expected Check_failure, got a return")
  | exception Failure.Check_failure fl -> fl
  | exception e ->
      fail (name ^ ": expected Check_failure, raised " ^ Printexc.to_string e)

let test_control_bytes_refined () =
  (* The refinement path. The mark moves with the text it marks: a span at
     raw byte 3 lands at display column 6, because the ESC before it now
     occupies four. *)
  let b =
    failure_block
      (Failure.equality ~expected:"\027[31mred\027[0m"
         ~actual:"\027[32mred\027[0m" ())
  in
  not_contains ~msg:"refined: no raw ESC reaches a plain block" ~sub:"\027" b;
  contains ~msg:"refined: both sides are escaped, a mark under each"
    ~sub:
      ("    expected  \\x1b[31mred\\x1b[0m\n" ^ String.make 20 ' '
     ^ "~\n    actual    \\x1b[32mred\\x1b[0m\n" ^ String.make 20 ' ' ^ "~\n")
    b;
  is_true ~msg:"refined: one mark line per changed side"
    (occurrences_of ~sub:"~" b = 2);
  (* A marked region that IS a control byte: the mark covers all four
     columns of the escape, not the one the raw span measured. *)
  let widened =
    failure_block
      (Failure.equality ~expected:"plain text here" ~actual:"plain\027text here"
         ())
  in
  contains ~msg:"refined: a marked control byte widens its mark"
    ~sub:("    actual    plain\\x1btext here\n" ^ String.make 19 ' ' ^ "~~~~\n")
    widened;
  contains
    ~msg:"refined: the expected side of a replacement carries its own mark"
    ~sub:
      ("    expected  plain text here\n" ^ String.make 19 ' '
     ^ "~\n    actual    plain")
    widened;
  (* A pure deletion marks [expected], and a deleted control byte is as
     wide there as anywhere. *)
  let deleted =
    failure_block
      (Failure.equality ~expected:"plain\027text here" ~actual:"plaintext here"
         ())
  in
  contains ~msg:"refined: a deletion is marked under expected, widened"
    ~sub:
      ("    expected  plain\\x1btext here\n" ^ String.make 19 ' '
     ^ "~~~~\n    actual    plaintext here\n")
    deleted;
  (* Under ansi the payload's sequence is still text; only the renderer's
     own styling is live. *)
  let colored =
    failure_block ~ansi:true
      (Failure.equality ~expected:"\027[31mred\027[0m"
         ~actual:"\027[32mred\027[0m" ())
  in
  not_contains ~msg:"refined: the payload's own sequence never runs"
    ~sub:"\027[31mred" colored;
  contains
    ~msg:
      "refined: the changed span is bold in its side's colour, inside the \
       escaped value"
    ~sub:
      "\027[2mexpected\027[0m  \\x1b[3\027[1;32m1\027[0mmred\\x1b[0m\n\
      \    \027[2mactual\027[0m    \\x1b[3\027[1;31m2\027[0mmred\\x1b[0m\n"
    colored;
  not_contains ~msg:"refined: no mark prints under colour" ~sub:"~" colored

(* The mark prints only where it lands under what it marks: one [~] per
   code point, and none at all when a tab or a code point of no fixed width
   sits on either side. *)
let test_mark_criterion () =
  let block expected actual =
    failure_block (Failure.equality ~expected ~actual ())
  in
  contains ~msg:"the mark counts code points, not bytes"
    ~sub:
      ("    actual    the caf\u{00E9} is brawn today\n" ^ String.make 28 ' '
     ^ "~\n")
    (block "the caf\u{00E9} is brown today" "the caf\u{00E9} is brawn today");
  let tabbed = block "the\tquick brown fox" "the\tquick brawn fox" in
  not_contains ~msg:"a tab on a side: no mark" ~sub:"~" tabbed;
  contains ~msg:"a tab on a side: both values still print"
    ~sub:
      "    expected  the\tquick brown fox\n    actual    the\tquick brawn fox\n"
    tabbed;
  not_contains ~msg:"a code point past U+024F on a side: no mark" ~sub:"~"
    (block "\u{65E5}\u{672C} quick brown fox" "\u{65E5}\u{672C} quick brawn fox");
  not_contains ~msg:"malformed UTF-8 on a side: no mark" ~sub:"~"
    (block "\xff quick brown fox" "\xff quick brawn fox");
  not_contains ~msg:"a combining accent on a side: no mark" ~sub:"~"
    (block "cafe\u{0301} quick brown fox" "cafe\u{0301} quick brawn fox");
  not_contains ~msg:"an emoji on a side: no mark" ~sub:"~"
    (block "\u{1F642} quick brown fox" "\u{1F642} quick brawn fox");
  not_contains ~msg:"refinement declines on short values: no mark" ~sub:"~"
    (block "ab" "cd");
  (* A span at either end of the value, and a change that only inserted. *)
  equal ~msg:"a span at the very start, marked under each side" string
    "    expected  Xbcdefghij\n\
    \              ~\n\
    \    actual    Ybcdefghij\n\
    \              ~\n"
    (block "Xbcdefghij" "Ybcdefghij");
  equal ~msg:"a span at the very end, marked under each side" string
    "    expected  abcdefghiX\n\
    \                       ~\n\
    \    actual    abcdefghiY\n\
    \                       ~\n"
    (block "abcdefghiX" "abcdefghiY");
  equal ~msg:"a pure insertion is marked under actual only" string
    "    expected  user:alice\n\
    \    actual    user:alice:admin\n\
    \                        ~~~~~~\n"
    (block "user:alice" "user:alice:admin");
  equal ~msg:"a pure deletion is marked under expected only" string
    "    expected  user:alice:admin\n\
    \                        ~~~~~~\n\
    \    actual    user:alice\n"
    (block "user:alice:admin" "user:alice");
  (* Colour replaces the [~] lines and nothing else: stripped of its
     styling, the coloured block is the plain one without them. *)
  List.iter
    (fun (expected, actual) ->
      let f = Failure.equality ~expected ~actual () in
      equal
        ~msg:
          "the coloured block, stripped, is the plain block without its marks"
        string
        (without_marks (failure_block f))
        (strip_ansi (failure_block ~ansi:true f)))
    [
      ("Xbcdefghij", "Ybcdefghij");
      ("user:alice", "user:alice:admin");
      ("user:alice:admin", "user:alice");
      ("the\tquick brown fox", "the\tquick brawn fox");
      ("ab", "cd");
      ("one\ntwo\nthree\n", "one\nTWO\nthree\n");
    ]

let test_control_bytes_hunks () =
  (* The multi-line path, through the witness that produces multi-line
     renderings: [text] prints verbatim, so a styled line arrives at the
     diff with its ESC intact. *)
  let expected = "header\n\027[31malert\027[0m\nfooter" in
  let f =
    caught "text equality" (fun () ->
        Check.equal Testable.text expected
          "header\n\027[32malert\027[0m\nfooter")
  in
  let b = failure_block f in
  not_contains ~msg:"hunks: no raw ESC reaches a plain block" ~sub:"\027" b;
  contains ~msg:"hunks: both changed lines are escaped"
    ~sub:"    - \\x1b[31malert\\x1b[0m\n    + \\x1b[32malert\\x1b[0m\n" b;
  contains ~msg:"hunks: context lines are escaped too" ~sub:"      header\n" b;
  (* A carriage return no longer eats the line it shares. *)
  let cr =
    failure_block
      (Failure.equality ~expected:"one\ntwo\r\nthree" ~actual:"one\ntwo\nthree"
         ())
  in
  contains ~msg:"hunks: CR renders as its hex escape" ~sub:"- two\\x0d\n" cr;
  not_contains ~msg:"hunks: no raw CR survives" ~sub:"two\r" cr;
  (* Baseline mismatches share [pp_hunks], the single producer. *)
  let snap =
    failure_block
      (Failure.baseline
         (Failure.Literal { exact = false })
         (Failure.Mismatch
            {
              expected = Failure.text "\027[1mbold\027[0m\n";
              actual = Failure.text "bold\n";
            }))
  in
  contains ~msg:"hunks: baselines escape as well"
    ~sub:"- \\x1b[1mbold\\x1b[0m\n" snap

let test_control_bytes_containment () =
  (* The excerpt path: the occurrence marker is computed from the payload's
     raw byte offset and drawn in display columns. *)
  let f =
    caught "containment" (fun () ->
        Check.contains ~sub:"NOPE" "\027[31mred\027[0m text")
  in
  let b = failure_block f in
  not_contains ~msg:"containment: no raw ESC reaches a plain block" ~sub:"\027"
    b;
  contains ~msg:"containment: the excerpt is escaped"
    ~sub:"    haystack  \\x1b[31mred\\x1b[0m text\n" b;
  (* not_contains: the needle occurs, and its mark must land under the
     escaped occurrence rather than at its raw offset. *)
  let found =
    caught "not_contains" (fun () ->
        Check.not_contains ~sub:"red" "\027[31mred\027[0m text")
  in
  let b = failure_block found in
  contains ~msg:"containment: the needle keeps its own %S escapes"
    ~sub:"    needle    \"red\": found at byte 5\n" b;
  contains ~msg:"containment: the mark sits under the escaped occurrence"
    ~sub:
      ("    haystack  \\x1b[31mred\\x1b[0m text\n" ^ String.make 22 ' '
     ^ "~~~\n")
    b;
  let colored = failure_block ~ansi:true found in
  not_contains ~msg:"containment: the excerpt's own sequence never runs"
    ~sub:"\027[31mred\027[0m text" colored;
  contains
    ~msg:
      "containment: under colour the occurrence is bold red in the plain \
       excerpt, and no mark prints"
    ~sub:
      "    \027[2mhaystack\027[0m  \\x1b[31m\027[1;31mred\027[0m\\x1b[0m text\n"
    colored;
  not_contains ~msg:"containment: no mark under colour" ~sub:"~" colored;
  is_true
    ~msg:
      "containment: the coloured bytes, stripped, are the plain ones without \
       the mark"
    (strip_ansi colored = without_marks b)

let test_control_bytes_alphabet () =
  (* One rule: every C0 byte and DEL as [\xNN], LF and TAB excepted because
     the block's layout is made of them. *)
  let b =
    failure_block
      (Failure.predicate ~claim:"a clean value" "a\x00b\x07c\rd\x7fe\x1ff")
  in
  contains ~msg:"alphabet: NUL, BEL, CR, DEL and US all escape"
    ~sub:"    actual    a\\x00b\\x07c\\x0dd\\x7fe\\x1ff\n" b;
  let kept =
    failure_block (Failure.predicate ~claim:"a clean value" "one\ttwo\nthree")
  in
  contains ~msg:"alphabet: TAB survives inside a line" ~sub:"      one\ttwo\n"
    kept;
  contains ~msg:"alphabet: LF still breaks the block into lines"
    ~sub:"      one\ttwo\n      three\n" kept

let test_control_bytes_are_render_only () =
  (* The guarantee's other half: nothing below the renderer sees the
     escape. Equality still compares bytes, and the payload still stores
     them. *)
  let f =
    caught "byte-exact comparison" (fun () ->
        Check.equal Testable.string "\027" "\\x1b")
  in
  (match f.Failure.kind with
  | Failure.Equality { expected; actual; _ } ->
      equal ~msg:"payloads store the raw bytes" string {|"\027"|} expected.kept;
      equal ~msg:"and the other side's own bytes" string {|"\\x1b"|} actual.kept
  | _ -> fail "expected an Equality payload");
  (* Two values a lossy printer merges are reported as such; two the
     ESCAPING would merge are not, because the test is made on the raw
     renderings. [Testable.text] renders both verbatim. *)
  let merged =
    caught "escaping is not a printer" (fun () ->
        Check.equal Testable.text "\027" "\\x1b")
  in
  not_contains ~msg:"an escape collision is never called an identical rendering"
    ~sub:"both sides render as" (failure_block merged)

let test_diff_truncation () =
  let text prefix =
    String.concat "\n" (List.init 300 (fun i -> Printf.sprintf "%s%d" prefix i))
  in
  let b =
    failure_block (Failure.equality ~expected:(text "e") ~actual:(text "a") ())
  in
  contains ~msg:"huge diffs end in a truncation mark" ~sub:"more diff lines)" b;
  not_contains ~msg:"huge diffs are display-bounded" ~sub:"+ a299" b;
  let snap =
    failure_block
      (Failure.baseline (Failure.File "p.expected")
         (Failure.Mismatch
            {
              expected = Failure.text (text "e" ^ "\n");
              actual = Failure.text (text "a" ^ "\n");
            }))
  in
  contains ~msg:"baseline diff truncation mark" ~sub:"more diff lines)" snap;
  contains ~msg:"acceptance survives a truncated diff"
    ~sub:"accept: dune promote" snap

let test_proposed_truncation () =
  let proposed =
    String.concat "" (List.init 25 (fun i -> Printf.sprintf "line %d\n" i))
  in
  let b =
    failure_block
      (Failure.baseline (Failure.File "p.expected")
         (Failure.Missing { proposed = Failure.text proposed }))
  in
  (* A missing baseline has no file to diff against: its proposed text
     prints under a heading that counts it, as [+] lines, 20 at most. *)
  contains ~msg:"a missing file: the fact line, the heading, then + lines"
    ~sub:
      "    expect_file \"p.expected\": no baseline\n\
      \    proposed (25 lines):\n\
      \      + line 0\n\
      \      + line 1\n"
    b;
  contains ~msg:"proposed content bounded with a mark" ~sub:"(+5 more lines)" b;
  contains
    ~msg:
      "a missing file: 20 lines indented two, the cap line with them, then the \
       command at the block's column"
    ~sub:"      + line 19\n      \u{2026} (+5 more lines)\n    accept: " b;
  not_contains ~msg:"a missing file: no line over the bound" ~sub:"line 20" b;
  not_contains ~msg:"proposed lines over the bound absent" ~sub:"line 24" b;
  not_contains
    ~msg:"a missing file: no hunk header for a file that does not exist"
    ~sub:"@@" b;
  not_contains ~msg:"a missing file: no second gutter" ~sub:"\u{2506}" b;
  let short =
    failure_block
      (Failure.baseline (Failure.File "p.expected")
         (Failure.Missing { proposed = Failure.text "only\n" }))
  in
  contains ~msg:"a missing file under the cap prints whole, uncapped"
    ~sub:"    proposed (1 line):\n      + only\n    accept: " short;
  contains ~msg:"the + lines are red under colour, their indentation plain"
    ~sub:"    proposed (1 line):\n      \027[31m+ only\027[0m\n    accept: "
    (failure_block ~ansi:true
       (Failure.baseline (Failure.File "p.expected")
          (Failure.Missing { proposed = Failure.text "only\n" })));
  contains ~msg:"acceptance survives a bounded proposal"
    ~sub:
      "    accept: touch 'p.expected' && dune runtest; dune promote p.expected\n"
    b

let test_excerpt () =
  (* The excerpt source is generated in the test's scratch directory: the
     renderer reads it back through the failure's location. *)
  let file = Filename.concat (temp_dir ()) "excerpt_src.ml" in
  Out_channel.with_open_bin file (fun oc ->
      output_string oc "let one = 1\n    \tlet two = 2\nlet three = 3\n");
  let f =
    Failure.equality
      ~loc:{ Loc.file; line = 2; column = 0 }
      ~expected:"1" ~actual:"2" ()
  in
  equal
    ~msg:
      "excerpt: the located source line sits under the bare location, its \
       leading whitespace removed, one blank line after it"
    string
    (Printf.sprintf
       "    %s:2\n      2 │ let two = 2\n\n    expected  1\n    actual    2\n"
       file)
    (failure_block ~excerpt:true f);
  contains ~msg:"excerpt: the gutter is faint, the source plain"
    ~sub:":2\027[0m\n      \027[2m2 │\027[0m let two = 2\n\n"
    (failure_block ~ansi:true ~excerpt:true f);
  not_contains ~msg:"excerpt: off by default" ~sub:"let two" (failure_block f);
  (* Under every location: a property's own, whose inner assertion keeps
     its line too. *)
  let law =
    failure_block ~excerpt:true
      (Failure.property
         ~loc:{ Loc.file; line = 1; column = 0 }
         ~inner:f ~rendered:"0" ~case_index:0 ~shrink_steps:0
         ~root:Fixtures.root ~examples:false ())
  in
  contains
    ~msg:"excerpt: under a property's own location, one blank line after it"
    ~sub:
      (Printf.sprintf "    %s:1\n      1 │ let one = 1\n\n    counterexample "
         file)
    law;
  contains
    ~msg:
      "excerpt: the inner assertion keeps its source line, indented with its \
       entry, and no blank line follows an inner one"
    ~sub:
      (Printf.sprintf
         "    which failed at:\n\
         \      %s:2\n\
         \        2 │ let two = 2\n\
         \      expected  1\n"
         file)
    law;
  (* A source line is a file's bytes: escaped as a value's are, under both
     colour settings, so that none drives the terminal. *)
  let wild = Filename.concat (temp_dir ()) "excerpt_wild.ml" in
  Out_channel.with_open_bin wild (fun oc ->
      output_string oc
        ("  let red = \"\027[31mred\027[0m\" (* \r \007 *)\r\n"
       ^ String.make 900 'x' ^ "\n\n"));
  let at line =
    Failure.equality
      ~loc:{ Loc.file = wild; line; column = 0 }
      ~expected:"1" ~actual:"2" ()
  in
  List.iter
    (fun ansi ->
      contains
        ~msg:
          (Printf.sprintf
             "excerpt: a source line's control bytes are escaped (ansi:%b)" ansi)
        ~sub:{| let red = "\x1b[31mred\x1b[0m" (* \x0d \x07 *)|}
        (failure_block ~ansi ~excerpt:true (at 1)))
    [ false; true ];
  contains ~msg:"excerpt: a CRLF file's line ends where its text does"
    ~sub:"\\x07 *)\n\n"
    (failure_block ~excerpt:true (at 1));
  contains ~msg:"excerpt: a source line over 800 bytes is elided as a value is"
    ~sub:"xxxx\u{2026} (100 bytes elided)xxxx"
    (failure_block ~excerpt:true (at 2));
  contains ~msg:"excerpt: an empty source line is its gutter alone"
    ~sub:(Printf.sprintf "    %s:3\n      3 \u{2502}\n\n    expected" wild)
    (failure_block ~excerpt:true (at 3));
  let gone =
    Failure.equality
      ~loc:{ Loc.file = "does_not_exist.ml"; line = 2; column = 0 }
      ~expected:"1" ~actual:"2" ()
  in
  equal
    ~msg:
      "excerpt: an unreadable file prints neither the line nor the blank line"
    string "    does_not_exist.ml:2\n    expected  1\n    actual    2\n"
    (failure_block ~excerpt:true gone)

(* Captured tail *)

let tail_block tail =
  let result =
    Fixtures.result [ "t" ]
      (Failure.Fail [ Failure.with_output_tail tail (Failure.message "boom") ])
  in
  with_renderer (fun r ->
      Report.finish r ~release_failures:[] ~results:[ result ] ~duration:0.01 ())

(* The tail is a fixed ten lines over the bytes the capture kept: not a
   knob, so a twelve-line tail shows its last ten. *)
let test_tail () =
  let twelve =
    Failure.tail
      (String.concat ""
         (List.init 12 (fun i -> Printf.sprintf "l%d\n" (i + 1))))
  in
  let b = tail_block twelve in
  contains
    ~msg:"tail: line-bounded heading, undecorated, its lines indented two more"
    ~sub:"    captured output (last 10 of 12 lines):\n      l3\n" b;
  not_contains ~msg:"tail: no rule glyph before the heading"
    ~sub:"\u{2500} captured" b;
  not_contains ~msg:"tail: no rule glyph after the heading" ~sub:": \u{2500}" b;
  contains ~msg:"tail: last lines shown" ~sub:"      l3\n      l4\n" b;
  contains ~msg:"tail: through the last line, then the closing rule"
    ~sub:("      l12\n" ^ closing_rule ^ "\n")
    b;
  not_contains ~msg:"tail: earlier lines dropped" ~sub:"l2\n" b;
  let full = Failure.tail ~log_path:"log.output" "only\n" in
  let b = tail_block full in
  contains
    ~msg:"tail: complete output heading, then the log at the heading's column"
    ~sub:"    captured output (1 line):\n      only\n    full log: log.output\n"
    b;
  contains ~msg:"tail: the heading and the log are faint, the lines plain"
    ~sub:
      "    \027[2mcaptured output (1 line):\027[0m\n\
      \      only\n\
      \    \027[2mfull log: log.output\027[0m\n"
    (with_renderer ~ansi:true (fun r ->
         Report.finish r ~release_failures:[]
           ~results:
             [
               Fixtures.result [ "t" ]
                 (Failure.Fail
                    [ Failure.with_output_tail full (Failure.message "boom") ]);
             ]
           ~duration:0.01 ()));
  contains ~msg:"tail: a whole tail of several lines counts them"
    ~sub:"    captured output (3 lines):\n      a\n"
    (tail_block (Failure.tail "a\nb\nc\n"));
  let dropped = Failure.tail ~omitted_bytes:512 "kept\n" in
  contains ~msg:"tail: drop count reported"
    ~sub:"    captured output (last 1 line, 512 earlier bytes omitted):\n"
    (tail_block dropped);
  let many =
    Failure.tail ~omitted_bytes:9000
      (String.concat ""
         (List.init 12 (fun i -> Printf.sprintf "l%d\n" (i + 1))))
  in
  (* The count is of every byte before the first line shown: the 9000 the
     capture cut and the two kept lines the cap drops, [l1\n] and [l2\n]. *)
  contains ~msg:"tail: the byte-cut head, at the cap, counts the dropped lines"
    ~sub:
      "    captured output (last 10 lines, 9006 earlier bytes omitted):\n\
      \      l3\n"
    (tail_block many)

(* Bounds: a backtrace shares the captured tail's cap, and a long
   single-line value keeps its two ends *)

let test_backtrace_cap () =
  let frames n =
    String.concat ""
      (List.init n (fun i -> Printf.sprintf "frame %d\n" (i + 1)))
  in
  let block n =
    failure_block (Failure.raised ~actual:"Not_found" ~backtrace:(frames n) ())
  in
  is_true
    ~msg:"ten frames, then the count of the rest, and the block ends there"
    (String.ends_with
       ~suffix:"    frame 9\n    frame 10\n    \u{2026} (+3 more frames)\n"
       (block 13));
  not_contains ~msg:"the eleventh frame never prints" ~sub:"frame 11" (block 13);
  is_true ~msg:"ten frames print whole, and the block ends on the tenth"
    (String.ends_with ~suffix:"    frame 9\n    frame 10\n" (block 10));
  not_contains ~msg:"and draw no cap line" ~sub:"more frames" (block 10);
  contains ~msg:"the cap line is dim, as the frames are"
    ~sub:"    \027[2m\u{2026} (+3 more frames)\027[0m\n"
    (failure_block ~ansi:true
       (Failure.raised ~actual:"Not_found" ~backtrace:(frames 13) ()))

let test_value_elision () =
  let e n = String.concat "" (List.init n (fun _ -> "\u{00e9}")) in
  (* 401 two-byte characters and a letter, 803 bytes: the first 400 bytes
     are 200 characters, and the last 400 would open inside one, so the
     tail starts a byte later and four bytes are left out. *)
  let b =
    failure_block
      (Failure.equality ~expected:(e 401 ^ "x") ~actual:(e 401 ^ "y") ())
  in
  equal ~msg:"over 800 bytes: both ends around the count, cut on code points"
    string
    ("    expected  " ^ e 200 ^ "\u{2026} (4 bytes elided)" ^ e 199 ^ "x\n"
   ^ "    actual    " ^ e 200 ^ "\u{2026} (4 bytes elided)" ^ e 199 ^ "y\n")
    b;
  not_contains ~msg:"an elided side draws no mark" ~sub:"~" b;
  (* One side is enough to lose the mark: its columns are gone. *)
  not_contains ~msg:"one elided side draws no mark either" ~sub:"~"
    (failure_block
       (Failure.equality ~expected:(String.make 900 'a' ^ "x") ~actual:"ax" ()));
  let whole =
    failure_block
      (Failure.equality ~expected:(e 399 ^ "ax") ~actual:(e 399 ^ "ay") ())
  in
  contains ~msg:"800 bytes print whole"
    ~sub:("    expected  " ^ e 399 ^ "ax\n")
    whole;
  contains ~msg:"and keep their mark"
    ~sub:("\n" ^ String.make (14 + 400) ' ' ^ "~\n")
    whole;
  is_true ~msg:"one under each side"
    (occurrences_of ~sub:("\n" ^ String.make (14 + 400) ' ' ^ "~\n") whole = 2);
  (* The count is the carried value's: an escape is four columns of one
     byte. *)
  contains ~msg:"the count is of the carried bytes, not the escaped ones"
    ~sub:
      ("    both sides equal: \\x01" ^ String.make 399 'a'
     ^ "\u{2026} (101 bytes elided)" ^ String.make 400 'a' ^ "\n")
    (failure_block
       (Failure.equality ~not_:true
          ~expected:("\001" ^ String.make 900 'a')
          ~actual:("\001" ^ String.make 900 'a')
          ()));
  contains ~msg:"a counterexample is a value too"
    ~sub:
      ("    counterexample (case 0): " ^ String.make 400 'a'
     ^ "\u{2026} (1 bytes elided)" ^ String.make 400 'a' ^ "\n")
    (failure_block
       (Failure.property ~rendered:(String.make 801 'a') ~case_index:0
          ~shrink_steps:0 ~root:Fixtures.root ~examples:false ()));
  (* A needle is cut in its carried bytes and each end quoted: 300
     three-byte characters keep 133 at each end (399 bytes), no escape is
     cut in two, and the count is of bytes, not of escape digits. *)
  let nichi n = String.concat "" (List.init n (fun _ -> "\\230\\151\\165")) in
  contains ~msg:"a needle is elided in its carried bytes, inside its quotes"
    ~sub:
      ("    needle    \"" ^ nichi 133 ^ "\u{2026} (102 bytes elided)"
     ^ nichi 133 ^ "\": not found\n")
    (failure_block
       (Failure.containment ~demand:Failure.Anywhere
          ~needle:(String.concat "" (List.init 300 (fun _ -> "\u{65e5}")))
          ~haystack:"hay" ()));
  (* A multi-line value is lines, bounded as lines are. *)
  let lines = String.make 500 'a' ^ "\n" ^ String.make 500 'b' in
  not_contains ~msg:"a multi-line value is not elided" ~sub:"bytes elided"
    (failure_block
       (Failure.equality ~not_:true ~expected:lines ~actual:lines ()))

(* Exception message diffs *)

let test_raise_message_diff () =
  let b = failure_block Fixtures.raise_message_failure in
  contains ~msg:"raise: constructor named once"
    ~sub:"    raised Invalid_argument with the wrong message:\n" b;
  contains
    ~msg:"raise: messages compared as strings, a mark under each changed one"
    ~sub:
      ("    raised Invalid_argument with the wrong message:\n\
       \    expected  \"index 3 out of bounds\"\n" ^ String.make 21 ' '
     ^ "~\n    actual    \"index 4 out of bounds\"\n" ^ String.make 21 ' '
     ^ "~\n")
    b;
  is_true ~msg:"raise: one mark line per changed side"
    (occurrences_of ~sub:"~" b = 2);
  not_contains ~msg:"raise: constructor not repeated on both sides"
    ~sub:"expected exception" b;
  let colored = failure_block ~ansi:true Fixtures.raise_message_failure in
  contains
    ~msg:"raise: under colour the changed span is bold in its side's colour"
    ~sub:
      "    \027[2mexpected\027[0m  \"index \027[1;32m3\027[0m out of bounds\"\n\
      \    \027[2mactual\027[0m    \"index \027[1;31m4\027[0m out of bounds\"\n"
    colored;
  not_contains ~msg:"raise: and no mark prints" ~sub:"~" colored

let test_raise_message_diff_guards () =
  (* No recorded diff means both exceptions print, whatever their
     constructors: the option is the whole of the renderer's decision. *)
  let b = failure_block Fixtures.raise_failure in
  contains ~msg:"raise: different constructors unchanged"
    ~sub:"expected exception" b;
  let no_diff =
    Failure.raised ~expected:{|Failure("boom")|} ~actual:{|Failure("boom!")|} ()
  in
  contains ~msg:"raise: same constructor without a diff keeps the plain form"
    ~sub:
      "    expected exception  Failure(\"boom\")\n\
      \    raised              Failure(\"boom!\")\n"
    (failure_block no_diff);
  not_contains ~msg:"raise: no mark without a recorded message diff" ~sub:"~"
    (failure_block no_diff);
  not_contains ~msg:"raise: no message diff without the payload"
    ~sub:"with the wrong message" (failure_block no_diff);
  (* raises_match's payload still prints the raised exception, under the
     sentence that says what was wrong with it. *)
  let predicate_miss =
    Failure.raised ~actual:{|Invalid_argument("nope")|} ~predicate:true ()
  in
  contains ~msg:"raises_match: actually-raised exception printed"
    ~sub:
      "    raised exception does not satisfy the predicate:\n\
      \      Invalid_argument(\"nope\")\n"
    (failure_block predicate_miss);
  not_contains ~msg:"raises_match: the predicate is no fake exception"
    ~sub:"one the predicate accepts"
    (failure_block predicate_miss);
  contains ~msg:"raises: nothing raised"
    ~sub:
      "    expected exception  Failure(\"boom\")\n\
      \    but no exception was raised\n"
    (failure_block (Failure.raised ~expected:{|Failure("boom")|} ()));
  not_contains ~msg:"raises: nothing is no exception's name" ~sub:"nothing"
    (failure_block (Failure.raised ~expected:{|Failure("boom")|} ()));
  contains ~msg:"raises_match: nothing raised"
    ~sub:"    expected an exception, but none was raised\n"
    (failure_block (Failure.raised ~predicate:true ()));
  contains ~msg:"raises: a multi-line expectation that nothing met"
    ~sub:
      "    expected exception:\n\
      \      Parse_error(\n\
      \        line 3)\n\
      \    but no exception was raised\n"
    (failure_block (Failure.raised ~expected:"Parse_error(\n  line 3)" ()));
  (* A rendering that spans lines is a block under its anchor. *)
  contains ~msg:"raise: a multi-line exception is a block under its anchor"
    ~sub:
      "    expected exception  Failure(\"boom\")\n\
      \    raised:\n\
      \      Parse_error(\n\
      \        line 3)\n"
    (failure_block
       (Failure.raised ~expected:{|Failure("boom")|}
          ~actual:"Parse_error(\n  line 3)" ()))

(* Expected failures *)

let test_xfail_line () =
  let line =
    with_renderer ~mode:`Verbose (fun r ->
        Report.result r Fixtures.excused_result)
  in
  contains
    ~msg:"xfail line: XFAIL tag, the duration in its column, then the reason"
    ~sub:
      ("  XFAIL  known › broken carry" ^ String.make 22 ' '
     ^ "0.2ms (expected failure: issue #42)")
    line;
  not_contains ~msg:"xfail line: not a FAIL" ~sub:"  FAIL  " line;
  let no_reason =
    with_renderer ~mode:`Verbose (fun r ->
        Report.result r
          {
            Fixtures.excused_result with
            Run.xfail = Some { Test_tree.reason = None };
          })
  in
  contains ~msg:"xfail line: reasonless form" ~sub:"(expected failure)"
    no_reason;
  let pass_ignores =
    with_renderer ~mode:`Verbose (fun r ->
        Report.result r
          (Fixtures.result [ "t" ] Failure.Pass ~xfail:Fixtures.xfail_reason))
  in
  contains ~msg:"an xfail annotation on a pass changes nothing" ~sub:"PASS"
    pass_ignores

(* Under -v an expected failure shows its failure: the block of a counted
   one without its hints, dim, under the row. Compact prints nothing of it,
   and neither counts it. *)
let test_xfail_block () =
  let property =
    Failure.property
      ~loc:(Fixtures.loc "test/test_carry.ml" 5)
      ~inner:(Failure.message "carry lost")
      ~rendered:"(1, 2)" ~case_index:3 ~shrink_steps:0 ~root:Fixtures.root
      ~examples:false ()
  in
  let excused =
    { Fixtures.excused_result with Run.outcome = Failure.Fail [ property ] }
  in
  equal ~msg:"the block under the row, closed by a blank line, no replay:"
    string
    ("  XFAIL  known \u{203a} broken carry" ^ String.make 22 ' '
   ^ "0.2ms (expected failure: issue #42)\n\
     \    test/test_carry.ml:5\n\
     \    counterexample (case 3): (1, 2)\n\
     \    which failed with:\n\
     \      carry lost\n\n")
    (with_renderer ~mode:`Verbose ~invocation:(`Exe "./t.exe") (fun r ->
         Report.result r excused));
  equal ~msg:"styled, each line is dim past its indent" string
    ("  \027[2mXFAIL\027[0m  known \u{203a} broken carry" ^ String.make 22 ' '
   ^ "\027[2m0.2ms\027[0m \027[2m(expected failure: issue #42)\027[0m\n\
     \    \027[2mtest/test_carry.ml:5\027[0m\n\
     \    \027[2mcounterexample (case 3): (1, 2)\027[0m\n\
     \    \027[2mwhich failed with:\027[0m\n\
     \      \027[2mcarry lost\027[0m\n\n")
    (with_renderer ~ansi:true ~mode:`Verbose (fun r -> Report.result r excused));
  equal ~msg:"compact prints nothing of it" string ""
    (with_renderer (fun r -> Report.result r excused))

let test_excused_collision () =
  (* The F4 regression, renderer level: an xfail test whose REAL failure
     message equals the runner's unexpected-pass string. The record says
     excused ([counted = false]); classification is record-driven, so the
     stream agrees with the exit code and the summary. No failure message
     is ever inspected. *)
  let collide =
    Fixtures.result [ "collide" ]
      (Failure.Fail [ Failure.message "expected to fail, but the test passed" ])
      ~xfail:{ Test_tree.reason = None }
  in
  let verbose =
    with_renderer ~mode:`Verbose (fun r -> Report.result r collide)
  in
  contains ~msg:"collision record renders XFAIL, not FAIL" ~sub:"  XFAIL  "
    verbose;
  not_contains ~msg:"collision record: no loud FAIL line" ~sub:"  FAIL  "
    verbose;
  let summary =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r collide;
        Report.finish r ~release_failures:[] ~results:[ collide ] ~duration:0.1
          ())
  in
  equal ~msg:"collision record: stream, summary, and count agree" string
    "s: 1 expected failure in 100ms.\n" summary

let test_finish_excused () =
  let results =
    [
      Fixtures.result [ "ok" ] Failure.Pass;
      Fixtures.excused_result;
      Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "boom" ]);
    ]
  in
  let t =
    with_renderer ~invocation:(`Exe "exe") (fun r ->
        Report.finish r ~release_failures:[] ~results ~duration:0.2 ())
  in
  is_true ~msg:"finish: excused leaves the failure section its one block"
    (occurrences_of ~sub:(failures_rule ^ "\n") t = 1
    && occurrences_of ~sub:"\n  FAIL  " t = 1);
  not_contains ~msg:"finish: excused block absent" ~sub:"broken carry" t;
  contains ~msg:"finish: summary counts the expected failure"
    ~sub:"1 passed, 1 expected failure, 1 failed in 200ms." t;
  let only_excused =
    with_renderer ~invocation:(`Exe "exe") (fun r ->
        Report.finish r ~release_failures:[]
          ~results:
            [ Fixtures.result [ "ok" ] Failure.Pass; Fixtures.excused_result ]
          ~duration:0.2 ())
  in
  not_contains ~msg:"finish: no failure section when all failures excused"
    ~sub:"failures \u{2500}" only_excused;
  not_contains ~msg:"finish: and no closing rule" ~sub:closing_rule only_excused;
  not_contains ~msg:"finish: no rerun hint when all failures excused"
    ~sub:"--failed" only_excused;
  contains ~msg:"finish: green summary with excused failures"
    ~sub:"1 passed, 1 expected failure in 200ms." only_excused

let test_xpass_is_loud () =
  (* The runner records an xfail test that passed as an ordinary counted
     failure whose message names the reason: no excused marking, loud FAIL. *)
  let line =
    with_renderer ~mode:`Verbose (fun r ->
        Report.result r Fixtures.xpass_result)
  in
  contains ~msg:"unexpected pass: loud FAIL line" ~sub:"  FAIL  known › fixed"
    line;
  let t =
    with_renderer (fun r ->
        Report.finish r ~release_failures:[] ~results:[ Fixtures.xpass_result ]
          ~duration:0.1 ())
  in
  contains ~msg:"unexpected pass: reason in the failure block"
    ~sub:"expected to fail (issue #42), but the test passed" t

(* Subtest failures *)

let test_subtest_projection () =
  is_true ~msg:"subtest entries recognized by their components"
    (Report.is_subtest_failure (Fixtures.subtest_failure "shape [0]"));
  is_true ~msg:"plain failures are not subtest entries"
    (not (Report.is_subtest_failure (Failure.message "boom")));
  (* The collision regression: classification is record-driven, so a user
     [?msg] spelling out the [leaf › name] prefix stays an ordinary
     annotation instead of being dressed as a sub-case. *)
  let collision =
    {
      (Failure.message "boom") with
      Failure.msg = Some (Failure.text "contract \u{203a} shape [0]");
    }
  in
  is_true ~msg:"a user msg spelling the label prefix is not a subtest entry"
    (not (Report.is_subtest_failure collision));
  let t =
    with_renderer (fun r ->
        Report.finish r ~release_failures:[]
          ~results:
            [
              Fixtures.result [ "backend"; "contract" ]
                (Failure.Fail [ collision ]);
            ]
          ~duration:0.1 ())
  in
  contains ~msg:"the colliding msg renders as an ordinary annotation"
    ~sub:"contract \u{203a} shape [0]" t;
  is_true ~msg:"the colliding msg adds no subtest count to the summary"
    (not (has ~sub:"subtest failure" t))

let test_subtest_rendering () =
  let t =
    with_renderer (fun r ->
        Report.finish r ~release_failures:[]
          ~results:[ Fixtures.subtest_result ]
          ~duration:0.1 ())
  in
  contains ~msg:"a subtest entry names its subtest under its location"
    ~sub:"    test/test_backend.ml:40\n    subtest   shape [0]\n    expected" t;
  contains ~msg:"the subtest's name sits in the column of the values under it"
    ~sub:"    subtest   shape [0]\n    expected  [1; 2]\n" t;
  contains ~msg:"a blank line separates two entries of one test"
    ~sub:
      ("    actual    [1; 3]\n" ^ String.make 18 ' '
     ^ "~\n\n    test/test_backend.ml:40\n    subtest   shape [2]\n")
    t;
  contains ~msg:"the last entry ends the block on its facts"
    ~sub:("\n\n    test/test_backend.ml:61\n    final check\n" ^ closing_rule)
    t;
  not_contains ~msg:"the parent's name is the title's, not repeated per entry"
    ~sub:"contract › shape" t;
  contains ~msg:"summary states the subtest count"
    ~sub:"1 failed (2 subtest failures) in 100ms." t;
  let one =
    with_renderer (fun r ->
        Report.finish r ~release_failures:[]
          ~results:
            [
              Fixtures.result [ "backend"; "contract" ]
                (Failure.Fail [ Fixtures.subtest_failure "shape [0]" ]);
            ]
          ~duration:0.1 ())
  in
  contains ~msg:"summary subtest count is singular"
    ~sub:"1 failed (1 subtest failure) in 100ms." one

(* Property stats *)

let test_prop_stats () =
  let stats =
    {
      Property.cases = 100;
      discards = 3;
      collected = [ ("empty", 36); ("nonempty", 64) ];
      coverage =
        [
          { Property.label = "collision"; hits = 0; satisfied = false };
          { Property.label = "singleton"; hits = 9; satisfied = true };
        ];
    }
  in
  let result =
    Fixtures.result [ "p" ]
      (Failure.Fail [ Failure.message "coverage unsatisfied" ])
      ~prop_stats:stats
  in
  let b =
    with_renderer (fun r ->
        Report.finish r ~release_failures:[] ~results:[ result ] ~duration:0.01
          ())
  in
  contains ~msg:"prop stats: label distribution"
    ~sub:"labels (100 passing cases):" b;
  contains ~msg:"prop stats: percentages" ~sub:"36.0%  empty" b;
  contains ~msg:"prop stats: uncovered label"
    ~sub:"      collision  0  never covered\n" b;
  (* The list carries the covered label too. That is what it adds over the
     failure headline, which names only the ones that were not. *)
  contains ~msg:"prop stats: covered label listed alongside" ~sub:"singleton  9"
    b;
  (* With a single label the list would only restate the headline, so it
     does not print at all. *)
  let single =
    { stats with Property.coverage = [ List.hd stats.Property.coverage ] }
  in
  let b1 =
    with_renderer (fun r ->
        Report.finish r ~release_failures:[]
          ~results:
            [
              Fixtures.result [ "p" ]
                (Failure.Fail [ Failure.message "coverage unsatisfied" ])
                ~prop_stats:single;
            ]
          ~duration:0.01 ())
  in
  not_contains ~msg:"prop stats: a lone label is not restated"
    ~sub:"covered labels:" b1;
  contains ~msg:"prop stats: its labels still print"
    ~sub:"labels (100 passing cases):" b1

(* A cut text prints what the failure kept and then the marker, which the
   payload holds as a length, never as bytes of the value. *)
let test_cut_text_marker () =
  let marker = "... (truncated; 70000 bytes total)" in
  let long = String.make 70_000 'm' in
  contains ~msg:"a cut message ends in the marker"
    ~sub:(String.make 20 'm' ^ marker ^ "\n")
    (failure_block (Failure.message long));
  contains ~msg:"a cut value ends in the marker, after the report's elision"
    ~sub:(marker ^ "\n")
    (failure_block (Failure.equality ~expected:long ~actual:"m" ()));
  contains ~msg:"the headline prints it too" ~sub:"mmm"
    (Report.headline (Failure.message long))

(* Two sides cut to the same 64 KiB are not known equal: the report says
   what it knows, never that the printer merged them. A diff with a cut
   side names the cut after its hunks, and the marker is never a line of
   the diff. *)
let test_cut_comparisons () =
  let head = String.make 65_536 'a' in
  let agree =
    "the sides agree on the 65536 bytes a failure keeps of each (expected \
     65537 bytes, actual 65537 bytes)"
  in
  let merged b =
    not_contains ~msg:"never the printer's fault" ~sub:"printer shows less" b;
    not_contains ~msg:"never one rendering" ~sub:"both sides render as" b;
    not_contains ~msg:"never a newline" ~sub:"trailing newline" b
  in
  let single =
    Failure.equality ~expected:(head ^ "x") ~actual:(head ^ "y") ()
  in
  let b = failure_block single in
  contains ~msg:"single-line: the sides agree on what was kept"
    ~sub:("    " ^ agree ^ "\n")
    b;
  merged b;
  is_true ~msg:"the headline says it too"
    (String.starts_with ~prefix:"the sides agree on the 65536 bytes"
       (Report.headline single));
  let lines =
    String.concat "\n" (List.init 8_000 (fun i -> Printf.sprintf "line %05d" i))
  in
  let b =
    failure_block
      (Failure.equality ~expected:(lines ^ "\nA") ~actual:(lines ^ "\nB") ())
  in
  contains ~msg:"multi-line: the same sentence"
    ~sub:
      "the sides agree on the 65536 bytes a failure keeps of each (expected \
       88001 bytes, actual 88001 bytes)"
    b;
  merged b;
  let b =
    failure_block
      (Failure.baseline (Failure.File "p.expected")
         (Failure.Mismatch
            {
              expected = Failure.text (head ^ "x");
              actual = Failure.text (head ^ "y");
            }))
  in
  contains ~msg:"a baseline: the same sentence after the mismatch"
    ~sub:("    expect_file \"p.expected\": mismatch\n    " ^ agree ^ "\n")
    b;
  merged b;
  let b =
    failure_block
      (Failure.raised ~expected:"Failure(_)" ~actual:"Failure(_)"
         ~message_diff:
           {
             Failure.constructor = "Failure";
             expected_message = Failure.text (head ^ "x");
             actual_message = Failure.text (head ^ "y");
           }
         ())
  in
  contains ~msg:"a message diff: the same sentence"
    ~sub:("    " ^ agree ^ "\n")
    b;
  merged b;
  (* One side cut, one whole: the hunks are over the kept bytes, and a line
     after them names the cut. *)
  let long = "a\n" ^ String.make 70_000 'x' in
  let short = "a\n" ^ String.make 100 'x' in
  let b = failure_block (Failure.equality ~expected:long ~actual:short ()) in
  contains ~msg:"the cut is named after the hunks"
    ~sub:
      "    (the diff covers the first 65536 of the 70002 bytes of expected)\n"
    b;
  not_contains ~msg:"the marker is no line of the diff" ~sub:"truncated" b;
  let b =
    failure_block
      (Failure.baseline (Failure.File "p.expected")
         (Failure.Mismatch
            {
              expected = Failure.text long;
              actual = Failure.text ("b\n" ^ String.make 70_001 'x');
            }))
  in
  contains ~msg:"both sides cut: both are named"
    ~sub:
      "(the diff covers the first 65536 of the 70002 bytes of expected and the \
       first 65536 of the 70003 bytes of actual)"
    b;
  not_contains ~msg:"and nothing is claimed of their equal kept bytes"
    ~sub:"trailing newline" b

(* Containment blocks *)

let not_contains_failure =
  Failure.containment ~found_at:10 ~demand:Failure.Anywhere ~needle:"secret"
    ~haystack:"0123456789secret-end" ()

let test_containment_block () =
  let b = failure_block not_contains_failure in
  contains ~msg:"not_contains: needle line carries the byte offset"
    ~sub:"    needle    \"secret\": found at byte 10\n" b;
  not_contains ~msg:"not_contains: no em dash" ~sub:"\u{2014}" b;
  contains ~msg:"not_contains: marker line sits under the occurrence"
    ~sub:
      ("    haystack  0123456789secret-end\n" ^ String.make 24 ' ' ^ "~~~~~~\n")
    b;
  not_contains ~msg:"not_contains: no fake equality labels" ~sub:"expected" b;
  not_contains ~msg:"not_contains: no excerpt line for a complete excerpt"
    ~sub:"(excerpt:" b;
  (* Under colour the occurrence is coloured in place of the mark. *)
  let colored = failure_block ~ansi:true not_contains_failure in
  contains
    ~msg:
      "not_contains: under colour the haystack prints whole and plain, the \
       occurrence bold red in it"
    ~sub:"    \027[2mhaystack\027[0m  0123456789\027[1;31msecret\027[0m-end\n"
    colored;
  not_contains ~msg:"not_contains: no mark under colour" ~sub:"~" colored;
  is_true ~msg:"not_contains: colour replaces the mark and nothing else"
    (strip_ansi colored = without_marks b);
  (* Off a terminal the escapes may be stripped, by dune for one, or read
     raw: the mark prints under colour too. *)
  is_true
    ~msg:
      "not_contains: off a terminal the coloured block, stripped, is the plain \
       one"
    (strip_ansi (failure_block ~ansi:true ~terminal:false not_contains_failure)
    = b);
  (* contains: needle absent, display-capped head excerpt of a huge
     haystack. *)
  let haystack = String.make 20_006 'a' in
  let contains_failure =
    Failure.containment ~demand:Failure.Anywhere ~needle:"NOPE" ~haystack ()
  in
  let b = failure_block contains_failure in
  contains ~msg:"contains: needle line with the not-found verdict"
    ~sub:"    needle    \"NOPE\": not found\n" b;
  contains ~msg:"contains: elision line states the capped display window"
    ~sub:"    (excerpt: bytes 0-1023 of a 20006-byte haystack)\n" b;
  contains ~msg:"contains: the capped excerpt prints verbatim"
    ~sub:("haystack  " ^ String.make 100 'a')
    b;
  not_contains ~msg:"contains: nothing is marked when nothing occurs" ~sub:"~~~"
    b

let test_containment_multiline () =
  let f =
    Failure.containment ~demand:Failure.Anywhere ~needle:"user=bob"
      ~haystack:"line one\nline two user=alice\nline three" ()
  in
  let b = failure_block f in
  contains ~msg:"multi-line haystack: block form"
    ~sub:
      "    needle    \"user=bob\": not found\n\
      \    haystack:\n\
      \      line one\n\
      \      line two user=alice\n\
      \      line three\n"
    b;
  not_contains ~msg:"multi-line haystack: no unified diff" ~sub:"--- expected" b;
  (* A found occurrence in a multi-line excerpt is marked under its line
     without colour, and coloured on it with. *)
  let found =
    Failure.containment ~found_at:14 ~demand:Failure.Anywhere ~needle:"secret"
      ~haystack:"line one\nthe1 secret here\nline three" ()
  in
  let plain = failure_block found in
  contains ~msg:"multi-line occurrence marked under its line"
    ~sub:
      "    haystack:\n\
      \      line one\n\
      \      the1 secret here\n\
      \           ~~~~~~\n\
      \      line three\n"
    plain;
  let colored = failure_block ~ansi:true found in
  contains
    ~msg:
      "multi-line occurrence: under colour it is bold red on its line, unmarked"
    ~sub:
      "      line one\n\
      \      the1 \027[1;31msecret\027[0m here\n\
      \      line three\n"
    colored;
  is_true
    ~msg:"multi-line occurrence: colour replaces the mark and nothing else"
    (strip_ansi colored = without_marks plain)

(* The not-found display cap: with no occurrence to mark, the haystack is
   context rather than evidence, so the display shows a small head window
   (at most 10 lines and 1 KiB) and the excerpt line states the cut in
   the same words it states the stored bound. A found occurrence keeps the
   full stored window: there the excerpt is the evidence. *)
let test_containment_not_found_cap () =
  (* Single-line content cuts at the byte bound, and the verdict sits
     directly above the excerpt. The cap exists so an 8 KiB context dump
     cannot scroll the diagnosis away. *)
  let haystack = String.make 20_006 'a' in
  let f =
    Failure.containment ~demand:Failure.Anywhere ~needle:"NOPE" ~haystack ()
  in
  let b = failure_block f in
  contains ~msg:"cap: the verdict line is adjacent to the excerpt"
    ~sub:
      ("    needle    \"NOPE\": not found\n    haystack  " ^ String.make 64 'a')
    b;
  not_contains ~msg:"cap: nothing beyond the display window prints"
    ~sub:(String.make 1025 'a') b;
  contains ~msg:"cap: the elision line states the shown range"
    ~sub:"    (excerpt: bytes 0-1023 of a 20006-byte haystack)\n" b;
  (* Line-structured content cuts after ten complete lines, well under the
     byte bound. 40 lines of 21 bytes: the cut lands after "line 09"'s
     newline, byte 219. *)
  let line i = Printf.sprintf "line %02d filler filler" i in
  let haystack = String.concat "\n" (List.init 40 line) in
  let f =
    Failure.containment ~demand:Failure.Anywhere ~needle:"NOPE" ~haystack ()
  in
  let b = failure_block f in
  contains ~msg:"cap: the tenth line still prints"
    ~sub:"      line 09 filler filler\n" b;
  not_contains ~msg:"cap: the eleventh line does not" ~sub:"line 10" b;
  contains ~msg:"cap: the multi-line elision line states the shown range"
    ~sub:"    (excerpt: bytes 0-219 of a 879-byte haystack)\n" b;
  (* A found occurrence keeps the stored window whole: not_contains on a
     3 KiB haystack shows all of it, uncapped and unelided. *)
  let haystack = String.make 2_994 'x' ^ "secret" in
  let f =
    Failure.containment ~found_at:2_994 ~demand:Failure.Anywhere
      ~needle:"secret" ~haystack ()
  in
  let b = failure_block f in
  contains ~msg:"found-at: the full stored window prints" ~sub:haystack b;
  not_contains ~msg:"found-at: no elision line for a complete excerpt"
    ~sub:"(excerpt:" b;
  (* An in_order chain break keeps its window even with nothing found: the
     cursor-anchored excerpt is the region still to be matched, which is
     the diagnosis, not context. *)
  let f =
    Failure.containment
      ~demand:(Failure.Ordered { index = 1; resumed_at = 9_000 })
      ~needle:"NOPE" ~haystack:(String.make 10_000 'a') ()
  in
  contains ~msg:"in_order: the cursor-anchored window is not capped"
    ~sub:"    (excerpt: bytes 4904-9999 of a 10000-byte haystack)\n"
    (failure_block f)

(* An affix is named for its demand, and a misplaced one says where it was
   demanded. An absent suffix shows the end of the haystack, where it was
   demanded. *)
let test_affix_blocks () =
  let path = "sessions/ghost/session.json" in
  let prefix_absent =
    Failure.containment ~demand:Failure.Prefix ~needle:"users/" ~haystack:path
      ()
  in
  let prefix_elsewhere =
    Failure.containment ~demand:Failure.Prefix ~found_at:9 ~needle:"ghost"
      ~haystack:path ()
  in
  let suffix_elsewhere =
    Failure.containment ~demand:Failure.Suffix ~found_at:0 ~needle:"session"
      ~haystack:path ()
  in
  contains ~msg:"starts_with absent: the prefix is not found"
    ~sub:"    prefix    \"users/\": not found\n"
    (failure_block prefix_absent);
  contains ~msg:"starts_with elsewhere: not at the start"
    ~sub:"    prefix    \"ghost\": found at byte 9, not at the start\n"
    (failure_block prefix_elsewhere);
  contains ~msg:"ends_with elsewhere: not at the end"
    ~sub:"    suffix    \"session\": found at byte 0, not at the end\n"
    (failure_block suffix_elsewhere);
  equal ~msg:"headline: an absent prefix" string
    {|prefix "users/" not found (27-byte haystack)|}
    (Report.headline prefix_absent);
  equal ~msg:"headline: a misplaced prefix" string
    {|prefix "ghost" found at byte 9, not at the start|}
    (Report.headline prefix_elsewhere);
  equal ~msg:"headline: a misplaced suffix" string
    {|suffix "session" found at byte 0, not at the end|}
    (Report.headline suffix_elsewhere);
  let line i = Printf.sprintf "line %02d filler filler" i in
  let b =
    failure_block
      (Failure.containment ~demand:Failure.Suffix ~needle:"END"
         ~haystack:(String.concat "\n" (List.init 40 line))
         ())
  in
  contains ~msg:"ends_with absent: the last line prints"
    ~sub:"      line 39 filler filler\n" b;
  not_contains ~msg:"ends_with absent: the head does not" ~sub:"line 00" b;
  contains ~msg:"ends_with absent: the range is the end"
    ~sub:"    (excerpt: bytes 660-878 of a 879-byte haystack)\n" b

let test_containment_headlines () =
  is_true ~msg:"headline: not_contains names the offset"
    (Report.headline not_contains_failure = {|needle "secret" found at byte 10|});
  let contains_failure =
    Failure.containment ~demand:Failure.Anywhere ~needle:"NOPE"
      ~haystack:(String.make 20_006 'a') ()
  in
  is_true ~msg:"headline: contains names the haystack size"
    (Report.headline contains_failure
    = {|needle "NOPE" not found (20006-byte haystack)|})

(* The demanded-occurrence block: [in_order]'s chain break. The fixtures
   are the payloads the assertions chapter's transcripts come from, so the
   manual cannot drift from the renderer without failing here.

   Byte offsets in the chain haystack: connect 0, send 8, disconnect 13,
   authenticate 24, end 36. *)

let chain_haystack = "connect send disconnect authenticate"

(* [in_order ~subs:["connect"; "authenticate"; "disconnect"]]: the log shows
   the last two events the wrong way round, so the search for "disconnect"
   resumed at 36 (past "authenticate") and its only occurrence, byte 13,
   is behind the cursor. *)
let out_of_order_failure =
  Failure.containment ~found_at:13
    ~demand:(Failure.Ordered { index = 2; resumed_at = 36 })
    ~needle:"disconnect" ~haystack:chain_haystack ()

let missing_element_failure =
  Failure.containment
    ~demand:(Failure.Ordered { index = 2; resumed_at = 36 })
    ~needle:"teardown" ~haystack:chain_haystack ()

let test_in_order_block () =
  let b = failure_block out_of_order_failure in
  (* Which element broke the chain is its own line, on the label gutter;
     the verdict slot carries where the search stood and where the element
     actually is ("there, but too early" rather than "not there"). *)
  contains ~msg:"in_order: the element index is a line of its own"
    ~sub:"    element   2\n" b;
  contains ~msg:"in_order: the verdict names both offsets"
    ~sub:
      "    needle    \"disconnect\": found at byte 13, before the search \
       resumed at byte 36\n"
    b;
  contains ~msg:"in_order: the early occurrence is marked in the haystack"
    ~sub:
      ("    haystack  " ^ chain_haystack ^ "\n" ^ String.make 27 ' '
     ^ "~~~~~~~~~~\n")
    b;
  let colored = failure_block ~ansi:true out_of_order_failure in
  contains ~msg:"in_order: under colour the early occurrence is bold red"
    ~sub:
      "    \027[2mhaystack\027[0m  connect send \027[1;31mdisconnect\027[0m \
       authenticate\n"
    colored;
  is_true ~msg:"in_order: colour replaces the mark and nothing else"
    (strip_ansi colored = without_marks b);
  (* No occurrence anywhere: the verdict is the cursor alone and there is
     nothing to mark. *)
  let b = failure_block missing_element_failure in
  contains ~msg:"in_order: a missing element names only the cursor"
    ~sub:
      "    element   2\n\
      \    needle    \"teardown\": not found at or after byte 36\n"
    b;
  not_contains ~msg:"in_order: nothing is marked when nothing occurs" ~sub:"~~~"
    b

let test_demand_headlines () =
  is_true ~msg:"headline: in_order names the element, its offset and the cursor"
    (Report.headline out_of_order_failure
    = {|element 2 "disconnect" out of order: at byte 13, before byte 36|});
  let missing =
    {|element 2 "teardown" not found at or after byte 36 (36-byte haystack)|}
  in
  is_true
    ~msg:"headline: a missing element names the cursor and the haystack size"
    (Report.headline missing_element_failure = missing)

let test_satisfies_no_refinement () =
  (* The claim sentence is a description, not a rendering: never diff or
     refine the two. *)
  let f = Failure.predicate ~claim:"value satisfying the predicate" "-3" in
  let b = failure_block f in
  contains ~msg:"satisfies: two label lines"
    ~sub:"    expected  value satisfying the predicate\n    actual    -3\n" b;
  not_contains ~msg:"satisfies: no marker line against the claim" ~sub:"~" b;
  let multi =
    failure_block
      (Failure.predicate ~claim:"value satisfying the predicate" "[0; 1;\n 2]")
  in
  contains ~msg:"satisfies: multi-line value prints in block form"
    ~sub:
      "    expected  value satisfying the predicate\n\
      \    actual:\n\
      \      [0; 1;\n\
      \       2]\n"
    multi;
  not_contains ~msg:"satisfies: no unified diff against the claim"
    ~sub:"--- expected" multi;
  contains ~msg:"satisfies: each line of the block is red, no style spans two"
    ~sub:
      "    \027[2mactual:\027[0m\n\
      \      \027[31m[0; 1;\027[0m\n\
      \      \027[31m 2]\027[0m\n"
    (failure_block ~ansi:true
       (Failure.predicate ~claim:"value satisfying the predicate" "[0; 1;\n 2]"));
  let matches =
    failure_block (Failure.predicate ~claim:"a match" "Error \"boom\"")
  in
  not_contains ~msg:"matches: no refinement either" ~sub:"~" matches

(* An invisible difference in hunks: the [~] line under the [-] line *)

let test_trailing_whitespace_hunks () =
  let b =
    failure_block
      (Failure.equality ~expected:"line one \nline two"
         ~actual:"line one\nline two" ())
  in
  contains
    ~msg:
      "a one-to-one pair differing in trailing space: the line keeps its space \
       and a ~ sits under it"
    ~sub:
      "    @@ -1,2 +1,2 @@\n\
      \    - line one \n\
      \              ~\n\
      \    + line one\n\
      \      line two\n"
    b;
  not_contains ~msg:"no middle dot" ~sub:"\u{00B7}" b;
  (* The space is the [+] line's: the mark stands where it will be. *)
  let b =
    failure_block
      (Failure.equality ~expected:"ab\nline two" ~actual:"ab  \nline two" ())
  in
  contains ~msg:"an inserted trailing run is marked at its columns"
    ~sub:"    - ab\n        ~~\n    + ab  \n" b;
  (* A tab has no fixed width: the mark line repeats the line's tabs, so
     it aligns on any tab stops. Context lines keep their bytes. *)
  let b =
    failure_block
      (Failure.equality ~expected:"a\tx\t\ncommon \ny"
         ~actual:"a\tx\ncommon \ny" ())
  in
  contains ~msg:"a trailing tab is marked, the mark line repeating the tabs"
    ~sub:"    - a\tx\t\n       \t ~\n    + a\tx\n" b;
  not_contains ~msg:"no arrow glyph" ~sub:"\u{2192}" b;
  contains ~msg:"context lines keep raw trailing whitespace"
    ~sub:"      common \n" b;
  is_true ~msg:"context lines draw no mark" (occurrences_of ~sub:"~" b = 1);
  (* A visible difference needs no mark, whatever its trailing spaces; nor
     does a run of changes, whose lines answer each other in no order. *)
  let b =
    failure_block
      (Failure.equality ~expected:"foo \nline two" ~actual:"bar\nline two" ())
  in
  not_contains ~msg:"a visible difference draws no mark" ~sub:"~" b;
  let b =
    failure_block (Failure.equality ~expected:"a \nb \nz" ~actual:"a\nb\nz" ())
  in
  not_contains ~msg:"a run of changes is no one-to-one pair" ~sub:"~" b;
  (* The mark is in its role under colour, outside the line's own span,
     and the two settings print the same bytes. *)
  let plain =
    failure_block
      (Failure.equality ~expected:"line one \nline two"
         ~actual:"line one\nline two" ())
  in
  let colored =
    failure_block ~ansi:true
      (Failure.equality ~expected:"line one \nline two"
         ~actual:"line one\nline two" ())
  in
  contains ~msg:"ansi path: the expected line whole, then the mark in its role"
    ~sub:
      ("\027[32m- line one \027[0m\n" ^ String.make 14 ' '
     ^ "\027[31m~\027[0m\n")
    colored;
  is_true ~msg:"ansi path: the same bytes once the escapes are stripped"
    (strip_ansi colored = plain);
  (* One meaning for green, across both diff paths.

     A transcript routinely shows both (a short value marks its spans,
     a multi-line one emits hunks) and until [text] made the multi-line
     path ordinary, nobody hit them side by side often enough to notice
     that green meant "expected" on one and "actual" on the other. The
     [-]/[+] sigils carry the diff convention; the colour carries the
     report's. Pin both here so they cannot drift apart again. *)
  let spans =
    failure_block ~ansi:true
      (Failure.equality ~expected:"the quick brown fox"
         ~actual:"the quick brawn fox" ())
  in
  contains ~msg:"span path: expected side is green"
    ~sub:"the quick br\027[1;32mo\027[0mwn fox" spans;
  contains ~msg:"span path: actual side is red"
    ~sub:"the quick br\027[1;31ma\027[0mwn fox" spans;
  let hunks =
    failure_block ~ansi:true
      (Failure.equality ~expected:"keep\nexpected\n" ~actual:"keep\nactual\n" ())
  in
  contains ~msg:"hunk path: expected side is green too"
    ~sub:"\027[32m- expected\027[0m" hunks;
  contains ~msg:"hunk path: actual side is red too"
    ~sub:"\027[31m+ actual\027[0m" hunks;
  not_contains ~msg:"hunk path: no diff-tool colouring survives"
    ~sub:"\027[31m- expected" hunks;
  (* Baseline mismatch diffs share pp_hunks, the single producer. *)
  let snap =
    failure_block
      (Failure.baseline
         (Failure.Literal { exact = true })
         (Failure.Mismatch
            {
              expected = Failure.text "a \nb\n";
              actual = Failure.text "a\nb\n";
            }))
  in
  contains ~msg:"baseline diffs mark a trailing space too"
    ~sub:
      "    expect_exact: mismatch\n\
      \    @@ -1,2 +1,2 @@\n\
      \    - a \n\
      \       ~\n\
      \    + a\n"
    snap

(* Uncaught exceptions *)

let test_uncaught_wording () =
  let b = failure_block (Failure.raised ~actual:"Not_found" ()) in
  contains ~msg:"uncaught: the sentence, the exception indented under it"
    ~sub:"    uncaught exception:\n      Not_found\n" b;
  not_contains ~msg:"uncaught: never the bare anchor of a raises assertion"
    ~sub:"    raised  " b;
  not_contains ~msg:"uncaught: no expected side" ~sub:"expected exception" b;
  not_contains ~msg:"uncaught: never borrows the predicate wording"
    ~sub:"predicate" b;
  (* Inside a property block, recursively. *)
  let prop =
    failure_block
      (Failure.property
         ~inner:(Failure.raised ~actual:"Dune__exe__V.Boom(50)" ())
         ~rendered:"50" ~case_index:3 ~shrink_steps:2 ~root:Fixtures.root
         ~examples:false ())
  in
  contains ~msg:"uncaught inside a property inner"
    ~sub:
      "    which failed with:\n\
      \      uncaught exception:\n\
      \        Dune__exe__V.Boom(50)\n"
    prop;
  contains ~msg:"uncaught: a multi-line exception is a block under the sentence"
    ~sub:"    uncaught exception:\n      Parse_error(\n        line 3)\n"
    (failure_block (Failure.raised ~actual:"Parse_error(\n  line 3)" ()));
  (* raises_match keeps its wording, pinned above in
     [test_raise_message_diff_guards]; the (None, None) arm serves both. *)
  contains ~msg:"wanted-any arm unchanged"
    ~sub:"    expected an exception, but none was raised\n"
    (failure_block (Failure.raised ()))

(* Timed-out shrink searches (D2) *)

let test_timed_out_marker () =
  let f =
    Failure.property ~shrink_end:(Failure.Timed_out 0.3) ~rendered:"9"
      ~case_index:4 ~shrink_steps:2 ~root:Fixtures.root ~examples:false ()
  in
  let b = failure_block f in
  contains ~msg:"timed-out: marker line follows the counterexample"
    ~sub:
      "    counterexample (case 4, shrunk 2 steps): 9\n\
      \    timed out after 0.3s while shrinking; counterexample may not be \
       minimal\n\
      \    replay: "
    b;
  not_contains
    ~msg:"timed-out: the block says it once, in the line under the head"
    ~sub:"shrinking timed out" b;
  is_true ~msg:"timed-out: headline carries the clause"
    (Report.headline f
   = "property failed (case 4, shrunk 2 steps, shrinking timed out): 9");
  let plain = failure_block Fixtures.prop_failure in
  not_contains ~msg:"no marker without a timeout" ~sub:"timed out" plain;
  not_contains ~msg:"no headline mark without a timeout" ~sub:"timed out"
    (Report.headline Fixtures.prop_failure)

(* Spent shrink budgets (D2's other stopping condition)

   The flag on the payload is not the report: a reader sees a line under
   the block's counterexample and a clause in the one-line headline, both
   saying that "shrunk N steps" here is where the search stopped counting,
   not where it converged. Pinned present and absent, because a mark that
   printed unconditionally would call every converged search truncated. *)

let test_budget_spent_marker () =
  let f =
    Failure.property ~shrink_end:Failure.Budget_spent ~rendered:"9"
      ~case_index:4 ~shrink_steps:50 ~root:Fixtures.root ~examples:false ()
  in
  let b = failure_block f in
  contains ~msg:"budget spent: detail line follows the counterexample"
    ~sub:
      "    counterexample (case 4, shrunk 50 steps): 9\n\
      \    shrinking stopped after 50 steps; counterexample may not be minimal\n\
      \    replay: "
    b;
  not_contains
    ~msg:"budget spent: the block says it once, in the line under the head"
    ~sub:"shrink limit reached" b;
  is_true ~msg:"budget spent: headline carries the clause"
    (Report.headline f
   = "property failed (case 4, shrunk 50 steps, shrink limit reached): 9");
  let plain = failure_block Fixtures.prop_failure in
  not_contains ~msg:"no detail line without a spent budget"
    ~sub:"shrinking stopped" plain;
  not_contains ~msg:"no clause without a spent budget" ~sub:"may not be minimal"
    plain;
  not_contains ~msg:"no headline clause without a spent budget"
    ~sub:"shrink limit"
    (Report.headline Fixtures.prop_failure);
  (* An example never shrinks, so neither mark applies to one. *)
  let example =
    Failure.property ~shrink_end:Failure.Budget_spent ~rendered:"9"
      ~case_index:0 ~shrink_steps:0 ~root:Fixtures.root ~examples:true ()
  in
  is_true ~msg:"an example carries no clause"
    (Report.headline example = "property failed (example 1): 9")

(* A candidate whose forcing raised: the block names the exception on a line
   of its own, above the line every stopped search prints, and the headline
   says that shrinking stopped, not that a limit was reached. *)
let test_candidate_raised_marker () =
  let f =
    Failure.property
      ~shrink_end:
        (Failure.Candidate_raised (Failure.text {|Failure("no small values")|}))
      ~rendered:"9" ~case_index:4 ~shrink_steps:3 ~root:Fixtures.root
      ~examples:false ()
  in
  let b = failure_block f in
  contains ~msg:"candidate raised: two lines follow the counterexample"
    ~sub:
      "    counterexample (case 4, shrunk 3 steps): 9\n\
      \    shrinking stopped after 3 steps: a candidate raised Failure(\"no \
       small values\")\n\
      \    counterexample may not be minimal\n\
      \    replay: "
    b;
  not_contains ~msg:"candidate raised: no limit is claimed"
    ~sub:"shrink limit reached" b;
  is_true ~msg:"candidate raised: headline says shrinking stopped"
    (Report.headline f
   = "property failed (case 4, shrunk 3 steps, shrinking stopped): 9")

(* Inner failures without a location (D4) *)

let test_inner_label_without_location () =
  let inner_no_loc = Failure.equality ~expected:"true" ~actual:"false" () in
  let b =
    failure_block
      (Failure.property ~inner:inner_no_loc ~rendered:"7" ~case_index:0
         ~shrink_steps:0 ~root:Fixtures.root ~examples:false ())
  in
  contains
    ~msg:"a location-less inner failure: [which failed with:], then the facts"
    ~sub:"    which failed with:\n      expected  true\n" b;
  not_contains ~msg:"and no dangling [at:]" ~sub:"which failed at:" b;
  let located = failure_block Fixtures.prop_failure in
  contains
    ~msg:"a located inner failure: [which failed at:] over its bare location"
    ~sub:
      "    which failed at:\n      test/test_geo.ml:18\n      expected  true\n"
    located;
  not_contains ~msg:"and no [with:]" ~sub:"which failed with:" located

(* Command hints per invocation *)

let test_hints_per_invocation () =
  let exe = `Exe "./_build/default/qa/x/t.exe" in
  let accept =
    failure_block ~invocation:exe ~filter:"cli › cli help" Fixtures.snap_missing
  in
  contains ~msg:"accept completes the executable, scoped to the block's test"
    ~sub:"    accept: ./_build/default/qa/x/t.exe -u -f 'cli › cli help'\n"
    accept;
  not_contains ~msg:"accept carries no trailing advice" ~sub:"then review"
    accept;
  not_contains ~msg:"accept under Exe never spells dune promote"
    ~sub:"dune promote" accept;
  let replay =
    failure_block ~invocation:exe ~filter:"mod7" Fixtures.prop_failure
  in
  contains ~msg:"replay hint completes the executable with the flags"
    ~sub:
      "    replay: ./_build/default/qa/x/t.exe --seed s1:7be1d2c904aa31f5 -f \
       'mod7'\n"
    replay;
  let bare = failure_block ~invocation:exe Fixtures.prop_failure in
  contains ~msg:"replay hint without a filter carries the seed alone"
    ~sub:"    replay: ./_build/default/qa/x/t.exe --seed s1:7be1d2c904aa31f5\n"
    bare;
  (* Under a build action acceptance is dune's, given this block's file:
     a literal's source file, a file baseline's path. *)
  let mirrors = failure_block Fixtures.snap_mismatch in
  contains ~msg:"Mirrors accept promotes the literal's source file"
    ~sub:"    accept: dune promote test/test_cli.ml\n" mirrors;
  not_contains ~msg:"Mirrors accept spelling names no flag" ~sub:" -u" mirrors;
  contains ~msg:"Mirrors accept promotes a file baseline by its path"
    ~sub:"    accept: dune promote test/help.expected\n"
    (failure_block
       (Failure.baseline (Failure.File "test/help.expected")
          (Failure.Mismatch
             { expected = Failure.text "a\n"; actual = Failure.text "b\n" })));
  (* Promotion never creates a file: a missing file baseline under dune
     is accepted by creating it first, and the hint says so. *)
  let missing = failure_block Fixtures.snap_missing in
  contains ~msg:"Mirrors accept spelling for a missing file creates it first"
    ~sub:
      "    accept: touch 'test/help.expected' && dune runtest; dune promote \
       test/help.expected\n"
    missing;
  not_contains ~msg:"and never spells a bare dune promote"
    ~sub:"    accept: dune promote\n" missing

(* A block ends on a command only when the command says what the block
   does not: one line per distinct command line, in the order accept,
   replay, and none at all otherwise. No block prints a [rerun:]. *)
let test_hint_lines () =
  let plain = Failure.message "b" in
  equal ~msg:"a block with nothing to accept or replay ends on its facts" string
    "    b\n"
    (failure_block ~invocation:(`Exe "./t.exe") ~filter:"math › adds" plain);
  equal ~msg:"under a build action too" string "    b\n"
    (failure_block ~filter:"math › adds" plain);
  contains ~msg:"a path's quote is closed around"
    ~sub:"    accept: ./t.exe -u -f 'it'\\''s'\n"
    (failure_block ~invocation:(`Exe "./t.exe") ~filter:"it's"
       Fixtures.snap_mismatch);
  (* An armed run's failures are the mutant's: a replay keeps it armed,
     and an armed run accepts nothing. *)
  let armed = "lib/calc.ml:9:12:add" in
  equal ~msg:"an armed run's block with nothing to replay ends on its facts"
    string "    b\n"
    (failure_block ~invocation:(`Exe "./t.exe") ~armed ~filter:"math › adds"
       plain);
  contains ~msg:"an identifier a shell would split is quoted"
    ~sub:
      "    replay: ./t.exe --arm 'my lib/calc.ml:9:12:add' --seed \
       s1:7be1d2c904aa31f5\n"
    (failure_block ~invocation:(`Exe "./t.exe") ~armed:"my lib/calc.ml:9:12:add"
       Fixtures.prop_failure);
  contains ~msg:"an armed run's replay carries --arm"
    ~sub:
      "    replay: ./t.exe --arm lib/calc.ml:9:12:add --seed \
       s1:7be1d2c904aa31f5 -f 'mod7'\n"
    (failure_block ~invocation:(`Exe "./t.exe") ~armed ~filter:"mod7"
       Fixtures.prop_failure);
  contains ~msg:"an armed run's replay carries the mirror under a build action"
    ~sub:
      "    replay: WINDTRAP_MUTATE_ARM=lib/calc.ml:9:12:add \
       WINDTRAP_SEED=s1:7be1d2c904aa31f5 WINDTRAP_FILTER='mod7' dune runtest\n"
    (failure_block ~armed ~filter:"mod7" Fixtures.prop_failure);
  is_true
    ~msg:"an armed run's baseline failure is never accepted: no hint at all"
    (Sections.hints ~armed ~invocation:(`Exe "./t.exe") ~filter:(Some "t")
       [ Fixtures.snap_mismatch; Fixtures.snap_missing ]
    = []);
  contains ~msg:"a control byte in a path never breaks the hint's line"
    ~sub:
      "    replay: ./t.exe --seed s1:7be1d2c904aa31f5 -f \
       $'it\\'s\\ttwo\\nlines\\x1b[0m'\n"
    (failure_block ~invocation:(`Exe "./t.exe")
       ~filter:"it's\ttwo\nlines\027[0m" Fixtures.prop_failure);
  let hints = Sections.hints ~invocation:(`Exe "./t.exe") ~filter:(Some "t") in
  is_true ~msg:"a failure with no command of its own adds none"
    (hints [ plain ] = []
    && hints [ plain; Fixtures.snap_mismatch ] = [ "accept: ./t.exe -u -f 't'" ]
    );
  is_true ~msg:"one line per distinct command line, accept before replay"
    (hints
       [ Fixtures.prop_failure; Fixtures.snap_mismatch; Fixtures.snap_missing ]
    = [
        "accept: ./t.exe -u -f 't'";
        "replay: ./t.exe --seed s1:7be1d2c904aa31f5 -f 't'";
      ]);
  is_true ~msg:"two files under a build action are two acceptances"
    (Sections.hints ~filter:(Some "t")
       [ Fixtures.snap_mismatch; Fixtures.snap_missing ]
    = [
        "accept: dune promote test/test_cli.ml";
        "accept: touch 'test/help.expected' && dune runtest; dune promote \
         test/help.expected";
      ]);
  (* In the transcript the hints close the block, once for the whole test,
     after the captured tail. *)
  let two =
    Fixtures.result [ "cli"; "both" ]
      (Failure.Fail
         [
           Fixtures.snap_mismatch;
           Failure.with_output_tail
             (Failure.tail "log line\n")
             Fixtures.prop_failure;
         ])
  in
  let t =
    with_renderer ~invocation:(`Exe "./t.exe") (fun r ->
        Report.finish r ~release_failures:[] ~results:[ two ] ~duration:0.1 ())
  in
  contains ~msg:"the block's hints follow the tail, accept then replay"
    ~sub:
      ("      log line\n\
       \    accept: ./t.exe -u -f 'cli › both'\n\
       \    replay: ./t.exe --seed s1:7be1d2c904aa31f5 -f 'cli › both'\n"
     ^ closing_rule ^ "\n\n1 failed in 100ms.\n")
    t;
  is_true ~msg:"a hint prints once per block"
    (occurrences_of ~sub:"accept:" t = 1)

(* A withheld correction (Run, Corrections): the run kept none of the
   attempt's corrections, so no command accepts the block's baselines. The
   runner records it on the failure, and these fixtures mark theirs as it
   does. *)

let test_withheld_correction () =
  let outside = Failure.with_withheld Failure.Failed_outside in
  let kept_none =
    "no correction was kept: the test also failed outside its expectations; \
     fix that failure and rerun"
  in
  let literal = outside Fixtures.snap_mismatch
  and file =
    outside
      (Failure.baseline (Failure.File "p.expected")
         (Failure.Mismatch
            { expected = Failure.text "a\n"; actual = Failure.text "b\n" }))
  and missing = outside Fixtures.snap_missing in
  (* Run by hand, whatever the mode: [-u] would rewrite nothing. *)
  let exe = failure_block ~invocation:(`Exe "./t.exe") ~filter:"t" in
  is_true ~msg:"a literal by hand: the reason, and nothing after it"
    (String.ends_with
       ~suffix:("    + line 2\n      line three\n    " ^ kept_none ^ "\n")
       (exe literal));
  not_contains ~msg:"a literal by hand: no -u to type" ~sub:"accept:"
    (exe literal);
  is_true ~msg:"a file by hand: the reason, and nothing after it"
    (String.ends_with ~suffix:("    + b\n    " ^ kept_none ^ "\n") (exe file));
  not_contains ~msg:"a file by hand: no -u to type" ~sub:"accept:" (exe file);
  (* Under a build action ([--corrected], an inline runner): nothing was
     written beside the file, so [dune promote] has nothing to promote. *)
  let action = failure_block ~filter:"t" in
  is_true
    ~msg:"a literal under a build action: the reason, and nothing after it"
    (String.ends_with
       ~suffix:("      line three\n    " ^ kept_none ^ "\n")
       (action literal));
  not_contains ~msg:"a literal under a build action: nothing to promote"
    ~sub:"dune promote" (action literal);
  is_true ~msg:"a file under a build action: the reason, and nothing after it"
    (String.ends_with
       ~suffix:("    + b\n    " ^ kept_none ^ "\n")
       (action file));
  not_contains ~msg:"a file under a build action: nothing to promote"
    ~sub:"dune promote" (action file);
  not_contains
    ~msg:"a missing file under a build action: no file to touch either"
    ~sub:"touch" (action missing);
  contains ~msg:"a missing file: the proposed text still prints"
    ~sub:"    proposed (3 lines):\n" (action missing);
  (* The reason is a fact line: it opens the block's closing lines, once,
     before a replay when the block has one. *)
  let plain = Failure.message "boom" in
  let hints = Sections.hints ~invocation:(`Exe "./t.exe") ~filter:(Some "t") in
  is_true ~msg:"a block's closing lines: the reason once, and nothing after it"
    (hints [ plain; literal; missing ] = [ kept_none ]);
  is_true ~msg:"a property beside it keeps its replay"
    (hints [ Fixtures.prop_failure; literal ]
    = [ kept_none; "replay: ./t.exe --seed s1:7be1d2c904aa31f5 -f 't'" ]);
  is_true ~msg:"a kept correction beside nothing else is accepted as before"
    (hints [ Fixtures.snap_mismatch ] = [ "accept: ./t.exe -u -f 't'" ]);
  not_contains ~msg:"and draws no reason" ~sub:"no correction was kept"
    (exe Fixtures.snap_mismatch);
  (* A test that failed an expectation and skipped keeps none either, and
     did not fail anywhere else. *)
  let skipped = Failure.with_withheld Failure.Skipped Fixtures.snap_mismatch in
  is_true ~msg:"a skip beside the expectation: its own reason"
    (hints [ skipped ]
    = [
        "no correction was kept: the test also skipped; skip before the \
         expectation or not at all, and rerun";
      ]);
  (* A correction the source refused names its literal's line, beside the
     accept of a correction the attempt kept; each refused literal has its
     line, and a fact of the attempt follows them. *)
  let refused line =
    Failure.with_withheld
      (Failure.Refused { line; reason = "the source file cannot be read: x" })
      Fixtures.snap_mismatch
  in
  is_true ~msg:"a refused literal beside a kept one: its fact, then the accept"
    (hints [ refused 4; Fixtures.snap_mismatch ]
    = [
        "correction refused (line 4): the source file cannot be read: x";
        "accept: ./t.exe -u -f 't'";
      ]);
  is_true ~msg:"two refused literals, then a failure outside: three facts"
    (hints [ plain; outside (refused 4); outside (refused 9); literal ]
    = [
        "correction refused (line 4): the source file cannot be read: x";
        "correction refused (line 9): the source file cannot be read: x";
        kept_none;
      ]);
  is_true ~msg:"a conflict: its own fact, no accept"
    (hints [ Failure.with_withheld Failure.Conflict Fixtures.snap_mismatch ]
    = [
        "no correction was kept: another check of this baseline produced a \
         different text earlier in the run";
      ]);
  (* An unresolvable path never had a correction to keep. *)
  is_true ~msg:"an unresolvable path draws no reason"
    (hints
       [
         outside
           (Failure.baseline (Failure.File "../x")
              (Failure.Unresolvable { candidate = "/tmp/x" }));
       ]
    = []);
  (* An armed run accepts nothing in the first place and says nothing
     about corrections: the difference is the mutant's. *)
  let armed = "lib/calc.ml:9:12:add" in
  is_true ~msg:"an armed run: no line at all"
    (Sections.hints ~armed ~invocation:(`Exe "./t.exe") ~filter:(Some "t")
       [ plain; literal ]
    = []);
  (* In the transcript the reason sits after the captured tail and closes
     the block. *)
  let both =
    Fixtures.result [ "cli"; "both" ]
      (Failure.Fail
         [ Failure.with_output_tail (Failure.tail "log line\n") plain; literal ])
  in
  let t =
    with_renderer ~invocation:(`Exe "./t.exe") (fun r ->
        Report.finish r ~release_failures:[] ~results:[ both ] ~duration:0.1 ())
  in
  contains ~msg:"the transcript: tail, reason, closing rule, summary"
    ~sub:
      ("      log line\n    " ^ kept_none ^ "\n" ^ closing_rule
     ^ "\n\n1 failed in 100ms.\n")
    t;
  not_contains ~msg:"the transcript offers no acceptance" ~sub:"accept:" t;
  (* Every transport projects the same failure. *)
  let annotated = Report.annotations ~release_failures:[] [ both ] in
  not_contains ~msg:"the annotations offer no acceptance" ~sub:"accept:"
    annotated;
  is_true ~msg:"the baseline's annotation ends on the reason"
    (String.ends_with ~suffix:("%0A    " ^ kept_none ^ "\n") annotated)

let test_armed_titles () =
  let failing =
    [
      Fixtures.result [ "sub"; "subtracts" ]
        (Failure.Fail [ Failure.message "b" ]);
      Fixtures.result [ "sub"; "retried" ]
        (Failure.Fail [ Failure.message "b" ])
        ~attempts:2;
    ]
  in
  let t =
    with_renderer ~invocation:(`Exe "./t.exe") ~armed:"lib/calc.ml:9:12:add"
      (fun r ->
        Report.finish r ~release_failures:[] ~results:failing ~duration:0.1 ())
  in
  contains ~msg:"an armed run's FAIL title says so"
    ~sub:
      "  FAIL  sub › subtracts (mutant armed)\n\
      \    b\n\n\
      \  FAIL  sub › retried (2 attempts, mutant armed)\n"
    t;
  contains ~msg:"the qualifier shares its parenthesis with the attempts"
    ~sub:"  FAIL  sub › retried (2 attempts, mutant armed)\n" t;
  let unarmed =
    with_renderer (fun r ->
        Report.finish r ~release_failures:[] ~results:failing ~duration:0.1 ())
  in
  not_contains ~msg:"an ordinary run's titles carry no qualifier"
    ~sub:"mutant armed" unarmed

(* Verbose label distributions *)

let test_verbose_pass_labels () =
  let stats =
    {
      Property.cases = 100;
      discards = 0;
      collected = [ ("even", 46) ];
      coverage = [];
    }
  in
  let passing =
    Fixtures.result ~prop_stats:stats ~duration:0.0012 [ "labels visible" ]
      Failure.Pass
  in
  let verbose =
    with_renderer ~mode:`Verbose (fun r -> Report.result r passing)
  in
  contains ~msg:"verbose: a passing property prints its label table"
    ~sub:"    labels (100 passing cases):\n       46.0%  even\n" verbose;
  is_true ~msg:"verbose: the table follows the PASS line"
    (String.starts_with ~prefix:"  PASS  labels visible" verbose);
  let compact = with_renderer (fun r -> Report.result r passing) in
  not_contains ~msg:"compact: no label table" ~sub:"labels (" compact;
  let unlabeled =
    with_renderer ~mode:`Verbose (fun r ->
        Report.result r
          (Fixtures.result
             ~prop_stats:
               {
                 Property.cases = 100;
                 discards = 0;
                 collected = [];
                 coverage = [];
               }
             [ "no labels" ] Failure.Pass))
  in
  not_contains ~msg:"verbose: no table without collected labels" ~sub:"labels ("
    unlabeled;
  let excused =
    with_renderer ~mode:`Verbose (fun r ->
        Report.result r
          { Fixtures.excused_result with Run.prop_stats = Some stats })
  in
  (* An expected failure's table is its block's, as a failure's is. *)
  contains ~msg:"verbose: an XFAIL's table is in its block" ~sub:"labels ("
    excused

(* Names on terminal surfaces: escaped by the sink, as every text *)

let test_name_sanitization () =
  let hostile = [ "first\nhalf" ] in
  let failing =
    Fixtures.result hostile (Failure.Fail [ Failure.message "b" ])
  in
  let verbose =
    with_renderer ~mode:`Verbose (fun r -> Report.result r failing)
  in
  contains ~msg:"verbose line escapes the newline" ~sub:{|FAIL  first\x0ahalf|}
    verbose;
  equal ~msg:"the row and the message stay one line each" string
    "  FAIL  first\\x0ahalf                              0.2ms\n    b\n\n"
    verbose;
  contains ~msg:"and so does a hint that spells the path"
    ~sub:
      "      actual    false\n\
      \    replay: WINDTRAP_SEED=s1:7be1d2c904aa31f5 \
       WINDTRAP_FILTER=$'first\\nhalf' dune runtest\n\n"
    (with_renderer ~mode:`Verbose (fun r ->
         Report.result r
           (Fixtures.result hostile (Failure.Fail [ Fixtures.prop_failure ]))));
  let block =
    with_renderer (fun r ->
        Report.finish r ~release_failures:[] ~results:[ failing ] ~duration:0.1
          ())
  in
  contains ~msg:"FAIL header escapes the newline" ~sub:{|  FAIL  first\x0ahalf|}
    block;
  let live =
    with_renderer ~ansi:true ~terminal:true (fun r ->
        Report.header r ~suite:"vnames" ~tests:2 ~seed:None ();
        Report.begin_test r ~path:hostile)
  in
  contains ~msg:"live tail escapes the newline" ~sub:{|first\x0ahalf|} live;
  not_contains ~msg:"live tail carries no raw newline" ~sub:"first\nhalf" live;
  (* Suite names: header, and the one-liner's prefix. *)
  let named =
    with_renderer (fun r ->
        Report.header r ~suite:"my\tsuite" ~tests:1 ~seed:None ();
        Report.result r (Fixtures.result [ "t" ] Failure.Pass);
        Report.finish r ~release_failures:[]
          ~results:[ Fixtures.result [ "t" ] Failure.Pass ]
          ~duration:0.1 ())
  in
  contains ~msg:"summary prefix keeps the tab" ~sub:"my\tsuite: 1 passed" named;
  let header =
    with_renderer ~mode:`Verbose (fun r ->
        Report.header r ~suite:"a\x07b" ~tests:1 ~seed:None ())
  in
  contains ~msg:"header escapes control bytes" ~sub:{|a\x07b: 1 test|} header;
  (* The slow section's rows share the treatment. *)
  let slow = Fixtures.result [ "sl\now" ] Failure.Pass ~duration:1.5 in
  let warned =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r slow;
        Report.finish r ~release_failures:[] ~results:[ slow ] ~duration:1.5 ())
  in
  contains ~msg:"slow row escapes the newline" ~sub:{|  1.5s  sl\x0aow|} warned;
  (* ESC is escaped as every control byte is, under both [ansi] settings:
     pinned in [test_ansi_hygiene]. *)
  let note =
    with_renderer ~mode:`Verbose (fun r -> Report.note r "releasing d\nb")
  in
  equal ~msg:"notes escape their fixture name" string "releasing d\\x0ab\n" note;
  (* The author's own words sit among the report's: a [?msg] prints its
     lines at the block's indentation, a skip reason and an
     expected-failure reason stay in their row, and the control bytes of
     all three are escaped as every text is. *)
  let annotated =
    Fixtures.result [ "t" ]
      (Failure.Fail
         [
           Failure.equality ~msg:"first\nsecond\x07" ~expected:"1" ~actual:"2"
             ();
         ])
  in
  let block =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r annotated)
  in
  contains
    ~msg:"a ?msg keeps its lines, each inside the block, control bytes escaped"
    ~sub:"\n    first\n    second\\x07\n    expected  1\n" block;
  let rows =
    with_renderer ~mode:`Verbose (fun r ->
        Report.header r ~suite:"s" ~tests:2 ~seed:None ();
        Report.result r
          (Fixtures.result [ "skipped" ] (Failure.Skip (Some "no\ndb")));
        Report.result r
          (Fixtures.result [ "excused" ]
             ~xfail:{ Test_tree.reason = Some "issue\t42" }
             (Failure.Fail [ Fixtures.eq_failure ])))
  in
  contains ~msg:"a skip reason stays in its row"
    ~sub:{|  SKIP  skipped (no\x0adb)|} rows;
  contains ~msg:"and so does an expected failure's"
    ~sub:
      ("  XFAIL  excused" ^ String.make 35 ' '
     ^ "0.2ms (expected failure: issue\t42)")
    rows

(* Source excerpts resolve against the project root *)

(* The location is the bare [file:line]: no anchor word, and nothing about
   [~__POS__] anywhere in the output. A phase other than the body is its
   tag before it. *)
let test_location_forms () =
  let declared = Fixtures.loc "test/test_users.ml" 88 in
  let located f = { f with Failure.loc = Some declared } in
  let tail = located (Failure.equality ~expected:"1" ~actual:"2" ()) in
  equal ~msg:"declaration: the bare location opens the entry" string
    "    test/test_users.ml:88\n    expected  1\n    actual    2\n"
    (failure_block tail);
  contains ~msg:"declaration: ansi renders the whole line dim"
    ~sub:"    \027[2mtest/test_users.ml:88\027[0m\n"
    (failure_block ~ansi:true tail);
  (* An uncaught exception was raised by no verb, and its backtrace names
     the line. *)
  let uncaught =
    Failure.raised ~actual:"Not_found"
      ~backtrace:"Raised at Parser.parse in file \"lib/parser.ml\", line 40" ()
  in
  (* Every kind. *)
  List.iter
    (fun (kind, f) ->
      let b = failure_block (located f) in
      is_true
        ~msg:(kind ^ ": the bare location opens the entry")
        (String.starts_with ~prefix:"    test/test_users.ml:88\n" b);
      not_contains ~msg:(kind ^ ": nothing about ~__POS__") ~sub:"__POS__" b;
      not_contains ~msg:(kind ^ ": no [test declared at]") ~sub:"declared" b;
      not_contains ~msg:(kind ^ ": no [at] anchor") ~sub:" at test/" b)
    [
      ("a file baseline", Fixtures.snap_missing);
      ("a literal baseline", Fixtures.snap_mismatch);
      ("a message", Failure.message "boom");
      ("a raise verb", Fixtures.raise_failure);
      ("a property", Fixtures.prop_failure);
      ("an uncaught exception", uncaught);
    ];
  contains ~msg:"declaration: a property's counterexample follows its location"
    ~sub:"    test/test_users.ml:88\n    counterexample"
    (failure_block (located Fixtures.prop_failure));
  contains ~msg:"declaration: an uncaught exception follows its location"
    ~sub:"    test/test_users.ml:88\n    uncaught exception:\n      Not_found\n"
    (failure_block (located uncaught));
  let recorded = failure_block Fixtures.eq_failure in
  is_true ~msg:"recorded: <file:line> opens the entry"
    (String.starts_with ~prefix:"    test/test_users.ml:31\n    expected  "
       recorded);
  not_contains ~msg:"recorded: nothing about a declaration" ~sub:"declared"
    recorded;
  equal ~msg:"runner: <file:line>, then the fact" string
    "    test/test_users.ml:88\n    timed out after 0.2s\n"
    (failure_block (located (Failure.timeout 0.2)));
  (* A property's timeout before any failure names the case it cut and the
     passes before it, and replays: the seed reaches that case again. *)
  let in_case ?count ~examples case_index passed =
    located
      (Failure.timeout
         ~case:
           { Failure.case_index; examples; passed; root = Fixtures.root; count }
         0.5)
  in
  equal ~msg:"a property's timeout: the case, the passes, the replay" string
    "    test/test_users.ml:88\n\
    \    timed out after 0.5s in case 7 (7 passed)\n\
    \    replay: ./t.exe --seed s1:7be1d2c904aa31f5 --prop-count 500 -f 'p'\n"
    (failure_block ~invocation:(`Exe "./t.exe") ~filter:"p"
       (in_case ~count:500 ~examples:false 7 7));
  equal ~msg:"the headline is the fact" string
    "timed out after 0.5s in case 7 (7 passed)"
    (Report.headline (in_case ~examples:false 7 7));
  equal ~msg:"an example has no replay" string
    "    test/test_users.ml:88\n\
    \    timed out after 0.5s in example 2 (1 passed)\n"
    (failure_block ~invocation:(`Exe "./t.exe") ~filter:"p"
       (in_case ~examples:true 1 1));
  is_true ~msg:"no location: the entry opens on its facts"
    (String.starts_with ~prefix:"    expected  1\n"
       (failure_block (Failure.equality ~expected:"1" ~actual:"2" ())));
  (* A phase is its bracketed tag before the location. *)
  let teardown = Failure.with_phase Failure.Teardown in
  contains ~msg:"phase: the tag before a runner-made failure's location"
    ~sub:
      "    [teardown] test/test_users.ml:88\n\
      \    uncaught exception:\n\
      \      Not_found\n"
    (failure_block (teardown (located uncaught)));
  contains ~msg:"phase: the tag before a tail-position failure's location"
    ~sub:"    [setup] test/test_users.ml:88\n    expected  1\n"
    (failure_block (Failure.with_phase Failure.Setup tail));
  contains ~msg:"phase: the tag before a recorded line"
    ~sub:"    [teardown] test/test_users.ml:88\n    could not restore\n"
    (failure_block
       (teardown (Failure.message ~loc:declared "could not restore")));
  contains ~msg:"phase: a fixture release names the fixture's site"
    ~sub:"    [release] test/test_users.ml:88\n"
    (failure_block
       (Failure.with_phase Failure.Release
          (Failure.message ~loc:declared "db: release raised Exit")));
  contains ~msg:"phase: the tag is yellow, the location dim"
    ~sub:"\027[33m[teardown]\027[0m \027[2mtest/test_users.ml:88\027[0m\n"
    (failure_block ~ansi:true
       (teardown (Failure.message ~loc:declared "could not restore")));
  List.iter
    (fun anchored ->
      is_true ~msg:"the tag opens the location line, never the bare word"
        (String.starts_with ~prefix:"    [teardown] " anchored);
      not_contains ~msg:"the comma form is gone" ~sub:"teardown," anchored)
    [
      failure_block (teardown (located uncaught));
      failure_block (teardown (Failure.message ~loc:declared "x"));
    ]

let test_excerpt_project_root () =
  (* The recorded location is project-root-relative, exactly as __POS__
     records it. Under [dune runtest] the process cwd is inside _build,
     where this path never opens. Resolution against the project root
     must find it; run directly from the repo root, the relative open
     works too, and the block renders identically. *)
  let f =
    Failure.equality
      ~loc:{ Loc.file = "test/unit/test_report.ml"; line = 1; column = 0 }
      ~expected:"1" ~actual:"2" ()
  in
  contains ~msg:"relative recorded paths resolve under dune runtest"
    ~sub:"1 \u{2502} (*---"
    (failure_block ~excerpt:true f)

(* Whole reports, colour and plain

   A report's expected text is written once, with its colour roles marked
   (see [roles] above), and stripping the escapes gives the plain bytes, so
   one literal pins both and a failure is a readable diff. *)

(* Checks [render] against [marked] twice: the escapes under colour, and
   the same text without one under none. *)
let check_report name ~marked render =
  equal ~msg:(name ^ ": with colour") string (roles marked) (render ~ansi:true);
  equal ~msg:(name ^ ": without") string
    (strip_ansi (roles marked))
    (render ~ansi:false)

let lines_of ranges =
  List.concat_map (fun (s, e) -> List.init (e - s + 1) (fun i -> s + i)) ranges

(* The coverage report

   Driven through the real [coverage_report] over section data built by
   hand: the sections name no runtime, so this is the whole of their
   input. The builder that derives it from the runtime's file reports is
   the reporting command's, driven over the real binary in
   test/instr/coverage_cmd. *)

let coverage_file ?source file visited total uncovered =
  {
    Sections.file;
    visited;
    total;
    uncovered = lines_of uncovered;
    source;
    stale = false;
  }

let table_files =
  [
    coverage_file "lib/env.ml" 50 52 [ (88, 89) ];
    coverage_file "lib/eval.ml" 69 119
      [
        (41, 47);
        (60, 60);
        (93, 104);
        (131, 131);
        (140, 152);
        (160, 170);
        (180, 180);
        (190, 195);
        (200, 200);
        (210, 210);
        (220, 230);
      ];
    coverage_file "lib/lexer.ml" 38 38 [];
    coverage_file "lib/parser.ml" 91 142
      [
        (17, 17);
        (52, 58);
        (77, 77);
        (102, 119);
        (140, 140);
        (151, 160);
        (170, 170);
        (180, 180);
        (190, 190);
        (200, 200);
      ];
    coverage_file "lib/printer.ml" 64 86 [ (23, 31); (70, 74); (90, 90) ];
  ]

let table_data = { Sections.visited = 312; total = 437; files = table_files }

let coverage ?(mode = `Report) ?min data ~ansi =
  sections ~ansi (Sections.coverage_report ~mode ~min data)

let table_rows =
  "\u{ab}d|   cover    points   file             uncovered lines (-u shows the \
   source)\u{bb}\n\
  \   96.2%    50/52    lib/env.ml       88-89\n\
  \  \u{ab}r| 58.0%\u{bb}    69/119   lib/eval.ml      41-47, 60, 93-104, 131, \
   140-152, 160-170, 180, 190-195 (+3 more)\n\
  \  100.0%    38/38    lib/lexer.ml\n\
  \  \u{ab}r| 64.1%\u{bb}    91/142   lib/parser.ml    17, 52-58, 77, 102-119, \
   140, 151-160, 170, 180 (+2 more)\n"

let test_coverage_table () =
  check_report "the table under a gate it misses"
    ~marked:
      (table_rows
     ^ "  \u{ab}r| 74.4%\u{bb}    64/86    lib/printer.ml   23-31, 70-74, 90\n\
        coverage: \u{ab}r|71.4%\u{bb} (312/437 points), minimum 80%: \
        \u{ab}r|FAILED\u{bb}\n")
    (coverage ~min:80. table_data);
  check_report "the table under a gate it meets"
    ~marked:
      (table_rows
     ^ "   74.4%    64/86    lib/printer.ml   23-31, 70-74, 90\n\
        coverage: 71.4% (312/437 points), minimum 70%: \u{ab}g|ok\u{bb}\n")
    (coverage ~min:70. table_data)

(* The source view. Two regions in one file, so the block carries a
   [·····] separator; the first file is above its gate and the second
   below, so both heading forms print. *)

let source_of texts =
  let last = List.fold_left (fun n (line, _) -> max n line) 0 texts in
  String.concat "\n"
    (List.init last (fun i ->
         Option.value ~default:"" (List.assoc_opt (i + 1) texts)))
  ^ "\n"

let env_source =
  source_of
    [
      (87, "  | Some frame ->");
      (88, "      if frame.sealed then invalid_arg \"Env.set: sealed frame\"");
      (89, "      else Hashtbl.replace frame.vars name v");
      (90, "  | None -> raise Not_found");
    ]

let eval_source =
  source_of
    [
      (40, "  | Let (x, e, body) ->");
      (41, "      let v = eval env e in");
      (42, "      eval (Env.bind env x v) body");
      (43, "  | If (c, t, e) ->");
      (59, "  | Div (a, b) ->");
      (60, "      if eval env b = Int 0 then raise Division_by_zero");
      (61, "      else div (eval env a) (eval env b)");
    ]

let source_data =
  {
    Sections.visited = 119;
    total = 171;
    files =
      [
        coverage_file ~source:env_source "lib/env.ml" 50 52 [ (88, 89) ];
        coverage_file ~source:eval_source "lib/eval.ml" 69 119
          [ (41, 42); (60, 60) ];
      ];
  }

let source_view =
  "\u{ab}d|   cover    points   file          uncovered lines\u{bb}\n\
  \   96.2%    50/52    lib/env.ml    88-89\n\
  \  \u{ab}r| 58.0%\u{bb}    69/119   lib/eval.ml   41-42, 60\n\n\
   \u{ab}b|lib/env.ml\u{bb}: 96.2% (50/52)\n\n\
  \     87 \u{2502}   | Some frame ->\n\
   \u{ab}r|  \u{258c}\u{bb}  88 \u{2502}       if frame.sealed then \
   invalid_arg \"Env.set: sealed frame\"\n\
   \u{ab}r|  \u{258c}\u{bb}  89 \u{2502}       else Hashtbl.replace frame.vars \
   name v\n\
  \     90 \u{2502}   | None -> raise Not_found\n\n\
   \u{ab}b|lib/eval.ml\u{bb}: \u{ab}r|58.0%\u{bb} (69/119)\n\n\
  \     40 \u{2502}   | Let (x, e, body) ->\n\
   \u{ab}r|  \u{258c}\u{bb}  41 \u{2502}       let v = eval env e in\n\
   \u{ab}r|  \u{258c}\u{bb}  42 \u{2502}       eval (Env.bind env x v) body\n\
  \     43 \u{2502}   | If (c, t, e) ->\n\
   \u{ab}d|   \u{00b7}\u{00b7}\u{00b7}\u{00b7}\u{00b7}\u{bb}\n\
  \     59 \u{2502}   | Div (a, b) ->\n\
   \u{ab}r|  \u{258c}\u{bb}  60 \u{2502}       if eval env b = Int 0 then \
   raise Division_by_zero\n\
  \     61 \u{2502}       else div (eval env a) (eval env b)\n\n\
   coverage: \u{ab}r|69.6%\u{bb} (119/171 points), minimum 80%: \
   \u{ab}r|FAILED\u{bb}\n"

let test_coverage_source () =
  check_report "the source view" ~marked:source_view
    (coverage ~mode:`Full ~min:80. source_data);
  let colored = coverage ~mode:`Full ~min:80. source_data ~ansi:true in
  contains ~msg:"the marker's escape opens at column zero"
    ~sub:"\n\027[31m  \u{258c}\027[0m  88 \u{2502}" colored;
  not_contains ~msg:"the marker is not styled past the margin"
    ~sub:"  \027[31m\u{258c}" colored;
  (* A region at the top of a file has no line above it to show. *)
  let top =
    {
      Sections.visited = 1;
      total = 2;
      files =
        [
          coverage_file ~source:"let a = 1\nlet b = 2\nlet c = 3\n" "top.ml" 1 2
            [ (1, 1) ];
        ];
    }
  in
  contains ~msg:"a region is clipped against the top of the file"
    ~sub:"\n\n  \u{258c}   1 \u{2502} let a = 1\n      2 \u{2502} let b = 2\n\n"
    (coverage ~mode:`Full top ~ansi:false)

(* The outcome is the last line in every mode, the gate on it. The table
   under a gate, met and missed, is [test_coverage_table]'s. *)

let last_line out =
  match List.rev (String.split_on_char '\n' out) with
  | "" :: last :: _ -> last
  | _ -> "\u{ab}the output does not end on a newline\u{bb}"

let test_coverage_outcome_last () =
  let last ?mode ?min data = last_line (coverage ?mode ?min data ~ansi:false) in
  equal ~msg:"the table, no gate" string "coverage: 71.4% (312/437 points)"
    (last table_data);
  equal ~msg:"the source view, no gate" string
    "coverage: 69.6% (119/171 points)"
    (last ~mode:`Full source_data);
  equal ~msg:"the source view, a gate missed" string
    "coverage: 69.6% (119/171 points), minimum 80%: FAILED"
    (last ~mode:`Full ~min:80. source_data);
  equal ~msg:"the source view, a gate met" string
    "coverage: 69.6% (119/171 points), minimum 69.5%: ok"
    (last ~mode:`Full ~min:69.5 source_data);
  let plain = coverage ~min:80. table_data ~ansi:false in
  is_true ~msg:"the outcome prints once"
    (occurrences_of ~sub:"coverage: " plain = 1);
  is_true ~msg:"and the gate with it" (occurrences_of ~sub:"minimum" plain = 1)

let test_coverage_header () =
  let first out = List.hd (String.split_on_char '\n' out) in
  not_contains ~msg:"no row repeats the column's name" ~sub:"uncovered:"
    (coverage table_data ~ansi:false);
  let long =
    {
      Sections.visited = 1;
      total = 2;
      files =
        [ coverage_file "examples/07-coverage/a_long_module.ml" 1 2 [ (9, 9) ] ];
    }
  in
  equal
    ~msg:
      "long paths do not cost the header its hint: it is what says how to see \
       a cut row's other ranges"
    string
    "   cover    points   file                                    uncovered \
     lines (-u shows the source)"
    (first (coverage long ~ansi:false));
  (* Narrow cells do not pull a label off its column. *)
  let narrow =
    {
      Sections.visited = 4;
      total = 8;
      files = [ coverage_file "a.ml" 4 8 [ (3, 3) ] ];
    }
  in
  equal ~msg:"a label is a floor on its column's width" string
    "   cover    points   file   uncovered lines (-u shows the source)\n\
    \   50.0%    4/8      a.ml   3\n\
     coverage: 50.0% (4/8 points)\n"
    (coverage narrow ~ansi:false);
  (* The two notes, at the width a real path gives them: a stale row is
     a sentence, and passes 80 columns. *)
  let notes =
    {
      Sections.visited = 2;
      total = 4;
      files =
        [
          coverage_file "examples/07-coverage/half_a.ml" 1 2 [];
          {
            (coverage_file "examples/07-coverage/half_b.ml" 1 2 []) with
            Sections.stale = true;
          };
        ];
    }
  in
  equal
    ~msg:
      "unvisited points without a line say why; a stale file says so, and what \
       to do"
    string
    "   cover    points   file                             uncovered lines (-u \
     shows the source)\n\
    \   50.0%    1/2      examples/07-coverage/half_a.ml   (source not found)\n\
    \   50.0%    1/2      examples/07-coverage/half_b.ml   stale: the source \
     changed; re-run the instrumented tests\n\
     coverage: 50.0% (2/4 points)\n"
    (coverage notes ~ansi:false)

(* A barely tested file has hundreds of uncovered ranges. A row prints its
   first eight and counts the rest, whatever the width of its path. *)

let test_coverage_cap () =
  let row (f : Sections.coverage_file) =
    let out =
      coverage
        { Sections.visited = f.visited; total = f.total; files = [ f ] }
        ~ansi:false
    in
    List.nth (String.split_on_char '\n' out) 1
  in
  let ranges n name =
    coverage_file name 0 60 (List.init n (fun i -> ((i * 4) + 1, (i * 4) + 2)))
  in
  let eight = "1-2, 5-6, 9-10, 13-14, 17-18, 21-22, 25-26, 29-30" in
  let long_path =
    "a/very/long/path/to/a/barely/tested/module/in/a/deep/tree/wide.ml"
  in
  equal ~msg:"eight ranges, then the count of the rest" string
    ("    0.0%    0/60     lib/wide.ml   " ^ eight ^ " (+22 more)")
    (row (ranges 30 "lib/wide.ml"));
  equal ~msg:"eight ranges print whole, with no count" string
    ("    0.0%    0/60     lib/wide.ml   " ^ eight)
    (row (ranges 8 "lib/wide.ml"));
  equal ~msg:"a long path does not cut the ranges" string
    ("    0.0%    0/60     " ^ long_path ^ "   " ^ eight ^ " (+1 more)")
    (row (ranges 9 long_path))

(* A percentage is red below the gate, or below 80 when there is none,
   and plain otherwise: on a row, on a heading and on the outcome line. *)

let test_coverage_colour () =
  let outcome ?min ~visited ~total () =
    coverage ?min { Sections.visited; total; files = [] } ~ansi:true
  in
  equal ~msg:"no gate: 80 percent is plain" string
    "coverage: 80.0% (80/100 points)\n"
    (outcome ~visited:80 ~total:100 ());
  equal ~msg:"no gate: below 80 is red" string
    "coverage: \027[31m79.9%\027[0m (799/1000 points)\n"
    (outcome ~visited:799 ~total:1000 ());
  equal ~msg:"a gate met at its boundary: plain, and ok green" string
    "coverage: 70.0% (70/100 points), minimum 70%: \027[32mok\027[0m\n"
    (outcome ~min:70. ~visited:70 ~total:100 ());
  equal ~msg:"a gate missed: red, and FAILED red" string
    "coverage: \027[31m69.9%\027[0m (699/1000 points), minimum 70%: \
     \027[31mFAILED\027[0m\n"
    (outcome ~min:70. ~visited:699 ~total:1000 ());
  equal ~msg:"a gate above 80 reddens what the default would not" string
    "coverage: \027[31m85.0%\027[0m (85/100 points), minimum 90%: \
     \027[31mFAILED\027[0m\n"
    (outcome ~min:90. ~visited:85 ~total:100 ());
  equal ~msg:"the gate compares the measurement, not its rounding" string
    "coverage: \027[31m66.7%\027[0m (2/3 points), minimum 66.7%: \
     \027[31mFAILED\027[0m\n"
    (outcome ~min:66.7 ~visited:2 ~total:3 ());
  equal ~msg:"no file to tabulate: the outcome alone, 100% and plain" string
    "coverage: 100.0% (0/0 points)\n"
    (outcome ~visited:0 ~total:0 ());
  (* The minimum prints as it was given: a rounding would show a gate the
     line then contradicts. *)
  equal ~msg:"the minimum prints as given, never rounded" string
    "coverage: 100.0% (1/1 points), minimum 99.99999%: \027[32mok\027[0m\n"
    (outcome ~min:99.99999 ~visited:1 ~total:1 ());
  equal ~msg:"and a whole minimum without a point" string
    "coverage: 80.0% (80/100 points), minimum 80%: \027[32mok\027[0m\n"
    (outcome ~min:80.0 ~visited:80 ~total:100 ());
  let rows =
    coverage ~min:50.
      {
        Sections.visited = 9;
        total = 20;
        files =
          [ coverage_file "at.ml" 5 10 []; coverage_file "under.ml" 4 10 [] ];
      }
      ~ansi:true
  in
  contains ~msg:"a row at the gate is plain" ~sub:"\n   50.0%    5/10 " rows;
  contains ~msg:"a row under it is red, its pad inside the colour"
    ~sub:"\n  \027[31m 40.0%\027[0m    4/10 " rows;
  let ungated =
    coverage
      {
        Sections.visited = 24;
        total = 30;
        files =
          [
            coverage_file "full.ml" 10 10 [];
            coverage_file "most.ml" 8 10 [];
            coverage_file "some.ml" 6 10 [];
          ];
      }
      ~ansi:true
  in
  contains ~msg:"below 80 without a gate is red on a row too"
    ~sub:"\n  \027[31m 60.0%\027[0m     6/10 " ungated;
  not_contains ~msg:"no percentage is green" ~sub:"\027[32m" ungated;
  not_contains ~msg:"nor yellow" ~sub:"\027[33m" ungated

(* A source file's bytes are not the report's: a control byte in one
   prints as a failure block's source line prints it, under both colour
   settings. *)

let test_coverage_escapes () =
  let data =
    {
      Sections.visited = 1;
      total = 2;
      files =
        [
          coverage_file
            ~source:"let plain = 1\nlet red = \"\027[31mred\r\"\nlet c = 3\n"
            "esc.ml" 1 2
            [ (2, 2) ];
        ];
    }
  in
  List.iter
    (fun ansi ->
      let out = coverage ~mode:`Full data ~ansi in
      contains ~msg:"the bytes print as text"
        ~sub:"   2 \u{2502} let red = \"\\x1b[31mred\\x0d\"\n" out;
      not_contains ~msg:"and never raw" ~sub:"\027[31mred" out;
      not_contains ~msg:"a carriage return neither" ~sub:"\r" out)
    [ true; false ]

(* The mutation report

   Two producers, one layout. The loop commits a survivor's block when
   its child ends and closes its report when the last one does; [windtrap
   mutants] prints the same sections at rest over a merge. The loop is
   driven here as [Mutate_loop] drives it, over a renderer whose sink is
   read between two calls, so what is committed when is pinned with the
   bytes; the loop itself runs in test/instr/loop. *)

let calc_source =
  source_of
    [
      (13, "  | Sub -> a - b");
      (21, "let sign n = if n > 0 then 1 else 0");
      (40, "let clamp lo hi n = if n < lo then lo else if n > hi then hi else n");
      (41, "let pred n = n - 1");
    ]

let witness ?exe ?file test line =
  {
    Sections.test;
    loc = Option.map (fun file -> { Loc.file; line; column = 0 }) file;
    exe;
  }

let mutant ?(source = calc_source) id line before after =
  { Sections.id; line; before; after; source = Some source }

let add_survivor =
  {
    Sections.mutant = mutant "lib/calc.ml:13:11:add" 13 "a - b" "a + b";
    witnesses =
      [
        witness ~file:"test/test_calc.ml" "subtraction \u{203a} stays positive"
          19;
      ];
  }

let ge_survivor =
  {
    Sections.mutant = mutant "lib/calc.ml:21:16:ge" 21 "n > 0" "n >= 0";
    witnesses =
      [
        witness ~file:"test/test_calc.ml" "sign of a negative" 31;
        witness ~file:"test/test_calc.ml" "sign of a positive" 30;
      ];
  }

let loop_report =
  {
    Sections.survivors = [ add_survivor; ge_survivor ];
    unreached = [ ("lib/calc.ml", 40); ("lib/calc.ml", 41) ];
    killed = 3;
    not_tested = 0;
    scope = Sections.Suite;
  }

let exe_invocation =
  `Exe "dune exec --instrument-with ppx_windtrap.mutate test/test_calc.exe --"

(* A loop's report, from the line after its dry run's summary. *)
let loop ?(invocation = exe_invocation) ?terminal (m : Sections.mutation) ~ansi
    =
  with_renderer ~ansi ?terminal ~invocation (fun r ->
      let total = List.length m.Sections.survivors + m.Sections.killed in
      List.iteri
        (fun i (s : Sections.survivor) ->
          Report.mutation_testing r ~index:(i + 1) ~total
            ~id:s.Sections.mutant.Sections.id;
          Report.mutation_survivor r s)
        m.Sections.survivors;
      Report.mutation_finish r m)

let at_rest ?(invocation = `Mirrors) m ~ansi =
  sections ~ansi (Sections.mutation_report ~invocation m)

let add_block =
  "  \u{ab}r|SURVIVED\u{bb}  \u{ab}b|lib/calc.ml:13:11:add\u{bb}  a - b \
   \u{2192} a + b\n\
  \      \u{ab}d|13 \u{2502}\u{bb} | Sub -> a - b\n\n\
  \    1 test ran this line and did not fail:\n\
  \      subtraction \u{203a} stays positive  \u{ab}d|test/test_calc.ml:19\u{bb}\n"

let ge_block =
  "  \u{ab}r|SURVIVED\u{bb}  \u{ab}b|lib/calc.ml:21:16:ge\u{bb}  n > 0 \
   \u{2192} n >= 0\n\
  \      \u{ab}d|21 \u{2502}\u{bb} let sign n = if n > 0 then 1 else 0\n\n\
  \    2 tests ran this line and none failed:\n\
  \      sign of a negative  \u{ab}d|test/test_calc.ml:31\u{bb}\n\
  \      sign of a positive  \u{ab}d|test/test_calc.ml:30\u{bb}\n"

let survivors_rule =
  "\u{ab}d|─────────────────────── survivors ────────────────────────\u{bb}\n"

let closing = "\u{ab}d|" ^ closing_rule ^ "\u{bb}\n"

let loop_text =
  "\n" ^ survivors_rule ^ add_block ^ "\n" ^ ge_block ^ closing
  ^ "\n\
     \u{ab}d|─────────────────── never reached (2) ────────────────────\u{bb}\n\
    \  \u{ab}y|2\u{bb}  lib/calc.ml   lines 40-41\n" ^ closing
  ^ "\n\
     reproduce: dune exec --instrument-with ppx_windtrap.mutate \
     test/test_calc.exe -- --arm lib/calc.ml:13:11:add\n\
     mutants: \u{ab}r|2 survived\u{bb} of 5 reached by this suite, \u{ab}g|3 \
     killed\u{bb}, \u{ab}y|2 never reached\u{bb}\n"

let test_mutation_loop () =
  check_report "the loop's report" ~marked:loop_text (loop loop_report);
  equal ~msg:"every mutant killed and none unreached: the outcome alone" string
    "mutants: 3 reached by this suite, \027[32m3 killed\027[0m\n"
    (loop
       { loop_report with Sections.survivors = []; unreached = [] }
       ~ansi:true);
  not_contains ~msg:"no survivor: no rule" ~sub:"\u{2500}"
    (loop
       { loop_report with Sections.survivors = []; unreached = [] }
       ~ansi:false);
  (* The block is the finding and the command the remedy. *)
  let out = loop loop_report ~ansi:false in
  not_contains ~msg:"no arm line in a block" ~sub:"    arm " out;
  not_contains ~msg:"no [@mutate off] to paste" ~sub:"[@mutate off" out;
  not_contains ~msg:"the loop's rule carries no count" ~sub:"survivors (" out

(* What a loop commits as it runs. Read off the sink without flushing it
   here, so the order of the bytes and the flush are both pinned. *)

let test_mutation_streams () =
  let buf = Buffer.create 256 in
  let r =
    Report.create
      ~out:(Format.formatter_of_buffer buf)
      ~ansi:false
      { (config ()) with Run.invocation = exe_invocation }
  in
  let committed () = Buffer.contents buf in
  let plain marked = strip_ansi (roles marked) in
  Report.mutation_testing r ~index:1 ~total:5 ~id:"lib/calc.ml:9:3:sub";
  equal ~msg:"trying a mutant commits nothing" string "" (committed ());
  Report.mutation_testing r ~index:2 ~total:5 ~id:"lib/calc.ml:13:11:add";
  Report.mutation_survivor r add_survivor;
  let first = plain ("\n" ^ survivors_rule ^ add_block) in
  equal
    ~msg:
      "a survivor commits the blank line, the opening rule and its block when \
       its child ends, flushed"
    string first (committed ());
  Report.mutation_testing r ~index:3 ~total:5 ~id:"lib/calc.ml:21:16:ge";
  Report.mutation_survivor r ge_survivor;
  let second = first ^ plain ("\n" ^ ge_block) in
  equal ~msg:"the next block follows one blank line" string second
    (committed ());
  is_true ~msg:"the opening rule prints once, before the first block"
    (occurrences_of ~sub:" survivors " (committed ()) = 1);
  not_contains ~msg:"the closing rule is the end of the loop's, not a block's"
    ~sub:closing_rule (committed ());
  not_contains ~msg:"and so is the outcome" ~sub:"mutants:" (committed ());
  Report.mutation_finish r loop_report;
  equal ~msg:"the last child's end commits the rest" string (plain loop_text)
    (committed ())

let test_mutation_live () =
  let tail =
    loop ~terminal:true
      { loop_report with Sections.survivors = [ add_survivor ]; unreached = [] }
      ~ansi:true
  in
  is_true ~msg:"the live line is erased before the first committed byte"
    (String.starts_with
       ~prefix:
         ("\r\027[2K\027[2m  [1/4] \
           lib/calc.ml:13:11:add\u{2026}\027[0m\r\027[2K"
         ^ roles ("\n" ^ survivors_rule))
       tail);
  is_true ~msg:"and none is drawn or erased after it"
    (occurrences_of ~sub:"\r\027[2K" tail = 2);
  let killed =
    with_renderer ~ansi:true ~terminal:true (fun r ->
        Report.mutation_testing r ~index:1 ~total:2 ~id:"lib/calc.ml:9:3:sub";
        Report.mutation_testing r ~index:2 ~total:2 ~id:"lib/calc.ml:13:11:add")
  in
  equal ~msg:"a killed mutant leaves nothing: the next line draws over it"
    string
    "\r\027[2K\027[2m  [1/2] \
     lib/calc.ml:9:3:sub\u{2026}\027[0m\r\027[2K\r\027[2K\027[2m  [2/2] \
     lib/calc.ml:13:11:add\u{2026}\027[0m"
    killed;
  equal ~msg:"off without a terminal" string ""
    (with_renderer ~ansi:true (fun r ->
         Report.mutation_testing r ~index:1 ~total:2 ~id:"lib/calc.ml:9:3:sub"));
  equal ~msg:"off without colour" string ""
    (with_renderer ~ansi:false ~terminal:true (fun r ->
         Report.mutation_testing r ~index:1 ~total:2 ~id:"lib/calc.ml:9:3:sub"))

(* An interrupted loop closes as a complete one does, over the children
   that ended: the reached mutants left without a verdict are counted
   last. Its [windtrap:] line is standard error's, pinned with a real
   signal in test/instr/loop. *)

let test_mutation_interrupted () =
  let stopped =
    {
      loop_report with
      Sections.survivors = [ add_survivor ];
      killed = 0;
      not_tested = 2;
    }
  in
  equal ~msg:"the closing sections, and what was not tested" string
    (strip_ansi (roles closing)
    ^ "\n\
       ─────────────────── never reached (2) ────────────────────\n\
      \  2  lib/calc.ml   lines 40-41\n" ^ closing_rule
    ^ "\n\n\
       reproduce: dune exec --instrument-with ppx_windtrap.mutate \
       test/test_calc.exe -- --arm lib/calc.ml:13:11:add\n\
       mutants: 1 survived of 3 reached by this suite, 2 never reached, 2 not \
       tested\n")
    (with_renderer ~invocation:exe_invocation (fun r ->
         Report.mutation_finish r stopped));
  equal ~msg:"stopped before a child ended" string
    "mutants: 5 reached by this suite, 5 not tested\n"
    (with_renderer (fun r ->
         Report.mutation_finish r
           {
             loop_report with
             Sections.survivors = [];
             unreached = [];
             killed = 0;
             not_tested = 5;
           }))

(* [windtrap mutants]: the same sections at rest, the survivors counted,
   the executables one column for the report. *)

let eq_survivor =
  {
    Sections.mutant =
      mutant
        ~source:
          (source_of
             [ (60, "      if eval env b = Int 0 then raise Division_by_zero") ])
        "lib/eval.ml:60:24:eq" 60 "eval env b = Int 0" "eval env b <> Int 0";
    witnesses =
      [
        witness ~exe:"test_eval.exe" "division \u{203a} divides" 0;
        witness ~exe:"test_eval.exe" "division \u{203a} rounds toward zero" 0;
        witness ~exe:"test_printer.exe" "round trip \u{203a} arithmetic" 0;
      ];
  }

let not_survivor =
  {
    Sections.mutant =
      mutant
        ~source:
          (source_of
             [ (102, "    if at_end p then Error (Unexpected_eof p.pos)") ])
        "lib/parser.ml:102:9:not" 102 "at_end p" "not (at_end p)";
    witnesses =
      [
        witness ~exe:"test_parser.exe" "errors \u{203a} unexpected end of input"
          0;
      ];
  }

let merge_report =
  {
    Sections.survivors = [ eq_survivor; not_survivor ];
    unreached =
      List.map (fun line -> ("lib/text.ml", line)) [ 12; 13; 14; 32 ]
      @ [ ("lib/run.ml", 40); ("lib/run.ml", 40); ("lib/report.ml", 61) ];
    killed = 16;
    not_tested = 0;
    scope = Sections.Executables 3;
  }

let merge_invocation =
  `Exe "dune exec --instrument-with ppx_windtrap.mutate test/test_eval.exe --"

let merge_text =
  "\u{ab}d|───────────────────── survivors (2) ──────────────────────\u{bb}\n\
  \  \u{ab}r|SURVIVED\u{bb}  \u{ab}b|lib/eval.ml:60:24:eq\u{bb}  eval env b = \
   Int 0 \u{2192} eval env b <> Int 0\n\
  \      \u{ab}d|60 \u{2502}\u{bb} if eval env b = Int 0 then raise \
   Division_by_zero\n\n\
  \    3 tests in 2 executables ran this line and none failed:\n\
  \      test_eval.exe     division \u{203a} divides\n\
  \      test_eval.exe     division \u{203a} rounds toward zero\n\
  \      test_printer.exe  round trip \u{203a} arithmetic\n\n\
  \  \u{ab}r|SURVIVED\u{bb}  \u{ab}b|lib/parser.ml:102:9:not\u{bb}  at_end p \
   \u{2192} not (at_end p)\n\
  \      \u{ab}d|102 \u{2502}\u{bb} if at_end p then Error (Unexpected_eof \
   p.pos)\n\n\
  \    1 test ran this line and did not fail:\n\
  \      test_parser.exe   errors \u{203a} unexpected end of input\n" ^ closing
  ^ "\n\
     \u{ab}d|─────────────────── never reached (7) ────────────────────\u{bb}\n\
    \  \u{ab}y|1\u{bb}  lib/report.ml   lines 61\n\
    \  \u{ab}y|2\u{bb}  lib/run.ml      lines 40\n\
    \  \u{ab}y|4\u{bb}  lib/text.ml     lines 12-14, 32\n" ^ closing
  ^ "\n\
     reproduce: dune exec --instrument-with ppx_windtrap.mutate \
     test/test_eval.exe -- --arm lib/eval.ml:60:24:eq\n\
     mutants: \u{ab}r|2 survived\u{bb} of 18 reached, \u{ab}g|16 killed\u{bb}, \
     \u{ab}y|7 never reached\u{bb}, 3 executables\n"

let test_mutation_at_rest () =
  check_report "the merge's report" ~marked:merge_text
    (at_rest ~invocation:merge_invocation merge_report);
  let out = at_rest ~invocation:merge_invocation merge_report ~ansi:false in
  not_contains ~msg:"no blank line just inside a rule: after one"
    ~sub:"\u{2500}\n\n  SURVIVED" out;
  not_contains ~msg:"nor before one" ~sub:("\n\n" ^ closing_rule) out;
  is_true ~msg:"nothing precedes the report"
    (String.starts_with ~prefix:"\u{2500}" out);
  (* Each finding stands alone. *)
  equal ~msg:"never reached alone: its rule, its rows, the outcome" string
    ("─────────────────── never reached (7) ────────────────────\n\
     \  1  lib/report.ml   lines 61\n\
     \  2  lib/run.ml      lines 40\n\
     \  4  lib/text.ml     lines 12-14, 32\n" ^ closing_rule
   ^ "\n\nmutants: 16 reached, 16 killed, 7 never reached, 3 executables\n")
    (at_rest { merge_report with Sections.survivors = [] } ~ansi:false);
  not_contains ~msg:"survivors alone: no never-reached section"
    ~sub:"never reached"
    (at_rest { merge_report with Sections.unreached = [] } ~ansi:false);
  equal ~msg:"neither: the outcome alone" string
    "mutants: 16 reached, 16 killed, 3 executables\n"
    (at_rest
       { merge_report with Sections.survivors = []; unreached = [] }
       ~ansi:false);
  (* An unreadable source drops the source line and nothing else. *)
  let sourceless =
    {
      merge_report with
      Sections.survivors =
        List.map
          (fun (s : Sections.survivor) ->
            {
              s with
              Sections.mutant =
                { s.Sections.mutant with Sections.source = None };
            })
          merge_report.Sections.survivors;
    }
  in
  not_contains ~msg:"an unreadable source drops the source line" ~sub:"\u{2502}"
    (at_rest sourceless ~ansi:false);
  contains ~msg:"and keeps the title, the sentence one blank line under it"
    ~sub:
      "  SURVIVED  lib/parser.ml:102:9:not  at_end p \u{2192} not (at_end p)\n\n\
      \    1 test ran this line and did not fail:\n"
    (at_rest sourceless ~ansi:false);
  not_contains ~msg:"a line past the end of the source prints no source line"
    ~sub:"\u{2502}"
    (at_rest
       {
         merge_report with
         Sections.survivors =
           [
             {
               not_survivor with
               Sections.mutant =
                 {
                   not_survivor.Sections.mutant with
                   Sections.source = Some "one line\n";
                 };
             };
           ];
       }
       ~ansi:false)

let test_mutation_sentence () =
  let block witnesses =
    at_rest
      {
        loop_report with
        Sections.survivors = [ { add_survivor with Sections.witnesses } ];
      }
      ~ansi:false
  in
  let one = witness ~file:"test/test_calc.ml" "calc \u{203a} sub to zero" 19 in
  let other = witness ~file:"test/test_eval.ml" "eval \u{203a} Sub node" 31 in
  contains ~msg:"singular" ~sub:"\n    1 test ran this line and did not fail:\n"
    (block [ one ]);
  contains ~msg:"plural" ~sub:"\n    2 tests ran this line and none failed:\n"
    (block [ one; other ]);
  contains ~msg:"names are padded to the widest of the block"
    ~sub:
      "\n\
      \      calc \u{203a} sub to zero  test/test_calc.ml:19\n\
      \      eval \u{203a} Sub node     test/test_eval.ml:31\n"
    (block [ one; other ]);
  (* The executable column appears exactly when a witness names one, and
     it is one column for the report: a row without an executable still
     leaves the column. *)
  let named = { one with Sections.exe = Some "test_calc.exe" } in
  not_contains ~msg:"no executable column without an executable"
    ~sub:"test_calc.exe"
    (block [ one; other ]);
  contains ~msg:"the column appears when one witness names an executable"
    ~sub:
      "\n\
      \      test_calc.exe  calc \u{203a} sub to zero  test/test_calc.ml:19\n\
      \                     eval \u{203a} Sub node     test/test_eval.ml:31\n"
    (block [ named; other ]);
  contains ~msg:"one executable is just tests"
    ~sub:"\n    2 tests ran this line and none failed:\n"
    (block [ named; { other with Sections.exe = Some "test_calc.exe" } ]);
  contains ~msg:"several executables are counted"
    ~sub:"\n    2 tests in 2 executables ran this line and none failed:\n"
    (block [ named; { other with Sections.exe = Some "test_eval.exe" } ])

let test_mutation_summary_forms () =
  let summary ?(survivors = []) ?(unreached = []) ?(not_tested = 0) ~killed
      scope =
    last_line
      (at_rest
         { Sections.survivors; unreached; killed; not_tested; scope }
         ~ansi:false)
  in
  let unreached = [ ("lib/calc.ml", 22); ("lib/calc.ml", 31) ] in
  (* The clean form is the absence of a survived term, not a zero. *)
  equal ~msg:"suite, clean" string "mutants: 5 reached by this suite, 5 killed"
    (summary ~killed:5 Sections.Suite);
  equal ~msg:"selected, clean" string
    "mutants: 2 reached by the 2 selected tests, 2 killed"
    (summary ~killed:2 (Sections.Selected 2));
  equal ~msg:"one selected test" string
    "mutants: 1 reached by the 1 selected test, 1 killed"
    (summary ~killed:1 (Sections.Selected 1));
  equal ~msg:"executables, clean" string
    "mutants: 14 reached, 14 killed, 3 executables"
    (summary ~killed:14 (Sections.Executables 3));
  equal ~msg:"one executable" string
    "mutants: 3 reached, 3 killed, 1 executable"
    (summary ~killed:3 (Sections.Executables 1));
  (* Zero terms are omitted: nothing killed and nothing reached. *)
  equal ~msg:"nothing reached, nothing killed" string
    "mutants: 0 reached by this suite"
    (summary ~killed:0 Sections.Suite);
  equal ~msg:"nothing reached in the aggregate is all never reached" string
    "mutants: 0 reached, 2 never reached, 1 executable"
    (summary ~unreached ~killed:0 (Sections.Executables 1));
  (* With survivors the reached count is the sum of the lists. *)
  equal ~msg:"suite, survivors" string
    "mutants: 1 survived of 5 reached by this suite, 4 killed"
    (summary ~survivors:[ add_survivor ] ~killed:4 Sections.Suite);
  equal ~msg:"selected, survivors" string
    "mutants: 1 survived of 2 reached by the 3 selected tests, 1 killed"
    (summary ~survivors:[ add_survivor ] ~killed:1 (Sections.Selected 3));
  equal ~msg:"executables, survivors and never reached" string
    "mutants: 1 survived of 12 reached, 11 killed, 2 never reached, 3 \
     executables"
    (summary ~survivors:[ add_survivor ] ~unreached ~killed:11
       (Sections.Executables 3));
  equal ~msg:"every kill a survivor: no killed term" string
    "mutants: 1 survived of 1 reached by this suite"
    (summary ~survivors:[ add_survivor ] ~killed:0 Sections.Suite);
  equal ~msg:"what an interrupted loop did not test is reached, and last" string
    "mutants: 1 survived of 6 reached by this suite, 2 killed, 2 never \
     reached, 3 not tested"
    (summary ~survivors:[ add_survivor ] ~unreached ~not_tested:3 ~killed:2
       Sections.Suite);
  not_contains ~msg:"no middle dot joins the terms" ~sub:"\u{00b7}"
    (summary ~survivors:[ add_survivor ] ~unreached ~killed:11
       (Sections.Executables 3))

(* One command, once, above the outcome: the one that arms the first
   survivor printed, under the run's launcher and the run's selection. *)

let test_mutation_reproduce () =
  let line ?invocation m =
    match
      List.filter
        (String.starts_with ~prefix:"reproduce: ")
        (String.split_on_char '\n' (loop ?invocation m ~ansi:true))
    with
    | [ l ] -> l
    | [] -> "\u{ab}no reproduce line\u{bb}"
    | _ -> "\u{ab}several reproduce lines\u{bb}"
  in
  equal ~msg:"under dune: dune exec, the backend before the target" string
    "reproduce: dune exec --instrument-with ppx_windtrap.mutate \
     test/test_calc.exe -- --arm lib/calc.ml:13:11:add"
    (line loop_report);
  equal ~msg:"by hand: the bare executable" string
    "reproduce: ./test_calc.exe --arm lib/calc.ml:13:11:add"
    (line ~invocation:(`Exe "./test_calc.exe") loop_report);
  equal ~msg:"under a build action: the mirror, and a run dune does not replay"
    string
    "reproduce: WINDTRAP_MUTATE_ARM=lib/calc.ml:13:11:add dune runtest --force \
     --instrument-with ppx_windtrap.mutate"
    (line ~invocation:`Mirrors loop_report);
  equal ~msg:"the first survivor printed is the one it arms" string
    "reproduce: ./t.exe --arm lib/calc.ml:21:16:ge"
    (line ~invocation:(`Exe "./t.exe")
       { loop_report with Sections.survivors = [ ge_survivor; add_survivor ] });
  equal ~msg:"an identifier a shell would split is quoted" string
    "reproduce: ./t.exe --arm 'lib/my calc.ml:13:11:add'"
    (line ~invocation:(`Exe "./t.exe")
       {
         loop_report with
         Sections.survivors =
           [
             {
               add_survivor with
               Sections.mutant =
                 {
                   add_survivor.Sections.mutant with
                   Sections.id = "lib/my calc.ml:13:11:add";
                 };
             };
           ];
       });
  let narrowed invocation =
    let config =
      {
        (config ~invocation ()) with
        Run.filter = [ "stays positive" ];
        exclude = [ "slow"; "flaky io" ];
        tags = [ "unit"; "fast" ];
        exclude_tags = [ "flaky" ];
        shard = Some (2, 4);
        failed_only = true;
      }
    in
    List.find
      (String.starts_with ~prefix:"reproduce: ")
      (String.split_on_char '\n'
         (sections (Sections.mutation_closing ~config loop_report)))
  in
  equal ~msg:"a narrowed run: each selection flag, restated" string
    "reproduce: ./t.exe --arm lib/calc.ml:13:11:add -f 'stays positive' -e \
     'slow' -e 'flaky io' --tag unit --tag fast --exclude-tag flaky --shard \
     2/4 --failed"
    (narrowed (`Exe "./t.exe"));
  equal
    ~msg:
      "under a build action: their mirrors, and neither --failed nor two \
       patterns have one"
    string
    "reproduce: WINDTRAP_MUTATE_ARM=lib/calc.ml:13:11:add \
     WINDTRAP_FILTER='stays positive' WINDTRAP_TAG=unit,fast \
     WINDTRAP_EXCLUDE_TAG=flaky WINDTRAP_SHARD=2/4 dune runtest --force \
     --instrument-with ppx_windtrap.mutate"
    (narrowed `Mirrors);
  equal ~msg:"no survivor, nothing to arm: never reached alone has none" string
    "\u{ab}no reproduce line\u{bb}"
    (line { loop_report with Sections.survivors = [] });
  let out = loop loop_report ~ansi:false in
  contains ~msg:"above the outcome, which stays last"
    ~sub:"--arm lib/calc.ml:13:11:add\nmutants: 2 survived" out;
  not_contains ~msg:"no placeholder to fill" ~sub:"<id>" out;
  not_contains ~msg:"and no selection restated" ~sub:" -f " out

(* Never-reached mutants are one row per file, in path order, their
   distinct lines fitted as a coverage row's are. *)

let test_mutation_unreached () =
  let wide =
    List.init 60 (fun i -> ("lib/report.ml", (i * 3) + 10))
    @ List.init 60 (fun i -> ("lib/report.ml", (i * 3) + 10))
    @ [ ("lib/a.ml", 7); ("lib/a.ml", 8); ("lib/a.ml", 8); ("lib/b.ml", 1) ]
  in
  let out =
    at_rest
      {
        Sections.survivors = [];
        unreached = wide;
        killed = 1;
        not_tested = 0;
        scope = Sections.Executables 1;
      }
      ~ansi:true
  in
  equal ~msg:"one row per file; the count right-aligned in yellow" string
    (roles
       "\u{ab}d|────────────────── never reached (124) ───────────────────\u{bb}\n\
       \  \u{ab}y|  3\u{bb}  lib/a.ml        lines 7-8\n\
       \  \u{ab}y|  1\u{bb}  lib/b.ml        lines 1\n\
       \  \u{ab}y|120\u{bb}  lib/report.ml   lines 10, 13, 16, 19, 22, 25, 28, \
        31 (+52 more)\n"
    ^ roles closing
    ^ "\n\
       mutants: 1 reached, \027[32m1 killed\027[0m, \027[33m124 never \
       reached\027[0m, 1 executable\n")
    out;
  not_contains ~msg:"no block per mutant" ~sub:"UNREACHED" out

let test_mutation_escapes () =
  let out =
    at_rest
      {
        loop_report with
        Sections.survivors =
          [
            {
              add_survivor with
              Sections.mutant =
                {
                  add_survivor.Sections.mutant with
                  Sections.source =
                    Some (source_of [ (13, "\t  | Sub -> \027[31ma\r - b") ]);
                };
            };
          ];
      }
      ~ansi:true
  in
  contains ~msg:"a survivor's source line prints its control bytes as text"
    ~sub:"\027[2m13 \u{2502}\027[0m | Sub -> \\x1b[31ma\\x0d - b\n" out;
  not_contains ~msg:"never raw" ~sub:"\027[31ma" out

let test_mutation_armed_verdict () =
  let survived hits =
    with_renderer (fun r -> Report.mutation_survived r ~hits)
  in
  equal ~msg:"evaluated once" string
    "mutant survived: the armed site was evaluated 1 time and no test failed.\n"
    (survived 1);
  equal ~msg:"evaluated twice" string
    "mutant survived: the armed site was evaluated 2 times and no test failed.\n"
    (survived 2)

(* The GitHub Actions envelope: golden ::error annotation, %0A/%25
   data encoding, %3A/%2C property encoding, ANSI stripping, group folding
   commands, and the run-level annotations block. *)

let test_github_golden () =
  let actual =
    Report.annotation
      ~path:[ "users"; "sessions after login" ]
      Fixtures.eq_failure
  in
  expect_file actual "test/unit/expected/test_report/annotation.expected"

let test_github_data_encoding () =
  let f = Failure.message "50% done\r\nnext: a,b" in
  let a = Report.annotation ~path:[ "t" ] f in
  contains ~msg:"percent encoded first, a CR shown as the block shows it"
    ~sub:"50%25 done\\x0d%0A    next" a;
  contains ~msg:"colons and commas untouched in message data" ~sub:"next: a,b" a;
  is_true ~msg:"annotation is one command line"
    (String.length a > 0
    && a.[String.length a - 1] = '\n'
    && occurrences_of ~sub:"\n" a = 1)

let test_github_property_encoding () =
  let f =
    Failure.message
      ~loc:{ Loc.file = "dir,x:y/test.ml"; line = 7; column = 0 }
      "boom"
  in
  let a = Report.annotation ~path:[ "suite: a,b"; "case" ] f in
  contains ~msg:"file property encodes delimiters"
    ~sub:"file=dir%2Cx%3Ay/test.ml,line=7," a;
  contains ~msg:"title encodes delimiters"
    ~sub:"title=Test failure%3A suite%3A a%2Cb › case::" a;
  (* The title is the block's title: a control byte in a test name is
     spelled out, never sent raw. *)
  contains ~msg:"title spells control bytes as the block's title does"
    ~sub:"title=Test failure%3A a\\x01b\\x0ac::"
    (Report.annotation ~path:[ "a\001b\nc" ] f)

let test_github_no_location () =
  let a = Report.annotation ~path:[ "t" ] (Failure.message "boom") in
  contains ~msg:"no location: title only" ~sub:"::error title=" a;
  not_contains ~msg:"no location: no file property" ~sub:"file=" a

let test_github_declaration_location () =
  (* A failure located at the declaration annotates that line, and its
     message opens on the same bare location: no transport says anything
     about [~__POS__]. *)
  let f =
    {
      Fixtures.eq_failure with
      Failure.loc =
        Some { Loc.file = "test/test_users.ml"; line = 88; column = 2 };
    }
  in
  let a = Report.annotation ~path:[ "t" ] f in
  contains ~msg:"declaration: annotates the recorded line"
    ~sub:"file=test/test_users.ml,line=88," a;
  contains ~msg:"declaration: the message opens on the bare location"
    ~sub:"::    test/test_users.ml:88%0A    expected  [(\"alice\"" a;
  is_true ~msg:"declaration: nothing about ~__POS__ in an annotation"
    (occurrences_of ~sub:"__POS__" a = 0);
  not_contains ~msg:"declaration: no anchor word" ~sub:"declared" a

let test_github_replay_info () =
  let a =
    Report.annotation ~path:[ "geo"; "area non-negative" ] Fixtures.prop_failure
  in
  contains ~msg:"property annotation carries the replay line"
    ~sub:
      "replay: WINDTRAP_SEED=s1:7be1d2c904aa31f5 WINDTRAP_FILTER='geo › area \
       non-negative' dune runtest"
    a;
  contains ~msg:"counterexample in the message"
    ~sub:"counterexample (case 12, shrunk 4 steps): Rect (2, 0)" a

let test_github_invocation_hints () =
  (* Annotation messages carry the same hint bytes as the terminal block.
     Both derive from the one startup-computed invocation. *)
  let invocation = `Exe "dune exec qa/x/t.exe --" in
  let a =
    Report.annotation ~invocation
      ~path:[ "geo"; "area non-negative" ]
      Fixtures.prop_failure
  in
  contains ~msg:"replay hint spelled from the invocation, %0A-encoded"
    ~sub:
      "%0A    replay: dune exec qa/x/t.exe -- --seed s1:7be1d2c904aa31f5 -f \
       'geo › area non-negative'"
    a;
  not_contains ~msg:"no Mirrors spelling under Exe" ~sub:"WINDTRAP_SEED" a;
  let block =
    Report.annotations ~release_failures:[] ~invocation
      [
        Fixtures.result [ "cli"; "cli help" ]
          (Failure.Fail [ Fixtures.snap_missing ]);
      ]
  in
  contains ~msg:"annotations thread the invocation to accept hints"
    ~sub:"%0A    accept: dune exec qa/x/t.exe -- -u -f 'cli › cli help'\n" block;
  (* A replay is armed when the run was; a failure with no command of its
     own ends on its facts. *)
  contains ~msg:"annotations thread the armed mutant to replay hints"
    ~sub:
      "%0A    replay: dune exec qa/x/t.exe -- --arm lib/calc.ml:9:12:add \
       --seed s1:7be1d2c904aa31f5 -f 'geo'\n"
    (Report.annotations ~release_failures:[] ~invocation
       ~armed:"lib/calc.ml:9:12:add"
       [ Fixtures.result [ "geo" ] (Failure.Fail [ Fixtures.prop_failure ]) ]);
  equal ~msg:"an annotation with no command ends on its facts" string
    "::error title=Test failure%3A bad::    boom\n"
    (Report.annotations ~release_failures:[] ~invocation
       ~armed:"lib/calc.ml:9:12:add"
       [ Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "boom" ]) ])

let test_github_ansi_stripped () =
  let f =
    Failure.equality ~expected:"\027[32mgreen\027[0m" ~actual:"plain" ()
  in
  let a = Report.annotation ~path:[ "t" ] f in
  not_contains ~msg:"ANSI stripped from annotations" ~sub:"\027" a;
  (* The annotation shares [pp_failure] at [ansi:false], so a comparison
     value reaches it escaped rather than stripped: the bytes survive the
     workflow-command encoding as ordinary text. *)
  contains ~msg:"the compared value keeps its own bytes"
    ~sub:{|\x1b[32mgreen\x1b[0m|} a

let test_github_groups () =
  equal ~msg:"group start" string "::group::mylib\n"
    (Report.group_start "mylib");
  equal ~msg:"group end" string "::endgroup::\n" Report.group_end;
  equal ~msg:"group name newline encoded" string "::group::a%0Ab\n"
    (Report.group_start "a\nb")

let test_github_excused_filtered () =
  (* Classification is record-driven: an excused expected failure (a
     failing record that did not count) annotates nothing, while the
     unexpected-pass record (counted, annotation and all) stays loud. *)
  let results =
    [
      Fixtures.excused_result;
      Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "boom" ]);
    ]
  in
  let block = Report.annotations ~release_failures:[] results in
  is_true ~msg:"excused failures produce no annotation"
    (occurrences_of ~sub:"::error " block = 1);
  not_contains ~msg:"excused test absent from the block" ~sub:"broken carry"
    block;
  contains ~msg:"counted failures still annotate"
    ~sub:"title=Test failure%3A bad::" block;
  equal ~msg:"all failures excused, no output" string ""
    (Report.annotations ~release_failures:[] [ Fixtures.excused_result ]);
  contains ~msg:"an unexpected pass still annotates"
    ~sub:"title=Test failure%3A known › fixed already::"
    (Report.annotations ~release_failures:[] [ Fixtures.xpass_result ])

let test_github_subtest_annotations () =
  let block =
    Report.annotations ~release_failures:[] [ Fixtures.subtest_result ]
  in
  is_true ~msg:"one annotation per failure entry, subtests included"
    (occurrences_of ~sub:"::error " block = 3);
  contains ~msg:"subtest annotations are titled by the parent test"
    ~sub:"title=Test failure%3A backend › contract::" block;
  contains ~msg:"subtest annotations point into the parent's body"
    ~sub:"file=test/test_backend.ml,line=40," block;
  contains ~msg:"the subtest line follows the entry's location"
    ~sub:"::    test/test_backend.ml:40%0A    subtest   shape [0]%0A" block

let test_github_annotations () =
  let block =
    Report.annotations
      ~release_failures:[ Fixtures.release_failure ]
      Fixtures.results
  in
  is_true
    ~msg:
      "one command per failure entry (teardown pair gives two, the release one)"
    (occurrences_of ~sub:"::error " block = 8);
  is_true ~msg:"every command on its own line"
    (occurrences_of ~sub:"\n" block = 8);
  contains ~msg:"paths name the failing tests"
    ~sub:"title=Test failure%3A db › insert::" block;
  equal ~msg:"no failures, no output" string ""
    (Report.annotations ~release_failures:[]
       [
         Fixtures.result [ "ok" ] Failure.Pass;
         Fixtures.result [ "s" ] (Failure.Skip None);
       ]);
  equal ~msg:"empty run, no output" string ""
    (Report.annotations ~release_failures:[] [])

(* The composed envelope, as [Report.run] assembles it: the fold opens,
   the transcript streams inside it, the fold closes against the last
   section, the annotation block follows the close so that it is never
   folded away, and the summary stays the last line. [Report.run] itself
   calls [Run.execute], which refuses to nest inside the run this suite is
   part of; its composition is pinned at process level by test_windtrap.ml,
   and the envelope's order here. *)

let test_github_envelope_composed () =
  let bad =
    Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "boom" ])
  in
  let renderer =
    Report.create ~out:Format.std_formatter ~ansi:false (config ())
  in
  print_string (Report.group_start "mylib");
  Report.observe renderer ~seed:Fixtures.root ~selection:None
    (Run.Run_started
       { suite = "mylib"; total = 1; selected = 1; properties = false });
  Report.observe renderer ~seed:Fixtures.root ~selection:None
    (Run.Test_finished bad);
  Report.finish renderer ~release_failures:[] ~results:[ bad ] ~duration:0.01
    ~before_summary:(fun () ->
      print_string Report.group_end;
      print_string
        (Report.annotations ~release_failures:[] ~invocation:`Mirrors [ bad ]))
    ();
  equal
    ~msg:
      "the failures and their closing rule, the close, the annotations, then \
       the summary"
    string
    ("::group::mylib\nmylib: 1 test\n" ^ failures_rule
   ^ "\n  FAIL  bad\n    boom\n" ^ closing_rule
   ^ "\n\
      ::endgroup::\n\
      ::error title=Test failure%3A bad::    boom\n\n\
      1 failed in 10ms.\n")
    (output ());
  (* A later section sits inside the fold too, and a green run is the
     fold's two lines over its summary. *)
  let slow = Fixtures.result [ "slow one" ] Failure.Pass ~duration:1.5 in
  let folded results =
    let buf = Buffer.create 256 in
    let ppf = Format.formatter_of_buffer buf in
    let r = Report.create ~out:ppf ~ansi:false (config ()) in
    Report.header r ~suite:"mylib" ~tests:(List.length results) ~seed:None ();
    List.iter (Report.result r) results;
    (* The hook writes past the formatter, as [print_string] does: what the
       renderer committed is flushed before it runs. *)
    Report.finish r ~release_failures:[] ~results ~duration:2.0
      ~before_summary:(fun () -> Buffer.add_string buf "::endgroup::\n")
      ();
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  equal ~msg:"the close follows the last section, the blank line the close"
    string
    ("mylib: 2 tests\n" ^ failures_rule ^ "\n  FAIL  bad\n    boom\n"
   ^ closing_rule
   ^ "\n\n\
      slow tests (1, over 1s):\n\
     \  1.5s  slow one\n\
      ::endgroup::\n\n\
      1 passed, 1 failed in 2.0s.\n")
    (folded [ bad; slow ]);
  equal ~msg:"a green run: the close, then the one line" string
    "::endgroup::\nmylib: 1 passed in 2.0s.\n"
    (folded [ Fixtures.result [ "ok" ] Failure.Pass ])

(* The observer: the seed prints iff the selection holds a property, which
   the executor decides before the first result. *)

let test_observe_seed_policy () =
  let observe r = Report.observe r ~seed:Fixtures.root ~selection:None in
  let started ~properties =
    Run.Run_started { suite = "s"; total = 2; selected = 2; properties }
  in
  let header ~properties =
    with_renderer ~mode:`Verbose (fun r -> observe r (started ~properties))
  in
  equal ~msg:"a selection holding a property prints the root token" string
    "s: 2 tests (seed s1:7be1d2c904aa31f5)\n" (header ~properties:true);
  equal ~msg:"a selection holding none prints no seed" string "s: 2 tests\n"
    (header ~properties:false);
  (* A compact green run is one line, and it is where the seed goes. *)
  let green ~properties =
    with_renderer (fun r ->
        observe r (started ~properties);
        let results =
          [
            Fixtures.result [ "a" ] Failure.Pass;
            Fixtures.result [ "b" ] Failure.Pass;
          ]
        in
        List.iter (fun res -> observe r (Run.Test_finished res)) results;
        Report.finish r ~release_failures:[] ~results ~duration:0.06 ())
  in
  equal ~msg:"a green property run ends on its seed" string
    "s: 2 passed in 60ms (seed s1:7be1d2c904aa31f5).\n" (green ~properties:true);
  equal ~msg:"a green run without a property carries no seed" string
    "s: 2 passed in 60ms.\n" (green ~properties:false);
  let streamed =
    with_renderer ~mode:`Verbose (fun r ->
        observe r
          (Run.Run_started
             { suite = "s"; total = 1; selected = 1; properties = false });
        observe r (Run.Test_started { path = [ "t" ] });
        observe r (Run.Test_finished (Fixtures.result [ "t" ] Failure.Pass));
        observe r (Run.Fixture_release { name = "db" }))
  in
  equal ~msg:"every event has its line under verbose" string
    "s: 1 test\n\
    \  PASS  t                                          0.2ms\n\
     releasing db\n"
    streamed

(* The corrections section

   What the run wrote for its baselines, and what it could not write, is
   a section before the summary and terms of it, so the summary stays the
   last line. *)

let corrections_transcript ?invocation baselines =
  with_renderer ?invocation (fun r ->
      Report.header r ~suite:"s" ~tests:1 ~seed:None ();
      Report.finish r ~release_failures:[]
        ~results:[ Fixtures.result [ "t" ] Failure.Pass ]
        ~duration:0.002 ~baselines ())

let write_source root =
  let path = Filename.concat root "t.ml" in
  Os.mkdir_p root;
  Out_channel.with_open_bin path (fun oc ->
      Out_channel.output_string oc "let () = expect x @@ __POS_OF__ {| a |}\n");
  path

let test_corrections_section () =
  (* One row per file written, paths spelled by [Os.display_path]: the
     verb names the mode, and a source file counts its expectations. *)
  let root = temp_dir () in
  let baselines = Baseline.create ~root ~cwd:root ~mode:Baseline.Update () in
  Baseline.check baselines (Baseline.File "help.expected") "hello\n";
  ignore (Baseline.settle baselines ~keep:true);
  Baseline.write baselines;
  equal ~msg:"update: an accepted row per file, above the summary" string
    (Printf.sprintf
       "s: 1 test\n\
        corrections (1):\n\
       \  accepted %s\n\n\
        1 passed, 1 correction accepted in 2.0ms.\n"
       (Os.display_path (Filename.concat root "help.expected")))
    (corrections_transcript baselines);
  let root = temp_dir () in
  let source = write_source root in
  let baselines = Baseline.create ~root ~cwd:root ~mode:Baseline.Corrected () in
  (try
     Baseline.check baselines
       (Baseline.Literal
          { pos = ("t.ml", 1, 21, 0); value = " a "; exact = false })
       "b"
   with Failure.Check_failure _ -> ());
  (try Baseline.check baselines (Baseline.File "help.expected") "hello\n"
   with Failure.Check_failure _ -> ());
  ignore (Baseline.settle baselines ~keep:true);
  Baseline.write baselines;
  equal
    ~msg:
      "corrected: a wrote row per .corrected, sorted, expectations counted on \
       the source file only, files counted in the summary"
    string
    (Printf.sprintf
       "s: 1 test\n\
        corrections (2):\n\
       \  wrote %s\n\
       \  wrote %s (1 expectation)\n\n\
        1 passed, 2 corrections written in 2.0ms.\n"
       (Os.display_path (Filename.concat root "help.expected.corrected"))
       (Os.display_path (source ^ ".corrected")))
    (corrections_transcript baselines);
  equal ~msg:"the section does not depend on the invocation" string
    (corrections_transcript ~invocation:`Mirrors baselines)
    (corrections_transcript ~invocation:(`Exe "./t.exe") baselines);
  (* A literal accepted in place is compiled into the executable: its row
     says that the tests see it only after a build. *)
  let root = temp_dir () in
  let source = write_source root in
  let baselines = Baseline.create ~root ~cwd:root ~mode:Baseline.Update () in
  Baseline.check baselines
    (Baseline.Literal { pos = ("t.ml", 1, 21, 0); value = " a "; exact = false })
    "b";
  ignore (Baseline.settle baselines ~keep:true);
  Baseline.write baselines;
  equal ~msg:"update: an accepted literal asks for a rebuild" string
    (Printf.sprintf
       "s: 1 test\n\
        corrections (1):\n\
       \  accepted %s (1 expectation; rebuild before the tests see it)\n\n\
        1 passed, 1 correction accepted in 2.0ms.\n"
       (Os.display_path source))
    (corrections_transcript baselines)

let test_corrections_quiet () =
  equal ~msg:"nothing written: the green run stays one line" string
    "s: 1 passed in 2.0ms.\n"
    (corrections_transcript (Baseline.create ~mode:Baseline.Check ()));
  (* A file the run could not write is a row of the section, with its
     reason, and a term of the summary: the correction reached nothing. *)
  let root = temp_dir () in
  let baselines = Baseline.create ~root ~cwd:root ~mode:Baseline.Update () in
  Baseline.check baselines (Baseline.File "a.expected") "a\n";
  Baseline.check baselines (Baseline.File "help.expected") "hello\n";
  ignore (Baseline.settle baselines ~keep:true);
  (* A directory where the file goes: its rename fails. *)
  Os.mkdir_p (Filename.concat root "help.expected");
  Baseline.write baselines;
  let display name = Os.display_path (Filename.concat root name) in
  match String.split_on_char '\n' (corrections_transcript baselines) with
  | [ "s: 1 test"; "corrections (2):"; accepted; refused; ""; summary; "" ] ->
      equal ~msg:"the file written keeps its row" string
        ("  accepted " ^ display "a.expected")
        accepted;
      let head = "  could not write " ^ display "help.expected" ^ ": " in
      starts_with ~msg:"the file refused is a row that says why" ~affix:head
        refused;
      not_contains ~msg:"and whose reason does not repeat the path"
        ~sub:"help.expected"
        (String.sub refused (String.length head)
           (String.length refused - String.length head));
      equal ~msg:"the summary counts it after the corrections" string
        "1 passed, 1 correction accepted, 1 not written in 2.0ms." summary
  | _ -> failf "unexpected transcript:\n%s" (corrections_transcript baselines)

(* Report_sections, value by value *)

let test_headline_whitespace () =
  equal ~msg:"TAB, CR and LF become spaces, other control bytes stay" string
    "a b c d\x01e"
    (Sections.headline (Failure.message "a\tb\rc\nd\x01e"))

(* A relative location is read under the project root first, then as
   given: the two directories hold a file of the same name. *)
let test_excerpt_root_first () =
  let root = temp_dir () and cwd = temp_dir () in
  let write dir text =
    Out_channel.with_open_bin (Filename.concat dir "x.ml") (fun oc ->
        output_string oc text)
  in
  write root "under the root\n";
  write cwd "as given\n";
  setenv "WINDTRAP_PROJECT_ROOT" (Some root);
  chdir cwd;
  let block () =
    failure_block ~excerpt:true
      (Failure.message ~loc:{ Loc.file = "x.ml"; line = 1; column = 0 } "m")
  in
  contains ~msg:"the root's file first" ~sub:"under the root" (block ());
  Sys.remove (Filename.concat root "x.ml");
  contains ~msg:"then the path as given" ~sub:"as given" (block ())

let test_hints_default () =
  let entry ?hints () =
    let buf = Buffer.create 256 in
    let ppf = Format.formatter_of_buffer buf in
    Report.pp_failure ~ansi:false ?hints ppf Fixtures.prop_failure;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  contains ~msg:"hints by default" ~sub:"replay:" (entry ());
  not_contains ~msg:"none under ~hints:false" ~sub:"replay:"
    (entry ~hints:false ())

let test_diff_cap () =
  let side c =
    String.concat "\n" (List.init 500 (fun i -> Printf.sprintf "%c%d" c i))
  in
  let block =
    failure_block (Failure.equality ~expected:(side 'e') ~actual:(side 'a') ())
  in
  (* Every line differs, so the hunks are a head, deletions and
     insertions; the two file headers above them are not counted. *)
  let hunk_line l =
    l <> "    --- expected" && l <> "    +++ actual"
    && (String.starts_with ~prefix:"    @@" l
       || String.starts_with ~prefix:"    -" l
       || String.starts_with ~prefix:"    +" l)
  in
  equal ~msg:"200 lines of hunks, hunk heads included" int 200
    (List.length (List.filter hunk_line (String.split_on_char '\n' block)));
  contains ~msg:"then the count of the rest" ~sub:"801 more" block

let test_render () =
  let line = [ Sections.plain "a\nb\tc"; Sections.styled `Red "\027[0m" ] in
  equal ~msg:"every span escaped, TAB kept, no style without ansi" string
    "a\\x0ab\tc\\x1b[0m"
    (Sections.render ~ansi:false line);
  equal ~msg:"under ansi the style wraps the escaped text" string
    "a\\x0ab\tc\027[31m\\x1b[0m\027[0m"
    (Sections.render ~ansi:true line);
  equal ~msg:"the width counts each escape's four columns" int 15
    (Sections.width line);
  (* Styling nothing is nothing: lines are assembled from optional
     fragments, and an empty one must not leave an open code and its reset
     behind. *)
  equal ~msg:"an empty styled span is left bare" string ""
    (Sections.render ~ansi:true [ Sections.styled `Faint "" ]);
  (* A style is picked by name at the call site, so a swapped code is a
     silently wrong colour rather than a failure. *)
  List.iter
    (fun (name, style, code) ->
      equal ~msg:name string
        ("\027[" ^ code ^ "mx\027[0m")
        (Sections.render ~ansi:true [ Sections.styled style "x" ]))
    [
      ("bold", `Bold, "1");
      ("faint", `Faint, "2");
      ("red", `Red, "31");
      ("green", `Green, "32");
      ("yellow", `Yellow, "33");
      ("bold red is one sequence", `Bold_red, "1;31");
      ("bold green is one sequence", `Bold_green, "1;32");
    ]

let test_shell_word () =
  equal ~msg:"the bare-word alphabet as is" string "aZ9_-./:=+,@%"
    (Sections.shell_word "aZ9_-./:=+,@%");
  equal ~msg:"the empty word is quoted" string "''" (Sections.shell_word "");
  equal ~msg:"a space quotes the word" string "'a b'"
    (Sections.shell_word "a b");
  equal ~msg:"a control byte takes the $'...' form" string "$'a\\nb'"
    (Sections.shell_word "a\nb")

let test_spans () =
  is_true ~msg:"plain is unstyled"
    (Sections.plain "x" = { Sections.style = None; text = "x" });
  is_true ~msg:"styled carries its style"
    (Sections.styled `Red "x" = { Sections.style = Some `Red; text = "x" })

let test_section_print () =
  equal ~msg:"a hint carries no style" string "cmd --flag\n"
    (sections ~ansi:true [ Sections.Hint "cmd --flag" ]);
  let rows =
    Sections.Rows
      {
        margin = "  ";
        columns =
          [
            { Sections.gap = ""; align = `Left; width = None };
            { Sections.gap = " "; align = `Left; width = None };
          ];
        rows =
          [
            [ Sections.plain "a"; Sections.plain ""; Sections.plain "dropped" ];
            [ Sections.plain "bb"; Sections.plain "c" ];
          ];
      }
  in
  equal ~msg:"trailing spaces stripped, a cell beyond the columns not printed"
    string "  a\n  bb c\n" (sections [ rows ]);
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  Sections.print ~out:ppf ~ansi:false [ Sections.Line [ Sections.plain "x" ] ];
  equal ~msg:"print flushes its formatter" string "x\n" (Buffer.contents buf)

let test_rule_width () =
  equal ~msg:"a rule is width columns" int 20
    (Text.length_utf8 (Sections.rule ~width:20 None));
  let label = String.make 30 'l' in
  let long = Sections.rule ~width:20 (Some label) in
  contains ~msg:"a long label is whole" ~sub:label long;
  is_true ~msg:"and takes the rule past its width" (Text.length_utf8 long > 20)

let test_survivor_block_exe_width () =
  let s =
    {
      Sections.mutant = add_survivor.Sections.mutant;
      witnesses =
        [ witness ~exe:"a.exe" "with an executable" 0; witness "without one" 0 ];
    }
  in
  let block = sections (Sections.survivor_block ~exe_width:(Some 12) s) in
  let column sub =
    match
      List.find_opt (fun l -> has ~sub l) (String.split_on_char '\n' block)
    with
    | Some l -> Option.get (Text.first_occurrence ~pattern:sub l)
    | None -> failf "no row holds %S" sub
  in
  equal ~msg:"an empty cell keeps the names in one column" int
    (column "with an executable")
    (column "without one");
  is_true ~msg:"the executable column is at least 12 wide"
    (column "with an executable" - column "a.exe" >= 12)

(* Report, the unpinned edges *)

let test_live_line_cut () =
  let long = String.make 200 'n' in
  let t =
    with_renderer ~ansi:true ~terminal:true (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.begin_test r ~path:[ long ])
  in
  let drawn =
    strip_ansi t |> String.split_on_char '\r' |> List.filter (fun l -> l <> "")
  in
  (match drawn with
  | [ line ] ->
      is_true ~msg:"the live line fits 80 columns" (Text.length_utf8 line <= 80)
  | _ -> failf "one live line, got %S" t);
  is_true ~msg:"the name is not whole" (not (has ~sub:long t))

let test_terminal () =
  let block color =
    let r = Report.terminal { (config ~mode:`Verbose ()) with Run.color } in
    Report.header r ~suite:"s" ~tests:1 ~seed:None ();
    Report.begin_test r ~path:[ "bad" ];
    Report.result r
      (Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "b" ]));
    output ()
  in
  contains ~msg:"--color=always styles standard output" ~sub:"\027[31mFAIL"
    (block Os.Always);
  let plain = block Os.Never in
  contains ~msg:"--color=never does not" ~sub:"FAIL" plain;
  not_contains ~msg:"no escape, no live line off a terminal" ~sub:"\027" plain

let test_infinite_threshold () =
  raises_match ~msg:"+infinity is not finite" Check.Exn.invalid_arg (fun () ->
      Report.create
        ~out:(Format.formatter_of_buffer (Buffer.create 8))
        ~ansi:false
        (config ~slow_threshold:Float.infinity ()))

(* Under --stream the renderer drains the process's buffers before it
   writes, so a streamed test's bytes come first. Standard error's byte is
   buffered and would otherwise reach the descriptors after the report's. *)
let test_stream_drains () =
  let r =
    Report.create ~out:Format.std_formatter ~ansi:false
      { (config ~mode:`Verbose ()) with Run.stream = true }
  in
  let before write =
    Printf.eprintf "E";
    write ();
    let out = output () in
    match Text.first_occurrence ~pattern:"E" out with
    | Some 0 -> ()
    | _ -> failf "the test's byte does not come first: %S" out
  in
  before (fun () -> Report.note r "a notice");
  before (fun () ->
      Report.result r (Fixtures.result [ "t" ] Failure.Pass ~duration:0.1));
  before (fun () ->
      Report.finish r ~release_failures:[]
        ~results:[ Fixtures.result [ "t" ] Failure.Pass ]
        ~duration:0.5 ())

let test_note_under_stream () =
  let t =
    let buf = Buffer.create 64 in
    let ppf = Format.formatter_of_buffer buf in
    let r =
      Report.create ~out:ppf ~ansi:true ~terminal:true
        { (config ()) with Run.stream = true }
    in
    Report.note r "releasing db";
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  equal ~msg:"a compact streamed run shows no notice" string "" t

let test_observe_interrupted () =
  let t =
    with_renderer (fun r ->
        Report.observe r ~seed:Fixtures.root ~selection:None
          (Run.Run_started
             { suite = "s"; total = 2; selected = 2; properties = false });
        Report.observe r ~seed:Fixtures.root ~selection:None
          (Run.Interrupted
             {
               running = Some [ "g"; "t" ];
               releasing = None;
               results = [];
               duration = 0.5;
             }))
  in
  equal ~msg:"the summary, on the renderer" string "s: 2 not run in 500ms.\n" t;
  equal ~msg:"the interruption, on standard error" string
    "windtrap: interrupted in g \u{203a} t\n" (output ())

let test_observe_raises_nothing () =
  let fail = Fixtures.result [ "x" ] (Failure.Fail [ Failure.message "m" ]) in
  ignore
    (with_renderer ~ansi:true ~terminal:true (fun r ->
         let observe = Report.observe r ~seed:Fixtures.root ~selection:None in
         observe (Run.Test_finished fail);
         observe (Run.Fixture_release { name = "" });
         observe (Run.Test_started { path = [] });
         observe
           (Run.Run_started
              { suite = ""; total = 0; selected = 5; properties = true });
         observe (Run.Test_finished fail);
         observe
           (Run.Interrupted
              {
                running = None;
                releasing = Some "\027";
                results = [ fail; fail ];
                duration = Float.nan;
              })))

let test_selection_escapes () =
  equal ~msg:"a double quote and a backslash are escaped" (option string)
    (Some {|filter "a\"b\\c"|})
    (Report.selection_description ~focused:false
       { (Run.default_config ()) with Run.filter = [ {|a"b\c|} ] })

let test_excused_is_slow () =
  let excused = { Fixtures.excused_result with Run.duration = 2.0 } in
  contains ~msg:"an excused result over the threshold is listed"
    ~sub:"known \u{203a} broken carry"
    ( with_renderer (fun r ->
          Report.finish r ~release_failures:[] ~results:[ excused ]
            ~duration:2.0 ())
    |> fun t ->
      match Text.first_occurrence ~pattern:"slow tests" t with
      | Some i -> String.sub t i (String.length t - i)
      | None -> "" )

let test_armed_lines_do_not_flush () =
  List.iter
    (fun (name, write) ->
      let buf = Buffer.create 64 in
      let ppf = Format.formatter_of_buffer buf in
      let r = Report.create ~out:ppf ~ansi:false (config ~armed:"x" ()) in
      write r;
      equal
        ~msg:(name ^ " leaves its line in the formatter")
        string "" (Buffer.contents buf);
      Format.pp_print_flush ppf ();
      is_true ~msg:(name ^ " wrote its line") (Buffer.length buf > 0))
    [
      ( "mutation_armed",
        fun r -> Report.mutation_armed r ~id:"x" ~before:"a" ~after:"b" );
      ("mutation_killed", Report.mutation_killed);
      ("mutation_survived", fun r -> Report.mutation_survived r ~hits:2);
      ("mutation_not_evaluated", Report.mutation_not_evaluated);
    ]

let test_mutation_refused () =
  let r =
    Report.create ~out:Format.std_formatter ~ansi:true ~terminal:true
      (config ())
  in
  Report.mutation_testing r ~index:1 ~total:2 ~id:"lib/a.ml:1:0:add";
  Report.mutation_refused r "no mutant";
  let out = output () in
  match
    ( Text.first_occurrence ~pattern:"lib/a.ml:1:0:add" out,
      Text.first_occurrence ~pattern:"windtrap: no mutant" out )
  with
  | Some drawn, Some said ->
      let erased = String.sub out drawn (said - drawn) in
      contains ~msg:"the live line is erased before the message"
        ~sub:"\r\027[2K" erased;
      let buf = Buffer.create 64 in
      let ppf = Format.formatter_of_buffer buf in
      let r = Report.create ~out:ppf ~ansi:true ~terminal:true (config ()) in
      Report.mutation_testing r ~index:1 ~total:2 ~id:"x";
      Report.mutation_refused r "no mutant";
      is_true ~msg:"and the formatter is flushed, the erase in it"
        (String.ends_with ~suffix:"\r\027[2K" (Buffer.contents buf))
  | _ -> failf "the live line and the message: %S" out

let test_mutation_interrupted_probe () =
  ignore
    (with_renderer (fun r ->
         Report.mutation_interrupted r ~testing:None
           { loop_report with Sections.survivors = []; unreached = [] }));
  equal ~msg:"the probe, on standard error" string
    "windtrap: interrupted during the determinism probe\n" (output ())

let edge_tests =
  [
    test "report: the live line is cut to 80 columns" test_live_line_cut;
    test "report: terminal styles by --color off a terminal" test_terminal;
    test "report: an infinite slow threshold is refused" test_infinite_threshold;
    test "report: under --stream the process's buffers go first"
      test_stream_drains;
    test "report: a compact streamed run shows no notice" test_note_under_stream;
    test "report: observe maps Interrupted" test_observe_interrupted;
    test "report: observe raises nothing of its own" test_observe_raises_nothing;
    test "report: the selection's quotes and backslashes" test_selection_escapes;
    test "report: an excused result can be slow" test_excused_is_slow;
    test "report: the armed lines do not flush" test_armed_lines_do_not_flush;
    test "report: mutation_refused erases the live line first"
      test_mutation_refused;
    test "report: the determinism probe interrupted"
      test_mutation_interrupted_probe;
    test "sections: headline whitespace" test_headline_whitespace;
    test "sections: a relative excerpt is tried under the root first"
      test_excerpt_root_first;
    test "sections: hints are on by default" test_hints_default;
    test "sections: the diff cap is 200 lines" test_diff_cap;
    test "sections: render escapes every span" test_render;
    test "sections: shell_word" test_shell_word;
    test "sections: plain and styled spans" test_spans;
    test "sections: print's hints, rows and flush" test_section_print;
    test "sections: a rule and its label" test_rule_width;
    test "sections: survivor_block's executable column"
      test_survivor_block_exe_width;
  ]

let tests =
  [
    test "golden compact transcript (default)" test_golden_compact;
    test "golden verbose transcript (-v)" test_golden_verbose;
    test "golden verbose transcript, coloured" test_golden_ansi;
    test "golden compact transcript, coloured" test_golden_compact_ansi;
    test "ansi styling and diff highlighting" test_ansi;
    test "live progress line (verbose)" test_live;
    test "live compact tail" test_live_compact_tail;
    test "header forms" test_header_forms;
    test "seed token consistency (guarantee 7)" test_seed_token_consistency;
    test "duration forms" test_duration_forms;
    test "create validation" test_create_validation;
    test "empty run" test_no_tests;
    test "a signal ends the transcript on the summary" test_interrupted;
    test "--stream keeps the transcript's shape" test_stream_shape;
    test "the selection described in words" test_selection_description;
    test "the summary's terms" test_summary_terms;
    test "compact prints nothing per test" test_compact_is_silent_per_test;
    test "compact commits a failure block when its test finishes"
      test_compact_commits_blocks;
    test "verbose commits a failure block under its row"
      test_verbose_commits_blocks;
    test "run-scoped notes" test_note;
    test "green compact run is one named line" test_compact_green_one_liner;
    test "slow untagged tests are noteworthy" test_compact_slow_trigger;
    test "slow reads the recorded duration; failing slow tests warn once"
      test_slow_duration_semantics;
    test "slow threshold zero disables the machinery" test_slow_threshold_zero;
    test "verbose gains the slow warnings" test_verbose_slow_warnings;
    test "the flaky block" test_flaky_block;
    test "headline projection" test_headline;
    test "property projections" test_property_projections;
    test "kind details" test_kind_details;
    test "degenerate equalities" test_degenerate_equalities;
    test "ansi hygiene under ansi:false" test_ansi_hygiene;
    test "control bytes: the refined path escapes and moves its marks"
      test_control_bytes_refined;
    test "control bytes: the hunk path escapes every line"
      test_control_bytes_hunks;
    test "control bytes: the containment excerpt and its mark"
      test_control_bytes_containment;
    test "control bytes: the escape alphabet" test_control_bytes_alphabet;
    test "control bytes: escaping is render-only"
      test_control_bytes_are_render_only;
    test "diff display bounds" test_diff_truncation;
    test "proposed-content display bounds" test_proposed_truncation;
    test "source excerpt" test_excerpt;
    test "captured tail" test_tail;
    test "bounds: a backtrace's ten frames" test_backtrace_cap;
    test "bounds: a long value's middle" test_value_elision;
    test "raise message diff" test_raise_message_diff;
    test "raise message diff guards" test_raise_message_diff_guards;
    test "xfail line" test_xfail_line;
    test "an expected failure's block is dim under -v" test_xfail_block;
    test "xpass-string collision stays excused (F4)" test_excused_collision;
    test "finish with excused failures" test_finish_excused;
    test "unexpected pass is loud" test_xpass_is_loud;
    test "subtest projection" test_subtest_projection;
    test "subtest rendering" test_subtest_rendering;
    test "property stats" test_prop_stats;
    test "a cut text prints its marker" test_cut_text_marker;
    test "cut texts are compared as cut" test_cut_comparisons;
    test "containment: claim-aware block" test_containment_block;
    test "containment: multi-line haystack block" test_containment_multiline;
    test "containment: not-found display cap" test_containment_not_found_cap;
    test "containment: headline forms" test_containment_headlines;
    test "containment: prefix and suffix blocks" test_affix_blocks;
    test "containment: in_order chain-break block" test_in_order_block;
    test "containment: demanded-occurrence headlines" test_demand_headlines;
    test "satisfies/matches: no refinement against the claim"
      test_satisfies_no_refinement;
    test "hunks: trailing whitespace visualized on changed lines"
      test_trailing_whitespace_hunks;
    test "raise: uncaught wording" test_uncaught_wording;
    test "property: timed-out shrink marker (D2)" test_timed_out_marker;
    test "property: a raising candidate is named" test_candidate_raised_marker;
    test "property: spent shrink budget marker (D2)" test_budget_spent_marker;
    test "property: inner label without a location (D4)"
      test_inner_label_without_location;
    test "hints: accept and replay per invocation" test_hints_per_invocation;
    test "hints: armed runs, one line per command line, no rerun"
      test_hint_lines;
    test "a withheld correction: no accept, the reason, nothing after it"
      test_withheld_correction;
    test "an armed run's FAIL titles" test_armed_titles;
    test "verbose PASS prints the label table" test_verbose_pass_labels;
    test "terminal name sanitization" test_name_sanitization;
    test "the location forms" test_location_forms;
    test "the mark prints only where it aligns" test_mark_criterion;
    test "excerpts resolve against the project root" test_excerpt_project_root;
    test "corrections: the written files, per mode" test_corrections_section;
    test "corrections: the quiet gate and refusals" test_corrections_quiet;
    test "coverage: the whole table, colour and plain" test_coverage_table;
    test "coverage: the whole source view, colour and plain"
      test_coverage_source;
    test "coverage: the outcome is the last line" test_coverage_outcome_last;
    test "coverage: the header row and the notes" test_coverage_header;
    test "coverage: a row shows eight ranges" test_coverage_cap;
    test "coverage: red below the gate, plain otherwise" test_coverage_colour;
    test "coverage: source text is escaped" test_coverage_escapes;
    test "mutation: a loop's whole report, colour and plain" test_mutation_loop;
    test "mutation: a survivor's block is committed when its child ends"
      test_mutation_streams;
    test "mutation: the live line" test_mutation_live;
    test "mutation: an interrupted loop's closing" test_mutation_interrupted;
    test "mutation: the merge's whole report, colour and plain"
      test_mutation_at_rest;
    test "mutation: the sentence and the executable column"
      test_mutation_sentence;
    test "mutation: the outcome line's forms" test_mutation_summary_forms;
    test "mutation: the reproduce command" test_mutation_reproduce;
    test "mutation: never reached, one row per file" test_mutation_unreached;
    test "mutation: a survivor's source line is escaped" test_mutation_escapes;
    test "mutation: the armed verdict counts in English"
      test_mutation_armed_verdict;
    test "github: golden annotation" test_github_golden;
    test "github: data encoding (%0A/%0D/%25)" test_github_data_encoding;
    test "github: property encoding (%3A/%2C)" test_github_property_encoding;
    test "github: annotation without a location" test_github_no_location;
    test "github: an annotation located at the declaration"
      test_github_declaration_location;
    test "github: replay info" test_github_replay_info;
    test "github: invocation-spelled hints" test_github_invocation_hints;
    test "github: ANSI stripped" test_github_ansi_stripped;
    test "github: group folding commands" test_github_groups;
    test "github: excused failures filtered" test_github_excused_filtered;
    test "github: subtest annotations" test_github_subtest_annotations;
    test "github: run-level annotations block" test_github_annotations;
    test "github: the envelope composed around a transcript"
      test_github_envelope_composed;
    test "observer: the header-seed policy and the stream"
      test_observe_seed_policy;
  ]
  @ edge_tests

let () = exit @@ Windtrap.run "report" tests
