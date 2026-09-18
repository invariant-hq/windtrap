(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Report and Report_sections: golden transcripts over a
   synthetic run covering every failure kind (equality with diff, raise,
   baseline missing/mismatch, property with inner failure, body + teardown
   pair, captured tail with a drop count) at both levels — compact (nothing
   per test, the header iff a block follows) and verbose (a line per test)
   — the noteworthy rule, the slow and flaky blocks, ANSI styling and diff
   highlighting, ANSI hygiene under ansi:false (payload-borne escapes
   stripped), the live displays, the failure projections (headline,
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

let check name cond = is_true ~msg:name cond
let check_string name ~expected ~actual = equal ~msg:name string expected actual
let has ~sub s = Text.contains_substring ~pattern:sub s

let occurrences_of ~sub s =
  let n = String.length sub in
  let rec go i acc =
    if i + n > String.length s then acc
    else if String.sub s i n = sub then go (i + 1) (acc + 1)
    else go (i + 1) acc
  in
  go 0 0

let check_contains name ~sub s = Windtrap.contains ~msg:name ~sub s
let check_absent name ~sub s = not_contains ~msg:name ~sub s

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

let with_renderer ?(ansi = false) ?mode ?live ?slow_threshold ?invocation ?armed
    fn =
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  let r =
    Report.create ~out:ppf ~ansi ?live
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

let transcript ?ansi ?mode ?live ?invocation ?(seed = Some Fixtures.root) () =
  with_renderer ?ansi ?mode ?live ?invocation (fun r ->
      Report.header r ~suite:"mylib"
        ~tests:(List.length Fixtures.results)
        ~seed ();
      List.iter
        (fun (res : Run.result) ->
          Report.begin_test r ~path:res.path;
          Report.result r res)
        Fixtures.results;
      Report.finish r ~results:Fixtures.results ~duration:Fixtures.duration ())

let failure_block ?(ansi = false) ?excerpt ?filter ?invocation ?armed f =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Report.pp_failure ~ansi ?excerpt ?filter ?invocation ?armed ppf f;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

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
  check_absent "plain transcript has no escape codes" ~sub:"\027" actual

let test_golden_verbose () =
  let actual = transcript ~mode:`Verbose ~invocation:golden_invocation () in
  golden "verbose" actual;
  check_absent "plain transcript has no escape codes" ~sub:"\027" actual

(* The coloured transcript, which had no golden at all: [test_ansi] pins
   nine substrings, so every escape run BETWEEN them was unpinned — and a
   colour bug is exactly a wrong byte next to a right one. A baseline of
   the whole thing costs one file and pins the escapes literally, which
   is the only way to review them. *)
let test_golden_ansi () =
  let actual =
    transcript ~ansi:true ~mode:`Verbose ~invocation:golden_invocation ()
  in
  golden "verbose-ansi" actual;
  check_contains "the ansi golden really is coloured" ~sub:"\027[" actual

(* The compact one too: the rules around the failures are its own, and
   dim. *)
let test_golden_compact_ansi () =
  let actual = transcript ~ansi:true ~invocation:golden_invocation () in
  golden "compact-ansi" actual;
  check_contains "the opening rule is dim"
    ~sub:("\n\027[2m" ^ failures_rule ^ "\027[0m\n  \027[31mFAIL\027[0m")
    actual;
  check_contains "the closing rule is dim, a blank line after it"
    ~sub:("\n\027[2m" ^ closing_rule ^ "\027[0m\n\n")
    actual

let test_ansi () =
  let t = transcript ~ansi:true ~mode:`Verbose () in
  check_contains "ansi: FAIL tag is red" ~sub:"\027[31mFAIL\027[0m" t;
  check_contains "ansi: PASS tag is green" ~sub:"\027[32mPASS\027[0m" t;
  check_contains
    "ansi: the inserted span is bold red inside a plain value, and no mark \
     prints under colour"
    ~sub:
      "\027[2mexpected\027[0m  [(\"alice\", [1; 2; 3]); (\"bob\", [4])]\n\
      \    \027[2mactual\027[0m    [(\"alice\", [1; 2; 3]); (\"bob\", \
       [4\027[1;31m; 5]); (\"carol\", [\027[0m])]\n\
      \    \027[2mcaptured output"
    t;
  check_absent "ansi: no [~] line anywhere in a coloured transcript" ~sub:"~" t;
  check_contains "ansi: slow entry is caution yellow, one style"
    ~sub:"\n\027[33m  2.5s  slow › big sort\027[0m\n" t;
  check_contains "ansi: slow heading is caution yellow, one style"
    ~sub:"\n\027[33mslow tests (2, over 1s):\027[0m\n" t;
  check_absent "ansi: the slow section carries no advice line"
    ~sub:"exempt with the" t;
  let c = transcript ~ansi:true () in
  (* The summary counts wear the block palette — one convention for the
     whole transcript, not two for the same run. *)
  check_contains "ansi: summary skip count is yellow"
    ~sub:"\027[33m1 skipped\027[0m" c;
  check_contains "ansi: summary fail count is red"
    ~sub:"\027[31m6 failed\027[0m" c;
  (* The transcript fixture carries no excused result, so the faint count
     gets its own run. *)
  let x =
    with_renderer ~ansi:true (fun r ->
        Report.finish r ~results:[ Fixtures.excused_result ] ~duration:0.1 ())
  in
  check_contains "ansi: summary excused count is faint"
    ~sub:"\027[2m1 expected failure\027[0m" x;
  (* A pair that refines: ["true"]/["false"] marks 80% of a side, which the
     noise rule declines — the styling has to be shown on a real span. *)
  let b =
    failure_block ~ansi:true
      (Failure.equality ~expected:"the quick brown fox"
         ~actual:"the quick brawn fox" ())
  in
  check_contains "ansi: the expected side's changed span is bold green"
    ~sub:"\027[2mexpected\027[0m  the quick br\027[1;32mo\027[0mwn fox\n" b;
  check_contains "ansi: the actual side's changed span is bold red"
    ~sub:"\027[2mactual\027[0m    the quick br\027[1;31ma\027[0mwn fox\n" b;
  check_absent "ansi: a refined pair prints no [~] line under colour" ~sub:"~" b;
  let plain_refined =
    failure_block
      (Failure.equality ~expected:"the quick brown fox"
         ~actual:"the quick brawn fox" ())
  in
  check_string "plain: a [~] line under each changed side"
    ~expected:
      "    expected  the quick brown fox\n\
      \                          ~\n\
      \    actual    the quick brawn fox\n\
      \                          ~\n"
    ~actual:plain_refined;
  (* A span of spaces has no glyph to colour: its [~] line prints under
     colour too, on the side that holds it. *)
  let spaces =
    failure_block ~ansi:true
      (Failure.equality ~expected:"a long enough  value"
         ~actual:"a long enough value" ())
  in
  check_string "ansi: a changed span of spaces keeps its mark"
    ~expected:
      "    \027[2mexpected\027[0m  a long enough \027[1;32m \027[0mvalue\n\
      \                            \027[1;32m~\027[0m\n\
      \    \027[2mactual\027[0m    a long enough value\n"
    ~actual:spaces;
  (* Refinement declined: each side is colored whole rather than losing its
     color, so an equality failure reads the same way either way. *)
  let d =
    failure_block ~ansi:true
      (Failure.equality ~expected:"true" ~actual:"false" ())
  in
  check_contains "ansi: a declined pair colors expected whole"
    ~sub:"\027[32mtrue\027[0m" d;
  check_contains "ansi: a declined pair colors actual whole"
    ~sub:"\027[31mfalse\027[0m" d;
  let plain =
    failure_block (Failure.equality ~expected:"true" ~actual:"false" ())
  in
  check_absent "plain: a declined pair gets no marker line" ~sub:"~" plain;
  check_contains "plain: a declined pair still shows both values"
    ~sub:"expected  true\n    actual    false" plain

let test_live () =
  let t =
    with_renderer ~ansi:true ~mode:`Verbose ~live:true (fun r ->
        Report.header r ~suite:"mylib" ~tests:2 ~seed:None ();
        Report.begin_test r ~path:[ "math"; "addition" ];
        Report.result r (List.hd Fixtures.results))
  in
  check_contains "live: progress line drawn"
    ~sub:"Running [1/2] math › addition" t;
  check_contains "live: cursor clear emitted" ~sub:"\r\027[2K" t;
  let plain =
    with_renderer ~ansi:false ~mode:`Verbose ~live:true (fun r ->
        Report.header r ~suite:"mylib" ~tests:2 ~seed:None ();
        Report.begin_test r ~path:[ "math"; "addition" ])
  in
  check_absent "live: off without ansi" ~sub:"Running" plain

let test_live_compact_tail () =
  (* The compact erasable tail is the only thing a green compact run
     prints while it runs: it draws from column zero, never brings the
     header out, and its erasure re-prints nothing — a green run's screen
     stays blank, and what a pipe sees is exactly the committed
     transcript. *)
  let t =
    with_renderer ~ansi:true ~live:true (fun r ->
        Report.header r ~suite:"mylib" ~tests:2 ~seed:None ();
        Report.begin_test r ~path:[ "math"; "addition" ];
        Report.result r (List.hd Fixtures.results);
        Report.begin_test r ~path:[ "users"; "sessions after login" ])
  in
  check_contains "compact tail: counter and name drawn"
    ~sub:"[1/2] math › addition" t;
  check_absent "compact tail: the tail never brings the header out"
    ~sub:"mylib: 2 tests" t;
  check_contains "compact tail: the erase re-prints nothing"
    ~sub:"\027[0m\r\027[2K\r\027[2K\027[2m  [2/2]" t;
  check_contains "compact tail: next tail follows from column zero"
    ~sub:"[2/2] users › sessions after login" t;
  (* A failure commits its block when the test finishes: the tail is
     erased before the first committed byte, and the next tail draws from
     column zero under the block. *)
  let after_failure =
    with_renderer ~ansi:true ~live:true (fun r ->
        Report.header r ~suite:"mylib" ~tests:2 ~seed:None ();
        Report.begin_test r ~path:[ "bad" ];
        Report.result r
          (Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "b" ]));
        Report.begin_test r ~path:[ "math"; "addition" ];
        Report.result r (List.hd Fixtures.results))
  in
  check_string "compact tail: erased before the block, redrawn under it"
    ~expected:
      ("\r\027[2K\027[2m  [1/2] bad…\027[0m\r\027[2Kmylib: 2 tests\n\027[2m"
     ^ failures_rule
     ^ "\027[0m\n\
       \  \027[31mFAIL\027[0m  \027[1mbad\027[0m\n\
       \    b\n\
        \r\027[2K\027[2m  [2/2] math › addition…\027[0m\r\027[2K")
    ~actual:after_failure;
  let plain =
    with_renderer ~ansi:false ~live:true (fun r ->
        Report.header r ~suite:"mylib" ~tests:2 ~seed:None ();
        Report.begin_test r ~path:[ "math"; "addition" ])
  in
  check_absent "compact tail: off without ansi" ~sub:"[1/2]" plain

let test_header_forms () =
  let one =
    with_renderer ~mode:`Verbose (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ())
  in
  check_string "header: singular, no seed" ~expected:"s: 1 test\n" ~actual:one;
  let zero =
    with_renderer ~mode:`Verbose (fun r ->
        Report.header r ~suite:"s" ~tests:0 ~seed:None ())
  in
  check_string "header: zero tests" ~expected:"s: 0 tests\n" ~actual:zero;
  (* Compact prints the header before its first block or section, from
     the recorded fields (the golden transcripts pin it). *)
  let compact =
    with_renderer (fun r -> Report.header r ~suite:"s" ~tests:1 ~seed:None ())
  in
  check_string "header: compact prints nothing at the start" ~expected:""
    ~actual:compact

let test_seed_token_consistency () =
  (* Guarantee 7: the replay line prints exactly the token the header printed. *)
  let token = Seed.to_string Fixtures.root in
  let t = transcript () in
  check_contains "header carries the root token"
    ~sub:(Printf.sprintf "(seed %s)" token)
    t;
  check_contains "replay line carries exactly the header token"
    ~sub:(Printf.sprintf "replay: WINDTRAP_SEED=%s " token)
    t

let test_duration_forms () =
  let line duration =
    with_renderer ~mode:`Verbose (fun r ->
        Report.result r (Fixtures.result [ "t" ] Failure.Pass ~duration))
  in
  let summary duration =
    with_renderer (fun r ->
        Report.finish r
          ~results:[ Fixtures.result [ "t" ] Failure.Pass ]
          ~duration ())
  in
  (* One format in every measured slot: a verbose row and the summary
     print the same bytes for the same duration, rounded before the unit
     is chosen. *)
  List.iter
    (fun (secs, form) ->
      check_contains
        (Printf.sprintf "a row prints %gs as %s" secs form)
        ~sub:("  " ^ form ^ "\n")
        (line secs);
      check_contains
        (Printf.sprintf "the summary prints %gs as %s" secs form)
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
  check "no 10.0ms around 9.95 ms" (edge 9_900 10_050);
  check "no 1000ms around 999.5 ms" (edge 999_000 1_000_600)

let test_create_validation () =
  let raises fn =
    match fn () with
    | (_ : Report.t) -> false
    | exception Invalid_argument _ -> true
  in
  let ppf = Format.formatter_of_buffer (Buffer.create 8) in
  check "create: negative slow_threshold rejected"
    (raises (fun () ->
         Report.create ~out:ppf ~ansi:false (config ~slow_threshold:(-1.0) ())));
  check "create: non-finite slow_threshold rejected"
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
    Report.finish r ~results ~duration:0.5 ();
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  let pass = Fixtures.result [ "ok" ] Failure.Pass in
  let bad = Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "b" ]) in
  let tail =
    let buf = Buffer.create 64 in
    let ppf = Format.formatter_of_buffer buf in
    let r =
      Report.create ~out:ppf ~ansi:true ~live:true
        { (config ()) with Run.stream = true }
    in
    Report.begin_test r ~path:[ "ok" ];
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  check_string "no live tail for a streamed test's bytes to land on"
    ~expected:"" ~actual:tail;
  check_string "a green streamed run is the one line"
    ~expected:"s: 1 passed in 500ms.\n" ~actual:(streamed [ pass ]);
  check_string "a streamed run's failures are the compact section"
    ~expected:
      ("s: 2 tests\n" ^ failures_rule ^ "\n  FAIL  bad\n    b\n" ^ closing_rule
     ^ "\n\n1 passed, 1 failed in 500ms.\n")
    ~actual:(streamed [ pass; bad ]);
  check_string "rows are -v's, under --stream too"
    ~expected:
      "s: 1 test\n\
      \  PASS  ok                                         0.2ms\n\
       1 passed in 500ms.\n"
    ~actual:(streamed ~verbose:true [ pass ])

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
  check_string "the summary counts the stopped test among the not run"
    ~expected:"s: 1 passed, 3 not run in 500ms.\n"
    ~actual:(transcript (Some [ "g"; "sleeps\n" ]));
  check_string "stderr names the stopped test, its control byte escaped"
    ~expected:"windtrap: interrupted in g \u{203a} sleeps\\n\n"
    ~actual:(output ());
  check_string "a verbose run keeps its rows above the summary"
    ~expected:
      "s: 4 tests\n\
      \  PASS  g \u{203a} ok                                     0.2ms\n\
       1 passed, 3 not run in 500ms.\n"
    ~actual:(transcript ~mode:`Verbose None);
  check_string "with no test running it says so"
    ~expected:"windtrap: interrupted between tests\n" ~actual:(output ());
  let stopped_first =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:4 ~seed:None ();
        Report.interrupted r ~releasing:"fixture (db.ml:3)\027[31m"
          ~running:None ~results:[] ~duration:0.5 ())
  in
  check_string "a run stopped before its first result is its not-run count"
    ~expected:"s: 4 not run in 500ms.\n" ~actual:stopped_first;
  check_string "a stopped release is named, its control byte escaped"
    ~expected:
      "windtrap: interrupted while releasing fixture (db.ml:3)\\x1b[31m\n"
    ~actual:(output ())

(* The selection in words that hold whichever layer set it, a flag or a
   mirror: the empty run's sentence is read, and its values retyped. *)
let test_selection_description () =
  let describe config = Report.selection_description config in
  let base = Run.default_config () in
  check "nothing narrows a default run" (describe base = None);
  check_string "every part named, the last joined with and"
    ~expected:
      "filter \"pars er\", exclusion \"it's\", tag \"a\", \"b c\", excluded \
       tag \"d\", --failed and shard 1/3"
    ~actual:
      (Option.get
         (describe
            {
              base with
              Run.filter = Some "pars er";
              exclude = Some "it's";
              tags = [ "a"; "b c" ];
              exclude_tags = [ "d" ];
              failed_only = true;
              shard = Some (1, 3);
            }));
  check_string "a control byte is escaped, so the line stays one"
    ~expected:"filter \"a\\nb\""
    ~actual:(Option.get (describe { base with Run.filter = Some "a\nb" }))

let test_no_tests () =
  (* No header, so no selection and no declared count: nothing to say
     beyond the fact. *)
  let t =
    with_renderer (fun r -> Report.finish r ~results:[] ~duration:0.01 ())
  in
  check_string "finish: empty run" ~expected:"no tests ran.\n" ~actual:t;
  (* A suite that declares nothing is not a mistyped filter. *)
  let declares_none =
    with_renderer (fun r ->
        Report.header r ~suite:"mylib" ~tests:0 ~declared:0 ~seed:None ();
        Report.finish r ~results:[] ~duration:0.01 ())
  in
  check_string "empty suite names itself as the cause"
    ~expected:"mylib: no tests ran: the suite declares none.\n"
    ~actual:declares_none;
  (* A selection that matched nothing names itself and the denominator,
     and points at the way to see what there was: the one line allowed
     after an outcome. A build action has no launcher to restate and names
     the flag; a suite that declares nothing has nothing to list. *)
  let filtered invocation =
    with_renderer ~invocation (fun r ->
        Report.header r ~suite:"mylib" ~tests:0 ~declared:48
          ~selection:{|filter "parsr"|} ~seed:None ();
        Report.finish r ~results:[] ~duration:0.01 ())
  in
  check_string "empty selection names the selection, the total and the way out"
    ~expected:
      "mylib: no tests ran: filter \"parsr\" matched none of 48 tests.\n\
       list: ./t.exe -l\n"
    ~actual:(filtered (`Exe "./t.exe"));
  check_string "a build action's empty selection names the flag"
    ~expected:
      "mylib: no tests ran: filter \"parsr\" matched none of 48 tests.\n\
       (list the suite's tests with -l)\n"
    ~actual:(filtered `Mirrors);
  check_string "nor has a suite that declares nothing"
    ~expected:"mylib: no tests ran: the suite declares none.\n"
    ~actual:
      (with_renderer ~invocation:(`Exe "./t.exe") (fun r ->
           Report.header r ~suite:"mylib" ~tests:0 ~declared:0 ~seed:None ();
           Report.finish r ~results:[] ~duration:0.01 ()));
  let singular =
    with_renderer (fun r ->
        Report.header r ~suite:"mylib" ~tests:0 ~declared:1
          ~selection:"tag \"slow\"" ~seed:None ();
        Report.finish r ~results:[] ~duration:0.01 ())
  in
  check_contains "one declared test is not \"1 tests\""
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
  check_string "compact: a pass prints nothing" ~expected:""
    ~actual:(silent [ Fixtures.result [ "t" ] Failure.Pass ]);
  check_string "compact: a skip prints nothing" ~expected:""
    ~actual:(silent [ Fixtures.result [ "t" ] (Failure.Skip None) ]);
  check_string "compact: an excused failure prints nothing" ~expected:""
    ~actual:(silent [ Fixtures.excused_result ])

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
  check_string "the start of the run and a pass commit nothing" ~expected:""
    ~actual:(committed ());
  observe (Run.Test_started { path = [ "first" ] });
  observe (Run.Test_finished (bad "first"));
  let first = "s: 4 tests\n" ^ failures_rule ^ "\n" ^ block "first" in
  check_string
    "a failure commits the header, the opening rule and its block when its \
     test finishes, flushed"
    ~expected:first ~actual:(committed ());
  observe (Run.Test_finished (bad "second"));
  let second = first ^ "\n" ^ block "second" in
  check_string
    "the next block follows one blank line; the opening rule prints once"
    ~expected:second ~actual:(committed ());
  check "the opening rule prints once, before the first block"
    (occurrences_of ~sub:failures_rule (committed ()) = 1);
  check_absent "the closing rule is the end of the run's, not a block's"
    ~sub:closing_rule (committed ());
  (* The executor records a fixture release that raised after the last
     test, with no event: its block is what [finish] still owes. *)
  let release =
    {
      (Fixtures.result Run.fixture_release_path
         (Failure.Fail
            [
              Failure.with_phase Failure.Release
                (Failure.message
                   ~loc:(Fixtures.loc "test/t.ml" 4)
                   "db: release raised Exit");
            ]))
      with
      Run.subject = Run.Fixture_release;
    }
  in
  Report.finish r
    ~results:[ pass; bad "first"; bad "second"; release ]
    ~duration:0.0042 ();
  check_string
    "finish adds the block it still owes, the closing rule, then the summary: \
     one test of the four never ran"
    ~expected:
      (second
     ^ "\n\
       \  FAIL  fixture release\n\
       \    [release] test/t.ml:4\n\
       \    db: release raised Exit\n" ^ closing_rule
     ^ "\n\n1 passed, 3 failed, 1 not run in 4.2ms.\n")
    ~actual:(committed ());
  let exe =
    with_renderer ~invocation:(`Exe "./t.exe") (fun r ->
        Report.finish r ~results:[ release ] ~duration:0.001 ())
  in
  check_contains "a release row ends on its facts: no command closes it"
    ~sub:("\n    db: release raised Exit\n" ^ closing_rule ^ "\n")
    exe;
  check_absent "no block carries a rerun hint" ~sub:"rerun" exe

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
  check_string "the header is committed at the start, flushed"
    ~expected:"s: 4 tests\n" ~actual:(committed ());
  observe (Run.Test_finished pass);
  let passed = "s: 4 tests\n" ^ row "PASS" "ok" in
  check_string "a pass commits its row" ~expected:passed ~actual:(committed ());
  observe (Run.Test_finished (bad "first"));
  let first = passed ^ block "first" in
  check_string
    "a failure commits its row and, under it, its block's lines when its test \
     finishes, flushed"
    ~expected:first ~actual:(committed ());
  observe (Run.Test_finished (bad "second"));
  let second = first ^ block "second" in
  check_string
    "a blank line closes each block; no heading and no rule separates two rows"
    ~expected:second ~actual:(committed ());
  let release =
    {
      (Fixtures.result Run.fixture_release_path
         (Failure.Fail
            [
              Failure.with_phase Failure.Release
                (Failure.message
                   ~loc:(Fixtures.loc "test/t.ml" 4)
                   "db: release raised Exit");
            ]))
      with
      Run.subject = Run.Fixture_release;
    }
  in
  Report.finish r
    ~results:[ pass; bad "first"; bad "second"; release ]
    ~duration:0.0042 ();
  check_string
    "finish adds the row it still owes with its block, then the summary: no \
     failures section repeats the blocks"
    ~expected:
      (second
      ^ row "FAIL" "fixture release"
      ^ "    [release] test/t.ml:4\n\
        \    db: release raised Exit\n\n\
         1 passed, 3 failed, 1 not run in 4.2ms.\n")
    ~actual:(committed ());
  check_absent "verbose draws no rule" ~sub:"\u{2500}" (committed ());
  let armed =
    with_renderer ~mode:`Verbose ~ansi:true ~invocation:(`Exe "./t.exe")
      ~armed:"lib/calc.ml:9:12:add" (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r
          (Fixtures.result [ "first" ] (Failure.Fail [ Fixtures.prop_failure ])))
  in
  check_contains
    "the row is a title: its path bold, and (mutant armed) in an armed run"
    ~sub:
      "  \027[31mFAIL\027[0m  \027[1mfirst\027[0m \027[2m(mutant armed)\027[0m"
    armed;
  check_contains "its hints are the block's, and the blank line closes it"
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
  check_contains "a missing baseline qualifies the row, in parentheses"
    ~sub:"  FAIL  help (no baseline)  " (missing ());
  check_absent "and no dash does" ~sub:"\u{2014}" (missing ());
  check_contains "the armed qualifier shares the slot"
    ~sub:"  FAIL  help (no baseline, mutant armed)  "
    (missing ~armed:"lib/calc.ml:9:12:add" ())

let test_note () =
  (* Run-scoped notices (fixture releases) land between results. Compact
     prints nothing per test, so the notice is an erasable live line and
     never part of the transcript — a green run keeps its one line and a
     noteworthy one its blocks; verbose prints it in position. *)
  let green =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:2 ~seed:None ();
        Report.result r (Fixtures.result [ "a" ] Failure.Pass);
        Report.result r (Fixtures.result [ "b" ] Failure.Pass);
        Report.note r "releasing db";
        Report.finish r
          ~results:
            [
              Fixtures.result [ "a" ] Failure.Pass;
              Fixtures.result [ "b" ] Failure.Pass;
            ]
          ~duration:0.01 ())
  in
  check_string "note: a green compact run stays one line"
    ~expected:"s: 2 passed in 10ms.\n" ~actual:green;
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
        Report.finish r ~results ~duration:0.01 ())
  in
  check_absent "note: a compact transcript never carries the notice"
    ~sub:"releasing" noteworthy;
  check "note: the compact transcript opens with the header"
    (String.starts_with
       ~prefix:("s: 2 tests\n" ^ failures_rule ^ "\n  FAIL  b\n")
       noteworthy);
  let verbose =
    with_renderer ~mode:`Verbose (fun r -> Report.note r "releasing db")
  in
  check_string "note: verbose prints the plain line" ~expected:"releasing db\n"
    ~actual:verbose;
  let live =
    with_renderer ~ansi:true ~live:true (fun r ->
        Report.header r ~suite:"s" ~tests:2 ~seed:None ();
        Report.result r (Fixtures.result [ "a" ] Failure.Pass);
        Report.begin_test r ~path:[ "b" ];
        Report.note r "releasing db";
        Report.finish r
          ~results:[ Fixtures.result [ "a" ] Failure.Pass ]
          ~duration:0.01 ())
  in
  check_contains "note: the live tail is erased, the notice drawn erasable"
    ~sub:"\r\027[2K\027[2mreleasing db\027[0m" live;
  check_contains "note: the erasable notice is erased before the one-liner"
    ~sub:"releasing db\027[0m\r\027[2Ks: \027[32m1 passed" live

(* The summary's terms, in their order, zero terms omitted. [not run] is
   what a stopped run ([-x]) selected and never reached. *)

let test_summary_terms () =
  let bad = Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "b" ]) in
  let stopped =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:5 ~seed:None ();
        Report.result r bad;
        Report.finish r ~results:[ bad ] ~duration:0.0004 ())
  in
  check "a stopped run counts what it never reached"
    (String.ends_with ~suffix:"\n\n1 failed, 4 not run in 0.4ms.\n" stopped);
  let complete =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r bad;
        Report.finish r ~results:[ bad ] ~duration:0.0004 ())
  in
  check_absent "a run that reached every selected test omits the term"
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
        Report.finish r ~results ~duration:6.5 ~baselines ())
  in
  check "every term, in order, the summary last"
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
  check_string "zero terms are omitted, [passed] included; a plural term"
    ~expected:"s: 2 expected failures in 1.0ms.\n"
    ~actual:
      (with_renderer (fun r ->
           Report.header r ~suite:"s" ~tests:2 ~seed:None ();
           Report.finish r ~results:excused ~duration:0.001 ()))

(* The noteworthy rule *)

let test_compact_green_one_liner () =
  let passes = [ Fixtures.result [ "a" ] Failure.Pass ] in
  let named =
    with_renderer (fun r ->
        Report.header r ~suite:"mylib" ~tests:1 ~seed:None ();
        List.iter (Report.result r) passes;
        Report.finish r ~results:passes ~duration:1.2 ())
  in
  check_string "green compact run: exactly one named line"
    ~expected:"mylib: 1 passed in 1.2s.\n" ~actual:named;
  let seeded =
    with_renderer (fun r ->
        Report.header r ~suite:"mylib" ~tests:1 ~seed:(Some Fixtures.root) ();
        List.iter (Report.result r) passes;
        Report.finish r ~results:passes ~duration:1.2 ())
  in
  check_string "green compact run: the seed the header carried is appended"
    ~expected:"mylib: 1 passed in 1.2s (seed s1:7be1d2c904aa31f5).\n"
    ~actual:seeded;
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
        Report.finish r ~results ~duration:0.2 ())
  in
  check_string
    "green compact run: skip and expected-failure segments stay on the line"
    ~expected:"mylib: 1 passed, 1 skipped, 1 expected failure in 200ms.\n"
    ~actual:segments;
  let empty =
    with_renderer (fun r ->
        Report.header r ~suite:"mylib" ~tests:0 ~seed:None ();
        Report.finish r ~results:[] ~duration:0.01 ())
  in
  (* [~declared] defaults to [~tests], which is 0 here: the suite really
     does declare nothing. *)
  check_string "empty compact selection: one named line, no header"
    ~expected:"mylib: no tests ran: the suite declares none.\n" ~actual:empty

let test_compact_slow_trigger () =
  let slow_pass = Fixtures.result [ "t" ] Failure.Pass ~duration:1.2 in
  let t =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r slow_pass;
        Report.finish r ~results:[ slow_pass ] ~duration:1.2 ())
  in
  check_string
    "an untagged over-threshold pass is noteworthy: header, then the section \
     with its threshold, and no advice line"
    ~expected:
      "s: 1 test\nslow tests (1, over 1s):\n  1.2s  t\n\n1 passed in 1.2s.\n"
    ~actual:t;
  let at_threshold =
    let r1 = Fixtures.result [ "t" ] Failure.Pass ~duration:1.0 in
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r r1;
        Report.finish r ~results:[ r1 ] ~duration:1.0 ())
  in
  check "the threshold is inclusive (duration >= threshold)"
    (String.starts_with
       ~prefix:"s: 1 test\nslow tests (1, over 1s):\n  1.0s  t\n" at_threshold);
  (* The threshold is configured, not measured: it prints as it was
     written, in seconds, never in the measured format or an exponent. *)
  List.iter
    (fun (slow_threshold, written) ->
      check_contains
        (Printf.sprintf "a threshold of %s seconds prints as given" written)
        ~sub:(Printf.sprintf "slow tests (1, over %ss):\n" written)
        (with_renderer ~slow_threshold (fun r ->
             Report.finish r ~results:[ slow_pass ] ~duration:1.2 ())))
    [ (0.01, "0.01"); (0.5, "0.5"); (1e-9, "0.000000001") ];
  let tagged_pass = { slow_pass with Run.slow_tagged = true } in
  let tagged =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r tagged_pass;
        Report.finish r ~results:[ tagged_pass ] ~duration:1.2 ())
  in
  check_string "a slow-tagged test is exempt everywhere: one line, no warning"
    ~expected:"s: 1 passed in 1.2s.\n" ~actual:tagged;
  let skip = Fixtures.result [ "t" ] (Failure.Skip None) ~duration:2.0 in
  let skipped =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r skip;
        Report.finish r ~results:[ skip ] ~duration:2.0 ())
  in
  check_string "a skip never triggers the threshold"
    ~expected:"s: 1 skipped in 2.0s.\n" ~actual:skipped;
  (* An excused expected failure is not a counted failure — but its
     duration still counts against the threshold when untagged. *)
  let excused_fast =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r Fixtures.excused_result;
        Report.finish r ~results:[ Fixtures.excused_result ] ~duration:0.1 ())
  in
  check_string "an excused failure alone is not noteworthy"
    ~expected:"s: 1 expected failure in 100ms.\n" ~actual:excused_fast

let test_slow_duration_semantics () =
  (* The compared duration is [Run.result.duration] — the attempts summed
     (run.mli) — so a retried test whose attempts together cross the
     threshold is slow even when its final attempt was fast. *)
  let retried =
    Fixtures.result [ "flaky" ] Failure.Pass ~duration:1.2 ~attempts:3
  in
  let t =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r retried;
        Report.finish r ~results:[ retried ] ~duration:1.2 ())
  in
  check "a retried test is noteworthy on its summed duration"
    (String.starts_with ~prefix:"s: 1 test\n" t);
  check_contains "the warning shows the summed duration"
    ~sub:"slow tests (1, over 1s):\n  1.2s  flaky\n" t;
  (* A slow test that also fails: one block and one warning — they report
     different things — and the summary counts the failure once. *)
  let slow_fail =
    Fixtures.result [ "boom" ]
      (Failure.Fail [ Failure.message "b" ])
      ~duration:2.0
  in
  let t =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r slow_fail;
        Report.finish r ~results:[ slow_fail ] ~duration:2.0 ())
  in
  check_contains "a slow failing test keeps its failure block"
    ~sub:(failures_rule ^ "\n  FAIL  boom\n")
    t;
  check_contains
    "the section follows the closing rule and a blank line, before the summary"
    ~sub:
      ("    b\n" ^ closing_rule
     ^ "\n\nslow tests (1, over 1s):\n  2.0s  boom\n\n1 failed in 2.0s.\n")
    t;
  check_contains "the failure is counted once" ~sub:"\n1 failed in 2.0s.\n" t;
  let occurrences ~sub s =
    let n = String.length sub in
    let rec go i acc =
      if i + n > String.length s then acc
      else if String.sub s i n = sub then go (i + 1) (acc + 1)
      else go (i + 1) acc
    in
    go 0 0
  in
  check "exactly one warning line for the slow failure"
    (occurrences ~sub:"  2.0s  boom" t = 1)

let test_slow_threshold_zero () =
  let slow_pass = Fixtures.result [ "t" ] Failure.Pass ~duration:5.0 in
  let t =
    with_renderer ~slow_threshold:0.0 (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r slow_pass;
        Report.finish r ~results:[ slow_pass ] ~duration:5.0 ())
  in
  check_string "threshold 0 disables the trigger and the warnings"
    ~expected:"s: 1 passed in 5.0s.\n" ~actual:t;
  let still_noteworthy =
    let fail = Fixtures.result [ "t" ] (Failure.Fail [ Failure.message "x" ]) in
    with_renderer ~slow_threshold:0.0 (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r fail;
        Report.finish r ~results:[ fail ] ~duration:0.1 ())
  in
  check "threshold 0 still makes a counted failure noteworthy"
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
        Report.finish r ~results:[ slow_pass ] ~duration:1.5 ())
  in
  check_contains "verbose: header and status line stream as always"
    ~sub:"s: 1 test\n  PASS  t" t;
  check_contains "verbose: the slow section before the summary"
    ~sub:"\nslow tests (1, over 1s):\n  1.5s  t\n\n" t;
  check "verbose: the summary is the last line"
    (String.ends_with ~suffix:"  1.5s  t\n\n1 passed in 1.5s.\n" t);
  let tagged_pass = { slow_pass with Run.slow_tagged = true } in
  let tagged =
    with_renderer ~mode:`Verbose (fun r ->
        Report.result r tagged_pass;
        Report.finish r ~results:[ tagged_pass ] ~duration:1.5 ())
  in
  check_absent "verbose: slow-tagged tests warn nowhere" ~sub:"slow tests ("
    tagged

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
        Report.finish r ~results:[ steady; flaky ] ~duration:0.3 ())
  in
  check_string "a flaky pass is noteworthy: header, section, summary term"
    ~expected:
      "s: 2 tests\n\
       flaky tests (1):\n\
      \  passed on attempt 2  network › fetches the manifest\n\n\
       2 passed (1 flaky) in 300ms.\n"
    ~actual:t;
  (* Between the slow block and the summary, after the failure section. *)
  let slow = Fixtures.result [ "slow one" ] Failure.Pass ~duration:1.5 in
  let bad = Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "b" ]) in
  let ordered =
    with_renderer (fun r ->
        Report.finish r ~results:[ bad; slow; flaky ] ~duration:2.0 ())
  in
  check_contains "the flaky section follows the slow section"
    ~sub:
      "slow tests (1, over 1s):\n\
      \  1.5s  slow one\n\n\
       flaky tests (1):\n\
      \  passed on attempt 2  network › fetches the manifest\n\n\
       2 passed (1 flaky), 1 failed in 2.0s.\n"
    ordered;
  check "the failure section precedes it, closed by its rule"
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
        Report.finish r ~results:[ hopeless; steady ] ~duration:0.1 ())
  in
  check_absent "a retried failure is not flaky" ~sub:"flaky tests" not_flaky;
  check_contains "a retried failure keeps its attempt count in the block"
    ~sub:"  FAIL  hopeless (3 attempts)" not_flaky;
  (* Verbose keeps the block, and its status line already carried the
     count. *)
  let verbose =
    with_renderer ~mode:`Verbose (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r flaky;
        Report.finish r ~results:[ flaky ] ~duration:0.3 ())
  in
  check_contains "verbose: the status line carries the count"
    ~sub:"  PASS  network › fetches the manifest" verbose;
  check_contains "verbose: the status line names the attempts"
    ~sub:"(2 attempts)" verbose;
  check_contains "verbose: the block prints too, one blank line after the rows"
    ~sub:
      "0.2ms (2 attempts)\n\n\
       flaky tests (1):\n\
      \  passed on attempt 2  network › fetches the manifest\n\n\
       1 passed (1 flaky) in 300ms.\n"
    verbose;
  let colored =
    with_renderer ~ansi:true (fun r ->
        Report.finish r ~results:[ flaky ] ~duration:0.3 ())
  in
  check_contains "ansi: the flaky section wears the slow section's caution"
    ~sub:"\027[33mflaky tests (1):\027[0m\n" colored;
  check_contains "ansi: the summary's flaky term is caution, beside the pass"
    ~sub:"\027[32m1 passed\027[0m \027[33m(1 flaky)\027[0m in 300ms." colored

(* Failure projections *)

let test_headline () =
  let h f = Report.headline f in
  check "headline: equality"
    (h (Failure.equality ~expected:"true" ~actual:"false" ())
    = "expected true, got false");
  check "headline: negated equality"
    (h (Failure.equality ~not_:true ~expected:"3" ~actual:"3" ())
    = "both sides equal: 3");
  check "headline: raise both sides"
    (h (Failure.raised ~expected:"A" ~actual:"B" ())
    = "expected exception A, raised B");
  check "headline: raise nothing raised"
    (h (Failure.raised ~expected:"A" ()) = "expected exception A, none raised");
  check "headline: predicate miss"
    (h (Failure.raised ~actual:"B" ~predicate:true ())
    = "exception did not satisfy the predicate: B");
  check "headline: a predicate that saw nothing raised"
    (h (Failure.raised ~predicate:true ())
    = "expected an exception, none raised");
  check "headline: uncaught exception"
    (h (Failure.raised ~actual:"Not_found" ()) = "uncaught exception: Not_found");
  check "headline: raise wanted any"
    (h (Failure.raised ()) = "expected an exception, none raised");
  check "headline: a predicate's claim is the expected side"
    (h (Failure.predicate ~claim:"a power of two" "12")
    = "expected a power of two, got 12");
  check "headline: equal renderings"
    (h (Failure.equality ~expected:"nan" ~actual:"nan" ())
    = "both sides render as: nan");
  check "headline: a diff is a sentence counting the lines the block prints"
    (h (Failure.equality ~expected:"a\nb\nc" ~actual:"a\nB\nc" ())
    = "expected and actual differ (5 diff lines)");
  check "headline: a newline-only difference is the block's sentence"
    (h (Failure.equality ~expected:"a\nb" ~actual:"a\nb\n" ())
    = "values differ only by a trailing newline (on the actual side)");
  check "headline: file baseline missing"
    (h Fixtures.snap_missing = {|expect_file "test/help.expected": no baseline|});
  check "headline: literal mismatch"
    (h Fixtures.snap_mismatch = "expect: mismatch");
  check "headline: unresolvable"
    (h
       (Failure.baseline (Failure.File "../x")
          (Failure.Unresolvable { candidate = "/tmp/x" }))
    = {|expect_file "../x": cannot resolve the path under the project root|});
  check "headline: property"
    (h Fixtures.prop_failure
   = "property failed (case 12, shrunk 4 steps): Rect (2, 0)");
  let one_step =
    Failure.property ~rendered:"0" ~case_index:0 ~shrink_steps:1
      ~root:Fixtures.root ~examples:false ()
  in
  check "headline: one shrink step is singular"
    (h one_step = "property failed (case 0, shrunk 1 step): 0");
  check_contains "and so it is in the block"
    ~sub:"    counterexample (case 0, shrunk 1 step): 0\n"
    (failure_block one_step);
  check "headline: a pre-image is marked as the block marks it"
    (h
       (Failure.property ~rendered:"20" ~case_index:0 ~shrink_steps:0
          ~root:Fixtures.root ~examples:false ~rendering:Failure.Pre_image ())
    = "property failed (case 0): computed from 20");
  check "headline: a summarized counterexample is its summary, not its table"
    (h
       (Failure.property ~summary:"2 calls, last: get"
          ~rendered:" #  call\n 1  inc\n 2  get" ~case_index:0 ~shrink_steps:2
          ~root:Fixtures.root ~examples:false ())
    = "property failed (case 0, shrunk 2 steps): 2 calls, last: get");
  check "headline: message" (h (Failure.message "boom") = "boom");
  check "headline: the user message leads, then a colon"
    (h
       {
         (Failure.equality ~expected:"1" ~actual:"2" ()) with
         Failure.msg = Some "deliberate";
       }
    = "deliberate: expected 1, got 2");
  check_contains "headline: msg annotation prefixed" ~sub:"context: boom"
    (h { (Failure.message "boom") with Failure.msg = Some "context" });
  (* Two failing subtests of one test must not read the same. *)
  let labelled label =
    {
      (Failure.equality ~expected:"[1; 2]" ~actual:"[1; 3]" ()) with
      Failure.subtest = [ "contract"; label ];
    }
  in
  check "headline: the subtest label leads"
    (h (labelled "shape [0]")
    = "contract \u{203a} shape [0]: expected [1; 2], got [1; 3]");
  check "headline: sibling subtests differ by their label"
    (h (labelled "shape [0]") <> h (labelled "shape [2]"));
  check "headline: the label, the user message, then the sentence"
    (h { (labelled "shape [0]") with Failure.msg = Some "deliberate" }
    = "contract \u{203a} shape [0]: deliberate: expected [1; 2], got [1; 3]");
  (* 80 code points, not bytes, then the ellipsis. *)
  let long = String.concat "" (List.init 300 (fun _ -> "\u{00e9}")) in
  let hl = h (Failure.equality ~expected:long ~actual:"y" ()) in
  check "headline: cut at 80 code points, then an ellipsis"
    (hl
    = "expected "
      ^ String.concat "" (List.init 71 (fun _ -> "\u{00e9}"))
      ^ "\u{2026}");
  check_absent "headline: no em dash" ~sub:"\u{2014}"
    (h { (Failure.message "boom") with Failure.msg = Some "context" });
  let multi = h (Failure.message "line one\nline two") in
  check "headline: never multi-line" (not (String.contains multi '\n'));
  let esc = h (Failure.message "\027[31mred\027[0m alert") in
  check "headline: payload escapes stripped" (esc = "red alert");
  check "headline: empty message named"
    (h (Failure.message "") = "(empty failure message)");
  check_contains "block: empty message named" ~sub:"(empty failure message)"
    (failure_block (Failure.message ""))

let test_property_projections () =
  let example =
    Failure.property ~rendered:"Rect (2, 0)" ~case_index:0 ~shrink_steps:0
      ~root:Fixtures.root ~examples:true ()
  in
  let b = failure_block example in
  check_contains "example: numbered from one"
    ~sub:"counterexample (example 1): Rect (2, 0)" b;
  check_absent "example: no replay line (examples always replay)" ~sub:"replay:"
    b;
  check_absent "example: no seed token" ~sub:"WINDTRAP_SEED" b;
  (* A pre-image is marked in the slot, [computed from], and explained in
     the aside under it; a printing generator draws no aside at all. *)
  let pre_image =
    Failure.property ~rendered:"[2; 3] -> ([1.; 2.], [0.; 0.])" ~case_index:19
      ~shrink_steps:9 ~root:Fixtures.root ~examples:false
      ~rendering:Failure.Pre_image ()
  in
  check_contains "pre-image: marked in the slot"
    ~sub:
      "counterexample (case 19, shrunk 9 steps): computed from [2; 3] -> ([1.; \
       2.], [0.; 0.])"
    (failure_block pre_image);
  check_contains
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
  check_contains "pre-image: the aside is faint, line by line"
    ~sub:
      "\n\
      \      \027[2m(the value has no printer, so this is the input that map \
       and bind\027[0m\n\
      \      \027[2m computed it from; attach a printer with Gen.with_pp to \
       see the value)\027[0m\n"
    (failure_block ~ansi:true pre_image);
  check_absent "pre-image: no em dash in the explanation" ~sub:"\u{2014}"
    (failure_block pre_image);
  check_absent "a printing generator draws no note" ~sub:"no printer"
    (failure_block Fixtures.prop_failure);
  check_absent "nor the remedy" ~sub:"Gen.with_pp"
    (failure_block Fixtures.prop_failure);
  let multi_pre_image =
    failure_block
      (Failure.property ~rendered:"1 ->\n  [2; 3]" ~case_index:3 ~shrink_steps:0
         ~root:Fixtures.root ~examples:false ~rendering:Failure.Pre_image ())
  in
  check_contains "multi-line pre-image: marked head, block form"
    ~sub:
      "    counterexample (case 3): computed from\n\
      \      1 ->\n\
      \        [2; 3]\n\
      \      (the value has no printer, so this is the input that map and bind\n\
      \       computed it from; attach a printer with Gen.with_pp to see the \
       value)\n"
    multi_pre_image;
  let no_filter = failure_block Fixtures.prop_failure in
  check_contains "replay without filter: seed only"
    ~sub:"replay: WINDTRAP_SEED=s1:7be1d2c904aa31f5 dune runtest" no_filter;
  let quoted = failure_block ~filter:"it's › tricky" Fixtures.prop_failure in
  check_contains "replay filter is shell-quoted"
    ~sub:{|WINDTRAP_FILTER='it'\''s › tricky'|} quoted;
  (* A config-sourced count rides the payload and the replay line restates
     it — replaying a late case needs at least as many cases as the failing
     run. A payload without a count (the declaration-site form) is pinned
     flagless just above. *)
  let counted =
    Failure.property ~count:1000 ~rendered:"0" ~case_index:499 ~shrink_steps:1
      ~root:Fixtures.root ~examples:false ()
  in
  check_contains "config-sourced count: Mirrors replay restates the mirror"
    ~sub:
      "replay: WINDTRAP_SEED=s1:7be1d2c904aa31f5 WINDTRAP_PROP_COUNT=1000 dune \
       runtest"
    (failure_block counted);
  check_contains "config-sourced count: Exe replay restates --prop-count"
    ~sub:
      "replay: ./t.exe --seed s1:7be1d2c904aa31f5 --prop-count 1000 -f 'late'"
    (failure_block ~invocation:(`Exe "./t.exe") ~filter:"late" counted);
  (* The seed and the count are the whole replay line: the shrink budget
     is fixed, so no clause restates it, even for a search that spent it. *)
  let spent =
    Failure.property ~count:1000 ~rendered:"0" ~case_index:499
      ~shrink_steps:10_000 ~shrink_exhausted:true ~root:Fixtures.root
      ~examples:false ()
  in
  check_contains "a spent budget: the Mirrors replay line ends at the count"
    ~sub:
      "replay: WINDTRAP_SEED=s1:7be1d2c904aa31f5 WINDTRAP_PROP_COUNT=1000 dune \
       runtest\n"
    (failure_block spent);
  check_contains "a spent budget: the Exe replay line ends at the filter"
    ~sub:
      "replay: ./t.exe --seed s1:7be1d2c904aa31f5 --prop-count 1000 -f 'late'\n"
    (failure_block ~invocation:(`Exe "./t.exe") ~filter:"late" spent);
  let multi =
    failure_block
      (Failure.property ~rendered:"Rect\n  (2, 0)" ~case_index:3 ~shrink_steps:0
         ~root:Fixtures.root ~examples:false ())
  in
  check_contains "multi-line counterexample: block form"
    ~sub:"counterexample (case 3):\n      Rect\n        (2, 0)" multi;
  (* A summarized counterexample is a table: the summary on the head line,
     the table under it, its header row faint. *)
  let program =
    Failure.property ~summary:"2 calls, last: get"
      ~rendered:
        " #  model before  call\n 1  0             inc 3\n 2  3             get"
      ~case_index:0 ~shrink_steps:2 ~root:Fixtures.root ~examples:false ()
  in
  check_contains "a summarized counterexample: summary on the head, then rows"
    ~sub:
      "    counterexample (case 0, shrunk 2 steps): 2 calls, last: get\n\
      \       #  model before  call\n\
      \       1  0             inc 3\n\
      \       2  3             get\n\
      \    replay: "
    (failure_block program);
  check_contains "a summarized counterexample: only the header row is faint"
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
  check_contains "a phase with nothing located is its tag, alone on the line"
    ~sub:"    [teardown]\n    x\n" b;
  let b =
    failure_block
      (Failure.equality ~msg:"context note" ~expected:"1" ~actual:"2" ())
  in
  check_contains "msg annotation printed" ~sub:"    context note\n" b;
  let b =
    failure_block
      (Failure.baseline (Failure.File "../n.expected")
         (Failure.Unresolvable { candidate = "some/candidate" }))
  in
  check_string
    "unresolvable: the subject and the rule, then the candidate path and the \
     remedy naming the variable, and no command after it"
    ~expected:
      "    expect_file \"../n.expected\": the path cannot be proven to lie \
       under the project root\n\
      \    unverified path: some/candidate\n\
      \    (set WINDTRAP_PROJECT_ROOT to the directory the path is relative to)\n"
    ~actual:b;
  check_absent
    "unresolvable: never a claim about where the path lies, which can be false"
    ~sub:"outside the project root" b;
  (* What the block composes on that line fits the width; the path may not. *)
  check "unresolvable: the composed text of the remedy line fits 80 columns"
    (List.exists
       (fun line ->
         has ~sub:"unverified path:" line
         && String.length line - String.length "some/candidate" <= 80)
       (String.split_on_char '\n' b));
  check_absent "unresolvable: no acceptance line" ~sub:"accept:" b;
  check_contains "unresolvable: the remedy is a line of its own"
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
  check_contains "unresolvable literal: the subject is expect"
    ~sub:"    expect: the path cannot be proven to lie under the project root\n"
    b;
  (* The first fact line names the verb that read the literal. *)
  let verb exact =
    failure_block
      (Failure.baseline
         (Failure.Literal { exact })
         (Failure.Mismatch { expected = "a\n"; actual = "b\n" }))
  in
  check_contains "expect: the flexible verb, then the hunks with no head"
    ~sub:"    expect: mismatch\n    @@ -1,1 +1,1 @@\n    - a\n    + b\n"
    (verb false);
  check_contains "expect_exact: the exact verb, then the hunks with no head"
    ~sub:"    expect_exact: mismatch\n    @@ -1,1 +1,1 @@\n    - a\n    + b\n"
    (verb true);
  check_absent "a correction has no ---/+++ head" ~sub:"--- expected"
    (verb true);
  (* A line diff cannot show a trailing newline, the one difference that
     leaves [expect_exact] no hunk: the block says it in words. *)
  check_contains "expect_exact: a newline-only difference is said in words"
    ~sub:
      "    expect_exact: mismatch\n\
      \    values differ only by a trailing newline (on the actual side)\n\
      \    accept: "
    (failure_block
       (Failure.baseline
          (Failure.Literal { exact = true })
          (Failure.Mismatch { expected = "exact"; actual = "exact\n" })));
  check "the headline is that first fact line"
    (Report.headline
       (Failure.baseline
          (Failure.Literal { exact = true })
          (Failure.Mismatch { expected = "a\n"; actual = "b\n" }))
    = "expect_exact: mismatch");
  let b =
    failure_block (Failure.equality ~expected:"a\nb\nc" ~actual:"a\nB\nc" ())
  in
  check_contains "multi-line equality: unified diff header"
    ~sub:"--- expected\n    +++ actual\n" b;
  check_contains "multi-line equality: hunk" ~sub:"@@ -1,3 +1,3 @@" b;
  check_contains "multi-line equality: delete line" ~sub:"- b" b;
  check_contains "multi-line equality: insert line" ~sub:"+ B" b

let test_degenerate_equalities () =
  (* Renderings line-equal but byte-different: the only such difference is a
     trailing newline, which a line diff cannot show — say so instead of
     printing an empty diff. *)
  let b =
    failure_block (Failure.equality ~expected:"a\nb" ~actual:"a\nb\n" ())
  in
  check_contains "trailing-newline-only difference is stated, its side named"
    ~sub:"    values differ only by a trailing newline (on the actual side)\n" b;
  check_contains "the other side is named when it holds the newline"
    ~sub:"    values differ only by a trailing newline (on the expected side)\n"
    (failure_block (Failure.equality ~expected:"a\nb\n" ~actual:"a\nb" ()));
  check_absent "trailing-newline case prints no empty diff" ~sub:"--- expected"
    b;
  (* Renderings byte-equal while the equality distinguishes (lossy pp, e.g.
     [equal float nan nan]): two identical lines need an explanation. *)
  let b = failure_block (Failure.equality ~expected:"nan" ~actual:"nan" ()) in
  check_contains "identical renderings print once, then say why"
    ~sub:
      "    both sides render as: nan\n\
      \    the printer shows less than the equality compares\n"
    b;
  check_absent "identical renderings print no expected/actual pair"
    ~sub:"expected" b;
  (* Identical and multi-line: printed once, in block form — inlining after
     an [expected] label would put continuation lines at column zero. *)
  let b =
    failure_block
      (Failure.equality ~expected:"line a\nline b" ~actual:"line a\nline b" ())
  in
  check_contains "identical multi-line renderings print once, indented"
    ~sub:"    both sides render as:\n      line a\n      line b\n" b;
  check_contains "identical multi-line explanation retained"
    ~sub:"      line b\n    the printer shows less than the equality compares\n"
    b;
  check_absent "identical multi-line has no column-zero payload line"
    ~sub:"\nline b" b

let test_ansi_hygiene () =
  (* User pp output may carry raw escapes; under [ansi:false] the transcript
     must contain none (render.mli), under [ansi:true] they pass through.

     The two ways it contains none are not the same. A comparison surface
     escapes them, keeping every byte the value had — the block below is
     [test_control_bytes_refined]'s guarantee seen from the hygiene side, so
     it pins the escaped bytes rather than the stripped remains. The
     surfaces that print verbatim — a message, a test name, a captured
     tail — are stripped at the sink, as they always were. *)
  let esc = "\027[31mred\027[0m" in
  let f =
    Failure.equality ~expected:(esc ^ " one") ~actual:"\027]0;title\007 two" ()
  in
  let plain = failure_block f in
  check_absent "ansi:false: payload escapes stripped from blocks" ~sub:"\027"
    plain;
  check_contains "ansi:false: the payload's own bytes survive, escaped"
    ~sub:{|\x1b[31mred\x1b[0m one|} plain;
  check_contains "ansi:false: an OSC payload survives the same way"
    ~sub:{|\x1b]0;title\x07 two|} plain;
  let colored = failure_block ~ansi:true (Failure.message (esc ^ " boom")) in
  check_contains "ansi:true: payload escapes pass through" ~sub:esc colored;
  let hostile_line =
    with_renderer ~mode:`Verbose (fun r ->
        Report.result r
          (Fixtures.result
             [ "suite"; esc ^ " name" ]
             (Failure.Fail [ Failure.message "boom" ])))
  in
  check_absent "ansi:false: test-line names stripped" ~sub:"\027" hostile_line;
  let hostile_tail =
    let tail = Failure.tail ~log_path:"log" (esc ^ " captured\n") in
    let result =
      Fixtures.result [ "t" ]
        (Failure.Fail [ Failure.with_output_tail tail (Failure.message "boom") ])
    in
    with_renderer (fun r ->
        Report.finish r ~results:[ result ] ~duration:0.01 ())
  in
  check_absent "ansi:false: captured tail stripped" ~sub:"\027" hostile_tail;
  check_contains "ansi:false: stripped tail text survives" ~sub:" captured"
    hostile_tail

(* Control bytes on comparison surfaces

   A value carrying ESC drove the terminal instead of appearing in the
   report, and a grep for the reported bytes found nothing. Comparison
   surfaces escape C0 and DEL at render time; comparison and storage stay
   byte-raw. The three surfaces the escape has to reach are the short-value
   refinement path, the multi-line hunk path, and the containment excerpt —
   and on all three the marks are computed against the raw value and drawn
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
  check_absent "refined: no raw ESC reaches a plain block" ~sub:"\027" b;
  check_contains "refined: both sides are escaped, a mark under each"
    ~sub:
      ("    expected  \\x1b[31mred\\x1b[0m\n" ^ String.make 20 ' '
     ^ "~\n    actual    \\x1b[32mred\\x1b[0m\n" ^ String.make 20 ' ' ^ "~\n")
    b;
  check "refined: one mark line per changed side" (occurrences_of ~sub:"~" b = 2);
  (* A marked region that IS a control byte: the mark covers all four
     columns of the escape, not the one the raw span measured. *)
  let widened =
    failure_block
      (Failure.equality ~expected:"plain text here" ~actual:"plain\027text here"
         ())
  in
  check_contains "refined: a marked control byte widens its mark"
    ~sub:("    actual    plain\\x1btext here\n" ^ String.make 19 ' ' ^ "~~~~\n")
    widened;
  check_contains
    "refined: the expected side of a replacement carries its own mark"
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
  check_contains "refined: a deletion is marked under expected, widened"
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
  check_absent "refined: the payload's own sequence never runs"
    ~sub:"\027[31mred" colored;
  check_contains
    "refined: the changed span is bold in its side's colour, inside the \
     escaped value"
    ~sub:
      "\027[2mexpected\027[0m  \\x1b[3\027[1;32m1\027[0mmred\\x1b[0m\n\
      \    \027[2mactual\027[0m    \\x1b[3\027[1;31m2\027[0mmred\\x1b[0m\n"
    colored;
  check_absent "refined: no mark prints under colour" ~sub:"~" colored

(* The mark prints only where it lands under what it marks: one [~] per
   code point, and none at all when a tab or a code point of no fixed width
   sits on either side. *)
let test_mark_criterion () =
  let block expected actual =
    failure_block (Failure.equality ~expected ~actual ())
  in
  check_contains "the mark counts code points, not bytes"
    ~sub:
      ("    actual    the caf\u{00E9} is brawn today\n" ^ String.make 28 ' '
     ^ "~\n")
    (block "the caf\u{00E9} is brown today" "the caf\u{00E9} is brawn today");
  let tabbed = block "the\tquick brown fox" "the\tquick brawn fox" in
  check_absent "a tab on a side: no mark" ~sub:"~" tabbed;
  check_contains "a tab on a side: both values still print"
    ~sub:
      "    expected  the\tquick brown fox\n    actual    the\tquick brawn fox\n"
    tabbed;
  check_absent "a code point past U+024F on a side: no mark" ~sub:"~"
    (block "\u{65E5}\u{672C} quick brown fox" "\u{65E5}\u{672C} quick brawn fox");
  check_absent "malformed UTF-8 on a side: no mark" ~sub:"~"
    (block "\xff quick brown fox" "\xff quick brawn fox");
  check_absent "a combining accent on a side: no mark" ~sub:"~"
    (block "cafe\u{0301} quick brown fox" "cafe\u{0301} quick brawn fox");
  check_absent "an emoji on a side: no mark" ~sub:"~"
    (block "\u{1F642} quick brown fox" "\u{1F642} quick brawn fox");
  check_absent "refinement declines on short values: no mark" ~sub:"~"
    (block "ab" "cd");
  (* A span at either end of the value, and a change that only inserted. *)
  check_string "a span at the very start, marked under each side"
    ~expected:
      "    expected  Xbcdefghij\n\
      \              ~\n\
      \    actual    Ybcdefghij\n\
      \              ~\n"
    ~actual:(block "Xbcdefghij" "Ybcdefghij");
  check_string "a span at the very end, marked under each side"
    ~expected:
      "    expected  abcdefghiX\n\
      \                       ~\n\
      \    actual    abcdefghiY\n\
      \                       ~\n"
    ~actual:(block "abcdefghiX" "abcdefghiY");
  check_string "a pure insertion is marked under actual only"
    ~expected:
      "    expected  user:alice\n\
      \    actual    user:alice:admin\n\
      \                        ~~~~~~\n"
    ~actual:(block "user:alice" "user:alice:admin");
  check_string "a pure deletion is marked under expected only"
    ~expected:
      "    expected  user:alice:admin\n\
      \                        ~~~~~~\n\
      \    actual    user:alice\n"
    ~actual:(block "user:alice:admin" "user:alice");
  (* Colour replaces the [~] lines and nothing else: stripped of its
     styling, the coloured block is the plain one without them. *)
  List.iter
    (fun (expected, actual) ->
      let f = Failure.equality ~expected ~actual () in
      check_string
        "the coloured block, stripped, is the plain block without its marks"
        ~expected:(without_marks (failure_block f))
        ~actual:(Text.strip_ansi (failure_block ~ansi:true f)))
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
  check_absent "hunks: no raw ESC reaches a plain block" ~sub:"\027" b;
  check_contains "hunks: both changed lines are escaped"
    ~sub:"    - \\x1b[31malert\\x1b[0m\n    + \\x1b[32malert\\x1b[0m\n" b;
  check_contains "hunks: context lines are escaped too" ~sub:"      header\n" b;
  (* A carriage return no longer eats the line it shares. *)
  let cr =
    failure_block
      (Failure.equality ~expected:"one\ntwo\r\nthree" ~actual:"one\ntwo\nthree"
         ())
  in
  check_contains "hunks: CR renders as its hex escape" ~sub:"- two\\x0d\n" cr;
  check_absent "hunks: no raw CR survives" ~sub:"two\r" cr;
  (* Baseline mismatches share [pp_hunks] — the single producer. *)
  let snap =
    failure_block
      (Failure.baseline
         (Failure.Literal { exact = false })
         (Failure.Mismatch
            { expected = "\027[1mbold\027[0m\n"; actual = "bold\n" }))
  in
  check_contains "hunks: baselines escape as well"
    ~sub:"- \\x1b[1mbold\\x1b[0m\n" snap

let test_control_bytes_containment () =
  (* The excerpt path: the occurrence marker is computed from the payload's
     raw byte offset and drawn in display columns. *)
  let f =
    caught "containment" (fun () ->
        Check.contains ~sub:"NOPE" "\027[31mred\027[0m text")
  in
  let b = failure_block f in
  check_absent "containment: no raw ESC reaches a plain block" ~sub:"\027" b;
  check_contains "containment: the excerpt is escaped"
    ~sub:"    haystack  \\x1b[31mred\\x1b[0m text\n" b;
  (* not_contains: the needle occurs, and its mark must land under the
     escaped occurrence rather than at its raw offset. *)
  let found =
    caught "not_contains" (fun () ->
        Check.not_contains ~sub:"red" "\027[31mred\027[0m text")
  in
  let b = failure_block found in
  check_contains "containment: the needle keeps its own %S escapes"
    ~sub:"    needle    \"red\": found at byte 5\n" b;
  check_contains "containment: the mark sits under the escaped occurrence"
    ~sub:
      ("    haystack  \\x1b[31mred\\x1b[0m text\n" ^ String.make 22 ' '
     ^ "~~~\n")
    b;
  let colored = failure_block ~ansi:true found in
  check_absent "containment: the excerpt's own sequence never runs"
    ~sub:"\027[31mred\027[0m text" colored;
  check_contains
    "containment: under colour the occurrence is bold red in the plain \
     excerpt, and no mark prints"
    ~sub:
      "    \027[2mhaystack\027[0m  \\x1b[31m\027[1;31mred\027[0m\\x1b[0m text\n"
    colored;
  check_absent "containment: no mark under colour" ~sub:"~" colored;
  check
    "containment: the coloured bytes, stripped, are the plain ones without the \
     mark"
    (Text.strip_ansi colored = without_marks b)

let test_control_bytes_alphabet () =
  (* One rule: every C0 byte and DEL as [\xNN], LF and TAB excepted because
     the block's layout is made of them. *)
  let b =
    failure_block
      (Failure.predicate ~claim:"a clean value" "a\x00b\x07c\rd\x7fe\x1ff")
  in
  check_contains "alphabet: NUL, BEL, CR, DEL and US all escape"
    ~sub:"    actual    a\\x00b\\x07c\\x0dd\\x7fe\\x1ff\n" b;
  let kept =
    failure_block (Failure.predicate ~claim:"a clean value" "one\ttwo\nthree")
  in
  check_contains "alphabet: TAB survives inside a line" ~sub:"      one\ttwo\n"
    kept;
  check_contains "alphabet: LF still breaks the block into lines"
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
      check_string "payloads store the raw bytes" ~expected:{|"\027"|}
        ~actual:expected;
      check_string "and the other side's own bytes" ~expected:{|"\\x1b"|}
        ~actual
  | _ -> fail "expected an Equality payload");
  (* Two values a lossy printer merges are reported as such; two the
     ESCAPING would merge are not, because the test is made on the raw
     renderings. [Testable.text] renders both verbatim. *)
  let merged =
    caught "escaping is not a printer" (fun () ->
        Check.equal Testable.text "\027" "\\x1b")
  in
  check_absent "an escape collision is never called an identical rendering"
    ~sub:"render identically" (failure_block merged)

let test_diff_truncation () =
  let text prefix =
    String.concat "\n" (List.init 300 (fun i -> Printf.sprintf "%s%d" prefix i))
  in
  let b =
    failure_block (Failure.equality ~expected:(text "e") ~actual:(text "a") ())
  in
  check_contains "huge diffs end in a truncation mark" ~sub:"more diff lines)" b;
  check_absent "huge diffs are display-bounded" ~sub:"+ a299" b;
  let snap =
    failure_block
      (Failure.baseline (Failure.File "p.expected")
         (Failure.Mismatch
            { expected = text "e" ^ "\n"; actual = text "a" ^ "\n" }))
  in
  check_contains "baseline diff truncation mark" ~sub:"more diff lines)" snap;
  check_contains "acceptance survives a truncated diff"
    ~sub:"accept: dune promote" snap

let test_proposed_truncation () =
  let proposed =
    String.concat "" (List.init 25 (fun i -> Printf.sprintf "line %d\n" i))
  in
  let b =
    failure_block
      (Failure.baseline (Failure.File "p.expected")
         (Failure.Missing { proposed }))
  in
  (* A missing baseline has no file to diff against: its proposed text
     prints under a heading that counts it, as [+] lines, 20 at most. *)
  check_contains "a missing file: the fact line, the heading, then + lines"
    ~sub:
      "    expect_file \"p.expected\": no baseline\n\
      \    proposed (25 lines):\n\
      \      + line 0\n\
      \      + line 1\n"
    b;
  check_contains "proposed content bounded with a mark" ~sub:"(+5 more lines)" b;
  check_contains
    "a missing file: 20 lines indented two, the cap line with them, then the \
     command at the block's column"
    ~sub:"      + line 19\n      \u{2026} (+5 more lines)\n    accept: " b;
  check_absent "a missing file: no line over the bound" ~sub:"line 20" b;
  check_absent "proposed lines over the bound absent" ~sub:"line 24" b;
  check_absent "a missing file: no hunk header for a file that does not exist"
    ~sub:"@@" b;
  check_absent "a missing file: no second gutter" ~sub:"\u{2506}" b;
  let short =
    failure_block
      (Failure.baseline (Failure.File "p.expected")
         (Failure.Missing { proposed = "only\n" }))
  in
  check_contains "a missing file under the cap prints whole, uncapped"
    ~sub:"    proposed (1 line):\n      + only\n    accept: " short;
  check_contains "the + lines are red under colour, their indentation plain"
    ~sub:"    proposed (1 line):\n      \027[31m+ only\027[0m\n    accept: "
    (failure_block ~ansi:true
       (Failure.baseline (Failure.File "p.expected")
          (Failure.Missing { proposed = "only\n" })));
  check_contains "acceptance survives a bounded proposal"
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
  check_string
    "excerpt: the located source line sits under the bare location, its \
     leading whitespace removed, one blank line after it"
    ~expected:
      (Printf.sprintf
         "    %s:2\n      2 │ let two = 2\n\n    expected  1\n    actual    2\n"
         file)
    ~actual:(failure_block ~excerpt:true f);
  check_contains "excerpt: the gutter is faint, the source plain"
    ~sub:":2\027[0m\n      \027[2m2 │\027[0m let two = 2\n\n"
    (failure_block ~ansi:true ~excerpt:true f);
  check_absent "excerpt: off by default" ~sub:"let two" (failure_block f);
  (* Under every location: a property's own, whose inner assertion keeps
     its line too. *)
  let law =
    failure_block ~excerpt:true
      (Failure.property
         ~loc:{ Loc.file; line = 1; column = 0 }
         ~inner:f ~rendered:"0" ~case_index:0 ~shrink_steps:0
         ~root:Fixtures.root ~examples:false ())
  in
  check_contains
    "excerpt: under a property's own location, one blank line after it"
    ~sub:
      (Printf.sprintf "    %s:1\n      1 │ let one = 1\n\n    counterexample "
         file)
    law;
  check_contains
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
      check_contains
        (Printf.sprintf
           "excerpt: a source line's control bytes are escaped (ansi:%b)" ansi)
        ~sub:{| let red = "\x1b[31mred\x1b[0m" (* \x0d \x07 *)|}
        (failure_block ~ansi ~excerpt:true (at 1)))
    [ false; true ];
  check_contains "excerpt: a CRLF file's line ends where its text does"
    ~sub:"\\x07 *)\n\n"
    (failure_block ~excerpt:true (at 1));
  check_contains "excerpt: a source line over 800 bytes is elided as a value is"
    ~sub:"xxxx\u{2026} (100 bytes elided)xxxx"
    (failure_block ~excerpt:true (at 2));
  check_contains "excerpt: an empty source line is its gutter alone"
    ~sub:(Printf.sprintf "    %s:3\n      3 \u{2502}\n\n    expected" wild)
    (failure_block ~excerpt:true (at 3));
  let gone =
    Failure.equality
      ~loc:{ Loc.file = "does_not_exist.ml"; line = 2; column = 0 }
      ~expected:"1" ~actual:"2" ()
  in
  check_string
    "excerpt: an unreadable file prints neither the line nor the blank line"
    ~expected:"    does_not_exist.ml:2\n    expected  1\n    actual    2\n"
    ~actual:(failure_block ~excerpt:true gone)

(* Captured tail *)

let tail_block tail =
  let result =
    Fixtures.result [ "t" ]
      (Failure.Fail [ Failure.with_output_tail tail (Failure.message "boom") ])
  in
  with_renderer (fun r -> Report.finish r ~results:[ result ] ~duration:0.01 ())

(* The tail is a fixed ten lines over the bytes the capture kept: not a
   knob, so a twelve-line tail shows its last ten. *)
let test_tail () =
  let twelve =
    Failure.tail
      (String.concat ""
         (List.init 12 (fun i -> Printf.sprintf "l%d\n" (i + 1))))
  in
  let b = tail_block twelve in
  check_contains
    "tail: line-bounded heading, undecorated, its lines indented two more"
    ~sub:"    captured output (last 10 of 12 lines):\n      l3\n" b;
  check_absent "tail: no rule glyph before the heading" ~sub:"\u{2500} captured"
    b;
  check_absent "tail: no rule glyph after the heading" ~sub:": \u{2500}" b;
  check_contains "tail: last lines shown" ~sub:"      l3\n      l4\n" b;
  check_contains "tail: through the last line, then the closing rule"
    ~sub:("      l12\n" ^ closing_rule ^ "\n")
    b;
  check_absent "tail: earlier lines dropped" ~sub:"l2\n" b;
  let full = Failure.tail ~log_path:"log.output" "only\n" in
  let b = tail_block full in
  check_contains
    "tail: complete output heading, then the log at the heading's column"
    ~sub:"    captured output (1 line):\n      only\n    full log: log.output\n"
    b;
  check_contains "tail: the heading and the log are faint, the lines plain"
    ~sub:
      "    \027[2mcaptured output (1 line):\027[0m\n\
      \      only\n\
      \    \027[2mfull log: log.output\027[0m\n"
    (with_renderer ~ansi:true (fun r ->
         Report.finish r
           ~results:
             [
               Fixtures.result [ "t" ]
                 (Failure.Fail
                    [ Failure.with_output_tail full (Failure.message "boom") ]);
             ]
           ~duration:0.01 ()));
  check_contains "tail: a whole tail of several lines counts them"
    ~sub:"    captured output (3 lines):\n      a\n"
    (tail_block (Failure.tail "a\nb\nc\n"));
  let dropped = Failure.tail ~omitted_bytes:512 "kept\n" in
  check_contains "tail: drop count reported"
    ~sub:"    captured output (last 1 line, 512 earlier bytes omitted):\n"
    (tail_block dropped);
  let many =
    Failure.tail ~omitted_bytes:9000
      (String.concat ""
         (List.init 12 (fun i -> Printf.sprintf "l%d\n" (i + 1))))
  in
  (* The count is of every byte before the first line shown: the 9000 the
     capture cut and the two kept lines the cap drops, [l1\n] and [l2\n]. *)
  check_contains "tail: the byte-cut head, at the cap, counts the dropped lines"
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
  check "ten frames, then the count of the rest, and the block ends there"
    (String.ends_with
       ~suffix:"    frame 9\n    frame 10\n    \u{2026} (+3 more frames)\n"
       (block 13));
  check_absent "the eleventh frame never prints" ~sub:"frame 11" (block 13);
  check "ten frames print whole, and the block ends on the tenth"
    (String.ends_with ~suffix:"    frame 9\n    frame 10\n" (block 10));
  check_absent "and draw no cap line" ~sub:"more frames" (block 10);
  check_contains "the cap line is dim, as the frames are"
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
  check_string "over 800 bytes: both ends around the count, cut on code points"
    ~expected:
      ("    expected  " ^ e 200 ^ "\u{2026} (4 bytes elided)" ^ e 199 ^ "x\n"
     ^ "    actual    " ^ e 200 ^ "\u{2026} (4 bytes elided)" ^ e 199 ^ "y\n")
    ~actual:b;
  check_absent "an elided side draws no mark" ~sub:"~" b;
  (* One side is enough to lose the mark: its columns are gone. *)
  check_absent "one elided side draws no mark either" ~sub:"~"
    (failure_block
       (Failure.equality ~expected:(String.make 900 'a' ^ "x") ~actual:"ax" ()));
  let whole =
    failure_block
      (Failure.equality ~expected:(e 399 ^ "ax") ~actual:(e 399 ^ "ay") ())
  in
  check_contains "800 bytes print whole"
    ~sub:("    expected  " ^ e 399 ^ "ax\n")
    whole;
  check_contains "and keep their mark"
    ~sub:("\n" ^ String.make (14 + 400) ' ' ^ "~\n")
    whole;
  check "one under each side"
    (occurrences_of ~sub:("\n" ^ String.make (14 + 400) ' ' ^ "~\n") whole = 2);
  (* The count is the carried value's: an escape is four columns of one
     byte. *)
  check_contains "the count is of the carried bytes, not the escaped ones"
    ~sub:
      ("    both sides equal: \\x01" ^ String.make 399 'a'
     ^ "\u{2026} (101 bytes elided)" ^ String.make 400 'a' ^ "\n")
    (failure_block
       (Failure.equality ~not_:true
          ~expected:("\001" ^ String.make 900 'a')
          ~actual:("\001" ^ String.make 900 'a')
          ()));
  check_contains "a counterexample is a value too"
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
  check_contains "a needle is elided in its carried bytes, inside its quotes"
    ~sub:
      ("    needle    \"" ^ nichi 133 ^ "\u{2026} (102 bytes elided)"
     ^ nichi 133 ^ "\": not found\n")
    (failure_block
       (Failure.containment ~claim:"c"
          ~needle:(String.concat "" (List.init 300 (fun _ -> "\u{65e5}")))
          ~haystack:"hay" ()));
  (* A multi-line value is lines, bounded as lines are. *)
  let lines = String.make 500 'a' ^ "\n" ^ String.make 500 'b' in
  check_absent "a multi-line value is not elided" ~sub:"bytes elided"
    (failure_block
       (Failure.equality ~not_:true ~expected:lines ~actual:lines ()))

(* Exception message diffs (amendment B1) *)

let test_raise_message_diff () =
  let b = failure_block Fixtures.raise_message_failure in
  check_contains "raise: constructor named once"
    ~sub:"    raised Invalid_argument with the wrong message:\n" b;
  check_contains
    "raise: messages compared as strings, a mark under each changed one"
    ~sub:
      ("    raised Invalid_argument with the wrong message:\n\
       \    expected  \"index 3 out of bounds\"\n" ^ String.make 21 ' '
     ^ "~\n    actual    \"index 4 out of bounds\"\n" ^ String.make 21 ' '
     ^ "~\n")
    b;
  check "raise: one mark line per changed side" (occurrences_of ~sub:"~" b = 2);
  check_absent "raise: constructor not repeated on both sides"
    ~sub:"expected exception" b;
  let colored = failure_block ~ansi:true Fixtures.raise_message_failure in
  check_contains
    "raise: under colour the changed span is bold in its side's colour"
    ~sub:
      "    \027[2mexpected\027[0m  \"index \027[1;32m3\027[0m out of bounds\"\n\
      \    \027[2mactual\027[0m    \"index \027[1;31m4\027[0m out of bounds\"\n"
    colored;
  check_absent "raise: and no mark prints" ~sub:"~" colored

let test_raise_message_diff_guards () =
  (* No recorded diff means both exceptions print, whatever their
     constructors: the option is the whole of the renderer's decision. *)
  let b = failure_block Fixtures.raise_failure in
  check_contains "raise: different constructors unchanged"
    ~sub:"expected exception" b;
  let no_diff =
    Failure.raised ~expected:{|Failure("boom")|} ~actual:{|Failure("boom!")|} ()
  in
  check_contains "raise: same constructor without a diff keeps the plain form"
    ~sub:
      "    expected exception  Failure(\"boom\")\n\
      \    raised              Failure(\"boom!\")\n"
    (failure_block no_diff);
  check_absent "raise: no mark without a recorded message diff" ~sub:"~"
    (failure_block no_diff);
  check_absent "raise: no message diff without the payload"
    ~sub:"with the wrong message" (failure_block no_diff);
  (* raises_match's payload still prints the raised exception, under the
     sentence that says what was wrong with it. *)
  let predicate_miss =
    Failure.raised ~actual:{|Invalid_argument("nope")|} ~predicate:true ()
  in
  check_contains "raises_match: actually-raised exception printed"
    ~sub:
      "    raised exception does not satisfy the predicate:\n\
      \      Invalid_argument(\"nope\")\n"
    (failure_block predicate_miss);
  check_absent "raises_match: the predicate is no fake exception"
    ~sub:"one the predicate accepts"
    (failure_block predicate_miss);
  check_contains "raises: nothing raised"
    ~sub:
      "    expected exception  Failure(\"boom\")\n\
      \    but no exception was raised\n"
    (failure_block (Failure.raised ~expected:{|Failure("boom")|} ()));
  check_absent "raises: nothing is no exception's name" ~sub:"nothing"
    (failure_block (Failure.raised ~expected:{|Failure("boom")|} ()));
  check_contains "raises_match: nothing raised"
    ~sub:"    expected an exception, but none was raised\n"
    (failure_block (Failure.raised ~predicate:true ()));
  check_contains "raises: a multi-line expectation that nothing met"
    ~sub:
      "    expected exception:\n\
      \      Parse_error(\n\
      \        line 3)\n\
      \    but no exception was raised\n"
    (failure_block (Failure.raised ~expected:"Parse_error(\n  line 3)" ()));
  (* A rendering that spans lines is a block under its anchor. *)
  check_contains "raise: a multi-line exception is a block under its anchor"
    ~sub:
      "    expected exception  Failure(\"boom\")\n\
      \    raised:\n\
      \      Parse_error(\n\
      \        line 3)\n"
    (failure_block
       (Failure.raised ~expected:{|Failure("boom")|}
          ~actual:"Parse_error(\n  line 3)" ()))

(* Expected failures (amendment B12) *)

let test_xfail_line () =
  let line =
    with_renderer ~mode:`Verbose (fun r ->
        Report.result r Fixtures.excused_result)
  in
  check_contains "xfail line: XFAIL tag and reason"
    ~sub:"  XFAIL  known › broken carry (expected failure: issue #42)" line;
  check_absent "xfail line: not a FAIL" ~sub:"  FAIL  " line;
  let no_reason =
    with_renderer ~mode:`Verbose (fun r ->
        Report.result r
          {
            Fixtures.excused_result with
            Run.xfail = Some { Test_tree.reason = None };
          })
  in
  check_contains "xfail line: reasonless form" ~sub:"(expected failure)"
    no_reason;
  let pass_ignores =
    with_renderer ~mode:`Verbose (fun r ->
        Report.result r
          (Fixtures.result [ "t" ] Failure.Pass ~xfail:Fixtures.xfail_reason))
  in
  check_contains "an xfail annotation on a pass changes nothing" ~sub:"PASS"
    pass_ignores

let test_excused_collision () =
  (* The F4 regression, renderer level: an xfail test whose REAL failure
     message equals the runner's unexpected-pass string. The record says
     excused ([counted = false]); classification is record-driven, so the
     stream agrees with the exit code and the summary — no failure message
     is ever inspected. *)
  let collide =
    Fixtures.result [ "collide" ]
      (Failure.Fail [ Failure.message "expected to fail, but the test passed" ])
      ~xfail:{ Test_tree.reason = None }
  in
  let verbose =
    with_renderer ~mode:`Verbose (fun r -> Report.result r collide)
  in
  check_contains "collision record renders XFAIL, not FAIL" ~sub:"  XFAIL  "
    verbose;
  check_absent "collision record: no loud FAIL line" ~sub:"  FAIL  " verbose;
  let summary =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r collide;
        Report.finish r ~results:[ collide ] ~duration:0.1 ())
  in
  check_string "collision record: stream, summary, and count agree"
    ~expected:"s: 1 expected failure in 100ms.\n" ~actual:summary

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
        Report.finish r ~results ~duration:0.2 ())
  in
  check "finish: excused leaves the failure section its one block"
    (occurrences_of ~sub:(failures_rule ^ "\n") t = 1
    && occurrences_of ~sub:"\n  FAIL  " t = 1);
  check_absent "finish: excused block absent" ~sub:"broken carry" t;
  check_contains "finish: summary counts the expected failure"
    ~sub:"1 passed, 1 expected failure, 1 failed in 200ms." t;
  let only_excused =
    with_renderer ~invocation:(`Exe "exe") (fun r ->
        Report.finish r
          ~results:
            [ Fixtures.result [ "ok" ] Failure.Pass; Fixtures.excused_result ]
          ~duration:0.2 ())
  in
  check_absent "finish: no failure section when all failures excused"
    ~sub:"failures \u{2500}" only_excused;
  check_absent "finish: and no closing rule" ~sub:closing_rule only_excused;
  check_absent "finish: no rerun hint when all failures excused" ~sub:"--failed"
    only_excused;
  check_contains "finish: green summary with excused failures"
    ~sub:"1 passed, 1 expected failure in 200ms." only_excused

let test_xpass_is_loud () =
  (* The runner records an xfail test that passed as an ordinary counted
     failure whose message names the reason: no excused marking, loud FAIL. *)
  let line =
    with_renderer ~mode:`Verbose (fun r ->
        Report.result r Fixtures.xpass_result)
  in
  check_contains "unexpected pass: loud FAIL line" ~sub:"  FAIL  known › fixed"
    line;
  let t =
    with_renderer (fun r ->
        Report.finish r ~results:[ Fixtures.xpass_result ] ~duration:0.1 ())
  in
  check_contains "unexpected pass: reason in the failure block"
    ~sub:"expected to fail (issue #42), but the test passed" t

(* Subtest failures (amendment B13) *)

let test_subtest_projection () =
  check "subtest entries recognized by their components"
    (Report.is_subtest_failure (Fixtures.subtest_failure "shape [0]"));
  check "plain failures are not subtest entries"
    (not (Report.is_subtest_failure (Failure.message "boom")));
  (* The collision regression: classification is record-driven, so a user
     [?msg] spelling out the [leaf › name] prefix stays an ordinary
     annotation instead of being dressed as a sub-case. *)
  let collision =
    {
      (Failure.message "boom") with
      Failure.msg = Some "contract \u{203a} shape [0]";
    }
  in
  check "a user msg spelling the label prefix is not a subtest entry"
    (not (Report.is_subtest_failure collision));
  let t =
    with_renderer (fun r ->
        Report.finish r
          ~results:
            [
              Fixtures.result [ "backend"; "contract" ]
                (Failure.Fail [ collision ]);
            ]
          ~duration:0.1 ())
  in
  check_contains "the colliding msg renders as an ordinary annotation"
    ~sub:"contract \u{203a} shape [0]" t;
  check "the colliding msg adds no subtest count to the summary"
    (not (has ~sub:"subtest failure" t))

let test_subtest_rendering () =
  let t =
    with_renderer (fun r ->
        Report.finish r ~results:[ Fixtures.subtest_result ] ~duration:0.1 ())
  in
  check_contains "a subtest entry names its subtest under its location"
    ~sub:"    test/test_backend.ml:40\n    subtest   shape [0]\n    expected" t;
  check_contains "the subtest's name sits in the column of the values under it"
    ~sub:"    subtest   shape [0]\n    expected  [1; 2]\n" t;
  check_contains "a blank line separates two entries of one test"
    ~sub:
      ("    actual    [1; 3]\n" ^ String.make 18 ' '
     ^ "~\n\n    test/test_backend.ml:40\n    subtest   shape [2]\n")
    t;
  check_contains "the last entry ends the block on its facts"
    ~sub:("\n\n    test/test_backend.ml:61\n    final check\n" ^ closing_rule)
    t;
  check_absent "the parent's name is the title's, not repeated per entry"
    ~sub:"contract › shape" t;
  check_contains "summary states the subtest count"
    ~sub:"1 failed (2 subtest failures) in 100ms." t;
  let one =
    with_renderer (fun r ->
        Report.finish r
          ~results:
            [
              Fixtures.result [ "backend"; "contract" ]
                (Failure.Fail [ Fixtures.subtest_failure "shape [0]" ]);
            ]
          ~duration:0.1 ())
  in
  check_contains "summary subtest count is singular"
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
        Report.finish r ~results:[ result ] ~duration:0.01 ())
  in
  check_contains "prop stats: label distribution"
    ~sub:"labels (100 passing cases):" b;
  check_contains "prop stats: percentages" ~sub:"36.0%  empty" b;
  check_contains "prop stats: uncovered label"
    ~sub:"      collision  0  never covered\n" b;
  (* The list carries the covered label too — that is what it adds over the
     failure headline, which names only the ones that were not. *)
  check_contains "prop stats: covered label listed alongside"
    ~sub:"singleton  9" b;
  (* With a single label the list would only restate the headline, so it
     does not print at all. *)
  let single =
    { stats with Property.coverage = [ List.hd stats.Property.coverage ] }
  in
  let b1 =
    with_renderer (fun r ->
        Report.finish r
          ~results:
            [
              Fixtures.result [ "p" ]
                (Failure.Fail [ Failure.message "coverage unsatisfied" ])
                ~prop_stats:single;
            ]
          ~duration:0.01 ())
  in
  check_absent "prop stats: a lone label is not restated" ~sub:"covered labels:"
    b1;
  check_contains "prop stats: its labels still print"
    ~sub:"labels (100 passing cases):" b1

(* Containment blocks (D5 §2) *)

let not_contains_failure =
  Failure.containment ~found_at:10 ~claim:{|string not containing "secret"|}
    ~needle:"secret" ~haystack:"0123456789secret-end" ()

let test_containment_block () =
  let b = failure_block not_contains_failure in
  check_contains "not_contains: needle line carries the byte offset"
    ~sub:"    needle    \"secret\": found at byte 10\n" b;
  check_absent "not_contains: no em dash" ~sub:"\u{2014}" b;
  check_contains "not_contains: marker line sits under the occurrence"
    ~sub:
      ("    haystack  0123456789secret-end\n" ^ String.make 24 ' ' ^ "~~~~~~\n")
    b;
  check_absent "not_contains: the claim description never prints"
    ~sub:"string not containing" b;
  check_absent "not_contains: no fake equality labels" ~sub:"expected" b;
  check_absent "not_contains: no excerpt line for a complete excerpt"
    ~sub:"(excerpt:" b;
  (* Under colour the occurrence is coloured in place of the mark. *)
  let colored = failure_block ~ansi:true not_contains_failure in
  check_contains
    "not_contains: under colour the haystack prints whole and plain, the \
     occurrence bold red in it"
    ~sub:"    \027[2mhaystack\027[0m  0123456789\027[1;31msecret\027[0m-end\n"
    colored;
  check_absent "not_contains: no mark under colour" ~sub:"~" colored;
  check "not_contains: colour replaces the mark and nothing else"
    (Text.strip_ansi colored = without_marks b);
  (* contains: needle absent, display-capped head excerpt of a huge
     haystack. *)
  let haystack = String.make 20_006 'a' in
  let contains_failure =
    Failure.containment ~claim:{|string containing "NOPE"|} ~needle:"NOPE"
      ~haystack ()
  in
  let b = failure_block contains_failure in
  check_contains "contains: needle line with the not-found verdict"
    ~sub:"    needle    \"NOPE\": not found\n" b;
  check_contains "contains: elision line states the capped display window"
    ~sub:"    (excerpt: bytes 0-1023 of a 20006-byte haystack)\n" b;
  check_contains "contains: the capped excerpt prints verbatim"
    ~sub:("haystack  " ^ String.make 100 'a')
    b;
  check_absent "contains: no diff against the claim sentence" ~sub:"~~~" b

let test_containment_multiline () =
  let f =
    Failure.containment ~claim:{|string containing "user=bob"|}
      ~needle:"user=bob" ~haystack:"line one\nline two user=alice\nline three"
      ()
  in
  let b = failure_block f in
  check_contains "multi-line haystack: block form"
    ~sub:
      "    needle    \"user=bob\": not found\n\
      \    haystack:\n\
      \      line one\n\
      \      line two user=alice\n\
      \      line three\n"
    b;
  check_absent "multi-line haystack: no unified diff" ~sub:"--- expected" b;
  (* A found occurrence in a multi-line excerpt is marked under its line
     without colour, and coloured on it with. *)
  let found =
    Failure.containment ~found_at:14 ~claim:{|string not containing "secret"|}
      ~needle:"secret" ~haystack:"line one\nthe1 secret here\nline three" ()
  in
  let plain = failure_block found in
  check_contains "multi-line occurrence marked under its line"
    ~sub:
      "    haystack:\n\
      \      line one\n\
      \      the1 secret here\n\
      \           ~~~~~~\n\
      \      line three\n"
    plain;
  let colored = failure_block ~ansi:true found in
  check_contains
    "multi-line occurrence: under colour it is bold red on its line, unmarked"
    ~sub:
      "      line one\n\
      \      the1 \027[1;31msecret\027[0m here\n\
      \      line three\n"
    colored;
  check "multi-line occurrence: colour replaces the mark and nothing else"
    (Text.strip_ansi colored = without_marks plain)

(* The not-found display cap: with no occurrence to mark, the haystack is
   context rather than evidence, so the display shows a small head window
   — at most 10 lines and 1 KiB — and the excerpt line states the cut in
   the same words it states the stored bound. A found occurrence keeps the
   full stored window: there the excerpt is the evidence. *)
let test_containment_not_found_cap () =
  (* Single-line content cuts at the byte bound, and the verdict sits
     directly above the excerpt — the cap exists so an 8 KiB context dump
     cannot scroll the diagnosis away. *)
  let haystack = String.make 20_006 'a' in
  let f =
    Failure.containment ~claim:{|string containing "NOPE"|} ~needle:"NOPE"
      ~haystack ()
  in
  let b = failure_block f in
  check_contains "cap: the verdict line is adjacent to the excerpt"
    ~sub:
      ("    needle    \"NOPE\": not found\n    haystack  " ^ String.make 64 'a')
    b;
  check_absent "cap: nothing beyond the display window prints"
    ~sub:(String.make 1025 'a') b;
  check_contains "cap: the elision line states the shown range"
    ~sub:"    (excerpt: bytes 0-1023 of a 20006-byte haystack)\n" b;
  (* Line-structured content cuts after ten complete lines, well under the
     byte bound. 40 lines of 21 bytes: the cut lands after "line 09"'s
     newline, byte 219. *)
  let line i = Printf.sprintf "line %02d filler filler" i in
  let haystack = String.concat "\n" (List.init 40 line) in
  let f =
    Failure.containment ~claim:{|string containing "NOPE"|} ~needle:"NOPE"
      ~haystack ()
  in
  let b = failure_block f in
  check_contains "cap: the tenth line still prints"
    ~sub:"      line 09 filler filler\n" b;
  check_absent "cap: the eleventh line does not" ~sub:"line 10" b;
  check_contains "cap: the multi-line elision line states the shown range"
    ~sub:"    (excerpt: bytes 0-219 of a 879-byte haystack)\n" b;
  (* A found occurrence keeps the stored window whole: not_contains on a
     3 KiB haystack shows all of it, uncapped and unelided. *)
  let haystack = String.make 2_994 'x' ^ "secret" in
  let f =
    Failure.containment ~found_at:2_994
      ~claim:{|string not containing "secret"|} ~needle:"secret" ~haystack ()
  in
  let b = failure_block f in
  check_contains "found-at: the full stored window prints" ~sub:haystack b;
  check_absent "found-at: no elision line for a complete excerpt"
    ~sub:"(excerpt:" b;
  (* An in_order chain break keeps its window even with nothing found: the
     cursor-anchored excerpt is the region still to be matched, which is
     the diagnosis, not context. *)
  let f =
    Failure.containment
      ~demand:(Failure.Ordered { index = 1; resumed_at = 9_000 })
      ~claim:{|string containing "NOPE" at or after byte 9000|} ~needle:"NOPE"
      ~haystack:(String.make 10_000 'a') ()
  in
  check_contains "in_order: the cursor-anchored window is not capped"
    ~sub:"    (excerpt: bytes 4904-9999 of a 10000-byte haystack)\n"
    (failure_block f)

let test_containment_headlines () =
  check "headline: not_contains names the offset"
    (Report.headline not_contains_failure = {|needle "secret" found at byte 10|});
  let contains_failure =
    Failure.containment ~claim:{|string containing "NOPE"|} ~needle:"NOPE"
      ~haystack:(String.make 20_006 'a') ()
  in
  check "headline: contains names the haystack size"
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
   resumed at 36 — past "authenticate" — and its only occurrence, byte 13,
   is behind the cursor. *)
let out_of_order_failure =
  Failure.containment ~found_at:13
    ~demand:(Failure.Ordered { index = 2; resumed_at = 36 })
    ~claim:{|string containing "disconnect" at or after byte 36|}
    ~needle:"disconnect" ~haystack:chain_haystack ()

let missing_element_failure =
  Failure.containment
    ~demand:(Failure.Ordered { index = 2; resumed_at = 36 })
    ~claim:{|string containing "teardown" at or after byte 36|}
    ~needle:"teardown" ~haystack:chain_haystack ()

let test_in_order_block () =
  let b = failure_block out_of_order_failure in
  (* Which element broke the chain is its own line, on the label gutter;
     the verdict slot carries where the search stood and where the element
     actually is — "there, but too early" rather than "not there". *)
  check_contains "in_order: the element index is a line of its own"
    ~sub:"    element   2\n" b;
  check_contains "in_order: the verdict names both offsets"
    ~sub:
      "    needle    \"disconnect\": found at byte 13, before the search \
       resumed at byte 36\n"
    b;
  check_contains "in_order: the early occurrence is marked in the haystack"
    ~sub:
      ("    haystack  " ^ chain_haystack ^ "\n" ^ String.make 27 ' '
     ^ "~~~~~~~~~~\n")
    b;
  check_absent "in_order: the claim description never prints"
    ~sub:"at or after byte 36\n" b;
  let colored = failure_block ~ansi:true out_of_order_failure in
  check_contains "in_order: under colour the early occurrence is bold red"
    ~sub:
      "    \027[2mhaystack\027[0m  connect send \027[1;31mdisconnect\027[0m \
       authenticate\n"
    colored;
  check "in_order: colour replaces the mark and nothing else"
    (Text.strip_ansi colored = without_marks b);
  (* No occurrence anywhere: the verdict is the cursor alone and there is
     nothing to mark. *)
  let b = failure_block missing_element_failure in
  check_contains "in_order: a missing element names only the cursor"
    ~sub:
      "    element   2\n\
      \    needle    \"teardown\": not found at or after byte 36\n"
    b;
  check_absent "in_order: nothing is marked when nothing occurs" ~sub:"~~~" b

let test_demand_headlines () =
  check "headline: in_order names the element, its offset and the cursor"
    (Report.headline out_of_order_failure
    = {|element 2 "disconnect" out of order: at byte 13, before byte 36|});
  let missing =
    {|element 2 "teardown" not found at or after byte 36 (36-byte haystack)|}
  in
  check "headline: a missing element names the cursor and the haystack size"
    (Report.headline missing_element_failure = missing)

let test_satisfies_no_refinement () =
  (* The claim sentence is a description, not a rendering: never diff or
     refine the two (D5 §2). *)
  let f = Failure.predicate ~claim:"value satisfying the predicate" "-3" in
  let b = failure_block f in
  check_contains "satisfies: two label lines"
    ~sub:"    expected  value satisfying the predicate\n    actual    -3\n" b;
  check_absent "satisfies: no marker line against the claim" ~sub:"~" b;
  let multi =
    failure_block
      (Failure.predicate ~claim:"value satisfying the predicate" "[0; 1;\n 2]")
  in
  check_contains "satisfies: multi-line value prints in block form"
    ~sub:
      "    expected  value satisfying the predicate\n\
      \    actual:\n\
      \      [0; 1;\n\
      \       2]\n"
    multi;
  check_absent "satisfies: no unified diff against the claim"
    ~sub:"--- expected" multi;
  check_contains "satisfies: each line of the block is red, no style spans two"
    ~sub:
      "    \027[2mactual:\027[0m\n\
      \      \027[31m[0; 1;\027[0m\n\
      \      \027[31m 2]\027[0m\n"
    (failure_block ~ansi:true
       (Failure.predicate ~claim:"value satisfying the predicate" "[0; 1;\n 2]"));
  let matches =
    failure_block (Failure.predicate ~claim:"a match" "Error \"boom\"")
  in
  check_absent "matches: no refinement either" ~sub:"~" matches

(* An invisible difference in hunks: the [~] line under the [-] line *)

let test_trailing_whitespace_hunks () =
  let b =
    failure_block
      (Failure.equality ~expected:"line one \nline two"
         ~actual:"line one\nline two" ())
  in
  check_contains
    "a one-to-one pair differing in trailing space: the line keeps its space \
     and a ~ sits under it"
    ~sub:
      "    @@ -1,2 +1,2 @@\n\
      \    - line one \n\
      \              ~\n\
      \    + line one\n\
      \      line two\n"
    b;
  check_absent "no middle dot" ~sub:"\u{00B7}" b;
  (* The space is the [+] line's: the mark stands where it will be. *)
  let b =
    failure_block
      (Failure.equality ~expected:"ab\nline two" ~actual:"ab  \nline two" ())
  in
  check_contains "an inserted trailing run is marked at its columns"
    ~sub:"    - ab\n        ~~\n    + ab  \n" b;
  (* A tab has no fixed width: the mark line repeats the line's tabs, so
     it aligns on any tab stops. Context lines keep their bytes. *)
  let b =
    failure_block
      (Failure.equality ~expected:"a\tx\t\ncommon \ny"
         ~actual:"a\tx\ncommon \ny" ())
  in
  check_contains "a trailing tab is marked, the mark line repeating the tabs"
    ~sub:"    - a\tx\t\n       \t ~\n    + a\tx\n" b;
  check_absent "no arrow glyph" ~sub:"\u{2192}" b;
  check_contains "context lines keep raw trailing whitespace"
    ~sub:"      common \n" b;
  check "context lines draw no mark" (occurrences_of ~sub:"~" b = 1);
  (* A visible difference needs no mark, whatever its trailing spaces; nor
     does a run of changes, whose lines answer each other in no order. *)
  let b =
    failure_block
      (Failure.equality ~expected:"foo \nline two" ~actual:"bar\nline two" ())
  in
  check_absent "a visible difference draws no mark" ~sub:"~" b;
  let b =
    failure_block (Failure.equality ~expected:"a \nb \nz" ~actual:"a\nb\nz" ())
  in
  check_absent "a run of changes is no one-to-one pair" ~sub:"~" b;
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
  check_contains "ansi path: the expected line whole, then the mark in its role"
    ~sub:
      ("\027[32m- line one \027[0m\n" ^ String.make 14 ' '
     ^ "\027[31m~\027[0m\n")
    colored;
  check "ansi path: the same bytes once the escapes are stripped"
    (Text.strip_ansi colored = plain);
  (* One meaning for green, across both diff paths.

     A transcript routinely shows both — a short value marks its spans,
     a multi-line one emits hunks — and until [text] made the multi-line
     path ordinary, nobody hit them side by side often enough to notice
     that green meant "expected" on one and "actual" on the other. The
     [-]/[+] sigils carry the diff convention; the colour carries the
     report's. Pin both here so they cannot drift apart again. *)
  let spans =
    failure_block ~ansi:true
      (Failure.equality ~expected:"the quick brown fox"
         ~actual:"the quick brawn fox" ())
  in
  check_contains "span path: expected side is green"
    ~sub:"the quick br\027[1;32mo\027[0mwn fox" spans;
  check_contains "span path: actual side is red"
    ~sub:"the quick br\027[1;31ma\027[0mwn fox" spans;
  let hunks =
    failure_block ~ansi:true
      (Failure.equality ~expected:"keep\nexpected\n" ~actual:"keep\nactual\n" ())
  in
  check_contains "hunk path: expected side is green too"
    ~sub:"\027[32m- expected\027[0m" hunks;
  check_contains "hunk path: actual side is red too"
    ~sub:"\027[31m+ actual\027[0m" hunks;
  check_absent "hunk path: no diff-tool colouring survives"
    ~sub:"\027[31m- expected" hunks;
  (* Baseline mismatch diffs share pp_hunks — the single producer. *)
  let snap =
    failure_block
      (Failure.baseline
         (Failure.Literal { exact = true })
         (Failure.Mismatch { expected = "a \nb\n"; actual = "a\nb\n" }))
  in
  check_contains "baseline diffs mark a trailing space too"
    ~sub:
      "    expect_exact: mismatch\n\
      \    @@ -1,2 +1,2 @@\n\
      \    - a \n\
      \       ~\n\
      \    + a\n"
    snap

(* Uncaught exceptions (D5 §5) *)

let test_uncaught_wording () =
  let b = failure_block (Failure.raised ~actual:"Not_found" ()) in
  check_contains "uncaught: the sentence, the exception indented under it"
    ~sub:"    uncaught exception:\n      Not_found\n" b;
  check_absent "uncaught: never the bare anchor of a raises assertion"
    ~sub:"    raised  " b;
  check_absent "uncaught: no expected side" ~sub:"expected exception" b;
  check_absent "uncaught: never borrows the predicate wording" ~sub:"predicate"
    b;
  (* Inside a property block, recursively. *)
  let prop =
    failure_block
      (Failure.property
         ~inner:(Failure.raised ~actual:"Dune__exe__V.Boom(50)" ())
         ~rendered:"50" ~case_index:3 ~shrink_steps:2 ~root:Fixtures.root
         ~examples:false ())
  in
  check_contains "uncaught inside a property inner"
    ~sub:
      "    which failed with:\n\
      \      uncaught exception:\n\
      \        Dune__exe__V.Boom(50)\n"
    prop;
  check_contains
    "uncaught: a multi-line exception is a block under the sentence"
    ~sub:"    uncaught exception:\n      Parse_error(\n        line 3)\n"
    (failure_block (Failure.raised ~actual:"Parse_error(\n  line 3)" ()));
  (* raises_match keeps its wording — pinned above in
     [test_raise_message_diff_guards]; the (None, None) arm serves both. *)
  check_contains "wanted-any arm unchanged"
    ~sub:"    expected an exception, but none was raised\n"
    (failure_block (Failure.raised ()))

(* Timed-out shrink searches (D2) *)

let test_timed_out_marker () =
  let f =
    Failure.property ~timed_out:0.3 ~rendered:"9" ~case_index:4 ~shrink_steps:2
      ~root:Fixtures.root ~examples:false ()
  in
  let b = failure_block f in
  check_contains "timed-out: marker line follows the counterexample"
    ~sub:
      "    counterexample (case 4, shrunk 2 steps): 9\n\
      \    timed out after 0.3s while shrinking; counterexample may not be \
       minimal\n\
      \    replay: "
    b;
  check_absent "timed-out: the block says it once, in the line under the head"
    ~sub:"shrinking timed out" b;
  check "timed-out: headline carries the clause"
    (Report.headline f
   = "property failed (case 4, shrunk 2 steps, shrinking timed out): 9");
  let plain = failure_block Fixtures.prop_failure in
  check_absent "no marker without a timeout" ~sub:"timed out" plain;
  check_absent "no headline mark without a timeout" ~sub:"timed out"
    (Report.headline Fixtures.prop_failure)

(* Spent shrink budgets (D2's other stopping condition)

   The flag on the payload is not the report: a reader sees a line under
   the block's counterexample and a clause in the one-line headline, both
   saying that "shrunk N steps" here is where the search stopped counting,
   not where it converged. Pinned present and absent, because a mark that
   printed unconditionally would call every converged search truncated. *)

let test_budget_spent_marker () =
  let f =
    Failure.property ~shrink_exhausted:true ~rendered:"9" ~case_index:4
      ~shrink_steps:50 ~root:Fixtures.root ~examples:false ()
  in
  let b = failure_block f in
  check_contains "budget spent: detail line follows the counterexample"
    ~sub:
      "    counterexample (case 4, shrunk 50 steps): 9\n\
      \    shrinking stopped after 50 steps; counterexample may not be minimal\n\
      \    replay: "
    b;
  check_absent
    "budget spent: the block says it once, in the line under the head"
    ~sub:"shrink limit reached" b;
  check "budget spent: headline carries the clause"
    (Report.headline f
   = "property failed (case 4, shrunk 50 steps, shrink limit reached): 9");
  let plain = failure_block Fixtures.prop_failure in
  check_absent "no detail line without a spent budget" ~sub:"shrinking stopped"
    plain;
  check_absent "no clause without a spent budget" ~sub:"may not be minimal"
    plain;
  check_absent "no headline clause without a spent budget" ~sub:"shrink limit"
    (Report.headline Fixtures.prop_failure);
  (* An example never shrinks, so neither mark applies to one. *)
  let example =
    Failure.property ~shrink_exhausted:true ~rendered:"9" ~case_index:0
      ~shrink_steps:0 ~root:Fixtures.root ~examples:true ()
  in
  check "an example carries no clause"
    (Report.headline example = "property failed (example 1): 9")

(* Inner failures without a location (D4) *)

let test_inner_label_without_location () =
  let inner_no_loc = Failure.equality ~expected:"true" ~actual:"false" () in
  let b =
    failure_block
      (Failure.property ~inner:inner_no_loc ~rendered:"7" ~case_index:0
         ~shrink_steps:0 ~root:Fixtures.root ~examples:false ())
  in
  check_contains
    "a location-less inner failure: [which failed with:], then the facts"
    ~sub:"    which failed with:\n      expected  true\n" b;
  check_absent "and no dangling [at:]" ~sub:"which failed at:" b;
  let located = failure_block Fixtures.prop_failure in
  check_contains
    "a located inner failure: [which failed at:] over its bare location"
    ~sub:
      "    which failed at:\n      test/test_geo.ml:18\n      expected  true\n"
    located;
  check_absent "and no [with:]" ~sub:"which failed with:" located

(* Command hints per invocation (D5 §1) *)

let test_hints_per_invocation () =
  let exe = `Exe "./_build/default/qa/x/t.exe" in
  let accept =
    failure_block ~invocation:exe ~filter:"cli › cli help" Fixtures.snap_missing
  in
  check_contains "accept completes the executable, scoped to the block's test"
    ~sub:"    accept: ./_build/default/qa/x/t.exe -u -f 'cli › cli help'\n"
    accept;
  check_absent "accept carries no trailing advice" ~sub:"then review" accept;
  check_absent "accept under Exe never spells dune promote" ~sub:"dune promote"
    accept;
  let replay =
    failure_block ~invocation:exe ~filter:"mod7" Fixtures.prop_failure
  in
  check_contains "replay hint completes the executable with the flags"
    ~sub:
      "    replay: ./_build/default/qa/x/t.exe --seed s1:7be1d2c904aa31f5 -f \
       'mod7'\n"
    replay;
  let bare = failure_block ~invocation:exe Fixtures.prop_failure in
  check_contains "replay hint without a filter carries the seed alone"
    ~sub:"    replay: ./_build/default/qa/x/t.exe --seed s1:7be1d2c904aa31f5\n"
    bare;
  (* Under a build action acceptance is dune's, given this block's file:
     a literal's source file, a file baseline's path. *)
  let mirrors = failure_block Fixtures.snap_mismatch in
  check_contains "Mirrors accept promotes the literal's source file"
    ~sub:"    accept: dune promote test/test_cli.ml\n" mirrors;
  check_absent "Mirrors accept spelling names no flag" ~sub:" -u" mirrors;
  check_contains "Mirrors accept promotes a file baseline by its path"
    ~sub:"    accept: dune promote test/help.expected\n"
    (failure_block
       (Failure.baseline (Failure.File "test/help.expected")
          (Failure.Mismatch { expected = "a\n"; actual = "b\n" })));
  (* Promotion never creates a file: a missing file baseline under dune
     is accepted by creating it first, and the hint says so. *)
  let missing = failure_block Fixtures.snap_missing in
  check_contains "Mirrors accept spelling for a missing file creates it first"
    ~sub:
      "    accept: touch 'test/help.expected' && dune runtest; dune promote \
       test/help.expected\n"
    missing;
  check_absent "and never spells a bare dune promote"
    ~sub:"    accept: dune promote\n" missing

(* A block ends on a command only when the command says what the block
   does not: one line per distinct command line, in the order accept,
   replay, and none at all otherwise. No block prints a [rerun:]. *)
let test_hint_lines () =
  let plain = Failure.message "b" in
  check_string "a block with nothing to accept or replay ends on its facts"
    ~expected:"    b\n"
    ~actual:
      (failure_block ~invocation:(`Exe "./t.exe") ~filter:"math › adds" plain);
  check_string "under a build action too" ~expected:"    b\n"
    ~actual:(failure_block ~filter:"math › adds" plain);
  check_contains "a path's quote is closed around"
    ~sub:"    accept: ./t.exe -u -f 'it'\\''s'\n"
    (failure_block ~invocation:(`Exe "./t.exe") ~filter:"it's"
       Fixtures.snap_mismatch);
  (* An armed run's failures are the mutant's: a replay keeps it armed,
     and an armed run accepts nothing. *)
  let armed = "lib/calc.ml:9:12:add" in
  check_string "an armed run's block with nothing to replay ends on its facts"
    ~expected:"    b\n"
    ~actual:
      (failure_block ~invocation:(`Exe "./t.exe") ~armed ~filter:"math › adds"
         plain);
  check_contains "an identifier a shell would split is quoted"
    ~sub:
      "    replay: ./t.exe --arm 'my lib/calc.ml:9:12:add' --seed \
       s1:7be1d2c904aa31f5\n"
    (failure_block ~invocation:(`Exe "./t.exe") ~armed:"my lib/calc.ml:9:12:add"
       Fixtures.prop_failure);
  check_contains "an armed run's replay carries --arm"
    ~sub:
      "    replay: ./t.exe --arm lib/calc.ml:9:12:add --seed \
       s1:7be1d2c904aa31f5 -f 'mod7'\n"
    (failure_block ~invocation:(`Exe "./t.exe") ~armed ~filter:"mod7"
       Fixtures.prop_failure);
  check_contains "an armed run's replay carries the mirror under a build action"
    ~sub:
      "    replay: WINDTRAP_MUTATE_ARM=lib/calc.ml:9:12:add \
       WINDTRAP_SEED=s1:7be1d2c904aa31f5 WINDTRAP_FILTER='mod7' dune runtest\n"
    (failure_block ~armed ~filter:"mod7" Fixtures.prop_failure);
  check "an armed run's baseline failure is never accepted: no hint at all"
    (Sections.hints ~armed ~invocation:(`Exe "./t.exe") ~filter:(Some "t")
       [ Fixtures.snap_mismatch; Fixtures.snap_missing ]
    = []);
  check_contains "a control byte in a path never breaks the hint's line"
    ~sub:
      "    replay: ./t.exe --seed s1:7be1d2c904aa31f5 -f \
       $'it\\'s\\ttwo\\nlines\\x1b[0m'\n"
    (failure_block ~invocation:(`Exe "./t.exe")
       ~filter:"it's\ttwo\nlines\027[0m" Fixtures.prop_failure);
  let hints = Sections.hints ~invocation:(`Exe "./t.exe") ~filter:(Some "t") in
  check "a failure with no command of its own adds none"
    (hints [ plain ] = []
    && hints [ plain; Fixtures.snap_mismatch ] = [ "accept: ./t.exe -u -f 't'" ]
    );
  check "one line per distinct command line, accept before replay"
    (hints
       [ Fixtures.prop_failure; Fixtures.snap_mismatch; Fixtures.snap_missing ]
    = [
        "accept: ./t.exe -u -f 't'";
        "replay: ./t.exe --seed s1:7be1d2c904aa31f5 -f 't'";
      ]);
  check "two files under a build action are two acceptances"
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
        Report.finish r ~results:[ two ] ~duration:0.1 ())
  in
  check_contains "the block's hints follow the tail, accept then replay"
    ~sub:
      ("      log line\n\
       \    accept: ./t.exe -u -f 'cli › both'\n\
       \    replay: ./t.exe --seed s1:7be1d2c904aa31f5 -f 'cli › both'\n"
     ^ closing_rule ^ "\n\n1 failed in 100ms.\n")
    t;
  check "a hint prints once per block" (occurrences_of ~sub:"accept:" t = 1)

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
         (Failure.Mismatch { expected = "a\n"; actual = "b\n" }))
  and missing = outside Fixtures.snap_missing in
  (* Run by hand, whatever the mode: [-u] would rewrite nothing. *)
  let exe = failure_block ~invocation:(`Exe "./t.exe") ~filter:"t" in
  check "a literal by hand: the reason, and nothing after it"
    (String.ends_with
       ~suffix:("    + line 2\n      line three\n    " ^ kept_none ^ "\n")
       (exe literal));
  check_absent "a literal by hand: no -u to type" ~sub:"accept:" (exe literal);
  check "a file by hand: the reason, and nothing after it"
    (String.ends_with ~suffix:("    + b\n    " ^ kept_none ^ "\n") (exe file));
  check_absent "a file by hand: no -u to type" ~sub:"accept:" (exe file);
  (* Under a build action ([--corrected], an inline runner): nothing was
     written beside the file, so [dune promote] has nothing to promote. *)
  let action = failure_block ~filter:"t" in
  check "a literal under a build action: the reason, and nothing after it"
    (String.ends_with
       ~suffix:("      line three\n    " ^ kept_none ^ "\n")
       (action literal));
  check_absent "a literal under a build action: nothing to promote"
    ~sub:"dune promote" (action literal);
  check "a file under a build action: the reason, and nothing after it"
    (String.ends_with
       ~suffix:("    + b\n    " ^ kept_none ^ "\n")
       (action file));
  check_absent "a file under a build action: nothing to promote"
    ~sub:"dune promote" (action file);
  check_absent "a missing file under a build action: no file to touch either"
    ~sub:"touch" (action missing);
  check_contains "a missing file: the proposed text still prints"
    ~sub:"    proposed (3 lines):\n" (action missing);
  (* The reason is a fact line: it opens the block's closing lines, once,
     before a replay when the block has one. *)
  let plain = Failure.message "boom" in
  let hints = Sections.hints ~invocation:(`Exe "./t.exe") ~filter:(Some "t") in
  check "a block's closing lines: the reason once, and nothing after it"
    (hints [ plain; literal; missing ] = [ kept_none ]);
  check "a property beside it keeps its replay"
    (hints [ Fixtures.prop_failure; literal ]
    = [ kept_none; "replay: ./t.exe --seed s1:7be1d2c904aa31f5 -f 't'" ]);
  check "a kept correction beside nothing else is accepted as before"
    (hints [ Fixtures.snap_mismatch ] = [ "accept: ./t.exe -u -f 't'" ]);
  check_absent "and draws no reason" ~sub:"no correction was kept"
    (exe Fixtures.snap_mismatch);
  (* A test that failed an expectation and skipped keeps none either, and
     did not fail anywhere else. *)
  let skipped = Failure.with_withheld Failure.Skipped Fixtures.snap_mismatch in
  check "a skip beside the expectation: its own reason"
    (hints [ skipped ]
    = [
        "no correction was kept: the test also skipped; skip before the \
         expectation or not at all, and rerun";
      ]);
  (* An unresolvable path never had a correction to keep. *)
  check "an unresolvable path draws no reason"
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
  check "an armed run: no line at all"
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
        Report.finish r ~results:[ both ] ~duration:0.1 ())
  in
  check_contains "the transcript: tail, reason, closing rule, summary"
    ~sub:
      ("      log line\n    " ^ kept_none ^ "\n" ^ closing_rule
     ^ "\n\n1 failed in 100ms.\n")
    t;
  check_absent "the transcript offers no acceptance" ~sub:"accept:" t;
  (* Every transport projects the same failure. *)
  let annotated = Report.annotations [ both ] in
  check_absent "the annotations offer no acceptance" ~sub:"accept:" annotated;
  check "the baseline's annotation ends on the reason"
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
      (fun r -> Report.finish r ~results:failing ~duration:0.1 ())
  in
  check_contains "an armed run's FAIL title says so"
    ~sub:
      "  FAIL  sub › subtracts (mutant armed)\n\
      \    b\n\n\
      \  FAIL  sub › retried (2 attempts, mutant armed)\n"
    t;
  check_contains "the qualifier shares its parenthesis with the attempts"
    ~sub:"  FAIL  sub › retried (2 attempts, mutant armed)\n" t;
  let unarmed =
    with_renderer (fun r -> Report.finish r ~results:failing ~duration:0.1 ())
  in
  check_absent "an ordinary run's titles carry no qualifier" ~sub:"mutant armed"
    unarmed

(* [--failed] is an optimization, not a step, so no run advertises it. The
   acceptance commands are the opposite case — they name a verb nobody can
   guess — and stay under every mismatch (guarantee 3). *)
let test_no_rerun_hint () =
  let failing =
    [ Fixtures.result [ "t" ] (Failure.Fail [ Failure.message "b" ]) ]
  in
  let exe =
    with_renderer ~invocation:(`Exe "dune exec qa/x/t.exe --") (fun r ->
        Report.finish r ~results:failing ~duration:0.1 ())
  in
  check_absent "a failing run does not advertise --failed" ~sub:"--failed" exe;
  check_absent "and its blocks print no rerun hint" ~sub:"rerun:" exe;
  check "the summary is the last line"
    (String.ends_with ~suffix:"\n\n1 failed in 100ms.\n" exe);
  let mirrors =
    with_renderer (fun r -> Report.finish r ~results:failing ~duration:0.1 ())
  in
  check_absent "nor under Mirrors" ~sub:"--failed" mirrors;
  check_absent "no rerun hint under Mirrors either" ~sub:"rerun:" mirrors;
  List.iter
    (fun mode ->
      check_absent "no rerun hint anywhere in a transcript of every kind"
        ~sub:"rerun:"
        (transcript ~mode ~invocation:golden_invocation ()))
    [ `Compact; `Verbose ]

(* The property replay line, and the fact that it is the only replay line
   a failure block prints. *)

let test_property_replay_line () =
  let prop_result =
    Fixtures.result
      [ "geo"; "area non-negative" ]
      (Failure.Fail [ Fixtures.prop_failure ])
  in
  let t =
    with_renderer (fun r ->
        Report.finish r ~results:[ prop_result ] ~duration:0.1 ())
  in
  check "a property failure prints exactly one replay line"
    (occurrences_of ~sub:"replay:" t = 1);
  let plain =
    with_renderer (fun r ->
        Report.finish r
          ~results:
            [ Fixtures.result [ "t" ] (Failure.Fail [ Failure.message "b" ]) ]
          ~duration:0.1 ())
  in
  check_absent "an ordinary failure prints none" ~sub:"replay:" plain

(* Verbose label distributions (D5 §7) *)

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
  check_contains "verbose: a passing property prints its label table"
    ~sub:"    labels (100 passing cases):\n       46.0%  even\n" verbose;
  check "verbose: the table follows the PASS line"
    (String.starts_with ~prefix:"  PASS  labels visible" verbose);
  let compact = with_renderer (fun r -> Report.result r passing) in
  check_absent "compact: no label table" ~sub:"labels (" compact;
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
  check_absent "verbose: no table without collected labels" ~sub:"labels ("
    unlabeled;
  let excused =
    with_renderer ~mode:`Verbose (fun r ->
        Report.result r
          { Fixtures.excused_result with Run.prop_stats = Some stats })
  in
  check_absent "verbose: XFAIL lines print no table" ~sub:"labels (" excused

(* Name sanitization on terminal surfaces (render/F-2) *)

let test_name_sanitization () =
  let hostile = [ "first\nhalf" ] in
  let failing =
    Fixtures.result hostile (Failure.Fail [ Failure.message "b" ])
  in
  let verbose =
    with_renderer ~mode:`Verbose (fun r -> Report.result r failing)
  in
  check_contains "verbose line escapes the newline" ~sub:{|FAIL  first\nhalf|}
    verbose;
  check_string "the row and the message stay one line each"
    ~expected:
      "  FAIL  first\\nhalf                                0.2ms\n    b\n\n"
    ~actual:verbose;
  check_contains "and so does a hint that spells the path"
    ~sub:
      "      actual    false\n\
      \    replay: WINDTRAP_SEED=s1:7be1d2c904aa31f5 \
       WINDTRAP_FILTER=$'first\\nhalf' dune runtest\n\n"
    (with_renderer ~mode:`Verbose (fun r ->
         Report.result r
           (Fixtures.result hostile (Failure.Fail [ Fixtures.prop_failure ]))));
  let block =
    with_renderer (fun r ->
        Report.finish r ~results:[ failing ] ~duration:0.1 ())
  in
  check_contains "FAIL header escapes the newline" ~sub:{|  FAIL  first\nhalf|}
    block;
  let live =
    with_renderer ~ansi:true ~live:true (fun r ->
        Report.header r ~suite:"vnames" ~tests:2 ~seed:None ();
        Report.begin_test r ~path:hostile)
  in
  check_contains "live tail escapes the newline" ~sub:{|first\nhalf|} live;
  check_absent "live tail carries no raw newline" ~sub:"first\nhalf" live;
  (* Suite names: header, and the one-liner's prefix. *)
  let named =
    with_renderer (fun r ->
        Report.header r ~suite:"my\tsuite" ~tests:1 ~seed:None ();
        Report.result r (Fixtures.result [ "t" ] Failure.Pass);
        Report.finish r
          ~results:[ Fixtures.result [ "t" ] Failure.Pass ]
          ~duration:0.1 ())
  in
  check_contains "summary prefix escapes the tab" ~sub:{|my\tsuite: 1 passed|}
    named;
  let header =
    with_renderer ~mode:`Verbose (fun r ->
        Report.header r ~suite:"a\x07b" ~tests:1 ~seed:None ())
  in
  check_contains "header escapes control bytes" ~sub:{|a\x07b: 1 test|} header;
  (* The slow section's rows share the treatment. *)
  let slow = Fixtures.result [ "sl\now" ] Failure.Pass ~duration:1.5 in
  let warned =
    with_renderer (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.result r slow;
        Report.finish r ~results:[ slow ] ~duration:1.5 ())
  in
  check_contains "slow row escapes the newline" ~sub:{|  1.5s  sl\now|} warned;
  (* ESC is left to the ansi policy (stripped under ansi:false) — pinned in
     [test_ansi_hygiene]. *)
  let note =
    with_renderer ~mode:`Verbose (fun r -> Report.note r "releasing d\nb")
  in
  check_string "notes escape their fixture name" ~expected:"releasing d\\nb\n"
    ~actual:note;
  (* The author's own words sit among the report's: a [?msg] prints its
     lines at the block's indentation, a skip reason and an
     expected-failure reason stay in their row, and the control bytes of
     all three are escaped as a name's are. *)
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
  check_contains
    "a ?msg keeps its lines, each inside the block, control bytes escaped"
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
  check_contains "a skip reason stays in its row"
    ~sub:{|  SKIP  skipped (no\ndb)|} rows;
  check_contains "and so does an expected failure's"
    ~sub:{|  XFAIL  excused (expected failure: issue\t42)|} rows

(* Source excerpts resolve against the project root (render/F-1) *)

(* The location is the bare [file:line]: no anchor word, and nothing about
   [~__POS__] anywhere in the output. A phase other than the body is its
   tag before it. *)
let test_location_forms () =
  let declared = Fixtures.loc "test/test_users.ml" 88 in
  let located f = { f with Failure.loc = Some declared } in
  let tail = located (Failure.equality ~expected:"1" ~actual:"2" ()) in
  check_string "declaration: the bare location opens the entry"
    ~expected:"    test/test_users.ml:88\n    expected  1\n    actual    2\n"
    ~actual:(failure_block tail);
  check_contains "declaration: ansi renders the whole line dim"
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
      check
        (kind ^ ": the bare location opens the entry")
        (String.starts_with ~prefix:"    test/test_users.ml:88\n" b);
      check_absent (kind ^ ": nothing about ~__POS__") ~sub:"__POS__" b;
      check_absent (kind ^ ": no [test declared at]") ~sub:"declared" b;
      check_absent (kind ^ ": no [at] anchor") ~sub:" at test/" b)
    [
      ("a file baseline", Fixtures.snap_missing);
      ("a literal baseline", Fixtures.snap_mismatch);
      ("a message", Failure.message "boom");
      ("a raise verb", Fixtures.raise_failure);
      ("a property", Fixtures.prop_failure);
      ("an uncaught exception", uncaught);
    ];
  check_contains "declaration: a property's counterexample follows its location"
    ~sub:"    test/test_users.ml:88\n    counterexample"
    (failure_block (located Fixtures.prop_failure));
  check_contains "declaration: an uncaught exception follows its location"
    ~sub:"    test/test_users.ml:88\n    uncaught exception:\n      Not_found\n"
    (failure_block (located uncaught));
  let recorded = failure_block Fixtures.eq_failure in
  check "recorded: <file:line> opens the entry"
    (String.starts_with ~prefix:"    test/test_users.ml:31\n    expected  "
       recorded);
  check_absent "recorded: nothing about a declaration" ~sub:"declared" recorded;
  check_string "runner: <file:line>, then the fact"
    ~expected:"    test/test_users.ml:88\n    timed out after 0.2s\n"
    ~actual:(failure_block (located (Failure.message "timed out after 0.2s")));
  check "no location: the entry opens on its facts"
    (String.starts_with ~prefix:"    expected  1\n"
       (failure_block (Failure.equality ~expected:"1" ~actual:"2" ())));
  (* A phase is its bracketed tag before the location. *)
  let teardown = Failure.with_phase Failure.Teardown in
  check_contains "phase: the tag before a runner-made failure's location"
    ~sub:
      "    [teardown] test/test_users.ml:88\n\
      \    uncaught exception:\n\
      \      Not_found\n"
    (failure_block (teardown (located uncaught)));
  check_contains "phase: the tag before a tail-position failure's location"
    ~sub:"    [setup] test/test_users.ml:88\n    expected  1\n"
    (failure_block (Failure.with_phase Failure.Setup tail));
  check_contains "phase: the tag before a recorded line"
    ~sub:"    [teardown] test/test_users.ml:88\n    could not restore\n"
    (failure_block
       (teardown (Failure.message ~loc:declared "could not restore")));
  check_contains "phase: a fixture release names the fixture's site"
    ~sub:"    [release] test/test_users.ml:88\n"
    (failure_block
       (Failure.with_phase Failure.Release
          (Failure.message ~loc:declared "db: release raised Exit")));
  check_contains "phase: the tag is yellow, the location dim"
    ~sub:"\027[33m[teardown]\027[0m \027[2mtest/test_users.ml:88\027[0m\n"
    (failure_block ~ansi:true
       (teardown (Failure.message ~loc:declared "could not restore")));
  List.iter
    (fun anchored ->
      check "the tag opens the location line, never the bare word"
        (String.starts_with ~prefix:"    [teardown] " anchored);
      check_absent "the comma form is gone" ~sub:"teardown," anchored)
    [
      failure_block (teardown (located uncaught));
      failure_block (teardown (Failure.message ~loc:declared "x"));
    ]

let test_excerpt_project_root () =
  (* The recorded location is project-root-relative, exactly as __POS__
     records it. Under [dune runtest] the process cwd is inside _build,
     where this path never opens — resolution against the project root
     must find it; run directly from the repo root, the relative open
     works too, and the block renders identically. *)
  let f =
    Failure.equality
      ~loc:{ Loc.file = "test/unit/test_report.ml"; line = 1; column = 0 }
      ~expected:"1" ~actual:"2" ()
  in
  check_contains "relative recorded paths resolve under dune runtest"
    ~sub:"1 \u{2502} (*---"
    (failure_block ~excerpt:true f)

(* The coverage detail block, escape for escape

   One gutter renderer serves coverage and mutation, and this is
   where its bytes are pinned: the three-column gutter, the number
   right-aligned in at least four, the [│] rule, [·····] between regions,
   the regions themselves (touching windows merged, the first clipped
   against the top of the file), and the [1, 5-6, 11] range dialect the
   table prints. Driven through the real [coverage_report] over section
   data built by hand — the sections name no runtime, so this is the
   whole of their input; the builder that derives it from the runtime's
   file reports is the reporting command's, driven over the real binary
   in test/coverage_cli, and the runtime's line attribution is pinned in
   test/coverage — rather than through the projection's own vals, because
   the difference a review over stripped output cannot see is *where* an
   escape opens: a marker spelled [margin ^ red "▌"] prints the same
   glyphs as [red (margin ^ "▌")]. So this pins the plain bytes whole,
   then pins that colour adds escapes and nothing else, and that the
   marker's escape opens at column zero. *)

let coverage_fixture_source =
  String.concat "\n"
    (List.init 12 (fun i -> Printf.sprintf "let v%d = %d" (i + 1) (i + 1)))
  ^ "\n"

(* One file, four of its eight points never visited, the four falling
   into three runs of lines — so the block carries two [·····] separators
   and a region clipped against the top of the file. *)
let coverage_fixture_data : Sections.coverage =
  {
    Sections.visited = 4;
    total = 8;
    files =
      [
        {
          Sections.file = "lib/fake.ml";
          visited = 4;
          total = 8;
          uncovered = [ 1; 5; 6; 11 ];
          source = Some coverage_fixture_source;
          stale = false;
        };
      ];
  }

let expected_coverage_report =
  "coverage: 50.0% (4/8 points)\n\
  \   50.0%  4/8  lib/fake.ml   uncovered: 1, 5-6, 11\n\n\
   lib/fake.ml \u{2014} 50.0% (4/8)\n\n\
  \  \u{258c}   1 \u{2502} let v1 = 1\n\
  \      2 \u{2502} let v2 = 2\n\
  \   \u{00b7}\u{00b7}\u{00b7}\u{00b7}\u{00b7}\n\
  \      4 \u{2502} let v4 = 4\n\
  \  \u{258c}   5 \u{2502} let v5 = 5\n\
  \  \u{258c}   6 \u{2502} let v6 = 6\n\
  \      7 \u{2502} let v7 = 7\n\
  \   \u{00b7}\u{00b7}\u{00b7}\u{00b7}\u{00b7}\n\
  \     10 \u{2502} let v10 = 10\n\
  \  \u{258c}  11 \u{2502} let v11 = 11\n\
  \     12 \u{2502} let v12 = 12\n"

let test_coverage_report_bytes () =
  let data = coverage_fixture_data in
  let render ?ansi () =
    sections ?ansi (Sections.coverage_report ~mode:`Full data)
  in
  let plain = render () and colored = render ~ansi:true () in
  check_string "coverage report: the frozen bytes, full mode"
    ~expected:expected_coverage_report ~actual:plain;
  check_string "coverage report: colour adds escapes and nothing else"
    ~expected:plain ~actual:(Text.strip_ansi colored);
  check_contains "coverage report: the marker's escape opens at column zero"
    ~sub:"\027[31m  \u{258c}\027[0m   5 \u{2502} let v5 = 5" colored;
  check_absent "coverage report: the marker is not styled past the margin"
    ~sub:"  \027[31m\u{258c}" colored;
  check_contains "coverage report: the table percentage is styled with its pad"
    ~sub:"  \027[31m 50.0%\027[0m  4/8  lib/fake.ml" colored;
  check_string "coverage report: report mode stops before the excerpts"
    ~expected:
      "coverage: 50.0% (4/8 points)\n\
      \   50.0%  4/8  lib/fake.ml   uncovered: 1, 5-6, 11\n"
    ~actual:(sections (Sections.coverage_report ~mode:`Report data))

(* A barely-tested file has hundreds of uncovered regions, and their
   ranges would render as one cell of thousands of characters. The cell
   is bounded, and names the flag that shows the rest. *)

let test_coverage_uncovered_cap () =
  let data =
    {
      Sections.visited = 0;
      total = 60;
      files =
        [
          {
            Sections.file = "lib/wide.ml";
            visited = 0;
            total = 60;
            uncovered = List.init 30 (fun i -> (i * 2) + 1);
            source = None;
            stale = false;
          };
        ];
    }
  in
  let out = sections (Sections.coverage_report ~mode:`Report data) in
  check_contains "the uncovered cell stops after eight regions"
    ~sub:"uncovered: 1, 3, 5, 7, 9, 11, 13, 15 (+22 more, -u shows them)" out;
  check_absent "and drops the ninth" ~sub:"17" out

(* The coverage thresholds, pinned at the bytes

   Green at 80% and above, yellow at 60%, red below — the classification
   used to live on the runtime as [style]; it is styling, so it lives
   with the renderer now, and the summary line is where it shows. *)

let test_coverage_thresholds () =
  let line ~visited ~total =
    sections ~ansi:true
      (Sections.coverage_report ~mode:`Report
         { Sections.visited; total; files = [] })
  in
  check_contains "80 percent is green" ~sub:"\027[32m80.0%\027[0m"
    (line ~visited:8 ~total:10);
  check_contains "60 percent is yellow" ~sub:"\027[33m60.0%\027[0m"
    (line ~visited:6 ~total:10);
  check_contains "79 percent is yellow" ~sub:"\027[33m79.0%\027[0m"
    (line ~visited:79 ~total:100);
  check_contains "59 percent is red" ~sub:"\027[31m59.0%\027[0m"
    (line ~visited:59 ~total:100);
  check_contains "an empty summary is 100% and green"
    ~sub:"\027[32m100.0%\027[0m (0/0 points)" (line ~visited:0 ~total:0)

(* The mutation report (SPEC transcripts, byte for byte)

   The survivor block is the ordinary failure block: the same 54-column
   labelled rule, the same [  VERB  subject] head row, the same excerpt
   row. One fixture serves both reports the type carries: the
   per-executable one (no executable column, no unreached) and the
   aggregate (both). *)

let calc_source =
  String.concat "\n"
    (List.init 31 (fun i ->
         match i + 1 with
         | 9 -> "  | Sub -> a - b"
         | 11 ->
             "  | Div -> if b = 0 then invalid_arg \"division by zero\" else a \
              / b"
         | 22 -> "  if n < limit then"
         | 31 -> "  List.fold_left (fun acc x -> acc + x) 0"
         | n -> Printf.sprintf "(* line %d *)" n))
  ^ "\n"

let witness ?exe test file line =
  { Sections.test; loc = Some { Loc.file; line; column = 0 }; exe }

let mutant id line before after =
  {
    Sections.id;
    file = "lib/calc.ml";
    line;
    before;
    after;
    source = Some calc_source;
  }

let add_mutant = mutant "lib/calc.ml:9:12:add" 9 "a - b" "a + b"
let neq_mutant = mutant "lib/calc.ml:11:15:neq" 11 "b = 0" "b <> 0"
let le_mutant = mutant "lib/calc.ml:22:5:le" 22 "n < limit" "n <= limit"
let sub_mutant = mutant "lib/calc.ml:31:14:sub" 31 "acc + x" "acc - x"

let suite_report =
  {
    (* Pre-spelled, as the loop spells it with the runtime's own
       function: the identifier in its canonical form. *)
    Sections.survivors =
      [
        {
          Sections.mutant = add_mutant;
          witnesses =
            [
              witness "calc \u{203a} sub of two positives" "test/test_calc.ml"
                14;
              witness "calc \u{203a} sub to zero" "test/test_calc.ml" 19;
              witness "eval \u{203a} Sub node" "test/test_eval.ml" 31;
            ];
        };
        {
          Sections.mutant = neq_mutant;
          witnesses =
            [
              witness "calc \u{203a} div by zero raises" "test/test_calc.ml" 24;
            ];
        };
      ];
    unreached = [];
    killed = 181;
    scope = Sections.Suite;
    filter = None;
  }

let aggregate_report =
  {
    Sections.survivors =
      [
        {
          Sections.mutant = add_mutant;
          witnesses =
            [
              witness ~exe:"test_calc.exe" "calc \u{203a} sub of two positives"
                "test/test_calc.ml" 14;
              witness ~exe:"test_calc.exe" "calc \u{203a} sub to zero"
                "test/test_calc.ml" 19;
              witness ~exe:"test_eval.exe" "eval \u{203a} Sub node"
                "test/test_eval.ml" 31;
            ];
        };
      ];
    unreached = [ le_mutant; sub_mutant ];
    killed = 11;
    scope = Sections.Executables 3;
    filter = None;
  }

let mutation_report ?ansi ?mode ?invocation m =
  with_renderer ?ansi ?mode ?invocation (fun r -> Report.mutation_report r m)

let exe_invocation =
  `Exe "dune exec --instrument-with ppx_windtrap.mutate test/test_calc.exe --"

let expected_suite_report =
  {|
─────────────────── survivors (2) ────────────────────

  SURVIVED  lib/calc.ml:9:12:add    a - b  →  a + b
       9 │   | Sub -> a - b

    3 tests ran this line and none failed:
      calc › sub of two positives      test/test_calc.ml:14
      calc › sub to zero               test/test_calc.ml:19
      eval › Sub node                  test/test_eval.ml:31

  SURVIVED  lib/calc.ml:11:15:neq   b = 0  →  b <> 0
      11 │   | Div -> if b = 0 then invalid_arg "division by zero" else a / b

    1 test ran this line and did not fail:
      calc › div by zero raises        test/test_calc.ml:24

──────────────────────────────────────────────────────

mutants: 2 survived of 183 reached by this suite · 181 killed
reproduce: dune exec --instrument-with ppx_windtrap.mutate test/test_calc.exe -- --arm <id>
|}

let expected_aggregate_report =
  {|
─────────────────── survivors (1) ────────────────────

  SURVIVED  lib/calc.ml:9:12:add   a - b  →  a + b
       9 │   | Sub -> a - b

    3 tests in 2 executables ran this line and none failed:
      test_calc.exe   calc › sub of two positives      test/test_calc.ml:14
      test_calc.exe   calc › sub to zero               test/test_calc.ml:19
      test_eval.exe   eval › Sub node                  test/test_eval.ml:31

───────────────── never reached (2) ──────────────────

  UNREACHED  lib/calc.ml:22:5:le     n < limit  →  n <= limit
      22 │   if n < limit then

  UNREACHED  lib/calc.ml:31:14:sub   acc + x  →  acc - x
      31 │   List.fold_left (fun acc x -> acc + x) 0

──────────────────────────────────────────────────────

mutants: 1 survived of 12 reached · 11 killed · 2 never reached · 3 executables
reproduce: WINDTRAP_MUTATE_ARM=<id> <re-run the instrumented suite>
|}

let test_mutation_report () =
  check_string "the per-executable report, byte for byte"
    ~expected:expected_suite_report
    ~actual:(mutation_report ~invocation:exe_invocation suite_report);
  check_string "the aggregate report, byte for byte"
    ~expected:expected_aggregate_report
    ~actual:(mutation_report aggregate_report);
  let out = mutation_report ~invocation:exe_invocation suite_report in
  (* The block is the finding and the footer is the remedy: no per-block
     command, no attribute to paste. *)
  check_absent "no arm line in a block" ~sub:"    arm " out;
  check_absent "no dismiss line in a block" ~sub:"dismiss" out;
  check_absent "no [@mutate off] to paste" ~sub:"[@mutate off" out

let test_mutation_sentence () =
  let block witnesses =
    mutation_report
      {
        suite_report with
        Sections.survivors = [ { Sections.mutant = add_mutant; witnesses } ];
      }
  in
  let one = witness "calc \u{203a} sub to zero" "test/test_calc.ml" 19 in
  let other = witness "eval \u{203a} Sub node" "test/test_eval.ml" 31 in
  check_contains "singular"
    ~sub:"\n    1 test ran this line and did not fail:\n" (block [ one ]);
  check_contains "plural" ~sub:"\n    2 tests ran this line and none failed:\n"
    (block [ one; other ]);
  (* The executable column appears exactly when a witness names one, and
     it is one column for the report: a row without an executable still
     leaves the column. *)
  let named = { one with Sections.exe = Some "test_calc.exe" } in
  check_absent "no executable column without an executable" ~sub:"test_calc.exe"
    (block [ one; other ]);
  check_contains "the column appears when one witness names an executable"
    ~sub:
      "\n\
      \      test_calc.exe   calc \u{203a} sub to zero      test/test_calc.ml:19\n\
      \                      eval \u{203a} Sub node         test/test_eval.ml:31\n"
    (block [ named; other ]);
  check_contains "one executable is just tests"
    ~sub:"\n    2 tests ran this line and none failed:\n"
    (block [ named; { other with Sections.exe = Some "test_calc.exe" } ]);
  check_contains "several executables are counted"
    ~sub:"\n    2 tests in 2 executables ran this line and none failed:\n"
    (block [ named; { other with Sections.exe = Some "test_eval.exe" } ])

let test_mutation_colors () =
  let out =
    mutation_report ~ansi:true ~invocation:exe_invocation suite_report
  in
  check_contains "SURVIVED wears the failure red, the identifier the bold"
    ~sub:"  \027[31mSURVIVED\027[0m  \027[1mlib/calc.ml:9:12:add\027[0m" out;
  check_contains "the witness location is faint"
    ~sub:"\027[2mtest/test_calc.ml:14\027[0m" out;
  check_contains "the labelled rule is faint" ~sub:"\027[2m\u{2500}" out;
  check_contains "the survived count is red, the killed count green"
    ~sub:
      "mutants: \027[31m2 survived\027[0m of 183 reached by this suite \
       \u{00b7} \027[32m181 killed\027[0m\n"
    out;
  (* No color in the footer, as in every hint — pinned by the whole
     line, so a footer that went missing fails too. *)
  check_contains "the reproduce footer carries no color"
    ~sub:
      "\n\
       reproduce: dune exec --instrument-with ppx_windtrap.mutate \
       test/test_calc.exe -- --arm <id>\n"
    out;
  let out = mutation_report ~ansi:true aggregate_report in
  check_contains "UNREACHED wears yellow, the identifier the bold"
    ~sub:"  \027[33mUNREACHED\027[0m  \027[1mlib/calc.ml:22:5:le\027[0m" out;
  check_contains "the never-reached count is yellow, the rest plain"
    ~sub:
      "mutants: \027[31m1 survived\027[0m of 12 reached \u{00b7} \027[32m11 \
       killed\027[0m \u{00b7} \027[33m2 never reached\027[0m \u{00b7} 3 \
       executables\n"
    out;
  (* The same lines without color carry no escape at all: the styling is
     the renderer's decision, never the data's. *)
  check_absent "no escape without color" ~sub:"\027["
    (mutation_report ~invocation:exe_invocation suite_report);
  check_absent "no escape without color, aggregate" ~sub:"\027["
    (mutation_report aggregate_report)

let test_mutation_summary_forms () =
  let summary ?(survivors = []) ?(unreached = []) ~killed scope =
    mutation_report ~invocation:exe_invocation
      { suite_report with Sections.survivors; unreached; killed; scope }
  in
  let survivor =
    { Sections.mutant = add_mutant; witnesses = [ witness "t" "test/t.ml" 1 ] }
  in
  (* A report with nothing to say is one line, and the clean form is the
     absence of a survived term, not a zero. *)
  check_string "suite, clean"
    ~expected:"mutants: 5 reached by this suite \u{00b7} 5 killed\n"
    ~actual:(summary ~killed:5 Sections.Suite);
  check_string "selected, clean"
    ~expected:"mutants: 2 reached by the 2 selected tests \u{00b7} 2 killed\n"
    ~actual:(summary ~killed:2 (Sections.Selected 2));
  check_string "one selected test"
    ~expected:"mutants: 1 reached by the 1 selected test \u{00b7} 1 killed\n"
    ~actual:(summary ~killed:1 (Sections.Selected 1));
  check_string "executables, clean"
    ~expected:"mutants: 14 reached \u{00b7} 14 killed \u{00b7} 3 executables\n"
    ~actual:(summary ~killed:14 (Sections.Executables 3));
  check_string "one executable"
    ~expected:"mutants: 3 reached \u{00b7} 3 killed \u{00b7} 1 executable\n"
    ~actual:(summary ~killed:3 (Sections.Executables 1));
  (* Zero terms are omitted: nothing killed and nothing reached. *)
  check_string "nothing reached, nothing killed"
    ~expected:"mutants: 0 reached by this suite\n"
    ~actual:(summary ~killed:0 Sections.Suite);
  let line out =
    match
      List.filter
        (String.starts_with ~prefix:"mutants: ")
        (String.split_on_char '\n' out)
    with
    | [ l ] -> l
    | _ -> "\u{ab}no single mutants line\u{bb}"
  in
  check_string "nothing reached in the aggregate is all never reached"
    ~expected:
      "mutants: 0 reached \u{00b7} 2 never reached \u{00b7} 1 executable"
    ~actual:
      (line
         (summary ~unreached:[ le_mutant; sub_mutant ] ~killed:0
            (Sections.Executables 1)));
  (* With survivors the reached count is the sum of both lists. *)
  check_string "suite, survivors"
    ~expected:"mutants: 1 survived of 5 reached by this suite \u{00b7} 4 killed"
    ~actual:(line (summary ~survivors:[ survivor ] ~killed:4 Sections.Suite));
  check_string "selected, survivors"
    ~expected:
      "mutants: 1 survived of 2 reached by the 3 selected tests \u{00b7} 1 \
       killed"
    ~actual:
      (line (summary ~survivors:[ survivor ] ~killed:1 (Sections.Selected 3)));
  check_string "executables, survivors and never reached"
    ~expected:
      "mutants: 1 survived of 12 reached \u{00b7} 11 killed \u{00b7} 2 never \
       reached \u{00b7} 3 executables"
    ~actual:
      (line
         (summary ~survivors:[ survivor ] ~unreached:[ le_mutant; sub_mutant ]
            ~killed:11 (Sections.Executables 3)));
  check_string "every kill a survivor: no killed term"
    ~expected:"mutants: 1 survived of 1 reached by this suite"
    ~actual:(line (summary ~survivors:[ survivor ] ~killed:0 Sections.Suite))

let test_mutation_footer () =
  (* The footer is the one command, under the summary, with the
     placeholder where the identifier goes: [--arm] after the invocation
     under [`Exe], the flag's mirror before the reader's own suite
     command under [`Mirrors], where no command line reaches the suite. *)
  check_contains "the footer follows the exe invocation"
    ~sub:
      "mutants: 2 survived of 183 reached by this suite \u{00b7} 181 killed\n\
       reproduce: dune exec --instrument-with ppx_windtrap.mutate \
       test/test_calc.exe -- --arm <id>\n"
    (mutation_report ~invocation:exe_invocation suite_report);
  check_contains "the footer under mirrors names no build tool"
    ~sub:
      "\nreproduce: WINDTRAP_MUTATE_ARM=<id> <re-run the instrumented suite>\n"
    (mutation_report suite_report);
  (* A clean report has nothing to reproduce. *)
  let clean = { suite_report with Sections.survivors = []; killed = 183 } in
  check_absent "no footer on a clean report" ~sub:"reproduce:"
    (mutation_report ~invocation:exe_invocation clean);
  (* Never-reached mutants alone are still something to arm. *)
  check_contains "never reached alone keeps the footer" ~sub:"\nreproduce: "
    (mutation_report
       { aggregate_report with Sections.survivors = []; killed = 12 });
  (* A filtered run's survivor survived that selection, so the footer
     restates the filter exactly as the replay line does: [-f], quoted,
     after the command under [`Exe]; [WINDTRAP_FILTER] before the
     placeholder under [`Mirrors]. *)
  let filtered =
    {
      suite_report with
      Sections.scope = Sections.Selected 2;
      filter = Some "sub";
    }
  in
  check_contains "the exe footer carries the filter"
    ~sub:
      "\n\
       reproduce: dune exec --instrument-with ppx_windtrap.mutate \
       test/test_calc.exe -- --arm <id> -f 'sub'\n"
    (mutation_report ~invocation:exe_invocation filtered);
  check_contains "the mirror footer carries the filter"
    ~sub:
      "\n\
       reproduce: WINDTRAP_MUTATE_ARM=<id> WINDTRAP_FILTER='sub' <re-run the \
       instrumented suite>\n"
    (mutation_report filtered);
  check_contains "the filter is shell-quoted, as the replay line's is"
    ~sub:" -f 'it'\\''s'\n"
    (mutation_report ~invocation:exe_invocation
       { filtered with Sections.filter = Some "it's" })

let test_mutation_sections () =
  (* Every survivor gets a block: a survivor is a failure block, and
     windtrap caps no failure block. *)
  check_contains "the label counts the blocks it printed" ~sub:"survivors (1) "
    (mutation_report
       {
         suite_report with
         Sections.survivors = [ List.hd suite_report.Sections.survivors ];
       });
  (* Each finding stands alone: never reached without survivors, and
     survivors without never reached. *)
  let unreached_only =
    { aggregate_report with Sections.survivors = []; killed = 12 }
  in
  check_string "never reached alone: its rule, its blocks, the summary"
    ~expected:
      "\n\
       \u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500} \
       never reached (2) \
       \u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\n\n\
      \  UNREACHED  lib/calc.ml:22:5:le     n < limit  \u{2192}  n <= limit\n\
      \      22 \u{2502}   if n < limit then\n\n\
      \  UNREACHED  lib/calc.ml:31:14:sub   acc + x  \u{2192}  acc - x\n\
      \      31 \u{2502}   List.fold_left (fun acc x -> acc + x) 0\n\n\
       \u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\u{2500}\n\n\
       mutants: 12 reached \u{00b7} 12 killed \u{00b7} 2 never reached \
       \u{00b7} 3 executables\n\
       reproduce: WINDTRAP_MUTATE_ARM=<id> <re-run the instrumented suite>\n"
    ~actual:(mutation_report unreached_only);
  check_absent "survivors alone: no never-reached section" ~sub:"never reached"
    (mutation_report ~invocation:exe_invocation suite_report);
  (* An unreadable source drops the excerpt row and nothing else. *)
  let sourceless =
    {
      aggregate_report with
      Sections.survivors =
        List.map
          (fun (s : Sections.survivor) ->
            {
              s with
              Sections.mutant =
                { s.Sections.mutant with Sections.source = None };
            })
          aggregate_report.Sections.survivors;
      unreached =
        List.map
          (fun (m : Sections.mutant) -> { m with Sections.source = None })
          aggregate_report.Sections.unreached;
    }
  in
  check_absent "an unreadable source drops the excerpt row" ~sub:"\u{2502}"
    (mutation_report sourceless);
  check_contains "an unreadable source keeps the survivor head row"
    ~sub:"  SURVIVED  lib/calc.ml:9:12:add"
    (mutation_report sourceless);
  check_contains "an unreadable source keeps the unreached head row"
    ~sub:"  UNREACHED  lib/calc.ml:22:5:le"
    (mutation_report sourceless)

(* The GitHub Actions envelope: golden ::error annotation, %0A/%0D/%25
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
  check_contains "percent encoded first" ~sub:"50%25 done%0D%0A    next" a;
  check_contains "colons and commas untouched in message data" ~sub:"next: a,b"
    a;
  check "annotation is one command line"
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
  check_contains "file property encodes delimiters"
    ~sub:"file=dir%2Cx%3Ay/test.ml,line=7," a;
  check_contains "title encodes delimiters"
    ~sub:"title=Test failure%3A suite%3A a%2Cb › case::" a;
  (* The title is the block's title: a control byte in a test name is
     spelled out, never sent raw. *)
  check_contains "title spells control bytes as the block's title does"
    ~sub:"title=Test failure%3A a\\x01b\\nc::"
    (Report.annotation ~path:[ "a\001b\nc" ] f)

let test_github_no_location () =
  let a = Report.annotation ~path:[ "t" ] (Failure.message "boom") in
  check_contains "no location: title only" ~sub:"::error title=" a;
  check_absent "no location: no file property" ~sub:"file=" a

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
  check_contains "declaration: annotates the recorded line"
    ~sub:"file=test/test_users.ml,line=88," a;
  check_contains "declaration: the message opens on the bare location"
    ~sub:"::    test/test_users.ml:88%0A    expected  [(\"alice\"" a;
  check "declaration: nothing about ~__POS__ in an annotation"
    (occurrences_of ~sub:"__POS__" a = 0);
  check_absent "declaration: no anchor word" ~sub:"declared" a

let test_github_replay_info () =
  let a =
    Report.annotation ~path:[ "geo"; "area non-negative" ] Fixtures.prop_failure
  in
  check_contains "property annotation carries the replay line"
    ~sub:
      "replay: WINDTRAP_SEED=s1:7be1d2c904aa31f5 WINDTRAP_FILTER='geo › area \
       non-negative' dune runtest"
    a;
  check_contains "counterexample in the message"
    ~sub:"counterexample (case 12, shrunk 4 steps): Rect (2, 0)" a

let test_github_invocation_hints () =
  (* Annotation messages carry the same hint bytes as the terminal block —
     both derive from the one startup-computed invocation. *)
  let invocation = `Exe "dune exec qa/x/t.exe --" in
  let a =
    Report.annotation ~invocation
      ~path:[ "geo"; "area non-negative" ]
      Fixtures.prop_failure
  in
  check_contains "replay hint spelled from the invocation, %0A-encoded"
    ~sub:
      "%0A    replay: dune exec qa/x/t.exe -- --seed s1:7be1d2c904aa31f5 -f \
       'geo › area non-negative'"
    a;
  check_absent "no Mirrors spelling under Exe" ~sub:"WINDTRAP_SEED" a;
  let block =
    Report.annotations ~invocation
      [
        Fixtures.result [ "cli"; "cli help" ]
          (Failure.Fail [ Fixtures.snap_missing ]);
      ]
  in
  check_contains "annotations thread the invocation to accept hints"
    ~sub:"%0A    accept: dune exec qa/x/t.exe -- -u -f 'cli › cli help'\n" block;
  (* A replay is armed when the run was; a failure with no command of its
     own ends on its facts. *)
  check_contains "annotations thread the armed mutant to replay hints"
    ~sub:
      "%0A    replay: dune exec qa/x/t.exe -- --arm lib/calc.ml:9:12:add \
       --seed s1:7be1d2c904aa31f5 -f 'geo'\n"
    (Report.annotations ~invocation ~armed:"lib/calc.ml:9:12:add"
       [ Fixtures.result [ "geo" ] (Failure.Fail [ Fixtures.prop_failure ]) ]);
  check_string "an annotation with no command ends on its facts"
    ~expected:"::error title=Test failure%3A bad::    boom\n"
    ~actual:
      (Report.annotations ~invocation ~armed:"lib/calc.ml:9:12:add"
         [ Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "boom" ]) ])

let test_github_ansi_stripped () =
  let f =
    Failure.equality ~expected:"\027[32mgreen\027[0m" ~actual:"plain" ()
  in
  let a = Report.annotation ~path:[ "t" ] f in
  check_absent "ANSI stripped from annotations" ~sub:"\027" a;
  (* The annotation shares [pp_failure] at [ansi:false], so a comparison
     value reaches it escaped rather than stripped: the bytes survive the
     workflow-command encoding as ordinary text. *)
  check_contains "the compared value keeps its own bytes"
    ~sub:{|\x1b[32mgreen\x1b[0m|} a

let test_github_groups () =
  check_string "group start" ~expected:"::group::mylib\n"
    ~actual:(Report.group_start "mylib");
  check_string "group end" ~expected:"::endgroup::\n" ~actual:Report.group_end;
  check_string "group name newline encoded" ~expected:"::group::a%0Ab\n"
    ~actual:(Report.group_start "a\nb")

let test_github_excused_filtered () =
  (* Classification is record-driven: an excused expected failure — a
     failing record that did not count — annotates nothing, while the
     unexpected-pass record (counted, annotation and all) stays loud. *)
  let results =
    [
      Fixtures.excused_result;
      Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "boom" ]);
    ]
  in
  let block = Report.annotations results in
  check "excused failures produce no annotation"
    (occurrences_of ~sub:"::error " block = 1);
  check_absent "excused test absent from the block" ~sub:"broken carry" block;
  check_contains "counted failures still annotate"
    ~sub:"title=Test failure%3A bad::" block;
  check_string "all failures excused, no output" ~expected:""
    ~actual:(Report.annotations [ Fixtures.excused_result ]);
  check_contains "an unexpected pass still annotates"
    ~sub:"title=Test failure%3A known › fixed already::"
    (Report.annotations [ Fixtures.xpass_result ])

let test_github_subtest_annotations () =
  let block = Report.annotations [ Fixtures.subtest_result ] in
  check "one annotation per failure entry, subtests included"
    (occurrences_of ~sub:"::error " block = 3);
  check_contains "subtest annotations are titled by the parent test"
    ~sub:"title=Test failure%3A backend › contract::" block;
  check_contains "subtest annotations point into the parent's body"
    ~sub:"file=test/test_backend.ml,line=40," block;
  check_contains "the subtest line follows the entry's location"
    ~sub:"::    test/test_backend.ml:40%0A    subtest   shape [0]%0A" block

let test_github_annotations () =
  let block = Report.annotations Fixtures.results in
  check "one command per failure entry (teardown pair gives two)"
    (occurrences_of ~sub:"::error " block = 7);
  check "every command on its own line" (occurrences_of ~sub:"\n" block = 7);
  check_contains "paths name the failing tests"
    ~sub:"title=Test failure%3A db › insert::" block;
  check_string "no failures, no output" ~expected:""
    ~actual:
      (Report.annotations
         [
           Fixtures.result [ "ok" ] Failure.Pass;
           Fixtures.result [ "s" ] (Failure.Skip None);
         ]);
  check_string "empty run, no output" ~expected:""
    ~actual:(Report.annotations [])

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
  Report.finish renderer ~results:[ bad ] ~duration:0.01
    ~before_summary:(fun () ->
      print_string Report.group_end;
      print_string (Report.annotations ~invocation:`Mirrors [ bad ]))
    ();
  check_string
    "the failures and their closing rule, the close, the annotations, then the \
     summary"
    ~expected:
      ("::group::mylib\nmylib: 1 test\n" ^ failures_rule
     ^ "\n  FAIL  bad\n    boom\n" ^ closing_rule
     ^ "\n\
        ::endgroup::\n\
        ::error title=Test failure%3A bad::    boom\n\n\
        1 failed in 10ms.\n")
    ~actual:(output ());
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
    Report.finish r ~results ~duration:2.0
      ~before_summary:(fun () -> Buffer.add_string buf "::endgroup::\n")
      ();
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  check_string "the close follows the last section, the blank line the close"
    ~expected:
      ("mylib: 2 tests\n" ^ failures_rule ^ "\n  FAIL  bad\n    boom\n"
     ^ closing_rule
     ^ "\n\n\
        slow tests (1, over 1s):\n\
       \  1.5s  slow one\n\
        ::endgroup::\n\n\
        1 passed, 1 failed in 2.0s.\n")
    ~actual:(folded [ bad; slow ]);
  check_string "a green run: the close, then the one line"
    ~expected:"::endgroup::\nmylib: 1 passed in 2.0s.\n"
    ~actual:(folded [ Fixtures.result [ "ok" ] Failure.Pass ])

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
  check_string "a selection holding a property prints the root token"
    ~expected:"s: 2 tests (seed s1:7be1d2c904aa31f5)\n"
    ~actual:(header ~properties:true);
  check_string "a selection holding none prints no seed"
    ~expected:"s: 2 tests\n" ~actual:(header ~properties:false);
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
        Report.finish r ~results ~duration:0.06 ())
  in
  check_string "a green property run ends on its seed"
    ~expected:"s: 2 passed in 60ms (seed s1:7be1d2c904aa31f5).\n"
    ~actual:(green ~properties:true);
  check_string "a green run without a property carries no seed"
    ~expected:"s: 2 passed in 60ms.\n" ~actual:(green ~properties:false);
  let streamed =
    with_renderer ~mode:`Verbose (fun r ->
        observe r
          (Run.Run_started
             { suite = "s"; total = 1; selected = 1; properties = false });
        observe r (Run.Test_started { path = [ "t" ] });
        observe r (Run.Test_finished (Fixtures.result [ "t" ] Failure.Pass));
        observe r (Run.Fixture_release { name = "db" }))
  in
  check_string "every event has its line under verbose"
    ~expected:
      "s: 1 test\n\
      \  PASS  t                                          0.2ms\n\
       releasing db\n"
    ~actual:streamed

(* Tree-wide summary dialect

   The meta harness (test/unit/harness.ml) prints its one-liner by hand;
   this pins its bytes to the renderer's with color forced: the same
   styling bytes must wrap the same semantic elements, the harness
   differing only by the documented word "checks" (it counts assertions,
   a windtrap suite counts tests) and by always carrying the suite
   prefix (it prints no header). The harness expectation is derived from
   the rendered line, not hardcoded twice, so the two dialects cannot
   drift apart silently — restyle the renderer's summary and this fails
   until the harness follows. *)

let test_summary_dialect () =
  let chomp s =
    let n = String.length s in
    if n > 0 && s.[n - 1] = '\n' then String.sub s 0 (n - 1) else s
  in
  (* "N passed" -> "N checks passed", first occurrence. *)
  let insert_checks line =
    let marker = " passed" in
    let n = String.length line and m = String.length marker in
    let rec find i =
      if i + m > n then failwith "summary line lost its passed segment"
      else if String.sub line i m = marker then i
      else find (i + 1)
    in
    let i = find 0 in
    String.sub line 0 i ^ " checks" ^ String.sub line i (n - i)
  in
  let transcript ~results ~duration =
    with_renderer ~ansi:true (fun r ->
        Report.header r ~suite:"mylib" ~tests:(List.length results) ~seed:None
          ();
        List.iter (fun res -> Report.result r res) results;
        Report.finish r ~results ~duration ())
  in
  let pass = Fixtures.result [ "t" ] Failure.Pass in
  (* Green: a compact run with nothing to show is exactly the one named line. *)
  let green = chomp (transcript ~results:[ pass; pass ] ~duration:0.5) in
  check_string "renderer green one-liner styles the passed segment"
    ~expected:"mylib: \027[32m2 passed\027[0m in 500ms." ~actual:green;
  check_string "harness green one-liner is the renderer's bytes plus \"checks\""
    ~expected:(insert_checks green)
    ~actual:
      (Harness.summary_line ~ansi:true ~suite:"mylib" ~failures:0 ~count:2
         ~duration:0.5);
  (* Failing: the summary ends the transcript; the harness line is the
     same bytes with the suite prefix (the renderer's header already
     named the suite) and "checks". *)
  let failing =
    Fixtures.result [ "u" ] (Failure.Fail [ Fixtures.eq_failure ])
  in
  let failing_lines =
    String.split_on_char '\n'
      (chomp (transcript ~results:[ pass; failing ] ~duration:0.5))
  in
  let failing_summary =
    match List.rev failing_lines with last :: _ -> last | [] -> ""
  in
  check_string "renderer failing summary styles the failed segment"
    ~expected:"1 passed, \027[31m1 failed\027[0m in 500ms."
    ~actual:failing_summary;
  check_string "harness failing line matches the renderer's styling bytes"
    ~expected:("mylib: " ^ insert_checks failing_summary)
    ~actual:
      (Harness.summary_line ~ansi:true ~suite:"mylib" ~failures:1 ~count:2
         ~duration:0.5);
  (* The harness check lines' FAIL tag: the renderer's own FAIL header
     bytes, derived from the rendered block ("  FAIL  <name>"), not
     hardcoded — restyle the renderer's tag and this fails until the
     harness follows. *)
  let renderer_fail_tag =
    let sep = "  " in
    let header =
      List.find_opt
        (fun l ->
          String.length l > 2 && String.sub l 0 2 = sep && has ~sub:"FAIL" l)
        failing_lines
    in
    match header with
    | None -> failwith "failing transcript lost its FAIL header"
    | Some l ->
        let rec find i =
          if i + 2 > String.length l then
            failwith "FAIL header lost its separator"
          else if String.sub l i 2 = sep then i
          else find (i + 1)
        in
        String.sub l 2 (find 2 - 2)
  in
  check_string "harness FAIL tag carries the renderer's styling bytes"
    ~expected:renderer_fail_tag
    ~actual:(Harness.fail_tag ~ansi:true);
  (* Monochrome: identical wording, zero escape bytes on both sides. *)
  let plain =
    Harness.summary_line ~ansi:false ~suite:"mylib" ~failures:0 ~count:2
      ~duration:0.5
  in
  check_string "harness monochrome line carries no styling bytes"
    ~expected:"mylib: 2 checks passed in 500ms." ~actual:plain;
  check_string "harness monochrome FAIL tag is bare" ~expected:"FAIL"
    ~actual:(Harness.fail_tag ~ansi:false)

(* The corrections section

   What the run wrote for its baselines is a section before the summary
   and a term of it, so the summary stays the last line. A file the run
   could not write is the runner's voice: a standard-error line, never a
   transcript one. *)

let corrections_transcript ?invocation baselines =
  with_renderer ?invocation (fun r ->
      Report.header r ~suite:"s" ~tests:1 ~seed:None ();
      Report.finish r
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
  check_string "update: an accepted row per file, above the summary"
    ~expected:
      (Printf.sprintf
         "s: 1 test\n\
          corrections (1):\n\
         \  accepted %s\n\n\
          1 passed, 1 correction accepted in 2.0ms.\n"
         (Os.display_path (Filename.concat root "help.expected")))
    ~actual:(corrections_transcript baselines);
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
  check_string
    "corrected: a wrote row per .corrected, sorted, expectations counted on \
     the source file only, files counted in the summary"
    ~expected:
      (Printf.sprintf
         "s: 1 test\n\
          corrections (2):\n\
         \  wrote %s\n\
         \  wrote %s (1 expectation)\n\n\
          1 passed, 2 corrections written in 2.0ms.\n"
         (Os.display_path (Filename.concat root "help.expected.corrected"))
         (Os.display_path (source ^ ".corrected")))
    ~actual:(corrections_transcript baselines);
  check_string "the section does not depend on the invocation"
    ~expected:(corrections_transcript ~invocation:`Mirrors baselines)
    ~actual:(corrections_transcript ~invocation:(`Exe "./t.exe") baselines)

let test_corrections_quiet () =
  check_string "nothing written: the green run stays one line"
    ~expected:"s: 1 passed in 2.0ms.\n"
    ~actual:(corrections_transcript (Baseline.create ~mode:Baseline.Check ()));
  (* A refusal is named with its reason: the correction reached nothing. *)
  let root = temp_dir () in
  let baselines = Baseline.create ~root ~cwd:root ~mode:Baseline.Update () in
  Baseline.check baselines
    (Baseline.Literal
       { pos = ("missing.ml", 1, 0, 0); value = "a"; exact = true })
    "b";
  ignore (Baseline.settle baselines ~keep:true);
  Baseline.write baselines;
  (match Report.refusals baselines with
  | [ line ] ->
      let head =
        Printf.sprintf "could not write %s: "
          (Os.display_path (Filename.concat root "missing.ml"))
      in
      check "a refusal is a sentence naming the file, then why, for Os.say"
        (String.starts_with ~prefix:head line)
  | lines -> failf "expected one refusal line, got %d" (List.length lines));
  check_absent "a refusal never prints in the transcript" ~sub:"could not write"
    (corrections_transcript baselines)

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
    test "slow durations sum attempts; failing slow tests warn once"
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
    test "raise message diff (B1)" test_raise_message_diff;
    test "raise message diff guards" test_raise_message_diff_guards;
    test "xfail line (B12)" test_xfail_line;
    test "xpass-string collision stays excused (F4)" test_excused_collision;
    test "finish with excused failures" test_finish_excused;
    test "unexpected pass is loud" test_xpass_is_loud;
    test "subtest projection (B13)" test_subtest_projection;
    test "subtest rendering" test_subtest_rendering;
    test "property stats" test_prop_stats;
    test "containment: claim-aware block (D5 §2)" test_containment_block;
    test "containment: multi-line haystack block" test_containment_multiline;
    test "containment: not-found display cap" test_containment_not_found_cap;
    test "containment: headline forms" test_containment_headlines;
    test "containment: in_order chain-break block" test_in_order_block;
    test "containment: demanded-occurrence headlines" test_demand_headlines;
    test "satisfies/matches: no refinement against the claim"
      test_satisfies_no_refinement;
    test "hunks: trailing whitespace visualized on changed lines (D5 §4)"
      test_trailing_whitespace_hunks;
    test "raise: uncaught wording (D5 §5)" test_uncaught_wording;
    test "property: timed-out shrink marker (D2)" test_timed_out_marker;
    test "property: spent shrink budget marker (D2)" test_budget_spent_marker;
    test "property: inner label without a location (D4)"
      test_inner_label_without_location;
    test "hints: accept and replay per invocation (D5 §1)"
      test_hints_per_invocation;
    test "hints: armed runs, one line per command line, no rerun"
      test_hint_lines;
    test "a withheld correction: no accept, the reason, nothing after it"
      test_withheld_correction;
    test "an armed run's FAIL titles" test_armed_titles;
    test "hints: no run advertises --failed" test_no_rerun_hint;
    test "the property replay line is the only one" test_property_replay_line;
    test "verbose PASS prints the label table (D5 §7)" test_verbose_pass_labels;
    test "terminal name sanitization (render/F-2)" test_name_sanitization;
    test "the location forms" test_location_forms;
    test "the mark prints only where it aligns" test_mark_criterion;
    test "excerpts resolve against the project root (render/F-1)"
      test_excerpt_project_root;
    test "corrections: the written files, per mode" test_corrections_section;
    test "corrections: the quiet gate and refusals" test_corrections_quiet;
    test "the coverage report's frozen bytes" test_coverage_report_bytes;
    test "the uncovered cell is bounded" test_coverage_uncovered_cap;
    test "the coverage thresholds are the renderer's" test_coverage_thresholds;
    test "mutation: both reports, byte for byte" test_mutation_report;
    test "mutation: the sentence and the executable column"
      test_mutation_sentence;
    test "mutation: the blocks and the summary wear the palette"
      test_mutation_colors;
    test "mutation: summary line forms" test_mutation_summary_forms;
    test "mutation: the reproduce footer" test_mutation_footer;
    test "mutation: sections stand alone" test_mutation_sections;
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
    test "tree-wide summary dialect (harness parity)" test_summary_dialect;
  ]

let () = exit @@ Windtrap.run "report" tests
