(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Render: golden transcripts over a synthetic run covering every
   failure kind (equality with diff, raise, snapshot missing/mismatch,
   property with inner failure, body + teardown pair, captured tail with a
   drop count) at each of the three levels — compact (the default glyph
   row), verbose (line per test), quiet (failures and summary only) — the
   glyph vocabulary and the 60-glyph wrap counter, ANSI styling and diff
   highlighting, ANSI hygiene under ansi:false (payload-borne escapes
   stripped), the live displays, the failure projections (headline,
   pp_failure), degenerate equalities (identical renderings,
   trailing-newline-only differences), diff and proposed-content display
   bounds, duration forms, replay-line quoting and root-token consistency,
   captured-tail bounding, and the source excerpt. Drives [Render] directly
   over synthetic [Run] results; detection goes through string equality and
   containment, so a broken renderer cannot hide its own failure. *)

open Windtrap
open Windtrap.Private
module Fixtures = Render_fixtures

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

(* Drivers *)

let with_renderer ?(ansi = false) ?mode ?live ?columns ?tail_lines
    ?slow_threshold ?invocation fn =
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  let r =
    Render.create ~out:ppf ~ansi ?mode ?live ?columns ?tail_lines
      ?slow_threshold ?invocation ()
  in
  fn r;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let transcript ?ansi ?mode ?live ?invocation ?coverage
    ?(seed = Some Fixtures.root) () =
  with_renderer ?ansi ?mode ?live ?invocation (fun r ->
      Render.header r ~suite:"mylib"
        ~tests:(List.length Fixtures.results)
        ~seed ();
      List.iter
        (fun (res : Run.result) ->
          Render.begin_test r ~path:res.path;
          Render.result r res)
        Fixtures.results;
      Render.finish r ?coverage ~results:Fixtures.results
        ~duration:Fixtures.duration ())

let failure_block ?(ansi = false) ?excerpt ?filter ?invocation f =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Render.pp_failure ~ansi ?excerpt ?filter ?invocation ppf f;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* The golden transcripts

   Reviewed against the RFC "The runner" transcript and the 0.2.0 output
   spec: per-test progress (one glyph in compact, one line with timing in
   verbose), failures re-printed in full inside the labeled rule, bounded
   captured tail with drop count and full-log path, typed-payload-derived
   accept/replay commands, slow warnings, summary, rerun hint, slowest-5
   (verbose only), coverage line. The fixture run is noteworthy from its
   second result, so the compact transcript still opens with the header
   and the glyph row — byte-identical to streaming from the start.

   These are snapshots, not string literals in this file. A transcript IS
   an artifact — box-drawing rules, column alignment, a glyph row, ANSI
   runs — and the reason to keep one is to read the diff when it changes.
   As a literal it could only be reviewed by retyping it; as a baseline
   under __snapshots__/ the review is `git diff` and the acceptance is
   `dune exec test/unit/main.exe -- -u`. Read every accepted diff: this
   is the whole of what a windtrap run prints. *)

let golden_exe = "dune exec test/main.exe --"
let golden_invocation = `Exe golden_exe
let golden_coverage = { Run.visited = 312; total = 358; siblings = false }

let test_golden_compact () =
  let actual =
    transcript ~invocation:golden_invocation ~coverage:golden_coverage ()
  in
  snapshot "compact" actual;
  check_absent "plain transcript has no escape codes" ~sub:"\027" actual

let test_golden_verbose () =
  let actual =
    transcript ~mode:`Verbose ~invocation:golden_invocation
      ~coverage:golden_coverage ()
  in
  snapshot "verbose" actual;
  check_absent "plain transcript has no escape codes" ~sub:"\027" actual

(* The coloured transcript, which had no golden at all: [test_ansi] pins
   nine substrings, so every escape run BETWEEN them was unpinned — and a
   colour bug is exactly a wrong byte next to a right one. Snapshotting
   the whole thing costs one baseline and pins the escapes literally,
   which is the only way to review them. *)
let test_golden_ansi () =
  let actual =
    transcript ~ansi:true ~mode:`Verbose ~invocation:golden_invocation
      ~coverage:golden_coverage ()
  in
  snapshot "verbose-ansi" actual;
  check_contains "the ansi golden really is coloured" ~sub:"\027[" actual

let test_coverage_line_siblings () =
  (* The sibling fact is payload, not filesystem (the driver reads it at
     snapshot time): a summary recording siblings scopes the line to this
     executable and points at the aggregate instead of the report hint. *)
  let t =
    with_renderer (fun r ->
        Render.finish r
          ~results:[ Fixtures.result [ "t" ] Failure.Pass ]
          ~duration:0.1
          ~coverage:{ golden_coverage with Run.siblings = true }
          ())
  in
  check_contains "sibling summary scopes the line and names the aggregate"
    ~sub:
      "coverage: 87.2% (312/358 points, this executable) · project: dune build \
       @cover\n"
    t;
  check_absent "the scoped line drops the report hint"
    ~sub:"WINDTRAP_COVERAGE=report" t

let test_quiet () =
  let t = transcript ~mode:`Quiet () in
  check_absent "quiet: no header" ~sub:"mylib: 11 tests" t;
  check_absent "quiet: no PASS lines" ~sub:"PASS" t;
  check_absent "quiet: no SKIP lines" ~sub:"SKIP" t;
  check_absent "quiet: no glyph row" ~sub:".FFFFFF" t;
  check_absent "quiet: no stream FAIL lines (no timings at all)" ~sub:"0.2ms" t;
  check_absent "quiet: no slowest" ~sub:"slowest tests:" t;
  check_absent "quiet: no slow warnings" ~sub:"slow tests (" t;
  check_contains "quiet: failure blocks survive"
    ~sub:"  FAIL  users › sessions after login\n" t;
  check_contains "quiet: failure rule survives" ~sub:"failures (6)" t;
  check_contains "quiet: summary survives"
    ~sub:"4 passed, 1 skipped, 6 failed in 6.5s." t;
  check "quiet: the blocks open the transcript"
    (String.length t > 0 && t.[0] = '\xe2' (* the failures rule *))

let test_quiet_green_run () =
  let t =
    with_renderer ~mode:`Quiet (fun r ->
        Render.header r ~suite:"s" ~tests:1 ~seed:None ();
        Render.result r (Fixtures.result [ "t" ] Failure.Pass);
        Render.finish r
          ~results:[ Fixtures.result [ "t" ] Failure.Pass ]
          ~duration:0.000619 ())
  in
  check_string "quiet: a green run is exactly the named summary line"
    ~expected:"s: 1 passed in 0.000619s.\n" ~actual:t;
  let unnamed =
    with_renderer ~mode:`Quiet (fun r ->
        Render.result r (Fixtures.result [ "t" ] Failure.Pass);
        Render.finish r
          ~results:[ Fixtures.result [ "t" ] Failure.Pass ]
          ~duration:0.000619 ())
  in
  check_string "quiet: no header seen, summary stays bare"
    ~expected:"1 passed in 0.000619s.\n" ~actual:unnamed

let test_ansi () =
  let t = transcript ~ansi:true ~mode:`Verbose () in
  check_contains "ansi: FAIL tag is red" ~sub:"\027[31mFAIL\027[0m" t;
  check_contains "ansi: PASS tag is green" ~sub:"\027[32mPASS\027[0m" t;
  check_absent "ansi: no marker line" ~sub:"~~~~" t;
  check_contains "ansi: slow entry is faint yellow"
    ~sub:"\027[2m\027[33m  2.50s  slow › big sort\027[0m\027[0m" t;
  check_contains "ansi: slow heading is faint yellow"
    ~sub:"\027[2m\027[33mslow tests (2):\027[0m\027[0m" t;
  check_contains "ansi: slow hint is faint"
    ~sub:"\027[2m(exempt with the \"slow\" tag" t;
  let c = transcript ~ansi:true () in
  check_contains "ansi: pass glyph is green" ~sub:"\027[32m.\027[0m" c;
  check_contains "ansi: fail glyph is red" ~sub:"\027[31mF\027[0m" c;
  check_contains "ansi: skip glyph is yellow" ~sub:"\027[33mS\027[0m" c;
  (* The summary counts wear the same colours as the glyphs above them —
     one convention for the whole transcript, not two for the same run. *)
  check_contains "ansi: summary skip count is yellow"
    ~sub:"\027[33m1 skipped\027[0m" c;
  check_contains "ansi: summary fail count is red"
    ~sub:"\027[31m6 failed\027[0m" c;
  (* The transcript fixture carries no excused result, so the faint count
     gets its own run. *)
  let x =
    with_renderer ~ansi:true (fun r ->
        Render.finish r ~results:[ Fixtures.excused_result ] ~duration:0.1 ())
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
  check_contains "ansi: expected diff span green" ~sub:"\027[32mo\027[0m" b;
  check_contains "ansi: actual diff span red" ~sub:"\027[31ma\027[0m" b;
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
        Render.header r ~suite:"mylib" ~tests:2 ~seed:None ();
        Render.begin_test r ~path:[ "math"; "addition" ];
        Render.result r (List.hd Fixtures.results))
  in
  check_contains "live: progress line drawn"
    ~sub:"Running [1/2] math › addition" t;
  check_contains "live: cursor clear emitted" ~sub:"\r\027[2K" t;
  let plain =
    with_renderer ~ansi:false ~mode:`Verbose ~live:true (fun r ->
        Render.header r ~suite:"mylib" ~tests:2 ~seed:None ();
        Render.begin_test r ~path:[ "math"; "addition" ])
  in
  check_absent "live: off without ansi" ~sub:"Running" plain

let test_live_compact_tail () =
  (* The compact erasable tail works from the start of the run, before any
     noteworthy flush: it draws from column zero, never forces the header
     out, and its erasure re-prints nothing — a green run's screen stays
     blank. *)
  let t =
    with_renderer ~ansi:true ~live:true (fun r ->
        Render.header r ~suite:"mylib" ~tests:2 ~seed:None ();
        Render.begin_test r ~path:[ "math"; "addition" ];
        Render.result r (List.hd Fixtures.results);
        Render.begin_test r ~path:[ "users"; "sessions after login" ])
  in
  check_contains "compact tail: counter and name drawn"
    ~sub:"[1/2] math › addition" t;
  check_absent "compact tail: the tail never forces the header early"
    ~sub:"mylib: 2 tests" t;
  check_contains "compact tail: deferred erase re-prints nothing"
    ~sub:"\027[0m\r\027[2K\027[2m  [2/2]" t;
  check_contains "compact tail: next tail follows from column zero"
    ~sub:"[2/2] users › sessions after login" t;
  (* Once a noteworthy event flushed, the tail follows the committed row
     and its erasure re-prints the row, as always. *)
  let flushed =
    with_renderer ~ansi:true ~live:true (fun r ->
        Render.header r ~suite:"mylib" ~tests:2 ~seed:None ();
        Render.result r
          (Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "b" ]));
        Render.begin_test r ~path:[ "math"; "addition" ];
        Render.result r (List.hd Fixtures.results))
  in
  check_contains "compact tail: flushed erase re-prints the committed row"
    ~sub:"\r\027[2K\027[31mF\027[0m" flushed;
  let plain =
    with_renderer ~ansi:false ~live:true (fun r ->
        Render.header r ~suite:"mylib" ~tests:2 ~seed:None ();
        Render.begin_test r ~path:[ "math"; "addition" ])
  in
  check_absent "compact tail: off without ansi" ~sub:"[1/2]" plain

let test_header_forms () =
  let one =
    with_renderer ~mode:`Verbose (fun r ->
        Render.header r ~suite:"s" ~tests:1 ~seed:None ())
  in
  check_string "header: singular, no seed" ~expected:"s: 1 test\n" ~actual:one;
  let zero =
    with_renderer ~mode:`Verbose (fun r ->
        Render.header r ~suite:"s" ~tests:0 ~seed:None ())
  in
  check_string "header: zero tests" ~expected:"s: 0 tests\n" ~actual:zero;
  (* Compact defers the header until the run proves noteworthy; the same
     line then prints from the recorded fields (the golden transcripts pin
     the flushed form). *)
  let deferred =
    with_renderer (fun r -> Render.header r ~suite:"s" ~tests:1 ~seed:None ())
  in
  check_string "header: compact defers until noteworthy" ~expected:""
    ~actual:deferred

let test_seed_token_consistency () =
  (* Law 7: the replay line prints exactly the token the header printed. *)
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
        Render.result r (Fixtures.result [ "t" ] Failure.Pass ~duration))
  in
  check_contains "minute durations carry seconds overflow" ~sub:"2m0s"
    (line 119.6);
  let summary duration =
    with_renderer (fun r ->
        Render.finish r
          ~results:[ Fixtures.result [ "t" ] Failure.Pass ]
          ~duration ())
  in
  check_contains "summary duration is never scientific" ~sub:"in 5400s."
    (summary 5400.0);
  check_contains "near-zero summary duration is plain" ~sub:"in 0s."
    (summary 2e-05)

let test_create_validation () =
  let raises fn =
    match fn () with
    | (_ : Render.t) -> false
    | exception Invalid_argument _ -> true
  in
  let ppf = Format.formatter_of_buffer (Buffer.create 8) in
  check "create: columns < 20 rejected"
    (raises (fun () -> Render.create ~out:ppf ~ansi:false ~columns:10 ()));
  check "create: negative tail_lines rejected"
    (raises (fun () -> Render.create ~out:ppf ~ansi:false ~tail_lines:(-1) ()));
  check "create: negative slow_threshold rejected"
    (raises (fun () ->
         Render.create ~out:ppf ~ansi:false ~slow_threshold:(-1.0) ()));
  check "create: non-finite slow_threshold rejected"
    (raises (fun () ->
         Render.create ~out:ppf ~ansi:false ~slow_threshold:Float.nan ()))

let test_no_tests () =
  (* No header, so no selection and no declared count: nothing to say
     beyond the fact. *)
  let t =
    with_renderer (fun r -> Render.finish r ~results:[] ~duration:0.01 ())
  in
  check_string "finish: empty run" ~expected:"no tests ran.\n" ~actual:t;
  (* A suite that declares nothing is not a mistyped filter. *)
  let declares_none =
    with_renderer (fun r ->
        Render.header r ~suite:"mylib" ~tests:0 ~declared:0 ~seed:None ();
        Render.finish r ~results:[] ~duration:0.01 ())
  in
  check_string "empty suite names itself as the cause"
    ~expected:"mylib: no tests ran: the suite declares none.\n"
    ~actual:declares_none;
  (* A selection that matched nothing names itself and the denominator,
     and points at the way to see what there was. *)
  let filtered =
    with_renderer (fun r ->
        Render.header r ~suite:"mylib" ~tests:0 ~declared:48
          ~selection:{|filter "parsr"|} ~seed:None ();
        Render.finish r ~results:[] ~duration:0.01 ())
  in
  check_string "empty selection names the selection and the total"
    ~expected:
      "mylib: no tests ran: filter \"parsr\" matched none of 48 tests.\n\
       (list the suite's tests with -l)\n"
    ~actual:filtered;
  let singular =
    with_renderer (fun r ->
        Render.header r ~suite:"mylib" ~tests:0 ~declared:1
          ~selection:"tag \"slow\"" ~seed:None ();
        Render.finish r ~results:[] ~duration:0.01 ())
  in
  check_contains "one declared test is not \"1 tests\""
    ~sub:"matched none of 1 test." singular

(* The compact glyph row *)

let test_glyph_vocabulary () =
  (* A counted failure first flushes the deferred transcript, so the
     probed glyph streams; the leading [F] is dropped below. *)
  let glyph result () =
    let flushed =
      with_renderer (fun r ->
          Render.result r
            (Fixtures.result [ "!" ] (Failure.Fail [ Failure.message "x" ]));
          Render.result r result)
    in
    String.sub flushed 1 (String.length flushed - 1)
  in
  check_string "a buffered green glyph prints nothing until noteworthy"
    ~expected:""
    ~actual:
      (with_renderer (fun r ->
           Render.result r (Fixtures.result [ "t" ] Failure.Pass)));
  check_string "glyph: pass is a dot" ~expected:"."
    ~actual:(glyph (Fixtures.result [ "t" ] Failure.Pass) ());
  check_string "glyph: counted failure is F" ~expected:"F"
    ~actual:
      (glyph
         (Fixtures.result [ "t" ] (Failure.Fail [ Failure.message "b" ]))
         ());
  check_string "glyph: skip is S" ~expected:"S"
    ~actual:(glyph (Fixtures.result [ "t" ] (Failure.Skip None)) ());
  check_string "glyph: expected failure is x" ~expected:"x"
    ~actual:(glyph Fixtures.excused_result ());
  check_string "glyph: an xfail annotation on a pass changes nothing"
    ~expected:"."
    ~actual:
      (glyph
         (Fixtures.result [ "t" ] Failure.Pass ~xfail:Fixtures.xfail_reason)
         ());
  (* Unexpected pass: an ordinary counted failure, an ordinary F — the
     record arrives counted even though it carries the annotation. *)
  check_string "glyph: unexpected pass is a loud F" ~expected:"F"
    ~actual:(glyph Fixtures.xpass_result ())

let test_glyph_wrap () =
  (* [n] passes buffer (wraps and counters included), then one counted
     failure flushes: the committed bytes must equal what streaming from
     the start would have printed. *)
  let results n =
    List.init n (fun i -> Fixtures.result [ string_of_int i ] Failure.Pass)
    @ [ Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "b" ]) ]
  in
  let run ~header n =
    let rs = results n in
    with_renderer (fun r ->
        if header then Render.header r ~suite:"s" ~tests:(n + 1) ~seed:None ();
        List.iter (Render.result r) rs;
        Render.finish r ~results:rs ~duration:0.01 ())
  in
  let t = run ~header:true 70 in
  check "wrap: the flush opens with the deferred header"
    (String.starts_with ~prefix:"s: 71 tests\n" t);
  check_contains "wrap: buffered full row carries the faint [k/n] counter"
    ~sub:(String.make 60 '.' ^ " [60/71]\n")
    t;
  check_contains "wrap: partial row closed by a newline before the failures"
    ~sub:("\n" ^ String.make 10 '.' ^ "F\n────")
    t;
  check_contains "wrap: the summary counts the run" ~sub:"70 passed, 1 failed" t;
  let bare = run ~header:false 60 in
  check_contains "wrap: bare newline when the total is unknown"
    ~sub:(String.make 60 '.' ^ "\nF")
    bare;
  check_absent "wrap: no counter without a total" ~sub:"[60/" bare;
  let exact = run ~header:true 59 in
  check_contains "wrap: an exact row wraps once, no empty row"
    ~sub:" [60/60]\n────" exact

let test_glyph_row_before_failures () =
  let results =
    [
      Fixtures.result [ "ok" ] Failure.Pass;
      Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "boom" ]);
    ]
  in
  let t =
    with_renderer (fun r ->
        Render.header r ~suite:"s" ~tests:2 ~seed:None ();
        List.iter (Render.result r) results;
        Render.finish r ~results ~duration:0.01 ())
  in
  check "compact: the failure flushes header then the accumulated row"
    (String.starts_with ~prefix:"s: 2 tests\n.F\n" t);
  check_contains "compact: partial row closed before the failure rule"
    ~sub:".F\n────" t

let test_note () =
  (* Run-scoped notices (fixture releases) land between results, while a
     compact row can still be open: the row closes first. On a still
     deferred transcript the notice buffers with the row — a green run
     keeps its one-line transcript, a noteworthy one shows the notice in
     position. *)
  let green =
    with_renderer (fun r ->
        Render.header r ~suite:"s" ~tests:2 ~seed:None ();
        Render.result r (Fixtures.result [ "a" ] Failure.Pass);
        Render.result r (Fixtures.result [ "b" ] Failure.Pass);
        Render.note r "releasing db";
        Render.finish r
          ~results:
            [
              Fixtures.result [ "a" ] Failure.Pass;
              Fixtures.result [ "b" ] Failure.Pass;
            ]
          ~duration:0.01 ())
  in
  check_string "note: a green compact run stays one line"
    ~expected:"s: 2 passed in 0.01s.\n" ~actual:green;
  let noteworthy =
    let results =
      [
        Fixtures.result [ "a" ] Failure.Pass;
        Fixtures.result [ "b" ] (Failure.Fail [ Failure.message "boom" ]);
      ]
    in
    with_renderer (fun r ->
        Render.header r ~suite:"s" ~tests:2 ~seed:None ();
        Render.result r (List.nth results 0);
        Render.note r "releasing db";
        Render.result r (List.nth results 1);
        Render.finish r ~results ~duration:0.01 ())
  in
  check_contains "note: a later flush shows the buffered notice in position"
    ~sub:"s: 2 tests\n.\nreleasing db\nF\n" noteworthy;
  let flushed =
    with_renderer (fun r ->
        Render.header r ~suite:"s" ~tests:2 ~seed:None ();
        Render.result r
          (Fixtures.result [ "a" ] (Failure.Fail [ Failure.message "x" ]));
        Render.result r (Fixtures.result [ "b" ] Failure.Pass);
        Render.note r "releasing db")
  in
  check_contains "note: a flushed compact row closes before the notice"
    ~sub:"F.\nreleasing db\n" flushed;
  let verbose =
    with_renderer ~mode:`Verbose (fun r -> Render.note r "releasing db")
  in
  check_string "note: verbose prints the plain line" ~expected:"releasing db\n"
    ~actual:verbose;
  let quiet =
    with_renderer ~mode:`Quiet (fun r -> Render.note r "releasing db")
  in
  check_string "note: suppressed under quiet" ~expected:"" ~actual:quiet;
  let live =
    with_renderer ~ansi:true ~live:true (fun r ->
        Render.header r ~suite:"s" ~tests:2 ~seed:None ();
        Render.result r (Fixtures.result [ "a" ] Failure.Pass);
        Render.begin_test r ~path:[ "b" ];
        Render.note r "releasing db";
        Render.finish r
          ~results:[ Fixtures.result [ "a" ] Failure.Pass ]
          ~duration:0.01 ())
  in
  check_contains "note: deferred live tail erased, notice drawn erasable"
    ~sub:"\r\027[2K\027[2mreleasing db\027[0m" live;
  check_contains "note: the erasable notice is erased before the one-liner"
    ~sub:"releasing db\027[0m\r\027[2Ks: \027[32m1 passed" live

(* The noteworthy rule *)

let test_compact_green_one_liner () =
  let passes = [ Fixtures.result [ "a" ] Failure.Pass ] in
  let named =
    with_renderer (fun r ->
        Render.header r ~suite:"mylib" ~tests:1 ~seed:None ();
        List.iter (Render.result r) passes;
        Render.finish r ~results:passes ~duration:1.2 ())
  in
  check_string "green compact run: exactly one named line"
    ~expected:"mylib: 1 passed in 1.2s.\n" ~actual:named;
  let seeded =
    with_renderer (fun r ->
        Render.header r ~suite:"mylib" ~tests:1 ~seed:(Some Fixtures.root) ();
        List.iter (Render.result r) passes;
        Render.finish r ~results:passes ~duration:1.2 ())
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
        Render.header r ~suite:"mylib" ~tests:3 ~seed:None ();
        Render.result r (List.nth results 0);
        Render.result r (List.nth results 1);
        Render.result r (List.nth results 2);
        Render.finish r ~results ~duration:0.2 ())
  in
  check_string
    "green compact run: skip and expected-failure segments stay on the line"
    ~expected:"mylib: 1 passed, 1 skipped, 1 expected failure in 0.2s.\n"
    ~actual:segments;
  let empty =
    with_renderer (fun r ->
        Render.header r ~suite:"mylib" ~tests:0 ~seed:None ();
        Render.finish r ~results:[] ~duration:0.01 ())
  in
  (* [~declared] defaults to [~tests], which is 0 here: the suite really
     does declare nothing. *)
  check_string "empty compact selection: one named line, no header"
    ~expected:"mylib: no tests ran: the suite declares none.\n" ~actual:empty

let test_compact_flush_streams_after () =
  (* The buffered rows commit on the first noteworthy event; subsequent
     glyphs stream immediately, per glyph. *)
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let r = Render.create ~out:ppf ~ansi:false () in
  let so_far () =
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  Render.header r ~suite:"s" ~tests:4 ~seed:None ();
  Render.result r (Fixtures.result [ "a" ] Failure.Pass);
  Render.result r (Fixtures.result [ "b" ] Failure.Pass);
  check_string "before the flush nothing is committed" ~expected:""
    ~actual:(so_far ());
  Render.result r
    (Fixtures.result [ "c" ] (Failure.Fail [ Failure.message "x" ]));
  check_string "the first counted failure commits header, rows, and itself"
    ~expected:"s: 4 tests\n..F" ~actual:(so_far ());
  Render.result r (Fixtures.result [ "d" ] Failure.Pass);
  check_string "later glyphs stream live" ~expected:"s: 4 tests\n..F."
    ~actual:(so_far ())

let test_compact_slow_trigger () =
  let slow_pass = Fixtures.result [ "t" ] Failure.Pass ~duration:1.2 in
  let t =
    with_renderer (fun r ->
        Render.header r ~suite:"s" ~tests:1 ~seed:None ();
        Render.result r slow_pass;
        Render.finish r ~results:[ slow_pass ] ~duration:1.2 ())
  in
  check_string
    "an untagged over-threshold pass is noteworthy: header, row, warning"
    ~expected:
      "s: 1 test\n\
       .\n\
       slow tests (1):\n\
      \  1.20s  t\n\
       (exempt with the \"slow\" tag, or raise WINDTRAP_SLOW_THRESHOLD)\n\n\
       1 passed in 1.2s.\n"
    ~actual:t;
  let at_threshold =
    let r1 = Fixtures.result [ "t" ] Failure.Pass ~duration:1.0 in
    with_renderer (fun r -> Render.result r r1)
  in
  check_string "the threshold is inclusive (duration >= threshold)"
    ~expected:"." ~actual:at_threshold;
  let tagged_pass = { slow_pass with Run.slow_tagged = true } in
  let tagged =
    with_renderer (fun r ->
        Render.header r ~suite:"s" ~tests:1 ~seed:None ();
        Render.result r tagged_pass;
        Render.finish r ~results:[ tagged_pass ] ~duration:1.2 ())
  in
  check_string "a slow-tagged test is exempt everywhere: one line, no warning"
    ~expected:"s: 1 passed in 1.2s.\n" ~actual:tagged;
  let skip = Fixtures.result [ "t" ] (Failure.Skip None) ~duration:2.0 in
  let skipped =
    with_renderer (fun r ->
        Render.header r ~suite:"s" ~tests:1 ~seed:None ();
        Render.result r skip;
        Render.finish r ~results:[ skip ] ~duration:2.0 ())
  in
  check_string "a skip never triggers the threshold"
    ~expected:"s: 1 skipped in 2s.\n" ~actual:skipped;
  (* An excused expected failure is not a counted failure — but its
     duration still counts against the threshold when untagged. *)
  let excused_fast =
    with_renderer (fun r ->
        Render.header r ~suite:"s" ~tests:1 ~seed:None ();
        Render.result r Fixtures.excused_result;
        Render.finish r ~results:[ Fixtures.excused_result ] ~duration:0.1 ())
  in
  check_string "an excused failure alone is not noteworthy"
    ~expected:"s: 1 expected failure in 0.1s.\n" ~actual:excused_fast

let test_slow_duration_semantics () =
  (* The compared duration is [Run.result.duration] — the attempts summed
     (run.mli) — so a retried test whose attempts together cross the
     threshold is slow even when its final attempt was fast. *)
  let retried =
    Fixtures.result [ "flaky" ] Failure.Pass ~duration:1.2 ~attempts:3
  in
  let t =
    with_renderer (fun r ->
        Render.header r ~suite:"s" ~tests:1 ~seed:None ();
        Render.result r retried;
        Render.finish r ~results:[ retried ] ~duration:1.2 ())
  in
  check "a retried test is noteworthy on its summed duration"
    (String.starts_with ~prefix:"s: 1 test\n" t);
  check_contains "the warning shows the summed duration" ~sub:"  1.20s  flaky" t;
  (* A slow test that also fails: one block and one warning — they report
     different things — and the summary counts the failure once. *)
  let slow_fail =
    Fixtures.result [ "boom" ]
      (Failure.Fail [ Failure.message "b" ])
      ~duration:2.0
  in
  let t =
    with_renderer (fun r ->
        Render.header r ~suite:"s" ~tests:1 ~seed:None ();
        Render.result r slow_fail;
        Render.finish r ~results:[ slow_fail ] ~duration:2.0 ())
  in
  check_contains "a slow failing test keeps its failure block"
    ~sub:"failures (1)" t;
  check_contains "the warning follows the blocks, before the summary"
    ~sub:"slow tests (1):\n  2.00s  boom\n(exempt with" t;
  check_contains "the failure is counted once" ~sub:"1 failed in 2s." t;
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
    (occurrences ~sub:"  2.00s  boom" t = 1)

let test_slow_threshold_zero () =
  let slow_pass = Fixtures.result [ "t" ] Failure.Pass ~duration:5.0 in
  let t =
    with_renderer ~slow_threshold:0.0 (fun r ->
        Render.header r ~suite:"s" ~tests:1 ~seed:None ();
        Render.result r slow_pass;
        Render.finish r ~results:[ slow_pass ] ~duration:5.0 ())
  in
  check_string "threshold 0 disables the trigger and the warnings"
    ~expected:"s: 1 passed in 5s.\n" ~actual:t;
  let still_flushes =
    let fail = Fixtures.result [ "t" ] (Failure.Fail [ Failure.message "x" ]) in
    with_renderer ~slow_threshold:0.0 (fun r ->
        Render.header r ~suite:"s" ~tests:1 ~seed:None ();
        Render.result r fail)
  in
  check_string "threshold 0 still flushes on a counted failure"
    ~expected:"s: 1 test\nF" ~actual:still_flushes

let test_verbose_slow_warnings () =
  (* Verbose gains the warning lines (before the summary) and keeps the
     slowest list; a green verbose run still streams everything. *)
  let slow_pass = Fixtures.result [ "t" ] Failure.Pass ~duration:1.5 in
  let t =
    with_renderer ~mode:`Verbose (fun r ->
        Render.header r ~suite:"s" ~tests:1 ~seed:None ();
        Render.result r slow_pass;
        Render.finish r ~results:[ slow_pass ] ~duration:1.5 ())
  in
  check_contains "verbose: header and status line stream as always"
    ~sub:"s: 1 test\n  PASS  t" t;
  check_contains "verbose: slow warning before the summary"
    ~sub:"slow tests (1):\n  1.50s  t\n(exempt with" t;
  check_contains "verbose: summary follows the hint"
    ~sub:"WINDTRAP_SLOW_THRESHOLD)\n\n1 passed in 1.5s.\n" t;
  let tagged_pass = { slow_pass with Run.slow_tagged = true } in
  let tagged =
    with_renderer ~mode:`Verbose (fun r ->
        Render.result r tagged_pass;
        Render.finish r ~results:[ tagged_pass ] ~duration:1.5 ())
  in
  check_absent "verbose: slow-tagged tests warn nowhere" ~sub:"slow tests ("
    tagged

(* Failure projections *)

let test_headline () =
  let h f = Render.headline f in
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
  check "headline: uncaught exception"
    (h (Failure.raised ~actual:"Not_found" ()) = "uncaught exception: Not_found");
  check "headline: raise wanted any"
    (h (Failure.raised ()) = "expected an exception, none raised");
  check "headline: snapshot missing"
    (h Fixtures.snap_missing = {|snapshot "help": no baseline|});
  check "headline: snapshot mismatch"
    (h Fixtures.snap_mismatch = {|snapshot "version": mismatch|});
  check "headline: property"
    (h Fixtures.prop_failure
   = "property failed (case 12, shrunk 4 steps): Rect (2, 0)");
  check "headline: message" (h (Failure.message "boom") = "boom");
  check_contains "headline: msg annotation prefixed" ~sub:"context — boom"
    (h { (Failure.message "boom") with Failure.msg = Some "context" });
  let long = String.make 300 'x' in
  let hl = h (Failure.equality ~expected:long ~actual:"y" ()) in
  check "headline: long payloads truncated"
    (String.length hl < 200 && has ~sub:"..." hl);
  let multi = h (Failure.message "line one\nline two") in
  check "headline: never multi-line" (not (String.contains multi '\n'));
  let esc = h (Failure.message "\027[31mred\027[0m alert") in
  check "headline: payload escapes stripped" (esc = "red alert");
  check "headline: empty message named"
    (h (Failure.message "") = "(empty failure message)");
  check_contains "block: empty message named" ~sub:"(empty failure message)"
    (failure_block (Failure.message ""))

(* The --strict-snapshots verdict's projection: the payload carries the
   orphan paths and nothing else; the [stale baseline:] lines and the
   removal hint — a command hint like any other — are spelled here, at
   render time, from the invocation. The same producer feeds Driver's
   advisory block, so the failure section and the advisory cannot drift. *)
let test_stale_baselines_projections () =
  let f = Failure.stale_baselines [ "/tmp/a.snap"; "/tmp/b.snap" ] in
  check_string "block under Exe: the files, then the way out"
    ~expected:
      (Printf.sprintf
         "    stale baseline: %s\n\
         \    stale baseline: %s\n\
         \    remove stale baselines: ./t.exe -u --prune\n"
         (Path_ops.display "/tmp/a.snap")
         (Path_ops.display "/tmp/b.snap"))
    ~actual:(failure_block ~invocation:(`Exe "./t.exe") f);
  check_contains "block under Mirrors: the hint spells the mirrors"
    ~sub:
      "remove stale baselines: WINDTRAP_UPDATE=1 WINDTRAP_PRUNE=1 dune runtest"
    (failure_block f);
  (* The line producers are the exported pair Driver's advisory block
     prints — one spelling. *)
  check "stale_lines_with_hint is stale_lines plus the hint"
    (Render.stale_lines_with_hint ~invocation:(`Exe "./t.exe") [ "/tmp/a.snap" ]
    = Render.stale_lines [ "/tmp/a.snap" ]
      @ [ "remove stale baselines: ./t.exe -u --prune" ]);
  (* The headline flattens the block's lines whole, hint included, into
     the one-line bound (a short path, so nothing truncates here; real
     baseline paths push the invocation-specific tail past the bound). *)
  check_string "headline: files and hint flattened, invocation spelled"
    ~expected:"stale baseline: a remove stale baselines: ./t -u --prune"
    ~actual:
      (Render.headline ~invocation:(`Exe "./t")
         (Failure.stale_baselines [ "a" ]));
  let long = Render.headline (Failure.stale_baselines [ "/tmp/a.snap" ]) in
  check "headline: never multi-line, bounded"
    ((not (String.contains long '\n')) && has ~sub:"..." long)

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
  (* A printerless counterexample is a placeholder, and the block says so
     once — under the counterexample, whichever placeholder shape the engine
     produced ([<no printer>], [<example k>]). A printing generator must
     never draw the advice. *)
  let printerless =
    Failure.property ~rendered:"<no printer>" ~case_index:19 ~shrink_steps:9
      ~root:Fixtures.root ~examples:false ~printerless:true ()
  in
  let hint = "attach one with Gen.with_pp" in
  check_contains "printerless: names the remedy" ~sub:hint
    (failure_block printerless);
  check_contains "printerless: keeps the placeholder rendering"
    ~sub:"counterexample (case 19, shrunk 9 steps): <no printer>"
    (failure_block printerless);
  check_absent "printerless: advice is not repeated" ~sub:"add Gen.with_pp>"
    (failure_block printerless);
  check_absent "printing generator: no remedy line" ~sub:hint
    (failure_block example);
  check_absent "printing generator: no remedy line either" ~sub:hint
    (failure_block Fixtures.prop_failure);
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
  (* The shrink budget rides the payload on the same terms and restates
     itself for the same reason: a replay under a different budget stops the
     descent at a different node, so the counterexample it prints is not the
     one being replayed. An engine-default budget needs no flag. *)
  let budgeted =
    Failure.property ~count:1000 ~max_shrink:50 ~rendered:"0" ~case_index:499
      ~shrink_steps:50 ~shrink_exhausted:true ~root:Fixtures.root
      ~examples:false ()
  in
  check_contains "config-sourced budget: Mirrors replay restates the mirror"
    ~sub:
      "replay: WINDTRAP_SEED=s1:7be1d2c904aa31f5 WINDTRAP_PROP_COUNT=1000 \
       WINDTRAP_MAX_SHRINK=50 dune runtest"
    (failure_block budgeted);
  check_contains "config-sourced budget: Exe replay restates --max-shrink"
    ~sub:
      "replay: ./t.exe --seed s1:7be1d2c904aa31f5 --prop-count 1000 \
       --max-shrink 50 -f 'late'"
    (failure_block ~invocation:(`Exe "./t.exe") ~filter:"late" budgeted);
  check_absent "engine-default budget: no flag" ~sub:"--max-shrink"
    (failure_block ~invocation:(`Exe "./t.exe") Fixtures.prop_failure);
  check_absent "engine-default budget: no mirror" ~sub:"WINDTRAP_MAX_SHRINK"
    no_filter;
  let multi =
    failure_block
      (Failure.property ~rendered:"Rect\n  (2, 0)" ~case_index:3 ~shrink_steps:0
         ~root:Fixtures.root ~examples:false ())
  in
  check_contains "multi-line counterexample: block form"
    ~sub:"counterexample (case 3):\n      Rect\n        (2, 0)" multi

let test_kind_details () =
  let b =
    failure_block (Failure.with_phase Failure.Teardown (Failure.message "x"))
  in
  check_contains "teardown phase labeled" ~sub:"[teardown]" b;
  let b =
    failure_block
      (Failure.equality ~msg:"context note" ~expected:"1" ~actual:"2" ())
  in
  check_contains "msg annotation printed" ~sub:"    context note\n" b;
  let b =
    failure_block
      (Failure.snapshot ~name:"n" ~path:"some/candidate" Failure.Unresolvable)
  in
  check_contains "unresolvable: RFC message"
    ~sub:{|snapshot "n": cannot resolve a source file — pass ~pos:__POS__|} b;
  check_contains "unresolvable: candidate path shown"
    ~sub:"unverified path: some/candidate" b;
  let b =
    failure_block
      (Failure.snapshot ~name:"n" ~path:"p"
         (Failure.Duplicate
            {
              first = Some (Fixtures.loc "test/a.ml" 3);
              first_test = "g › first";
            }))
  in
  check_contains "duplicate: first site shown"
    ~sub:{|snapshot "n": duplicate name — first checked at test/a.ml:3|} b;
  let b =
    failure_block
      (Failure.snapshot ~name:"n" ~path:"p"
         (Failure.Duplicate { first = None; first_test = "g › first" }))
  in
  check_contains "duplicate without a site renders the first checking test"
    ~sub:{|snapshot "n": duplicate name — first checked by "g › first"|} b;
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
  check_contains "trailing-newline-only difference is stated"
    ~sub:"differ only by a trailing newline" b;
  check_contains "trailing-newline side is named" ~sub:"actual" b;
  check_absent "trailing-newline case prints no empty diff" ~sub:"--- expected"
    b;
  (* Renderings byte-equal while the equality distinguishes (lossy pp, e.g.
     [equal float nan nan]): two identical lines need an explanation. *)
  let b = failure_block (Failure.equality ~expected:"nan" ~actual:"nan" ()) in
  check_contains "identical renderings are called out" ~sub:"render identically"
    b;
  (* Identical and multi-line: printed once, in block form — inlining after
     an [expected] label would put continuation lines at column zero. *)
  let b =
    failure_block
      (Failure.equality ~expected:"line a\nline b" ~actual:"line a\nline b" ())
  in
  check_contains "identical multi-line renderings print once, indented"
    ~sub:"    both render as:\n      line a\n      line b\n" b;
  check_contains "identical multi-line explanation retained"
    ~sub:"render identically" b;
  check_absent "identical multi-line has no column-zero payload line"
    ~sub:"\nline b" b

let test_ansi_hygiene () =
  (* User pp output may carry raw escapes; under [ansi:false] the transcript
     must contain none (render.mli), under [ansi:true] they pass through. *)
  let esc = "\027[31mred\027[0m" in
  let f =
    Failure.equality ~expected:(esc ^ " one") ~actual:"\027]0;title\007 two" ()
  in
  let plain = failure_block f in
  check_absent "ansi:false: payload escapes stripped from blocks" ~sub:"\027"
    plain;
  check_contains "ansi:false: stripped payload text survives" ~sub:"red one"
    plain;
  let colored = failure_block ~ansi:true (Failure.message (esc ^ " boom")) in
  check_contains "ansi:true: payload escapes pass through" ~sub:esc colored;
  let hostile_line =
    with_renderer ~mode:`Verbose (fun r ->
        Render.result r
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
        Render.finish r ~results:[ result ] ~duration:0.01 ())
  in
  check_absent "ansi:false: captured tail stripped" ~sub:"\027" hostile_tail;
  check_contains "ansi:false: stripped tail text survives" ~sub:" captured"
    hostile_tail

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
      (Failure.snapshot ~name:"big" ~path:"p.snap"
         (Failure.Mismatch
            { expected = text "e" ^ "\n"; actual = text "a" ^ "\n" }))
  in
  check_contains "snapshot diff truncation mark" ~sub:"more diff lines)" snap;
  check_contains "acceptance survives a truncated diff"
    ~sub:"accept: WINDTRAP_UPDATE=1" snap

let test_proposed_truncation () =
  let proposed =
    String.concat "" (List.init 25 (fun i -> Printf.sprintf "line %d\n" i))
  in
  let b =
    failure_block
      (Failure.snapshot ~name:"big" ~path:"p.snap"
         (Failure.Missing { proposed }))
  in
  check_contains "proposed content bounded with a mark" ~sub:"(+5 more lines)" b;
  check_absent "proposed lines over the bound absent" ~sub:"line 24" b;
  check_contains "acceptance survives a bounded proposal"
    ~sub:"accept: WINDTRAP_UPDATE=1" b

let test_excerpt () =
  (* The excerpt source is generated in the test's scratch directory: the
     renderer reads it back through the failure's location. *)
  let file = Filename.concat (temp_dir ()) "excerpt_src.ml" in
  Out_channel.with_open_bin file (fun oc ->
      output_string oc "let one = 1\nlet two = 2\nlet three = 3\n");
  let f =
    Failure.equality
      ~loc:{ Loc.file; line = 2; column = 0 }
      ~expected:"1" ~actual:"2" ()
  in
  check_contains "excerpt: source line read from disk" ~sub:"2 │ let two = 2"
    (failure_block ~excerpt:true f);
  check_absent "excerpt: off by default" ~sub:"let two" (failure_block f);
  let gone =
    Failure.equality
      ~loc:{ Loc.file = "does_not_exist.ml"; line = 2; column = 0 }
      ~expected:"1" ~actual:"2" ()
  in
  check_contains "excerpt: unreadable file silent"
    ~sub:"does_not_exist.ml:2\n    expected"
    (failure_block ~excerpt:true gone)

(* Captured tail *)

let tail_block ?tail_lines tail =
  let result =
    Fixtures.result [ "t" ]
      (Failure.Fail [ Failure.with_output_tail tail (Failure.message "boom") ])
  in
  with_renderer ?tail_lines (fun r ->
      Render.finish r ~results:[ result ] ~duration:0.01 ())

let test_tail () =
  let five = Failure.tail "l1\nl2\nl3\nl4\nl5\n" in
  let b = tail_block ~tail_lines:2 five in
  check_contains "tail: line-bounded heading"
    ~sub:"── captured output (last 2 of 5 lines) ──" b;
  check_contains "tail: last lines shown" ~sub:"    l4\n    l5\n" b;
  check_absent "tail: earlier lines dropped" ~sub:"l3" b;
  let full = Failure.tail ~log_path:"log.output" "only\n" in
  let b = tail_block full in
  check_contains "tail: complete output heading"
    ~sub:"── captured output (1 line) ──" b;
  check_contains "tail: full log path" ~sub:"full log: log.output" b;
  let dropped = Failure.tail ~omitted_bytes:512 "kept\n" in
  check_contains "tail: drop count reported"
    ~sub:"── captured output (last 1 line, 512 earlier bytes omitted) ──"
    (tail_block dropped)

(* Sequence summaries (amendment B7) *)

let render_testable w v = Testable.to_string w v

let test_sequence_summary () =
  let expected = List.init 100 (fun i -> i) in
  let actual =
    List.map
      (fun i -> if i = 37 || i = 70 || i = 71 then i + 1000 else i)
      expected
  in
  let b =
    failure_block
      (Failure.equality
         ~expected:(render_testable Testable.(list int) expected)
         ~actual:(render_testable Testable.(list int) actual)
         ())
  in
  check_contains "sequence summary: count and first index"
    ~sub:
      "lists differ at 3 of 100 elements; first at [37]: expected 37, actual \
       1037"
    b;
  check_contains "sequence summary: detailed diff still follows"
    ~sub:"--- expected" b;
  let arr =
    failure_block
      (Failure.equality
         ~expected:
           (render_testable Testable.(array int) (Array.init 10 (fun i -> i)))
         ~actual:
           (render_testable
              Testable.(array int)
              (Array.init 10 (fun i -> if i = 2 then 9 else i)))
         ())
  in
  check_contains "sequence summary: arrays wording"
    ~sub:"arrays differ at 1 of 10 elements; first at [2]: expected 2, actual 9"
    arr

let test_sequence_summary_length () =
  let b =
    failure_block
      (Failure.equality
         ~expected:(render_testable Testable.(list int) (List.init 12 Fun.id))
         ~actual:(render_testable Testable.(list int) (List.init 9 Fun.id))
         ())
  in
  check_contains "sequence summary: length difference"
    ~sub:"lists differ in length: expected 12 elements, actual 9" b;
  (* Aligned counting (D5 §3): a length difference now names its first
     unmatched element too — a truthful addition to the length wording. *)
  check_contains "sequence summary: length difference names the first extra"
    ~sub:
      "lists differ in length: expected 12 elements, actual 9; first at [9]: \
       expected 9, not in actual"
    b;
  (* Lengths differ and a pair below the shorter length differs too: the
     length form keeps the first mismatch. *)
  let b =
    failure_block
      (Failure.equality
         ~expected:(render_testable Testable.(list int) (List.init 12 Fun.id))
         ~actual:
           (render_testable
              Testable.(list int)
              (List.init 9 (fun i -> if i = 3 then 99 else i)))
         ())
  in
  check_contains "sequence summary: length difference keeps the first mismatch"
    ~sub:
      "lists differ in length: expected 12 elements, actual 9; first at [3]: \
       expected 3, actual 99"
    b;
  (* The empty side: an honest length statement, no invented index. *)
  let b =
    failure_block
      (Failure.equality ~expected:"[]"
         ~actual:(render_testable Testable.(list int) (List.init 100 Fun.id))
         ())
  in
  check_contains "sequence summary: empty side states lengths"
    ~sub:
      "lists differ in length: expected 0 elements, actual 100; first at [0]: \
       actual 0, not in expected"
    b

(* The usage-review evidence shape (SYNTHESIS #6 — rune's [check_arr] loops
   exist to report "index 37"): a float array under a tolerance witness, one
   bad index among 100. The summary line is what retires that wrapper layer. *)
let test_sequence_summary_float_tolerance_arrays () =
  let expected = Array.init 100 (fun i -> float_of_int i /. 7.) in
  let actual = Array.copy expected in
  actual.(37) <- actual.(37) +. 0.5;
  let b =
    failure_block
      (Failure.equality
         ~expected:(render_testable Testable.(array (float 1e-6)) expected)
         ~actual:(render_testable Testable.(array (float 1e-6)) actual)
         ())
  in
  check_contains "tolerance-array failure names the failing index"
    ~sub:"arrays differ at 1 of 100 elements; first at [37]" b

let test_sequence_summary_threshold () =
  let b =
    failure_block
      (Failure.equality
         ~expected:(render_testable Testable.(list int) [ 1; 2; 3 ])
         ~actual:(render_testable Testable.(list int) [ 1; 9; 3 ])
         ())
  in
  check_absent "sequence summary: below the threshold" ~sub:"differ at" b

let test_sequence_summary_bounded_elements () =
  let big prefix = String.make 200 prefix in
  let expected = List.init 20 (fun i -> Printf.sprintf "row-%d" i) in
  let actual =
    List.map (fun s -> if s = "row-5" then big 'x' else s) expected
  in
  let b =
    failure_block
      (Failure.equality
         ~expected:(render_testable Testable.(list string) expected)
         ~actual:(render_testable Testable.(list string) actual)
         ())
  in
  (* The summary line shows a bounded excerpt: [seq_element_display] code
     points including the ellipsis, which is inside the budget rather than
     added to it. The full element still appears in the detailed diff. *)
  check_contains "sequence summary: long element truncated with an ellipsis"
    ~sub:("actual \"" ^ String.make 36 'x' ^ "...")
    b

(* Exception message diffs (amendment B1) *)

let test_raise_message_diff () =
  let b = failure_block Fixtures.raise_message_failure in
  check_contains "raise: constructor named once"
    ~sub:"raised Invalid_argument with the wrong message:" b;
  check_contains "raise: messages diffed as strings"
    ~sub:
      "expected  \"index 3 out of bounds\"\n\
      \                     ~\n\
      \    actual    \"index 4 out of bounds\"\n\
      \                     ~"
    b;
  check_contains "raise: marker under the changed span" ~sub:"~" b;
  check_absent "raise: constructor not repeated on both sides"
    ~sub:"expected exception" b;
  let colored = failure_block ~ansi:true Fixtures.raise_message_failure in
  check_contains "raise: message diff highlighted under ansi" ~sub:"\027[31m"
    colored

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
    ~sub:"expected exception" (failure_block no_diff);
  check_absent "raise: no message diff without the payload"
    ~sub:"with the wrong message" (failure_block no_diff);
  (* raises_match's payload still prints the raised exception. *)
  let predicate_miss =
    Failure.raised ~actual:{|Invalid_argument("nope")|} ~predicate:true ()
  in
  check_contains "raises_match: actually-raised exception printed"
    ~sub:
      "raised exception does not satisfy the predicate:\n\
      \      Invalid_argument(\"nope\")"
    (failure_block predicate_miss)

(* Expected failures (amendment B12) *)

let test_xfail_line () =
  let line =
    with_renderer ~mode:`Verbose (fun r ->
        Render.result r Fixtures.excused_result)
  in
  check_contains "xfail line: XFAIL tag and reason"
    ~sub:"  XFAIL  known › broken carry (expected failure: issue #42)" line;
  check_absent "xfail line: not a FAIL" ~sub:"  FAIL  " line;
  let no_reason =
    with_renderer ~mode:`Verbose (fun r ->
        Render.result r
          {
            Fixtures.excused_result with
            Run.xfail = Some { Test_tree.reason = None };
          })
  in
  check_contains "xfail line: reasonless form" ~sub:"(expected failure)"
    no_reason;
  let quiet =
    with_renderer ~mode:`Quiet (fun r ->
        Render.result r Fixtures.excused_result)
  in
  check_string "xfail line: suppressed under quiet" ~expected:"" ~actual:quiet;
  let pass_ignores =
    with_renderer ~mode:`Verbose (fun r ->
        Render.result r
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
  let stream =
    with_renderer (fun r ->
        (* Flush with an unrelated counted failure so the probed glyph
           commits (the glyph-vocabulary driver's trick). *)
        Render.result r
          (Fixtures.result [ "!" ] (Failure.Fail [ Failure.message "x" ]));
        Render.result r collide)
  in
  check_string "collision record still streams the excused glyph" ~expected:"Fx"
    ~actual:stream;
  let verbose =
    with_renderer ~mode:`Verbose (fun r -> Render.result r collide)
  in
  check_contains "collision record renders XFAIL, not FAIL" ~sub:"  XFAIL  "
    verbose;
  check_absent "collision record: no loud FAIL line" ~sub:"  FAIL  " verbose;
  let summary =
    with_renderer (fun r ->
        Render.header r ~suite:"s" ~tests:1 ~seed:None ();
        Render.result r collide;
        Render.finish r ~results:[ collide ] ~duration:0.1 ())
  in
  check_string "collision record: stream, summary, and count agree"
    ~expected:"s: 1 expected failure in 0.1s.\n" ~actual:summary

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
        Render.finish r ~results ~duration:0.2 ())
  in
  check_contains "finish: excused leaves the failed count" ~sub:"failures (1)" t;
  check_absent "finish: excused block absent" ~sub:"broken carry" t;
  check_contains "finish: summary counts the expected failure"
    ~sub:"1 passed, 1 expected failure, 1 failed in 0.2s." t;
  let only_excused =
    with_renderer ~invocation:(`Exe "exe") (fun r ->
        Render.finish r
          ~results:
            [ Fixtures.result [ "ok" ] Failure.Pass; Fixtures.excused_result ]
          ~duration:0.2 ())
  in
  check_absent "finish: no failure section when all failures excused"
    ~sub:"failures (" only_excused;
  check_absent "finish: no rerun hint when all failures excused" ~sub:"--failed"
    only_excused;
  check_contains "finish: green summary with excused failures"
    ~sub:"1 passed, 1 expected failure in 0.2s." only_excused

let test_xpass_is_loud () =
  (* The runner records an xfail test that passed as an ordinary counted
     failure whose message names the reason: no excused marking, loud FAIL. *)
  let line =
    with_renderer ~mode:`Verbose (fun r ->
        Render.result r Fixtures.xpass_result)
  in
  check_contains "unexpected pass: loud FAIL line" ~sub:"  FAIL  known › fixed"
    line;
  let t =
    with_renderer (fun r ->
        Render.finish r ~results:[ Fixtures.xpass_result ] ~duration:0.1 ())
  in
  check_contains "unexpected pass: reason in the failure block"
    ~sub:"expected to fail (issue #42), but the test passed" t

(* Subtest failures (amendment B13) *)

let test_subtest_projection () =
  check "subtest entries recognized by their label"
    (Render.is_subtest_failure ~path:Fixtures.subtest_result.Run.path
       (Fixtures.subtest_failure "shape [0]"));
  check "plain failures are not subtest entries"
    (not
       (Render.is_subtest_failure ~path:Fixtures.subtest_result.Run.path
          (Failure.message "boom")));
  check "a user msg without the leaf prefix is not a subtest entry"
    (not
       (Render.is_subtest_failure ~path:Fixtures.subtest_result.Run.path
          (Failure.message ~loc:(Fixtures.loc "f.ml" 1) "x")));
  check "the leaf name alone is not enough — the separator is the label"
    (not
       (Render.is_subtest_failure ~path:Fixtures.subtest_result.Run.path
          {
            (Failure.message "context") with
            Failure.msg = Some "contract note: extra context";
          }))

let test_subtest_rendering () =
  let t =
    with_renderer (fun r ->
        Render.finish r ~results:[ Fixtures.subtest_result ] ~duration:0.1 ())
  in
  check_contains "subtest blocks carry the parent › name label"
    ~sub:"contract › shape [0]" t;
  check_contains "summary states the subtest count"
    ~sub:"1 failed (2 subtest failures) in 0.1s." t;
  let one =
    with_renderer (fun r ->
        Render.finish r
          ~results:
            [
              Fixtures.result [ "backend"; "contract" ]
                (Failure.Fail [ Fixtures.subtest_failure "shape [0]" ]);
            ]
          ~duration:0.1 ())
  in
  check_contains "summary subtest count is singular"
    ~sub:"1 failed (1 subtest failure) in 0.1s." one

(* Property stats *)

let test_prop_stats () =
  let stats =
    {
      Property.cases = 100;
      discards = 3;
      collected = [ ("empty", 36); ("nonempty", 64) ];
      coverage =
        [
          {
            Property.label = "collision";
            required = 5.0;
            actual = 4.0;
            hits = 4;
            satisfied = false;
          };
          {
            Property.label = "singleton";
            required = 5.0;
            actual = 9.0;
            hits = 9;
            satisfied = true;
          };
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
        Render.finish r ~results:[ result ] ~duration:0.01 ())
  in
  check_contains "prop stats: label distribution"
    ~sub:"labels (100 passing cases):" b;
  check_contains "prop stats: percentages" ~sub:"36.0%  empty" b;
  check_contains "prop stats: unsatisfied coverage"
    ~sub:"collision  4.0% (required 5.0%) — unsatisfied" b;
  (* The list carries the satisfied requirement too — that is what it adds
     over the failure headline, which names only the one that failed. *)
  check_contains "prop stats: satisfied coverage listed alongside"
    ~sub:"singleton  9.0% (required 5.0%)" b;
  (* With a single requirement the list would only restate the headline, so
     it does not print at all. *)
  let single =
    { stats with Property.coverage = [ List.hd stats.Property.coverage ] }
  in
  let b1 =
    with_renderer (fun r ->
        Render.finish r
          ~results:
            [
              Fixtures.result [ "p" ]
                (Failure.Fail [ Failure.message "coverage unsatisfied" ])
                ~prop_stats:single;
            ]
          ~duration:0.01 ())
  in
  check_absent "prop stats: a lone requirement is not restated"
    ~sub:"coverage requirements:" b1;
  check_contains "prop stats: its labels still print"
    ~sub:"labels (100 passing cases):" b1

(* Containment blocks (D5 §2) *)

let not_contains_failure =
  Failure.containment ~found_at:10 ~claim:{|string not containing "secret"|}
    ~needle:"secret" ~haystack:"0123456789secret-end" ()

let test_containment_block () =
  let b = failure_block not_contains_failure in
  check_contains "not_contains: needle line carries the byte offset"
    ~sub:"    needle    \"secret\" \u{2014} found at byte 10\n" b;
  check_contains "not_contains: marker line sits under the occurrence"
    ~sub:
      ("    haystack  0123456789secret-end\n" ^ String.make 24 ' ' ^ "~~~~~~\n")
    b;
  check_absent "not_contains: the claim description never prints"
    ~sub:"string not containing" b;
  check_absent "not_contains: no fake equality labels" ~sub:"expected" b;
  check_absent "not_contains: no excerpt line for a complete excerpt"
    ~sub:"(excerpt:" b;
  let colored = failure_block ~ansi:true not_contains_failure in
  check_contains "not_contains: occurrence highlighted red under ansi"
    ~sub:"0123456789\027[31msecret\027[0m-end" colored;
  check_absent "not_contains: no marker line under ansi" ~sub:"~~~" colored;
  (* contains: needle absent, bounded head excerpt of a huge haystack. *)
  let haystack = String.make 20_006 'a' in
  let contains_failure =
    Failure.containment ~claim:{|string containing "NOPE"|} ~needle:"NOPE"
      ~haystack ()
  in
  let b = failure_block contains_failure in
  check_contains "contains: needle line with the not-found verdict"
    ~sub:"    needle    \"NOPE\" \u{2014} not found\n" b;
  check_contains "contains: excerpt range line iff partial"
    ~sub:"    (excerpt: bytes 0-8191 of a 20006-byte haystack)\n" b;
  check_contains "contains: the stored excerpt prints verbatim"
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
      "    needle    \"user=bob\" \u{2014} not found\n\
      \    haystack:\n\
      \      line one\n\
      \      line two user=alice\n\
      \      line three\n"
    b;
  check_absent "multi-line haystack: no unified diff" ~sub:"--- expected" b;
  (* A found occurrence in a multi-line excerpt highlights on its line
     under ansi; without color the block prints unmarked. *)
  let found =
    Failure.containment ~found_at:9 ~claim:{|string not containing "secret"|}
      ~needle:"secret" ~haystack:"line one\nsecret here\nline three" ()
  in
  let colored = failure_block ~ansi:true found in
  check_contains "multi-line occurrence highlighted on its line"
    ~sub:"      \027[31msecret\027[0m here\n" colored;
  let plain = failure_block found in
  check_absent "multi-line block form carries no markers" ~sub:"~~~" plain

let test_containment_headlines () =
  check "headline: not_contains names the offset"
    (Render.headline not_contains_failure = {|needle "secret" found at byte 10|});
  let contains_failure =
    Failure.containment ~claim:{|string containing "NOPE"|} ~needle:"NOPE"
      ~haystack:(String.make 20_006 'a') ()
  in
  check "headline: contains names the haystack size"
    (Render.headline contains_failure
    = {|needle "NOPE" not found (20006-byte haystack)|})

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
  let matches =
    failure_block (Failure.predicate ~claim:"a match" "Error \"boom\"")
  in
  check_absent "matches: no refinement either" ~sub:"~" matches

(* Trailing whitespace in hunks (D5 §4) *)

let test_trailing_whitespace_hunks () =
  let b =
    failure_block
      (Failure.equality ~expected:"line one \nline two"
         ~actual:"line one\nline two" ())
  in
  check_contains "changed lines visualize the trailing run"
    ~sub:
      "    @@ -1,2 +1,2 @@\n\
      \    - line one\u{00B7}\n\
      \    + line one\n\
      \      line two\n"
    b;
  (* Tabs render as arrows; context lines keep their bytes untouched. *)
  let b =
    failure_block
      (Failure.equality ~expected:"x\t\ncommon \ny" ~actual:"x\ncommon \ny" ())
  in
  check_contains "a trailing tab renders as an arrow" ~sub:"- x\u{2192}\n" b;
  check_contains "context lines keep raw trailing whitespace"
    ~sub:"      common \n" b;
  check_absent "context lines gain no glyphs" ~sub:"common\u{00B7}" b;
  (* The glyphs sit inside the line's styling on the ansi path. *)
  let colored =
    failure_block ~ansi:true
      (Failure.equality ~expected:"line one \nline two"
         ~actual:"line one\nline two" ())
  in
  check_contains "ansi path carries the same glyph inside the expected span"
    ~sub:"\027[32m- line one\u{00B7}\027[0m" colored;
  (* One meaning for green, across both diff paths.

     A transcript routinely shows both — a short value marks its spans,
     a multi-line one emits hunks — and until [text] made the multi-line
     path ordinary, nobody hit them side by side often enough to notice
     that green meant "expected" on one and "actual" on the other. The
     [-]/[+] sigils carry the diff convention; the colour carries the
     report's. Pin both here so they cannot drift apart again. *)
  let spans =
    (* Long enough that refinement marks a span rather than colouring the
       whole side — the marked-span case is the one that pairs with a hunk
       in the same transcript. *)
    failure_block ~ansi:true
      (Failure.equality ~expected:"the quick brown fox"
         ~actual:"the quick brawn fox" ())
  in
  check_contains "span path: expected side is green" ~sub:"\027[32mo\027[0m"
    spans;
  check_contains "span path: actual side is red" ~sub:"\027[31ma\027[0m" spans;
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
  (* Snapshot mismatch diffs share pp_hunks — the single producer. *)
  let snap =
    failure_block
      (Failure.snapshot ~name:"n" ~path:"p.snap"
         (Failure.Mismatch { expected = "a \nb\n"; actual = "a\nb\n" }))
  in
  check_contains "snapshot diffs visualize trailing whitespace too"
    ~sub:"- a\u{00B7}\n" snap

(* Uncaught exceptions (D5 §5) *)

let test_uncaught_wording () =
  let b = failure_block (Failure.raised ~actual:"Not_found" ()) in
  check_contains "uncaught: new wording"
    ~sub:"    uncaught exception:\n      Not_found\n" b;
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
  (* raises_match keeps its wording — pinned above in
     [test_raise_message_diff_guards]; the (None, None) arm serves both. *)
  check_contains "wanted-any arm unchanged"
    ~sub:"expected an exception, but none was raised"
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
       minimal\n"
    b;
  check "timed-out: headline carries the mark"
    (Render.headline f
   = "property failed (case 4, shrunk 2 steps, timed out): 9");
  let plain = failure_block Fixtures.prop_failure in
  check_absent "no marker without a timeout" ~sub:"timed out" plain;
  check_absent "no headline mark without a timeout" ~sub:"timed out"
    (Render.headline Fixtures.prop_failure)

(* Spent shrink budgets (D2's other stopping condition)

   The flag on the payload is not the report: a reader sees two strings —
   the headline suffix and the detail line — and both say the same thing,
   that "shrunk N steps" here is where the search stopped counting, not
   where it converged. Pinned present and absent, because a mark that
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
      \    shrink budget of 50 steps spent; counterexample may not be minimal\n"
    b;
  check "budget spent: headline carries the mark"
    (Render.headline f
   = "property failed (case 4, shrunk 50 steps, budget spent): 9");
  let plain = failure_block Fixtures.prop_failure in
  check_absent "no detail line without a spent budget" ~sub:"shrink budget"
    plain;
  check_absent "no headline mark without a spent budget" ~sub:"budget spent"
    (Render.headline Fixtures.prop_failure);
  (* An example never shrinks, so neither mark applies to one. *)
  let example =
    Failure.property ~shrink_exhausted:true ~rendered:"9" ~case_index:0
      ~shrink_steps:0 ~root:Fixtures.root ~examples:true ()
  in
  check_absent "an example carries no headline mark" ~sub:"budget spent"
    (Render.headline example)

(* Inner failures without a location (D4) *)

let test_inner_label_without_location () =
  let inner_no_loc = Failure.equality ~expected:"true" ~actual:"false" () in
  let b =
    failure_block
      (Failure.property ~inner:inner_no_loc ~rendered:"7" ~case_index:0
         ~shrink_steps:0 ~root:Fixtures.root ~examples:false ())
  in
  check_contains "a location-less inner failure reads (with:)"
    ~sub:"    which failed with:\n      expected  true\n" b;
  check_absent "no dangling at: without a location line" ~sub:"failed at:" b;
  let located = failure_block Fixtures.prop_failure in
  check_contains "a located inner failure keeps (at:)" ~sub:"which failed at:"
    located

(* Command hints per invocation (D5 §1) *)

let test_hints_per_invocation () =
  let exe = `Exe "./_build/default/qa/x/t.exe" in
  let accept = failure_block ~invocation:exe Fixtures.snap_missing in
  check_contains "accept hint completes the executable"
    ~sub:
      "    accept: ./_build/default/qa/x/t.exe -u, then review with git diff\n"
    accept;
  check_absent "accept hint under Exe never spells the mirror"
    ~sub:"WINDTRAP_UPDATE" accept;
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
  (* The Mirrors spellings — the default — are pinned by the golden
     transcript's standalone block tests above. *)
  let mirrors = failure_block Fixtures.snap_missing in
  check_contains "Mirrors accept spelling is the dune-runtest mirror"
    ~sub:"accept: WINDTRAP_UPDATE=1 dune runtest, then review with git diff"
    mirrors

(* [--failed] is an optimization, not a step, so no run advertises it. The
   acceptance commands are the opposite case — they name a verb nobody can
   guess — and stay under every mismatch (Law 3). *)
let test_no_rerun_hint () =
  let failing =
    [ Fixtures.result [ "t" ] (Failure.Fail [ Failure.message "b" ]) ]
  in
  let exe =
    with_renderer ~invocation:(`Exe "dune exec qa/x/t.exe --") (fun r ->
        Render.finish r ~results:failing ~duration:0.1 ())
  in
  check_absent "a failing run does not advertise --failed" ~sub:"--failed" exe;
  check_contains "the summary is the last line" ~sub:"1 failed in 0.1s.\n" exe;
  let mirrors =
    with_renderer (fun r -> Render.finish r ~results:failing ~duration:0.1 ())
  in
  check_absent "nor under Mirrors" ~sub:"--failed" mirrors

(* The srandom replay line (D5 §6) *)

let test_srandom_replay_line () =
  let entry =
    Failure.with_output_tail
      (Failure.tail "drew 337709\n")
      (Failure.message "boom")
  in
  let failing =
    Fixtures.result ~srandom_root:Fixtures.root [ "draw" ]
      (Failure.Fail [ entry ])
  in
  let t =
    with_renderer
      ~invocation:(`Exe "./_build/default/qa/prop/verify2/v_srandom.exe")
      (fun r -> Render.finish r ~results:[ failing ] ~duration:0.1 ())
  in
  check_contains
    "srandom failure prints the replay line after the entries, before the tail"
    ~sub:
      "    boom\n\
      \    replay: ./_build/default/qa/prop/verify2/v_srandom.exe --seed \
       s1:7be1d2c904aa31f5 -f 'draw'\n\
      \    \u{2500}\u{2500} captured output"
    t;
  let mirrors =
    with_renderer (fun r ->
        Render.finish r ~results:[ failing ] ~duration:0.1 ())
  in
  check_contains "srandom replay under Mirrors spells the env prefixes"
    ~sub:
      "    replay: WINDTRAP_SEED=s1:7be1d2c904aa31f5 WINDTRAP_FILTER='draw' \
       dune runtest\n"
    mirrors;
  (* A property failure already prints its own replay line from the same
     root: never two replay lines per block. *)
  let prop_result =
    Fixtures.result ~srandom_root:Fixtures.root
      [ "geo"; "area non-negative" ]
      (Failure.Fail [ Fixtures.prop_failure ])
  in
  let t =
    with_renderer (fun r ->
        Render.finish r ~results:[ prop_result ] ~duration:0.1 ())
  in
  check "a property failure suppresses the per-test replay line"
    (occurrences_of ~sub:"replay:" t = 1);
  (* No line without a draw. *)
  let plain =
    with_renderer (fun r ->
        Render.finish r
          ~results:
            [ Fixtures.result [ "t" ] (Failure.Fail [ Failure.message "b" ]) ]
          ~duration:0.1 ())
  in
  check_absent "no replay line without an srandom draw" ~sub:"replay:" plain

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
    with_renderer ~mode:`Verbose (fun r -> Render.result r passing)
  in
  check_contains "verbose: a passing property prints its label table"
    ~sub:"    labels (100 passing cases):\n       46.0%  even\n" verbose;
  check "verbose: the table follows the PASS line"
    (String.starts_with ~prefix:"  PASS  labels visible" verbose);
  let compact = with_renderer (fun r -> Render.result r passing) in
  check_absent "compact: no label table" ~sub:"labels (" compact;
  let quiet = with_renderer ~mode:`Quiet (fun r -> Render.result r passing) in
  check_string "quiet: nothing streams" ~expected:"" ~actual:quiet;
  let unlabeled =
    with_renderer ~mode:`Verbose (fun r ->
        Render.result r
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
        Render.result r
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
    with_renderer ~mode:`Verbose (fun r -> Render.result r failing)
  in
  check_contains "verbose line escapes the newline" ~sub:{|FAIL  first\nhalf|}
    verbose;
  check "verbose line stays one line" (occurrences_of ~sub:"\n" verbose = 1);
  let block =
    with_renderer (fun r ->
        Render.finish r ~results:[ failing ] ~duration:0.1 ())
  in
  check_contains "FAIL header escapes the newline" ~sub:{|  FAIL  first\nhalf|}
    block;
  let live =
    with_renderer ~ansi:true ~live:true (fun r ->
        Render.header r ~suite:"vnames" ~tests:2 ~seed:None ();
        Render.begin_test r ~path:hostile)
  in
  check_contains "live tail escapes the newline" ~sub:{|first\nhalf|} live;
  check_absent "live tail carries no raw newline" ~sub:"first\nhalf" live;
  (* Suite names: header, deferred one-liner, quiet summary prefix. *)
  let named =
    with_renderer ~mode:`Quiet (fun r ->
        Render.header r ~suite:"my\tsuite" ~tests:1 ~seed:None ();
        Render.result r (Fixtures.result [ "t" ] Failure.Pass);
        Render.finish r
          ~results:[ Fixtures.result [ "t" ] Failure.Pass ]
          ~duration:0.1 ())
  in
  check_contains "summary prefix escapes the tab" ~sub:{|my\tsuite: 1 passed|}
    named;
  let header =
    with_renderer ~mode:`Verbose (fun r ->
        Render.header r ~suite:"a\x07b" ~tests:1 ~seed:None ())
  in
  check_contains "header escapes control bytes" ~sub:{|a\x07b: 1 test|} header;
  (* Slow warnings and the slowest list share the treatment. *)
  let slow = Fixtures.result [ "sl\now" ] Failure.Pass ~duration:1.5 in
  let warned =
    with_renderer (fun r ->
        Render.header r ~suite:"s" ~tests:1 ~seed:None ();
        Render.result r slow;
        Render.finish r ~results:[ slow ] ~duration:1.5 ())
  in
  check_contains "slow warning escapes the newline" ~sub:{|  1.50s  sl\now|}
    warned;
  (* ESC is left to the ansi policy (stripped under ansi:false) — pinned in
     [test_ansi_hygiene]. *)
  let note =
    with_renderer ~mode:`Verbose (fun r -> Render.note r "releasing d\nb")
  in
  check_string "notes escape their fixture name" ~expected:"releasing d\\nb\n"
    ~actual:note

(* Source excerpts resolve against the project root (render/F-1) *)

let test_excerpt_project_root () =
  (* The recorded location is project-root-relative, exactly as __POS__
     records it. Under [dune runtest] the process cwd is inside _build,
     where this path never opens — resolution against the project root
     must find it; run directly from the repo root, the relative open
     works too, and the block renders identically. *)
  let f =
    Failure.equality
      ~loc:{ Loc.file = "test/unit/test_render.ml"; line = 1; column = 0 }
      ~expected:"1" ~actual:"2" ()
  in
  check_contains "relative recorded paths resolve under dune runtest"
    ~sub:"1 \u{2502} (*---"
    (failure_block ~excerpt:true f)

(* The shared excerpt projection (Law 12)

   One gutter renderer serves coverage and mutation. These pin the bytes
   coverage has always printed — the three-column gutter, the number
   right-aligned in at least four, the [│] rule, [·····] between regions,
   trailing spaces stripped — and the marker's escape sequence opening at
   column zero, which is where a green-vs-plain inconsistency would hide
   from a stripped-output review. *)

let excerpt_source =
  String.concat ""
    (List.init 12 (fun i -> Printf.sprintf "line %d  \n" (i + 1)))

let test_shared_excerpt () =
  let render ?ansi ?context ?marker ?margin ?number_width e =
    with_renderer ?ansi (fun r ->
        Render.excerpt r ?context ?marker ?margin ?number_width e)
  in
  let coverage =
    {
      Render.file = "lib/eval.ml";
      heading = Some [ Render.plain "75.0% (111/148)" ];
      source = excerpt_source;
      marked_lines = [ 2; 9 ];
    }
  in
  check_string "coverage excerpt: heading, gutter, regions, separator"
    ~expected:
      "\n\
       lib/eval.ml \u{2014} 75.0% (111/148)\n\n\
      \      1 \u{2502} line 1\n\
      \  \u{258c}   2 \u{2502} line 2\n\
      \      3 \u{2502} line 3\n\
      \   \u{00b7}\u{00b7}\u{00b7}\u{00b7}\u{00b7}\n\
      \      8 \u{2502} line 8\n\
      \  \u{258c}   9 \u{2502} line 9\n\
      \     10 \u{2502} line 10\n"
    ~actual:(render coverage);
  (* The regression this guards: styling that starts after the margin
     looks identical once the escapes are stripped. *)
  check_contains "coverage excerpt: the marker's escape opens at column zero"
    ~sub:"\027[31m  \u{258c}\027[0m   2 \u{2502} line 2"
    (render ~ansi:true coverage);
  check_absent "coverage excerpt: no escape after the margin" ~sub:"  \027[31m"
    (render ~ansi:true coverage);
  (* A block whose head row already named the file, whose excerpt is its
     marked line, and which sits under an indent: no heading, no marker
     column, its own margin and its own number column. *)
  check_string "in-block excerpt: no heading, no marker column"
    ~expected:"      12 \u{2502} line 12\n"
    ~actual:
      (render ~context:0 ~marker:false ~margin:"      " ~number_width:2
         {
           Render.file = "lib/eval.ml";
           heading = None;
           source = excerpt_source;
           marked_lines = [ 12 ];
         });
  check_string "excerpt: a marked line outside the source draws nothing"
    ~expected:""
    ~actual:
      (render ~context:0
         {
           Render.file = "lib/eval.ml";
           heading = None;
           source = excerpt_source;
           marked_lines = [ 99 ];
         })

(* The excerpt regions and the range dialect

   Moved here from the coverage runtime with the layout they serve: the
   regions are what [Render.excerpt] draws, the ranges what the coverage
   table's uncovered lists and the mutation report's unreached list
   print. *)

let ten_lines =
  String.concat "" (List.init 10 (fun i -> Printf.sprintf "l%d\n" (i + 1)))

let test_excerpt_regions () =
  let numbers region = List.map (fun l -> l.Render.number) region in
  let marked region =
    List.filter_map
      (fun l -> if l.Render.marked then Some l.Render.number else None)
      region
  in
  (match Render.excerpts ~source:ten_lines [ 3; 4; 8 ] with
  | [ first; second ] ->
      check "first region spans the range plus context"
        (numbers first = [ 2; 3; 4; 5 ]);
      check "first region marks only marked lines" (marked first = [ 3; 4 ]);
      check "second region spans its range plus context"
        (numbers second = [ 7; 8; 9 ]);
      check "second region marks its marked line" (marked second = [ 8 ]);
      check "excerpt text is the source line"
        ((List.nth first 1).Render.text = "l3")
  | regions ->
      equal ~msg:"separated ranges yield two regions" int 2
        (List.length regions));
  (match Render.excerpts ~source:ten_lines [ 3; 6 ] with
  | [ only ] ->
      check "touching context windows merge into one region"
        (numbers only = [ 2; 3; 4; 5; 6; 7 ])
  | regions ->
      equal ~msg:"touching windows yield one region" int 1
        (List.length regions));
  (match Render.excerpts ~context:0 ~source:ten_lines [ 5 ] with
  | [ [ line ] ] ->
      check "zero context keeps the bare line"
        (line.Render.number = 5 && line.Render.marked)
  | _ -> check "zero context keeps the bare line" false);
  (match Render.excerpts ~source:ten_lines [ 1; 10 ] with
  | [ first; second ] ->
      check "context clamps at the top" (numbers first = [ 1; 2 ]);
      check "context clamps at the bottom" (numbers second = [ 9; 10 ])
  | _ -> check "boundary lines clamp their context" false);
  check "out-of-range lines are ignored"
    (Render.excerpts ~source:ten_lines [ 0; 11; 99 ] = []);
  check "an empty source yields no excerpts"
    (Render.excerpts ~source:"" [ 1 ] = []);
  match Render.excerpts ~source:"a\nb\n" [ 2 ] with
  | [ region ] ->
      check "a trailing newline opens no phantom line"
        (numbers region = [ 1; 2 ] && (List.nth region 1).Render.text = "b")
  | _ -> check "a trailing newline opens no phantom line" false

let test_line_ranges () =
  check "collapse of contiguous runs"
    (Render.collapse_ranges [ 1; 2; 3; 7; 8 ] = [ (1, 3); (7, 8) ]);
  check "collapse tolerates duplicates"
    (Render.collapse_ranges [ 1; 1; 2; 5; 5 ] = [ (1, 2); (5, 5) ]);
  check "collapse of the empty list" (Render.collapse_ranges [] = []);
  check "collapse of a singleton" (Render.collapse_ranges [ 4 ] = [ (4, 4) ]);
  check "a huge contiguous run collapses to one range"
    (Render.collapse_ranges (List.init 20_000 (fun i -> i + 1))
    = [ (1, 20_000) ]);
  check_string "range formatting matches the report shape"
    ~expected:"88-94, 121"
    ~actual:(Render.format_ranges [ (88, 94); (121, 121) ]);
  check_string "single-range formatting" ~expected:"1-3"
    ~actual:(Render.format_ranges [ (1, 3) ]);
  check_string "empty-range formatting" ~expected:""
    ~actual:(Render.format_ranges [])

(* The coverage detail block, escape for escape

   Coverage's rendered bytes are frozen, and the shared projection above
   had to leave every one of them where it was. The difference a review
   over stripped output cannot see is *where* an escape opens: a marker
   spelled [margin ^ red "▌"] prints the same glyphs as [red (margin ^
   "▌")]. So this drives the real [coverage_report] over a real
   collection and pins the plain bytes whole, then pins that colour adds
   escapes and nothing else, and that the marker's escape opens at column
   zero. *)

let coverage_fixture_lines =
  List.init 12 (fun i -> Printf.sprintf "let v%d = %d" (i + 1) (i + 1))

let coverage_fixture_source = String.concat "\n" coverage_fixture_lines ^ "\n"

(* The half-open byte extent of one 1-based line of the fixture. *)
let coverage_fixture_extent n =
  let rec go i offset = function
    | [] -> invalid_arg "coverage_fixture_extent"
    | line :: rest ->
        if i = n then (offset, offset + String.length line)
        else go (i + 1) (offset + String.length line + 1) rest
  in
  go 1 0 coverage_fixture_lines

(* One instrumented file in the coverage runtime's own on-disk format,
   parsed by the runtime rather than fabricated behind it. Four of the
   eight points are never visited, and they fall into three runs of
   lines — so the block carries two [·····] separators and a region
   clipped against the top of the file. *)
let coverage_fixture_collection ~file =
  let points =
    [ (1, 0); (3, 1); (5, 0); (6, 0); (8, 1); (10, 1); (11, 0); (12, 1) ]
  in
  let dump =
    String.concat "\n"
      ([
         "windtrap-coverage-v3";
         "1";
         Printf.sprintf "%d %s" (String.length file) file;
         string_of_int (List.length points);
       ]
      @ List.map
          (fun (line, count) ->
            let first, last = coverage_fixture_extent line in
            Printf.sprintf "%d %d %d" first last count)
          points)
    ^ "\n"
  in
  match Windtrap_coverage.of_string dump with
  | Ok (collection, _) -> collection
  | Error _ -> failwith "the coverage fixture does not parse"

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
  let root = temp_dir () in
  let file = "lib/fake.ml" in
  Path_ops.mkdir_p (Filename.concat root "lib");
  let oc = open_out (Filename.concat root file) in
  output_string oc coverage_fixture_source;
  close_out oc;
  let collection = coverage_fixture_collection ~file in
  (* Through the seam's one builder, as the driver and the [windtrap
     coverage] command render it: the collection is the runtime's, the
     section data the seam's, the layout the renderer's. *)
  let data = Driver.coverage_data ~source_roots:[ root ] collection in
  let render ?ansi () =
    with_renderer ?ansi (fun r -> Render.coverage_report r ~mode:`Full data)
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
    ~actual:
      (with_renderer (fun r -> Render.coverage_report r ~mode:`Report data))

(* The coverage thresholds, pinned at the bytes

   Green at 80% and above, yellow at 60%, red below — the classification
   used to live on the runtime as [style]; it is styling, so it lives
   with the renderer now, and the summary line is where it shows. *)

let test_coverage_thresholds () =
  let line ~visited ~total =
    with_renderer ~ansi:true (fun r ->
        Render.coverage_report r ~mode:`Report
          { Render.visited; total; files = [] })
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

(* The mutation report (RFC §Asking, byte for byte)

   The survivor block is the ordinary failure block: the same 54-column
   labelled rule, the same [  VERB  subject] head row, the same excerpt
   row. The fixture is the RFC's worked example, with the seed token the
   one the shared fixtures carry. *)

let calc_source =
  String.concat "\n"
    (List.init 11 (fun i ->
         match i + 1 with
         | 9 -> "  | Sub -> a - b"
         | 11 ->
             "  | Div -> if b = 0 then invalid_arg \"division by zero\" else a \
              / b"
         | n -> Printf.sprintf "(* line %d *)" n))
  ^ "\n"

let witness test file line =
  { Render.test; loc = Some { Loc.file; line; column = 0 } }

let rfc_report =
  {
    (* Pre-spelled, as the loop spells them with the runtime's own
       functions: the identifier in its canonical form, the arming
       variable by name. *)
    Render.arm_variable = "WINDTRAP_MUTATE_ARM";
    survivors =
      [
        {
          Render.id = "lib/calc.ml:9:12:add";
          file = "lib/calc.ml";
          line = 9;
          before = "a - b";
          after = "a + b";
          source = Some calc_source;
          witnesses =
            [
              witness "calc \u{203a} sub of two positives" "test/test_calc.ml"
                14;
              witness "calc \u{203a} sub to zero" "test/test_calc.ml" 19;
              witness "eval \u{203a} Sub node" "test/test_eval.ml" 31;
            ];
        };
        {
          Render.id = "lib/calc.ml:11:15:neq";
          file = "lib/calc.ml";
          line = 11;
          before = "b = 0";
          after = "b <> 0";
          source = Some calc_source;
          witnesses =
            [
              witness "calc \u{203a} div by zero raises" "test/test_calc.ml" 24;
            ];
        };
      ];
    survivors_total = 2;
    unreached =
      [
        { Render.file = "lib/calc.ml"; lines = [ 52; 61 ] };
        { Render.file = "lib/lexer.ml"; lines = [ 14; 96 ] };
      ];
    unreached_total = 4;
    killed = 181;
    total = 187;
    duration = Some 104.;
    seed = Some Fixtures.root;
    siblings = false;
  }

let mutation_report ?ansi ?mode ?invocation m =
  with_renderer ?ansi ?mode ?invocation (fun r -> Render.mutation_report r m)

let expected_rfc_report =
  {|
─────────────────── survivors (2) ────────────────────

  SURVIVED  lib/calc.ml:9:12:add    a - b  →  a + b
       9 │   | Sub -> a - b

    3 tests ran this line and none failed when it changed:
      calc › sub of two positives      test/test_calc.ml:14
      calc › sub to zero               test/test_calc.ml:19
      eval › Sub node                  test/test_eval.ml:31

    arm      WINDTRAP_MUTATE_ARM=lib/calc.ml:9:12:add dune exec --instrument-with ppx_windtrap.mutate test/test_calc.exe
    dismiss  ((a - b) [@mutate off "reason"])

  SURVIVED  lib/calc.ml:11:15:neq   b = 0  →  b <> 0
      11 │   | Div -> if b = 0 then invalid_arg "division by zero" else a / b

    1 test ran this line and did not fail when it changed:
      calc › div by zero raises        test/test_calc.ml:24

    arm      WINDTRAP_MUTATE_ARM=lib/calc.ml:11:15:neq dune exec --instrument-with ppx_windtrap.mutate test/test_calc.exe
    dismiss  ((b = 0) [@mutate off "reason"])

──────────────────────────────────────────────────────

unreached (4) — no test evaluates these
   lib/calc.ml    52, 61
   lib/lexer.ml   14, 96

mutants: 2 survived of 187 · 181 killed, 4 unreached in 1m44s (seed s1:7be1d2c904aa31f5)
|}

let arm_invocation =
  `Exe "dune exec --instrument-with ppx_windtrap.mutate test/test_calc.exe"

let test_mutation_report () =
  check_string "the worked survivor report, byte for byte"
    ~expected:expected_rfc_report
    ~actual:(mutation_report ~invocation:arm_invocation rfc_report);
  (* A survivor is a failure block, and quiet keeps failure blocks and
     the summary — a mutation report is both. *)
  check_string "quiet keeps the whole report" ~expected:expected_rfc_report
    ~actual:(mutation_report ~mode:`Quiet ~invocation:arm_invocation rfc_report);
  (* Without a CLI the arm line mirrors the variable onto dune runtest,
     as every other hint does — and carries the instrumentation flag,
     because a build without the backend has no mutant to arm and the
     line would name a command that cannot do what it says. *)
  check_contains "the arm line follows the invocation"
    ~sub:
      "    arm      WINDTRAP_MUTATE_ARM=lib/calc.ml:9:12:add dune runtest \
       --force --instrument-with ppx_windtrap.mutate\n"
    (mutation_report rfc_report)

let test_mutation_colors () =
  let out = mutation_report ~ansi:true ~invocation:arm_invocation rfc_report in
  check_contains "SURVIVED wears the failure red, the identifier the bold"
    ~sub:"  \027[31mSURVIVED\027[0m  \027[1mlib/calc.ml:9:12:add\027[0m" out;
  check_contains "the witness location is faint"
    ~sub:"\027[2mtest/test_calc.ml:14\027[0m" out;
  check_contains "the survivor count is red, the unreached count yellow"
    ~sub:
      "mutants: \027[31m2 survived\027[0m of 187 \u{00b7} 181 killed, \
       \027[33m4 unreached\027[0m in 1m44s"
    out;
  (* The section furniture is faint, as the failure rules are, and the
     rows it introduces are not — pinned together so a style that leaked
     from the heading onto the list fails here. *)
  check_contains "the rules and the unreached heading are faint, the rows plain"
    ~sub:
      "\027[2munreached (4) \u{2014} no test evaluates these\027[0m\n\
      \   lib/calc.ml    52, 61\n"
    out;
  check_contains "the labelled rule is faint" ~sub:"\027[2m\u{2500}" out;
  (* No color in any hint, as everywhere else in the transcript — pinned
     by the whole line, so a hint that went missing fails too. *)
  let line ~sub =
    match List.filter (has ~sub) (String.split_on_char '\n' out) with
    | l :: _ -> l
    | [] -> "\u{ab}no line matching " ^ sub ^ "\u{bb}"
  in
  check_string "the arm hint carries no color"
    ~expected:
      "    arm      WINDTRAP_MUTATE_ARM=lib/calc.ml:11:15:neq dune exec \
       --instrument-with ppx_windtrap.mutate test/test_calc.exe"
    ~actual:(line ~sub:"arm      WINDTRAP_MUTATE_ARM=lib/calc.ml:11:15");
  check_string "the dismiss hint carries no color"
    ~expected:"    dismiss  ((b = 0) [@mutate off \"reason\"])"
    ~actual:(line ~sub:"dismiss  ((b = 0)")

let test_mutation_summary_forms () =
  let clean =
    {
      rfc_report with
      Render.survivors = [];
      survivors_total = 0;
      unreached = [];
      unreached_total = 0;
      killed = 187;
      duration = Some 101.;
    }
  in
  check_string "a report with nothing to say is one line"
    ~expected:
      "mutants: 0 survived of 187 \u{00b7} 187 killed in 1m41s (seed \
       s1:7be1d2c904aa31f5)\n"
    ~actual:(mutation_report clean);
  check_string "zero terms are omitted, and so is an absent seed"
    ~expected:"mutants: 0 survived of 0 in 0.0ms\n"
    ~actual:
      (mutation_report
         {
           clean with
           Render.killed = 0;
           total = 0;
           duration = Some 0.;
           seed = None;
         });
  check_string "a merge ran nothing and times nothing"
    ~expected:"mutants: 0 survived of 187 \u{00b7} 187 killed\n"
    ~actual:(mutation_report { clean with Render.duration = None; seed = None });
  (* Siblings: this executable's view, pointing at the merge, in
     coverage's wording. *)
  check_contains "siblings scope the total and name the merge"
    ~sub:
      "mutants: 2 survived of 41 (this executable) \u{00b7} 37 killed, 4 \
       unreached in 1m44s (seed s1:7be1d2c904aa31f5) \u{00b7} project: dune \
       build @mutate\n"
    (mutation_report
       { rfc_report with Render.siblings = true; total = 41; killed = 37 })

let test_mutation_sections () =
  (* The cap is in the label so nobody thinks they saw everything. *)
  let capped =
    {
      rfc_report with
      Render.survivors = [ List.hd rfc_report.Render.survivors ];
      survivors_total = 37;
    }
  in
  check_contains "the cap is named in the rule label"
    ~sub:"survivors (1 of 37) " (mutation_report capped);
  (* Each finding stands alone: unreached without survivors, and
     survivors without unreached. *)
  let unreached_only =
    { rfc_report with Render.survivors = []; survivors_total = 0; killed = 183 }
  in
  check_string "unreached alone: no rule, no blocks"
    ~expected:
      "\n\
       unreached (4) \u{2014} no test evaluates these\n\
      \   lib/calc.ml    52, 61\n\
      \   lib/lexer.ml   14, 96\n\n\
       mutants: 0 survived of 187 \u{00b7} 183 killed, 4 unreached in 1m44s \
       (seed s1:7be1d2c904aa31f5)\n"
    ~actual:(mutation_report unreached_only);
  check_absent "survivors alone: no unreached heading" ~sub:"unreached"
    (mutation_report ~invocation:arm_invocation
       { rfc_report with Render.unreached = []; unreached_total = 0 });
  (* Consecutive lines collapse into ranges, in coverage's dialect. *)
  check_contains "unreached lines collapse into ranges"
    ~sub:"   lib/calc.ml   52-54, 61\n"
    (mutation_report
       {
         unreached_only with
         Render.unreached =
           [ { Render.file = "lib/calc.ml"; lines = [ 52; 53; 54; 61 ] } ];
       });
  (* An unreadable source drops the excerpt row and nothing else. *)
  let sourceless =
    {
      rfc_report with
      Render.survivors =
        List.map
          (fun (s : Render.survivor) -> { s with Render.source = None })
          rfc_report.Render.survivors;
    }
  in
  check_absent "an unreadable source drops the excerpt row" ~sub:"\u{2502}"
    (mutation_report sourceless);
  check_contains "an unreadable source keeps the head row"
    ~sub:"  SURVIVED  lib/calc.ml:9:12:add"
    (mutation_report sourceless)

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
        Render.header r ~suite:"mylib" ~tests:(List.length results) ~seed:None
          ();
        List.iter (fun res -> Render.result r res) results;
        Render.finish r ~results ~duration ())
  in
  let pass = Fixtures.result [ "t" ] Failure.Pass in
  (* Green: a deferred compact run is exactly the one named line. *)
  let green = chomp (transcript ~results:[ pass; pass ] ~duration:0.5) in
  check_string "renderer green one-liner styles the passed segment"
    ~expected:"mylib: \027[32m2 passed\027[0m in 0.5s." ~actual:green;
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
    ~expected:"1 passed, \027[31m1 failed\027[0m in 0.5s."
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
    ~expected:"mylib: 2 checks passed in 0.5s." ~actual:plain;
  check_string "harness monochrome FAIL tag is bare" ~expected:"FAIL"
    ~actual:(Harness.fail_tag ~ansi:false)

(* The snapshot/prune report

   The advisory baseline-maintenance lines the driver prints after
   [finish] — a projection of run data, so every transcript byte leaves
   through the renderer (Law 4). One producer for both runners: the line
   classes under both invocations and the quiet gate are pinned here. *)

let make_run ?snapshots () =
  let snapshots =
    match snapshots with
    | Some s -> s
    | None -> Snapshot.create ~mode:Snapshot.Check ()
  in
  Run.create (Run.default_config ()) ~capture:Capture.disabled ~snapshots

let snapshot_report ?mode ?invocation ?(orphans = []) ?pruned run =
  with_renderer ?mode ?invocation (fun r ->
      Render.report_snapshots r ~orphans ~pruned run)

let test_snapshot_report_writes () =
  (* One [wrote] line per accepted baseline, paths spelled by
     [Path_ops.display] — the one producer for both runners (ppx/F-6). *)
  let root = temp_dir () in
  let snapshots = Snapshot.create ~root ~mode:Snapshot.Update () in
  Snapshot.check snapshots ~test:"t" ~scope:(Some "qa/x.ml") ~name:"greeting"
    "hello\n";
  let written =
    match Snapshot.writes snapshots with
    | [ (path, Snapshot.Created) ] -> path
    | _ -> failf "expected exactly one Created write"
  in
  check_string "wrote line: Path_ops.display spelling, (new) status"
    ~expected:(Printf.sprintf "wrote %s (new)\n" (Path_ops.display written))
    ~actual:(snapshot_report (make_run ~snapshots ()));
  check_string "quiet prints no maintenance lines" ~expected:""
    ~actual:(snapshot_report ~mode:`Quiet (make_run ~snapshots ()))

let test_snapshot_report_prune () =
  check_string "granted prune: one line per deleted baseline"
    ~expected:
      (Printf.sprintf "pruned %s\npruned %s\n"
         (Path_ops.display "/tmp/a.snap")
         (Path_ops.display "/tmp/b.snap"))
    ~actual:
      (snapshot_report
         ~pruned:(Ok [ "/tmp/a.snap"; "/tmp/b.snap" ])
         (make_run ()));
  let refusal =
    {
      Snapshot.not_update_run = true;
      filtered = false;
      skipped = 0;
      failed = 2;
      focused = 0;
    }
  in
  check_string "refused prune: stale lines then the explanation"
    ~expected:
      (Printf.sprintf
         "stale baseline: %s\n\
          prune refused: the run was not an update run (-u / \
          WINDTRAP_UPDATE=1); 2 selected test(s) failed\n"
         (Path_ops.display "/tmp/stale.snap"))
    ~actual:
      (snapshot_report ~orphans:[ "/tmp/stale.snap" ] ~pruned:(Error refusal)
         (make_run ()))

let test_snapshot_report_orphan_hint () =
  (* The removal hint is spelled from the invocation — the one hint-context
     difference between the runners. *)
  let orphans = [ "/tmp/stale.snap" ] in
  let expected_stale =
    Printf.sprintf "stale baseline: %s\n" (Path_ops.display "/tmp/stale.snap")
  in
  check_string "orphans under Exe: hint completes the executable"
    ~expected:(expected_stale ^ "remove stale baselines: ./t.exe -u --prune\n")
    ~actual:
      (snapshot_report ~invocation:(`Exe "./t.exe") ~orphans (make_run ()));
  check_string "orphans under Mirrors: hint spells the environment prefixes"
    ~expected:
      (expected_stale
     ^ "remove stale baselines: WINDTRAP_UPDATE=1 WINDTRAP_PRUNE=1 dune runtest\n"
      )
    ~actual:(snapshot_report ~invocation:`Mirrors ~orphans (make_run ()));
  check_string "no writes, no orphans, no prune: nothing prints" ~expected:""
    ~actual:(snapshot_report (make_run ()))

(* The --strict-snapshots verdict

   The runner records the verdict as a result row ({!Run.Stale_baselines};
   pinned at runner level in test_runner.ml), so the same stale lines reach
   the failure section of every sink. What this pins is the report's side
   of the bargain: the advisory block stands down when the run carries the
   row — one printing — while a prune refusal keeps its explanation. *)

let strict_run ~orphans =
  let run = make_run () in
  Run.record run
    {
      Run.path = [ "stale baselines" ];
      subject = Run.Stale_baselines;
      outcome = Failure.Fail [ Failure.stale_baselines orphans ];
      counted = true;
      xfail = None;
      slow_tagged = false;
      duration = 0.;
      attempts = 1;
      prop_stats = None;
      srandom_root = None;
    };
  run

let test_strict_snapshots_report () =
  let orphans = [ "/tmp/a.snap"; "/tmp/b.snap" ] in
  (* One printing: the advisory block stands down when the failure block
     already carried the same lines on the recorded row. *)
  check_string "the advisory block stands down under the flag" ~expected:""
    ~actual:
      (snapshot_report ~invocation:(`Exe "./t.exe") ~orphans
         (strict_run ~orphans));
  (* A refused prune still explains itself — the failure says what is
     stale, the refusal says why nothing was deleted. *)
  let refusal =
    {
      Snapshot.not_update_run = true;
      filtered = false;
      skipped = 0;
      failed = 0;
      focused = 0;
    }
  in
  check_string "a refused prune keeps its explanation"
    ~expected:
      "prune refused: the run was not an update run (-u / WINDTRAP_UPDATE=1)\n"
    ~actual:
      (snapshot_report ~invocation:(`Exe "./t.exe") ~orphans
         ~pruned:(Error refusal) (strict_run ~orphans))

let tests =
  [
    test "golden compact transcript (default)" test_golden_compact;
    test "golden verbose transcript (-v)" test_golden_verbose;
    test "golden verbose transcript, coloured" test_golden_ansi;
    test "coverage line scopes itself on siblings" test_coverage_line_siblings;
    test "quiet mode (-q)" test_quiet;
    test "quiet green run is one line" test_quiet_green_run;
    test "ansi styling and diff highlighting" test_ansi;
    test "live progress line (verbose)" test_live;
    test "live compact tail" test_live_compact_tail;
    test "header forms" test_header_forms;
    test "seed token consistency (Law 7)" test_seed_token_consistency;
    test "duration forms" test_duration_forms;
    test "create validation" test_create_validation;
    test "empty run" test_no_tests;
    test "glyph vocabulary" test_glyph_vocabulary;
    test "glyph row wraps at 60 with the [k/n] counter" test_glyph_wrap;
    test "glyph row closes before the failure section"
      test_glyph_row_before_failures;
    test "run-scoped notes close the row" test_note;
    test "green compact run is one named line" test_compact_green_one_liner;
    test "the noteworthy flush streams from then on"
      test_compact_flush_streams_after;
    test "slow untagged tests are noteworthy" test_compact_slow_trigger;
    test "slow durations sum attempts; failing slow tests warn once"
      test_slow_duration_semantics;
    test "slow threshold zero disables the machinery" test_slow_threshold_zero;
    test "verbose gains the slow warnings" test_verbose_slow_warnings;
    test "headline projection" test_headline;
    test "stale-baselines projections (one result model)"
      test_stale_baselines_projections;
    test "property projections" test_property_projections;
    test "kind details" test_kind_details;
    test "degenerate equalities" test_degenerate_equalities;
    test "ansi hygiene under ansi:false" test_ansi_hygiene;
    test "diff display bounds" test_diff_truncation;
    test "proposed-content display bounds" test_proposed_truncation;
    test "source excerpt" test_excerpt;
    test "captured tail" test_tail;
    test "sequence summary" test_sequence_summary;
    test "sequence summary: length differences" test_sequence_summary_length;
    test "sequence summary: tolerance arrays name the failing index"
      test_sequence_summary_float_tolerance_arrays;
    test "sequence summary: below the threshold" test_sequence_summary_threshold;
    test "sequence summary: long elements bounded"
      test_sequence_summary_bounded_elements;
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
    test "containment: headline forms" test_containment_headlines;
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
    test "hints: no run advertises --failed" test_no_rerun_hint;
    test "srandom replay line in failure blocks (D5 §6)"
      test_srandom_replay_line;
    test "verbose PASS prints the label table (D5 §7)" test_verbose_pass_labels;
    test "terminal name sanitization (render/F-2)" test_name_sanitization;
    test "excerpts resolve against the project root (render/F-1)"
      test_excerpt_project_root;
    test "the shared excerpt projection (Law 12)" test_shared_excerpt;
    test "excerpt regions window their context" test_excerpt_regions;
    test "line ranges collapse and format" test_line_ranges;
    test "snapshot report: wrote lines and the quiet gate"
      test_snapshot_report_writes;
    test "snapshot report: prune lines and refusals" test_snapshot_report_prune;
    test "snapshot report: orphan hints per invocation"
      test_snapshot_report_orphan_hint;
    test "snapshot report: the advisory stands down under --strict-snapshots"
      test_strict_snapshots_report;
    test "the coverage report's frozen bytes" test_coverage_report_bytes;
    test "the coverage thresholds are the renderer's"
      test_coverage_thresholds;
    test "mutation: the worked survivor report" test_mutation_report;
    test "mutation: the block wears the failure colours" test_mutation_colors;
    test "mutation: summary line forms" test_mutation_summary_forms;
    test "mutation: sections stand alone" test_mutation_sections;
    test "tree-wide summary dialect (harness parity)" test_summary_dialect;
  ]
