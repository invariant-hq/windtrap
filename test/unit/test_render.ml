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
      Render.header r ~suite:"mylib" ~tests:(List.length Fixtures.results) ~seed;
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
   and the glyph row — byte-identical to streaming from the start. *)

let expected_header = "mylib: 11 tests (seed s1:7be1d2c904aa31f5)\n"

(* One glyph per fixture result: pass, six counted failures, three more
   passes, one skip. The first pass buffers; the first counted failure
   flushes the header and the accumulated row, then glyphs stream. *)
let expected_glyph_row = ".FFFFFF...S\n"

let expected_verbose_lines =
  {|  PASS  math › addition                            0.1ms
  FAIL  users › sessions after login               0.2ms
  FAIL  parser › rejects empty                     0.2ms
  FAIL  cli › cli help — no baseline               0.3ms
  FAIL  cli › snapshot drift                       0.2ms
  FAIL  geo › area non-negative                    18ms
  FAIL  db › insert                                0.2ms
  PASS  flaky › eventually                         0.2ms (3 attempts)
  PASS  slow › big sort                            2.50s
  PASS  slow › hash                                3.00s
  SKIP  platform › windows paths (unix only)
|}

let expected_failures =
  {|──────────────────── failures (6) ────────────────────
  FAIL  users › sessions after login
    test/test_users.ml:31
    expected  [("alice", [1; 2; 3]); ("bob", [4])]
                                     ~~~~~~~~~~~~
    actual    [("alice", [1; 2; 3]); ("bob", [4; 5]); ("carol", [])]
                                     ~~~~~~~~~~~~~~~  ~~~~~~~~~~~~~
    ── captured output (last 2 lines, 12034 earlier bytes omitted) ──
    [debug] session table resize 2 -> 4
    [debug] carol: ghost session from pool reuse
    full log: _build/_tests/mylib/latest/users/sessions-after-login.output

  FAIL  parser › rejects empty
    test/test_parser.ml:12
    expected exception  Parse_error("empty")
    raised              Not_found
    Raised at Parser.parse in file "lib/parser.ml", line 40

  FAIL  cli › cli help
    test/test_cli.ml:9
    snapshot "help": no baseline at test/__snapshots__/test_cli/help.snap
    proposed (3 lines):
      ┆ Usage: mytool [OPTIONS] COMMAND
      ┆ Commands:
      ┆   run
    accept: dune exec test/main.exe -- -u, then review with git diff

  FAIL  cli › snapshot drift
    test/test_cli.ml:14
    snapshot "version": mismatch with test/__snapshots__/test_cli/version.snap
    @@ -1,3 +1,3 @@
      line one
    - line two
    + line 2
      line three
    accept: dune exec test/main.exe -- -u, then review with git diff

  FAIL  geo › area non-negative
    test/test_geo.ml:17
    counterexample (case 12, shrunk 4 steps): Rect (2, 0)
    which failed at:
      test/test_geo.ml:18
      expected  true
      actual    false
    replay: dune exec test/main.exe -- --seed s1:7be1d2c904aa31f5 -f 'geo › area non-negative'

  FAIL  db › insert
    test/test_db.ml:21
    body exploded
    [teardown]
    teardown exploded
──────────────────────────────────────────────────────

|}

(* The two second-plus passes are untagged in the fixture data, so they
   earn the faint-yellow block and the one trailing opt-out hint. Slowest
   first, durations right-aligned in their own column; the hint names the
   flag because these transcripts run under an [`Exe] invocation. *)
let expected_slow_warnings =
  {|slow tests (2):
  3.00s  slow › hash
  2.50s  slow › big sort
(exempt with the "slow" tag, or raise --slow-threshold SECONDS)

|}

let expected_summary = {|4 passed, 1 skipped, 6 failed in 6.5s.
|}

(* Verbose only: the slowest list is diagnosis, not signal. *)
let expected_slowest =
  {|
slowest tests:
  3.00s  slow › hash
  2.50s  slow › big sort
   18ms  geo › area non-negative
  0.3ms  cli › cli help
  0.2ms  users › sessions after login
|}

let expected_coverage_line =
  "coverage: 87.2% (312/358 points) · WINDTRAP_COVERAGE=report for detail\n"

let expected_end = expected_failures ^ expected_slow_warnings ^ expected_summary
let golden_exe = "dune exec test/main.exe --"
let golden_invocation = `Exe golden_exe
let golden_coverage = { Run.visited = 312; total = 358; siblings = false }

let test_golden_compact () =
  let actual =
    transcript ~invocation:golden_invocation ~coverage:golden_coverage ()
  in
  check_string "golden compact transcript (the default)"
    ~expected:
      (expected_header ^ expected_glyph_row ^ expected_end
     ^ expected_coverage_line)
    ~actual;
  check_absent "plain transcript has no escape codes" ~sub:"\027" actual

let test_golden_verbose () =
  let actual =
    transcript ~mode:`Verbose ~invocation:golden_invocation
      ~coverage:golden_coverage ()
  in
  check_string "golden verbose transcript"
    ~expected:
      (expected_header ^ expected_verbose_lines ^ expected_end
     ^ expected_slowest ^ expected_coverage_line)
    ~actual;
  check_absent "plain transcript has no escape codes" ~sub:"\027" actual

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
        Render.header r ~suite:"s" ~tests:1 ~seed:None;
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
        Render.header r ~suite:"mylib" ~tests:2 ~seed:None;
        Render.begin_test r ~path:[ "math"; "addition" ];
        Render.result r (List.hd Fixtures.results))
  in
  check_contains "live: progress line drawn"
    ~sub:"Running [1/2] math › addition" t;
  check_contains "live: cursor clear emitted" ~sub:"\r\027[2K" t;
  let plain =
    with_renderer ~ansi:false ~mode:`Verbose ~live:true (fun r ->
        Render.header r ~suite:"mylib" ~tests:2 ~seed:None;
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
        Render.header r ~suite:"mylib" ~tests:2 ~seed:None;
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
        Render.header r ~suite:"mylib" ~tests:2 ~seed:None;
        Render.result r
          (Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "b" ]));
        Render.begin_test r ~path:[ "math"; "addition" ];
        Render.result r (List.hd Fixtures.results))
  in
  check_contains "compact tail: flushed erase re-prints the committed row"
    ~sub:"\r\027[2K\027[31mF\027[0m" flushed;
  let plain =
    with_renderer ~ansi:false ~live:true (fun r ->
        Render.header r ~suite:"mylib" ~tests:2 ~seed:None;
        Render.begin_test r ~path:[ "math"; "addition" ])
  in
  check_absent "compact tail: off without ansi" ~sub:"[1/2]" plain

let test_header_forms () =
  let one =
    with_renderer ~mode:`Verbose (fun r ->
        Render.header r ~suite:"s" ~tests:1 ~seed:None)
  in
  check_string "header: singular, no seed" ~expected:"s: 1 test\n" ~actual:one;
  let zero =
    with_renderer ~mode:`Verbose (fun r ->
        Render.header r ~suite:"s" ~tests:0 ~seed:None)
  in
  check_string "header: zero tests" ~expected:"s: 0 tests\n" ~actual:zero;
  (* Compact defers the header until the run proves noteworthy; the same
     line then prints from the recorded fields (the golden transcripts pin
     the flushed form). *)
  let deferred =
    with_renderer (fun r -> Render.header r ~suite:"s" ~tests:1 ~seed:None)
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
  let t =
    with_renderer (fun r -> Render.finish r ~results:[] ~duration:0.01 ())
  in
  check_string "finish: empty run" ~expected:"no tests ran.\n" ~actual:t

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
        if header then Render.header r ~suite:"s" ~tests:(n + 1) ~seed:None;
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
        Render.header r ~suite:"s" ~tests:2 ~seed:None;
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
        Render.header r ~suite:"s" ~tests:2 ~seed:None;
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
        Render.header r ~suite:"s" ~tests:2 ~seed:None;
        Render.result r (List.nth results 0);
        Render.note r "releasing db";
        Render.result r (List.nth results 1);
        Render.finish r ~results ~duration:0.01 ())
  in
  check_contains "note: a later flush shows the buffered notice in position"
    ~sub:"s: 2 tests\n.\nreleasing db\nF\n" noteworthy;
  let flushed =
    with_renderer (fun r ->
        Render.header r ~suite:"s" ~tests:2 ~seed:None;
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
        Render.header r ~suite:"s" ~tests:2 ~seed:None;
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
        Render.header r ~suite:"mylib" ~tests:1 ~seed:None;
        List.iter (Render.result r) passes;
        Render.finish r ~results:passes ~duration:1.2 ())
  in
  check_string "green compact run: exactly one named line"
    ~expected:"mylib: 1 passed in 1.2s.\n" ~actual:named;
  let seeded =
    with_renderer (fun r ->
        Render.header r ~suite:"mylib" ~tests:1 ~seed:(Some Fixtures.root);
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
        Render.header r ~suite:"mylib" ~tests:3 ~seed:None;
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
        Render.header r ~suite:"mylib" ~tests:0 ~seed:None;
        Render.finish r ~results:[] ~duration:0.01 ())
  in
  check_string "empty compact selection: one named line, no header"
    ~expected:"mylib: no tests ran.\n" ~actual:empty

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
  Render.header r ~suite:"s" ~tests:4 ~seed:None;
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
        Render.header r ~suite:"s" ~tests:1 ~seed:None;
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
        Render.header r ~suite:"s" ~tests:1 ~seed:None;
        Render.result r tagged_pass;
        Render.finish r ~results:[ tagged_pass ] ~duration:1.2 ())
  in
  check_string "a slow-tagged test is exempt everywhere: one line, no warning"
    ~expected:"s: 1 passed in 1.2s.\n" ~actual:tagged;
  let skip = Fixtures.result [ "t" ] (Failure.Skip None) ~duration:2.0 in
  let skipped =
    with_renderer (fun r ->
        Render.header r ~suite:"s" ~tests:1 ~seed:None;
        Render.result r skip;
        Render.finish r ~results:[ skip ] ~duration:2.0 ())
  in
  check_string "a skip never triggers the threshold"
    ~expected:"s: 1 skipped in 2s.\n" ~actual:skipped;
  (* An excused expected failure is not a counted failure — but its
     duration still counts against the threshold when untagged. *)
  let excused_fast =
    with_renderer (fun r ->
        Render.header r ~suite:"s" ~tests:1 ~seed:None;
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
        Render.header r ~suite:"s" ~tests:1 ~seed:None;
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
        Render.header r ~suite:"s" ~tests:1 ~seed:None;
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
        Render.header r ~suite:"s" ~tests:1 ~seed:None;
        Render.result r slow_pass;
        Render.finish r ~results:[ slow_pass ] ~duration:5.0 ())
  in
  check_string "threshold 0 disables the trigger and the warnings"
    ~expected:"s: 1 passed in 5s.\n" ~actual:t;
  let still_flushes =
    let fail = Fixtures.result [ "t" ] (Failure.Fail [ Failure.message "x" ]) in
    with_renderer ~slow_threshold:0.0 (fun r ->
        Render.header r ~suite:"s" ~tests:1 ~seed:None;
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
        Render.header r ~suite:"s" ~tests:1 ~seed:None;
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
  (* The summary line shows a bounded excerpt (39 code points then an
     ellipsis); the full element still appears in the detailed diff below. *)
  check_contains "sequence summary: long element truncated with an ellipsis"
    ~sub:("actual \"" ^ String.make 38 'x' ^ "...")
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
  (* Different constructors keep the two-line rendering. *)
  let b = failure_block Fixtures.raise_failure in
  check_contains "raise: different constructors unchanged"
    ~sub:"expected exception" b;
  (* Same constructor but a hand-built failure with equal messages: nothing
     to diff, keep the plain rendering. *)
  let equal_messages =
    Failure.raised ~expected:{|Failure("boom")|} ~actual:{|Failure("boom")|}
      ~same_constructor:true ~expected_message:"boom" ~actual_message:"boom" ()
  in
  check_contains "raise: equal messages keep the plain form"
    ~sub:"expected exception"
    (failure_block equal_messages);
  (* raises_match's enriched payload still prints the raised exception. *)
  let predicate_miss =
    Failure.raised ~actual:{|Invalid_argument("nope")|} ~actual_message:"nope"
      ~predicate:true ()
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
        Render.header r ~suite:"s" ~tests:1 ~seed:None;
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

(* Claim-aware containment (D5 §2) *)

let not_contains_failure =
  Failure.containment ~found_at:10 ~expected:{|string not containing "secret"|}
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
    Failure.containment ~expected:{|string containing "NOPE"|} ~needle:"NOPE"
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
    Failure.containment ~expected:{|string containing "user=bob"|}
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
    Failure.containment ~found_at:9 ~expected:{|string not containing "secret"|}
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
    Failure.containment ~expected:{|string containing "NOPE"|} ~needle:"NOPE"
      ~haystack:(String.make 20_006 'a') ()
  in
  check "headline: contains names the haystack size"
    (Render.headline contains_failure
    = {|needle "NOPE" not found (20006-byte haystack)|})

let test_satisfies_no_refinement () =
  (* The claim sentence is a description, not a rendering: never diff or
     refine the two (D5 §2). *)
  let f =
    Failure.equality ~claim:Failure.Satisfies
      ~expected:"value satisfying the predicate" ~actual:"-3" ()
  in
  let b = failure_block f in
  check_contains "satisfies: two label lines"
    ~sub:"    expected  value satisfying the predicate\n    actual    -3\n" b;
  check_absent "satisfies: no marker line against the claim" ~sub:"~" b;
  let multi =
    failure_block
      (Failure.equality ~claim:Failure.Satisfies
         ~expected:"value satisfying the predicate" ~actual:"[0; 1;\n 2]" ())
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
    failure_block
      (Failure.equality ~claim:Failure.Matches ~expected:"a match"
         ~actual:"Error \"boom\"" ())
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
  check_contains "ansi path carries the same glyph inside the red span"
    ~sub:"\027[31m- line one\u{00B7}\027[0m" colored;
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
        Render.header r ~suite:"vnames" ~tests:2 ~seed:None;
        Render.begin_test r ~path:hostile)
  in
  check_contains "live tail escapes the newline" ~sub:{|first\nhalf|} live;
  check_absent "live tail carries no raw newline" ~sub:"first\nhalf" live;
  (* Suite names: header, deferred one-liner, quiet summary prefix. *)
  let named =
    with_renderer ~mode:`Quiet (fun r ->
        Render.header r ~suite:"my\tsuite" ~tests:1 ~seed:None;
        Render.result r (Fixtures.result [ "t" ] Failure.Pass);
        Render.finish r
          ~results:[ Fixtures.result [ "t" ] Failure.Pass ]
          ~duration:0.1 ())
  in
  check_contains "summary prefix escapes the tab" ~sub:{|my\tsuite: 1 passed|}
    named;
  let header =
    with_renderer ~mode:`Verbose (fun r ->
        Render.header r ~suite:"a\x07b" ~tests:1 ~seed:None)
  in
  check_contains "header escapes control bytes" ~sub:{|a\x07b: 1 test|} header;
  (* Slow warnings and the slowest list share the treatment. *)
  let slow = Fixtures.result [ "sl\now" ] Failure.Pass ~duration:1.5 in
  let warned =
    with_renderer (fun r ->
        Render.header r ~suite:"s" ~tests:1 ~seed:None;
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
        Render.header r ~suite:"mylib" ~tests:(List.length results) ~seed:None;
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

let tests =
  [
    test "golden compact transcript (default)" test_golden_compact;
    test "golden verbose transcript (-v)" test_golden_verbose;
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
    test "tree-wide summary dialect (harness parity)" test_summary_dialect;
  ]
