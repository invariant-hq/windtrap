(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The executable under test: an ordinary windtrap suite over an
   instrumented module, so that the whole mutation loop runs in a real
   process with a real catalogue. Which tests it DECLARES is chosen by
   MUTATE_FIXTURE, because the loop's refusals are properties of a suite
   and not of a build — the catalogue is the binary and is the same in
   every fixture.

   [catalogue] prints the mutant identifiers instead of running: the
   driver needs them to spell WINDTRAP_MUTATE_ARM, and reading them from
   the binary is the only spelling that cannot go stale when a line moves
   in subject.ml. *)

open Windtrap
module Subject = Mutate_loop_subject.Subject

(* Law 16(e)'s witness. Every process that reaches Stdlib's exit machinery
   appends its pid here; a mutation child must never appear, because it
   leaves through [Unix._exit] and its whole body is wrapped so that not
   even a fatal exception escapes to the toplevel handler — which runs
   [at_exit] before it prints. This is the general form of "no child
   overwrote the parent's .coverage dump": the coverage dump IS an at_exit
   handler, and so is this. *)
let () =
  match Sys.getenv_opt "MUTATE_ATEXIT_LOG" with
  | None | Some "" -> ()
  | Some path ->
      at_exit (fun () ->
          let oc = open_out_gen [ Open_append; Open_creat ] 0o644 path in
          output_string oc (string_of_int (Unix.getpid ()) ^ "\n");
          close_out oc)

let strong =
  [
    test "sub of two positives" (fun () -> equal int 6 (Subject.sub 10 4));
    test "sub to zero" (fun () -> equal int 0 (Subject.sub 4 4));
    test "sub of two negatives" (fun () ->
        equal int (-6) (Subject.sub (-10) (-4)));
  ]

(* Weak on purpose: true of [a + b] and of [a - b] alike, which is what
   makes the mutant survive and the survivor block a defect report about
   these two tests. *)
let weak =
  [
    test "widen is nonzero" (fun () -> is_true (Subject.widen 3 4 <> 0));
    test "widen is not 99" (fun () -> is_true (Subject.widen 1 2 <> 99));
  ]

(* Reaches the [@mutate off] site and pins nothing about it. Only the
   dismissal keeps it out of the survivor blocks and out of the
   denominator. *)
let dismissed =
  [
    test "the dismissed site is run and not pinned" (fun () ->
        is_true (Subject.dismissed 1 2 <> 99));
  ]

(* Two survivors, so that the cap has something to drop and the label has
   something to announce. [orphan] is weakly reached by one test and
   [widen] by two, so the block order is also a claim: most-watched
   first. *)
let two_survivors =
  [ test "orphan is nonzero" (fun () -> is_true (Subject.orphan 3 4 <> 0)) ]

let red = [ test "always red" (fun () -> equal int 1 2) ]

(* Green in the process that measured the reach map and red in every fork
   of it — the sharpest possible non-determinism, and the one the probe
   exists to catch: without it the loop would score every mutant against a
   suite that fails for reasons of its own. *)
let dry_run_pid = Unix.getpid ()

let flaky =
  [
    test "passes where it was measured and fails where it is re-run" (fun () ->
        is_true (Unix.getpid () = dry_run_pid));
  ]

(* Leaves through [Unix._exit], which the runner's exit guard does not
   intercept: armed, the child dies with no verdict on the pipe. *)
let crash =
  [
    test "crasher leaves without a word" (fun () ->
        if Subject.crasher 3 1 <> 2 then Unix._exit 3;
        equal int 2 (Subject.crasher 3 1));
  ]

(* Leaves through a FATAL exception, which no failure boundary in the
   runner may swallow ({!Failure.is_fatal}): armed, it escapes
   [Runner.execute] and reaches the mutation child's own wrapper, which is
   the only thing between it and OCaml's uncaught-exception handler — and
   that handler runs [at_exit]. Unarmed the answer is 2 and nothing
   raises, so the dry run is green. *)
let fatal =
  [
    test "crasher raises a fatal exception when the answer changes" (fun () ->
        if Subject.crasher 3 1 <> 2 then raise Stack_overflow;
        equal int 2 (Subject.crasher 3 1));
  ]

(* The reach map's boundaries, in one suite.

   - [orphan] is evaluated at module initialization below, before any test
     starts. It must stay unreached: a warm-fork loop can never arm it
     (module initialization ran before the fork), so folding that window
     into the first test would report a permanent false survivor.
   - [widen] is evaluated by the first and third tests and by no other, so
     the survivor block's witness list is a claim with four wrong answers
     available.
   - the first test needs a retry, and both attempts run the line: a
     retried test contributes one window, not two, and does not desync the
     determinism probe.
   - [crasher] is evaluated by a FIXTURE RELEASE, which runs after the
     last test finishes and belongs to no test. It must stay unreached
     too.

   [sub] is pinned by three tests and [widen] reached by two, so the
   most-reached mutant is one that dies and the forced-fail check passes
   on its own merits rather than on a tie-break. *)

(* Admission's shapes, one group each. Every group is built so a wrong
   answer from the machine changes a string the scenarios pin.

   - [vacuous] reaches two sites and pins neither, so the TRY cap has
     something to truncate and an exhaustive ruling something to list.
   - [skipper] skips when [widen] changes: a fault a test skipped under
     was never watched — it advances no tried count and appears in no
     UNJUSTIFIED list.
   - [capped_skipper] runs [widen] most and skips under it, with three
     sites in reach: a TRY=2 list is capped AND skip-shortened, so a
     ruling claiming its tried faults are the most-run ones would be
     false — the most-run one is exactly the one it skipped under.
   - [shared] declares a killer BEFORE a watcher of the same line, and
     the killer's own TRY=1 list holds [orphan], not [widen]: one no-bail
     fork of [widen] must admit the first through a ride-along kill and
     still deliver the second's pass outcome. Bail would starve the
     watcher of the one try it owns; an own-list-only batch would never
     run the killer under [widen] at all.
   - [fixture_kill] stands on [sub] through a bracket's setup: a kill
     through a dependency the test declared counts, and the witness says
     so.
   - [crash_pair] declares a watcher of [crasher]'s line BEFORE the test
     that dies under the same fault: the watcher's pass — its only try —
     is on the pipe when the child crashes, and the ruling must count it
     rather than discard the buffer with the child.
   - [always_skips] skips wherever it runs: an admission set with
     nothing executed in it is a refusal, not an empty report. *)

let vacuous =
  [
    test "touches widen and orphan and pins neither" (fun () ->
        is_true (Subject.widen 1 2 + Subject.orphan 3 4 <> 99));
  ]

let skipper =
  [
    test "skips when widen changes" (fun () ->
        let w = Subject.widen 3 4 in
        ignore (Subject.orphan 1 2);
        ignore (Subject.orphan 2 3);
        if w <> 7 then skip ~reason:"the subject changed under this test" ();
        is_true (Subject.orphan 1 2 <> 99));
  ]

let capped_skipper =
  [
    test "skips under the fault it runs most" (fun () ->
        let w = Subject.widen 3 4 in
        ignore (Subject.widen 1 2);
        ignore (Subject.widen 2 3);
        ignore (Subject.orphan 1 2);
        ignore (Subject.sub 10 4);
        if w <> 7 then skip ~reason:"the subject changed under this test" ();
        is_true (Subject.orphan 1 2 <> 99));
  ]

let shared =
  [
    test "pins widen through a shared fork" (fun () ->
        ignore (Subject.orphan 1 2);
        ignore (Subject.orphan 2 3);
        equal int 7 (Subject.widen 3 4));
    test "watches widen and pins nothing" (fun () ->
        is_true (Subject.widen 3 4 <> 0));
  ]

let fixture_kill =
  [
    bracket
      ~setup:(fun () ->
        equal ~msg:"the fixture stands on sub" int 6 (Subject.sub 10 4))
      ~teardown:(fun () -> ())
      "reads through a fixture"
      (fun () -> is_true true);
  ]

let crash_pair =
  [
    test "watches crasher and pins nothing" (fun () ->
        is_true (Subject.crasher 3 1 <> 99));
    test "dies when crasher changes" (fun () ->
        if Subject.crasher 3 1 <> 2 then Unix._exit 3;
        equal int 2 (Subject.crasher 3 1));
  ]

let always_skips =
  [
    test "skips wherever it runs" (fun () ->
        skip ~reason:"never runs on this fixture" ());
  ]

let retried = ref 0
let handle = fixture ~teardown:(fun () -> ignore (Subject.crasher 3 1)) Fun.id

let boundary =
  [
    test ~retries:1 "first reaches widen, after a retry" (fun () ->
        is_true (Subject.widen 3 4 <> 0);
        incr retried;
        if !retried = 1 then fail "flaky on the first attempt");
    test "second reaches sub and holds a fixture released at run end" (fun () ->
        handle ();
        equal int 6 (Subject.sub 10 4));
    test "third reaches widen" (fun () -> is_true (Subject.widen 1 2 <> 99));
    test "fourth reaches sub" (fun () -> equal int 0 (Subject.sub 4 4));
    test "fifth reaches sub" (fun () -> equal int (-6) (Subject.sub (-10) (-4)));
  ]

let () =
  match Option.value ~default:"green" (Sys.getenv_opt "MUTATE_FIXTURE") with
  | "catalogue" ->
      List.iter
        (fun (m : Windtrap_mutate.mutant) ->
          print_endline (Windtrap_mutate.id_to_string m.Windtrap_mutate.id))
        (Windtrap_mutate.catalogue ())
  | "weak" -> run "calc" [ group "widen" weak ]
  | "vacuous" -> run "calc" [ group "vacuous" vacuous ]
  | "skipper" -> run "calc" [ group "skipper" skipper ]
  | "capped_skipper" -> run "calc" [ group "capped" capped_skipper ]
  | "shared" -> run "calc" [ group "shared" shared ]
  | "fixture" -> run "calc" [ group "fixture" fixture_kill ]
  | "crash_pair" -> run "calc" [ group "crash" crash_pair ]
  | "skips" -> run "calc" [ group "skip" always_skips ]
  | "crash" ->
      run "calc"
        [ group "calc" strong; group "widen" weak; group "crash" crash ]
  | "fatal" ->
      run "calc"
        [ group "calc" strong; group "widen" weak; group "crash" fatal ]
  | "capped" ->
      run "calc"
        [
          group "calc" strong; group "widen" weak; group "orphan" two_survivors;
        ]
  | "red" -> run "calc" [ group "calc" (strong @ red); group "widen" weak ]
  | "flaky" -> run "calc" [ group "calc" strong; group "flaky" flaky ]
  | "boundary" ->
      (* Outside any test, and before the first one starts. *)
      ignore (Subject.orphan 1 2);
      run "calc" [ group "widen" boundary ]
  | "tagged" ->
      (* [Tag.default_predicate] drops [disabled] by itself, so this suite
         runs only under --tag/WINDTRAP_TAG: a tag predicate is not
         expressible as a set of paths, and a child that dropped it would
         run fewer tests than the dry run measured. *)
      run "calc"
        [
          group ~tags:[ "disabled" ] "calc" strong;
          group ~tags:[ "disabled" ] "widen" weak;
        ]
  | _ ->
      run "calc"
        [ group "calc" strong; group "widen" weak; group "dismissed" dismissed ]
