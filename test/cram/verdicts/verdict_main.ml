(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The suites the verdict sessions run, one per situation, selected by
   VERDICT_SUITE and run under that name. Each is the few tests its
   session's block describes. *)

open Windtrap

external spin : int -> unit = "verdict_spin"
external poke : int -> unit = "verdict_poke"

let passes = test "passes" ignore
let fails = test "fails" (fun () -> equal int 1 2)
let exits = test "exits" (fun () -> exit 0)

(* An exit raised on a domain that no test joins: [late] lets it go and
   waits until the domain has ended, or for a fifth of a second, since the
   domain dies with the worker of the test that spawned it. *)
let unjoined () =
  let go = Atomic.make false and gone = Atomic.make false in
  let spawns =
    test "spawns a domain" (fun () ->
        ignore
          (Domain.spawn (fun () ->
               Domain.at_exit (fun () -> Atomic.set gone true);
               while not (Atomic.get go) do
                 Domain.cpu_relax ()
               done;
               exit 0)))
  in
  let late =
    test "runs while it exits" (fun () ->
        Atomic.set go true;
        let start = Unix.gettimeofday () in
        while (not (Atomic.get gone)) && Unix.gettimeofday () -. start < 0.2 do
          Domain.cpu_relax ()
        done)
  in
  (spawns, late)

let registers handler = test "registers" (fun () -> at_exit handler)

let forks =
  test "forks" (fun () ->
      match Unix.fork () with
      | 0 -> exit 0
      | pid -> ignore (Unix.waitpid [] pid))

let exits_on_teardown =
  let held = fixture ~teardown:(fun () -> exit 0) ignore in
  test "acquires" (fun () -> held ())

let fails_on_teardown =
  let held = fixture ~teardown:(fun () -> failwith "teardown") ignore in
  test "acquires" (fun () -> held ())

let rec deep n = 1 + deep (n + 1)
let alpha = test ~tags:[ "fast" ] "alpha" ignore

let skips =
  group "skips"
    [
      test "skips" (fun () -> skip ());
      test "skips with a reason" (fun () -> skip ~reason:"not here" ());
    ]

let record = [ test "first" ignore; test "second" ignore ]

let interrupts =
  test "interrupts itself" (fun () -> Unix.kill (Unix.getpid ()) Sys.sigint)

let suite = function
  | "exit_before_fail" -> [ passes; exits; fails ]
  | "exit_after_fail" -> [ passes; fails; exits ]
  | "exit_in_prop" -> [ passes; prop "exits" Gen.int (fun _ -> exit 0) ]
  | "exit_release" -> [ exits_on_teardown ]
  | "exit_release_after_fail" -> [ fails; exits_on_teardown ]
  | "domain_before_fail" ->
      [
        passes;
        test "exits on a domain" (fun () ->
            Domain.join (Domain.spawn (fun () -> exit 0)));
        fails;
      ]
  | "domain_after_fail" ->
      [
        passes;
        fails;
        test "exits on a domain" (fun () ->
            Domain.join (Domain.spawn (fun () -> exit 0)));
      ]
  | "domain_unjoined_only" ->
      let spawns, late = unjoined () in
      [ spawns; late ]
  | "domain_unjoined" ->
      let spawns, late = unjoined () in
      [ fails; spawns; late ]
  | "atexit_in_test_exit" ->
      [
        passes;
        test "registers and exits" (fun () ->
            at_exit (fun () -> exit 0);
            exit 1);
        fails;
      ]
  | "atexit_after_fail" -> [ fails; registers (fun () -> exit 0); passes ]
  | "atexit_before_fail" -> [ registers (fun () -> exit 0); fails; passes ]
  | "atexit_raise" -> [ passes; registers (fun () -> failwith "cleanup") ]
  | "unix_exit_before_fail" ->
      [ test "exits at once" (fun () -> Unix._exit 0); fails ]
  | "unix_exit_after_fail" ->
      [ fails; test "exits at once" (fun () -> Unix._exit 0) ]
  | "fork_child_exit" -> [ passes; forks; fails ]
  | "try_with" ->
      [ test "catches its exit" (fun () -> try exit 0 with _ -> ()) ]
  | "try_with_assert_only" ->
      let tolerant () = try equal int 1 2 with _ -> () in
      [ test "swallows a failing verb" tolerant ]
  | "try_with_expect" ->
      [
        test "catches around expect" (fun () ->
            try expect "actual" @@ __POS_OF__ {|expected|} with _ -> ());
      ]
  | "stackoverflow" ->
      [ passes; test "recurses" (fun () -> ignore (deep 0 : int)) ]
  | "fatal_oom" ->
      [ fails; test "runs out of memory" (fun () -> raise Out_of_memory) ]
  | "fatal_break" -> [ fails; test "breaks" (fun () -> raise Sys.Break) ]
  | "skip" ->
      [
        passes;
        test "skips" (fun () -> skip ());
        test "skips with a reason" (fun () -> skip ~reason:"not here" ());
        prop "skips every case" Gen.int (fun _ -> skip ());
      ]
  | "skip_all" -> [ skips; passes ]
  | "xfail" ->
      [
        passes;
        xfail (test "fails as expected" (fun () -> equal int 1 2));
        xfail (test "exits as expected" (fun () -> exit 0));
      ]
  | "xpass" -> [ xfail (test "passes unexpectedly" ignore) ]
  | "selection" -> [ alpha ]
  | "one_line" -> [ passes; skips ]
  | "none" -> []
  | "stream_nonl" ->
      [ test "prints a partial line" (fun () -> print_string "partial"); fails ]
  | "record" -> record
  | "record_renamed" -> [ test "first" ignore; test "renamed" ignore ]
  | "prop_discard" ->
      [
        prop "assumes" Gen.int (fun _ -> assume false);
        prop "filters" (Gen.such_that (fun _ -> false) Gen.int) ignore;
        prop "rejects" Gen.int (fun _ -> reject ());
      ]
  | "prop_count_zero" ->
      [
        prop ~count:0 "never runs" Gen.int (fun _ -> fail "the law ran");
        prop "fails" Gen.int (fun _ -> fail "the law ran");
      ]
  | "table_zero" -> [ cases "rows" ~name:Fun.id [] ignore ]
  | "table_zero_plus" -> [ cases "rows" ~name:Fun.id [] ignore; passes ]
  | "baseline_literal" ->
      [ test "never recorded" (fun () -> expect "hello" @@ __POS_OF__ {||}) ]
  | "baseline_file" ->
      [
        test "reads a missing file" (fun () ->
            expect_file "hello\n" "test/cram/verdicts/never.expected");
      ]
  | "baseline_plain" ->
      [
        test "mismatches" (fun () -> expect "actual" @@ __POS_OF__ {|expected|});
      ]
  | "dup_paths" -> [ passes; passes ]
  | "focus" -> [ focus passes; fails ]
  | "release_fail" -> [ fails_on_teardown ]
  | "junit" -> [ passes ]
  | "junit_fail" -> [ passes; fails ]
  | "sigint" -> [ fails; interrupts; passes ]
  | "shared_a" -> [ test "same path" (fun () -> equal int 1 2) ]
  | "shared_b" -> [ test "same path" ignore ]
  | "segfault" ->
      [
        passes;
        test "writes at address 0" (fun () -> poke 0);
        test "comes after" ignore;
      ]
  | "c_loop" -> [ test ~timeout:0.05 "loops in C" (fun () -> spin 300); passes ]
  | other -> invalid_arg ("verdict_main: unknown VERDICT_SUITE " ^ other)

let name = function "shared_a" | "shared_b" -> "shared" | name -> name

(* Under VERDICT_DOMAIN_FIRST the program spawns a domain before [run],
   after which it cannot fork. *)
let () =
  if Sys.getenv_opt "VERDICT_DOMAIN_FIRST" <> None then
    Domain.join (Domain.spawn ignore);
  let selected = Sys.getenv "VERDICT_SUITE" in
  exit (run (name selected) (suite selected))
