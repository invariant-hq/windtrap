(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The suites of workers.t, which WORKERS_SUITE selects: baselines that
   tests share, across workers. Under REPLACE=domain the tests that spawn a
   domain retire their worker, which forces a replacement where an
   in-process run would have gone on; under REPLACE=crash the test between
   two checks of one site kills its worker. Naps make tests finish out of
   declaration order under -j N. *)

open Windtrap

let nap s = Unix.sleepf s

let between () =
  match Sys.getenv_opt "REPLACE" with
  | Some "domain" -> Domain.join (Domain.spawn ignore)
  | Some "crash" -> Unix._exit 3
  | Some _ | None -> ()

(* One literal site reached by two tests with different output: the second
   check is a conflict, whatever ran between them. *)
let conflict () =
  let check s = expect s @@ __POS_OF__ {|old|} in
  [
    test "a" (fun () -> check "one");
    test "between" between;
    test "b" (fun () -> check "two");
  ]

(* Corrections spread across tests: sites corrected, matched, conflicting
   and agreeing, a table, two files and two spellings of a third, a missing
   file, subtests, an expected failure, a failure outside the expectations
   and a retried conflict. *)
let spread () =
  let s1 x = expect x @@ __POS_OF__ {| s1 old |} in
  let s2 x = expect x @@ __POS_OF__ {| s2 old |} in
  let s3 x = expect_exact x @@ __POS_OF__ {|s3 old|} in
  let s4 x = expect x @@ __POS_OF__ {| s4 old |} in
  let file = "test/cram/run/h.expected" in
  [
    test "a1 slow, corrects s1" (fun () ->
        nap 0.05;
        s1 "s1 new");
    test "a2 conflicts on s1" (fun () ->
        between ();
        s1 "s1 other");
    test "b1 matches s2" (fun () -> s2 "s2 old");
    test "b2 slow, corrects s2" (fun () ->
        nap 0.03;
        between ();
        s2 "s2 new");
    test "b3 matches the old s2" (fun () -> s2 "s2 old");
    test "c1 corrects s3" (fun () -> s3 "s3\r\nnew");
    test "c2 agrees on s3" (fun () ->
        between ();
        s3 "s3\r\nnew");
    cases "row" fst
      [ ("x", __POS_OF__ {| row x old |}); ("y", __POS_OF__ {| row y old |}) ]
      (fun (n, lit) -> expect ("row " ^ n ^ " new") lit);
    test "d fails outside" (fun () ->
        s4 "s4 d";
        fail "d broke");
    xfail (test "e xfail" (fun () -> s4 "s4 e"));
    test "f subtests" (fun () ->
        subtest "x" (fun () -> s4 "s4 x");
        subtest "y" (fun () -> s4 "s4 y"));
    test "g labelled" (fun () -> subtest "inner" (fun () -> s1 "s1 g"));
    test "h1 corrects a file" (fun () ->
        nap 0.03;
        expect_file "h new" file);
    test "h2 conflicts on the file" (fun () ->
        between ();
        expect_file "h newer" file);
    test "m missing file" (fun () ->
        expect_file "m\n" "test/cram/run/missing.expected");
    test "k1 slow, one spelling" (fun () ->
        nap 0.05;
        expect_file "k one" "test/cram/run/k.expected");
    test "k2 other spelling" (fun () ->
        expect_file "k two" "test/cram/run/./k.expected");
    test ~retries:2 "r retried conflict" (fun () -> s1 "s1 r");
    test "o order" (fun () ->
        subtest "first" (fun () -> fail "first broke");
        s2 "s2 o";
        subtest "last" (fun () -> fail "last broke"));
    test "z corrects s4" (fun () -> s4 "s4 z");
  ]

(* A table whose rows check baselines, each settling alone and in order: a
   row that also fails outside its expectations keeps no correction, so a
   later row that checks its file is compared with the file as it was, and
   one more test checks the file the last row corrected, and a property's
   law checks a literal, which is never corrected. Under WORKERS_SUITE=table
   the table runs alone. *)
let rows () =
  let file = "test/cram/run/t.expected" in
  cases "rows"
    (fun (n, _, _) -> n)
    [
      ("a", __POS_OF__ {| a old |}, false);
      ("b", __POS_OF__ {| b old |}, true);
      ("c", __POS_OF__ {| c old |}, false);
      ("d", __POS_OF__ {| d new |}, false);
    ]
    (fun (n, lit, broken) ->
      expect (n ^ " new") lit;
      if n = "b" || n = "c" then expect_file ("t " ^ n) file;
      if broken then fail (n ^ " broke"))

let table () =
  [
    test "slow, corrects s5" (fun () ->
        nap 0.05;
        expect "s5 new" @@ __POS_OF__ {| s5 old |});
    rows ();
    prop "a law checks s6" (Gen.int_range 0 1000) (fun n ->
        if n >= 10 then expect (string_of_int n) @@ __POS_OF__ {| s6 old |});
    test "agrees on the file" (fun () ->
        between ();
        expect_file "t c" "test/cram/run/t.expected");
  ]

let () =
  let suite = Sys.getenv "WORKERS_SUITE" in
  let tests =
    match suite with
    | "conflict" -> conflict ()
    | "spread" -> spread ()
    | "table" -> [ rows () ]
    | "tables" -> table ()
    | other -> invalid_arg ("workers_main: unknown WORKERS_SUITE " ^ other)
  in
  exit (run suite tests)
