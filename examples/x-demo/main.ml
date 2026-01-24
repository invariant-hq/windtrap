(** Windtrap Visual Demo

    Run this example to see what test output looks like for:
    - Passing tests with nested groups
    - Failing assertions (equality, options, results)
    - Exception mismatches
    - Output/expect mismatches
    - Snapshot diffs
    - Property counterexamples
    - Skipped tests

    Usage: dune exec ./examples/x-demo/main.exe dune exec
    ./examples/x-demo/main.exe -- --stop-on-error dune exec
    ./examples/x-demo/main.exe -- --filter "Passing" *)

open Windtrap

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Passing Tests — Demonstrates nested groups and various assertions
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let passing_tests =
  group "Passing"
    [
      group "Math"
        [
          group "Arithmetic"
            [
              test "addition" (fun () -> equal Testable.int 5 (2 + 3));
              test "subtraction" (fun () -> equal Testable.int 7 (10 - 3));
              test "multiplication" (fun () -> equal Testable.int 12 (3 * 4));
            ];
          group "Comparisons"
            [
              test "less than" (fun () -> is_true (1 < 2));
              test "greater than" (fun () -> is_true (5 > 3));
            ];
        ];
      group "Data structures"
        [
          test "option Some" (fun () -> some Testable.int 42 (Some 42));
          test "option None" (fun () -> is_none None);
          test "result Ok" (fun () -> ok Testable.string "yes" (Ok "yes"));
          test "result Error" (fun () ->
              error Testable.string "no" (Error "no"));
          test "list equality" (fun () ->
              equal (Testable.list Testable.int) [ 1; 2; 3 ] [ 1; 2; 3 ]);
        ];
      group "Exceptions"
        [
          test "raises expected" (fun () ->
              raises (Failure "boom") (fun () -> failwith "boom"));
          test "no exception" (fun () ->
              let x = no_raise (fun () -> 2 + 2) in
              equal Testable.int 4 x);
        ];
    ]

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Failing Assertions — Shows what failure output looks like
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let failing_assertions =
  group "Failing Assertions"
    [
      group "Equality"
        [
          test "int mismatch" (fun () -> equal Testable.int 42 99);
          test "string mismatch" (fun () ->
              equal Testable.string "expected" "actual");
          test "list mismatch" (fun () ->
              equal (Testable.list Testable.int) [ 1; 2; 3 ] [ 1; 9; 3 ]);
        ];
      group "Options"
        [
          test "expected Some got None" (fun () -> some Testable.int 42 None);
          test "expected None got Some" (fun () -> is_none (Some "oops"));
        ];
      group "Results"
        [
          test "expected Ok got Error" (fun () ->
              ok Testable.int 42 (Error "failed"));
          test "wrong Ok value" (fun () -> ok Testable.string "yes" (Ok "no"));
        ];
      group "Booleans"
        [
          test "expected true" (fun () -> is_true false);
          test "expected false" (fun () -> is_false true);
        ];
    ]

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Failing Exceptions — Wrong or missing exceptions
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let failing_exceptions =
  group "Failing Exceptions"
    [
      test "wrong exception" (fun () ->
          raises (Failure "expected") (fun () -> failwith "actual"));
      test "no exception raised" (fun () ->
          raises (Failure "boom") (fun () -> 42 |> ignore));
      test "unexpected exception" (fun () ->
          let _ = no_raise (fun () -> failwith "surprise") in
          ());
    ]

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Expect Tests — Output capture mismatches
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let expect_tests =
  group "Expect"
    [
      group "Passing"
        [
          test "simple match" (fun () ->
              print_string "Hello, World!";
              expect "Hello, World!");
          test "multiline" (fun () ->
              print_endline "line 1";
              print_endline "line 2";
              expect "line 1\nline 2");
        ];
      group "Failing"
        [
          test "wrong output" (fun () ->
              print_string "Goodbye";
              expect "Hello");
          test "extra output" (fun () ->
              print_endline "first";
              print_endline "second";
              print_endline "third";
              expect "first\nsecond");
        ];
    ]

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Snapshot Tests — File-based comparison with diffs
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let snapshot_tests =
  group "Snapshots"
    [
      group "Passing"
        [
          test "json" (fun () ->
              snapshot ~pos:__POS__ ~name:"config"
                {|{"name": "test", "value": 42}|});
          test "multiline" (fun () ->
              snapshot ~pos:__POS__ ~name:"report" "Line 1\nLine 2\nLine 3");
        ];
      group "Failing"
        [
          test "single line diff" (fun () ->
              (* Snapshot says "hello" but we produce "goodbye" *)
              snapshot ~pos:__POS__ ~name:"greeting" "goodbye");
          test "multiline diff" (fun () ->
              (* Shows unified diff for changed content *)
              snapshot ~pos:__POS__ ~name:"document"
                {|{
  "name": "Bob",
  "age": 35,
  "city": "London"
}|});
        ];
    ]

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Property Tests — Shows shrunk counterexamples
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let property_tests =
  group "Properties"
    [
      group "Passing"
        [
          prop "reverse involutive"
            Testable.(list int)
            (fun l -> List.rev (List.rev l) = l);
          prop "addition commutative"
            Testable.(pair int int)
            (fun (a, b) -> a + b = b + a);
        ];
      group "Failing"
        [
          prop "all lists are short"
            Testable.(list int)
            (fun l -> List.length l < 5);
          prop "all numbers are small" Testable.int (fun n -> abs n < 100);
        ];
    ]

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Skipped Tests — Conditional execution
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let skipped_tests =
  group "Skipped"
    [
      test "platform check" (fun () -> skip ~reason:"Not running on Windows" ());
      test "feature flag" (fun () -> skip ~reason:"Feature X not enabled" ());
      test "network disabled" (fun () ->
          skip ~reason:"Network tests disabled in CI" ());
    ]

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Slow Tests — Skipped with --quick
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let slow_tests =
  group "Slow"
    [
      slow "computation" (fun () ->
          let sum = List.init 50000 succ |> List.fold_left ( + ) 0 in
          equal Testable.int 1250025000 sum);
    ]

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Custom Messages — Contextual failure info
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let message_tests =
  group "Custom Messages"
    [
      test "with context" (fun () ->
          let user_count = 5 in
          let expected = 10 in
          equal ~msg:"user count after signup" Testable.int expected user_count);
    ]

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Run All
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let () =
  run "Windtrap Demo"
    [
      passing_tests;
      failing_assertions;
      failing_exceptions;
      expect_tests;
      snapshot_tests;
      property_tests;
      skipped_tests;
      slow_tests;
      message_tests;
    ]
