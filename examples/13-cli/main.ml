(* Command-line options.

   Windtrap tests accept command-line options for filtering, output format, and
   more. Run with [--help] to see all options:

   {[dune exec ./examples/13-cli/main.exe -- --help]} *)

open Windtrap

let () =
  run "CLI Demo"
    [
      group "Unit tests"
        [
          test "fast test 1" (fun () -> equal Testable.int 1 1);
          test "fast test 2" (fun () -> equal Testable.int 2 2);
          test "fast test 3" (fun () -> equal Testable.int 3 3);
        ];
      group "Integration tests"
        [
          test "integration 1" (fun () ->
              print_endline "Running integration test 1...";
              is_true true);
          test "integration 2" (fun () ->
              print_endline "Running integration test 2...";
              is_true true);
        ];
      group "Slow tests"
        [
          slow "benchmark" (fun () ->
              print_endline "Running slow benchmark...";
              Unix.sleepf 0.1;
              is_true true);
          slow "stress test" (fun () ->
              print_endline "Running stress test...";
              Unix.sleepf 0.1;
              is_true true);
        ];
      group "Verbose output"
        [
          test "prints debug info" (fun () ->
              print_endline "Debug: starting test";
              print_endline "Debug: processing data";
              print_endline "Debug: test complete";
              is_true true);
        ];
    ]
