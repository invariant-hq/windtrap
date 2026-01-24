(** Output testing with expect.

    Test code that prints to stdout by capturing output and comparing it against
    expected values. Windtrap normalizes whitespace by default. *)

open Windtrap

let greet name = Printf.printf "Hello, %s!" name
let print_list items = List.iter (Printf.printf "- %s\n") items

let () =
  run "Expect"
    [
      group "Basic matching"
        [
          test "normalized whitespace" (fun () ->
              (* Trailing whitespace and extra blank lines are normalized. *)
              print_endline "  Hello  ";
              print_endline "";
              expect "Hello");
          test "simple output" (fun () ->
              greet "World";
              expect "Hello, World!");
        ];
      group "Exact matching"
        [
          test "exact preserves whitespace" (fun () ->
              print_string "no trailing newline";
              expect_exact "no trailing newline");
        ];
      group "Multiple expects"
        [
          test "sequential expects" (fun () ->
              (* Each expect clears the output buffer for the next check. *)
              print_string "First";
              expect "First";
              print_string "Second";
              expect "Second";
              print_string "Third";
              expect "Third");
        ];
      group "Capture with return value"
        [
          test "capture returns function result" (fun () ->
              let result =
                capture
                  (fun () ->
                    print_string "computing...";
                    42)
                  "computing..."
              in
              equal Testable.int 42 result);
        ];
      group "Raw output access"
        [
          test "output for post-processing" (fun () ->
              (* Use output() when you need to transform before comparing. *)
              Printf.printf "Time: %dms" 123;
              let out = output () in
              (* Mask variable parts *)
              let masked =
                String.map
                  (fun c -> if c >= '0' && c <= '9' then 'X' else c)
                  out
              in
              equal Testable.string "Time: XXXms" masked);
        ];
      group "Multiline output"
        [
          test "list output" (fun () ->
              print_list [ "apple"; "banana"; "cherry" ];
              expect "- apple\n- banana\n- cherry");
        ];
    ]
