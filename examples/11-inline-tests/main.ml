(* Inline tests with PPX.

   Tests embedded directly in source files, run with [dune runtest]. Dune
   generates a test runner that discovers and executes all [let%expect_test]
   definitions. Mismatched output produces .corrected files for
   [dune promote].

   See the dune file for required configuration:
   [(inline_tests)] and [(preprocess (pps ppx_windtrap))]. *)

(* A function we want to test. *)
let greet name = Printf.printf "Hello, %s!\n" name
let add x y = x + y

let format_list items =
  List.iter (fun item -> Printf.printf "- %s\n" item) items

(* Basic expect test. *)
let%expect_test hello_world =
  print_endline "Hello, world!";
  [%expect {|Hello, world!|}]

(* Multiple expects in one test. *)
let%expect_test multiple_expects =
  print_endline "First line";
  [%expect {|First line|}];
  print_endline "Second line";
  [%expect {|Second line|}]

(* Testing functions. *)
let%expect_test testing_functions =
  Printf.printf "add 2 3 = %d\n" (add 2 3);
  [%expect {|add 2 3 = 5|}];
  Printf.printf "add 10 20 = %d\n" (add 10 20);
  [%expect {|add 10 20 = 30|}]

(* Whitespace is normalized by default. *)
let%expect_test normalized_whitespace =
  print_endline "  Hello  ";
  print_endline "";
  print_endline "  World  ";
  [%expect {|
    Hello

    World
  |}]

(* Use [%expect.exact] when whitespace matters. *)
let%expect_test exact_matching =
  print_string "no newline";
  [%expect.exact {|no newline|}]

(* Empty expect — assert no output. *)
let%expect_test no_output =
  let _ = add 1 2 in
  [%expect {||}]

(* Testing formatted output. *)
let%expect_test formatted_list =
  format_list [ "apples"; "bananas"; "cherries" ];
  [%expect {|
    - apples
    - bananas
    - cherries
  |}]

(* Capture output for post-processing. *)
let%expect_test capture_output =
  print_string "value: 42";
  let out = [%expect.output] in
  print_endline (String.uppercase_ascii out);
  [%expect {|VALUE: 42|}]

(* Chained capture and transformation. *)
let%expect_test chained_capture =
  greet "Alice";
  let greeting = [%expect.output] in
  Printf.printf "Got %d chars: %s" (String.length greeting)
    (String.trim greeting);
  [%expect {|Got 14 chars: Hello, Alice!|}]
