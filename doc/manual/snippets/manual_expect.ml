(* Compiled mirrors of the expect-test snippets in
   doc/manual/snapshots-and-expect.md: a real (inline_tests) library driven
   by dune's runner, exactly as the manual's dune stanza shows. *)

type token = Int of int | Plus | Eof

let tokenize input =
  let tokens =
    String.split_on_char ' ' input
    |> List.filter (fun s -> s <> "")
    |> List.map (function "+" -> Plus | s -> Int (int_of_string s))
  in
  tokens @ [ Eof ]

let print_tokens tokens =
  List.iter
    (function
      | Int n -> Printf.printf "INT %d\n" n
      | Plus -> print_endline "PLUS"
      | Eof -> print_endline "EOF")
    tokens

let%expect_test "tokenize" =
  print_tokens (tokenize "1 + 2");
  [%expect {|
    INT 1
    PLUS
    INT 2
    EOF
  |}]

(* Shadowing Expect_test_config tunes expect tests for the rest of the
   file: here sanitize masks digits, so run-specific numbers never reach
   the comparison or a correction. *)

module Expect_test_config = struct
  include Expect_test_config

  let sanitize = String.map (fun c -> if c >= '0' && c <= '9' then '#' else c)
end

let report_duration ms = Printf.printf "finished in %d ms\n" ms

let%expect_test "durations are masked" =
  report_duration 37;
  [%expect {| finished in ## ms |}]
