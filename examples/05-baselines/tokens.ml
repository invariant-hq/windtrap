type token = Int of int | Plus | Eof

let tokenize input =
  let tokens =
    String.split_on_char ' ' input
    |> List.filter (fun s -> s <> "")
    |> List.map (function
      | "+" -> Plus
      | s -> (
          match int_of_string_opt s with
          | Some n -> Int n
          | None -> invalid_arg ("tokenize: " ^ s)))
  in
  tokens @ [ Eof ]

let print_tokens tokens =
  List.iter
    (function
      | Int n -> Printf.printf "INT %d\n" n
      | Plus -> print_endline "PLUS"
      | Eof -> print_endline "EOF")
    tokens

let%expect_test "a sum is two integers around a plus" =
  print_tokens (tokenize "1 + 2");
  [%expect {|
    INT 1
    PLUS
    INT 2
    EOF
    |}]

let%expect_test "repeated spaces are skipped" =
  print_tokens (tokenize "1   +  2");
  [%expect {|
    INT 1
    PLUS
    INT 2
    EOF
    |}]
