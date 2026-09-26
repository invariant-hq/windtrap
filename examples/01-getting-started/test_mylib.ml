open Windtrap

let add =
  group "add"
    [ test "adds two integers" (fun () -> equal int 5 (Calc.add 2 3)) ]

let parse =
  group "parse"
    [
      test "rejects the empty string" (fun () ->
          raises (Calc.Parse_error "empty") (fun () -> Calc.parse ""));
      prop "reads back any integer that string_of_int prints" Gen.int (fun n ->
          equal int n (Calc.parse (string_of_int n)));
    ]

let () = exit (run "mylib" [ add; parse ])
