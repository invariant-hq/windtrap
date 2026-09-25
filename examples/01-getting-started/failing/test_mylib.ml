open Windtrap

let add =
  group "add"
    [ test "adds two integers" (fun () -> equal int 6 (Calc.add 2 3)) ]

let parse =
  group "parse"
    [
      test "rejects the empty string" (fun () ->
          raises (Calc.Parse_error "empty") (fun () -> Calc.parse ""));
    ]

let () = exit (run "mylib" [ add; parse ])
