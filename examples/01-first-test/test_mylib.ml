(* doc/manual/getting-started.md's first example, verbatim (the
   Parse_error exception lives in Calc, hence the open). *)

open Windtrap
open Calc

let () =
  run "mylib"
    [
      test "addition" (fun () -> equal int 5 (Calc.add 2 3));
      group "parser"
        [
          test "empty input" (fun () ->
              raises (Parse_error "empty") (fun () -> Calc.parse ""));
        ];
    ]
