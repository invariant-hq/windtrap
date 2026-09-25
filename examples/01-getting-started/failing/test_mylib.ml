open Windtrap
open Calc

let () =
  exit
  @@ run "mylib"
       [
         test "addition" (fun () -> equal int 6 (Calc.add 2 3));
         group "parser"
           [
             test "empty input" (fun () ->
                 raises (Parse_error "empty") (fun () -> Calc.parse ""));
           ];
       ]
