(* The coverage example's suite — deliberately partial: the Sub and Mul
   arms stay untested so an instrumented run has arms to report (see the
   dune file for the commands). *)

open Windtrap
module Calc = Windtrap_example_coverage.Calc

let () =
  run "calc"
    [
      test "addition" (fun () -> equal int 5 (Calc.apply Calc.Add 2 3));
      group "division"
        [
          test "divides" (fun () -> equal int 3 (Calc.apply Calc.Div 7 2));
          test "rejects zero" (fun () ->
              raises_match (Exn.invalid_arg ~substring:"division by zero")
                (fun () -> Calc.apply Calc.Div 1 0));
        ];
      test "eval folds the steps" (fun () ->
          equal int 3 (Calc.eval 1 [ (Calc.Add, 5); (Calc.Div, 2) ]));
    ]
