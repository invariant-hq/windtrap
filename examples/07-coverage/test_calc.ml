open Windtrap
module Calc = Windtrap_example_coverage.Calc

let apply =
  group "apply"
    [
      test "adds" (fun () -> equal int 5 (Calc.apply Calc.Add 2 3));
      test "divides" (fun () -> equal int 3 (Calc.apply Calc.Div 7 2));
      test "rejects a zero divisor" (fun () ->
          raises_match (Exn.invalid_arg ~substring:"division by zero")
            (fun () -> Calc.apply Calc.Div 1 0));
    ]

let eval =
  group "eval"
    [
      test "folds the steps" (fun () ->
          equal int 3 (Calc.eval 1 [ (Calc.Add, 5); (Calc.Div, 2) ]));
    ]

let () = exit (run "calc" [ apply; eval ])
