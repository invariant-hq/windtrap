open Windtrap
module Calc = Windtrap_example_mutation.Calc

let addition =
  group "addition"
    [ test "adds" (fun () -> equal int 5 (Calc.apply Calc.Add 2 3)) ]

let subtraction =
  group "subtraction"
    [
      test "stays positive" (fun () -> is_true (Calc.apply Calc.Sub 10 4 > 0));
      test "subtracts" (fun () -> equal int 6 (Calc.apply Calc.Sub 10 4));
    ]

let multiplication =
  group "multiplication"
    [ test "multiplies" (fun () -> equal int 12 (Calc.apply Calc.Mul 3 4)) ]

let division =
  group "division"
    [
      test "divides" (fun () -> equal int 3 (Calc.apply Calc.Div 7 2));
      test "rejects a zero divisor" (fun () ->
          raises_match (Exn.invalid_arg ~substring:"division by zero")
            (fun () -> Calc.apply Calc.Div 1 0));
    ]

let sign =
  group "sign"
    [
      test "is 1 for a positive" (fun () -> equal int 1 (Calc.sign 5));
      test "is -1 for a negative" (fun () -> equal int (-1) (Calc.sign (-5)));
      test "is 0 for zero" (fun () -> equal int 0 (Calc.sign 0));
    ]

let abs =
  group "abs"
    [
      test "negates a negative" (fun () -> equal int 3 (Calc.abs (-3)));
      test "keeps zero" (fun () -> equal int 0 (Calc.abs 0));
    ]

let () =
  exit
    (run "calc" [ addition; subtraction; multiplication; division; sign; abs ])
