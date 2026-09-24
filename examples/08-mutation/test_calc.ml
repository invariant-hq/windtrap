(* The mutation example's suite. One test is deliberately weak (a boolean
   that the a - b → a + b mutant also satisfies), and the exact test beside
   it kills that mutant, so the whole suite kills everything it reaches
   while a survey filtered to the weak test shows a survivor (the dune
   file has the commands). *)

open Windtrap
module Calc = Windtrap_example_mutation.Calc

let () =
  exit
  @@ run "calc"
       [
         test "addition" (fun () -> equal int 5 (Calc.apply Calc.Add 2 3));
         group "subtraction"
           [
             (* Weak on purpose: 10 - 4 and 10 + 4 are both positive, so the
                a - b → a + b mutant survives this test alone. *)
             test "stays positive" (fun () ->
                 is_true (Calc.apply Calc.Sub 10 4 > 0));
             (* The exact value kills it. *)
             test "subtracts" (fun () -> equal int 6 (Calc.apply Calc.Sub 10 4));
           ];
         test "multiplication" (fun () ->
             equal int 12 (Calc.apply Calc.Mul 3 4));
         group "division"
           [
             test "divides" (fun () -> equal int 3 (Calc.apply Calc.Div 7 2));
             test "rejects zero" (fun () ->
                 raises_match (Exn.invalid_arg ~substring:"division by zero")
                   (fun () -> Calc.apply Calc.Div 1 0));
           ];
         group "abs"
           [
             test "negates a negative" (fun () -> equal int 3 (Calc.abs (-3)));
             test "keeps zero" (fun () -> equal int 0 (Calc.abs 0));
           ];
       ]
