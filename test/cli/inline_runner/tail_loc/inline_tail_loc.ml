open Windtrap

(* The assertion is two lines below the declaration, so the golden tells
   the declaration's line from the assertion's. *)
let%test "tail assertion" =
  let expected = 1 in
  equal int expected 2
