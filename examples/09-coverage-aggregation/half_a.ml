(* Half A of the shared library: string utilities. Exercised in full by
   test_a; test_b calls [greet] once, which links this whole module into
   its executable too — so test_b's inline line counts Half_a's points
   in its denominator while visiting almost none of them. *)

let greet = function "" -> "hello, stranger" | name -> "hello, " ^ name
let shout s = if s = "" then "!" else String.uppercase_ascii s ^ "!"

let parse_bool = function
  | "true" -> Ok true
  | "false" -> Ok false
  | s -> Error ("not a bool: " ^ s)
