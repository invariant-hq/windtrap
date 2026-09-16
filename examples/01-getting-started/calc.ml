(* The library under test in the five-minutes example. *)

exception Parse_error of string

let add a b = a + b

let parse input =
  let trimmed = String.trim input in
  if trimmed = "" then raise (Parse_error "empty")
  else
    match int_of_string_opt trimmed with
    | Some n -> n
    | None -> raise (Parse_error ("not a number: " ^ trimmed))
