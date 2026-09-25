let greet = function "" -> "hello, stranger" | name -> "hello, " ^ name
let shout s = if s = "" then "!" else String.uppercase_ascii s ^ "!"

let parse_bool = function
  | "true" -> Ok true
  | "false" -> Ok false
  | s -> Error ("not a bool: " ^ s)
