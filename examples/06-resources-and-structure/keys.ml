let normalize name = String.lowercase_ascii (String.trim name)

let valid name =
  let name = normalize name in
  name <> ""
  && String.for_all
       (function 'a' .. 'z' | '0' .. '9' | '_' -> true | _ -> false)
       name

let%test "a name is trimmed and lowercased" =
  Windtrap.(equal string "alice" (normalize "  Alice "))

module%test Valid = struct
  let%test "a name of letters is valid" = Windtrap.is_true (valid "Alice")
  let%test "a blank name is not" = Windtrap.is_false (valid "  ")
end
