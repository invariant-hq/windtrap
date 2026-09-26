(* A file whose every instrumented form is switched off allocates no point and
   is returned as parsed, with no generated module. *)

let sign n = if n > 0 then 1 else 0 [@@coverage off]
let arms = (function 0 -> "zero" | _ -> "other") [@coverage off]

module Off = struct
  let loop n =
    for _ = 1 to n do
      print_newline ()
    done
end
[@@coverage off]

[@@@coverage off]

let rest n = match n with 0 -> succ n | _ -> n
