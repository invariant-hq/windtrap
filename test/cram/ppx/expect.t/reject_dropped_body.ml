(* Under the cookie ["disabled"], the rest of a dropped body is not checked. *)

let%test "dropped" = ignore (1 [@expect.foo])
let kept = 1
