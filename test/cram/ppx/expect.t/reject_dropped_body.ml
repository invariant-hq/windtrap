(* E19, pwt:87-89: a family attribute inside a test body is refused by the
   check of the whole file. E30, pwt:60-61: under the cookie ["disabled"]
   the test is dropped, and the rest of its body is not checked: the rule
   cookie_disabled_body of ./dune expands this file to [kept] alone. *)

let%test "dropped" = ignore (1 [@expect.foo])
let kept = 1
