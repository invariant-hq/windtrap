(* M27, mut:105-106: a file that holds an extension node named [test], and
   none named [expect_test], declares inline tests and is returned as
   parsed. *)

let add a b = a + b
let%test "adds" = add 1 2 = 3
