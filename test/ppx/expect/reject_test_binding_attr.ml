let%test "n" = () [@@expect.uncaught_exn {| |}]

(* E18, pwt:87-89: a family attribute on a [let%test] binding is
   refused. *)
