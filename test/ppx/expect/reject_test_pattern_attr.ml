let%test ("n" [@expect.foo]) = ()

(* E18, pwt:87-89: a family attribute on a [let%test] name is refused. *)
