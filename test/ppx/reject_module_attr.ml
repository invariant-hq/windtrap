(* Fixture: an expect-family attribute on a module%test binding must be
   rejected (RFC compat mechanism (a)) — never silently dropped with the
   binding's other attributes. *)

module%test Boom = struct
  let%test "inner" = ()
end
[@@expect.uncaught_exn {| (Failure boom) |}]
