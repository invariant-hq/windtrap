module%test Boom = struct
  let%test "inner" = ()
end
[@@expect.uncaught_exn {| (Failure boom) |}]
