(* Fixture: let%test and module%test registration, anonymous naming,
   [@tags] in both spellings, and nested groups. *)

let%test "addition" = assert (1 + 1 = 2)
let%test _ = ()
let%test ("tagged" [@tags "slow"]) = ()
let%test ("multi" [@tags "slow", "io"]) = ()

module%test Outer = struct
  let helper = 41
  let%test "inner" = assert (helper + 1 = 42)

  module%test Nested = struct
    let%test _ = ()
  end
end

module%test Tagged = struct
  let%test "in tagged group" = ()
end
[@@tags "group-tag"] [@@warning "-60"]
