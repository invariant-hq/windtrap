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

(* Rules pinned here, by id in RULES.md and interface line: E2, pwt:23-25;
   E5, pwt:25-26; E8, pwt:23; E20, pwt:59-61; E21, pwt:63-66; E23, pwt:70-74;
   E24, pwt:70-71; E25, pwt:70-71; E35, pwt:76-83. *)
