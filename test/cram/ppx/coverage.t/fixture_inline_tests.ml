(* A library's functions beside its inline tests, as written, where a test
   is an extension node, and as ppx_windtrap expands them before a build
   instruments them. Every test body holds a block, and so does the helper
   of the group; the functions alone carry points. *)

let sum a b = a + b
let ordered a b = if a < b then 1 else 0
let%test "sums" = if sum 1 2 = 3 then () else failwith "sum"

module%test Grouped = struct
  let twice x = x + x
  let%test "orders" = if ordered 1 (twice 1) > 0 then () else failwith "order"
end

let%expect_test "prints" =
  print_int (sum 1 2 - 1);
  [%expect {| 2 |}]
