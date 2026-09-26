(* A file that declares inline tests is test code, not the population
   under test: it passes through unchanged, with no preamble. Recognized
   here in its written spelling, which is what a driver that does not
   link ppx_windtrap sees. *)

let sum a b = a + b
let ordered a b = if a < b then 1 else 0
let%test "sums" = assert (sum 1 2 = 3)

module%test Grouped = struct
  let%test "orders" = assert (ordered 1 2 = 1)
end

let%expect_test "prints" =
  print_int (sum 1 2);
  [%expect {| 3 |}]
