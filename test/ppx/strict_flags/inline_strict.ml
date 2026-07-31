(* Compiled with -w +a -warn-error +a (see ./dune): every extension this
   PPX implements appears at least once, so the build fails if any
   generated code resolves a record field or constructor by
   type-directed disambiguation instead of a qualified path. *)

let%test "strict unit test" = assert (1 + 1 = 2)

module%test Strict_group = struct
  let answer = 42
  let%test "grouped unit test" = assert (41 + 1 = answer)
end

let%expect_test "tagged payload" =
  print_string "tagged";
  [%expect {| tagged |}]

let%expect_test "quoted payload" =
  print_string "quoted";
  [%expect "quoted"]

let%expect_test "exact, bare, and output" =
  print_string "exact";
  [%expect_exact {|exact|}];
  print_string "consumed";
  ignore [%expect.output];
  [%expect]
