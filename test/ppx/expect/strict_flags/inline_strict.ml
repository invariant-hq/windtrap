(* Compiled with -w +a -warn-error +a (see ./dune): each form below
   expands to generated code under the harshest user regime, so the build
   fails if any of it provokes a warning. *)

let%test "strict unit test" = assert (1 + 1 = 2)

module%test Strict_group = struct
  let answer = 42
  let%test "grouped unit test" = assert (41 + 1 = answer)
end

let%expect_test ("tagged payload" [@tags "strict"]) =
  print_string "tagged";
  [%expect {| tagged |}]

let%expect_test "quoted payload" =
  print_string "quoted";
  [%expect "quoted"]

(* The anonymous names and a nested group too. *)
let%test _ = assert (String.length "ok" = 2)

let%expect_test _ =
  print_string "anonymous";
  [%expect {| anonymous |}]

module%test Outer = struct
  module%test Inner = struct
    let%test "nested unit test" = assert (2 * 21 = 42)
  end
end

let%expect_test "exact, bare, and output" =
  print_string "exact";
  [%expect_exact {|exact|}];
  print_string "consumed";
  ignore [%expect.output];
  [%expect]
