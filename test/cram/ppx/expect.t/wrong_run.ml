(* E28, pwt:29-32: the generated code applies Expect_test_config.run at the
   type [(unit -> unit) -> unit], so a run of another type is a type error
   located at the test that names it. *)

module Expect_test_config = struct
  let run (f : unit -> int) = ignore (f ())
  let sanitize s = s
end

let%expect_test "a run of another type" =
  print_string "x";
  [%expect {| x |}]
