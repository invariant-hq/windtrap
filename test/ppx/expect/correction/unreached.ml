(* E36, pwt:51-55: a node that a run of its test never reaches fails the
   test when the body returns, located at the first such node, and the
   failure names the lines of the others. A node reached once passes, and
   each functor instance is a test of its own. *)

let%expect_test "a node behind a branch not taken" =
  print_string "x";
  [%expect {| x |}];
  if Sys.opaque_identity false then [%expect {| never |}]

let%expect_test "every node not reached is named" =
  if Sys.opaque_identity false then begin
    [%expect {| a |}];
    [%expect_exact {|b|}];
    [%expect {| c |}]
  end

let%expect_test "a node in a loop is reached once or more" =
  for _ = 1 to 2 do
    print_string "same";
    [%expect {| same |}]
  done

module Per_instance (B : sig
  val reach : bool
end) =
struct
  let%expect_test "a node is judged in each instance" =
    if B.reach then [%expect {| |}]
end

module _ = Per_instance (struct
  let reach = true
end)

module _ = Per_instance (struct
  let reach = false
end)

(* E33, etc:37-39: an override of run that never calls the body reaches
   none of its nodes. *)
module Never = struct
  module Expect_test_config = struct
    include Expect_test_config

    let run _ = ()
  end

  let%expect_test "an override that never calls the body fails" =
    print_string "printed";
    [%expect {| printed |}]
end
