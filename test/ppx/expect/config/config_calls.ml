(* E33, etc:37-39: an override of run must call f once. One that never calls
   it passes its test with nothing checked; one that calls it twice runs
   every expectation of the body twice. *)

module Never = struct
  module Expect_test_config = struct
    include Expect_test_config

    let run _ = ()
  end

  let%expect_test "an override that never calls the body checks nothing" =
    print_string "printed";
    [%expect {| never matches |}]
end

module Twice = struct
  let sanitized = ref 0

  module Expect_test_config = struct
    include Expect_test_config

    let run f =
      sanitized := 0;
      f ();
      f ();
      assert (!sanitized = 2)

    let sanitize s =
      incr sanitized;
      s
  end

  let%expect_test "an override that calls the body twice runs its node twice" =
    print_string "twice";
    [%expect {| twice |}]
end
