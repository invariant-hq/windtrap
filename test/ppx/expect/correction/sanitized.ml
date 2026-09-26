(* The sanitized text is the text compared, and the text a correction writes as
   the new baseline: the correction of the first node reads [pid NNNN]. Beside
   it, the second node matches and keeps its spelling. *)

module Expect_test_config = struct
  include Expect_test_config

  let sanitize = String.map (function '0' .. '9' -> 'N' | c -> c)
end

let%expect_test "a correction writes the sanitized text" =
  print_string "pid 4242";
  [%expect {| stale |}];
  print_string "same";
  [%expect "same"]
