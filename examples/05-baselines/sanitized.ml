(* Shadowing [Expect_test_config] tunes expect tests for a whole file:
   here [sanitize] masks digits, so run-specific numbers never reach the
   comparison or a correction. This test passes only because the
   generated code references the ambient (shadowed) config. *)

module Expect_test_config = struct
  include Expect_test_config

  let sanitize = String.map (fun c -> if c >= '0' && c <= '9' then '#' else c)
end

let report_duration ms = Printf.printf "finished in %d ms\n" ms

let%expect_test "durations are masked" =
  report_duration 37;
  [%expect {| finished in ## ms |}]
