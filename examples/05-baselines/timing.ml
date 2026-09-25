module Expect_test_config = struct
  include Expect_test_config

  let sanitize = String.map (fun c -> if c >= '0' && c <= '9' then '#' else c)
end

let report_duration ms = Printf.printf "finished in %d ms\n" ms

let%expect_test "the duration is reported in milliseconds" =
  report_duration 37;
  [%expect {| finished in ## ms |}]
