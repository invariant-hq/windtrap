(* The payload-shape matrix under the real backend: every [%expect]
   spelling the PPX records must round-trip through the runtime's
   matcher without churn. Payloads are formatted exactly as a
   correction would write them, so a promote of any of these tests is
   a no-op — the transient promote-loop check relies on that. Two
   exceptions exercise matcher-accepted spellings that are not the
   writer's fixed point ({||} and bare [%expect]): they match and
   never correct on their own, but a correction elsewhere in this file
   standardizes them to {| |} on promote (ppx_expect's corrected-file
   style, pinned by the conformance corpus), so the byte-exact
   promote-loop check breaks a payload in inline_mixed.ml instead. *)

let%expect_test "multiline payload" =
  print_string "alpha\nbeta\ngamma\n";
  [%expect {|
    alpha
    beta
    gamma
    |}]

let%expect_test "relative indentation is preserved" =
  print_string "outer\n  inner\nouter\n";
  [%expect {|
    outer
      inner
    outer
    |}]

let%expect_test "empty literal payload" =
  print_string "";
  [%expect {| |}]

let%expect_test "empty braces literal" =
  print_string "";
  [%expect {||}]

let%expect_test "bare expect" =
  print_string "";
  [%expect]

let%expect_test "single-line payload" =
  print_string "one line";
  [%expect {| one line |}]

let%expect_test "quoted payload" =
  print_string "quoted";
  [%expect "quoted"]

let%expect_test "exact payload keeps whitespace" =
  Printf.printf "no trailing newline";
  [%expect_exact {|no trailing newline|}]

let%expect_test "output is consumed, not matched" =
  print_string "abcdef";
  let captured = [%expect.output] in
  Printf.printf "%d bytes\n" (String.length captured);
  [%expect {| 6 bytes |}]

let%expect_test "sanitize applies ambient config" =
  print_string "plain";
  [%expect {| plain |}]
