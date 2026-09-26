(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The payload-shape matrix under the real backend: every [%expect]
   spelling the PPX accepts must round-trip through the core's matcher
   without churn. Payloads are formatted as a correction would write
   them, so a promote of any of these tests is a no-op. Two exercise
   matcher-accepted spellings that are not the writer's fixed point
   ({||} and bare [%expect]): they match, and a correction patches only
   the literal it corrects, so they are never rewritten by a correction
   elsewhere in the file. *)

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

let%expect_test "string-extension spelling" =
  print_string "shorthand";
  {%expect| shorthand |}

let%expect_test "exact payload keeps whitespace" =
  Printf.printf "no trailing newline";
  [%expect_exact {|no trailing newline|}]

let%expect_test "output is consumed, not matched" =
  print_string "abcdef";
  let captured = [%expect.output] in
  Printf.printf "%d bytes\n" (String.length captured);
  [%expect {| 6 bytes |}]

let%expect_test "several nodes consume in turn" =
  print_string "first";
  [%expect {| first |}];
  print_string "second";
  [%expect {| second |}]

(* A node inside a branch is an ordinary call: it runs when the branch
   does, and a body ending in a match needs no parentheses, since nothing
   is ever inserted after it. *)
let%expect_test "a node inside a match arm" =
  match Some 1 with
  | Some n ->
      Printf.printf "got %d\n" n;
      [%expect {| got 1 |}]
  | None -> print_string "none\n"
