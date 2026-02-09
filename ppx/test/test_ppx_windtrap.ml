(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let%test ("basic_bool" [@tags "ppx"]) = ()
let%test _ = ()

module%test Tagged = struct
  let%test ("in_group" [@tags "ppx"]) = ()
end

let%expect_test "basic_output" =
  print_endline "Hello, world!";
  [%expect {|Hello, world!|}]

let%expect_test "multiple_expects" =
  print_endline "First";
  [%expect {|First|}];
  print_endline "Second";
  [%expect {|Second|}]

let%expect_test "empty_expect" = [%expect {||}]

let%expect_test "normalized_output" =
  print_endline "  Hello  ";
  [%expect {|Hello|}]

let%expect_test "multiline" =
  print_endline "Line 1";
  print_endline "Line 2";
  print_endline "Line 3";
  [%expect {|
    Line 1
    Line 2
    Line 3
  |}]

let%expect_test ("output_capture" [@tags "ppx"]) =
  print_string "captured: ";
  let out = [%expect.output] in
  print_endline (String.uppercase_ascii out ^ "done");
  [%expect {|CAPTURED: done|}]

let%expect_test _ =
  print_string "exact";
  [%expect_exact {|exact|}]
