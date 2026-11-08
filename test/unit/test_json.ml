(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Json = Windtrap.Private.Json

let strings () =
  equal string {|"q\"b\\s\n\t\r\u0001\u007fé"|}
    (Json.to_string (String "q\"b\\s\n\t\r\001\127\u{e9}"));
  (* A lone continuation byte, a truncated sequence and an overlong one. *)
  equal string {|"a�b�c��"|} (Json.to_string (String "a\x80b\xe2\x82c\xc0\xaf"));
  equal string {|{"�": 1}|} (Json.to_string (Object [ ("\xff", Int 1) ]))

let numbers () =
  equal string "[0, -7, 0.1, 100, 0.30000000000000004, 1e+300, -0, null, null]"
    (Json.to_string
       (List
          [
            Int 0;
            Int (-7);
            Float 0.1;
            Float 100.;
            Float (0.1 +. 0.2);
            Float 1e300;
            Float (-0.);
            Float Float.infinity;
            Float Float.nan;
          ]))

let layout () =
  equal string {|{"a": [true, false], "b": {}, "c": [], "a": "x"}|}
    (Json.to_string
       (Object
          [
            ("a", List [ Bool true; Bool false ]);
            ("b", Object []);
            ("c", List []);
            ("a", String "x");
          ]))

let () =
  exit
    (run "json"
       [
         test
           "a string escapes what JSON needs, and a malformed UTF-8 sequence \
            is U+FFFD"
           strings;
         test
           "a float is the shortest decimal that reads back, and null when not \
            finite"
           numbers;
         test "a value is written on one line, its fields in their order" layout;
       ])
