(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Pp = Windtrap.Private.Pp

let s = Pp.to_string

let tests =
  [
    test "basic printers" (fun () ->
        equal ~msg:"string prints verbatim" string "hello" (s Pp.string "hello");
        equal ~msg:"int" string "42" (s Pp.int 42);
        equal ~msg:"negative int" string "-7" (s Pp.int (-7));
        equal ~msg:"int32" string "5" (s Pp.int32 5l);
        equal ~msg:"int64" string "9007199254740993"
          (s Pp.int64 9007199254740993L);
        (* [float_exact] is the only float printer here: what it renders, a
           reader may copy back and get the same double — a property
           counterexample pasted into [~examples], a bit-exact witness. A
           fixed-precision rendering would print 0.3 for this value. *)
        equal ~msg:"float_exact keeps the bits" string "0.30000000000000004"
          (s Pp.float_exact (0.1 +. 0.2));
        List.iter
          (fun f ->
            let printed = s Pp.float_exact f in
            is_true
              ~msg:(Printf.sprintf "%s round-trips" printed)
              (Int64.equal
                 (Int64.bits_of_float (float_of_string printed))
                 (Int64.bits_of_float f)))
          [
            0.1 +. 0.2;
            1e300;
            -1.0000111797990339e300;
            Float.pi;
            5e-324;
            Float.max_float;
            -0.;
          ];
        (* [%g] drops the point on a whole value, and ["1"] is an int
           literal: a counterexample exists to be pasted back, so the
           rendering has to stay syntactically a float. *)
        equal ~msg:"float_exact keeps whole values float-shaped" string "1."
          (s Pp.float_exact 1.);
        equal ~msg:"and negative whole values" string "-3."
          (s Pp.float_exact (-3.));
        equal ~msg:"exponent form needs no point" string "1e+300"
          (s Pp.float_exact 1e300);
        equal ~msg:"float_exact keeps the sign of zero" string "-0."
          (s Pp.float_exact (-0.));
        equal ~msg:"float_exact renders nan" string "nan"
          (s Pp.float_exact Float.nan);
        equal ~msg:"float_exact renders inf" string "inf"
          (s Pp.float_exact Float.infinity);
        equal ~msg:"bool" string "true" (s Pp.bool true));
    test "str and pf agree with to_string" (fun () ->
        equal ~msg:"str formats like sprintf" string "a=1 b=two"
          (Pp.str "a=%d b=%s" 1 "two");
        equal ~msg:"pf into a buffer formatter" string "[7]"
          (let b = Buffer.create 8 in
           let ppf = Format.formatter_of_buffer b in
           Pp.pf ppf "[%d]" 7;
           Pp.flush ppf ();
           Buffer.contents b));
    test "combinators" (fun () ->
        equal ~msg:"list with default semi separator" string "1; 2; 3"
          (s (Pp.list Pp.int) [ 1; 2; 3 ]);
        (* [?sep] is honored, not merely accepted: [Testable] passes an
           explicit separator for every container instance it builds. *)
        equal ~msg:"list honors a caller's separator" string "1|2"
          (s (Pp.list ~sep:(fun ppf () -> Pp.pf ppf "|") Pp.int) [ 1; 2 ]);
        equal ~msg:"singleton list has no separator" string "9"
          (s (Pp.list Pp.int) [ 9 ]);
        equal ~msg:"empty list is empty" string "" (s (Pp.list Pp.int) []);
        equal ~msg:"array matches list" string "1; 2"
          (s (Pp.array Pp.int) [| 1; 2 |]);
        equal ~msg:"option none" string "None" (s (Pp.option Pp.int) None);
        equal ~msg:"option some" string "Some 3" (s (Pp.option Pp.int) (Some 3));
        equal ~msg:"result ok" string "Ok 1"
          (s (Pp.result ~ok:Pp.int ~error:Pp.string) (Ok 1));
        equal ~msg:"result error" string "Error boom"
          (s (Pp.result ~ok:Pp.int ~error:Pp.string) (Error "boom"));
        equal ~msg:"pair" string "(1, x)"
          (s (Pp.pair Pp.int Pp.string) (1, "x"));
        equal ~msg:"brackets" string "[1; 2]"
          (s (Pp.brackets (Pp.list Pp.int)) [ 1; 2 ]));
    test "styled_string is the identity without ansi and wraps with it"
      (fun () ->
        equal ~msg:"styled_string ~ansi:false is the identity" string "plain"
          (Pp.styled_string ~ansi:false `Green "plain");
        equal ~msg:"styled_string ~ansi:true wraps" string "\027[32mok\027[0m"
          (Pp.styled_string ~ansi:true `Green "ok");
        (* Each style's code, on the surface renderers reach for: a style is
           picked by name at the call site, so a swapped code is a silently
           wrong color rather than a failure. *)
        equal ~msg:"red code" string "\027[31mhi\027[0m"
          (Pp.styled_string ~ansi:true `Red "hi");
        equal ~msg:"bold code" string "\027[1mb\027[0m"
          (Pp.styled_string ~ansi:true `Bold "b");
        (* Styling nothing is nothing: report lines are assembled from
           optional fragments, and an empty one must not leave an open code
           and its reset behind. *)
        equal ~msg:"styled_string ~ansi:true leaves the empty string bare"
          string ""
          (Pp.styled_string ~ansi:true `Faint ""));
  ]

let () = Windtrap.run "pp" tests
