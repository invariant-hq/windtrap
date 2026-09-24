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
        equal ~msg:"float_exact renders -inf" string "-inf"
          (s Pp.float_exact Float.neg_infinity);
        equal ~msg:"float_exact prints one third at 16 digits" string
          "0.3333333333333333"
          (s Pp.float_exact (1. /. 3.));
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
    test "abstract is the placeholder <abstract>" (fun () ->
        equal string "<abstract>" Pp.abstract);
    test "option puts no parentheses around its value" (fun () ->
        equal string "Some Some 1"
          (s (Pp.option (Pp.option Pp.int)) (Some (Some 1))));
    test "to_string breaks a long list at 78 columns" (fun () ->
        let items = List.init 30 (fun i -> 1000 + i) in
        let lines = String.split_on_char '\n' (s (Pp.list Pp.int) items) in
        is_true ~msg:"the list breaks" (List.length lines > 1);
        List.iter
          (fun line ->
            at_most ~msg:"no line passes the margin" int ~than:78
              (String.length line))
          lines;
        (* Every line but the last is full: one more element and its
           separator would pass the margin, so the break is at the margin
           and not before it. *)
        List.iter
          (fun line ->
            greater ~msg:"a broken line is full" int ~than:72
              (String.length line))
          (List.rev (List.tl (List.rev lines))));
    test "no printer writes to a standard channel" (fun () ->
        let b = Buffer.create 64 in
        let ppf = Format.formatter_of_buffer b in
        Pp.string ppf "s";
        Pp.int ppf 1;
        Pp.int32 ppf 1l;
        Pp.int64 ppf 1L;
        Pp.float_exact ppf 1.;
        Pp.bool ppf true;
        Pp.list Pp.int ppf [ 1; 2 ];
        Pp.array Pp.int ppf [| 1 |];
        Pp.option Pp.int ppf (Some 1);
        Pp.result ~ok:Pp.int ~error:Pp.string ppf (Error "e");
        Pp.pair Pp.int Pp.int ppf (1, 2);
        Pp.brackets Pp.int ppf 1;
        Pp.semi ppf ();
        Pp.pf ppf "%d" 1;
        Pp.flush ppf ();
        ignore (Pp.str "%d" 1 : string);
        ignore (Pp.to_string Pp.int 1 : string);
        Format.pp_print_flush Format.std_formatter ();
        Format.pp_print_flush Format.err_formatter ();
        flush stdout;
        flush stderr;
        equal ~msg:"nothing reached stdout or stderr" string "" (output ());
        is_true ~msg:"the buffer got the text" (Buffer.length b > 0));
  ]

let () = exit @@ Windtrap.run "pp" tests
