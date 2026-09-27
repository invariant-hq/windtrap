(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Pp = Windtrap.Private.Pp

type printing = Prints : string * 'a Pp.t * 'a * string -> printing

let prints claim rows =
  cases claim
    ~name:(fun (Prints (name, _, _, _)) -> name)
    rows
    (fun (Prints (_, pp, v, printed)) ->
      equal string printed (Pp.to_string pp v))

(* Formatting *)

let into_a_buffer () =
  let b = Buffer.create 8 in
  let ppf = Format.formatter_of_buffer b in
  Pp.pf ppf "[%d]" 7;
  Pp.flush ppf ();
  equal string "[7]" (Buffer.contents b)

(* Every line but the last is full: one more element and its separator would
   pass the margin. *)
let breaks_at_the_margin () =
  let lines =
    String.split_on_char '\n'
      (Pp.to_string (Pp.list Pp.int) (List.init 30 (fun i -> 1000 + i)))
  in
  let lengths = List.map String.length lines in
  greater int ~than:1 (List.length lines);
  at_most int ~than:78 (List.fold_left max 0 lengths);
  greater int ~than:72 (List.fold_left min max_int (List.tl (List.rev lengths)))

let to_the_sink_only () =
  let b = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer b in
  Pp.string ppf "s";
  Pp.int ppf 1;
  Pp.int32 ppf 1l;
  Pp.int64 ppf 1L;
  Pp.decimal ppf 1.;
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
  equal string "" (output ());
  not_equal string "" (Buffer.contents b)

let formatting =
  group "Formatting"
    [
      test "str formats to a string as Format.asprintf does" (fun () ->
          equal string "a=1 b=two" (Pp.str "a=%d b=%s" 1 "two"));
      test "pf formats to its formatter, and flush flushes it" into_a_buffer;
      test "to_string breaks a long list at the 78 columns of asprintf"
        breaks_at_the_margin;
      test "no value writes to a standard channel" to_the_sink_only;
    ]

(* Printers *)

let reads_back f =
  equal int64 (Int64.bits_of_float f)
    (Int64.bits_of_float (float_of_string (Pp.to_string Pp.float_exact f)))

let printers =
  group "Printers"
    [
      test "abstract is <abstract>" (fun () ->
          equal string "<abstract>" Pp.abstract);
      prints "string formats a string verbatim"
        [
          Prints ("a word", Pp.string, "hello", "hello");
          Prints ("quotes and a newline", Pp.string, "\"a\"\n", "\"a\"\n");
        ];
      prints "int, int32 and int64 format in decimal, without a suffix"
        [
          Prints ("int", Pp.int, 42, "42");
          Prints ("a negative int", Pp.int, -7, "-7");
          Prints ("int32", Pp.int32, 5l, "5");
          Prints ("int64", Pp.int64, 9007199254740993L, "9007199254740993");
        ];
      prints
        "decimal formats the shortest decimal without an exponent that reads \
         back"
        [
          Prints ("a whole value, no point", Pp.decimal, 80., "80");
          Prints ("a fraction", Pp.decimal, 0.5, "0.5");
          Prints ("many places", Pp.decimal, 99.99999, "99.99999");
          Prints ("no exponent", Pp.decimal, 0.00001, "0.00001");
          Prints
            ( "at most 17 places, rounded below 0.1",
              Pp.decimal,
              1e-20,
              "0.00000000000000000" );
        ];
      prints "float_exact formats the shortest decimal that reads back"
        [
          Prints ("0.1 + 0.2", Pp.float_exact, 0.1 +. 0.2, "0.30000000000000004");
          Prints ("one third", Pp.float_exact, 1. /. 3., "0.3333333333333333");
        ];
      prints
        "float_exact keeps the point of a whole value, and an exponent gets \
         none"
        [
          Prints ("1.", Pp.float_exact, 1., "1.");
          Prints ("-3.", Pp.float_exact, -3., "-3.");
          Prints ("-0.", Pp.float_exact, -0., "-0.");
          Prints ("1e300", Pp.float_exact, 1e300, "1e+300");
        ];
      prints
        "float_exact formats the infinities and every NaN as the Stdlib values \
         that denote them"
        [
          Prints ("nan", Pp.float_exact, Float.nan, "nan");
          Prints ("the negated nan", Pp.float_exact, -.Float.nan, "nan");
          Prints
            ( "a signalling nan",
              Pp.float_exact,
              Int64.float_of_bits 0x7FF0000000000001L,
              "nan" );
          Prints ("infinity", Pp.float_exact, Float.infinity, "infinity");
          Prints
            ("neg_infinity", Pp.float_exact, Float.neg_infinity, "neg_infinity");
        ];
      prop "float_exact reads back to the same bits"
        ~examples:
          [
            0.1 +. 0.2;
            1e300;
            -1.0000111797990339e300;
            Float.pi;
            5e-324;
            Float.max_float;
            -0.;
          ]
        Gen.float reads_back;
      prints "bool formats true and false"
        [
          Prints ("true", Pp.bool, true, "true");
          Prints ("false", Pp.bool, false, "false");
        ];
    ]

(* Combinators *)

let bar ppf () = Pp.pf ppf "|"

let combinators =
  group "Combinators"
    [
      prints "list separates its elements with sep, semi by default"
        [
          Prints ("three elements", Pp.list Pp.int, [ 1; 2; 3 ], "1; 2; 3");
          Prints ("one element", Pp.list Pp.int, [ 9 ], "9");
          Prints ("no element", Pp.list Pp.int, [], "");
          Prints ("a caller's sep", Pp.list ~sep:bar Pp.int, [ 1; 2 ], "1|2");
        ];
      prints "array is list for arrays"
        [
          Prints ("two elements", Pp.array Pp.int, [| 1; 2 |], "1; 2");
          Prints ("a caller's sep", Pp.array ~sep:bar Pp.int, [| 1; 2 |], "1|2");
        ];
      prints "option formats None, and Some then its value unparenthesized"
        [
          Prints ("None", Pp.option Pp.int, None, "None");
          Prints ("Some", Pp.option Pp.int, Some 3, "Some 3");
          Prints
            ( "Some (Some 1)",
              Pp.option (Pp.option Pp.int),
              Some (Some 1),
              "Some Some 1" );
        ];
      prints "result formats Ok and Error then the value under its printer"
        [
          Prints ("Ok", Pp.result ~ok:Pp.int ~error:Pp.string, Ok 1, "Ok 1");
          Prints
            ( "Error",
              Pp.result ~ok:Pp.int ~error:Pp.string,
              Error "boom",
              "Error boom" );
        ];
      test "pair formats (x, y)" (fun () ->
          equal string "(1, x)"
            (Pp.to_string (Pp.pair Pp.int Pp.string) (1, "x")));
      test "brackets formats a value between [ and ]" (fun () ->
          equal string "[1; 2]"
            (Pp.to_string (Pp.brackets (Pp.list Pp.int)) [ 1; 2 ]));
    ]

let () = exit (run "pp" [ formatting; printers; combinators ])
