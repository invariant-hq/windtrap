(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The witnesses under test are judged through the bool and the string they
   give, never through a witness of their own kind. *)

open Windtrap

type equality =
  | Equal : 'a Testable.t * 'a * 'a -> equality
  | Differ : 'a Testable.t * 'a * 'a -> equality

type printing = Prints : 'a Testable.t * 'a * string -> printing
type ordering = Orders : 'a Testable.t * 'a * 'a * string -> ordering

let equalities claim rows =
  cases claim ~name:fst rows (function
    | _, Equal (w, a, b) -> equal bool true (Testable.equal w a b)
    | _, Differ (w, a, b) -> equal bool false (Testable.equal w a b))

let printings claim rows =
  cases claim ~name:fst rows (function _, Prints (w, v, s) ->
      equal string s (Testable.to_string w v))

let sign c = if c < 0 then "below" else if c > 0 then "above" else "same"

(* [a] against [b], [b] against [a], then [a] against itself. *)
let order w a b =
  match Testable.compare w with
  | None -> "no order"
  | Some cmp -> String.concat ", " (List.map sign [ cmp a b; cmp b a; cmp a a ])

let orderings claim rows =
  cases claim ~name:fst rows (function _, Orders (w, a, b, row) ->
      equal string row (order w a b))

let ordered = "below, above, same"

(* Witnesses *)

let mod3 =
  Testable.make ~pp:Format.pp_print_int ~equal:(fun a b -> a mod 3 = b mod 3)

let physical =
  Testable.make ~pp:(fun ppf r -> Format.fprintf ppf "ref %d" !r) ~equal:( == )

let pp_ratio ppf (a, b) = Format.fprintf ppf "%d / %d" a b
let ratio = Testable.structural ~pp:pp_ratio

let caseless =
  Testable.of_equal (fun a b ->
      String.lowercase_ascii a = String.lowercase_ascii b)

let length = Testable.contramap String.length Testable.int
let reversed = Testable.with_compare (fun a b -> Int.compare b a) Testable.int
let one = ref 1

let made_equal =
  let open Testable in
  [
    ("make, 4 and 7 mod 3", Equal (mod3, 4, 7));
    ("make, 4 and 6 mod 3", Differ (mod3, 4, 6));
    ("make, a reference and itself", Equal (physical, one, one));
    ("make, two references to 0", Differ (physical, ref 0, ref 0));
    ( "with_compare keeps the equality",
      Equal (with_compare Int.compare mod3, 4, 7) );
    ("structural, equal pairs", Equal (ratio, (1, 2), (1, 2)));
    ("structural, unequal pairs", Differ (ratio, (1, 2), (1, 3)));
    ("of_equal, Hello and HELLO", Equal (caseless, "Hello", "HELLO"));
    ("of_equal, Hello and World", Differ (caseless, "Hello", "World"));
    ("contramap, foo and bar by length", Equal (length, "foo", "bar"));
    ("contramap, foo and quux by length", Differ (length, "foo", "quux"));
    ("pass, 1 and 2", Equal (pass, 1, 2));
    ( "pair string pass, equal first components",
      Equal (pair string pass, ("k", 1), ("k", 2)) );
    ( "pair string pass, unequal first components",
      Differ (pair string pass, ("k", 1), ("j", 1)) );
    ( "contramap in a container, equal images",
      Equal
        ( list (pair (contramap fst int) pass),
          [ ((1, 2), "x") ],
          [ ((1, 9), "y") ] ) );
    ( "contramap in a container, unequal images",
      Differ
        ( list (pair (contramap fst int) pass),
          [ ((1, 2), "x") ],
          [ ((3, 2), "x") ] ) );
  ]

let made_prints =
  let open Testable in
  [
    ("make", Prints (physical, ref 42, "ref 42"));
    ( "with_compare keeps the printer",
      Prints (with_compare Int.compare mod3, 42, "42") );
    ("structural", Prints (ratio, (-7, 2), "-7 / 2"));
    ("of_equal", Prints (caseless, "a", "<abstract>"));
    ("pass", Prints (pass, 42, "<pass>"));
    ("contramap, the image", Prints (length, "abc", "3"));
  ]

let made_orders =
  let open Testable in
  [
    ("make", Orders (mod3, 1, 2, "no order"));
    ("with_compare", Orders (with_compare Int.compare mod3, 4, 7, ordered));
    ("with_compare replaces an order", Orders (reversed, 2, 1, ordered));
    ("structural", Orders (ratio, (1, 2), (1, 3), ordered));
    ("of_equal", Orders (caseless, "a", "b", "no order"));
    ("contramap of an order", Orders (length, "ab", "abc", ordered));
    ( "contramap of no order",
      Orders (contramap (fun n -> [ n ]) (list int), 1, 2, "no order") );
    ("pass", Orders (pass, 1, 2, "no order"));
  ]

let expected_first () =
  let calls = ref [] in
  let note a b =
    calls := (a ^ " then " ^ b) :: !calls;
    true
  in
  let w = Testable.make ~pp:Format.pp_print_string ~equal:note in
  equal w "expected" "actual";
  ignore (Testable.equal w "expected" "actual" : bool);
  equal string "expected then actual; expected then actual"
    (String.concat "; " !calls)

let raising_equal =
  Testable.make ~pp:Format.pp_print_int ~equal:(fun _ _ -> raise Exit)

let raising_pp = Testable.make ~pp:(fun _ _ -> raise Exit) ~equal:Int.equal

let unread_halves () =
  equal (Testable.with_compare (fun _ _ -> raise Exit) Testable.int) 1 1;
  less (Testable.with_compare Int.compare raising_equal) ~than:2 1

let witnesses =
  group "Witnesses"
    [
      equalities "a witness compares with the equality it was made with"
        made_equal;
      printings "a witness prints with the printer it was made with" made_prints;
      orderings "a witness carries the order it was made with" made_orders;
      test "an equality verb applies the equality to the expected value first"
        expected_first;
      cases "an exception from the equality or the printer escapes the verb"
        ~name:fst
        [
          ("the equality", fun () -> equal raising_equal 1 1);
          ("the printer of a failing verb", fun () -> equal raising_pp 1 2);
        ]
        (fun (_, verb) -> raises Exit verb);
      test
        "the equality verbs never read the order, nor the ordering verbs the \
         equality"
        unread_halves;
    ]

(* Instances *)

let doc = "alpha\nbeta\n"

let instance_prints =
  let open Testable in
  [
    ("unit", Prints (unit, (), "()"));
    ("bool", Prints (bool, true, "true"));
    ("char", Prints (char, 'a', "'a'"));
    ("char, escaped", Prints (char, '\n', "'\\n'"));
    ("string, quoted", Prints (string, "hello", "\"hello\""));
    ("string, escaped", Prints (string, "a\nb", "\"a\\nb\""));
    ("string, on one line", Prints (string, doc, "\"alpha\\nbeta\\n\""));
    ("text", Prints (text, "hello", "hello"));
    ("text, newlines kept", Prints (text, doc, doc));
    ("text, the empty string", Prints (text, "", ""));
    ("bytes", Prints (bytes, Bytes.of_string "hi", "\"hi\""));
    ("int", Prints (int, 42, "42"));
    ("int, negative", Prints (int, -7, "-7"));
    ("int32", Prints (int32, 42l, "42"));
    ("int64", Prints (int64, 42L, "42"));
    ("nativeint", Prints (nativeint, 42n, "42"));
  ]

let instance_equal =
  let open Testable in
  [
    ("unit", Equal (unit, (), ()));
    ("bool, equal", Equal (bool, true, true));
    ("bool, unequal", Differ (bool, true, false));
    ("int, equal", Equal (int, 42, 42));
    ("int, unequal", Differ (int, 42, 43));
    ("int32", Equal (int32, 1l, 1l));
    ("int64", Equal (int64, 1L, 1L));
    ("nativeint, equal", Equal (nativeint, 42n, 42n));
    ("nativeint, unequal", Differ (nativeint, 0n, 1n));
    ("char, equal", Equal (char, 'a', 'a'));
    ("char, unequal", Differ (char, 'a', 'b'));
    ("string, equal", Equal (string, "hello", "hello"));
    ("string, unequal", Differ (string, "hello", "world"));
    ("text, equal", Equal (text, "a\nb", "a\nb"));
    ("text, unequal", Differ (text, "a\nb", "a\nc"));
    ("text, a trailing space", Differ (text, "a", "a "));
    ("text, a trailing newline", Differ (text, "a", "a\n"));
    ("bytes, equal", Equal (bytes, Bytes.of_string "a", Bytes.of_string "a"));
    ("bytes, unequal", Differ (bytes, Bytes.of_string "a", Bytes.of_string "b"));
  ]

let instance_orders =
  let open Testable in
  [
    ("unit", Orders (unit, (), (), "same, same, same"));
    ("bool", Orders (bool, false, true, ordered));
    ("char", Orders (char, 'a', 'b', ordered));
    ("string", Orders (string, "a", "b", ordered));
    ("text", Orders (text, "a", "b", ordered));
    ("bytes", Orders (bytes, Bytes.of_string "a", Bytes.of_string "b", ordered));
    ("int", Orders (int, 1, 2, ordered));
    ("int32", Orders (int32, 1l, 2l, ordered));
    ("int64", Orders (int64, 1L, 2L, ordered));
    ("nativeint", Orders (nativeint, 1n, 2n, ordered));
  ]

let instances =
  group "Instances"
    [
      printings "an instance prints its type's values" instance_prints;
      equalities "an instance compares with its type's equality" instance_equal;
      orderings "an instance carries its type's order" instance_orders;
    ]

(* Floats *)

let exact_rows =
  let open Testable in
  [
    ("1.5 and 1.5", Equal (float_exact, 1.5, 1.5));
    ("1. and the next float", Differ (float_exact, 1.0, Float.succ 1.0));
    ("0.3 and 0.1 +. 0.2", Differ (float_exact, 0.3, 0.1 +. 0.2));
    ("nan and nan", Equal (float_exact, Float.nan, Float.nan));
    ("nan and its negation", Equal (float_exact, Float.nan, -.Float.nan));
    ("nan and 1.", Differ (float_exact, Float.nan, 1.0));
    ("1. and nan", Differ (float_exact, 1.0, Float.nan));
    ("0. and -0.", Differ (float_exact, 0., -0.));
    ("-0. and -0.", Equal (float_exact, -0., -0.));
    ( "infinity and infinity",
      Equal (float_exact, Float.infinity, Float.infinity) );
    ( "neg_infinity and neg_infinity",
      Equal (float_exact, Float.neg_infinity, Float.neg_infinity) );
    ( "infinity and neg_infinity",
      Differ (float_exact, Float.infinity, Float.neg_infinity) );
    ( "infinity and max_float",
      Differ (float_exact, Float.infinity, Float.max_float) );
    ("a subnormal and itself", Equal (float_exact, 1e-310, 1e-310));
    ("a subnormal and the next", Differ (float_exact, 1e-310, Float.succ 1e-310));
  ]

(* The tolerant witnesses are made inside each test, where a mutant of the
   checks of [float] and [float_rel] is armed. *)
let eps_rows =
  [
    ("1.5 and 1.5 within 1e-9", (1e-9, 1.5, 1.5, true));
    ("1. and 1.005 within 0.01", (0.01, 1.0, 1.005, true));
    ("1. and 1.005 within 0.001", (0.001, 1.0, 1.005, false));
    ("1. and 1.5 within 0.5", (0.5, 1.0, 1.5, true));
    ("1. and past 1.5 within 0.5", (0.5, 1.0, Float.succ 1.5, false));
    ("nan and nan", (0.001, Float.nan, Float.nan, false));
    ("nan and nan within 1e10", (1e10, Float.nan, Float.nan, false));
    ("nan and 1.", (1.0, Float.nan, 1.0, false));
    ("1. and nan", (1.0, 1.0, Float.nan, false));
    ("0. and -0.", (0.001, 0., -0., true));
    ("infinity and infinity", (0.001, Float.infinity, Float.infinity, true));
    ( "infinity and neg_infinity",
      (1e300, Float.infinity, Float.neg_infinity, false) );
    ("infinity and max_float", (1e300, Float.infinity, Float.max_float, false));
  ]

let within_eps (_, (eps, a, b, holds)) =
  equal bool holds (Testable.equal (Testable.float eps) a b)

let rel_rows =
  [
    ("100. and 100.5, rel 0.01", ((0.01, 0.), 100.0, 100.5, true));
    ("100. and 100.5, rel 0.001", ((0.001, 0.), 100.0, 100.5, false));
    ("0. and 0.05, abs 0.1", ((0., 0.1), 0.0, 0.05, true));
    ("1. and 1.5, rel and abs 0.001", ((0.001, 0.001), 1.0, 1.5, false));
    ("1. and 1.105, rel 0.1 of the actual", ((0.1, 0.), 1.0, 1.105, true));
    ("1.105 and 1., rel 0.1 of the expected", ((0.1, 0.), 1.105, 1.0, true));
    ("1. and 2., rel 0.5", ((0.5, 0.), 1.0, 2.0, true));
    ("1. and past 2., rel 0.5", ((0.5, 0.), 1.0, Float.succ 2.0, false));
    ("nan and nan", ((0.001, 0.001), Float.nan, Float.nan, false));
    ("nan and nan, wide bounds", ((1.0, 1e10), Float.nan, Float.nan, false));
    ("nan and 1.", ((1.0, 1.0), Float.nan, 1.0, false));
    ("0. and -0.", ((0.001, 0.), 0., -0., true));
    ("infinity and infinity", ((0.01, 0.), Float.infinity, Float.infinity, true));
    ( "infinity and neg_infinity",
      ((1.0, 0.), Float.infinity, Float.neg_infinity, false) );
    ( "infinity and max_float",
      ((1.0, 0.), Float.infinity, Float.max_float, false) );
  ]

let within_rel (_, ((rel, abs), a, b, holds)) =
  equal bool holds (Testable.equal (Testable.float_rel ~rel ~abs) a b)

let float_prints =
  let open Testable in
  [
    ("float, 1.5", Prints (float 0.1, 1.5, "1.5"));
    ("float, a whole value", Prints (float 0.1, 1.0, "1"));
    ("float, nan", Prints (float 0.1, Float.nan, "nan"));
    ("float_rel", Prints (float_rel ~rel:0.1 ~abs:0.1, 2.5, "2.5"));
    ( "float_exact, 0.1 +. 0.2",
      Prints (float_exact, 0.1 +. 0.2, "0.30000000000000004") );
  ]

let float_orders =
  let open Testable in
  [
    ("float", Orders (float 0.5, 1.0, 1.2, ordered));
    ("float_rel", Orders (float_rel ~rel:0.5 ~abs:0.5, 1.0, 1.2, ordered));
    ("float_exact", Orders (float_exact, 1.0, 1.2, ordered));
    ( "float, nan below neg_infinity",
      Orders (float 0.5, Float.nan, Float.neg_infinity, ordered) );
  ]

let exact_apart (a, b) =
  not_equal string
    (Testable.to_string Testable.float_exact a)
    (Testable.to_string Testable.float_exact b)

let refuses substring make = raises_match (Exn.invalid_arg ~substring) make

let floats =
  group "Floats"
    [
      equalities
        "float_exact compares bit for bit, every nan equal to every nan"
        exact_rows;
      cases "float eps holds when a = b or |a -. b| <= eps, never on nan"
        ~name:fst eps_rows within_eps;
      cases
        "float_rel holds when a = b, within abs, or within rel of the larger \
         magnitude, never on nan"
        ~name:fst rel_rows within_rel;
      printings
        "a float witness prints with %g, float_exact the shortest decimal"
        float_prints;
      cases "float_exact prints two unequal floats apart" ~name:fst
        [ ("0.3 and 0.1 +. 0.2", (0.3, 0.1 +. 0.2)); ("0. and -0.", (0., -0.)) ]
        (fun (_, pair) -> exact_apart pair);
      prop "float_exact prints a float apart from the next one" Gen.float
        (fun x -> exact_apart (x, Float.succ x));
      orderings
        "the float witnesses order with Float.compare, whatever the tolerance"
        float_orders;
      cases "float refuses an eps that is not strictly positive" ~name:fst
        [ ("0.", 0.); ("-0.", -0.); ("-1e-9", -1e-9); ("nan", Float.nan) ]
        (fun (_, eps) -> refuses "float_exact" (fun () -> Testable.float eps));
      cases "float_rel refuses a negative or nan bound, and two zero bounds"
        ~name:fst
        [
          ("a negative rel", ("~rel", -0.1, 0.1));
          ("a negative abs", ("~abs", 0.1, -0.1));
          ("a nan rel", ("~rel", Float.nan, 0.1));
          ("a nan abs", ("~abs", 0.1, Float.nan));
          ("two zero bounds", ("float_exact", 0., 0.));
        ]
        (fun (_, (substring, rel, abs)) ->
          refuses substring (fun () -> Testable.float_rel ~rel ~abs));
    ]

(* Containers *)

let container_prints =
  let open Testable in
  [
    ("option, None", Prints (option int, None, "None"));
    ("option, Some", Prints (option int, Some 1, "Some 1"));
    ("result, Ok", Prints (result int string, Ok 1, "Ok 1"));
    ("result, Error", Prints (result int string, Error "x", "Error \"x\""));
    ("either, Left", Prints (either int string, Either.Left 1, "Left (1)"));
    ( "either, Right",
      Prints (either int string, Either.Right "h", "Right (\"h\")") );
    ("list", Prints (list int, [ 1; 2; 3 ], "[1; 2; 3]"));
    ("list, empty", Prints (list int, [], "[]"));
    ("array", Prints (array int, [| 1; 2 |], "[|1; 2|]"));
    ("array, empty", Prints (array int, [||], "[||]"));
    ("pair", Prints (pair int string, (1, "x"), "(1, \"x\")"));
    ("triple", Prints (triple int int int, (1, 2, 3), "(1, 2, 3)"));
    ("quad", Prints (quad int int int int, (1, 2, 3, 4), "(1, 2, 3, 4)"));
    ("slist, sorted", Prints (slist int Int.compare, [ 3; 1; 2 ], "[1; 2; 3]"));
    ( "slist, sorted by its comparison",
      Prints (slist int (fun a b -> Int.compare b a), [ 3; 1; 2 ], "[3; 2; 1]")
    );
  ]

let container_equal =
  let open Testable in
  let loose = float 0.1 in
  let sorted = slist int Int.compare in
  let int3 = triple int int int and int4 = quad int int int int in
  [
    ("option, Some and Some", Equal (option int, Some 1, Some 1));
    ("option, None and None", Equal (option int, None, None));
    ("option, Some and None", Differ (option int, Some 1, None));
    ("option, unequal payloads", Differ (option int, Some 1, Some 2));
    ( "option, payloads under their witness",
      Equal (option loose, Some 1.0, Some 1.05) );
    ("result, Ok and Ok", Equal (result int string, Ok 1, Ok 1));
    ("result, Error and Error", Equal (result int string, Error "e", Error "e"));
    ("result, Ok and Error", Differ (result int string, Ok 1, Error "e"));
    ("result, unequal Ok payloads", Differ (result int string, Ok 1, Ok 2));
    ( "either, Left and Left",
      Equal (either int string, Either.Left 1, Either.Left 1) );
    ( "either, Right and Right",
      Equal (either int string, Either.Right "h", Either.Right "h") );
    ( "either, Left and Right",
      Differ (either int int, Either.Left 1, Either.Right 1) );
    ("list, equal", Equal (list int, [ 1; 2; 3 ], [ 1; 2; 3 ]));
    ("list, empty", Equal (list int, [], []));
    ("list, shorter", Differ (list int, [ 1; 2 ], [ 1; 2; 3 ]));
    ("list, an unequal element", Differ (list int, [ 1; 2; 3 ], [ 1; 9; 3 ]));
    ("list, elements under their witness", Equal (list loose, [ 1.0 ], [ 1.05 ]));
    ("array, equal", Equal (array int, [| 1; 2 |], [| 1; 2 |]));
    ("array, shorter", Differ (array int, [| 1 |], [| 1; 2 |]));
    ("array, an unequal element", Differ (array int, [| 1; 2 |], [| 1; 3 |]));
    ("slist, another order", Equal (sorted, [ 3; 1; 2 ], [ 1; 2; 3 ]));
    ("slist, a missing element", Differ (sorted, [ 1; 2 ], [ 1; 2; 3 ]));
    ( "slist, duplicates in another order",
      Equal (sorted, [ 1; 1; 2 ], [ 1; 2; 1 ]) );
    ("slist, another multiplicity", Differ (sorted, [ 1; 1; 2 ], [ 1; 2; 2 ]));
    ("pair, equal", Equal (pair int string, (1, "a"), (1, "a")));
    ("pair, first unequal", Differ (pair int string, (1, "a"), (2, "a")));
    ("pair, second unequal", Differ (pair int string, (1, "a"), (1, "b")));
    ( "pair, components under their witnesses",
      Equal (pair loose int, (1.0, 2), (1.05, 2)) );
    ("triple, equal", Equal (int3, (1, 2, 3), (1, 2, 3)));
    ("triple, last unequal", Differ (int3, (1, 2, 3), (1, 2, 4)));
    ("quad, equal", Equal (int4, (1, 2, 3, 4), (1, 2, 3, 4)));
    ("quad, last unequal", Differ (int4, (1, 2, 3, 4), (1, 2, 3, 5)));
  ]

let container_orders =
  let open Testable in
  [
    ("option", Orders (option int, None, Some 1, "no order"));
    ("result", Orders (result int int, Ok 1, Error 1, "no order"));
    ( "either",
      Orders (either int int, Either.Left 1, Either.Right 1, "no order") );
    ("list", Orders (list int, [ 1 ], [ 2 ], "no order"));
    ("array", Orders (array int, [| 1 |], [| 2 |], "no order"));
    ("slist", Orders (slist int Int.compare, [ 1 ], [ 2 ], "no order"));
    ("pair", Orders (pair int int, (1, 1), (1, 2), "no order"));
    ("triple", Orders (triple int int int, (1, 1, 1), (1, 1, 2), "no order"));
    ( "quad",
      Orders (quad int int int int, (1, 1, 1, 1), (1, 1, 1, 2), "no order") );
  ]

let containers =
  group "Containers"
    [
      printings "a container prints its elements with their witnesses"
        container_prints;
      equalities "a container compares its elements with their witnesses"
        container_equal;
      orderings "no container carries an order, whatever its components carry"
        container_orders;
    ]

let () = exit (run "testable" [ witnesses; instances; floats; containers ])
