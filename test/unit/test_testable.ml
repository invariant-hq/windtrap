(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Testable: instance printing and equality tables, tolerance
   semantics, and combinator composition. The witness sits below Check, so assertions on
   it go through booleans and string renderings, never through the witness
   under test. *)

open Windtrap
module T = Testable
module Pp = Windtrap.Private.Pp

let check_prints name witness value ~expected =
  equal ~msg:name string expected (T.to_string witness value)

let check_equal name witness a b = is_true ~msg:name (T.equal witness a b)
let check_differ name witness a b = is_false ~msg:name (T.equal witness a b)

(* Fixtures for the conventional [t]/[pp]/[equal] trio [make] consumes. *)

module Point = struct
  type t = { x : int; y : int }

  let pp ppf { x; y } = Format.fprintf ppf "(%d, %d)" x y
  let equal a b = a.x = b.x && a.y = b.y
end

(* A wider module: extra members beyond the trio are simply not read. *)
module Version = struct
  type t = int * int

  let make maj min = (maj, min)
  let pp ppf (maj, min) = Format.fprintf ppf "%d.%d" maj min
  let equal = ( = )
  let compare = Stdlib.compare
end

(* A module whose [equal] is coarser than structural equality: the witness
   must use the module's equality, not its representation. *)
module By_id = struct
  type t = { id : int; name : string }

  let pp ppf { id; name } = Format.fprintf ppf "#%d %s" id name
  let equal a b = a.id = b.id
end

(* A module whose [equal] is finer than structural equality — physical
   equality. The witness must pass it through untouched: no structural
   fallback, no comparison mediated by the printed form. *)
module Phys = struct
  type t = int ref

  let pp ppf r = Format.fprintf ppf "ref %d" !r
  let equal = ( == )
end

let point = T.make ~pp:Point.pp ~equal:Point.equal

(* [a] ranks strictly below [b] under the witness's order, which it must
   carry. *)
let ordered_or_fail name w a b =
  match T.compare w with
  | Some cmp ->
      is_true ~msg:(name ^ ": below") (cmp a b < 0);
      is_true ~msg:(name ^ ": above") (cmp b a > 0);
      is_true ~msg:(name ^ ": same") (cmp a a = 0)
  | None -> fail (name ^ ": carries no order")

let tests =
  [
    test "printing: base types" (fun () ->
        check_prints "prints unit" T.unit () ~expected:"()";
        check_prints "prints bool" T.bool true ~expected:"true";
        check_prints "prints char" T.char 'a' ~expected:"'a'";
        check_prints "prints escaped char" T.char '\n' ~expected:"'\\n'";
        check_prints "prints string quoted" T.string "hello"
          ~expected:"\"hello\"";
        check_prints "prints string escapes" T.string "a\nb"
          ~expected:"\"a\\nb\"";
        check_prints "prints text verbatim" T.text "hello" ~expected:"hello";
        check_prints "prints bytes quoted" T.bytes (Bytes.of_string "hi")
          ~expected:"\"hi\"";
        check_prints "prints int" T.int 42 ~expected:"42";
        check_prints "prints negative int" T.int (-7) ~expected:"-7";
        check_prints "prints int32" T.int32 42l ~expected:"42";
        check_prints "prints int64" T.int64 42L ~expected:"42";
        check_prints "prints nativeint" T.nativeint 42n ~expected:"42";
        check_prints "prints float with %g" (T.float 0.1) 1.5 ~expected:"1.5";
        check_prints "prints whole float compactly" (T.float 0.1) 1.0
          ~expected:"1";
        check_prints "prints nan" (T.float 0.1) Float.nan ~expected:"nan";
        check_prints "prints float_rel with %g"
          (T.float_rel ~rel:0.1 ~abs:0.1)
          2.5 ~expected:"2.5");
    test "printing: containers and combinators" (fun () ->
        check_prints "prints None" (T.option T.int) None ~expected:"None";
        check_prints "prints Some" (T.option T.int) (Some 1) ~expected:"Some 1";
        check_prints "prints Ok" (T.result T.int T.string) (Ok 1)
          ~expected:"Ok 1";
        check_prints "prints Error" (T.result T.int T.string) (Error "x")
          ~expected:"Error \"x\"";
        check_prints "prints Left" (T.either T.int T.string) (Either.Left 1)
          ~expected:"Left (1)";
        check_prints "prints Right" (T.either T.int T.string) (Either.Right "h")
          ~expected:"Right (\"h\")";
        check_prints "prints list" (T.list T.int) [ 1; 2; 3 ]
          ~expected:"[1; 2; 3]";
        check_prints "prints empty list" (T.list T.int) [] ~expected:"[]";
        check_prints "prints array" (T.array T.int) [| 1; 2 |]
          ~expected:"[|1; 2|]";
        check_prints "prints empty array" (T.array T.int) [||] ~expected:"[||]";
        check_prints "prints pair" (T.pair T.int T.string) (1, "x")
          ~expected:"(1, \"x\")";
        check_prints "prints triple"
          (T.triple T.int T.int T.int)
          (1, 2, 3) ~expected:"(1, 2, 3)";
        check_prints "prints quad"
          (T.quad T.int T.int T.int T.int)
          (1, 2, 3, 4) ~expected:"(1, 2, 3, 4)";
        (* Failures print the sides in the sorted order the equality
           compared: the diff shows the multiset difference, never
           the incidental arrival order. *)
        check_prints "slist prints the sorted sides the equality compared"
          (T.slist T.int Int.compare)
          [ 3; 1; 2 ] ~expected:"[1; 2; 3]";
        check_prints "slist sorts with the given comparison"
          (T.slist T.int (fun a b -> Int.compare b a))
          [ 3; 1; 2 ] ~expected:"[3; 2; 1]";
        check_prints "pass prints <pass>" T.pass 42 ~expected:"<pass>";
        check_prints "of_equal prints <abstract>" (T.of_equal Int.equal) 42
          ~expected:"<abstract>";
        check_prints "contramap prints the image, not the original"
          (T.contramap String.length T.int)
          "abc" ~expected:"3");
    (* [text] exists for one reason: its rendering keeps the newlines, and a
       rendering that spans lines is exactly what sends the report down the
       unified-diff path instead of marking spans in an escaped one-liner.
       Pin that property here, at the witness, so the two ends of the
       contract cannot drift apart. *)
    test "text: renders verbatim, where string escapes" (fun () ->
        let doc = "alpha\nbeta\n" in
        check_prints "keeps newlines" T.text doc ~expected:"alpha\nbeta\n";
        is_true ~msg:"the rendering spans lines"
          (String.contains (T.to_string T.text doc) '\n');
        check_prints "string collapses the same value to one escaped line"
          T.string doc ~expected:"\"alpha\\nbeta\\n\"";
        is_false ~msg:"string's rendering never spans lines"
          (String.contains (T.to_string T.string doc) '\n');
        check_prints "no quotes around the empty value" T.text "" ~expected:"";
        (* Byte equality, like [string]: the differences [text] renders
           without escapes are still differences it reports. *)
        check_differ "trailing space is a difference" T.text "a" "a ";
        check_differ "trailing newline is a difference" T.text "a" "a\n");
    test "equality: base types" (fun () ->
        check_equal "unit equal" T.unit () ();
        check_equal "bool equal" T.bool true true;
        check_differ "bool differs" T.bool true false;
        check_equal "int equal" T.int 42 42;
        check_differ "int differs" T.int 42 43;
        check_equal "int32 equal" T.int32 1l 1l;
        check_equal "int64 equal" T.int64 1L 1L;
        check_equal "nativeint equal" T.nativeint 42n 42n;
        check_differ "nativeint differs" T.nativeint 0n 1n;
        check_equal "char equal" T.char 'a' 'a';
        check_differ "char differs" T.char 'a' 'b';
        check_equal "string equal" T.string "hello" "hello";
        check_differ "string differs" T.string "hello" "world";
        check_equal "text equal" T.text "a\nb" "a\nb";
        check_differ "text differs" T.text "a\nb" "a\nc";
        check_equal "bytes equal" T.bytes (Bytes.of_string "a")
          (Bytes.of_string "a");
        check_differ "bytes differ" T.bytes (Bytes.of_string "a")
          (Bytes.of_string "b"));
    test "equality: option, result, either" (fun () ->
        check_equal "option: Some equals Some" (T.option T.int) (Some 1)
          (Some 1);
        check_equal "option: None equals None" (T.option T.int) None None;
        check_differ "option: Some differs from None" (T.option T.int) (Some 1)
          None;
        check_differ "option: differing payloads" (T.option T.int) (Some 1)
          (Some 2);
        check_equal "option: payload witness is used"
          (T.option (T.float 0.1))
          (Some 1.0) (Some 1.05);
        check_equal "result: Ok equals Ok" (T.result T.int T.string) (Ok 1)
          (Ok 1);
        check_equal "result: Error equals Error" (T.result T.int T.string)
          (Error "e") (Error "e");
        check_differ "result: Ok differs from Error" (T.result T.int T.string)
          (Ok 1) (Error "e");
        check_differ "result: differing Ok payloads" (T.result T.int T.string)
          (Ok 1) (Ok 2);
        check_equal "either: Left equals Left" (T.either T.int T.string)
          (Either.Left 1) (Either.Left 1);
        check_equal "either: Right equals Right" (T.either T.int T.string)
          (Either.Right "h") (Either.Right "h");
        check_differ "either: Left differs from Right" (T.either T.int T.int)
          (Either.Left 1) (Either.Right 1));
    test "equality: lists, arrays, slist" (fun () ->
        check_equal "list: equal" (T.list T.int) [ 1; 2; 3 ] [ 1; 2; 3 ];
        check_equal "list: empty" (T.list T.int) [] [];
        check_differ "list: different lengths" (T.list T.int) [ 1; 2 ]
          [ 1; 2; 3 ];
        check_differ "list: different element" (T.list T.int) [ 1; 2; 3 ]
          [ 1; 9; 3 ];
        check_equal "list: element witness is used"
          (T.list (T.float 0.1))
          [ 1.0 ] [ 1.05 ];
        check_equal "array: equal" (T.array T.int) [| 1; 2 |] [| 1; 2 |];
        check_differ "array: different lengths" (T.array T.int) [| 1 |]
          [| 1; 2 |];
        check_differ "array: different element" (T.array T.int) [| 1; 2 |]
          [| 1; 3 |];
        check_equal "slist: ignores order"
          (T.slist T.int Int.compare)
          [ 3; 1; 2 ] [ 1; 2; 3 ];
        check_differ "slist: detects missing elements"
          (T.slist T.int Int.compare)
          [ 1; 2 ] [ 1; 2; 3 ];
        check_equal "slist: duplicates as multiset"
          (T.slist T.int Int.compare)
          [ 1; 1; 2 ] [ 1; 2; 1 ];
        check_differ "slist: multiplicity matters"
          (T.slist T.int Int.compare)
          [ 1; 1; 2 ] [ 1; 2; 2 ]);
    test "equality: tuples" (fun () ->
        check_equal "pair: componentwise" (T.pair T.int T.string) (1, "a")
          (1, "a");
        check_differ "pair: first differs" (T.pair T.int T.string) (1, "a")
          (2, "a");
        check_differ "pair: second differs" (T.pair T.int T.string) (1, "a")
          (1, "b");
        check_equal "pair: component witnesses are used"
          (T.pair (T.float 0.1) T.int)
          (1.0, 2) (1.05, 2);
        check_equal "triple: componentwise"
          (T.triple T.int T.int T.int)
          (1, 2, 3) (1, 2, 3);
        check_differ "triple: last differs"
          (T.triple T.int T.int T.int)
          (1, 2, 3) (1, 2, 4);
        check_equal "quad: componentwise"
          (T.quad T.int T.int T.int T.int)
          (1, 2, 3, 4) (1, 2, 3, 4);
        check_differ "quad: last differs"
          (T.quad T.int T.int T.int T.int)
          (1, 2, 3, 4) (1, 2, 3, 5));
    test "constructors and combinators" (fun () ->
        let mod3 =
          T.make ~pp:Format.pp_print_int ~equal:(fun a b -> a mod 3 = b mod 3)
        in
        check_equal "make: custom equality is used" mod3 4 7;
        check_differ "make: custom equality can reject" mod3 4 6;
        let s = T.structural ~pp:(Pp.list Pp.int) in
        check_equal "structural: polymorphic equality" s [ 1; 2 ] [ 1; 2 ];
        check_differ "structural: rejects structural difference" s [ 1 ] [ 2 ];
        let ci =
          T.of_equal (fun a b ->
              String.lowercase_ascii a = String.lowercase_ascii b)
        in
        check_equal "of_equal: custom equality" ci "Hello" "HELLO";
        check_differ "of_equal: rejects" ci "Hello" "World";
        let by_length = T.contramap String.length T.int in
        check_equal "contramap: compares through the map" by_length "foo" "bar";
        check_differ "contramap: detects differences after the map" by_length
          "foo" "quux";
        check_equal "pass: everything is equal" T.pass 1 2;
        check_equal "pass: composes as an ignored component"
          (T.pair T.string T.pass) ("k", 1) ("k", 2);
        check_differ "pass: other components still compared"
          (T.pair T.string T.pass) ("k", 1) ("j", 1);
        check_equal "composition: contramap inside a container"
          (T.list (T.pair (T.contramap fst T.int) T.pass))
          [ ((1, 2), "x") ]
          [ ((1, 9), "y") ];
        check_differ "composition: mapped component still compared"
          (T.list (T.pair (T.contramap fst T.int) T.pass))
          [ ((1, 2), "x") ]
          [ ((3, 2), "x") ]);
    test "float_exact: equality" (fun () ->
        check_equal "identical floats are equal" T.float_exact 1.5 1.5;
        check_differ "adjacent floats differ" T.float_exact 1.0 (Float.succ 1.0);
        check_differ "no tolerance at all" T.float_exact 0.3 (0.1 +. 0.2);
        check_equal "NaN equals NaN" T.float_exact Float.nan Float.nan;
        check_equal "NaN equals sign-flipped NaN" T.float_exact Float.nan
          (-.Float.nan);
        check_differ "NaN differs from a number" T.float_exact Float.nan 1.0;
        check_differ "a number differs from NaN" T.float_exact 1.0 Float.nan;
        check_differ "positive and negative zero differ" T.float_exact 0. (-0.);
        check_equal "negative zero equals itself" T.float_exact (-0.) (-0.);
        check_equal "equal infinities" T.float_exact Float.infinity
          Float.infinity;
        check_equal "equal negative infinities" T.float_exact Float.neg_infinity
          Float.neg_infinity;
        check_differ "opposite infinities differ" T.float_exact Float.infinity
          Float.neg_infinity;
        check_differ "infinity differs from max_float" T.float_exact
          Float.infinity Float.max_float;
        check_equal "subnormals compare exactly" T.float_exact 1e-310 1e-310;
        check_differ "distinct subnormals differ" T.float_exact 1e-310
          (Float.succ 1e-310));
    test "float_exact: printing" (fun () ->
        check_prints "prints short decimals plainly" T.float_exact 1.5
          ~expected:"1.5";
        check_prints "keeps the sign of negatives" T.float_exact (-1.5)
          ~expected:"-1.5";
        (* Whole values keep their point: ["1"] is an int literal, and this
           witness renders values a reader may paste back. *)
        check_prints "prints whole floats as floats" T.float_exact 1.0
          ~expected:"1.";
        check_prints "prints 0.1 as written" T.float_exact 0.1 ~expected:"0.1";
        check_prints "prints positive zero" T.float_exact 0. ~expected:"0.";
        check_prints "prints negative zero with its sign" T.float_exact (-0.)
          ~expected:"-0.";
        check_prints "prints nan" T.float_exact Float.nan ~expected:"nan";
        check_prints "prints inf" T.float_exact Float.infinity ~expected:"inf";
        check_prints "prints -inf" T.float_exact Float.neg_infinity
          ~expected:"-inf";
        check_prints "exposes accumulated error" T.float_exact (0.1 +. 0.2)
          ~expected:"0.30000000000000004";
        check_prints "prints one third at 16 digits" T.float_exact (1. /. 3.)
          ~expected:"0.3333333333333333");
    test "float_exact: printing round-trips exact bits" (fun () ->
        (* Unequal floats never render identically: the printed decimal
           restores the exact bits, so bit-distinct values get distinct
           renderings. *)
        let round_trips v =
          let s = T.to_string T.float_exact v in
          Int64.equal
            (Int64.bits_of_float (float_of_string s))
            (Int64.bits_of_float v)
        in
        List.iter
          (fun (name, v) ->
            is_true ~msg:("float_exact round-trips " ^ name) (round_trips v))
          [
            ("0.1 +. 0.2", 0.1 +. 0.2);
            ("one third", 1. /. 3.);
            ("pi", Float.pi);
            ("max_float", Float.max_float);
            ("min_float", Float.min_float);
            ("epsilon", Float.epsilon);
            ("a subnormal", 1e-310);
            ("the smallest subnormal", Float.succ 0.);
            ("succ 1.0", Float.succ 1.0);
            ("negative zero", -0.);
            ("large integer", 9007199254740993.);
          ];
        not_equal ~msg:"renders 0.3 and 0.1 +. 0.2 differently" string
          (T.to_string T.float_exact 0.3)
          (T.to_string T.float_exact (0.1 +. 0.2));
        not_equal ~msg:"renders the zeros differently" string
          (T.to_string T.float_exact 0.)
          (T.to_string T.float_exact (-0.)));
    test "float: tolerance and IEEE default semantics" (fun () ->
        check_equal "equal values short-circuit the tolerance" (T.float 1e-9)
          1.5 1.5;
        check_equal "within epsilon" (T.float 0.01) 1.0 1.005;
        check_differ "outside epsilon" (T.float 0.001) 1.0 1.005;
        check_differ "NaN differs from NaN by default (use float_exact)"
          (T.float 0.001) Float.nan Float.nan;
        check_differ "NaN differs from NaN under a wide tolerance"
          (T.float 1e10) Float.nan Float.nan;
        check_differ "NaN differs from a number" (T.float 1.0) Float.nan 1.0;
        check_differ "a number differs from NaN" (T.float 1.0) 1.0 Float.nan;
        check_equal "signed zeros are equal" (T.float 0.001) 0. (-0.);
        check_equal "equal infinities" (T.float 0.001) Float.infinity
          Float.infinity;
        check_differ "opposite infinities differ" (T.float 1e300) Float.infinity
          Float.neg_infinity;
        check_differ "infinity differs from a finite value" (T.float 1e300)
          Float.infinity Float.max_float);
    test "float_rel: tolerance and IEEE default semantics" (fun () ->
        check_equal "relative tolerance"
          (T.float_rel ~rel:0.01 ~abs:0.0)
          100.0 100.5;
        check_differ "outside relative tolerance"
          (T.float_rel ~rel:0.001 ~abs:0.0)
          100.0 100.5;
        check_equal "absolute tolerance near zero"
          (T.float_rel ~rel:0.0 ~abs:0.1)
          0.0 0.05;
        check_differ "outside both tolerances"
          (T.float_rel ~rel:0.001 ~abs:0.001)
          1.0 1.5;
        check_differ "NaN differs from NaN by default (use float_exact)"
          (T.float_rel ~rel:0.001 ~abs:0.001)
          Float.nan Float.nan;
        check_differ "NaN differs from NaN under wide tolerances"
          (T.float_rel ~rel:1.0 ~abs:1e10)
          Float.nan Float.nan;
        check_differ "NaN differs from a number"
          (T.float_rel ~rel:1.0 ~abs:1.0)
          Float.nan 1.0;
        check_equal "signed zeros are equal"
          (T.float_rel ~rel:0.001 ~abs:0.0)
          0. (-0.);
        check_equal "equal infinities"
          (T.float_rel ~rel:0.01 ~abs:0.0)
          Float.infinity Float.infinity;
        check_differ "opposite infinities differ"
          (T.float_rel ~rel:1.0 ~abs:0.0)
          Float.infinity Float.neg_infinity;
        check_differ "infinity differs from a finite value"
          (T.float_rel ~rel:1.0 ~abs:0.0)
          Float.infinity Float.max_float);
    test "float: a non-positive eps is rejected" (fun () ->
        (* [float 0.] — and any eps below it — is exact equality wearing a
           tolerance's syntax; the guard names the honest spelling. *)
        let rejects msg fn =
          raises_match ~msg (Exn.invalid_arg ~substring:"float_exact") fn
        in
        rejects "zero eps" (fun () -> T.float 0.);
        rejects "negative zero eps" (fun () -> T.float (-0.));
        rejects "negative eps" (fun () -> T.float (-1e-9));
        rejects "NaN eps" (fun () -> T.float Float.nan));
    test "float_rel: degenerate bounds are rejected" (fun () ->
        raises_match ~msg:"negative rel" (Exn.invalid_arg ~substring:"~rel")
          (fun () -> T.float_rel ~rel:(-0.1) ~abs:0.1);
        raises_match ~msg:"negative abs" (Exn.invalid_arg ~substring:"~abs")
          (fun () -> T.float_rel ~rel:0.1 ~abs:(-0.1));
        raises_match ~msg:"NaN rel" (Exn.invalid_arg ~substring:"~rel")
          (fun () -> T.float_rel ~rel:Float.nan ~abs:0.1);
        raises_match ~msg:"NaN abs" (Exn.invalid_arg ~substring:"~abs")
          (fun () -> T.float_rel ~rel:0.1 ~abs:Float.nan);
        raises_match ~msg:"both bounds zero"
          (Exn.invalid_arg ~substring:"float_exact") (fun () ->
            T.float_rel ~rel:0. ~abs:0.);
        (* One zero bound stays legal: it switches a component off while the
           other remains a real tolerance. *)
        check_equal "pure relative still constructs"
          (T.float_rel ~rel:0.01 ~abs:0.)
          100.0 100.5;
        check_equal "pure absolute still constructs"
          (T.float_rel ~rel:0. ~abs:0.1)
          0.0 0.05);
    test "make: a module's trio" (fun () ->
        check_equal "equal per the module's equal" point { Point.x = 1; y = 2 }
          { Point.x = 1; y = 2 };
        check_differ "differ per the module's equal" point
          { Point.x = 1; y = 2 } { Point.x = 1; y = 3 };
        check_prints "prints with the module's pp" point { Point.x = 1; y = 2 }
          ~expected:"(1, 2)";
        check_equal "members beyond the trio are not read"
          (T.make ~pp:Version.pp ~equal:Version.equal)
          (Version.make 1 2) (1, 2);
        check_prints "wider module prints with its pp"
          (T.make ~pp:Version.pp ~equal:Version.equal)
          (3, 14) ~expected:"3.14";
        check_equal "the module's equal wins over structure"
          (T.make ~pp:By_id.pp ~equal:By_id.equal)
          { By_id.id = 1; name = "a" }
          { By_id.id = 1; name = "b" };
        check_differ "the module's equal still distinguishes"
          (T.make ~pp:By_id.pp ~equal:By_id.equal)
          { By_id.id = 1; name = "a" }
          { By_id.id = 2; name = "a" });
    test "make: physical equality passes through" (fun () ->
        let phys = T.make ~pp:Phys.pp ~equal:Phys.equal in
        let r = ref 0 in
        check_equal "physical equality holds on the same value" phys r r;
        check_differ "physical equality distinguishes structural twins" phys
          (ref 0) (ref 0);
        check_prints "printing is independent of the equality" phys (ref 42)
          ~expected:"ref 42");
    test "order: instances carry their module's, containers none" (fun () ->
        let ordered name w a b =
          match T.compare w with
          | Some cmp ->
              is_true ~msg:(name ^ ": below") (cmp a b < 0);
              is_true ~msg:(name ^ ": above") (cmp b a > 0);
              is_true ~msg:(name ^ ": same") (cmp a a = 0)
          | None -> fail (name ^ ": carries no order")
        in
        let unordered name w =
          is_none ~msg:(name ^ ": no order") (T.compare w)
        in
        ordered "int" T.int 1 2;
        ordered "int32" T.int32 1l 2l;
        ordered "int64" T.int64 1L 2L;
        ordered "nativeint" T.nativeint 1n 2n;
        ordered "char" T.char 'a' 'b';
        ordered "string" T.string "a" "b";
        ordered "text" T.text "a" "b";
        ordered "bytes" T.bytes (Bytes.of_string "a") (Bytes.of_string "b");
        ordered "bool" T.bool false true;
        (* Tolerance belongs to equality: every float witness orders exactly,
           and NaN sorts first, as [Float.compare] has it. *)
        ordered "float" (T.float 0.5) 1.0 1.2;
        ordered "float_rel" (T.float_rel ~rel:0.5 ~abs:0.5) 1.0 1.2;
        ordered "float_exact" T.float_exact 1.0 1.2;
        ordered "float: nan sorts below -inf" (T.float 0.5) Float.nan
          neg_infinity;
        is_true ~msg:"unit: one value, ranked the same"
          (match T.compare T.unit with
          | Some cmp -> cmp () () = 0
          | None -> false);
        unordered "option" (T.option T.int);
        unordered "result" (T.result T.int T.int);
        unordered "either" (T.either T.int T.int);
        unordered "list" (T.list T.int);
        unordered "array" (T.array T.int);
        unordered "slist" (T.slist T.int Int.compare);
        unordered "pair" (T.pair T.int T.int);
        unordered "triple" (T.triple T.int T.int T.int);
        unordered "quad" (T.quad T.int T.int T.int T.int);
        unordered "pass" T.pass;
        unordered "of_equal" (T.of_equal Int.equal);
        unordered "make" (T.make ~pp:Pp.int ~equal:Int.equal));
    test "order: with_compare, structural, contramap" (fun () ->
        (* [with_compare] gives a plain witness its order and leaves the
           printer and equality alone; a second call replaces the first. *)
        let v = T.make ~pp:Version.pp ~equal:Version.equal in
        ordered_or_fail "with_compare: the module's compare"
          (T.with_compare Version.compare v)
          (Version.make 1 2) (Version.make 1 3);
        check_prints "with_compare: printer untouched"
          (T.with_compare Version.compare v)
          (Version.make 3 14) ~expected:"3.14";
        check_equal "with_compare: equality untouched"
          (T.with_compare Version.compare v)
          (Version.make 1 2) (1, 2);
        ordered_or_fail "with_compare: replaces an existing order"
          (T.with_compare (fun a b -> Int.compare b a) T.int)
          2 1;
        (* [structural] carries polymorphic order next to polymorphic
           equality: both structural, both named by the constructor. *)
        ordered_or_fail "structural: Stdlib.compare"
          (T.structural ~pp:(Pp.list Pp.int))
          [ 1; 2 ] [ 1; 3 ];
        (* [contramap] sends the order through the projection with the
           equality and the printer — and has none to send when the
           underlying witness has none. *)
        ordered_or_fail "contramap: orders through the projection"
          (T.contramap String.length T.int)
          "ab" "abc";
        is_none ~msg:"contramap: no order without one underneath"
          (T.compare (T.contramap (fun p -> [ p ]) (T.list point))));
    test "make: composes as an ordinary witness" (fun () ->
        check_equal "composes into containers" (T.list point)
          [ { Point.x = 0; y = 0 }; { Point.x = 1; y = 1 } ]
          [ { Point.x = 0; y = 0 }; { Point.x = 1; y = 1 } ];
        check_differ "container elements still compared" (T.list point)
          [ { Point.x = 0; y = 0 } ]
          [ { Point.x = 0; y = 1 } ];
        check_prints "container printing uses the module's pp" (T.option point)
          (Some { Point.x = 4; y = 5 })
          ~expected:"Some (4, 5)";
        check_equal "contramap over a module's witness"
          (T.contramap (fun (p, _) -> p) point)
          ({ Point.x = 1; y = 2 }, "ignored")
          ({ Point.x = 1; y = 2 }, "also ignored"));
  ]

let () = exit @@ Windtrap.run "testable" tests
