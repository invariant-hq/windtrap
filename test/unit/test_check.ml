(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Check = Windtrap.Private.Check
module Failure = Windtrap.Private.Failure

let strf = Printf.sprintf

exception Payload of int * string
exception Fn_payload of (int -> int)
exception Bug

let[@inline never] raise_payload () = raise (Payload (2, "b"))

(* What a verb did *)

let demand = function
  | Failure.Anywhere -> "anywhere"
  | Prefix -> "prefix"
  | Suffix -> "suffix"
  | Ordered { index; resumed_at } -> strf "ordered %d from %d" index resumed_at

let kept = function Some (t : Failure.text) -> t.kept | None -> "nothing"

let message_diff = function
  | None -> ""
  | Some { Failure.constructor; expected_message; actual_message } ->
      strf ", %s message %S, not %S" constructor actual_message.kept
        expected_message.kept

(* The payload of a failure, without what Failure makes of it: the excerpt of
   a containment and the text of a backtrace. *)
let payload (f : Failure.t) =
  match f.kind with
  | Equality { expected; actual; not_; diffable } ->
      strf "%s%s, expected %s, actual %s"
        (if not_ then "not " else "")
        (if diffable then "equality" else "predicate")
        expected.kept actual.kept
  | Containment { needle; found_at; haystack_length; demand = d; _ } ->
      strf "%s %S, %s in %d bytes" (demand d) needle.kept
        (match found_at with Some i -> strf "at %d" i | None -> "nowhere")
        haystack_length
  | Raise { expected; actual; predicate; message_diff = d; backtrace } ->
      strf "%s, expected %s, raised %s%s%s"
        (if predicate then "raise predicate" else "raise")
        (kept expected) (kept actual) (message_diff d)
        (if Option.is_some backtrace then ", with a backtrace" else "")
  | Message t -> strf "message %S" t.kept
  | Baseline _ -> "baseline"
  | Property _ -> "property"
  | Law _ -> "law"
  | Timeout _ -> "timeout"

let control = function
  | `Skip None -> "skip"
  | `Skip (Some reason) -> "skip " ^ reason
  | `Timeout limit -> strf "timeout %g" limit
  | `Exit -> "exit"
  | `Discard -> "discard"

let outcome verb =
  match verb () with
  | () -> "returned"
  | exception Failure.Check_failure f -> payload f
  | exception Failure.Control c -> control c
  | exception e -> "raised " ^ Failure.exn_to_string e

let outcomes claim rows =
  cases claim rows
    ~name:(fun (name, _, _) -> name)
    (fun (_, verb, row) -> equal string row (outcome verb))

let annotation verb =
  match verb () with
  | () -> "returned"
  | exception Failure.Check_failure f ->
      strf "%s at %s" (kept f.msg)
        (match f.loc with
        | Some l -> strf "%s:%d:%d" l.file l.line l.column
        | None -> "nowhere")

let printer_calls use =
  let calls = ref 0 in
  let pp ppf n =
    incr calls;
    Format.pp_print_int ppf n
  in
  let w =
    Testable.make ~pp ~equal:Int.equal |> Testable.with_compare Int.compare
  in
  (match use w pp with () -> () | exception Failure.Check_failure _ -> ());
  !calls

let count_calls claim rows =
  cases claim rows
    ~name:(fun (name, _, _) -> name)
    (fun (_, use, calls) -> equal int calls (printer_calls use))

(* Every verb *)

let pos = ("fake.ml", 42, 3, 9)
let site = "why at fake.ml:42:3"
let unordered = Testable.make ~pp:Format.pp_print_int ~equal:Int.equal

let explosive =
  Testable.make ~pp:Format.pp_print_int ~equal:(fun _ _ -> raise Bug)

let bad_pp = Testable.make ~pp:(fun _ _ -> raise Bug) ~equal:Int.equal
let raising_pp _ _ = raise Bug

let every_verb =
  group "Every verb"
    [
      cases "?msg and ?__POS__ are the failure's msg and site" ~name:fst
        [
          ("equal", fun () -> Check.equal ~__POS__:pos ~msg:"why" int 1 2);
          ( "not_equal",
            fun () -> Check.not_equal ~__POS__:pos ~msg:"why" int 1 1 );
          ("is_true", fun () -> Check.is_true ~__POS__:pos ~msg:"why" false);
          ("is_false", fun () -> Check.is_false ~__POS__:pos ~msg:"why" true);
          ("is_none", fun () -> Check.is_none ~__POS__:pos ~msg:"why" (Some 1));
          ("is_some", fun () -> Check.is_some ~__POS__:pos ~msg:"why" None);
          ("is_ok", fun () -> Check.is_ok ~__POS__:pos ~msg:"why" (Error ()));
          ("is_error", fun () -> Check.is_error ~__POS__:pos ~msg:"why" (Ok ()));
          ( "require_some",
            fun () -> ignore (Check.require_some ~__POS__:pos ~msg:"why" None)
          );
          ( "require_ok",
            fun () ->
              ignore (Check.require_ok ~__POS__:pos ~msg:"why" (Error ())) );
          ( "require_error",
            fun () ->
              ignore (Check.require_error ~__POS__:pos ~msg:"why" (Ok ())) );
          ( "require_match",
            fun () ->
              ignore
                (Check.require_match ~__POS__:pos ~msg:"why" (fun _ -> None) 1)
          );
          ( "satisfies",
            fun () ->
              Check.satisfies ~__POS__:pos ~msg:"why" int (fun _ -> false) 1 );
          ("mem", fun () -> Check.mem ~__POS__:pos ~msg:"why" int 3 [ 1; 2 ]);
          ("less", fun () -> Check.less ~__POS__:pos ~msg:"why" int ~than:1 1);
          ( "at_most",
            fun () -> Check.at_most ~__POS__:pos ~msg:"why" int ~than:1 2 );
          ( "greater",
            fun () -> Check.greater ~__POS__:pos ~msg:"why" int ~than:1 1 );
          ( "at_least",
            fun () -> Check.at_least ~__POS__:pos ~msg:"why" int ~than:1 0 );
          ( "contains",
            fun () -> Check.contains ~__POS__:pos ~msg:"why" ~sub:"z" "abc" );
          ( "not_contains",
            fun () -> Check.not_contains ~__POS__:pos ~msg:"why" ~sub:"a" "abc"
          );
          ( "starts_with",
            fun () -> Check.starts_with ~__POS__:pos ~msg:"why" ~affix:"z" "abc"
          );
          ( "ends_with",
            fun () -> Check.ends_with ~__POS__:pos ~msg:"why" ~affix:"z" "abc"
          );
          ( "in_order",
            fun () ->
              Check.in_order ~__POS__:pos ~msg:"why" ~subs:[ "b"; "a" ] "abc" );
          ( "raises",
            fun () ->
              Check.raises ~__POS__:pos ~msg:"why" Not_found (fun () -> ()) );
          ( "raises_match",
            fun () ->
              Check.raises_match ~__POS__:pos ~msg:"why" (fun _ -> false) ignore
          );
        ]
        (fun (_, verb) -> equal string site (annotation verb));
      cases "fail and failf take ?__POS__ and leave msg empty" ~name:fst
        [
          ("fail", fun () -> Check.fail ~__POS__:pos "x");
          ("failf", fun () -> Check.failf ~__POS__:pos "x %d" 1);
        ]
        (fun (_, verb) ->
          equal string "nothing at fake.ml:42:3" (annotation verb));
      cases "without ?__POS__ a failure is located in the caller's file"
        ~name:fst
        [
          ("equal", fun () -> Check.equal int 1 2);
          ("failf, through Format", fun () -> Check.failf "boom %d" 1);
        ]
        (fun (_, verb) ->
          starts_with ~affix:"nothing at test/unit/test_check.ml:"
            (annotation verb));
      count_calls "a passing verb calls no printer"
        [
          ("equal", (fun w _ -> Check.equal w 5 5), 0);
          ("not_equal", (fun w _ -> Check.not_equal w 1 2), 0);
          ("mem", (fun w _ -> Check.mem w 2 [ 1; 2 ]), 0);
          ("satisfies", (fun w _ -> Check.satisfies w (fun n -> n > 0) 3), 0);
          ("less", (fun w _ -> Check.less w ~than:3 2), 0);
          ("is_none", (fun _ pp -> Check.is_none ~pp None), 0);
          ("is_ok", (fun _ pp -> Check.is_ok ~pp (Ok 1)), 0);
          ("is_error", (fun _ pp -> Check.is_error ~pp (Error 1)), 0);
          ("require_ok", (fun _ pp -> ignore (Check.require_ok ~pp (Ok 1))), 0);
          ( "require_error",
            (fun _ pp -> ignore (Check.require_error ~pp (Error 1))),
            0 );
          ( "require_match",
            (fun _ pp -> ignore (Check.require_match ~pp (fun n -> Some n) 7)),
            0 );
        ];
      count_calls "a failing verb calls its printer once per side it prints"
        [
          ("equal", (fun w _ -> Check.equal w 5 6), 2);
          ( "not_equal, which never prints b",
            (fun w _ -> Check.not_equal w 7 7),
            1 );
          ("satisfies", (fun w _ -> Check.satisfies w (fun _ -> false) 9), 1);
          ("less", (fun w _ -> Check.less w ~than:3 3), 2);
          ("is_none", (fun _ pp -> Check.is_none ~pp (Some 7)), 1);
        ];
      outcomes
        "what a witness, a ?pp, a predicate or an extractor raises escapes"
        [
          ( "equal, the equality",
            (fun () -> Check.equal explosive 1 1),
            "raised Test_check.Bug" );
          ( "not_equal, the equality",
            (fun () -> Check.not_equal explosive 1 2),
            "raised Test_check.Bug" );
          ( "mem, the equality",
            (fun () -> Check.mem explosive 1 [ 1 ]),
            "raised Test_check.Bug" );
          ( "equal, the printer",
            (fun () -> Check.equal bad_pp 1 2),
            "raised Test_check.Bug" );
          ( "less, the order",
            (fun () ->
              Check.less
                (Testable.with_compare (fun _ _ -> raise Bug) int)
                ~than:2 1),
            "raised Test_check.Bug" );
          ( "less, the printer",
            (fun () ->
              Check.less (Testable.with_compare Int.compare bad_pp) ~than:1 2),
            "raised Test_check.Bug" );
          ( "satisfies, the predicate",
            (fun () -> Check.satisfies int (fun _ -> raise Bug) 1),
            "raised Test_check.Bug" );
          ( "satisfies, the printer",
            (fun () -> Check.satisfies bad_pp (fun _ -> false) 1),
            "raised Test_check.Bug" );
          ( "is_none, the ?pp",
            (fun () -> Check.is_none ~pp:raising_pp (Some 1)),
            "raised Test_check.Bug" );
          ( "require_ok, the ?pp",
            (fun () -> ignore (Check.require_ok ~pp:raising_pp (Error 1))),
            "raised Test_check.Bug" );
          ( "require_match, the extractor",
            (fun () -> ignore (Check.require_match (fun _ -> raise Bug) 1)),
            "raised Test_check.Bug" );
          ( "raises_match, the predicate",
            (fun () ->
              Check.raises_match (fun _ -> raise Bug) (fun () -> raise Exit)),
            "raised Test_check.Bug" );
        ];
    ]

(* Equalities *)

let equalities =
  group "Equalities"
    [
      outcomes "an equality verb returns when its claim holds"
        [
          ("equal", (fun () -> Check.equal int 3 3), "returned");
          ( "equal, under the witness's tolerance",
            (fun () -> Check.equal (float 0.5) 1.0 1.2),
            "returned" );
          ("not_equal", (fun () -> Check.not_equal int 1 2), "returned");
          ("is_true", (fun () -> Check.is_true true), "returned");
          ("is_false", (fun () -> Check.is_false false), "returned");
          ("is_none", (fun () -> Check.is_none None), "returned");
          ("is_some", (fun () -> Check.is_some (Some 1)), "returned");
          ("is_ok", (fun () -> Check.is_ok (Ok 7)), "returned");
          ("is_error", (fun () -> Check.is_error (Error "e")), "returned");
        ];
      outcomes
        "a failing equality verb builds a diffable payload, expected first"
        [
          ( "equal",
            (fun () -> Check.equal int 3 4),
            "equality, expected 3, actual 4" );
          ( "equal, through the witness's printer",
            (fun () -> Check.equal string "a" "b"),
            {|equality, expected "a", actual "b"|} );
          ( "not_equal, the first value on both sides",
            (fun () -> Check.not_equal int 3 3),
            "not equality, expected 3, actual 3" );
          ( "not_equal, under the witness's tolerance",
            (fun () -> Check.not_equal (float 0.5) 1.0 1.2),
            "not equality, expected 1., actual 1." );
          ( "is_true",
            (fun () -> Check.is_true false),
            "equality, expected true, actual false" );
          ( "is_false",
            (fun () -> Check.is_false true),
            "equality, expected false, actual true" );
          ( "is_none, through ?pp",
            (fun () -> Check.is_none ~pp:Format.pp_print_int (Some 7)),
            "equality, expected None, actual Some 7" );
          ( "is_none, without ?pp",
            (fun () -> Check.is_none (Some 7)),
            "equality, expected None, actual Some <abstract>" );
          ( "is_some",
            (fun () -> Check.is_some (None : int option)),
            "equality, expected Some _, actual None" );
          ( "is_ok, without ?pp",
            (fun () -> Check.is_ok (Error 3)),
            "equality, expected Ok _, actual Error <abstract>" );
          ( "is_ok, through ?pp",
            (fun () -> Check.is_ok ~pp:Format.pp_print_int (Error 3)),
            "equality, expected Ok _, actual Error 3" );
          ( "is_error, without ?pp",
            (fun () -> Check.is_error (Ok 9)),
            "equality, expected Error _, actual Ok <abstract>" );
          ( "is_error, through ?pp",
            (fun () -> Check.is_error ~pp:Format.pp_print_int (Ok 9)),
            "equality, expected Error _, actual Ok 9" );
        ];
    ]

(* Unwrapping *)

let tcp = function `Tcp p -> Some p | `Unix _ -> None

let unwrapped () =
  equal int 42 (Check.require_some (Some 42));
  equal int 7 (Check.require_ok (Ok 7));
  equal string "e" (Check.require_error (Error "e"));
  equal int 8080 (Check.require_match tcp (`Tcp 8080))

let pp_address ppf = function
  | `Tcp p -> Format.fprintf ppf "tcp:%d" p
  | `Unix path -> Format.fprintf ppf "unix:%s" path

let unwrapping =
  group "Unwrapping"
    [
      test "an unwrapping verb returns the value under the constructor"
        unwrapped;
      outcomes "a failing unwrapping verb builds its payload"
        [
          ( "require_some",
            (fun () -> ignore (Check.require_some None)),
            "equality, expected Some _, actual None" );
          ( "require_ok, without ?pp",
            (fun () -> ignore (Check.require_ok (Error 3))),
            "equality, expected Ok _, actual Error <abstract>" );
          ( "require_ok, through ?pp",
            (fun () ->
              ignore (Check.require_ok ~pp:Format.pp_print_int (Error 3))),
            "equality, expected Ok _, actual Error 3" );
          ( "require_error, without ?pp",
            (fun () -> ignore (Check.require_error (Ok 9))),
            "equality, expected Error _, actual Ok <abstract>" );
          ( "require_error, through ?pp",
            (fun () ->
              ignore (Check.require_error ~pp:Format.pp_print_int (Ok 9))),
            "equality, expected Error _, actual Ok 9" );
          ( "require_match, without ?pp",
            (fun () -> ignore (Check.require_match tcp (`Unix "/tmp/sock"))),
            "predicate, expected a match, actual <abstract>" );
          ( "require_match, through ?pp",
            (fun () ->
              ignore
                (Check.require_match ~pp:pp_address tcp (`Unix "/tmp/sock"))),
            "predicate, expected a match, actual unix:/tmp/sock" );
        ];
    ]

(* Predicates *)

let mem_asks_element_first () =
  let asked = ref [] in
  let w =
    Testable.make ~pp:Format.pp_print_string ~equal:(fun a b ->
        asked := (a, b) :: !asked;
        false)
  in
  ignore (outcome (fun () -> Check.mem w "x" [ "a"; "b" ]));
  equal (list (pair string string)) [ ("x", "a"); ("x", "b") ] (List.rev !asked)

let predicates =
  group "Predicates"
    [
      outcomes "a predicate verb returns when its claim holds"
        [
          ( "satisfies",
            (fun () -> Check.satisfies int (fun n -> n > 0) 3),
            "returned" );
          ( "satisfies, which reads no equality",
            (fun () -> Check.satisfies explosive (fun n -> n = 5) 5),
            "returned" );
          ("mem", (fun () -> Check.mem int 2 [ 1; 2 ]), "returned");
          ( "mem, under the witness's equality",
            (fun () -> Check.mem (float 0.5) 1.0 [ 9.0; 1.2 ]),
            "returned" );
        ];
      outcomes "a failing predicate verb puts its claim on the expected side"
        [
          ( "satisfies",
            (fun () -> Check.satisfies int (fun n -> n > 0) (-4)),
            "predicate, expected value satisfying the predicate, actual -4" );
          ( "satisfies, which reads no equality",
            (fun () -> Check.satisfies explosive (fun _ -> false) 5),
            "predicate, expected value satisfying the predicate, actual 5" );
          ( "satisfies, through the witness's printer",
            (fun () -> Check.satisfies string (fun _ -> false) "a b"),
            {|predicate, expected value satisfying the predicate, actual "a b"|}
          );
          ( "satisfies, with ~claim",
            (fun () ->
              Check.satisfies ~claim:"greater than 0" int (fun n -> n > 0) 0),
            "predicate, expected greater than 0, actual 0" );
          ( "mem",
            (fun () -> Check.mem int 42 [ 2; 3; 5 ]),
            "predicate, expected a list containing 42, actual [2; 3; 5]" );
          ( "mem, an empty list",
            (fun () -> Check.mem string "a" []),
            {|predicate, expected a list containing "a", actual []|} );
        ];
      test "mem gives the element to the witness's equality first"
        mem_asks_element_first;
    ]

(* Orders *)

let close = float 0.5
let ordered_explosive = Testable.with_compare Int.compare explosive

let refuses_unordered verb e =
  Exn.invalid_arg ~substring:("Windtrap." ^ verb ^ ":") e
  && Exn.invalid_arg ~substring:"Testable.with_compare" e

let orders =
  group "Orders"
    [
      outcomes "an ordering verb returns when its relation holds"
        [
          ("less", (fun () -> Check.less int ~than:3 2), "returned");
          ("at_most, below", (fun () -> Check.at_most int ~than:3 2), "returned");
          ( "at_most, at the bound",
            (fun () -> Check.at_most int ~than:3 3),
            "returned" );
          ("greater", (fun () -> Check.greater int ~than:3 4), "returned");
          ( "at_least, above",
            (fun () -> Check.at_least int ~than:3 4),
            "returned" );
          ( "at_least, at the bound",
            (fun () -> Check.at_least int ~than:3 3),
            "returned" );
          ( "less, under a witness with a tolerance",
            (fun () -> Check.less close ~than:1.2 1.0),
            "returned" );
          ( "at_most, nan below every float",
            (fun () -> Check.at_most float_exact ~than:neg_infinity Float.nan),
            "returned" );
          ( "less, which reads no equality",
            (fun () -> Check.less ordered_explosive ~than:2 1),
            "returned" );
        ];
      outcomes "a failing ordering verb claims the relation and the bound"
        [
          ( "less, at the bound",
            (fun () -> Check.less int ~than:3 3),
            "predicate, expected less than 3, actual 3" );
          ( "at_most",
            (fun () -> Check.at_most int ~than:3 4),
            "predicate, expected at most 3, actual 4" );
          ( "greater, at the bound",
            (fun () -> Check.greater int ~than:3 3),
            "predicate, expected greater than 3, actual 3" );
          ( "at_least",
            (fun () -> Check.at_least int ~than:3 2),
            "predicate, expected at least 3, actual 2" );
          ( "greater, through the witness's printer",
            (fun () -> Check.greater string ~than:"m" "a"),
            {|predicate, expected greater than "m", actual "a"|} );
          ( "at_least, without the tolerance",
            (fun () -> Check.at_least close ~than:1.2 1.0),
            "predicate, expected at least 1.2, actual 1." );
          ( "greater, without a relative tolerance",
            (fun () ->
              Check.greater (float_rel ~rel:0.5 ~abs:0.5) ~than:1.2 1.0),
            "predicate, expected greater than 1.2, actual 1." );
          ( "at_least, nan",
            (fun () -> Check.at_least float_exact ~than:neg_infinity Float.nan),
            "predicate, expected at least -inf, actual nan" );
          ( "less, which reads no equality",
            (fun () -> Check.less ordered_explosive ~than:1 2),
            "predicate, expected less than 1, actual 2" );
        ];
      cases
        "an ordering verb refuses a witness without an order, holding or not"
        ~name:(fun (name, _, _) -> name)
        [
          ("less, holding", "less", fun () -> Check.less unordered ~than:3 2);
          ( "at_most, failing",
            "at_most",
            fun () -> Check.at_most unordered ~than:3 4 );
          ( "greater, holding",
            "greater",
            fun () -> Check.greater unordered ~than:3 4 );
          ( "at_least, failing",
            "at_least",
            fun () -> Check.at_least unordered ~than:3 2 );
          ("less, a list", "less", fun () -> Check.less (list int) ~than:[] []);
          ("less, pass", "less", fun () -> Check.less pass ~than:1 0);
        ]
        (fun (_, verb, f) -> raises_match (refuses_unordered verb) f);
    ]

(* String containment *)

let numbered n = String.concat "" (List.init n (fun i -> strf "%07d\n" i))
let filler = numbered 3_000
let log = "start connect send receive stop"
let path = "sessions/ghost/session.json"

let containment =
  group "String containment"
    [
      outcomes "a containment verb returns when its needle is where it demands"
        [
          ("contains", (fun () -> Check.contains ~sub:"ell" "hello"), "returned");
          ( "contains, the empty needle",
            (fun () -> Check.contains ~sub:"" ""),
            "returned" );
          ( "contains, the whole haystack",
            (fun () -> Check.contains ~sub:"hello" "hello"),
            "returned" );
          ( "contains, one of several",
            (fun () -> Check.contains ~sub:"ab" "ab-ab-ab"),
            "returned" );
          ( "not_contains",
            (fun () -> Check.not_contains ~sub:"zz" "hello"),
            "returned" );
          ( "starts_with",
            (fun () -> Check.starts_with ~affix:"sessions/" path),
            "returned" );
          ( "starts_with, the empty affix",
            (fun () -> Check.starts_with ~affix:"" path),
            "returned" );
          ( "starts_with, the whole haystack",
            (fun () -> Check.starts_with ~affix:path path),
            "returned" );
          ( "ends_with",
            (fun () -> Check.ends_with ~affix:".json" path),
            "returned" );
          ( "ends_with, the empty affix",
            (fun () -> Check.ends_with ~affix:"" path),
            "returned" );
          ( "in_order",
            (fun () -> Check.in_order ~subs:[ "start"; "send"; "stop" ] log),
            "returned" );
          ( "in_order, one element",
            (fun () -> Check.in_order ~subs:[ "connect" ] log),
            "returned" );
          ( "in_order, the whole haystack",
            (fun () -> Check.in_order ~subs:[ log ] log),
            "returned" );
          ( "in_order, a repeated element at a later occurrence",
            (fun () -> Check.in_order ~subs:[ "ab"; "ab" ] "abab"),
            "returned" );
          ( "in_order, adjacent matches",
            (fun () -> Check.in_order ~subs:[ "ab"; "cd" ] "abcd"),
            "returned" );
          ( "in_order, empty elements where the search stands",
            (fun () -> Check.in_order ~subs:[ ""; "a"; "" ] "a"),
            "returned" );
        ];
      outcomes
        "a failing containment verb holds its demand, its needle and its first \
         occurrence"
        [
          ( "contains, an absent needle",
            (fun () -> Check.contains ~sub:"zz" "hello world"),
            {|anywhere "zz", nowhere in 11 bytes|} );
          ( "contains, the whole length of a large haystack",
            (fun () -> Check.contains ~sub:"needle" (numbered 4_000)),
            {|anywhere "needle", nowhere in 32000 bytes|} );
          ( "not_contains",
            (fun () -> Check.not_contains ~sub:"NEEDLE" "abcNEEDLEdef"),
            {|anywhere "NEEDLE", at 3 in 12 bytes|} );
          ( "not_contains, the empty needle",
            (fun () -> Check.not_contains ~sub:"" "anything"),
            {|anywhere "", at 0 in 8 bytes|} );
          ( "not_contains, deep in a large haystack",
            (fun () ->
              Check.not_contains ~sub:"NEEDLE" (filler ^ "NEEDLE" ^ filler)),
            {|anywhere "NEEDLE", at 24000 in 48006 bytes|} );
          ( "starts_with, an absent affix",
            (fun () -> Check.starts_with ~affix:"users/" path),
            {|prefix "users/", nowhere in 27 bytes|} );
          ( "starts_with, an affix elsewhere",
            (fun () -> Check.starts_with ~affix:"ghost" path),
            {|prefix "ghost", at 9 in 27 bytes|} );
          ( "ends_with, the leftmost occurrence",
            (fun () -> Check.ends_with ~affix:"session" path),
            {|suffix "session", at 0 in 27 bytes|} );
          ( "ends_with, an affix longer than the haystack",
            (fun () -> Check.ends_with ~affix:"xxxxxxxxxx" "ab"),
            {|suffix "xxxxxxxxxx", nowhere in 2 bytes|} );
          ( "in_order, an element that is nowhere",
            (fun () -> Check.in_order ~subs:[ "start"; "abort" ] log),
            {|ordered 1 from 5 "abort", nowhere in 31 bytes|} );
          ( "in_order, an element only before the search",
            (fun () -> Check.in_order ~subs:[ "send"; "connect" ] log),
            {|ordered 1 from 18 "connect", at 6 in 31 bytes|} );
          ( "in_order, matches that would overlap",
            (fun () -> Check.in_order ~subs:[ "aa"; "aa" ] "aaa"),
            {|ordered 1 from 2 "aa", at 0 in 3 bytes|} );
          ( "in_order, the search deep in a large haystack",
            (fun () ->
              Check.in_order ~subs:[ "OPEN"; "MIDDLE" ]
                (filler ^ "OPEN" ^ filler)),
            {|ordered 1 from 24004 "MIDDLE", nowhere in 48004 bytes|} );
        ];
      cases "in_order refuses an empty chain, whatever the haystack" ~name:fst
        [ ("the empty haystack", ""); ("a haystack", "abc") ]
        (fun (_, s) ->
          raises_match Exn.invalid_arg (fun () -> Check.in_order ~subs:[] s));
    ]

(* Exceptions *)

let backtrace_of verb =
  match verb () with
  | () -> None
  | exception Failure.Check_failure { kind = Raise { backtrace; _ }; _ } ->
      Option.map (fun (t : Failure.text) -> t.kept) backtrace

(* The same raise, handled by the same [catch] that [raises] calls. *)
let trimmed_backtrace () =
  match Failure.catch raise_payload with
  | Error (`Exception (_, raw)) -> Some (Failure.backtrace_to_string raw)
  | Ok _ | Error (`Assertion _ | `Skip _ | `Timeout _ | `Exit | `Discard) ->
      None

let backtrace_as_failure_gives_it () =
  let expected = require_some (trimmed_backtrace ()) in
  contains ~sub:"Test_check.raise_payload" expected;
  equal (option string) (Some expected)
    (backtrace_of (fun () -> Check.raises Not_found raise_payload))

let no_backtrace_unrecorded () =
  let recording = Printexc.backtrace_status () in
  Fun.protect ~finally:(fun () -> Printexc.record_backtrace recording)
  @@ fun () ->
  Printexc.record_backtrace false;
  equal (option string) None
    (backtrace_of (fun () -> Check.raises Not_found raise_payload))

let raised_again =
  [
    ("a failed assertion", Failure.Check_failure (Failure.message "inner"));
    ("a skip", Failure.Control (`Skip (Some "r")));
    ("a timeout", Failure.Control (`Timeout 2.5));
    ("an exit", Failure.Control `Exit);
    ("a discard", Failure.Control `Discard);
    ("an interrupt", Sys.Break);
    ("exhausted memory", Out_of_memory);
  ]

let verbs =
  [
    ("raises naming it", fun e f -> Check.raises e f);
    ("raises naming another", fun _ f -> Check.raises Not_found f);
    ( "raises_match accepting all",
      fun _ f -> Check.raises_match (fun _ -> true) f );
    ( "raises_match rejecting all",
      fun _ f -> Check.raises_match (fun _ -> false) f );
  ]

let passed_through =
  List.concat_map
    (fun (what, e) ->
      List.map
        (fun (verb, apply) -> (strf "%s, %s" verb what, e, apply e))
        verbs)
    raised_again

let exceptions =
  group "Exceptions"
    [
      outcomes
        "an exception verb returns when the function raises what it demands"
        [
          ( "raises",
            (fun () -> Check.raises Not_found (fun () -> raise Not_found)),
            "returned" );
          ( "raises, a structurally equal payload",
            (fun () ->
              Check.raises
                (Payload (1, "x"))
                (fun () -> raise (Payload (1, "x")))),
            "returned" );
          ( "raises, a Stack_overflow",
            (fun () ->
              Check.raises Stack_overflow (fun () -> raise Stack_overflow)),
            "returned" );
          ( "raises_match",
            (fun () ->
              Check.raises_match
                (function Payload (1, _) -> true | _ -> false)
                (fun () -> raise (Payload (1, "any")))),
            "returned" );
          ( "raises_match with an Exn predicate",
            (fun () ->
              Check.raises_match
                (Check.Exn.invalid_arg ~substring:"unhandled op") (fun () ->
                  invalid_arg "step: unhandled op HALT")),
            "returned" );
        ];
      outcomes
        "a failing exception verb holds the exceptions as exn_to_string prints \
         them"
        [
          ( "raises, nothing raised",
            (fun () -> Check.raises Not_found (fun () -> 42)),
            "raise, expected Not_found, raised nothing" );
          ( "raises, another exception",
            (fun () ->
              Check.raises Not_found (fun () -> raise (Payload (0, "z")))),
            {|raise, expected Not_found, raised Test_check.Payload(0, "z"), with a backtrace|}
          );
          ( "raises, a structurally different payload",
            (fun () ->
              Check.raises
                (Payload (1, "x"))
                (fun () -> raise (Payload (1, "y")))),
            {|raise, expected Test_check.Payload(1, "x"), raised Test_check.Payload(1, "y"), with a backtrace|}
          );
          ( "raises, two Invalid_argument",
            (fun () ->
              Check.raises (Invalid_argument "index 3") (fun () ->
                  invalid_arg "index 4")),
            {|raise, expected Invalid_argument("index 3"), raised Invalid_argument("index 4"), Invalid_argument message "index 4", not "index 3", with a backtrace|}
          );
          ( "raises, two Failure",
            (fun () ->
              Check.raises (Stdlib.Failure "port 80") (fun () ->
                  failwith "port 81")),
            {|raise, expected Failure("port 80"), raised Failure("port 81"), Failure message "port 81", not "port 80", with a backtrace|}
          );
          ( "raises, two Sys_error",
            (fun () ->
              Check.raises (Sys_error "a: denied") (fun () ->
                  raise (Sys_error "b: denied"))),
            {|raise, expected Sys_error("a: denied"), raised Sys_error("b: denied"), Sys_error message "b: denied", not "a: denied", with a backtrace|}
          );
          ( "raises, two constructors that carry a message",
            (fun () ->
              Check.raises (Invalid_argument "index 3") (fun () ->
                  failwith "index 4")),
            {|raise, expected Invalid_argument("index 3"), raised Failure("index 4"), with a backtrace|}
          );
          ( "raises, nothing raised against a Failure",
            (fun () -> Check.raises (Stdlib.Failure "boom") (fun () -> 1)),
            {|raise, expected Failure("boom"), raised nothing|} );
          ( "raises_match, nothing raised",
            (fun () -> Check.raises_match (fun _ -> true) (fun () -> ())),
            "raise predicate, expected nothing, raised nothing" );
          ( "raises_match, a rejected exception",
            (fun () ->
              Check.raises_match
                (function Payload (9, _) -> true | _ -> false)
                (fun () -> raise (Payload (1, "no")))),
            {|raise predicate, expected nothing, raised Test_check.Payload(1, "no"), with a backtrace|}
          );
          ( "raises_match, a rejected Sys_error",
            (fun () ->
              Check.raises_match
                (fun _ -> false)
                (fun () -> raise (Sys_error "no such file"))),
            {|raise predicate, expected nothing, raised Sys_error("no such file"), with a backtrace|}
          );
          ( "raises_match, an Exn predicate's substring",
            (fun () ->
              Check.raises_match (Check.Exn.failure ~substring:"underflow")
                (fun () -> failwith "overflow")),
            {|raise predicate, expected nothing, raised Failure("overflow"), with a backtrace|}
          );
        ];
      cases
        "an exception verb raises a failure, a control or a fatal exception \
         again"
        ~name:(fun (name, _, _) -> name)
        passed_through
        (fun (_, e, verb) ->
          equal string
            (outcome (fun () -> raise e))
            (outcome (fun () -> verb (fun () -> raise e))));
      test "raises lets out the error of comparing a functional payload"
        (fun () ->
          raises_match Exn.invalid_arg (fun () ->
              Check.raises (Fn_payload Fun.id) (fun () ->
                  raise (Fn_payload succ))));
      test "raises holds the backtrace that backtrace_to_string makes"
        backtrace_as_failure_gives_it;
      test "raises holds no backtrace when none was recorded"
        no_backtrace_unrecorded;
      cases
        "an Exn predicate accepts its constructor, with the substring when \
         given"
        ~name:(fun (name, _, _) -> name)
        [
          ("invalid_arg", Check.Exn.invalid_arg (Invalid_argument "x"), true);
          ( "invalid_arg, a Failure",
            Check.Exn.invalid_arg (Stdlib.Failure "x"),
            false );
          ("invalid_arg, Not_found", Check.Exn.invalid_arg Not_found, false);
          ( "invalid_arg, the substring",
            Check.Exn.invalid_arg ~substring:"unhandled op"
              (Invalid_argument "step: unhandled op HALT"),
            true );
          ( "invalid_arg, a missing substring",
            Check.Exn.invalid_arg ~substring:"overflow" (Invalid_argument "x"),
            false );
          ( "invalid_arg, the empty substring",
            Check.Exn.invalid_arg ~substring:"" (Invalid_argument ""),
            true );
          ("failure", Check.Exn.failure (Stdlib.Failure "x"), true);
          ( "failure, an Invalid_argument",
            Check.Exn.failure (Invalid_argument "x"),
            false );
          ("sys_error", Check.Exn.sys_error (Sys_error "x"), true);
          ( "sys_error, a Failure",
            Check.Exn.sys_error (Stdlib.Failure "x"),
            false );
          ( "sys_error, the substring",
            Check.Exn.sys_error ~substring:"No such file"
              (Sys_error "nope.txt: No such file or directory"),
            true );
        ]
        (fun (_, accepted, expected) -> equal bool expected accepted);
    ]

(* Escape hatches *)

let escape_hatches =
  group "Escape hatches"
    [
      outcomes "fail, failf and skip raise their payload"
        [
          ("fail", (fun () -> Check.fail "boom"), {|message "boom"|});
          ( "failf",
            (fun () -> Check.failf "bad %s %d" "value" 42),
            {|message "bad value 42"|} );
          ( "skip",
            (fun () -> Check.skip ~reason:"needs docker" ()),
            "skip needs docker" );
          ("skip, without a reason", (fun () -> Check.skip ()), "skip");
        ];
    ]

let () =
  exit
    (run "check"
       [
         every_verb;
         equalities;
         unwrapping;
         predicates;
         orders;
         containment;
         exceptions;
         escape_hatches;
       ])
