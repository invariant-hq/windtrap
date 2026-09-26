(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Failure = Windtrap.Private.Failure
module Loc = Windtrap.Private.Loc

let strf = Printf.sprintf
let repeat n s = String.concat "" (List.init n (fun _ -> s))
let lines n = String.concat "" (List.init n (fun i -> strf "line %d\n" i))

exception Full
exception Printed of string

let () = Printexc.register_printer (function Printed s -> Some s | _ -> None)

(* Never inlined, so that a backtrace names their frames. *)
let[@inline never] raise_not_found () = raise Not_found
let[@inline never] through_delimit () = Loc.delimit raise_not_found

(* Well-formed UTF-8 in runs of one code point, long enough to pass the
   bound of a tail and of an excerpt. *)
let utf8 =
  let run =
    Gen.pair (Gen.int_range 0 3_000) (Gen.of_list [ "a"; "\n"; "é"; "€"; "𝄞" ])
  in
  Gen.with_pp
    (fun ppf s -> Format.fprintf ppf "%S" s)
    (Gen.map
       (fun runs -> String.concat "" (List.map (fun (n, s) -> repeat n s) runs))
       (Gen.list ~size:(Gen.int_range 0 6) run))

let on_code_point s i = i = String.length s || Char.code s.[i] land 0xC0 <> 0x80
let code_point s = satisfies ~claim:"an offset on a code point" int s

(* Texts *)

let bounded s =
  let t = Failure.text s in
  strf "%d of %d bytes, %s, %s" (String.length t.kept) t.length
    (if Failure.is_cut t then "cut" else "whole")
    (if String.starts_with ~prefix:t.kept s then "a prefix" else "not a prefix")

let texts =
  group "Texts"
    [
      cases
        "a text keeps the longest prefix of at most 65,536 bytes on a code \
         point"
        ~name:fst
        [
          ("the empty string", ("", "0 of 0 bytes, whole, a prefix"));
          ( "65,536 bytes",
            (String.make 65_536 'a', "65536 of 65536 bytes, whole, a prefix") );
          ( "65,537 bytes",
            (String.make 65_537 'a', "65536 of 65537 bytes, cut, a prefix") );
          ( "a two-byte code point across the bound",
            ("a" ^ repeat 40_000 "é", "65535 of 80001 bytes, cut, a prefix") );
          ( "a four-byte code point across the bound",
            ("a" ^ repeat 20_000 "𝄞", "65533 of 80001 bytes, cut, a prefix") );
        ]
        (fun (_, (s, row)) -> equal string row (bounded s));
    ]

(* Control *)

let control_printer () =
  let controls =
    [ `Skip None; `Skip (Some "why"); `Timeout 1.5; `Exit; `Discard ]
  in
  let printed c = Printexc.to_string (Failure.Control c) in
  expect (String.concat "\n" (List.map printed controls))
  @@ __POS_OF__
       {|
    windtrap skip
    windtrap skip: why
    windtrap timeout after 1.5s
    Exit_attempt (code under test called exit; intercepted by windtrap)
    windtrap discard (assume or reject outside a property)
  |}

let control =
  group "Control"
    [ test "the printer of Control names the control" control_printer ]

(* Catching the user's code *)

let boom = Failure.message "boom"

let caught_row = function
  | Ok () -> "returned"
  | Error (`Assertion f) -> if f == boom then "assertion boom" else "assertion"
  | Error (`Exception (e, _)) -> "exception " ^ Failure.exn_to_string e
  | Error (`Skip None) -> "skip"
  | Error (`Skip (Some reason)) -> "skip " ^ reason
  | Error (`Timeout limit) -> strf "timeout %g" limit
  | Error `Exit -> "exit"
  | Error `Discard -> "discard"

let catch_row f =
  match Failure.catch f with
  | r -> caught_row r
  | exception e -> "raises " ^ Printexc.to_string e

let exception_backtrace = function
  | Error (`Exception (_, raw)) -> Some raw
  | Ok _ | Error (`Assertion _ | `Skip _ | `Timeout _ | `Exit | `Discard) ->
      None

let cut_finally e () = Fun.protect ~finally:(fun () -> raise e) ignore

let raised =
  [
    ( "a Check_failure",
      (fun () -> raise (Failure.Check_failure boom)),
      "assertion boom" );
    ( "a control",
      (fun () -> raise (Failure.Control (`Skip (Some "r")))),
      "skip r" );
    ("an exit", (fun () -> raise (Failure.Control `Exit)), "exit");
    ("any other exception", raise_not_found, "exception Not_found");
    ( "a Stack_overflow",
      (fun () -> raise Stack_overflow),
      "exception Stack overflow" );
  ]

let reraise_row (_, f, _) =
  let c = require_error (Failure.catch f) in
  equal string (caught_row (Error c)) (catch_row (fun () -> Failure.reraise c))

let reraise_keeps_the_backtrace () =
  let c = require_error (Failure.catch raise_not_found) in
  let text r =
    Printexc.raw_backtrace_to_string (require_match exception_backtrace r)
  in
  let original = text (Error c) in
  contains ~sub:"Test_failure.raise_not_found" original;
  starts_with ~affix:original
    (text (Failure.catch (fun () -> Failure.reraise c)))

let catching =
  group "Catching the user's code"
    [
      cases "catch classifies what its function raised"
        ~name:(fun (n, _, _) -> n)
        ((("a return", (fun () -> ()), "returned") :: raised)
        @ [
            ( "a timeout that cut a finally",
              cut_finally (Failure.Control (`Timeout 1.5)),
              "timeout 1.5" );
            ( "another exception that cut a finally",
              cut_finally Not_found,
              "exception Fun.Finally_raised: Not_found" );
            ( "an interrupt",
              (fun () -> raise Sys.Break),
              "raises Stdlib.Sys.Break" );
            ( "exhausted memory",
              (fun () -> raise Out_of_memory),
              "raises Out of memory" );
            ( "an interrupt that cut a finally",
              cut_finally Sys.Break,
              "raises Stdlib.Sys.Break" );
          ])
        (fun (_, f, row) -> equal string row (catch_row f));
      cases "reraise raises again what catch returned"
        ~name:(fun (n, _, _) -> n)
        raised reraise_row;
      test "reraise continues the backtrace of an exception"
        reraise_keeps_the_backtrace;
      cases "exn_to_string drops Dune__exe__ where a name starts with it"
        ~name:(fun (n, _, _) -> n)
        [
          ("an executable's exception", Full, "Test_failure.Full");
          ( "a name inside another exception's printer",
            Fun.Finally_raised Full,
            "Fun.Finally_raised: Test_failure.Full" );
          ( "a name inside a printer's text",
            Printed "call (Dune__exe__M.E)",
            "call (M.E)" );
          ( "a name that only contains it",
            Printed "X_Dune__exe__M.E",
            "X_Dune__exe__M.E" );
          ( "every name of a text",
            Printed "(Dune__exe__M.E) raised Dune__exe__N.F in X_Dune__exe__T",
            "(M.E) raised N.F in X_Dune__exe__T" );
        ]
        (fun (_, e, s) -> equal string s (Failure.exn_to_string e));
      cases "caught_to_string is exn_to_string of the exception it classifies"
        ~name:fst
        [
          ("an exception", (`Exception (Full, Printexc.get_callstack 0), Full));
          ("an assertion", (`Assertion boom, Failure.Check_failure boom));
          ("a timeout", (`Timeout 1.5, Failure.Control (`Timeout 1.5)));
          ("a skip", (`Skip (Some "why"), Failure.Control (`Skip (Some "why"))));
          ("a discard", (`Discard, Failure.Control `Discard));
        ]
        (fun (_, (c, e)) ->
          equal string (Failure.exn_to_string e) (Failure.caught_to_string c));
    ]

(* Backtraces *)

(* This suite is an executable, so dune prefixes the names of its frames. *)
let unwrapped s =
  let prefix = "Dune__exe__" in
  let n = String.length prefix in
  let b = Buffer.create (String.length s) in
  let rec copy i =
    if i < String.length s then
      if i + n <= String.length s && String.sub s i n = prefix then copy (i + n)
      else begin
        Buffer.add_char b s.[i];
        copy (i + 1)
      end
  in
  copy 0;
  Buffer.contents b

(* [catch] handles the raise, so the trace ends in windtrap's frame. *)
let trailing_run_dropped () =
  let raw = require_match exception_backtrace (Failure.catch raise_not_found) in
  let whole = Printexc.raw_backtrace_to_string raw in
  let trimmed = Failure.backtrace_to_string raw in
  contains ~sub:"Windtrap__Failure.catch" whole;
  starts_with ~affix:"Raised at Dune__exe__Test_failure.raise_not_found" whole;
  starts_with ~affix:"Raised at Test_failure.raise_not_found" trimmed;
  starts_with ~affix:trimmed (unwrapped whole);
  not_contains ~sub:"Windtrap__" trimmed;
  not_contains ~sub:"Dune__exe__" trimmed

(* The raise passes two windtrap frames and is handled here, below them. *)
let interior_frames_kept () =
  let raw =
    match Loc.delimit through_delimit with
    | () -> Printexc.get_callstack 0
    | exception Not_found -> Printexc.get_raw_backtrace ()
  in
  let whole = Printexc.raw_backtrace_to_string raw in
  contains ~sub:"Windtrap__Loc.delimit" whole;
  equal string (unwrapped whole) (Failure.backtrace_to_string raw)

(* A raise in [reraise], handled in [catch]: no frame of the reader's. *)
let windtrap_frames_kept () =
  let empty = Printexc.get_callstack 0 in
  let reraise () = Failure.reraise (`Exception (Not_found, empty)) in
  let raw = require_match exception_backtrace (Failure.catch reraise) in
  let slots = Option.value ~default:[||] (Printexc.backtrace_slots raw) in
  let names = Array.to_list (Array.map Printexc.Slot.name slots) in
  let windtrap's = function Some name -> Loc.own_unit name | None -> false in
  satisfies ~claim:"windtrap's frames alone"
    (list (option string))
    (fun names -> names <> [] && List.for_all windtrap's names)
    names;
  let whole = Printexc.raw_backtrace_to_string raw in
  equal string whole (Failure.backtrace_to_string raw)

let backtraces =
  group "Backtraces"
    [
      test "a trailing run of windtrap's frames is dropped, and Dune__exe__"
        trailing_run_dropped;
      test "windtrap's frames above the reader's are kept" interior_frames_kept;
      test "a backtrace of windtrap's frames alone is kept whole"
        windtrap_frames_kept;
      test "an empty backtrace gives the empty string" (fun () ->
          equal string ""
            (Failure.backtrace_to_string (Printexc.get_callstack 0)));
    ]

(* Constructors *)

let big = String.make 200_000 'a'

let phase = function
  | Failure.Body -> "body"
  | Setup -> "setup"
  | Teardown -> "teardown"
  | Release -> "release"

let frame (f : Failure.t) =
  let absent what = function None -> "no " ^ what | Some _ -> "a " ^ what in
  let subtest = match f.subtest with [] -> None | l -> Some l in
  String.concat ", "
    [
      phase f.phase;
      absent "loc" f.loc;
      absent "msg" f.msg;
      absent "subtest" subtest;
      absent "output" f.output_tail;
    ]

let constructed =
  [
    ("equality", Failure.equality ~expected:"1" ~actual:"2" ());
    ( "containment",
      Failure.containment ~demand:Anywhere ~needle:"n" ~haystack:"abc" () );
    ("predicate", Failure.predicate ~claim:"a match" "None");
    ("raised", Failure.raised ());
    ( "baseline",
      Failure.baseline (File "p") (Missing { proposed = Failure.text "x" }) );
    ( "property",
      Failure.property ~rendered:"[]" ~case_index:0 ~shrink_steps:0 ~root:1L
        ~examples:false () );
    ("timeout", Failure.timeout 1.5);
    ("message", Failure.message "boom");
  ]

let texts_of (f : Failure.t) =
  let some name = Option.map (fun t -> (name, t)) in
  let msg = Option.to_list (some "msg" f.msg) in
  msg
  @
  match f.kind with
  | Equality { expected; actual; _ } ->
      [ ("expected", expected); ("actual", actual) ]
  | Containment { needle; _ } -> [ ("needle", needle) ]
  | Raise { expected; actual; backtrace; message_diff; _ } ->
      List.filter_map Fun.id
        [
          some "expected" expected;
          some "actual" actual;
          some "backtrace" backtrace;
        ]
      @ List.concat_map
          (fun (d : Failure.message_diff) ->
            [
              ("expected message", d.expected_message);
              ("actual message", d.actual_message);
            ])
          (Option.to_list message_diff)
  | Baseline { state = Missing { proposed }; _ } -> [ ("proposed", proposed) ]
  | Baseline { state = Mismatch { expected; actual }; _ } ->
      [ ("expected", expected); ("actual", actual) ]
  | Baseline { state = Unresolvable _; _ } -> []
  | Property { rendered; summary; _ } ->
      ("rendered", rendered) :: Option.to_list (some "summary" summary)
  | Timeout _ -> []
  | Message t -> [ ("message", t) ]

let bounded_texts (_, (f, fields)) =
  let row (name, (t : Failure.text)) =
    strf "%s %s of %d" name
      (if Failure.is_cut t then "cut" else "whole")
      t.length
  in
  equal (list string)
    (List.map (fun name -> name ^ " cut of 200000") fields)
    (List.map row (texts_of f))

let defaults (f : Failure.t) =
  let present = function Some _ -> "given" | None -> "none" in
  match f.kind with
  | Equality { expected; actual; not_; diffable } ->
      strf "expected %s, actual %s, not_ %b, diffable %b" expected.kept
        actual.kept not_ diffable
  | Raise { expected; actual; predicate; backtrace; message_diff } ->
      strf "expected %s, actual %s, predicate %b, backtrace %s, message_diff %s"
        (present expected) (present actual) predicate (present backtrace)
        (present message_diff)
  | Baseline { withheld; _ } -> "withheld " ^ present withheld
  | Property { inner; count; summary; shrink_end; rendering; _ } ->
      strf "inner %s, count %s, summary %s, %s, %s" (present inner)
        (present count) (present summary)
        (match shrink_end with
        | Converged -> "converged"
        | Budget_spent | Candidate_raised _ | Timed_out _ -> "not converged")
        (match rendering with Value -> "value" | Pre_image -> "pre-image")
  | Containment _ | Timeout _ | Message _ -> "no default"

let window (f : Failure.t) =
  require_match
    (function
      | Failure.Containment { excerpt; excerpt_offset; haystack_length; _ } ->
          Some (excerpt_offset, excerpt, haystack_length)
      | Equality _ | Raise _ | Baseline _ | Property _ | Timeout _ | Message _
        ->
          None)
    f.kind

let excerpt ?found_at demand haystack =
  window (Failure.containment ?found_at ~demand ~needle:"n" ~haystack ())

let anchored =
  let open Gen in
  with_pp
    (fun ppf (h, a) -> Format.fprintf ppf "%S at %d" h a)
    (let* haystack = utf8 in
     let+ anchor = int_range 0 (String.length haystack) in
     (haystack, anchor))

let anchored_law (haystack, anchor) =
  let offset, excerpt, length = excerpt ~found_at:anchor Anywhere haystack in
  let stop = offset + String.length excerpt in
  cover "longer than tail_bytes" (String.length haystack > Failure.tail_bytes);
  equal int (String.length haystack) length;
  equal string (String.sub haystack offset (stop - offset)) excerpt;
  at_most int ~than:Failure.tail_bytes (String.length excerpt);
  at_most int ~than:anchor offset;
  at_least int ~than:anchor stop;
  code_point (on_code_point haystack) offset;
  code_point (on_code_point haystack) stop

let ordered_anchor () =
  let offset, excerpt, _ =
    excerpt ~found_at:100
      (Ordered { index = 1; resumed_at = 20_000 })
      (String.make 30_000 'a')
  in
  greater int ~than:100 offset;
  at_most int ~than:20_000 offset;
  at_least int ~than:20_000 (offset + String.length excerpt)

let long_lines = repeat 10 (String.make 199 'x' ^ "\n")

let unanchored =
  [
    ("a short haystack is whole", Failure.Anywhere, "hello", (0, "hello"));
    ("short lines: the first 10", Anywhere, lines 20, (0, lines 10));
    ( "long lines: the first 1 KiB",
      Prefix,
      long_lines,
      (0, String.sub long_lines 0 1_024) );
    ( "one line: the first 1 KiB",
      Anywhere,
      String.make 5_000 'a',
      (0, String.make 1_024 'a') );
    ( "a cut inside a code point moves back",
      Anywhere,
      "a" ^ repeat 3_000 "é",
      (0, "a" ^ repeat 511 "é") );
    ( "suffix, short lines: the last 10",
      Suffix,
      lines 20,
      (70, String.concat "" (List.init 10 (fun i -> strf "line %d\n" (i + 10))))
    );
    ( "suffix, no final newline: the last 10",
      Suffix,
      "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10",
      (2, "1\n2\n3\n4\n5\n6\n7\n8\n9\n10") );
    ( "suffix, long lines: the last 1 KiB",
      Suffix,
      long_lines,
      (976, String.sub long_lines 976 1_024) );
    ( "suffix, one line: the last 1 KiB",
      Suffix,
      String.make 3_000 'x',
      (1_976, String.make 1_024 'x') );
    ( "suffix, a cut inside a code point moves forward",
      Suffix,
      repeat 3_000 "é" ^ "a",
      (4_978, repeat 511 "é" ^ "a") );
    ("suffix, fewer than 10 lines: whole", Suffix, lines 3, (0, lines 3));
    ("suffix, the empty haystack", Suffix, "", (0, ""));
  ]

let checked (found_at, resumed_at) =
  let demand =
    match resumed_at with
    | Some resumed_at -> Failure.Ordered { index = 0; resumed_at }
    | None -> Anywhere
  in
  match Failure.containment ?found_at ~demand ~needle:"" ~haystack:"abc" () with
  | _ -> "accepted"
  | exception Invalid_argument _ -> "raises Invalid_argument"

let baseline_names = function
  | Failure.Baseline
      { baseline = File p; state = Unresolvable { candidate }; _ } ->
      Some (p, candidate)
  | Baseline _ | Equality _ | Containment _ | Raise _ | Property _ | Timeout _
  | Message _ ->
      None

let names_whole () =
  let path = String.make 100_000 'p' in
  let f = Failure.baseline (File path) (Unresolvable { candidate = path }) in
  let p, candidate = require_match baseline_names f.kind in
  equal (pair int int) (100_000, 100_000)
    (String.length p, String.length candidate)

let constructors =
  group "Constructors"
    [
      cases
        "a constructor builds a body failure with no location, subtest or \
         output"
        ~name:fst constructed (fun (_, f) ->
          equal string "body, no loc, no msg, no subtest, no output" (frame f));
      cases "a constructor bounds every text it is given" ~name:fst
        [
          ( "equality",
            ( Failure.equality ~msg:big ~expected:big ~actual:big (),
              [ "msg"; "expected"; "actual" ] ) );
          ( "predicate",
            ( Failure.predicate ~msg:big ~claim:big big,
              [ "msg"; "expected"; "actual" ] ) );
          ( "containment",
            ( Failure.containment ~msg:big ~demand:Anywhere ~needle:big
                ~haystack:"abc" (),
              [ "msg"; "needle" ] ) );
          ( "raised",
            ( Failure.raised ~msg:big ~expected:big ~actual:big ~backtrace:big
                ~message_diff:
                  {
                    constructor = "Failure";
                    expected_message = Failure.text big;
                    actual_message = Failure.text big;
                  }
                (),
              [
                "msg";
                "expected";
                "actual";
                "backtrace";
                "expected message";
                "actual message";
              ] ) );
          ( "property",
            ( Failure.property ~summary:big ~rendered:big ~case_index:0
                ~shrink_steps:0 ~root:1L ~examples:false (),
              [ "rendered"; "summary" ] ) );
          ("message", (Failure.message big, [ "message" ]));
        ]
        bounded_texts;
      test "a baseline keeps its path and its unresolvable candidate whole"
        names_whole;
      cases
        "a constructor fills its payload and defaults its optional fields to \
         none"
        ~name:fst
        [
          ( "equality",
            ( Failure.equality ~expected:"1" ~actual:"2" (),
              "expected 1, actual 2, not_ false, diffable true" ) );
          ( "equality, negated",
            ( Failure.equality ~not_:true ~expected:"3" ~actual:"3" (),
              "expected 3, actual 3, not_ true, diffable true" ) );
          ( "predicate",
            ( Failure.predicate ~claim:"a match" "None",
              "expected a match, actual None, not_ false, diffable false" ) );
          ( "raised",
            ( Failure.raised (),
              "expected none, actual none, predicate false, backtrace none, \
               message_diff none" ) );
          ( "raised, an empty backtrace",
            ( Failure.raised ~backtrace:"" (),
              "expected none, actual none, predicate false, backtrace none, \
               message_diff none" ) );
          ( "baseline",
            ( Failure.baseline (File "p")
                (Missing { proposed = Failure.text "x" }),
              "withheld none" ) );
          ( "property",
            ( Failure.property ~rendered:"[]" ~case_index:0 ~shrink_steps:0
                ~root:1L ~examples:false (),
              "inner none, count none, summary none, converged, value" ) );
        ]
        (fun (_, (f, row)) -> equal string row (defaults f));
      cases "containment checks found_at and resumed_at against the haystack"
        ~name:fst
        [
          ("found_at -1", ((Some (-1), None), "raises Invalid_argument"));
          ("found_at 0", ((Some 0, None), "accepted"));
          ("found_at at the end", ((Some 3, None), "accepted"));
          ("found_at past the end", ((Some 4, None), "raises Invalid_argument"));
          ("resumed_at -1", ((None, Some (-1)), "raises Invalid_argument"));
          ("resumed_at 0", ((None, Some 0), "accepted"));
          ("resumed_at at the end", ((None, Some 3), "accepted"));
          ( "resumed_at past the end",
            ((None, Some 4), "raises Invalid_argument") );
        ]
        (fun (_, (offsets, row)) -> equal string row (checked offsets));
      cases
        "an unanchored excerpt is 10 lines or 1 KiB, from the head or under \
         Suffix the end"
        ~name:(fun (n, _, _, _) -> n)
        unanchored
        (fun (_, demand, haystack, window) ->
          let offset, excerpt, _ = excerpt demand haystack in
          equal (pair int string) window (offset, excerpt));
      prop
        "an anchored excerpt is a window of at most tail_bytes around its \
         anchor"
        anchored anchored_law;
      test "an ordered demand anchors its excerpt on resumed_at" ordered_anchor;
    ]

(* Updating *)

let withheld (f : Failure.t) =
  match f.kind with
  | Baseline { withheld = None; _ } -> "none"
  | Baseline { withheld = Some Failed_outside; _ } -> "failed outside"
  | Baseline { withheld = Some Skipped; _ } -> "skipped"
  | Baseline { withheld = Some (Refused { line; reason }); _ } ->
      strf "refused %d: %s" line reason
  | Baseline { withheld = Some Conflict; _ } -> "conflict"
  | Equality _ | Containment _ | Raise _ | Property _ | Timeout _ | Message _ ->
      "no baseline"

let baseline state = Failure.baseline (File "p") state

let mismatch =
  Failure.Mismatch { expected = Failure.text "a"; actual = Failure.text "b" }

let property_inner (f : Failure.t) =
  require_match
    (function
      | Failure.Property { inner; _ } -> inner
      | Equality _ | Containment _ | Raise _ | Baseline _ | Timeout _
      | Message _ ->
          None)
    f.kind

let mark_holds (_, (mark, row)) =
  let marked = Failure.with_withheld mark (baseline mismatch) in
  equal string row (withheld (Failure.with_withheld Failed_outside marked));
  equal string row (withheld (Failure.with_withheld Skipped marked))

let inner_unmarked () =
  let inner = baseline mismatch in
  let f =
    Failure.property ~inner ~rendered:"x" ~case_index:0 ~shrink_steps:0 ~root:1L
      ~examples:false ()
  in
  equal string "none"
    (withheld (property_inner (Failure.with_withheld Skipped f)))

let updating =
  group "Updating"
    [
      cases "with_withheld marks a baseline failure in any state" ~name:fst
        [
          ("missing", Failure.Missing { proposed = Failure.text "a" });
          ("mismatch", mismatch);
          ("unresolvable", Unresolvable { candidate = "c" });
        ]
        (fun (_, state) ->
          equal string "skipped"
            (withheld (Failure.with_withheld Skipped (baseline state))));
      cases "a refused or conflicting mark holds whatever the attempt adds"
        ~name:fst
        [
          ( "refused",
            (Failure.Refused { line = 3; reason = "why" }, "refused 3: why") );
          ("conflict", (Conflict, "conflict"));
        ]
        mark_holds;
      test "with_withheld leaves another kind unmarked" (fun () ->
          let f = Failure.with_withheld Skipped (Failure.message "boom") in
          equal string "no baseline" (withheld f));
      test "with_withheld does not reach the inner failure of a property"
        inner_unmarked;
    ]

(* Captured-output tails *)

let tail_law (output, omitted) =
  let t = Failure.tail ~omitted_bytes:omitted output in
  let len = String.length output and kept = String.length t.text in
  cover "longer than tail_bytes" (len > Failure.tail_bytes);
  ends_with ~affix:t.text output;
  at_most int ~than:Failure.tail_bytes kept;
  if len <= Failure.tail_bytes then equal int len kept
  else at_least int ~than:(Failure.tail_bytes - 3) kept;
  equal int (omitted + len - kept) t.omitted_bytes;
  code_point (on_code_point output) (len - kept)

let ascii_tail () =
  let t = Failure.tail (String.make 10_000 'a') in
  equal (pair int int) (8_192, 1_808) (String.length t.text, t.omitted_bytes)

let tails =
  group "Captured-output tails"
    [
      test "tail_bytes is 8 KiB" (fun () -> equal int 8_192 Failure.tail_bytes);
      test "an ASCII tail keeps the last 8,192 bytes and counts the rest"
        ascii_tail;
      prop
        "a tail is a suffix of at most tail_bytes that counts every byte it \
         omits"
        (Gen.pair utf8 (Gen.int_range 0 1_000))
        tail_law;
      test "tail raises on a negative omitted_bytes" (fun () ->
          raises_match Exn.invalid_arg (fun () ->
              Failure.tail ~omitted_bytes:(-1) "x"));
    ]

let () =
  exit
    (run "failure"
       [ texts; control; catching; backtraces; constructors; updating; tails ])
