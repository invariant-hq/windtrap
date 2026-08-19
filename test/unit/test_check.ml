(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Check: every verb's pass and fail path, payload shapes, [?pos]
   and [?msg] propagation, unwrap semantics, [?pp_error]/[?pp_ok] rendering,
   structural exception equality, and the control-exception re-raise guard.

   The probes call [Check.*] directly and classify what comes back through
   [outcome] — keeping [Raised] apart from [Failed] stops a stray exception
   from passing for either path, and keeps the meta-assertions from leaning
   on the verb under test. *)

open Windtrap
module Check = Windtrap.Private.Check
module F = Windtrap.Private.Failure
module Loc = Windtrap.Private.Loc

let check name cond = is_true ~msg:name cond
let check_string name ~expected ~actual = equal ~msg:name string expected actual

(* The three ways a verb can come back. *)
type outcome_ = Returned | Failed of F.t | Raised of exn

let outcome (f : unit -> unit) : outcome_ =
  match f () with
  | () -> Returned
  | exception F.Check_failure fl -> Failed fl
  | exception e -> Raised e

(* [passes name f] asserts the verb's pass path: [f] returns. *)
let passes name f =
  match outcome f with
  | Returned -> ()
  | Failed _ -> fail (name ^ ": Check_failure on the pass path")
  | Raised e -> fail (name ^ ": raised " ^ Printexc.to_string e)

(* [caught name f] is the failure raised by [f]; the test fails when [f]
   returns or raises anything else. *)
let caught name f =
  match outcome f with
  | Failed fl -> fl
  | Returned -> fail (name ^ ": expected Check_failure, got a return")
  | Raised e ->
      fail (name ^ ": expected Check_failure, raised " ^ Printexc.to_string e)

let equality_payload name f k =
  match caught name f with
  | { F.kind = F.Equality { expected; actual; not_; diffable = true }; _ } ->
      k (expected, actual, not_)
  | _ -> fail (name ^ ": kind is a diffable Equality")

(* [k] gets the claim description and the containment payload. The demand is
   projected separately by [containment_demand] below rather than widening
   this continuation to a seventh component. *)
let containment_payload name f k =
  match caught name f with
  | {
   F.kind =
     F.Containment
       {
         claim;
         needle;
         found_at;
         haystack_length;
         excerpt;
         excerpt_offset;
         demand = _;
       };
   _;
  } ->
      k (claim, excerpt, needle, found_at, haystack_length, excerpt_offset)
  | _ -> fail (name ^ ": kind is Containment")

(* The counted and ordered verbs: what the assertion demanded, and the
   excerpt bookkeeping that demand steers. *)
let containment_demand name f k =
  match caught name f with
  | {
   F.kind = F.Containment { demand; found_at; excerpt; excerpt_offset; _ };
   _;
  } ->
      k (demand, found_at, excerpt, excerpt_offset)
  | _ -> fail (name ^ ": kind is Containment")

(* The demand as a flat string, so a wrong one is legible in the report —
   the [describe_message_diff] precedent below. *)
let describe_demand = function
  | F.Anywhere -> "anywhere"
  | F.Ordered { index; resumed_at } ->
      Printf.sprintf "ordered %d from %d" index resumed_at

let describe_offset = function
  | Some i -> Printf.sprintf "Some %d" i
  | None -> "None"

(* [k] gets the claim description and the rejected value: an Equality whose
   expected side is a description, which is what [diffable = false] says. *)
let predicate_payload name f k =
  match caught name f with
  | { F.kind = F.Equality { expected; actual; diffable = false; _ }; _ } ->
      k (expected, actual)
  | _ -> fail (name ^ ": kind is an undiffable Equality")

let raise_payload name f k =
  match caught name f with
  | { F.kind = F.Raise { expected; actual; backtrace; _ }; _ } ->
      k (expected, actual, backtrace)
  | _ -> fail (name ^ ": kind is Raise")

(* Enrichment variant: [k] gets the recorded message diff (B1). *)
let raise_message_diff name f k =
  match caught name f with
  | { F.kind = F.Raise { message_diff; _ }; _ } -> k message_diff
  | _ -> fail (name ^ ": kind is Raise")

(* The diff as a flat string, so a wrong one is legible in the report. *)
let describe_message_diff = function
  | None -> "none"
  | Some { F.constructor; expected_message; actual_message } ->
      Printf.sprintf "%s: %S -> %S" constructor expected_message actual_message

(* A witness that counts printer calls, to pin down when rendering runs. *)
let counting_int calls =
  Testable.make
    ~pp:(fun ppf n ->
      incr calls;
      Format.pp_print_int ppf n)
    ~equal:Int.equal

let fake_pos = ("fake.ml", 42, 3, 9)
let fake_loc = Loc.of_pos fake_pos

exception Payload of int * string
exception Fn_payload of (int -> int)
exception Extractor_bug

let tcp = function `Tcp p -> Some p | `Unix _ -> None
let reserved = function `Reserved p -> Some p | `Redirect _ -> None

let tests =
  [
    test "equal: pass and fail paths" (fun () ->
        passes "equal: pass" (fun () -> Check.equal Testable.int 3 3);
        (* The witness's equality decides, not [(=)]: tolerance-equal floats
           print differently yet compare equal. *)
        passes "equal: witness equality decides" (fun () ->
            Check.equal (Testable.float 0.5) 1.0 1.2);
        equality_payload "equal: fail payload, expected before actual"
          (fun () -> Check.equal Testable.int 3 4)
          (fun (expected, actual, not_) ->
            check_string "equal: expected is the first argument" ~expected:"3"
              ~actual:expected;
            check_string "equal: actual is the second argument" ~expected:"4"
              ~actual;
            check "equal: not_ is false" (not not_));
        equality_payload "equal: renders with the witness printer"
          (fun () -> Check.equal Testable.string "a" "b")
          (fun (expected, actual, _) ->
            check_string "equal: string renders with %S" ~expected:{|"a"|}
              ~actual:expected;
            check_string "equal: actual string renders with %S"
              ~expected:{|"b"|} ~actual));
    test "equal: defaults, rendering discipline, bounding" (fun () ->
        let fl =
          caught "equal: defaults" (fun () -> Check.equal Testable.int 1 2)
        in
        check "equal: default phase is Body" (fl.F.phase = F.Body);
        check "equal: default msg is None" (fl.F.msg = None);
        check "equal: no output tail at the site" (fl.F.output_tail = None);
        check "equal: kind is a plain, un-negated equality"
          (match fl.F.kind with
          | F.Equality { not_ = false; _ } -> true
          | _ -> false);
        let calls = ref 0 in
        passes "equal: pass path returns" (fun () ->
            Check.equal (counting_int calls) 5 5);
        check "equal: pass path never renders" (!calls = 0);
        let calls = ref 0 in
        ignore (outcome (fun () -> Check.equal (counting_int calls) 5 6));
        check "equal: fail path renders each side once" (!calls = 2);
        (* Payload strings are bounded at construction: Check routes through
           the Failure constructors instead of building records directly.
           The exact bound and marker are Failure's contract; here only
           "far smaller than the rendering" matters. *)
        equality_payload "equal: oversized payloads are bounded"
          (fun () -> Check.equal Testable.string (String.make 100_000 'a') "b")
          (fun (expected, _, _) ->
            check "equal: oversized rendering is cut"
              (String.length expected < 70_000)));
    test "not_equal" (fun () ->
        let calls = ref 0 in
        passes "not_equal: pass" (fun () ->
            Check.not_equal (counting_int calls) 1 2);
        check "not_equal: pass path never renders" (!calls = 0);
        equality_payload "not_equal: fail payload"
          (fun () -> Check.not_equal Testable.int 3 3)
          (fun (expected, actual, not_) ->
            check "not_equal: not_ is true" not_;
            check_string "not_equal: value stored once, expected side"
              ~expected:"3" ~actual:expected;
            check_string "not_equal: value stored once, actual side"
              ~expected:"3" ~actual);
        (* Witness equality can be coarser than printing: under tolerance
           the two floats are equal but would print differently. The payload
           must still carry a single rendering — the first argument's. *)
        equality_payload "not_equal: tolerance-equal floats render once"
          (fun () -> Check.not_equal (Testable.float 0.5) 1.0 1.2)
          (fun (expected, actual, _) ->
            check_string "not_equal: rendering is the first argument's"
              ~expected:"1" ~actual:expected;
            check "not_equal: both sides carry the same string"
              (String.equal expected actual));
        let calls = ref 0 in
        ignore (outcome (fun () -> Check.not_equal (counting_int calls) 7 7));
        check "not_equal: renders the value exactly once" (!calls = 1));
    test "is_true and is_false" (fun () ->
        passes "is_true: pass" (fun () -> Check.is_true true);
        equality_payload "is_true: fail payload"
          (fun () -> Check.is_true false)
          (fun (expected, actual, not_) ->
            check "is_true: payload is true vs false"
              (expected = "true" && actual = "false" && not not_));
        passes "is_false: pass" (fun () -> Check.is_false false);
        equality_payload "is_false: fail payload"
          (fun () -> Check.is_false true)
          (fun (expected, actual, _) ->
            check "is_false: payload is false vs true"
              (expected = "false" && actual = "true")));
    test "contains" (fun () ->
        passes "contains: pass on a present needle" (fun () ->
            Check.contains ~sub:"ell" "hello");
        passes "contains: empty needle is contained in every string" (fun () ->
            Check.contains ~sub:"" "");
        passes "contains: needle equal to the haystack" (fun () ->
            Check.contains ~sub:"hello" "hello");
        containment_payload "contains: fail payload"
          (fun () -> Check.contains ~sub:"zz" "hello world")
          (fun ( claim,
                 excerpt,
                 needle,
                 found_at,
                 haystack_length,
                 excerpt_offset )
             ->
            check_string "contains: claim describes the assertion"
              ~expected:{|string containing "zz"|} ~actual:claim;
            check_string "contains: small haystack stored whole"
              ~expected:"hello world" ~actual:excerpt;
            check_string "contains: needle stored verbatim" ~expected:"zz"
              ~actual:needle;
            check "contains: found_at is None when the needle is absent"
              (found_at = None);
            check "contains: haystack_length is the full byte length"
              (haystack_length = String.length "hello world");
            check "contains: excerpt starts at the head" (excerpt_offset = 0));
        (* A huge haystack: the payload stores a bounded head excerpt, not
           the whole string, and records what the excerpt covers. *)
        let haystack =
          String.concat ""
            (List.init 4_000 (fun i -> Printf.sprintf "%07d\n" i))
        in
        containment_payload "contains: huge haystack excerpts the head"
          (fun () -> Check.contains ~sub:"needle" haystack)
          (fun (_, excerpt, _, found_at, haystack_length, excerpt_offset) ->
            check "contains: excerpt is bounded"
              (String.length excerpt < String.length haystack
              && String.length excerpt <= 8_195);
            check "contains: excerpt is a prefix of the haystack"
              (String.sub haystack 0 (String.length excerpt) = excerpt);
            check "contains: a head excerpt is not a window"
              (found_at = None && excerpt_offset = 0);
            check "contains: haystack_length survives excerpting"
              (haystack_length = String.length haystack)));
    test "not_contains" (fun () ->
        passes "not_contains: pass on an absent needle" (fun () ->
            Check.not_contains ~sub:"zz" "hello");
        containment_payload "not_contains: fail payload"
          (fun () -> Check.not_contains ~sub:"NEEDLE" "abcNEEDLEdef")
          (fun (claim, excerpt, _, found_at, _, _) ->
            check_string "not_contains: claim describes the assertion"
              ~expected:{|string not containing "NEEDLE"|} ~actual:claim;
            check_string "not_contains: small haystack stored whole"
              ~expected:"abcNEEDLEdef" ~actual:excerpt;
            check_string "not_contains: found_at is the occurrence offset"
              ~expected:"Some 3"
              ~actual:
                (match found_at with
                | Some i -> Printf.sprintf "Some %d" i
                | None -> "None"));
        check "not_contains: empty needle always fails"
          (match outcome (fun () -> Check.not_contains ~sub:"" "anything") with
          | Failed { F.kind = F.Containment { found_at = Some 0; _ }; _ } ->
              true
          | _ -> false);
        (* A match deep in a huge haystack: the excerpt windows around the
           match, so the failure shows the occurrence, not an unrelated
           head. *)
        let filler =
          String.concat ""
            (List.init 3_000 (fun i -> Printf.sprintf "%07d\n" i))
        in
        let haystack = filler ^ "NEEDLE" ^ filler in
        containment_payload "not_contains: deep match windows the excerpt"
          (fun () -> Check.not_contains ~sub:"NEEDLE" haystack)
          (fun (_, excerpt, _, found_at, haystack_length, excerpt_offset) ->
            check "not_contains: excerpt is bounded"
              (String.length excerpt <= 8_195);
            match found_at with
            | Some i ->
                check "not_contains: found_at is the real offset"
                  (i = String.length filler);
                check "not_contains: excerpt is cut from around the match"
                  (excerpt_offset > 0 && excerpt_offset <= i);
                check "not_contains: excerpt is the recorded window"
                  (String.sub haystack excerpt_offset (String.length excerpt)
                  = excerpt);
                check "not_contains: the match is inside the window"
                  (let rel = i - excerpt_offset in
                   rel >= 0
                   && rel + String.length "NEEDLE" <= String.length excerpt
                   && String.sub excerpt rel (String.length "NEEDLE") = "NEEDLE");
                check "not_contains: haystack_length is the full byte length"
                  (haystack_length = String.length haystack)
            | None -> check "not_contains: the occurrence is recorded" false));
    test "contains: presence, and nothing beyond it" (fun () ->
        let log = "ab-ab-ab" in
        passes "contains: one occurrence is enough" (fun () ->
            Check.contains ~sub:"ab" log);
        containment_demand "contains: the demand stays plain"
          (fun () -> Check.contains ~sub:"zz" log)
          (fun (demand, _, _, _) ->
            check_string "contains: a failure demands nothing more"
              ~expected:"anywhere" ~actual:(describe_demand demand)));
    test "in_order" (fun () ->
        (* Byte offsets: start 0, connect 6, send 14, receive 19, stop 27. *)
        let log = "start connect send receive stop" in
        passes "in_order: an ordered chain" (fun () ->
            Check.in_order ~subs:[ "start"; "send"; "stop" ] log);
        passes "in_order: a singleton chain is [contains]" (fun () ->
            Check.in_order ~subs:[ "connect" ] log);
        passes "in_order: the whole string as one element" (fun () ->
            Check.in_order ~subs:[ log ] log);
        (* Each match resumes at the END of the previous one, so a repeated
           element needs a second occurrence rather than re-matching the
           first. *)
        passes "in_order: a repeated element takes a later occurrence"
          (fun () -> Check.in_order ~subs:[ "ab"; "ab" ] "abab");
        passes "in_order: adjacent matches" (fun () ->
            Check.in_order ~subs:[ "ab"; "cd" ] "abcd");
        (* The empty needle occurs in every string, so a chain element
           inherits that: it matches at the cursor without advancing it. *)
        passes "in_order: an empty element matches trivially" (fun () ->
            Check.in_order ~subs:[ ""; "a"; "" ] "a");
        check "in_order: an empty chain is a programmer error"
          (match outcome (fun () -> Check.in_order ~subs:[] log) with
          | Raised (Invalid_argument _) -> true
          | _ -> false);
        (* The element is nowhere in the string: the index and the cursor
           name the break, and there is no occurrence to record. *)
        containment_demand "in_order: an element missing entirely"
          (fun () -> Check.in_order ~subs:[ "start"; "abort" ] log)
          (fun (demand, found_at, _, _) ->
            check_string "in_order: the break names its index and cursor"
              ~expected:"ordered 1 from 5" ~actual:(describe_demand demand);
            check_string "in_order: a missing element records no occurrence"
              ~expected:"None" ~actual:(describe_offset found_at));
        (* The out-of-order bug: the element IS in the string, before the
           cursor. [found_at] carries that occurrence — [starts_with]'s rule
           — so the report says "there, but too early", not "not there". *)
        containment_demand "in_order: an element present only before the cursor"
          (fun () -> Check.in_order ~subs:[ "send"; "connect" ] log)
          (fun (demand, found_at, _, _) ->
            check_string "in_order: the out-of-order break names its cursor"
              ~expected:"ordered 1 from 18" ~actual:(describe_demand demand);
            check_string "in_order: the earlier occurrence is recorded"
              ~expected:"Some 6" ~actual:(describe_offset found_at));
        (* Chain matches do not overlap: the first "aa" consumes bytes 0-1,
           so the second must start at 2 and "aaa" has no room for it. *)
        containment_demand "in_order: chain matches do not overlap"
          (fun () -> Check.in_order ~subs:[ "aa"; "aa" ] "aaa")
          (fun (demand, found_at, _, _) ->
            check_string "in_order: the second element resumes past the first"
              ~expected:"ordered 1 from 2" ~actual:(describe_demand demand);
            check_string "in_order: the overlapping occurrence is reported"
              ~expected:"Some 0" ~actual:(describe_offset found_at));
        containment_payload "in_order: fail payload"
          (fun () -> Check.in_order ~subs:[ "start"; "abort" ] log)
          (fun (claim, excerpt, needle, _, haystack_length, _) ->
            check_string "in_order: claim names the element and the cursor"
              ~expected:{|string containing "abort" at or after byte 5|}
              ~actual:claim;
            check_string "in_order: the needle is the element that broke"
              ~expected:"abort" ~actual:needle;
            check_string "in_order: small haystack stored whole" ~expected:log
              ~actual:excerpt;
            check "in_order: haystack_length is the whole string"
              (haystack_length = String.length log));
        (* On a haystack too big to store whole the excerpt shows where the
           search stood — the region still to be matched — not the head the
           reader has already matched past. *)
        let filler =
          String.concat ""
            (List.init 3_000 (fun i -> Printf.sprintf "%07d\n" i))
        in
        let haystack = filler ^ "OPEN" ^ filler in
        containment_demand "in_order: the excerpt windows on the cursor"
          (fun () -> Check.in_order ~subs:[ "OPEN"; "MIDDLE" ] haystack)
          (fun (demand, found_at, excerpt, excerpt_offset) ->
            let cursor = String.length filler + String.length "OPEN" in
            check_string "in_order: the cursor is the end of the first match"
              ~expected:(Printf.sprintf "ordered 1 from %d" cursor)
              ~actual:(describe_demand demand);
            check "in_order: excerpt is bounded"
              (String.length excerpt <= 8_195);
            check "in_order: the window is cut around the cursor, not the head"
              (excerpt_offset > 0 && excerpt_offset <= cursor);
            check "in_order: the excerpt is the recorded window"
              (String.sub haystack excerpt_offset (String.length excerpt)
              = excerpt);
            check "in_order: the cursor is inside the window"
              (cursor - excerpt_offset <= String.length excerpt);
            check_string "in_order: no occurrence to record" ~expected:"None"
              ~actual:(describe_offset found_at)));
    test "starts_with and ends_with" (fun () ->
        let path = "sessions/ghost/session.json" in
        passes "starts_with: pass" (fun () ->
            Check.starts_with ~affix:"sessions/" path);
        passes "ends_with: pass" (fun () ->
            Check.ends_with ~affix:".json" path);
        (* The empty affix bounds both ends of every string. *)
        passes "starts_with: empty affix" (fun () ->
            Check.starts_with ~affix:"" path);
        passes "ends_with: empty affix" (fun () ->
            Check.ends_with ~affix:"" path);
        passes "starts_with: the whole string" (fun () ->
            Check.starts_with ~affix:path path);
        (* Absent: same verdict a [contains] would give, because the reason
           is the same — the affix is nowhere in the string. *)
        containment_payload "starts_with: affix absent"
          (fun () -> Check.starts_with ~affix:"users/" path)
          (fun (claim, _, needle, found_at, _, _) ->
            check_string "claim names the relation"
              ~expected:{|string starting with "users/"|} ~actual:claim;
            check_string "needle is the affix" ~expected:"users/" ~actual:needle;
            check "no occurrence to report" (found_at = None));
        (* Present but misplaced: the offset is the whole point, and it is
           a report only these verbs can produce — [contains] passes here. *)
        containment_payload "starts_with: affix present elsewhere"
          (fun () -> Check.starts_with ~affix:"ghost" path)
          (fun (_, _, _, found_at, _, _) ->
            check "the misplaced occurrence is located" (found_at = Some 9));
        containment_payload "ends_with: affix present elsewhere"
          (fun () -> Check.ends_with ~affix:"session" path)
          (fun (claim, _, _, found_at, _, _) ->
            check_string "claim names the relation"
              ~expected:{|string ending with "session"|} ~actual:claim;
            check "located at its first occurrence" (found_at = Some 0));
        (* A suffix that overruns the string is absent, not a crash. *)
        containment_payload "ends_with: affix longer than the haystack"
          (fun () -> Check.ends_with ~affix:"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" "ab")
          (fun (_, _, _, found_at, _, _) ->
            check "nothing located" (found_at = None)));
    test "mem" (fun () ->
        let calls = ref 0 in
        passes "mem: pass" (fun () -> Check.mem (counting_int calls) 2 [ 1; 2 ]);
        check "mem: pass path never renders" (!calls = 0);
        passes "mem: the witness equality decides, not (=)" (fun () ->
            Check.mem (Testable.float 0.5) 1.0 [ 9.0; 1.2 ]);
        predicate_payload "mem: fail payload"
          (fun () -> Check.mem Testable.int 42 [ 2; 3; 5 ])
          (fun (claim, value) ->
            check_string "mem: claim names the element"
              ~expected:"a list containing 42" ~actual:claim;
            check_string "mem: value is the whole list" ~expected:"[2; 3; 5]"
              ~actual:value);
        predicate_payload "mem: empty list still shows both sides"
          (fun () -> Check.mem Testable.string "a" [])
          (fun (claim, value) ->
            check_string "mem: claim renders the element with the witness"
              ~expected:{|a list containing "a"|} ~actual:claim;
            check_string "mem: empty list renders as []" ~expected:"[]"
              ~actual:value));
    test "is_none and is_some" (fun () ->
        passes "is_none: pass" (fun () -> Check.is_none None);
        passes "is_some: pass" (fun () -> Check.is_some (Some 1));
        (* The point of the verb: no witness is demanded for a type it
           never compares, and the rejected value still prints. *)
        equality_payload "is_none: fail renders Some v with ?pp"
          (fun () -> Check.is_none ~pp:Format.pp_print_int (Some 7))
          (fun (expected, actual, not_) ->
            check_string "is_none: expected side" ~expected:"None"
              ~actual:expected;
            check_string "is_none: actual side names the constructor"
              ~expected:"Some 7" ~actual;
            check "is_none: not a negated equality" (not not_));
        equality_payload "is_none: fail without ?pp"
          (fun () -> Check.is_none (Some 7))
          (fun (_, actual, _) ->
            check_string "is_none: rejected value is <abstract>"
              ~expected:"Some <abstract>" ~actual);
        let calls = ref 0 in
        let counting ppf n =
          incr calls;
          Format.pp_print_int ppf n
        in
        passes "is_none: pass path never renders" (fun () ->
            Check.is_none ~pp:counting None);
        check "is_none: printer stayed unused" (!calls = 0);
        equality_payload "is_some: fail payload"
          (fun () -> Check.is_some (None : int option))
          (fun (expected, actual, _) ->
            (* Same payload as [require_some]'s: one wording for one claim. *)
            check_string "is_some: expected side" ~expected:"Some _"
              ~actual:expected;
            check_string "is_some: actual side" ~expected:"None" ~actual));
    test "satisfies" (fun () ->
        let calls = ref 0 in
        passes "satisfies: pass" (fun () ->
            Check.satisfies (counting_int calls) (fun n -> n > 0) 3);
        check "satisfies: pass path never renders" (!calls = 0);
        predicate_payload "satisfies: fail payload"
          (fun () -> Check.satisfies Testable.int (fun n -> n > 0) (-4))
          (fun (claim, value) ->
            (* The claim sentence is what tells the predicate verbs apart. *)
            check_string "satisfies: claim describes the assertion"
              ~expected:"value satisfying the predicate" ~actual:claim;
            check_string "satisfies: rejected value rendered by the witness"
              ~expected:"-4" ~actual:value);
        let calls = ref 0 in
        ignore
          (outcome (fun () ->
               Check.satisfies (counting_int calls) (fun _ -> false) 9));
        check "satisfies: fail path renders the value once" (!calls = 1);
        predicate_payload "satisfies: renders with the witness printer"
          (fun () -> Check.satisfies Testable.string (fun _ -> false) "a b")
          (fun (_, value) ->
            check_string "satisfies: string renders with %S" ~expected:{|"a b"|}
              ~actual:value);
        (* [?claim] is what makes this the comparison assertion: the bound
           stays the claim and the value stays the value, where
           [is_true (n > 0)] could only report true against false. *)
        predicate_payload "satisfies: ~claim replaces the default sentence"
          (fun () ->
            Check.satisfies ~claim:"greater than 0" Testable.int
              (fun n -> n > 0)
              0)
          (fun (claim, value) ->
            check_string "satisfies: claim is the caller's"
              ~expected:"greater than 0" ~actual:claim;
            check_string "satisfies: value is the value" ~expected:"0"
              ~actual:value);
        (* Renderings come from the witness, so a bound the caller builds
           with it speaks the reader's type. *)
        predicate_payload "satisfies: ~claim renders through the witness"
          (fun () ->
            let bound = "m" in
            Check.satisfies
              ~claim:
                (Printf.sprintf "greater than %s"
                   (Testable.to_string Testable.string bound))
              Testable.string
              (fun s -> s > bound)
              "a")
          (fun (claim, value) ->
            check_string "string bound is quoted"
              ~expected:{|greater than "m"|} ~actual:claim;
            check_string "string value is quoted" ~expected:{|"a"|}
              ~actual:value);
        (* The witness's equality plays no part: an always-raising equality
           is never consulted. *)
        let explosive =
          Testable.make ~pp:Format.pp_print_int ~equal:(fun _ _ -> assert false)
        in
        passes "satisfies: witness equality is not consulted" (fun () ->
            Check.satisfies explosive (fun n -> n = 5) 5);
        (* The B3 evidence shape (tolk's [is_true ~msg:(asprintf "…, got %a"
           pp r)]): the caller owns only a printer. [Testable.structural
           ~pp] supplies the witness, and the failure carries the rendered
           value instead of a hand-formatted message. *)
        let pp_div ppf (a, b) = Format.fprintf ppf "%d / %d" a b in
        predicate_payload "satisfies: printer-only witness (is_true migration)"
          (fun () ->
            Check.satisfies ~msg:"quotient is non-negative"
              (Testable.structural ~pp:pp_div)
              (fun (a, b) -> a / b >= 0)
              (-7, 2))
          (fun (_, value) ->
            check_string "satisfies: rendered by the caller's printer"
              ~expected:"-7 / 2" ~actual:value));
    test "require_some, require_ok, require_error" (fun () ->
        check "require_some: unwraps the payload"
          (Check.require_some (Some 42) = 42);
        equality_payload "require_some: fail payload"
          (fun () -> ignore (Check.require_some None))
          (fun (expected, actual, _) ->
            check "require_some: payload is Some _ vs None"
              (expected = "Some _" && actual = "None"));
        check "require_ok: unwraps the payload" (Check.require_ok (Ok 7) = 7);
        equality_payload "require_ok: fail payload without pp_error"
          (fun () -> ignore (Check.require_ok (Error 3)))
          (fun (expected, actual, _) ->
            check_string "require_ok: expected side" ~expected:"Ok _"
              ~actual:expected;
            check_string "require_ok: rejected side prints <abstract>"
              ~expected:"Error <abstract>" ~actual);
        equality_payload "require_ok: pp_error renders the rejected side"
          (fun () ->
            ignore (Check.require_ok ~pp_error:Format.pp_print_int (Error 3)))
          (fun (_, actual, _) ->
            check_string "require_ok: rendered error" ~expected:"Error 3"
              ~actual);
        let calls = ref 0 in
        let pp ppf n =
          incr calls;
          Format.pp_print_int ppf n
        in
        check "require_ok: pp_error not called on Ok"
          (Check.require_ok ~pp_error:pp (Ok 1) = 1 && !calls = 0);
        check "require_error: unwraps the payload"
          (Check.require_error (Error "e") = "e");
        equality_payload "require_error: fail payload without pp_ok"
          (fun () -> ignore (Check.require_error (Ok 9)))
          (fun (expected, actual, _) ->
            check_string "require_error: expected side" ~expected:"Error _"
              ~actual:expected;
            check_string "require_error: rejected side prints <abstract>"
              ~expected:"Ok <abstract>" ~actual);
        equality_payload "require_error: pp_ok renders the rejected side"
          (fun () ->
            ignore (Check.require_error ~pp_ok:Format.pp_print_int (Ok 9)))
          (fun (_, actual, _) ->
            check_string "require_error: rendered ok" ~expected:"Ok 9" ~actual));
    test "require_match" (fun () ->
        check "require_match: unwraps the matched payload"
          (Check.require_match tcp (`Tcp 8080) = 8080);
        predicate_payload "require_match: fail payload without pp"
          (fun () -> ignore (Check.require_match tcp (`Unix "/tmp/sock")))
          (fun (claim, value) ->
            (* The claim sentence is what tells the predicate verbs apart. *)
            check_string "require_match: claim describes the assertion"
              ~expected:"a match" ~actual:claim;
            check_string "require_match: scrutinee prints <abstract> without pp"
              ~expected:"<abstract>" ~actual:value);
        let pp ppf = function
          | `Tcp p -> Format.fprintf ppf "tcp:%d" p
          | `Unix path -> Format.fprintf ppf "unix:%s" path
        in
        predicate_payload "require_match: pp renders the scrutinee"
          (fun () -> ignore (Check.require_match ~pp tcp (`Unix "/tmp/sock")))
          (fun (_, value) ->
            check_string "require_match: rendered scrutinee"
              ~expected:"unix:/tmp/sock" ~actual:value);
        let calls = ref 0 in
        let pp ppf n =
          incr calls;
          Format.pp_print_int ppf n
        in
        check "require_match: pp not called on a match"
          (Check.require_match ~pp (fun n -> if n > 0 then Some n else None) 7
           = 7
          && !calls = 0);
        (* The extractor runs under no guard: its exceptions are the test's
           own bug, not a failed match. *)
        check "require_match: an exception from the extractor propagates raw"
          (match
             outcome (fun () ->
                 ignore (Check.require_match (fun _ -> raise Extractor_bug) 1))
           with
          | Raised Extractor_bug -> true
          | _ -> false);
        (* The B4 evidence shape (oauth2's [expect_reserved]): an
           application error arrives as [Error `Variant] and the test wants
           the payload. [require_error] unwraps the result half,
           [require_match] the poly-variant half. *)
        check "require_match composes with require_error (oauth2 shape)"
          (Check.require_match reserved
             (Check.require_error (Error (`Reserved "state")))
          = "state");
        predicate_payload
          "require_match: the poly-variant rejection is its own failure"
          (fun () ->
            ignore
              (Check.require_match reserved
                 (Check.require_error (Error (`Redirect "https://cb")))))
          (fun (claim, value) ->
            check_string "require_match: composed failure keeps the match claim"
              ~expected:"a match" ~actual:claim;
            check_string
              "require_match: composed scrutinee is abstract without pp"
              ~expected:"<abstract>" ~actual:value));
    test "raises: structural equality and payload shapes" (fun () ->
        passes "raises: pass on the exact exception" (fun () ->
            Check.raises Not_found (fun () -> raise Not_found));
        (* Structural equality: a freshly allocated payload equal to the
           expected one passes; a different payload does not. *)
        passes "raises: structurally equal payloads pass" (fun () ->
            Check.raises (Payload (1, "x")) (fun () -> raise (Payload (1, "x"))));
        raise_payload "raises: structurally different payloads fail"
          (fun () ->
            Check.raises (Payload (1, "x")) (fun () -> raise (Payload (1, "y"))))
          (fun (expected, actual, _) ->
            check "raises: both exceptions rendered"
              (expected <> None && actual <> None));
        (* Structural comparison cannot see through functional payloads: the
           compare raises and propagates raw — never a silent pass, never a
           "wrong exception" misreport. The .mli points such cases at
           [raises_match]. *)
        check "raises: non-comparable payload raises Invalid_argument"
          (match
             outcome (fun () ->
                 Check.raises
                   (Fn_payload (fun x -> x))
                   (fun () -> raise (Fn_payload (fun x -> x + 1))))
           with
          | Raised (Invalid_argument _) -> true
          | _ -> false);
        raise_payload "raises: nothing raised"
          (fun () -> Check.raises Not_found (fun () -> 42))
          (fun (expected, actual, backtrace) ->
            check "raises: expected exception recorded"
              (expected = Some "Not_found");
            check "raises: actual absent when nothing raised" (actual = None);
            check "raises: no backtrace when nothing raised" (backtrace = None));
        raise_payload "raises: wrong exception"
          (fun () ->
            Check.raises Not_found (fun () -> raise (Payload (0, "z"))))
          (fun (expected, actual, _) ->
            check "raises: expected rendered" (expected = Some "Not_found");
            check "raises: raised exception rendered"
              (match actual with
              | Some s ->
                  (* Printexc renders constructor and payload. *)
                  String.length s > 0
                  && String.sub s 0 (min 8 (String.length s)) <> "Not_foun"
              | None -> false)));
    test "raises: backtrace recording" (fun () ->
        let saved = Printexc.backtrace_status () in
        Fun.protect
          ~finally:(fun () -> Printexc.record_backtrace saved)
          (fun () ->
            Printexc.record_backtrace false;
            raise_payload "raises: no backtrace when recording is off"
              (fun () ->
                Check.raises Not_found (fun () -> raise (Payload (2, "b"))))
              (fun (_, _, backtrace) ->
                check "raises: backtrace absent with recording off"
                  (backtrace = None));
            Printexc.record_backtrace true;
            raise_payload "raises: backtrace captured when recording is on"
              (fun () ->
                Check.raises Not_found (fun () -> raise (Payload (2, "b"))))
              (fun (_, _, backtrace) ->
                check "raises: backtrace present with recording on"
                  (match backtrace with
                  | Some s -> String.length s > 0
                  | None -> false));
            (* [Check.raises] catches the exception, so the frames below the
               thunk are windtrap's own — the trailing run
               [Failure.backtrace_to_string] drops. Here that run is the
               whole of the backtrace bar the raise site, which is why an
               untrimmed report read half machinery. *)
            raise_payload "raises: the backtrace stops at the reader's code"
              (fun () ->
                Check.raises Not_found (fun () -> raise (Payload (2, "b"))))
              (fun (_, _, backtrace) ->
                let lines =
                  match backtrace with
                  | Some s ->
                      List.filter
                        (fun l -> String.length l > 0)
                        (Windtrap.Private.Text.split_lines s)
                  | None -> []
                in
                check "raises: backtrace is non-empty" (lines <> []);
                check "raises: no windtrap frame survives"
                  (not
                     (List.exists
                        (fun l ->
                          Windtrap.Private.Text.contains_substring
                            ~pattern:"Windtrap__" l)
                        lines)))));
    test "raises and raises_match: the control-exception re-raise guard"
      (fun () ->
        (* An assertion failing inside the thunk reports itself: the guard
           re-raises Check_failure instead of treating it as a wrong
           exception. *)
        (match
           caught "raises: inner Check_failure propagates" (fun () ->
               Check.raises Not_found (fun () -> Check.equal Testable.int 1 2))
         with
        | { F.kind = F.Equality { expected = "1"; actual = "2"; _ }; _ } -> ()
        | _ -> fail "raises: inner assertion failure survives unchanged");
        check "raises: inner Skip_test propagates"
          (match
             Check.raises Not_found (fun () -> Check.skip ~reason:"r" ())
           with
          | () -> false
          | exception F.Skip_test (Some "r") -> true
          | exception _ -> false);
        check "raises: inner Timeout propagates"
          (match Check.raises Not_found (fun () -> raise (F.Timeout 2.5)) with
          | () -> false
          | exception F.Timeout 2.5 -> true
          | exception _ -> false);
        (* Same guard, same order, in raises_match: the accept-all predicate
           never sees the control exceptions. *)
        check "raises_match: inner Skip_test propagates"
          (match
             Check.raises_match
               (fun _ -> true)
               (fun () -> Check.skip ~reason:"r" ())
           with
          | () -> false
          | exception F.Skip_test (Some "r") -> true
          | exception _ -> false);
        check "raises_match: inner Timeout propagates"
          (match
             Check.raises_match
               (fun _ -> true)
               (fun () -> raise (F.Timeout 0.1))
           with
          | () -> false
          | exception F.Timeout 0.1 -> true
          | exception _ -> false);
        (* The guard fires before the predicate: even an accept-all
           predicate cannot swallow an inner assertion failure. *)
        match
          caught "raises_match: inner Check_failure propagates" (fun () ->
              Check.raises_match (fun _ -> true) (fun () -> Check.fail "inner"))
        with
        | { F.kind = F.Message "inner"; _ } -> ()
        | _ -> fail "raises_match: inner failure survives unchanged");
    test "raises_match" (fun () ->
        passes "raises_match: pass on a matching exception" (fun () ->
            Check.raises_match
              (function Payload (1, _) -> true | _ -> false)
              (fun () -> raise (Payload (1, "any"))));
        raise_payload "raises_match: nothing raised"
          (fun () -> Check.raises_match (fun _ -> true) (fun () -> ()))
          (fun (expected, actual, backtrace) ->
            check "raises_match: no expected rendering for a predicate"
              (expected = None);
            check "raises_match: actual absent when nothing raised"
              (actual = None);
            check "raises_match: no backtrace when nothing raised"
              (backtrace = None));
        raise_payload "raises_match: predicate rejects"
          (fun () ->
            Check.raises_match
              (function Payload (9, _) -> true | _ -> false)
              (fun () -> raise (Payload (1, "no"))))
          (fun (expected, actual, _) ->
            check "raises_match: expected side stays absent" (expected = None);
            check "raises_match: rejected exception rendered"
              (match actual with Some _ -> true | None -> false)));
    test "raises: the message diff" (fun () ->
        raise_message_diff "raises: same constructor, different message"
          (fun () ->
            Check.raises (Invalid_argument "index 3") (fun () ->
                invalid_arg "index 4"))
          (fun diff ->
            check_string "raises: the diff names the shared constructor"
              ~expected:{|Invalid_argument: "index 3" -> "index 4"|}
              ~actual:(describe_message_diff diff));
        raise_message_diff "raises: different constructors"
          (fun () -> Check.raises Not_found (fun () -> failwith "boom"))
          (fun diff ->
            check_string "raises: constructors that differ have no message diff"
              ~expected:"none"
              ~actual:(describe_message_diff diff));
        raise_message_diff "raises: same user constructor, non-string payload"
          (fun () ->
            Check.raises (Payload (1, "x")) (fun () -> raise (Payload (1, "y"))))
          (fun diff ->
            check_string "raises: no diff without extractable messages"
              ~expected:"none"
              ~actual:(describe_message_diff diff));
        raise_message_diff "raises: nothing raised"
          (fun () -> Check.raises (Stdlib.Failure "boom") (fun () -> 1))
          (fun diff ->
            check_string "raises: nothing raised leaves nothing to diff"
              ~expected:"none"
              ~actual:(describe_message_diff diff));
        raise_message_diff "raises_match: rejected exception"
          (fun () ->
            Check.raises_match
              (fun _ -> false)
              (fun () -> raise (Sys_error "no such file")))
          (fun diff ->
            check_string
              "raises_match: a predicate has no expected side to diff"
              ~expected:"none"
              ~actual:(describe_message_diff diff)));
    test "Exn predicates" (fun () ->
        check "Exn.invalid_arg: matches the constructor"
          (Check.Exn.invalid_arg (Invalid_argument "x"));
        check "Exn.invalid_arg: rejects other exceptions"
          (not (Check.Exn.invalid_arg (Stdlib.Failure "x")));
        check "Exn.invalid_arg: rejects payloadless exceptions"
          (not (Check.Exn.invalid_arg Not_found));
        check "Exn.failure: matches the constructor"
          (Check.Exn.failure (Stdlib.Failure "x"));
        check
          "Exn.failure: rejects Invalid_argument even with a matching message"
          (not (Check.Exn.failure (Invalid_argument "x")));
        (* The third message-carrying exception [raises] diffs by message:
           [Exn] covering only two of the three was arbitrary. *)
        check "Exn.sys_error: matches the constructor"
          (Check.Exn.sys_error (Sys_error "x"));
        check "Exn.sys_error: rejects other exceptions"
          (not (Check.Exn.sys_error (Stdlib.Failure "x")));
        check "Exn.sys_error: ~substring matches inside the message"
          (Check.Exn.sys_error ~substring:"No such file"
             (Sys_error "nope.txt: No such file or directory"));
        check "Exn.invalid_arg: ~substring matches inside the message"
          (Check.Exn.invalid_arg ~substring:"unhandled op"
             (Invalid_argument "step: unhandled op HALT"));
        check "Exn.invalid_arg: ~substring rejects a missing needle"
          (not
             (Check.Exn.invalid_arg ~substring:"overflow" (Invalid_argument "x")));
        check "Exn.invalid_arg: empty ~substring matches any message"
          (Check.Exn.invalid_arg ~substring:"" (Invalid_argument ""));
        (* The composition the predicates exist for. *)
        passes "raises_match composes with Exn.invalid_arg" (fun () ->
            Check.raises_match (Check.Exn.invalid_arg ~substring:"unhandled op")
              (fun () -> invalid_arg "step: unhandled op HALT"));
        raise_payload "raises_match rejects via Exn.failure ~substring"
          (fun () ->
            Check.raises_match
              (Check.Exn.failure ~substring:"underflow")
              (fun () -> failwith "overflow"))
          (fun (_, actual, _) ->
            check "raises_match + Exn: rejected exception rendered"
              (actual <> None)));
    test "fail, failf, skip" (fun () ->
        (match
           caught "fail: raises a Message failure" (fun () -> Check.fail "boom")
         with
        | { F.kind = F.Message "boom"; msg = None; _ } -> ()
        | _ -> fail "fail: message stored, no msg annotation");
        check "fail: usable in expression position"
          (match
             outcome (fun () ->
                 let n : int = if true then Check.fail "nope" else 3 in
                 ignore n)
           with
          | Failed _ -> true
          | _ -> false);
        (match
           caught "failf: formats the message" (fun () ->
               Check.failf "bad %s %d" "value" 42)
         with
        | { F.kind = F.Message m; _ } ->
            check_string "failf: formatted payload" ~expected:"bad value 42"
              ~actual:m
        | _ -> fail "failf: kind is Message");
        check "failf: usable in expression position"
          (match
             outcome (fun () ->
                 let n : int = if true then Check.failf "no %d" 7 else 3 in
                 ignore n)
           with
          | Failed { F.kind = F.Message "no 7"; _ } -> true
          | _ -> false);
        check "skip: raises Skip_test with the reason"
          (match Check.skip ~reason:"needs docker" () with
          | _ -> false
          | exception F.Skip_test (Some "needs docker") -> true
          | exception _ -> false);
        check "skip: reason defaults to None"
          (match Check.skip () with
          | _ -> false
          | exception F.Skip_test None -> true
          | exception _ -> false));
    test "?pos wins over the captured location on every verb" (fun () ->
        let with_pos name f =
          let fl = caught name f in
          check (name ^ ": ?pos wins") (fl.F.loc = Some fake_loc)
        in
        with_pos "equal: ?pos" (fun () ->
            Check.equal ~pos:fake_pos Testable.int 1 2);
        with_pos "not_equal: ?pos" (fun () ->
            Check.not_equal ~pos:fake_pos Testable.int 1 1);
        with_pos "is_true: ?pos" (fun () -> Check.is_true ~pos:fake_pos false);
        with_pos "is_false: ?pos" (fun () -> Check.is_false ~pos:fake_pos true);
        with_pos "require_some: ?pos" (fun () ->
            ignore (Check.require_some ~pos:fake_pos None));
        with_pos "require_ok: ?pos" (fun () ->
            ignore (Check.require_ok ~pos:fake_pos (Error ())));
        with_pos "require_error: ?pos" (fun () ->
            ignore (Check.require_error ~pos:fake_pos (Ok ())));
        with_pos "raises: ?pos" (fun () ->
            Check.raises ~pos:fake_pos Not_found (fun () -> ()));
        with_pos "raises_match: ?pos" (fun () ->
            Check.raises_match ~pos:fake_pos (fun _ -> false) (fun () -> ()));
        with_pos "contains: ?pos" (fun () ->
            Check.contains ~pos:fake_pos ~sub:"z" "abc");
        with_pos "not_contains: ?pos" (fun () ->
            Check.not_contains ~pos:fake_pos ~sub:"a" "abc");
        with_pos "in_order: ?pos" (fun () ->
            Check.in_order ~pos:fake_pos ~subs:[ "b"; "a" ] "abc");
        with_pos "satisfies: ?pos" (fun () ->
            Check.satisfies ~pos:fake_pos Testable.int (fun _ -> false) 1);
        with_pos "require_match: ?pos" (fun () ->
            ignore (Check.require_match ~pos:fake_pos (fun _ -> None) 1));
        with_pos "fail: ?pos" (fun () -> Check.fail ~pos:fake_pos "x");
        with_pos "failf: ?pos" (fun () -> Check.failf ~pos:fake_pos "x %d" 1));
    test "default location is captured from the call stack" (fun () ->
        (* Without ?pos the location is captured from the call stack and
           points at this file — user code, not windtrap's frames. *)
        (match
           caught "equal: default location" (fun () ->
               Check.equal Testable.int 1 2)
         with
        | { F.loc = Some loc; _ } ->
            check_string "equal: captured location is the caller's file"
              ~expected:"test_check.ml"
              ~actual:(Filename.basename loc.Loc.file)
        | _ -> fail "equal: default location captured");
        (* The deepest indirection: failf raises from a kasprintf
           continuation, with Format machinery between the call site and the
           capture. The heuristic must still attribute to this file. *)
        match
          caught "failf: default location" (fun () -> Check.failf "boom %d" 1)
        with
        | { F.loc = Some loc; _ } ->
            check_string "failf: captured location is the caller's file"
              ~expected:"test_check.ml"
              ~actual:(Filename.basename loc.Loc.file)
        | _ -> fail "failf: default location captured");
    test "?msg propagates on every verb" (fun () ->
        let msg_of name f = (caught name f).F.msg in
        check "equal: ?msg stored"
          (msg_of "equal: ?msg" (fun () ->
               Check.equal ~msg:"ids" Testable.int 1 2)
          = Some "ids");
        check "not_equal: ?msg stored"
          (msg_of "not_equal: ?msg" (fun () ->
               Check.not_equal ~msg:"ids" Testable.int 1 1)
          = Some "ids");
        check "is_false: ?msg stored"
          (msg_of "is_false: ?msg" (fun () -> Check.is_false ~msg:"flag" true)
          = Some "flag");
        check "require_ok: ?msg stored"
          (msg_of "require_ok: ?msg" (fun () ->
               ignore (Check.require_ok ~msg:"cfg" (Error ())))
          = Some "cfg");
        check "raises: ?msg stored"
          (msg_of "raises: ?msg" (fun () ->
               Check.raises ~msg:"boom" Not_found (fun () -> ()))
          = Some "boom");
        check "contains: ?msg stored"
          (msg_of "contains: ?msg" (fun () ->
               Check.contains ~msg:"log" ~sub:"z" "abc")
          = Some "log");
        check "not_contains: ?msg stored"
          (msg_of "not_contains: ?msg" (fun () ->
               Check.not_contains ~msg:"log" ~sub:"a" "abc")
          = Some "log");
        check "in_order: ?msg stored"
          (msg_of "in_order: ?msg" (fun () ->
               Check.in_order ~msg:"trace" ~subs:[ "b"; "a" ] "abc")
          = Some "trace");
        check "satisfies: ?msg stored"
          (msg_of "satisfies: ?msg" (fun () ->
               Check.satisfies ~msg:"positive" Testable.int (fun _ -> false) 1)
          = Some "positive");
        check "require_match: ?msg stored"
          (msg_of "require_match: ?msg" (fun () ->
               ignore (Check.require_match ~msg:"tcp" (fun _ -> None) 1))
          = Some "tcp"));
  ]
