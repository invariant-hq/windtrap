(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Check: every verb's pass and fail path, payload shapes, [?__POS__]
   and [?msg] propagation, unwrap semantics, [?pp] rendering,
   structural exception equality, and the control-exception re-raise guard.

   The probes call [Check.*] directly and classify what comes back through
   [outcome]. Keeping [Raised] apart from [Failed] stops a stray exception
   from passing for either path, and keeps the meta-assertions from leaning
   on the verb under test. *)

open Windtrap
module Check = Windtrap.Private.Check
module F = Windtrap.Private.Failure
module Loc = Windtrap.Private.Loc

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
      k (expected.F.kept, actual.F.kept, not_)
  | _ -> fail (name ^ ": kind is a diffable Equality")

(* The demand as a flat string, so a wrong one is legible in the report
   (the [describe_message_diff] precedent below). *)
let describe_demand = function
  | F.Anywhere -> "anywhere"
  | F.Prefix -> "prefix"
  | F.Suffix -> "suffix"
  | F.Ordered { index; resumed_at } ->
      Printf.sprintf "ordered %d from %d" index resumed_at

(* [k] gets the demand, flattened by [describe_demand], and the containment
   payload. *)
let containment_payload name f k =
  match caught name f with
  | {
   F.kind =
     F.Containment
       { needle; found_at; haystack_length; excerpt; excerpt_offset; demand };
   _;
  } ->
      k
        ( describe_demand demand,
          excerpt,
          needle.F.kept,
          found_at,
          haystack_length,
          excerpt_offset )
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

let describe_offset = function
  | Some i -> Printf.sprintf "Some %d" i
  | None -> "None"

(* [k] gets the claim description and the rejected value: an Equality whose
   expected side is a description, which is what [diffable = false] says. *)
let predicate_payload name f k =
  match caught name f with
  | { F.kind = F.Equality { expected; actual; diffable = false; _ }; _ } ->
      k (expected.F.kept, actual.F.kept)
  | _ -> fail (name ^ ": kind is an undiffable Equality")

let raise_payload name f k =
  match caught name f with
  | { F.kind = F.Raise { expected; actual; backtrace; _ }; _ } ->
      let kept = Option.map (fun (t : F.text) -> t.kept) in
      k (kept expected, kept actual, kept backtrace)
  | _ -> fail (name ^ ": kind is Raise")

(* Enrichment variant: [k] gets the recorded message diff. *)
let raise_message_diff name f k =
  match caught name f with
  | { F.kind = F.Raise { message_diff; _ }; _ } -> k message_diff
  | _ -> fail (name ^ ": kind is Raise")

(* The diff as a flat string, so a wrong one is legible in the report. *)
let describe_message_diff = function
  | None -> "none"
  | Some { F.constructor; expected_message; actual_message } ->
      Printf.sprintf "%s: %S -> %S" constructor expected_message.F.kept
        actual_message.F.kept

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
            equal ~msg:"equal: expected is the first argument" string "3"
              expected;
            equal ~msg:"equal: actual is the second argument" string "4" actual;
            is_true ~msg:"equal: not_ is false" (not not_));
        equality_payload "equal: renders with the witness printer"
          (fun () -> Check.equal Testable.string "a" "b")
          (fun (expected, actual, _) ->
            equal ~msg:"equal: string renders with %S" string {|"a"|} expected;
            equal ~msg:"equal: actual string renders with %S" string {|"b"|}
              actual));
    test "equal: defaults, rendering discipline, bounding" (fun () ->
        let fl =
          caught "equal: defaults" (fun () -> Check.equal Testable.int 1 2)
        in
        is_true ~msg:"equal: default phase is Body" (fl.F.phase = F.Body);
        is_true ~msg:"equal: default msg is None" (fl.F.msg = None);
        is_true ~msg:"equal: no output tail at the site"
          (fl.F.output_tail = None);
        is_true ~msg:"equal: kind is a plain, un-negated equality"
          (match fl.F.kind with
          | F.Equality { not_ = false; _ } -> true
          | _ -> false);
        let calls = ref 0 in
        passes "equal: pass path returns" (fun () ->
            Check.equal (counting_int calls) 5 5);
        is_true ~msg:"equal: pass path never renders" (!calls = 0);
        let calls = ref 0 in
        ignore (outcome (fun () -> Check.equal (counting_int calls) 5 6));
        is_true ~msg:"equal: fail path renders each side once" (!calls = 2);
        (* Payload strings are bounded at construction: Check routes through
           the Failure constructors instead of building records directly.
           The exact bound and marker are Failure's contract; here only
           "far smaller than the rendering" matters. *)
        equality_payload "equal: oversized payloads are bounded"
          (fun () -> Check.equal Testable.string (String.make 100_000 'a') "b")
          (fun (expected, _, _) ->
            is_true ~msg:"equal: oversized rendering is cut"
              (String.length expected < 70_000)));
    test "not_equal" (fun () ->
        let calls = ref 0 in
        passes "not_equal: pass" (fun () ->
            Check.not_equal (counting_int calls) 1 2);
        is_true ~msg:"not_equal: pass path never renders" (!calls = 0);
        equality_payload "not_equal: fail payload"
          (fun () -> Check.not_equal Testable.int 3 3)
          (fun (expected, actual, not_) ->
            is_true ~msg:"not_equal: not_ is true" not_;
            equal ~msg:"not_equal: value stored once, expected side" string "3"
              expected;
            equal ~msg:"not_equal: value stored once, actual side" string "3"
              actual);
        (* Witness equality can be coarser than printing: under tolerance
           the two floats are equal but would print differently. The payload
           must still carry a single rendering, the first argument's. *)
        equality_payload "not_equal: tolerance-equal floats render once"
          (fun () -> Check.not_equal (Testable.float 0.5) 1.0 1.2)
          (fun (expected, actual, _) ->
            equal ~msg:"not_equal: rendering is the first argument's" string "1"
              expected;
            is_true ~msg:"not_equal: both sides carry the same string"
              (String.equal expected actual));
        let calls = ref 0 in
        ignore (outcome (fun () -> Check.not_equal (counting_int calls) 7 7));
        is_true ~msg:"not_equal: renders the value exactly once" (!calls = 1));
    test "is_true and is_false" (fun () ->
        passes "is_true: pass" (fun () -> Check.is_true true);
        equality_payload "is_true: fail payload"
          (fun () -> Check.is_true false)
          (fun (expected, actual, not_) ->
            is_true ~msg:"is_true: payload is true vs false"
              (expected = "true" && actual = "false" && not not_));
        passes "is_false: pass" (fun () -> Check.is_false false);
        equality_payload "is_false: fail payload"
          (fun () -> Check.is_false true)
          (fun (expected, actual, _) ->
            is_true ~msg:"is_false: payload is false vs true"
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
          (fun ( demand,
                 excerpt,
                 needle,
                 found_at,
                 haystack_length,
                 excerpt_offset )
             ->
            equal ~msg:"contains: an occurrence anywhere" string "anywhere"
              demand;
            equal ~msg:"contains: small haystack stored whole" string
              "hello world" excerpt;
            equal ~msg:"contains: needle stored verbatim" string "zz" needle;
            is_true ~msg:"contains: found_at is None when the needle is absent"
              (found_at = None);
            is_true ~msg:"contains: haystack_length is the full byte length"
              (haystack_length = String.length "hello world");
            is_true ~msg:"contains: excerpt starts at the head"
              (excerpt_offset = 0));
        (* A huge haystack: the payload stores a bounded head excerpt, not
           the whole string, and records what the excerpt covers. *)
        let haystack =
          String.concat ""
            (List.init 4_000 (fun i -> Printf.sprintf "%07d\n" i))
        in
        containment_payload "contains: huge haystack excerpts the head"
          (fun () -> Check.contains ~sub:"needle" haystack)
          (fun (_, excerpt, _, found_at, haystack_length, excerpt_offset) ->
            is_true ~msg:"contains: excerpt is bounded"
              (String.length excerpt < String.length haystack
              && String.length excerpt <= 8_195);
            is_true ~msg:"contains: excerpt is a prefix of the haystack"
              (String.sub haystack 0 (String.length excerpt) = excerpt);
            is_true ~msg:"contains: a head excerpt is not a window"
              (found_at = None && excerpt_offset = 0);
            is_true ~msg:"contains: haystack_length survives excerpting"
              (haystack_length = String.length haystack)));
    test "not_contains" (fun () ->
        passes "not_contains: pass on an absent needle" (fun () ->
            Check.not_contains ~sub:"zz" "hello");
        containment_payload "not_contains: fail payload"
          (fun () -> Check.not_contains ~sub:"NEEDLE" "abcNEEDLEdef")
          (fun (demand, excerpt, _, found_at, _, _) ->
            equal ~msg:"not_contains: no occurrence anywhere" string "anywhere"
              demand;
            equal ~msg:"not_contains: small haystack stored whole" string
              "abcNEEDLEdef" excerpt;
            equal ~msg:"not_contains: found_at is the occurrence offset" string
              "Some 3"
              (match found_at with
              | Some i -> Printf.sprintf "Some %d" i
              | None -> "None"));
        is_true ~msg:"not_contains: empty needle always fails"
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
            is_true ~msg:"not_contains: excerpt is bounded"
              (String.length excerpt <= 8_195);
            match found_at with
            | Some i ->
                is_true ~msg:"not_contains: found_at is the real offset"
                  (i = String.length filler);
                is_true
                  ~msg:"not_contains: excerpt is cut from around the match"
                  (excerpt_offset > 0 && excerpt_offset <= i);
                is_true ~msg:"not_contains: excerpt is the recorded window"
                  (String.sub haystack excerpt_offset (String.length excerpt)
                  = excerpt);
                is_true ~msg:"not_contains: the match is inside the window"
                  (let rel = i - excerpt_offset in
                   rel >= 0
                   && rel + String.length "NEEDLE" <= String.length excerpt
                   && String.sub excerpt rel (String.length "NEEDLE") = "NEEDLE");
                is_true
                  ~msg:"not_contains: haystack_length is the full byte length"
                  (haystack_length = String.length haystack)
            | None ->
                is_true ~msg:"not_contains: the occurrence is recorded" false));
    test "contains: presence, and nothing beyond it" (fun () ->
        let log = "ab-ab-ab" in
        passes "contains: one occurrence is enough" (fun () ->
            Check.contains ~sub:"ab" log);
        containment_demand "contains: the demand stays plain"
          (fun () -> Check.contains ~sub:"zz" log)
          (fun (demand, _, _, _) ->
            equal ~msg:"contains: a failure demands nothing more" string
              "anywhere" (describe_demand demand)));
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
        is_true ~msg:"in_order: an empty chain is a programmer error"
          (match outcome (fun () -> Check.in_order ~subs:[] log) with
          | Raised (Invalid_argument m) ->
              m = "Windtrap.in_order: subs is empty"
          | _ -> false);
        (* The element is nowhere in the string: the index and the cursor
           name the break, and there is no occurrence to record. *)
        containment_demand "in_order: an element missing entirely"
          (fun () -> Check.in_order ~subs:[ "start"; "abort" ] log)
          (fun (demand, found_at, _, _) ->
            equal ~msg:"in_order: the break names its index and cursor" string
              "ordered 1 from 5" (describe_demand demand);
            equal ~msg:"in_order: a missing element records no occurrence"
              string "None" (describe_offset found_at));
        (* The out-of-order bug: the element IS in the string, before the
           cursor. [found_at] carries that occurrence ([starts_with]'s rule)
           so the report says "there, but too early", not "not there". *)
        containment_demand "in_order: an element present only before the cursor"
          (fun () -> Check.in_order ~subs:[ "send"; "connect" ] log)
          (fun (demand, found_at, _, _) ->
            equal ~msg:"in_order: the out-of-order break names its cursor"
              string "ordered 1 from 18" (describe_demand demand);
            equal ~msg:"in_order: the earlier occurrence is recorded" string
              "Some 6" (describe_offset found_at));
        (* Chain matches do not overlap: the first "aa" consumes bytes 0-1,
           so the second must start at 2 and "aaa" has no room for it. *)
        containment_demand "in_order: chain matches do not overlap"
          (fun () -> Check.in_order ~subs:[ "aa"; "aa" ] "aaa")
          (fun (demand, found_at, _, _) ->
            equal ~msg:"in_order: the second element resumes past the first"
              string "ordered 1 from 2" (describe_demand demand);
            equal ~msg:"in_order: the overlapping occurrence is reported" string
              "Some 0" (describe_offset found_at));
        containment_payload "in_order: fail payload"
          (fun () -> Check.in_order ~subs:[ "start"; "abort" ] log)
          (fun (demand, excerpt, needle, _, haystack_length, _) ->
            equal ~msg:"in_order: the demand names the element and the cursor"
              string "ordered 1 from 5" demand;
            equal ~msg:"in_order: the needle is the element that broke" string
              "abort" needle;
            equal ~msg:"in_order: small haystack stored whole" string log
              excerpt;
            is_true ~msg:"in_order: haystack_length is the whole string"
              (haystack_length = String.length log));
        (* On a haystack too big to store whole the excerpt shows where the
           search stood (the region still to be matched), not the head the
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
            equal ~msg:"in_order: the cursor is the end of the first match"
              string
              (Printf.sprintf "ordered 1 from %d" cursor)
              (describe_demand demand);
            is_true ~msg:"in_order: excerpt is bounded"
              (String.length excerpt <= 8_195);
            is_true
              ~msg:"in_order: the window is cut around the cursor, not the head"
              (excerpt_offset > 0 && excerpt_offset <= cursor);
            is_true ~msg:"in_order: the excerpt is the recorded window"
              (String.sub haystack excerpt_offset (String.length excerpt)
              = excerpt);
            is_true ~msg:"in_order: the cursor is inside the window"
              (cursor - excerpt_offset <= String.length excerpt);
            equal ~msg:"in_order: no occurrence to record" string "None"
              (describe_offset found_at)));
    test "starts_with and ends_with" (fun () ->
        let path = "sessions/ghost/session.json" in
        passes "starts_with: pass" (fun () ->
            Check.starts_with ~affix:"sessions/" path);
        passes "ends_with: pass" (fun () -> Check.ends_with ~affix:".json" path);
        (* The empty affix bounds both ends of every string. *)
        passes "starts_with: empty affix" (fun () ->
            Check.starts_with ~affix:"" path);
        passes "ends_with: empty affix" (fun () ->
            Check.ends_with ~affix:"" path);
        passes "starts_with: the whole string" (fun () ->
            Check.starts_with ~affix:path path);
        (* Absent: same verdict a [contains] would give, because the reason
           is the same. The affix is nowhere in the string. *)
        containment_payload "starts_with: affix absent"
          (fun () -> Check.starts_with ~affix:"users/" path)
          (fun (demand, _, needle, found_at, _, _) ->
            equal ~msg:"the demand is the prefix" string "prefix" demand;
            equal ~msg:"needle is the affix" string "users/" needle;
            is_true ~msg:"no occurrence to report" (found_at = None));
        (* Present but misplaced: the offset is the whole point, and it is
           a report only these verbs can produce; [contains] passes here. *)
        containment_payload "starts_with: affix present elsewhere"
          (fun () -> Check.starts_with ~affix:"ghost" path)
          (fun (_, _, _, found_at, _, _) ->
            is_true ~msg:"the misplaced occurrence is located"
              (found_at = Some 9));
        containment_payload "ends_with: affix present elsewhere"
          (fun () -> Check.ends_with ~affix:"session" path)
          (fun (demand, _, _, found_at, _, _) ->
            equal ~msg:"the demand is the suffix" string "suffix" demand;
            is_true ~msg:"located at its first occurrence" (found_at = Some 0));
        (* A suffix that overruns the string is absent, not a crash. *)
        containment_payload "ends_with: affix longer than the haystack"
          (fun () ->
            Check.ends_with ~affix:"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" "ab")
          (fun (_, _, _, found_at, _, _) ->
            is_true ~msg:"nothing located" (found_at = None));
        (* An absent suffix was demanded at the end: the excerpt is the end
           of the haystack, where the head would show nothing of it. *)
        let lines =
          String.concat ""
            (List.init 40 (fun i -> Printf.sprintf "line %d\n" i))
        in
        containment_payload "ends_with: an absent suffix excerpts the end"
          (fun () -> Check.ends_with ~affix:"end" lines)
          (fun (_, excerpt, _, _, haystack_length, excerpt_offset) ->
            equal ~msg:"the last 10 lines" string
              (String.concat ""
                 (List.init 10 (fun i -> Printf.sprintf "line %d\n" (i + 30))))
              excerpt;
            is_true ~msg:"the window ends the haystack"
              (excerpt_offset + String.length excerpt = haystack_length));
        let long = String.make 3_000 'x' in
        containment_payload "ends_with: a long line excerpts its last 1 KiB"
          (fun () -> Check.ends_with ~affix:"end" long)
          (fun (_, excerpt, _, _, _, excerpt_offset) ->
            is_true ~msg:"the last 1 KiB"
              (excerpt_offset = 3_000 - 1_024 && String.length excerpt = 1_024)));
    test "mem" (fun () ->
        let calls = ref 0 in
        passes "mem: pass" (fun () -> Check.mem (counting_int calls) 2 [ 1; 2 ]);
        is_true ~msg:"mem: pass path never renders" (!calls = 0);
        passes "mem: the witness equality decides, not (=)" (fun () ->
            Check.mem (Testable.float 0.5) 1.0 [ 9.0; 1.2 ]);
        predicate_payload "mem: fail payload"
          (fun () -> Check.mem Testable.int 42 [ 2; 3; 5 ])
          (fun (claim, value) ->
            equal ~msg:"mem: claim names the element" string
              "a list containing 42" claim;
            equal ~msg:"mem: value is the whole list" string "[2; 3; 5]" value);
        predicate_payload "mem: empty list still shows both sides"
          (fun () -> Check.mem Testable.string "a" [])
          (fun (claim, value) ->
            equal ~msg:"mem: claim renders the element with the witness" string
              {|a list containing "a"|} claim;
            equal ~msg:"mem: empty list renders as []" string "[]" value));
    test "is_none and is_some" (fun () ->
        passes "is_none: pass" (fun () -> Check.is_none None);
        passes "is_some: pass" (fun () -> Check.is_some (Some 1));
        (* The point of the verb: no witness is demanded for a type it
           never compares, and the rejected value still prints. *)
        equality_payload "is_none: fail renders Some v with ?pp"
          (fun () -> Check.is_none ~pp:Format.pp_print_int (Some 7))
          (fun (expected, actual, not_) ->
            equal ~msg:"is_none: expected side" string "None" expected;
            equal ~msg:"is_none: actual side names the constructor" string
              "Some 7" actual;
            is_true ~msg:"is_none: not a negated equality" (not not_));
        equality_payload "is_none: fail without ?pp"
          (fun () -> Check.is_none (Some 7))
          (fun (_, actual, _) ->
            equal ~msg:"is_none: rejected value is <abstract>" string
              "Some <abstract>" actual);
        let calls = ref 0 in
        let counting ppf n =
          incr calls;
          Format.pp_print_int ppf n
        in
        passes "is_none: pass path never renders" (fun () ->
            Check.is_none ~pp:counting None);
        is_true ~msg:"is_none: printer stayed unused" (!calls = 0);
        equality_payload "is_some: fail payload"
          (fun () -> Check.is_some (None : int option))
          (fun (expected, actual, _) ->
            (* Same payload as [require_some]'s: one wording for one claim. *)
            equal ~msg:"is_some: expected side" string "Some _" expected;
            equal ~msg:"is_some: actual side" string "None" actual));
    test "satisfies" (fun () ->
        let calls = ref 0 in
        passes "satisfies: pass" (fun () ->
            Check.satisfies (counting_int calls) (fun n -> n > 0) 3);
        is_true ~msg:"satisfies: pass path never renders" (!calls = 0);
        predicate_payload "satisfies: fail payload"
          (fun () -> Check.satisfies Testable.int (fun n -> n > 0) (-4))
          (fun (claim, value) ->
            (* The claim sentence is what tells the predicate verbs apart. *)
            equal ~msg:"satisfies: claim describes the assertion" string
              "value satisfying the predicate" claim;
            equal ~msg:"satisfies: rejected value rendered by the witness"
              string "-4" value);
        let calls = ref 0 in
        ignore
          (outcome (fun () ->
               Check.satisfies (counting_int calls) (fun _ -> false) 9));
        is_true ~msg:"satisfies: fail path renders the value once" (!calls = 1);
        predicate_payload "satisfies: renders with the witness printer"
          (fun () -> Check.satisfies Testable.string (fun _ -> false) "a b")
          (fun (_, value) ->
            equal ~msg:"satisfies: string renders with %S" string {|"a b"|}
              value);
        (* [?claim] is what makes this the comparison assertion: the bound
           stays the claim and the value stays the value, where
           [is_true (n > 0)] could only report true against false. *)
        predicate_payload "satisfies: ~claim replaces the default sentence"
          (fun () ->
            Check.satisfies ~claim:"greater than 0" Testable.int
              (fun n -> n > 0)
              0)
          (fun (claim, value) ->
            equal ~msg:"satisfies: claim is the caller's" string
              "greater than 0" claim;
            equal ~msg:"satisfies: value is the value" string "0" value);
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
            equal ~msg:"string bound is quoted" string {|greater than "m"|}
              claim;
            equal ~msg:"string value is quoted" string {|"a"|} value);
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
            equal ~msg:"satisfies: rendered by the caller's printer" string
              "-7 / 2" value));
    test "less, at_most, greater, at_least" (fun () ->
        (* Bound first, value last: [less int ~than:3 v] is "v < 3". Each
           pair's boundary case is the whole difference between the strict
           verb and its inclusive twin. *)
        passes "less: pass" (fun () -> Check.less Testable.int ~than:3 2);
        passes "at_most: pass below" (fun () ->
            Check.at_most Testable.int ~than:3 2);
        passes "at_most: pass at the bound" (fun () ->
            Check.at_most Testable.int ~than:3 3);
        passes "greater: pass" (fun () -> Check.greater Testable.int ~than:3 4);
        passes "at_least: pass above" (fun () ->
            Check.at_least Testable.int ~than:3 4);
        passes "at_least: pass at the bound" (fun () ->
            Check.at_least Testable.int ~than:3 3);
        (* The claim is derived from the verb and the bound, the value from
           the same witness: [satisfies ~claim]'s shape with nothing for the
           caller to keep in step. *)
        let order_payload name f ~claim ~value =
          predicate_payload name f (fun (actual_claim, actual_value) ->
              equal
                ~msg:(name ^ ": claim is the relation and the bound")
                string claim actual_claim;
              equal
                ~msg:(name ^ ": value is the value")
                string value actual_value)
        in
        order_payload "less: fail at the bound"
          (fun () -> Check.less Testable.int ~than:3 3)
          ~claim:"less than 3" ~value:"3";
        order_payload "at_most: fail"
          (fun () -> Check.at_most Testable.int ~than:3 4)
          ~claim:"at most 3" ~value:"4";
        order_payload "greater: fail at the bound"
          (fun () -> Check.greater Testable.int ~than:3 3)
          ~claim:"greater than 3" ~value:"3";
        order_payload "at_least: fail"
          (fun () -> Check.at_least Testable.int ~than:3 2)
          ~claim:"at least 3" ~value:"2";
        (* Both sides render through the witness, so a string bound is
           quoted like a string value. *)
        order_payload "greater: bound and value render through the witness"
          (fun () -> Check.greater Testable.string ~than:"m" "a")
          ~claim:{|greater than "m"|} ~value:{|"a"|};
        (* Tolerance belongs to equality: under [float 0.5], 1.0 and 1.2 are
           equal and 1.0 is still less than 1.2. NaN sorts below every float,
           as [Float.compare] has it. *)
        let close = Testable.float 0.5 in
        passes "less: a tolerance witness orders exactly" (fun () ->
            Check.less close ~than:1.2 1.0);
        passes "equal: the same pair is equal under the tolerance" (fun () ->
            Check.equal close 1.2 1.0);
        order_payload "at_least: no tolerance on the bound"
          (fun () -> Check.at_least close ~than:1.2 1.0)
          ~claim:"at least 1.2" ~value:"1";
        order_payload "float_rel: orders exactly too"
          (fun () ->
            Check.greater (Testable.float_rel ~rel:0.5 ~abs:0.5) ~than:1.2 1.0)
          ~claim:"greater than 1.2" ~value:"1";
        passes "at_most: nan sorts below every float" (fun () ->
            Check.at_most Testable.float_exact ~than:neg_infinity Float.nan);
        order_payload "at_least: nan is at least nothing"
          (fun () ->
            Check.at_least Testable.float_exact ~than:neg_infinity Float.nan)
          ~claim:"at least -inf" ~value:"nan";
        (* The witness's equality is never consulted: an always-raising
           equality is fine on both paths. *)
        let explosive =
          Testable.make ~pp:Format.pp_print_int ~equal:(fun _ _ -> assert false)
          |> Testable.with_compare Int.compare
        in
        passes "less: witness equality is not consulted" (fun () ->
            Check.less explosive ~than:2 1);
        order_payload "less: fails through the given order only"
          (fun () -> Check.less explosive ~than:1 2)
          ~claim:"less than 1" ~value:"2";
        (* A witness without an order is a programmer error, and it surfaces
           whether or not the assertion would have held, on the first run,
           not the first failure. *)
        let no_order verb f =
          is_true
            ~msg:(verb ^ ": no order raises Invalid_argument naming the fix")
            (match outcome f with
            | Raised (Invalid_argument m) ->
                String.starts_with ~prefix:("Windtrap." ^ verb ^ ":") m
                && Windtrap.Private.Text.contains_substring
                     ~pattern:"Testable.with_compare" m
            | _ -> false)
        in
        let unordered =
          Testable.make ~pp:Format.pp_print_int ~equal:Int.equal
        in
        no_order "less" (fun () -> Check.less unordered ~than:3 2);
        no_order "at_most" (fun () -> Check.at_most unordered ~than:3 4);
        no_order "greater" (fun () -> Check.greater unordered ~than:3 4);
        no_order "at_least" (fun () -> Check.at_least unordered ~than:3 2);
        no_order "less" (fun () ->
            Check.less (Testable.list Testable.int) ~than:[] []);
        no_order "less" (fun () -> Check.less Testable.pass ~than:1 0);
        (* The pass path never renders, and the fail path renders the bound
           and the value once each. *)
        let calls = ref 0 in
        passes "less: pass path never renders" (fun () ->
            Check.less
              (counting_int calls |> Testable.with_compare Int.compare)
              ~than:3 2);
        is_true ~msg:"less: printer stayed unused" (!calls = 0);
        ignore
          (outcome (fun () ->
               Check.less
                 (counting_int calls |> Testable.with_compare Int.compare)
                 ~than:3 3));
        is_true ~msg:"less: fail path renders each side once" (!calls = 2));
    test "require_some, require_ok, require_error" (fun () ->
        is_true ~msg:"require_some: unwraps the payload"
          (Check.require_some (Some 42) = 42);
        equality_payload "require_some: fail payload"
          (fun () -> ignore (Check.require_some None))
          (fun (expected, actual, _) ->
            is_true ~msg:"require_some: payload is Some _ vs None"
              (expected = "Some _" && actual = "None"));
        is_true ~msg:"require_ok: unwraps the payload"
          (Check.require_ok (Ok 7) = 7);
        equality_payload "require_ok: fail payload without pp"
          (fun () -> ignore (Check.require_ok (Error 3)))
          (fun (expected, actual, _) ->
            equal ~msg:"require_ok: expected side" string "Ok _" expected;
            equal ~msg:"require_ok: rejected side prints <abstract>" string
              "Error <abstract>" actual);
        equality_payload "require_ok: pp renders the rejected side"
          (fun () ->
            ignore (Check.require_ok ~pp:Format.pp_print_int (Error 3)))
          (fun (_, actual, _) ->
            equal ~msg:"require_ok: rendered error" string "Error 3" actual);
        let calls = ref 0 in
        let pp ppf n =
          incr calls;
          Format.pp_print_int ppf n
        in
        is_true ~msg:"require_ok: pp not called on Ok"
          (Check.require_ok ~pp (Ok 1) = 1 && !calls = 0);
        is_true ~msg:"require_error: unwraps the payload"
          (Check.require_error (Error "e") = "e");
        equality_payload "require_error: fail payload without pp"
          (fun () -> ignore (Check.require_error (Ok 9)))
          (fun (expected, actual, _) ->
            equal ~msg:"require_error: expected side" string "Error _" expected;
            equal ~msg:"require_error: rejected side prints <abstract>" string
              "Ok <abstract>" actual);
        equality_payload "require_error: pp renders the rejected side"
          (fun () ->
            ignore (Check.require_error ~pp:Format.pp_print_int (Ok 9)))
          (fun (_, actual, _) ->
            equal ~msg:"require_error: rendered ok" string "Ok 9" actual));
    test "is_ok, is_error" (fun () ->
        (* The assert-only twins: the unwrapping verbs' payloads, exactly. *)
        Check.is_ok (Ok 7);
        Check.is_error (Error "e");
        equality_payload "is_ok: fail payload without pp"
          (fun () -> Check.is_ok (Error 3))
          (fun (expected, actual, _) ->
            equal ~msg:"is_ok: expected side" string "Ok _" expected;
            equal ~msg:"is_ok: rejected side prints <abstract>" string
              "Error <abstract>" actual);
        equality_payload "is_ok: pp renders the rejected side"
          (fun () -> Check.is_ok ~pp:Format.pp_print_int (Error 3))
          (fun (_, actual, _) ->
            equal ~msg:"is_ok: rendered error" string "Error 3" actual);
        equality_payload "is_error: fail payload without pp"
          (fun () -> Check.is_error (Ok 9))
          (fun (expected, actual, _) ->
            equal ~msg:"is_error: expected side" string "Error _" expected;
            equal ~msg:"is_error: rejected side prints <abstract>" string
              "Ok <abstract>" actual);
        equality_payload "is_error: pp renders the rejected side"
          (fun () -> Check.is_error ~pp:Format.pp_print_int (Ok 9))
          (fun (_, actual, _) ->
            equal ~msg:"is_error: rendered ok" string "Ok 9" actual);
        let calls = ref 0 in
        let pp ppf n =
          incr calls;
          Format.pp_print_int ppf n
        in
        Check.is_ok ~pp (Ok 1);
        Check.is_error ~pp (Error 1);
        is_true ~msg:"is_ok/is_error: pp not called on the wanted branch"
          (!calls = 0));
    test "require_match" (fun () ->
        is_true ~msg:"require_match: unwraps the matched payload"
          (Check.require_match tcp (`Tcp 8080) = 8080);
        predicate_payload "require_match: fail payload without pp"
          (fun () -> ignore (Check.require_match tcp (`Unix "/tmp/sock")))
          (fun (claim, value) ->
            (* The claim sentence is what tells the predicate verbs apart. *)
            equal ~msg:"require_match: claim describes the assertion" string
              "a match" claim;
            equal ~msg:"require_match: scrutinee prints <abstract> without pp"
              string "<abstract>" value);
        let pp ppf = function
          | `Tcp p -> Format.fprintf ppf "tcp:%d" p
          | `Unix path -> Format.fprintf ppf "unix:%s" path
        in
        predicate_payload "require_match: pp renders the scrutinee"
          (fun () -> ignore (Check.require_match ~pp tcp (`Unix "/tmp/sock")))
          (fun (_, value) ->
            equal ~msg:"require_match: rendered scrutinee" string
              "unix:/tmp/sock" value);
        let calls = ref 0 in
        let pp ppf n =
          incr calls;
          Format.pp_print_int ppf n
        in
        is_true ~msg:"require_match: pp not called on a match"
          (Check.require_match ~pp (fun n -> if n > 0 then Some n else None) 7
           = 7
          && !calls = 0);
        (* The extractor runs under no guard: its exceptions are the test's
           own bug, not a failed match. *)
        is_true
          ~msg:"require_match: an exception from the extractor propagates raw"
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
        is_true ~msg:"require_match composes with require_error (oauth2 shape)"
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
            equal ~msg:"require_match: composed failure keeps the match claim"
              string "a match" claim;
            equal
              ~msg:"require_match: composed scrutinee is abstract without pp"
              string "<abstract>" value));
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
            equal ~msg:"raises: the expected exception rendered" (option string)
              (Some (Printexc.to_string (Payload (1, "x"))))
              expected;
            equal ~msg:"raises: the raised exception rendered" (option string)
              (Some (Printexc.to_string (Payload (1, "y"))))
              actual);
        (* Structural comparison cannot see through functional payloads: the
           compare raises and propagates raw, never a silent pass, never a
           "wrong exception" misreport. The .mli points such cases at
           [raises_match]. *)
        is_true ~msg:"raises: non-comparable payload raises Invalid_argument"
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
            is_true ~msg:"raises: expected exception recorded"
              (expected = Some "Not_found");
            is_true ~msg:"raises: actual absent when nothing raised"
              (actual = None);
            is_true ~msg:"raises: no backtrace when nothing raised"
              (backtrace = None));
        raise_payload "raises: wrong exception"
          (fun () ->
            Check.raises Not_found (fun () -> raise (Payload (0, "z"))))
          (fun (expected, actual, _) ->
            is_true ~msg:"raises: expected rendered"
              (expected = Some "Not_found");
            equal ~msg:"raises: raised exception rendered" (option string)
              (Some (Printexc.to_string (Payload (0, "z"))))
              actual));
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
                is_true ~msg:"raises: backtrace absent with recording off"
                  (backtrace = None));
            Printexc.record_backtrace true;
            raise_payload "raises: backtrace captured when recording is on"
              (fun () ->
                Check.raises Not_found (fun () -> raise (Payload (2, "b"))))
              (fun (_, _, backtrace) ->
                is_true ~msg:"raises: backtrace present with recording on"
                  (match backtrace with
                  | Some s -> String.length s > 0
                  | None -> false));
            (* [Check.raises] catches the exception, so the frames below the
               thunk are windtrap's own, the trailing run
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
                is_true ~msg:"raises: backtrace is non-empty" (lines <> []);
                is_true ~msg:"raises: no windtrap frame survives"
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
        | {
         F.kind =
           F.Equality
             { expected = { F.kept = "1"; _ }; actual = { F.kept = "2"; _ }; _ };
         _;
        } ->
            ()
        | _ -> fail "raises: inner assertion failure survives unchanged");
        is_true ~msg:"raises: inner Skip_test propagates"
          (match
             Check.raises Not_found (fun () -> Check.skip ~reason:"r" ())
           with
          | () -> false
          | exception F.Control (`Skip (Some "r")) -> true
          | exception _ -> false);
        is_true ~msg:"raises: inner Timeout propagates"
          (match
             Check.raises Not_found (fun () -> raise (F.Control (`Timeout 2.5)))
           with
          | () -> false
          | exception F.Control (`Timeout 2.5) -> true
          | exception _ -> false);
        (* Same guard, same order, in raises_match: the accept-all predicate
           never sees the control exceptions. *)
        is_true ~msg:"raises_match: inner Skip_test propagates"
          (match
             Check.raises_match
               (fun _ -> true)
               (fun () -> Check.skip ~reason:"r" ())
           with
          | () -> false
          | exception F.Control (`Skip (Some "r")) -> true
          | exception _ -> false);
        is_true ~msg:"raises_match: inner Timeout propagates"
          (match
             Check.raises_match
               (fun _ -> true)
               (fun () -> raise (F.Control (`Timeout 0.1)))
           with
          | () -> false
          | exception F.Control (`Timeout 0.1) -> true
          | exception _ -> false);
        (* The guard fires before the predicate: even an accept-all
           predicate cannot swallow an inner assertion failure. *)
        match
          caught "raises_match: inner Check_failure propagates" (fun () ->
              Check.raises_match (fun _ -> true) (fun () -> Check.fail "inner"))
        with
        | { F.kind = F.Message { F.kept = "inner"; _ }; _ } -> ()
        | _ -> fail "raises_match: inner failure survives unchanged");
    test "raises_match" (fun () ->
        passes "raises_match: pass on a matching exception" (fun () ->
            Check.raises_match
              (function Payload (1, _) -> true | _ -> false)
              (fun () -> raise (Payload (1, "any"))));
        raise_payload "raises_match: nothing raised"
          (fun () -> Check.raises_match (fun _ -> true) (fun () -> ()))
          (fun (expected, actual, backtrace) ->
            is_true ~msg:"raises_match: no expected rendering for a predicate"
              (expected = None);
            is_true ~msg:"raises_match: actual absent when nothing raised"
              (actual = None);
            is_true ~msg:"raises_match: no backtrace when nothing raised"
              (backtrace = None));
        raise_payload "raises_match: predicate rejects"
          (fun () ->
            Check.raises_match
              (function Payload (9, _) -> true | _ -> false)
              (fun () -> raise (Payload (1, "no"))))
          (fun (expected, actual, _) ->
            is_true ~msg:"raises_match: expected side stays absent"
              (expected = None);
            is_true ~msg:"raises_match: rejected exception rendered"
              (match actual with Some _ -> true | None -> false)));
    test "raises: the message diff" (fun () ->
        raise_message_diff "raises: same constructor, different message"
          (fun () ->
            Check.raises (Invalid_argument "index 3") (fun () ->
                invalid_arg "index 4"))
          (fun diff ->
            equal ~msg:"raises: the diff names the shared constructor" string
              {|Invalid_argument: "index 3" -> "index 4"|}
              (describe_message_diff diff));
        raise_message_diff "raises: two Failures"
          (fun () ->
            Check.raises (Stdlib.Failure "port 80") (fun () ->
                failwith "port 81"))
          (fun diff ->
            equal ~msg:"raises: Failure messages are diffed" string
              {|Failure: "port 80" -> "port 81"|}
              (describe_message_diff diff));
        raise_message_diff "raises: two Sys_errors"
          (fun () ->
            Check.raises (Sys_error "a: denied") (fun () ->
                raise (Sys_error "b: denied")))
          (fun diff ->
            equal ~msg:"raises: Sys_error messages are diffed" string
              {|Sys_error: "a: denied" -> "b: denied"|}
              (describe_message_diff diff));
        raise_message_diff "raises: different constructors"
          (fun () -> Check.raises Not_found (fun () -> failwith "boom"))
          (fun diff ->
            equal ~msg:"raises: constructors that differ have no message diff"
              string "none"
              (describe_message_diff diff));
        raise_message_diff "raises: same user constructor, non-string payload"
          (fun () ->
            Check.raises (Payload (1, "x")) (fun () -> raise (Payload (1, "y"))))
          (fun diff ->
            equal ~msg:"raises: no diff without extractable messages" string
              "none"
              (describe_message_diff diff));
        raise_message_diff "raises: nothing raised"
          (fun () -> Check.raises (Stdlib.Failure "boom") (fun () -> 1))
          (fun diff ->
            equal ~msg:"raises: nothing raised leaves nothing to diff" string
              "none"
              (describe_message_diff diff));
        raise_message_diff "raises_match: rejected exception"
          (fun () ->
            Check.raises_match
              (fun _ -> false)
              (fun () -> raise (Sys_error "no such file")))
          (fun diff ->
            equal ~msg:"raises_match: a predicate has no expected side to diff"
              string "none"
              (describe_message_diff diff)));
    test "raises sets predicate false, raises_match sets it true" (fun () ->
        let predicate name f =
          match caught name f with
          | { F.kind = F.Raise { predicate; _ }; _ } -> predicate
          | _ -> fail (name ^ ": kind is Raise")
        in
        is_false ~msg:"raises, nothing raised"
          (predicate "raises" (fun () -> Check.raises Not_found ignore));
        is_false ~msg:"raises, another exception"
          (predicate "raises" (fun () ->
               Check.raises Not_found (fun () -> raise Exit)));
        is_true ~msg:"raises_match, nothing raised"
          (predicate "raises_match" (fun () ->
               Check.raises_match (fun _ -> true) ignore));
        is_true ~msg:"raises_match, a rejected exception"
          (predicate "raises_match" (fun () ->
               Check.raises_match (fun _ -> false) (fun () -> raise Exit))));
    (* Only an exception of the user's own is compared: every control, and
       an interrupt or an exhausted resource, passes through both verbs. A
       predicate that accepts everything cannot hide an intercepted [exit]. *)
    test "raises and raises_match pass exit, discard and fatal exceptions"
      (fun () ->
        List.iter
          (fun (name, e) ->
            let passes_through verb f =
              match outcome f with
              | Raised raised ->
                  is_true ~msg:(name ^ ": " ^ verb ^ " passes it") (raised = e)
              | Returned | Failed _ -> fail (name ^ ": " ^ verb ^ " consumed it")
            in
            passes_through "raises" (fun () ->
                Check.raises e (fun () -> raise e));
            passes_through "raises of another" (fun () ->
                Check.raises Not_found (fun () -> raise e));
            passes_through "raises_match accepting all" (fun () ->
                Check.raises_match (fun _ -> true) (fun () -> raise e));
            passes_through "raises_match rejecting all" (fun () ->
                Check.raises_match (fun _ -> false) (fun () -> raise e)))
          [
            ("Exit_attempt", F.Control `Exit);
            ("Discard", F.Control `Discard);
            ("Sys.Break", Sys.Break);
            ("Out_of_memory", Out_of_memory);
          ];
        passes "a Stack_overflow is compared as any exception" (fun () ->
            Check.raises Stack_overflow (fun () -> raise Stack_overflow)));
    test "an exception from a witness, a ?pp or a predicate escapes the verb"
      (fun () ->
        let escapes name f =
          match outcome f with
          | Raised Extractor_bug -> ()
          | Raised e -> fail (name ^ ": raised " ^ Printexc.to_string e)
          | Returned -> fail (name ^ ": returned")
          | Failed _ -> fail (name ^ ": became a Check_failure")
        in
        let bad_equal =
          Testable.make ~pp:Format.pp_print_int ~equal:(fun _ _ ->
              raise Extractor_bug)
        in
        let bad_pp =
          Testable.make ~pp:(fun _ _ -> raise Extractor_bug) ~equal:Int.equal
          |> Testable.with_compare Int.compare
        in
        let bad_order =
          Testable.with_compare (fun _ _ -> raise Extractor_bug) Testable.int
        in
        let raising_pp _ _ = raise Extractor_bug in
        escapes "equal: the equality" (fun () -> Check.equal bad_equal 1 1);
        escapes "not_equal: the equality" (fun () ->
            Check.not_equal bad_equal 1 2);
        escapes "mem: the equality" (fun () -> Check.mem bad_equal 1 [ 1 ]);
        escapes "equal: the printer" (fun () -> Check.equal bad_pp 1 2);
        escapes "less: the order" (fun () -> Check.less bad_order ~than:2 1);
        escapes "less: the printer" (fun () -> Check.less bad_pp ~than:1 2);
        escapes "satisfies: the predicate" (fun () ->
            Check.satisfies Testable.int (fun _ -> raise Extractor_bug) 1);
        escapes "satisfies: the printer" (fun () ->
            Check.satisfies bad_pp (fun _ -> false) 1);
        escapes "is_none: the ?pp" (fun () ->
            Check.is_none ~pp:raising_pp (Some 1));
        escapes "require_ok: the ?pp" (fun () ->
            ignore (Check.require_ok ~pp:raising_pp (Error 1)));
        escapes "raises_match: the predicate" (fun () ->
            Check.raises_match
              (fun _ -> raise Extractor_bug)
              (fun () -> raise Exit)));
    test "mem puts the element on the expected side" (fun () ->
        let calls = ref [] in
        let w =
          Testable.make ~pp:Format.pp_print_string ~equal:(fun a b ->
              calls := (a, b) :: !calls;
              false)
        in
        ignore (outcome (fun () -> Check.mem w "x" [ "a"; "b" ]));
        equal ~msg:"(x, element) at each call"
          (list (pair string string))
          [ ("x", "b"); ("x", "a") ]
          !calls);
    test "in_order refuses no needles, whatever the haystack" (fun () ->
        List.iter
          (fun s ->
            raises_match ~msg:(Printf.sprintf "%S" s) Exn.invalid_arg (fun () ->
                Check.in_order ~subs:[] s))
          [ ""; "abc" ]);
    test "each containment verb states its demand" (fun () ->
        List.iter
          (fun (name, f, expected) ->
            containment_demand name f (fun (demand, _, _, _) ->
                equal ~msg:name string expected (describe_demand demand)))
          [
            ( "not_contains",
              (fun () -> Check.not_contains ~sub:"a" "abc"),
              "anywhere" );
            ( "starts_with",
              (fun () -> Check.starts_with ~affix:"z" "abc"),
              "prefix" );
            ("ends_with", (fun () -> Check.ends_with ~affix:"z" "abc"), "suffix");
          ]);
    test "Exn predicates" (fun () ->
        is_true ~msg:"Exn.invalid_arg: matches the constructor"
          (Check.Exn.invalid_arg (Invalid_argument "x"));
        is_true ~msg:"Exn.invalid_arg: rejects other exceptions"
          (not (Check.Exn.invalid_arg (Stdlib.Failure "x")));
        is_true ~msg:"Exn.invalid_arg: rejects payloadless exceptions"
          (not (Check.Exn.invalid_arg Not_found));
        is_true ~msg:"Exn.failure: matches the constructor"
          (Check.Exn.failure (Stdlib.Failure "x"));
        is_true
          ~msg:
            "Exn.failure: rejects Invalid_argument even with a matching message"
          (not (Check.Exn.failure (Invalid_argument "x")));
        (* The third message-carrying exception [raises] diffs by message:
           [Exn] covering only two of the three was arbitrary. *)
        is_true ~msg:"Exn.sys_error: matches the constructor"
          (Check.Exn.sys_error (Sys_error "x"));
        is_true ~msg:"Exn.sys_error: rejects other exceptions"
          (not (Check.Exn.sys_error (Stdlib.Failure "x")));
        is_true ~msg:"Exn.sys_error: ~substring matches inside the message"
          (Check.Exn.sys_error ~substring:"No such file"
             (Sys_error "nope.txt: No such file or directory"));
        is_true ~msg:"Exn.invalid_arg: ~substring matches inside the message"
          (Check.Exn.invalid_arg ~substring:"unhandled op"
             (Invalid_argument "step: unhandled op HALT"));
        is_true ~msg:"Exn.invalid_arg: ~substring rejects a missing needle"
          (not
             (Check.Exn.invalid_arg ~substring:"overflow" (Invalid_argument "x")));
        is_true ~msg:"Exn.invalid_arg: empty ~substring matches any message"
          (Check.Exn.invalid_arg ~substring:"" (Invalid_argument ""));
        (* The composition the predicates exist for. *)
        passes "raises_match composes with Exn.invalid_arg" (fun () ->
            Check.raises_match (Check.Exn.invalid_arg ~substring:"unhandled op")
              (fun () -> invalid_arg "step: unhandled op HALT"));
        raise_payload "raises_match rejects via Exn.failure ~substring"
          (fun () ->
            Check.raises_match (Check.Exn.failure ~substring:"underflow")
              (fun () -> failwith "overflow"))
          (fun (_, actual, _) ->
            is_true ~msg:"raises_match + Exn: rejected exception rendered"
              (actual <> None)));
    test "fail, failf, skip" (fun () ->
        (match
           caught "fail: raises a Message failure" (fun () -> Check.fail "boom")
         with
        | { F.kind = F.Message { F.kept = "boom"; _ }; msg = None; _ } -> ()
        | _ -> fail "fail: message stored, no msg annotation");
        is_true ~msg:"fail: usable in expression position"
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
            equal ~msg:"failf: formatted payload" string "bad value 42" m.F.kept
        | _ -> fail "failf: kind is Message");
        is_true ~msg:"failf: usable in expression position"
          (match
             outcome (fun () ->
                 let n : int = if true then Check.failf "no %d" 7 else 3 in
                 ignore n)
           with
          | Failed { F.kind = F.Message { F.kept = "no 7"; _ }; _ } -> true
          | _ -> false);
        is_true ~msg:"skip: raises Skip_test with the reason"
          (match Check.skip ~reason:"needs docker" () with
          | _ -> false
          | exception F.Control (`Skip (Some "needs docker")) -> true
          | exception _ -> false);
        is_true ~msg:"skip: reason defaults to None"
          (match Check.skip () with
          | _ -> false
          | exception F.Control (`Skip None) -> true
          | exception _ -> false));
    (* Every verb of [Check] that fails with a location, each made to fail
       once. [skip] is the one verb without a site: a skip is not a failure. *)
    test "?__POS__ wins over the captured location on every verb" (fun () ->
        let p = fake_pos in
        List.iter
          (fun (name, f) ->
            let fl = caught name f in
            is_true ~msg:(name ^ ": ?__POS__ wins") (fl.F.loc = Some fake_loc))
          [
            ("equal", fun () -> Check.equal ~__POS__:p Testable.int 1 2);
            ("not_equal", fun () -> Check.not_equal ~__POS__:p Testable.int 1 1);
            ("is_true", fun () -> Check.is_true ~__POS__:p false);
            ("is_false", fun () -> Check.is_false ~__POS__:p true);
            ("is_none", fun () -> Check.is_none ~__POS__:p (Some 1));
            ("is_some", fun () -> Check.is_some ~__POS__:p None);
            ("is_ok", fun () -> Check.is_ok ~__POS__:p (Error ()));
            ("is_error", fun () -> Check.is_error ~__POS__:p (Ok ()));
            ( "require_some",
              fun () -> ignore (Check.require_some ~__POS__:p None) );
            ( "require_ok",
              fun () -> ignore (Check.require_ok ~__POS__:p (Error ())) );
            ( "require_error",
              fun () -> ignore (Check.require_error ~__POS__:p (Ok ())) );
            ( "require_match",
              fun () ->
                ignore (Check.require_match ~__POS__:p (fun _ -> None) 1) );
            ( "satisfies",
              fun () ->
                Check.satisfies ~__POS__:p Testable.int (fun _ -> false) 1 );
            ("mem", fun () -> Check.mem ~__POS__:p Testable.int 3 [ 1; 2 ]);
            ("less", fun () -> Check.less ~__POS__:p Testable.int ~than:1 1);
            ( "at_most",
              fun () -> Check.at_most ~__POS__:p Testable.int ~than:1 2 );
            ( "greater",
              fun () -> Check.greater ~__POS__:p Testable.int ~than:1 1 );
            ( "at_least",
              fun () -> Check.at_least ~__POS__:p Testable.int ~than:1 0 );
            ("contains", fun () -> Check.contains ~__POS__:p ~sub:"z" "abc");
            ( "not_contains",
              fun () -> Check.not_contains ~__POS__:p ~sub:"a" "abc" );
            ( "starts_with",
              fun () -> Check.starts_with ~__POS__:p ~affix:"z" "abc" );
            ("ends_with", fun () -> Check.ends_with ~__POS__:p ~affix:"z" "abc");
            ( "in_order",
              fun () -> Check.in_order ~__POS__:p ~subs:[ "b"; "a" ] "abc" );
            ( "raises",
              fun () -> Check.raises ~__POS__:p Not_found (fun () -> ()) );
            ( "raises_match",
              fun () -> Check.raises_match ~__POS__:p (fun _ -> false) ignore );
            ("fail", fun () -> Check.fail ~__POS__:p "x");
            ("failf", fun () -> Check.failf ~__POS__:p "x %d" 1);
          ]);
    test "default location is captured from the call stack" (fun () ->
        (* Without ?__POS__ the location is captured from the call stack and
           points at this file (user code, not windtrap's frames). *)
        (match
           caught "equal: default location" (fun () ->
               Check.equal Testable.int 1 2)
         with
        | { F.loc = Some loc; _ } ->
            equal ~msg:"equal: captured location is the caller's file" string
              "test_check.ml"
              (Filename.basename loc.Loc.file)
        | _ -> fail "equal: default location captured");
        (* The deepest indirection: failf raises from a kasprintf
           continuation, with Format machinery between the call site and the
           capture. The heuristic must still attribute to this file. *)
        match
          caught "failf: default location" (fun () -> Check.failf "boom %d" 1)
        with
        | { F.loc = Some loc; _ } ->
            equal ~msg:"failf: captured location is the caller's file" string
              "test_check.ml"
              (Filename.basename loc.Loc.file)
        | _ -> fail "failf: default location captured");
    (* Every verb of [Check] that takes [?msg]: all but [fail], [failf] and
       [skip], whose message is their argument. *)
    test "?msg propagates on every verb" (fun () ->
        let m = "why" in
        List.iter
          (fun (name, f) ->
            equal ~msg:(name ^ ": ?msg stored") (option string) (Some m)
              (Option.map (fun (t : F.text) -> t.kept) (caught name f).F.msg))
          [
            ("equal", fun () -> Check.equal ~msg:m Testable.int 1 2);
            ("not_equal", fun () -> Check.not_equal ~msg:m Testable.int 1 1);
            ("is_true", fun () -> Check.is_true ~msg:m false);
            ("is_false", fun () -> Check.is_false ~msg:m true);
            ("is_none", fun () -> Check.is_none ~msg:m (Some 1));
            ("is_some", fun () -> Check.is_some ~msg:m None);
            ("is_ok", fun () -> Check.is_ok ~msg:m (Error ()));
            ("is_error", fun () -> Check.is_error ~msg:m (Ok ()));
            ("require_some", fun () -> ignore (Check.require_some ~msg:m None));
            ("require_ok", fun () -> ignore (Check.require_ok ~msg:m (Error ())));
            ( "require_error",
              fun () -> ignore (Check.require_error ~msg:m (Ok ())) );
            ( "require_match",
              fun () -> ignore (Check.require_match ~msg:m (fun _ -> None) 1) );
            ( "satisfies",
              fun () -> Check.satisfies ~msg:m Testable.int (fun _ -> false) 1
            );
            ("mem", fun () -> Check.mem ~msg:m Testable.int 3 [ 1; 2 ]);
            ("less", fun () -> Check.less ~msg:m Testable.int ~than:1 1);
            ("at_most", fun () -> Check.at_most ~msg:m Testable.int ~than:1 2);
            ("greater", fun () -> Check.greater ~msg:m Testable.int ~than:1 1);
            ("at_least", fun () -> Check.at_least ~msg:m Testable.int ~than:1 0);
            ("contains", fun () -> Check.contains ~msg:m ~sub:"z" "abc");
            ("not_contains", fun () -> Check.not_contains ~msg:m ~sub:"a" "abc");
            ("starts_with", fun () -> Check.starts_with ~msg:m ~affix:"z" "abc");
            ("ends_with", fun () -> Check.ends_with ~msg:m ~affix:"z" "abc");
            ( "in_order",
              fun () -> Check.in_order ~msg:m ~subs:[ "b"; "a" ] "abc" );
            ("raises", fun () -> Check.raises ~msg:m Not_found (fun () -> ()));
            ( "raises_match",
              fun () -> Check.raises_match ~msg:m (fun _ -> false) ignore );
          ]);
  ]

let () = exit @@ Windtrap.run "check" tests
