(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Failure: typed kinds, defaults, payload bounding, tails,
   outcomes, and the control exceptions. *)

open Windtrap
module F = Windtrap.Private.Failure
module Loc = Windtrap.Private.Loc

let has ~needle haystack =
  Windtrap.Private.Text.contains_substring ~pattern:needle haystack

let loc_of file line = Loc.of_pos (file, line, 0, 0)

(* Top-level and never inlined, so the backtrace-trimming test can name each
   frame of this file. [through_delimit] puts a windtrap frame between two
   of them, which is the interior run the trim must not touch. *)
let[@inline never] raise_not_found () = raise Not_found

let[@inline never] through_delimit () =
  Loc.delimit (fun () -> raise_not_found ())

(* The deepest frame: the last non-empty line, since a rendered backtrace
   ends with a newline. *)
let last_line s =
  let rec first_nonempty = function
    | "" :: rest -> first_nonempty rest
    | line :: _ -> line
    | [] -> ""
  in
  first_nonempty (List.rev (Windtrap.Private.Text.split_lines s))

(* [containment_parts f k] projects a containment failure's shape: the
   stored excerpt and the haystack bookkeeping. *)
let containment_parts name (f : F.t) k =
  match f.F.kind with
  | F.Containment
      { needle; found_at; haystack_length; excerpt; excerpt_offset; _ } ->
      k (excerpt, needle, found_at, haystack_length, excerpt_offset)
  | _ -> is_true ~msg:(name ^ ": Containment kind") false

let big = String.make 200_000 'a'

let big_haystack =
  String.concat "" (List.init 2_500 (fun i -> Printf.sprintf "%09d\n" i))

let tests =
  [
    test "equality constructor: defaults" (fun () ->
        let f = F.equality ~expected:"1" ~actual:"2" () in
        is_true ~msg:"kind payload"
          (match f.F.kind with
          | F.Equality
              { expected = "1"; actual = "2"; not_ = false; diffable = true } ->
              true
          | _ -> false);
        is_true ~msg:"default phase is Body" (f.F.phase = F.Body);
        is_true ~msg:"default loc is None" (f.F.loc = None);
        is_true ~msg:"default msg is None" (f.F.msg = None);
        is_true ~msg:"default output_tail is None" (f.F.output_tail = None));
    test "equality constructor: loc, msg, not_ stored" (fun () ->
        let loc = loc_of "test/t.ml" 12 in
        let f =
          F.equality ~loc ~msg:"ids" ~not_:true ~expected:"3" ~actual:"3" ()
        in
        is_true ~msg:"not_ recorded"
          (match f.F.kind with
          | F.Equality { not_ = true; _ } -> true
          | _ -> false);
        is_true ~msg:"loc stored" (f.F.loc = Some loc);
        is_true ~msg:"msg stored" (f.F.msg = Some "ids"));
    test "predicate constructor" (fun () ->
        let loc = loc_of "test/t.ml" 7 in
        let f = F.predicate ~loc ~msg:"positive" ~claim:"a match" "None" in
        (* The claim takes the expected side, and [diffable] is what stops a
           renderer refining a description against a value. *)
        is_true ~msg:"claim and value stored"
          (match f.F.kind with
          | F.Equality
              { expected = "a match"; actual = "None"; diffable = false; _ } ->
              true
          | _ -> false);
        is_true ~msg:"loc stored" (f.F.loc = Some loc);
        is_true ~msg:"msg stored" (f.F.msg = Some "positive");
        let f = F.predicate ~claim:big big in
        is_true ~msg:"claim and value are bounded"
          (match f.F.kind with
          | F.Equality { expected; actual; diffable = false; _ } ->
              String.length expected < 200_000
              && String.length actual < 200_000
              && has ~needle:"truncated" actual
          | _ -> false));
    test "payload bounding" (fun () ->
        (let f = F.equality ~expected:big ~actual:"2" () in
         match f.F.kind with
         | F.Equality { expected; actual = "2"; _ } ->
             is_true ~msg:"long payload is shorter than the original"
               (String.length expected < String.length big);
             is_true ~msg:"marker states the original byte count"
               (has ~needle:"truncated" expected
               && has ~needle:"200000 bytes" expected);
             is_true ~msg:"truncation keeps a prefix of the value"
               (String.length expected > 1_000 && expected.[0] = 'a')
         | _ -> is_true ~msg:"kind preserved" false);
        (* All-2-byte content: any code-point boundary is an even offset, so
           an odd-length kept prefix would mean a split UTF-8 sequence. *)
        (let s = String.concat "" (List.init 100_000 (fun _ -> "\xc3\xa9")) in
         let f = F.message s in
         match f.F.kind with
         | F.Message text -> (
             let rec marker_index i =
               if i + 3 > String.length text then None
               else if String.sub text i 3 = "..." then Some i
               else marker_index (i + 1)
             in
             match marker_index 0 with
             | None -> is_true ~msg:"utf-8 payload has a marker" false
             | Some i ->
                 is_true ~msg:"never splits a UTF-8 sequence" (i mod 2 = 0))
         | _ -> is_true ~msg:"message kind preserved" false);
        (let f = F.equality ~msg:big ~expected:"1" ~actual:"2" () in
         match f.F.msg with
         | Some msg ->
             is_true ~msg:"msg is bounded too" (String.length msg < 200_000)
         | None -> is_true ~msg:"msg kept" false);
        let f =
          F.raised ~expected:big ~actual:big ~backtrace:big
            ~message_diff:
              {
                F.constructor = big;
                expected_message = big;
                actual_message = big;
              }
            ()
        in
        is_true ~msg:"raise payloads are bounded"
          (match f.F.kind with
          | F.Raise
              {
                expected = Some e;
                actual = Some a;
                backtrace = Some b;
                message_diff =
                  Some { F.constructor; expected_message; actual_message };
                _;
              } ->
              String.length e < 200_000
              && String.length a < 200_000
              && String.length b < 200_000
              && String.length constructor < 200_000
              && String.length expected_message < 200_000
              && String.length actual_message < 200_000
          | _ -> false);
        let f =
          F.property ~rendered:big ~case_index:0 ~shrink_steps:0 ~root:1L
            ~examples:false ()
        in
        is_true ~msg:"property counterexample is bounded"
          (match f.F.kind with
          | F.Property { rendered; _ } -> String.length rendered < 200_000
          | _ -> false));
    test "a text is bounded at 65,536 bytes" (fun () ->
        let stored text =
          match (F.message text).F.kind with
          | F.Message stored -> stored
          | _ -> fail "message kind"
        in
        let at_bound = String.make 65_536 'a' in
        equal ~msg:"65,536 bytes are stored as given" string at_bound
          (stored at_bound);
        equal ~msg:"65,537 bytes keep 65,536 and the marker" string
          (at_bound ^ "... (truncated; 65537 bytes total)")
          (stored (at_bound ^ "b")));
    test "texts equal on their first 64 KiB with one length are stored equal"
      (fun () ->
        let head = String.make 65_536 'a' in
        let a = F.message (head ^ "xyz") and b = F.message (head ^ "XYZ") in
        is_true ~msg:"the two payloads are equal" (a.F.kind = b.F.kind));
    test "an empty backtrace is stored as none" (fun () ->
        let f = F.raised ~backtrace:"" () in
        is_true ~msg:"backtrace = None"
          (match f.F.kind with
          | F.Raise { backtrace = None; _ } -> true
          | _ -> false));
    test "raise constructor" (fun () ->
        let f = F.raised () in
        is_true ~msg:"all payloads default to absent"
          (match f.F.kind with
          | F.Raise
              {
                expected = None;
                actual = None;
                backtrace = None;
                predicate = false;
                message_diff = None;
              } ->
              true
          | _ -> false);
        let f =
          F.raised ~expected:"Not_found" ~actual:"Invalid_argument \"x\""
            ~backtrace:"Raised at ..." ()
        in
        is_true ~msg:"payloads stored"
          (match f.F.kind with
          | F.Raise
              {
                expected = Some "Not_found";
                actual = Some "Invalid_argument \"x\"";
                backtrace = Some "Raised at ...";
                _;
              } ->
              true
          | _ -> false);
        let f =
          F.raised ~expected:{|Invalid_argument("a")|}
            ~actual:{|Invalid_argument("b")|}
            ~message_diff:
              {
                F.constructor = "Invalid_argument";
                expected_message = "a";
                actual_message = "b";
              }
            ()
        in
        is_true ~msg:"message diff stored"
          (match f.F.kind with
          | F.Raise
              {
                message_diff =
                  Some
                    {
                      F.constructor = "Invalid_argument";
                      expected_message = "a";
                      actual_message = "b";
                    };
                _;
              } ->
              true
          | _ -> false));
    test "containment constructor: small haystack" (fun () ->
        let f =
          F.containment ~claim:"desc" ~needle:"zz" ~haystack:"hello world" ()
        in
        containment_parts "small haystack" f
          (fun (excerpt, needle, found_at, haystack_length, excerpt_offset) ->
            equal ~msg:"small haystack stored whole" string "hello world"
              excerpt;
            equal ~msg:"needle stored" string "zz" needle;
            is_true ~msg:"found_at defaults to None" (found_at = None);
            equal ~msg:"haystack_length" int
              (String.length "hello world")
              haystack_length;
            equal ~msg:"whole haystack starts at 0" int 0 excerpt_offset);
        is_true ~msg:"claim description stored"
          (match f.F.kind with
          | F.Containment { claim = "desc"; _ } -> true
          | _ -> false));
    test "containment constructor: excerpt windows" (fun () ->
        let f =
          F.containment ~claim:"d" ~needle:"n" ~haystack:big_haystack ()
        in
        containment_parts "head window" f
          (fun (excerpt, _, _, haystack_length, excerpt_offset) ->
            is_true ~msg:"head window is bounded"
              (String.length excerpt <= 8_195);
            is_true ~msg:"head window is a strict prefix"
              (String.length excerpt < String.length big_haystack
              && String.sub big_haystack 0 (String.length excerpt) = excerpt);
            equal ~msg:"head window starts at 0" int 0 excerpt_offset;
            equal ~msg:"full length recorded" int
              (String.length big_haystack)
              haystack_length);
        let found_at = 20_000 in
        let f =
          F.containment ~claim:"d" ~needle:"0002" ~haystack:big_haystack
            ~found_at ()
        in
        containment_parts "centered window" f
          (fun (excerpt, _, stored_found_at, _, excerpt_offset) ->
            is_true ~msg:"found_at stored" (stored_found_at = Some found_at);
            is_true ~msg:"window is bounded" (String.length excerpt <= 8_195);
            is_true ~msg:"window starts before the match"
              (excerpt_offset > 0 && excerpt_offset <= found_at);
            is_true ~msg:"window is the recorded slice of the haystack"
              (String.sub big_haystack excerpt_offset (String.length excerpt)
              = excerpt);
            is_true ~msg:"the match offset falls inside the window"
              (found_at - excerpt_offset < String.length excerpt));
        (* A match near the end: the window simply ends at the haystack's
           end. *)
        let found_at = String.length big_haystack - 5 in
        let f =
          F.containment ~claim:"d" ~needle:"x" ~haystack:big_haystack ~found_at
            ()
        in
        containment_parts "window near the end" f
          (fun (excerpt, _, _, haystack_length, excerpt_offset) ->
            is_true ~msg:"end window reaches the last byte"
              (excerpt_offset + String.length excerpt = haystack_length));
        (* All-2-byte content: code-point boundaries are even offsets, so an
           odd window offset or length would mean a split UTF-8 sequence. *)
        let s = String.concat "" (List.init 10_000 (fun _ -> "\xc3\xa9")) in
        let f =
          F.containment ~claim:"d" ~needle:"\xc3\xa9" ~haystack:s
            ~found_at:9_999 ()
        in
        containment_parts "utf-8 window" f
          (fun (excerpt, _, _, _, excerpt_offset) ->
            is_true ~msg:"window never starts inside a UTF-8 sequence"
              (excerpt_offset mod 2 = 0);
            is_true ~msg:"window never ends inside a UTF-8 sequence"
              ((excerpt_offset + String.length excerpt) mod 2 = 0)));
    test "containment constructor: an unanchored excerpt is the head" (fun () ->
        let excerpt haystack =
          match (F.containment ~claim:"d" ~needle:"n" ~haystack ()).F.kind with
          | F.Containment { excerpt; _ } -> excerpt
          | _ -> fail "containment kind"
        in
        let lines n width =
          String.concat ""
            (List.init n (fun i -> Printf.sprintf "%0*d\n" (width - 1) i))
        in
        equal ~msg:"short lines: the first 10" string (lines 10 10)
          (excerpt (lines 20 10));
        equal ~msg:"long lines: the first 1 KiB" string
          (String.sub (lines 10 200) 0 1_024)
          (excerpt (lines 10 200));
        equal ~msg:"one line: the first 1 KiB" string (String.make 1_024 'a')
          (excerpt (String.make 5_000 'a')));
    test "containment constructor: an anchored window may pass its bound by 3"
      (fun () ->
        (* The window's end falls on the first continuation byte of a
           4-byte character, so the cut moves forward three bytes. *)
        let found_at = 10_000 in
        let four = "\xf0\x9d\x84\x9e" in
        let haystack =
          String.make (found_at + (F.tail_bytes / 2) - 1) 'a'
          ^ four ^ String.make 10_000 'a'
        in
        match
          (F.containment ~claim:"d" ~needle:"a" ~haystack ~found_at ()).F.kind
        with
        | F.Containment { excerpt; _ } ->
            equal ~msg:"tail_bytes + 3" int (F.tail_bytes + 3)
              (String.length excerpt);
            is_true ~msg:"ends on the whole character"
              (String.ends_with ~suffix:four excerpt)
        | _ -> fail "containment kind");
    test "containment constructor: found_at validation and bounding" (fun () ->
        raises_match ~msg:"negative found_at rejected" Exn.invalid_arg
          (fun () ->
            F.containment ~claim:"d" ~needle:"n" ~haystack:"abc" ~found_at:(-1)
              ());
        raises_match ~msg:"found_at past the end rejected" Exn.invalid_arg
          (fun () ->
            F.containment ~claim:"d" ~needle:"n" ~haystack:"abc" ~found_at:4 ());
        is_true ~msg:"found_at at the end accepted (empty-needle case)"
          (match
             F.containment ~claim:"d" ~needle:"" ~haystack:"abc" ~found_at:3 ()
           with
          | _ -> true
          | exception Invalid_argument _ -> false);
        let f = F.containment ~claim:big ~needle:big ~haystack:"abc" () in
        is_true ~msg:"needle and description are bounded"
          (match f.F.kind with
          | F.Containment { claim; needle; _ } ->
              String.length claim < 200_000
              && String.length needle < 200_000
              && has ~needle:"truncated" needle
          | _ -> false));
    test "baseline constructor" (fun () ->
        let f =
          F.baseline (F.File "test/greeting.expected")
            (F.Mismatch { expected = "hi\n"; actual = "ho\n" })
        in
        is_true ~msg:"identity and state stored"
          (match f.F.kind with
          | F.Baseline
              {
                baseline = F.File "test/greeting.expected";
                state = F.Mismatch { expected = "hi\n"; actual = "ho\n" };
              } ->
              true
          | _ -> false);
        (* Renderers name the file from the path: it is an identity and must
           never be truncated. *)
        let path = String.make 100_000 'p' in
        let f =
          F.baseline (F.File path) (F.Unresolvable { candidate = path })
        in
        is_true ~msg:"path is stored unmodified"
          (match f.F.kind with
          | F.Baseline
              { baseline = F.File p; state = F.Unresolvable { candidate } } ->
              String.equal p path && String.equal candidate path
          | _ -> false);
        let f =
          F.baseline
            (F.Literal { exact = false })
            (F.Mismatch { expected = big; actual = "a" })
        in
        is_true ~msg:"mismatch contents are bounded"
          (match f.F.kind with
          | F.Baseline { state = F.Mismatch { expected; _ }; _ } ->
              String.length expected < 200_000
          | _ -> false);
        let f = F.baseline (F.File "p") (F.Missing { proposed = big }) in
        is_true ~msg:"proposed content is bounded"
          (match f.F.kind with
          | F.Baseline { state = F.Missing { proposed }; _ } ->
              String.length proposed < 200_000
          | _ -> false);
        let site = loc_of "test/a.ml" 3 in
        let f =
          F.baseline ~loc:site
            (F.Literal { exact = true })
            (F.Mismatch { expected = "a"; actual = "b" })
        in
        is_true ~msg:"a literal failure carries its site" (f.F.loc = Some site);
        is_true ~msg:"and the verb that read it"
          (match f.F.kind with
          | F.Baseline { baseline = F.Literal { exact }; _ } -> exact
          | _ -> false));
    test "baseline constructor: nothing withheld" (fun () ->
        match
          (F.baseline (F.File "p") (F.Missing { proposed = "x" })).F.kind
        with
        | F.Baseline { withheld; _ } -> is_true (withheld = None)
        | _ -> fail "baseline kind");
    test "property constructor: count, shrink_exhausted and rendering defaults"
      (fun () ->
        match
          (F.property ~rendered:"[]" ~case_index:0 ~shrink_steps:0 ~root:1L
             ~examples:false ())
            .F.kind
        with
        | F.Property { count; shrink_exhausted; rendering; _ } ->
            is_true ~msg:"count None" (count = None);
            is_false ~msg:"shrink_exhausted false" shrink_exhausted;
            is_true ~msg:"rendering Value" (rendering = F.Value)
        | _ -> fail "property kind");
    test "property constructor" (fun () ->
        let inner = F.equality ~expected:"true" ~actual:"false" () in
        let f =
          F.property ~inner ~rendered:"Rect (2, 0)" ~case_index:12
            ~shrink_steps:4 ~root:0x7be1d2c904aa31f5L ~examples:false ()
        in
        is_true ~msg:"payload stored"
          (match f.F.kind with
          | F.Property
              {
                rendered = "Rect (2, 0)";
                case_index = 12;
                shrink_steps = 4;
                timed_out = None;
                root = 0x7be1d2c904aa31f5L;
                examples = false;
                inner = Some i;
              } ->
              i == inner
          | _ -> false);
        let f =
          F.property ~rendered:"[]" ~case_index:0 ~shrink_steps:0 ~root:1L
            ~examples:true ()
        in
        is_true ~msg:"inner defaults to None, examples flag stored"
          (match f.F.kind with
          | F.Property { examples = true; inner = None; _ } -> true
          | _ -> false);
        is_true ~msg:"timed_out defaults to None"
          (match f.F.kind with
          | F.Property { timed_out = None; _ } -> true
          | _ -> false);
        is_true ~msg:"summary defaults to None"
          (match f.F.kind with
          | F.Property { summary = None; _ } -> true
          | _ -> false);
        is_true ~msg:"an explicit summary is stored beside the rendering"
          (match
             (F.property ~summary:"2 calls, last: get" ~rendered:" #  call"
                ~case_index:0 ~shrink_steps:0 ~root:1L ~examples:false ())
               .F.kind
           with
          | F.Property
              { summary = Some "2 calls, last: get"; rendered = " #  call"; _ }
            ->
              true
          | _ -> false);
        let f =
          F.property ~timed_out:0.3 ~rendered:"[]" ~case_index:0 ~shrink_steps:2
            ~root:1L ~examples:false ()
        in
        is_true ~msg:"an explicit timed_out limit is stored"
          (match f.F.kind with
          | F.Property { timed_out = Some 0.3; _ } -> true
          | _ -> false));
    test "with_phase and with_output_tail" (fun () ->
        let f = F.message "boom" in
        let g = F.with_phase F.Teardown f in
        is_true ~msg:"with_phase: replaces the phase" (g.F.phase = F.Teardown);
        is_true ~msg:"with_phase: original unchanged" (f.F.phase = F.Body);
        is_true ~msg:"with_phase: kind untouched" (g.F.kind = f.F.kind);
        let tl = F.tail "out" in
        let h = F.with_output_tail tl f in
        is_true ~msg:"with_output_tail: attaches the tail"
          (h.F.output_tail = Some tl);
        is_true ~msg:"with_output_tail: original unchanged"
          (f.F.output_tail = None);
        let tl' = F.tail "again" in
        is_true ~msg:"with_output_tail: replaces an existing tail"
          ((F.with_output_tail tl' h).F.output_tail = Some tl'));
    test "with_withheld marks a Baseline failure and nothing else" (fun () ->
        let withheld (f : F.t) =
          match f.F.kind with
          | F.Baseline { withheld; _ } -> withheld
          | _ -> None
        in
        List.iter
          (fun state ->
            is_true ~msg:"every Baseline state is marked"
              (withheld
                 (F.with_withheld F.Skipped (F.baseline (F.File "p") state))
              = Some F.Skipped))
          [
            F.Mismatch { expected = "a"; actual = "b" };
            F.Missing { proposed = "a" };
            F.Unresolvable { candidate = "c" };
          ];
        let eq = F.equality ~expected:"1" ~actual:"2" () in
        is_true ~msg:"another kind is returned as it is"
          (F.with_withheld F.Failed_outside eq = eq);
        let inner = F.baseline (F.File "p") (F.Missing { proposed = "a" }) in
        let prop =
          F.property ~inner ~rendered:"x" ~case_index:0 ~shrink_steps:0 ~root:1L
            ~examples:false ()
        in
        is_true ~msg:"a Property's inner is not reached"
          (F.with_withheld F.Skipped prop = prop));
    test "tail_bytes is 8 KiB, what a tail keeps" (fun () ->
        equal ~msg:"tail_bytes" int 8_192 F.tail_bytes;
        let tl = F.tail (String.make 10_000 'a') in
        equal ~msg:"an ASCII tail keeps exactly 8,192 bytes" int 8_192
          (String.length tl.F.text);
        equal ~msg:"and counts the rest omitted" int 1_808 tl.F.omitted_bytes);
    test "tails" (fun () ->
        let tl = F.tail "hello\n" in
        is_true ~msg:"short text kept verbatim"
          (tl.F.text = "hello\n" && tl.F.omitted_bytes = 0
         && tl.F.log_path = None);
        let tl =
          F.tail ~log_path:"_build/_tests/t.output" ~omitted_bytes:7 "x"
        in
        is_true ~msg:"log_path and prior omission recorded"
          (tl.F.log_path = Some "_build/_tests/t.output"
          && tl.F.omitted_bytes = 7);
        let line i = Printf.sprintf "[debug] line %05d\n" i in
        let full = String.concat "" (List.init 1_000 line) in
        let tl = F.tail full in
        let kept = String.length tl.F.text in
        is_true ~msg:"long output is bounded" (kept < String.length full);
        equal ~msg:"omitted accounts for every cut byte" int
          (String.length full - kept)
          tl.F.omitted_bytes;
        equal ~msg:"retains the final bytes" string
          (String.sub full (String.length full - kept) kept)
          tl.F.text;
        let tl' = F.tail ~omitted_bytes:11 full in
        equal ~msg:"prior omission accumulates" int
          (String.length full - String.length tl'.F.text + 11)
          tl'.F.omitted_bytes;
        (* All-3-byte content: a kept suffix must start at an offset
           divisible by 3 or a UTF-8 sequence was split. *)
        let s = String.concat "" (List.init 3_333 (fun _ -> "\xe2\x82\xac")) in
        let tl = F.tail s in
        is_true ~msg:"suffix cut never splits a UTF-8 sequence"
          (tl.F.omitted_bytes mod 3 = 0
          && Char.code tl.F.text.[0] land 0xC0 <> 0x80);
        raises_match ~msg:"negative omitted_bytes rejected" Exn.invalid_arg
          (fun () -> F.tail ~omitted_bytes:(-1) "x"));
    (* Only a *trailing* run of windtrap frames goes. Here the exception is
       caught in this file, so the deepest frame is the reader's and there
       is no trailing run at all: the two [Loc.delimit] frames the raise
       passed through are interior, and every one of them must survive —
       user code windtrap invoked sits between them. The trailing case,
       where windtrap itself catches, is pinned in test_check.ml. *)
    test "backtrace_to_string: keeps interior own frames" (fun () ->
        let raw =
          match Loc.delimit (fun () -> through_delimit ()) with
          | () -> assert false
          | exception Not_found -> Printexc.get_raw_backtrace ()
        in
        let whole = Printexc.raw_backtrace_to_string raw in
        let trimmed = F.backtrace_to_string raw in
        is_true ~msg:"premise: the raise passed through the delimiter"
          (has ~needle:"Windtrap__Loc.delimit" whole);
        is_true ~msg:"premise: the deepest frame is the reader's"
          (has ~needle:"Test_failure" (last_line whole));
        equal ~msg:"nothing is trimmed" string whole trimmed;
        is_true ~msg:"the raise site survives"
          (has ~needle:"Test_failure.raise_not_found" trimmed);
        is_true ~msg:"the first frame still reads as the raise site"
          (String.starts_with ~prefix:"Raised at" trimmed);
        (* Empty only from an empty raw backtrace, never because trimming
           emptied a real one. *)
        equal ~msg:"an empty raw backtrace renders empty" string ""
          (F.backtrace_to_string (Printexc.get_callstack 0)));
    test "catch classifies what its function raised" (fun () ->
        let failure = F.message "boom" in
        is_true ~msg:"a return is Ok" (F.catch (fun () -> 3) = Ok 3);
        is_true ~msg:"a Check_failure is an assertion"
          (match F.catch (fun () -> raise (F.Check_failure failure)) with
          | Error (`Assertion f) -> f == failure
          | _ -> false);
        is_true ~msg:"a Control is its control"
          (match F.catch (fun () -> raise (F.Control (`Skip (Some "r")))) with
          | Error (`Skip (Some "r")) -> true
          | _ -> false);
        is_true ~msg:"any other exception is an exception"
          (match F.catch (fun () -> raise Not_found) with
          | Error (`Exception (Not_found, _)) -> true
          | _ -> false));
    test "catch never returns an interrupt or an exhausted resource" (fun () ->
        List.iter
          (fun exn ->
            let name = Printexc.to_string exn in
            match F.catch (fun () -> raise exn) with
            | exception raised ->
                is_true ~msg:(name ^ " raised again") (raised == exn)
            | Ok () | Error _ -> failf "%s was returned" name)
          [ Sys.Break; Out_of_memory ];
        is_true ~msg:"a Stack_overflow is an exception"
          (match F.catch (fun () -> raise Stack_overflow) with
          | Error (`Exception (Stack_overflow, _)) -> true
          | _ -> false));
    test "catch unwraps a finally cut by a control or a fatal exception"
      (fun () ->
        let cut exn () = Fun.protect ~finally:(fun () -> raise exn) ignore in
        is_true ~msg:"a timeout in a finally is a timeout"
          (match F.catch (cut (F.Control (`Timeout 1.5))) with
          | Error (`Timeout 1.5) -> true
          | _ -> false);
        is_true ~msg:"an interrupt in a finally is raised as itself"
          (match F.catch (cut Sys.Break) with
          | exception Sys.Break -> true
          | _ -> false);
        is_true ~msg:"any other exception of a finally stays wrapped"
          (match F.catch (cut Not_found) with
          | Error (`Exception (Fun.Finally_raised Not_found, _)) -> true
          | _ -> false));
    test "reraise raises what catch returned" (fun () ->
        let saved = Printexc.backtrace_status () in
        Fun.protect ~finally:(fun () -> Printexc.record_backtrace saved)
        @@ fun () ->
        Printexc.record_backtrace true;
        let raise_not_found () = raise Not_found in
        match F.catch raise_not_found with
        | Error (`Exception (_, backtrace) as c) -> (
            match F.reraise c with
            | exception Not_found ->
                is_true ~msg:"the backtrace continues the original"
                  (String.starts_with
                     ~prefix:(Printexc.raw_backtrace_to_string backtrace)
                     (Printexc.raw_backtrace_to_string
                        (Printexc.get_raw_backtrace ())))
            | _ -> fail "reraise returned")
        | _ -> fail "Not_found was not an exception");
    test "caught_to_string prints the exception" (fun () ->
        equal string "windtrap timeout after 1.5s"
          (F.caught_to_string (`Timeout 1.5));
        equal string "windtrap discard (assume or reject outside a property)"
          (F.caught_to_string `Discard);
        equal string "windtrap skip: why"
          (F.caught_to_string (`Skip (Some "why")));
        equal string "windtrap skip" (F.caught_to_string (`Skip None));
        equal string "Not_found"
          (F.caught_to_string
             (`Exception (Not_found, Printexc.get_callstack 0))));
  ]

let () = exit @@ Windtrap.run "failure" tests
