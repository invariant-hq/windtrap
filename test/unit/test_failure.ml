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

(* Defined in an executable, so the runtime names it [Dune__exe__Test_failure.Full]. *)
exception Full

(* An exception whose text is its payload, for names a printer writes. *)
exception Printed of string

let () = Printexc.register_printer (function Printed s -> Some s | _ -> None)

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
      k (excerpt, needle.F.kept, found_at, haystack_length, excerpt_offset)
  | _ -> is_true ~msg:(name ^ ": Containment kind") false

(* What a text kept, for the pins that compare it with a string. *)
let kept (t : F.text) = t.kept

(* A text of 200,000 bytes is cut: its [kept] is shorter, holds no marker,
   and its [length] is the whole text's. *)
let cut_whole ~msg (t : F.text) =
  is_true ~msg
    (F.is_cut t
    && String.length t.kept < 200_000
    && t.length = 200_000
    && not (has ~needle:"truncated" t.kept))

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
              {
                expected = { kept = "1"; length = 1 };
                actual = { kept = "2"; length = 1 };
                not_ = false;
                diffable = true;
              } ->
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
        is_true ~msg:"msg stored" (Option.map kept f.F.msg = Some "ids"));
    test "predicate constructor" (fun () ->
        let loc = loc_of "test/t.ml" 7 in
        let f = F.predicate ~loc ~msg:"positive" ~claim:"a match" "None" in
        (* The claim takes the expected side, and [diffable] is what stops a
           renderer refining a description against a value. *)
        is_true ~msg:"claim and value stored"
          (match f.F.kind with
          | F.Equality
              {
                expected = { kept = "a match"; _ };
                actual = { kept = "None"; _ };
                diffable = false;
                _;
              } ->
              true
          | _ -> false);
        is_true ~msg:"loc stored" (f.F.loc = Some loc);
        is_true ~msg:"msg stored" (Option.map kept f.F.msg = Some "positive");
        match (F.predicate ~claim:big big).F.kind with
        | F.Equality { expected; actual; diffable = false; _ } ->
            cut_whole ~msg:"the claim is bounded" expected;
            cut_whole ~msg:"the value is bounded" actual
        | _ -> fail "predicate kind");
    test "payload bounding" (fun () ->
        (match (F.equality ~expected:big ~actual:"2" ()).F.kind with
        | F.Equality { expected; actual; _ } ->
            cut_whole ~msg:"a long side is cut" expected;
            is_true ~msg:"a short side is whole" (not (F.is_cut actual));
            is_true ~msg:"the cut keeps a prefix of the value"
              (String.length expected.kept > 1_000 && expected.kept.[0] = 'a')
        | _ -> fail "equality kind");
        (* All-2-byte content: any code-point boundary is an even offset, so
           an odd-length kept prefix would mean a split UTF-8 sequence. *)
        (let s = String.concat "" (List.init 100_000 (fun _ -> "\xc3\xa9")) in
         match (F.message s).F.kind with
         | F.Message text ->
             is_true ~msg:"the text is cut" (F.is_cut text);
             is_true ~msg:"never splits a UTF-8 sequence"
               (String.length text.kept mod 2 = 0)
         | _ -> fail "message kind");
        (match (F.equality ~msg:big ~expected:"1" ~actual:"2" ()).F.msg with
        | Some msg -> cut_whole ~msg:"msg is bounded too" msg
        | None -> fail "msg kept");
        (match
           (F.raised ~expected:big ~actual:big ~backtrace:big
              ~message_diff:
                {
                  F.constructor = "Failure";
                  expected_message = F.text big;
                  actual_message = F.text big;
                }
              ())
             .F.kind
         with
        | F.Raise
            {
              expected = Some e;
              actual = Some a;
              backtrace = Some b;
              message_diff = Some { expected_message; actual_message; _ };
              _;
            } ->
            cut_whole ~msg:"the expected exception is bounded" e;
            cut_whole ~msg:"the raised exception is bounded" a;
            cut_whole ~msg:"the backtrace is bounded" b;
            cut_whole ~msg:"the expected message is bounded" expected_message;
            cut_whole ~msg:"the raised message is bounded" actual_message
        | _ -> fail "raise kind");
        match
          (F.property ~rendered:big ~case_index:0 ~shrink_steps:0 ~root:1L
             ~examples:false ())
            .F.kind
        with
        | F.Property { rendered; _ } ->
            cut_whole ~msg:"property counterexample is bounded" rendered
        | _ -> fail "property kind");
    test "a text is bounded at 65,536 bytes" (fun () ->
        let at_bound = String.make 65_536 'a' in
        let whole = F.text at_bound and cut = F.text (at_bound ^ "b") in
        equal ~msg:"65,536 bytes are kept as given" string at_bound whole.kept;
        is_true ~msg:"and are whole" (not (F.is_cut whole));
        equal ~msg:"65,537 bytes keep 65,536, with no marker" string at_bound
          cut.kept;
        equal ~msg:"and record the whole length" int 65_537 cut.length;
        is_true ~msg:"and are cut" (F.is_cut cut));
    test "two texts equal on their first 64 KiB are both cut" (fun () ->
        let head = String.make 65_536 'a' in
        let a = F.text (head ^ "xyz") and b = F.text (head ^ "XYZ") in
        is_true ~msg:"they keep the same bytes" (String.equal a.kept b.kept);
        is_true ~msg:"and neither is whole" (F.is_cut a && F.is_cut b));
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
                expected = Some { kept = "Not_found"; _ };
                actual = Some { kept = "Invalid_argument \"x\""; _ };
                backtrace = Some { kept = "Raised at ..."; _ };
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
                expected_message = F.text "a";
                actual_message = F.text "b";
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
                      expected_message = { kept = "a"; _ };
                      actual_message = { kept = "b"; _ };
                    };
                _;
              } ->
              true
          | _ -> false));
    test "containment constructor: small haystack" (fun () ->
        let f =
          F.containment ~demand:F.Anywhere ~needle:"zz" ~haystack:"hello world"
            ()
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
        is_true ~msg:"demand stored"
          (match f.F.kind with
          | F.Containment { demand = F.Anywhere; _ } -> true
          | _ -> false));
    test "containment constructor: excerpt windows" (fun () ->
        let f =
          F.containment ~demand:F.Anywhere ~needle:"n" ~haystack:big_haystack ()
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
          F.containment ~demand:F.Anywhere ~needle:"0002" ~haystack:big_haystack
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
          F.containment ~demand:F.Anywhere ~needle:"x" ~haystack:big_haystack
            ~found_at ()
        in
        containment_parts "window near the end" f
          (fun (excerpt, _, _, haystack_length, excerpt_offset) ->
            is_true ~msg:"end window reaches the last byte"
              (excerpt_offset + String.length excerpt = haystack_length));
        (* All-2-byte content: code-point boundaries are even offsets, so an
           odd window offset or length would mean a split UTF-8 sequence. *)
        let s = String.concat "" (List.init 10_000 (fun _ -> "\xc3\xa9")) in
        let f =
          F.containment ~demand:F.Anywhere ~needle:"\xc3\xa9" ~haystack:s
            ~found_at:9_999 ()
        in
        containment_parts "utf-8 window" f
          (fun (excerpt, _, _, _, excerpt_offset) ->
            is_true ~msg:"window never starts inside a UTF-8 sequence"
              (excerpt_offset mod 2 = 0);
            is_true ~msg:"window never ends inside a UTF-8 sequence"
              ((excerpt_offset + String.length excerpt) mod 2 = 0)));
    test "containment constructor: an unanchored excerpt is the head" (fun () ->
        let excerpt ?(demand = F.Anywhere) haystack =
          match (F.containment ~demand ~needle:"n" ~haystack ()).F.kind with
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
          (excerpt (String.make 5_000 'a'));
        (* Under [Suffix] the same bounds, read from the end. *)
        let suffix = excerpt ~demand:F.Suffix in
        equal ~msg:"suffix, short lines: the last 10" string
          (String.concat ""
             (List.init 10 (fun i -> Printf.sprintf "%09d\n" (i + 10))))
          (suffix (lines 20 10));
        equal ~msg:"suffix, no final newline: the last 10" string
          "1\n2\n3\n4\n5\n6\n7\n8\n9\n10"
          (suffix "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10");
        equal ~msg:"suffix, long lines: the last 1 KiB" string
          (let all = lines 10 200 in
           String.sub all (String.length all - 1_024) 1_024)
          (suffix (lines 10 200));
        equal ~msg:"suffix, fewer than 10 short lines: whole" string
          (lines 3 10)
          (suffix (lines 3 10));
        equal ~msg:"suffix, an empty haystack: empty" string "" (suffix "");
        (* 2-byte characters: the byte bound moves forward to a boundary. *)
        equal ~msg:"suffix, one line: its end, on a code point" int 1_024
          (String.length
             (suffix (String.concat "" (List.init 3_000 (fun _ -> "\xc3\xa9"))))));
    test "containment constructor: an anchored window stays within its bound"
      (fun () ->
        (* The window's end falls on the first continuation byte of a
           4-byte character, so the cut moves back before it. *)
        let found_at = 10_000 in
        let four = "\xf0\x9d\x84\x9e" in
        let haystack =
          String.make (found_at + (F.tail_bytes / 2) - 1) 'a'
          ^ four ^ String.make 10_000 'a'
        in
        match
          (F.containment ~demand:F.Anywhere ~needle:"a" ~haystack ~found_at ())
            .F.kind
        with
        | F.Containment { excerpt; _ } ->
            equal ~msg:"tail_bytes - 1" int (F.tail_bytes - 1)
              (String.length excerpt);
            is_true ~msg:"ends before the character"
              (String.ends_with ~suffix:"a" excerpt)
        | _ -> fail "containment kind");
    test "containment constructor: found_at validation and bounding" (fun () ->
        raises_match ~msg:"negative found_at rejected" Exn.invalid_arg
          (fun () ->
            F.containment ~demand:F.Anywhere ~needle:"n" ~haystack:"abc"
              ~found_at:(-1) ());
        raises_match ~msg:"found_at past the end rejected" Exn.invalid_arg
          (fun () ->
            F.containment ~demand:F.Anywhere ~needle:"n" ~haystack:"abc"
              ~found_at:4 ());
        is_true ~msg:"found_at at the end accepted (empty-needle case)"
          (match
             F.containment ~demand:F.Anywhere ~needle:"" ~haystack:"abc"
               ~found_at:3 ()
           with
          | _ -> true
          | exception Invalid_argument _ -> false);
        let f =
          F.containment ~demand:F.Anywhere ~needle:big ~haystack:"abc" ()
        in
        is_true ~msg:"the needle is bounded"
          (match f.F.kind with
          | F.Containment { needle; _ } ->
              F.is_cut needle && needle.length = 200_000
          | _ -> false));
    test "baseline constructor" (fun () ->
        let f =
          F.baseline (F.File "test/greeting.expected")
            (F.Mismatch { expected = F.text "hi\n"; actual = F.text "ho\n" })
        in
        is_true ~msg:"identity and state stored"
          (match f.F.kind with
          | F.Baseline
              {
                baseline = F.File "test/greeting.expected";
                state =
                  F.Mismatch
                    {
                      expected = { kept = "hi\n"; _ };
                      actual = { kept = "ho\n"; _ };
                    };
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
        let site = loc_of "test/a.ml" 3 in
        let f =
          F.baseline ~loc:site
            (F.Literal { exact = true })
            (F.Mismatch { expected = F.text "a"; actual = F.text "b" })
        in
        is_true ~msg:"a literal failure carries its site" (f.F.loc = Some site);
        is_true ~msg:"and the verb that read it"
          (match f.F.kind with
          | F.Baseline { baseline = F.Literal { exact }; _ } -> exact
          | _ -> false));
    test "baseline constructor: nothing withheld" (fun () ->
        match
          (F.baseline (F.File "p") (F.Missing { proposed = F.text "x" })).F.kind
        with
        | F.Baseline { withheld; _ } -> is_true (withheld = None)
        | _ -> fail "baseline kind");
    test "property constructor: count, shrink_end and rendering defaults"
      (fun () ->
        match
          (F.property ~rendered:"[]" ~case_index:0 ~shrink_steps:0 ~root:1L
             ~examples:false ())
            .F.kind
        with
        | F.Property { count; shrink_end; rendering; _ } ->
            is_true ~msg:"count None" (count = None);
            is_true ~msg:"shrink_end Converged" (shrink_end = F.Converged);
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
                rendered = { kept = "Rect (2, 0)"; _ };
                case_index = 12;
                shrink_steps = 4;
                shrink_end = F.Converged;
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
              {
                summary = Some { kept = "2 calls, last: get"; _ };
                rendered = { kept = " #  call"; _ };
                _;
              } ->
              true
          | _ -> false);
        let f =
          F.property ~shrink_end:(F.Timed_out 0.3) ~rendered:"[]" ~case_index:0
            ~shrink_steps:2 ~root:1L ~examples:false ()
        in
        is_true ~msg:"an explicit shrink_end is stored"
          (match f.F.kind with
          | F.Property { shrink_end = F.Timed_out 0.3; _ } -> true
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
            F.Mismatch { expected = F.text "a"; actual = F.text "b" };
            F.Missing { proposed = F.text "a" };
            F.Unresolvable { candidate = "c" };
          ];
        let eq = F.equality ~expected:"1" ~actual:"2" () in
        is_true ~msg:"another kind is returned as it is"
          (F.with_withheld F.Failed_outside eq = eq);
        let inner =
          F.baseline (F.File "p") (F.Missing { proposed = F.text "a" })
        in
        let prop =
          F.property ~inner ~rendered:"x" ~case_index:0 ~shrink_steps:0 ~root:1L
            ~examples:false ()
        in
        is_true ~msg:"a Property's inner is not reached"
          (F.with_withheld F.Skipped prop = prop);
        let refused =
          F.with_withheld
            (F.Refused { line = 3; reason = "why" })
            (F.baseline (F.File "p")
               (F.Mismatch { expected = F.text "a"; actual = F.text "b" }))
        in
        is_true ~msg:"a Refused mark holds whatever the attempt adds"
          (withheld (F.with_withheld F.Failed_outside refused)
          = Some (F.Refused { line = 3; reason = "why" }));
        let conflict =
          F.with_withheld F.Conflict
            (F.baseline (F.File "p")
               (F.Mismatch { expected = F.text "a"; actual = F.text "b" }))
        in
        is_true ~msg:"so does a Conflict mark"
          (withheld (F.with_withheld F.Skipped conflict) = Some F.Conflict));
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
       passed through are interior, and every one of them must survive.
       User code windtrap invoked sits between them. The trailing case,
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
    test "exn_to_string names an executable's exception as its source does"
      (fun () ->
        equal string "Test_failure.Full" (F.exn_to_string Full);
        equal string "Test_failure.Full"
          (F.caught_to_string (`Exception (Full, Printexc.get_callstack 0)));
        equal string "Fun.Finally_raised: Test_failure.Full"
          (F.exn_to_string (Fun.Finally_raised Full));
        equal string "call (M.E)"
          (F.exn_to_string (Printed "call (Dune__exe__M.E)"));
        equal string "X_Dune__exe__M.E"
          (F.exn_to_string (Printed "X_Dune__exe__M.E"));
        equal string "(M.E) raised N.F in X_Dune__exe__T"
          (F.exn_to_string
             (Printed "(Dune__exe__M.E) raised Dune__exe__N.F in X_Dune__exe__T")));
  ]

let () = exit @@ Windtrap.run "failure" tests
