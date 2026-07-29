(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Failure: typed kinds, defaults, payload bounding, tails,
   outcomes, and the control exceptions. *)

open Windtrap
module F = Windtrap.Private.Failure
module Loc = Windtrap.Private.Loc

let check name cond = is_true ~msg:name cond
let check_int name ~expected ~actual = equal ~msg:name int expected actual
let check_string name ~expected ~actual = equal ~msg:name string expected actual

let has ~needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  let rec loop i =
    i + nl <= hl && (String.sub haystack i nl = needle || loop (i + 1))
  in
  nl = 0 || loop 0

let loc_of file line = Loc.of_pos (file, line, 0, 0)

(* [containment_parts f k] projects a containment failure's shape: the
   stored excerpt ([actual]) and the [Contains] claim fields. *)
let containment_parts name (f : F.t) k =
  match f.F.kind with
  | F.Equality
      {
        actual;
        not_ = false;
        claim = F.Contains { needle; found_at; haystack_length; excerpt_offset };
        _;
      } ->
      k (actual, needle, found_at, haystack_length, excerpt_offset)
  | _ -> check (name ^ ": Equality kind with a Contains claim") false

let big = String.make 200_000 'a'

let big_haystack =
  String.concat "" (List.init 2_500 (fun i -> Printf.sprintf "%09d\n" i))

let tests =
  [
    test "equality constructor: defaults" (fun () ->
        let f = F.equality ~expected:"1" ~actual:"2" () in
        check "kind payload"
          (match f.F.kind with
          | F.Equality
              { expected = "1"; actual = "2"; not_ = false; claim = F.Equal } ->
              true
          | _ -> false);
        check "default phase is Body" (f.F.phase = F.Body);
        check "default loc is None" (f.F.loc = None);
        check "default msg is None" (f.F.msg = None);
        check "default output_tail is None" (f.F.output_tail = None));
    test "equality constructor: loc, msg, not_, claim stored" (fun () ->
        let loc = loc_of "test/t.ml" 12 in
        let f =
          F.equality ~loc ~msg:"ids" ~not_:true ~expected:"3" ~actual:"3" ()
        in
        check "not_ recorded"
          (match f.F.kind with
          | F.Equality { not_ = true; _ } -> true
          | _ -> false);
        check "loc stored" (f.F.loc = Some loc);
        check "msg stored" (f.F.msg = Some "ids");
        let f = F.equality ~claim:F.Satisfies ~expected:"a" ~actual:"b" () in
        check "claim stored"
          (match f.F.kind with
          | F.Equality { claim = F.Satisfies; _ } -> true
          | _ -> false));
    test "equality constructor: not_ never pairs with a refined claim"
      (fun () ->
        (* The kind doc promises renderers that [not_] never pairs with a
           refined claim; the constructor is where that invariant is
           enforced. *)
        raises_match ~msg:"not_ with a refined claim is rejected"
          Exn.invalid_arg (fun () ->
            F.equality ~not_:true ~claim:F.Matches ~expected:"a" ~actual:"a" ());
        check "explicit ~not_:false with a refined claim is fine"
          (match
             F.equality ~not_:false ~claim:F.Satisfies ~expected:"a" ~actual:"b"
               ()
           with
          | { F.kind = F.Equality { claim = F.Satisfies; not_ = false; _ }; _ }
            ->
              true
          | _ -> false));
    test "payload bounding" (fun () ->
        (let f = F.equality ~expected:big ~actual:"2" () in
         match f.F.kind with
         | F.Equality { expected; actual = "2"; _ } ->
             check "long payload is shorter than the original"
               (String.length expected < String.length big);
             check "marker states the original byte count"
               (has ~needle:"truncated" expected
               && has ~needle:"200000 bytes" expected);
             check "truncation keeps a prefix of the value"
               (String.length expected > 1_000 && expected.[0] = 'a')
         | _ -> check "kind preserved" false);
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
             | None -> check "utf-8 payload has a marker" false
             | Some i -> check "never splits a UTF-8 sequence" (i mod 2 = 0))
         | _ -> check "message kind preserved" false);
        (let f = F.equality ~msg:big ~expected:"1" ~actual:"2" () in
         match f.F.msg with
         | Some msg -> check "msg is bounded too" (String.length msg < 200_000)
         | None -> check "msg kept" false);
        let f =
          F.raised ~expected:big ~actual:big ~backtrace:big
            ~expected_message:big ~actual_message:big ()
        in
        check "raise payloads are bounded"
          (match f.F.kind with
          | F.Raise
              {
                expected = Some e;
                actual = Some a;
                backtrace = Some b;
                expected_message = Some em;
                actual_message = Some am;
                _;
              } ->
              String.length e < 200_000
              && String.length a < 200_000
              && String.length b < 200_000
              && String.length em < 200_000
              && String.length am < 200_000
          | _ -> false);
        let f =
          F.property ~rendered:big ~case_index:0 ~shrink_steps:0 ~root:1L
            ~examples:false ()
        in
        check "property counterexample is bounded"
          (match f.F.kind with
          | F.Property { rendered; _ } -> String.length rendered < 200_000
          | _ -> false));
    test "raise constructor" (fun () ->
        let f = F.raised () in
        check "all payloads default to absent"
          (match f.F.kind with
          | F.Raise
              {
                expected = None;
                actual = None;
                backtrace = None;
                same_constructor = false;
                expected_message = None;
                actual_message = None;
              } ->
              true
          | _ -> false);
        let f =
          F.raised ~expected:"Not_found" ~actual:"Invalid_argument \"x\""
            ~backtrace:"Raised at ..." ()
        in
        check "payloads stored"
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
            ~actual:{|Invalid_argument("b")|} ~same_constructor:true
            ~expected_message:"a" ~actual_message:"b" ()
        in
        check "message-diff enrichment stored"
          (match f.F.kind with
          | F.Raise
              {
                same_constructor = true;
                expected_message = Some "a";
                actual_message = Some "b";
                _;
              } ->
              true
          | _ -> false));
    test "containment constructor: small haystack" (fun () ->
        let f =
          F.containment ~expected:"desc" ~needle:"zz" ~haystack:"hello world" ()
        in
        containment_parts "small haystack" f
          (fun (excerpt, needle, found_at, haystack_length, excerpt_offset) ->
            check_string "small haystack stored whole" ~expected:"hello world"
              ~actual:excerpt;
            check_string "needle stored" ~expected:"zz" ~actual:needle;
            check "found_at defaults to None" (found_at = None);
            check_int "haystack_length"
              ~expected:(String.length "hello world")
              ~actual:haystack_length;
            check_int "whole haystack starts at 0" ~expected:0
              ~actual:excerpt_offset);
        check "expected description stored"
          (match f.F.kind with
          | F.Equality { expected = "desc"; _ } -> true
          | _ -> false));
    test "containment constructor: excerpt windows" (fun () ->
        let f =
          F.containment ~expected:"d" ~needle:"n" ~haystack:big_haystack ()
        in
        containment_parts "head window" f
          (fun (excerpt, _, _, haystack_length, excerpt_offset) ->
            check "head window is bounded" (String.length excerpt <= 8_195);
            check "head window is a strict prefix"
              (String.length excerpt < String.length big_haystack
              && String.sub big_haystack 0 (String.length excerpt) = excerpt);
            check_int "head window starts at 0" ~expected:0
              ~actual:excerpt_offset;
            check_int "full length recorded"
              ~expected:(String.length big_haystack)
              ~actual:haystack_length);
        let found_at = 20_000 in
        let f =
          F.containment ~expected:"d" ~needle:"0002" ~haystack:big_haystack
            ~found_at ()
        in
        containment_parts "centered window" f
          (fun (excerpt, _, stored_found_at, _, excerpt_offset) ->
            check "found_at stored" (stored_found_at = Some found_at);
            check "window is bounded" (String.length excerpt <= 8_195);
            check "window starts before the match"
              (excerpt_offset > 0 && excerpt_offset <= found_at);
            check "window is the recorded slice of the haystack"
              (String.sub big_haystack excerpt_offset (String.length excerpt)
              = excerpt);
            check "the match offset falls inside the window"
              (found_at - excerpt_offset < String.length excerpt));
        (* A match near the end: the window simply ends at the haystack's
           end. *)
        let found_at = String.length big_haystack - 5 in
        let f =
          F.containment ~expected:"d" ~needle:"x" ~haystack:big_haystack
            ~found_at ()
        in
        containment_parts "window near the end" f
          (fun (excerpt, _, _, haystack_length, excerpt_offset) ->
            check "end window reaches the last byte"
              (excerpt_offset + String.length excerpt = haystack_length));
        (* All-2-byte content: code-point boundaries are even offsets, so an
           odd window offset or length would mean a split UTF-8 sequence. *)
        let s = String.concat "" (List.init 10_000 (fun _ -> "\xc3\xa9")) in
        let f =
          F.containment ~expected:"d" ~needle:"\xc3\xa9" ~haystack:s
            ~found_at:9_999 ()
        in
        containment_parts "utf-8 window" f
          (fun (excerpt, _, _, _, excerpt_offset) ->
            check "window never starts inside a UTF-8 sequence"
              (excerpt_offset mod 2 = 0);
            check "window never ends inside a UTF-8 sequence"
              ((excerpt_offset + String.length excerpt) mod 2 = 0)));
    test "containment constructor: found_at validation and bounding" (fun () ->
        raises_match ~msg:"negative found_at rejected" Exn.invalid_arg
          (fun () ->
            F.containment ~expected:"d" ~needle:"n" ~haystack:"abc"
              ~found_at:(-1) ());
        raises_match ~msg:"found_at past the end rejected" Exn.invalid_arg
          (fun () ->
            F.containment ~expected:"d" ~needle:"n" ~haystack:"abc" ~found_at:4
              ());
        check "found_at at the end accepted (empty-needle case)"
          (match
             F.containment ~expected:"d" ~needle:"" ~haystack:"abc" ~found_at:3
               ()
           with
          | _ -> true
          | exception Invalid_argument _ -> false);
        let f = F.containment ~expected:big ~needle:big ~haystack:"abc" () in
        check "needle and description are bounded"
          (match f.F.kind with
          | F.Equality { expected; claim = F.Contains { needle; _ }; _ } ->
              String.length expected < 200_000
              && String.length needle < 200_000
              && has ~needle:"truncated" needle
          | _ -> false));
    test "snapshot constructor" (fun () ->
        let f =
          F.snapshot ~name:"greeting"
            ~path:"test/__snapshots__/t.ml/greeting.snap"
            (F.Mismatch { expected = "hi\n"; actual = "ho\n" })
        in
        check "identity and state stored"
          (match f.F.kind with
          | F.Snapshot
              {
                name = "greeting";
                path = "test/__snapshots__/t.ml/greeting.snap";
                state = F.Mismatch { expected = "hi\n"; actual = "ho\n" };
              } ->
              true
          | _ -> false);
        (* Renderers derive acceptance commands from name and path (Law 3):
           they are identities and must never be truncated. *)
        let path = String.make 100_000 'p' in
        let f = F.snapshot ~name:"n" ~path F.Unresolvable in
        check "path is stored unmodified"
          (match f.F.kind with
          | F.Snapshot { path = p; _ } -> String.equal p path
          | _ -> false);
        let f =
          F.snapshot ~name:"n" ~path:"p"
            (F.Mismatch { expected = big; actual = "a" })
        in
        check "mismatch contents are bounded"
          (match f.F.kind with
          | F.Snapshot { state = F.Mismatch { expected; _ }; _ } ->
              String.length expected < 200_000
          | _ -> false);
        let f = F.snapshot ~name:"n" ~path:"p" (F.Missing { proposed = big }) in
        check "proposed content is bounded"
          (match f.F.kind with
          | F.Snapshot { state = F.Missing { proposed }; _ } ->
              String.length proposed < 200_000
          | _ -> false);
        let first = loc_of "test/a.ml" 3 in
        let second = loc_of "test/b.ml" 9 in
        let f =
          F.snapshot ~loc:second ~name:"n" ~path:"p"
            (F.Duplicate { first = Some first; first_test = "a › t" })
        in
        check "duplicate carries both sites and the first checker"
          (match f.F.kind with
          | F.Snapshot { state = F.Duplicate { first = l; first_test }; _ } ->
              l = Some first && first_test = "a › t" && f.F.loc = Some second
          | _ -> false));
    test "property constructor" (fun () ->
        let inner = F.equality ~expected:"true" ~actual:"false" () in
        let f =
          F.property ~inner ~rendered:"Rect (2, 0)" ~case_index:12
            ~shrink_steps:4 ~root:0x7be1d2c904aa31f5L ~examples:false ()
        in
        check "payload stored"
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
        check "inner defaults to None, examples flag stored"
          (match f.F.kind with
          | F.Property { examples = true; inner = None; _ } -> true
          | _ -> false);
        check "timed_out defaults to None"
          (match f.F.kind with
          | F.Property { timed_out = None; _ } -> true
          | _ -> false);
        let f =
          F.property ~timed_out:0.3 ~rendered:"[]" ~case_index:0 ~shrink_steps:2
            ~root:1L ~examples:false ()
        in
        check "an explicit timed_out limit is stored"
          (match f.F.kind with
          | F.Property { timed_out = Some 0.3; _ } -> true
          | _ -> false));
    test "with_phase and with_output_tail" (fun () ->
        let f = F.message "boom" in
        let g = F.with_phase F.Teardown f in
        check "with_phase: replaces the phase" (g.F.phase = F.Teardown);
        check "with_phase: original unchanged" (f.F.phase = F.Body);
        check "with_phase: kind untouched" (g.F.kind = f.F.kind);
        let tl = F.tail "out" in
        let h = F.with_output_tail tl f in
        check "with_output_tail: attaches the tail" (h.F.output_tail = Some tl);
        check "with_output_tail: original unchanged" (f.F.output_tail = None));
    test "tails" (fun () ->
        let tl = F.tail "hello\n" in
        check "short text kept verbatim"
          (tl.F.text = "hello\n" && tl.F.omitted_bytes = 0
         && tl.F.log_path = None);
        let tl =
          F.tail ~log_path:"_build/_tests/t.output" ~omitted_bytes:7 "x"
        in
        check "log_path and prior omission recorded"
          (tl.F.log_path = Some "_build/_tests/t.output"
          && tl.F.omitted_bytes = 7);
        let line i = Printf.sprintf "[debug] line %05d\n" i in
        let full = String.concat "" (List.init 1_000 line) in
        let tl = F.tail full in
        let kept = String.length tl.F.text in
        check "long output is bounded" (kept < String.length full);
        check_int "omitted accounts for every cut byte"
          ~expected:(String.length full - kept)
          ~actual:tl.F.omitted_bytes;
        check_string "retains the final bytes"
          ~expected:(String.sub full (String.length full - kept) kept)
          ~actual:tl.F.text;
        let tl' = F.tail ~omitted_bytes:11 full in
        check_int "prior omission accumulates"
          ~expected:(String.length full - String.length tl'.F.text + 11)
          ~actual:tl'.F.omitted_bytes;
        (* All-3-byte content: a kept suffix must start at an offset
           divisible by 3 or a UTF-8 sequence was split. *)
        let s = String.concat "" (List.init 3_333 (fun _ -> "\xe2\x82\xac")) in
        let tl = F.tail s in
        check "suffix cut never splits a UTF-8 sequence"
          (tl.F.omitted_bytes mod 3 = 0
          && Char.code tl.F.text.[0] land 0xC0 <> 0x80);
        raises_match ~msg:"negative omitted_bytes rejected" Exn.invalid_arg
          (fun () -> F.tail ~omitted_bytes:(-1) "x"));
    test "outcomes" (fun () ->
        let body = F.message "body failed" in
        let teardown = F.with_phase F.Teardown (F.message "teardown failed") in
        let describe = function
          | F.Pass -> "pass"
          | F.Fail fs -> Printf.sprintf "fail:%d" (List.length fs)
          | F.Skip None -> "skip"
          | F.Skip (Some r) -> "skip:" ^ r
        in
        check_string "pass" ~expected:"pass" ~actual:(describe F.Pass);
        check_string "body and teardown failures are two entries"
          ~expected:"fail:2"
          ~actual:(describe (F.Fail [ body; teardown ]));
        check_string "skip with reason" ~expected:"skip:windows only"
          ~actual:(describe (F.Skip (Some "windows only")));
        check_string "skip without reason" ~expected:"skip"
          ~actual:(describe (F.Skip None)));
    test "control exceptions carry their payloads" (fun () ->
        let f = F.message "boom" in
        check "Check_failure: carries the failure"
          (try raise (F.Check_failure f) with F.Check_failure g -> g == f);
        check "Skip_test: carries the reason"
          (try raise (F.Skip_test (Some "no docker"))
           with F.Skip_test r -> r = Some "no docker");
        check "Timeout: carries the limit"
          (try raise (F.Timeout 2.5) with F.Timeout t -> t = 2.5));
    test "is_fatal: exactly the never-swallowed exceptions" (fun () ->
        check "Sys.Break is fatal" (F.is_fatal Sys.Break);
        check "Out_of_memory is fatal" (F.is_fatal Out_of_memory);
        check "Stack_overflow is fatal" (F.is_fatal Stack_overflow);
        check "Check_failure is not fatal"
          (not (F.is_fatal (F.Check_failure (F.message "boom"))));
        check "an ordinary exception is not fatal" (not (F.is_fatal Not_found)));
  ]
