(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Diff: line hunks (Myers), hunk grouping, size-guard fallbacks,
   character refinement, UTF-8 span safety, and [Diff.sequences] (amendment
   B7) — element splitting of rendered lists and arrays, first-mismatch
   index and differing count, and the conservative Nones. Absorbs v1's
   test_myers.ml and the suite's Distance sections. *)

open Windtrap
module Diff = Windtrap.Private.Diff

let check name cond = is_true ~msg:name cond
let check_int name ~expected ~actual = equal ~msg:name int expected actual
let check_string name ~expected ~actual = equal ~msg:name string expected actual

(* One mismatch side: [None] is an element without a counterpart. *)
let check_side name ~expected ~actual =
  equal ~msg:name (option string) expected actual

(* Rendering helpers (test-side only; Diff itself renders nothing) *)

let show_line = function
  | Diff.Keep s -> " " ^ s
  | Diff.Delete s -> "-" ^ s
  | Diff.Insert s -> "+" ^ s

let show_hunk h =
  Printf.sprintf "@@ -%d,%d +%d,%d @@%s" h.Diff.expected_start
    h.Diff.expected_count h.Diff.actual_start h.Diff.actual_count
    (String.concat "" (List.map (fun l -> "|" ^ show_line l) h.Diff.lines))

let show_hunks hs = String.concat "\n" (List.map show_hunk hs)

let check_hunks name ?context ~expected ~actual pinned =
  check_string name ~expected:pinned
    ~actual:(show_hunks (Diff.hunks ?context ~expected ~actual ()))

(* Mirror of the documented line-splitting semantics. *)
let split_lines s =
  match List.rev (String.split_on_char '\n' s) with
  | "" :: rev_rest -> List.rev rev_rest
  | rev_parts -> List.rev rev_parts

let text_of_lines = function
  | [] -> ""
  | lines -> String.concat "\n" lines ^ "\n"

(* Apply [hunks] to the expected lines: the reconstruction must be exactly
   the actual lines, or the diff data lied. *)
exception Bad_patch of string

let apply_hunks expected_lines hunks =
  let arr = Array.of_list expected_lines in
  let n = Array.length arr in
  let out = ref [] in
  let cursor = ref 0 in
  let copy_until stop =
    while !cursor < stop do
      if !cursor >= n then raise (Bad_patch "copy past end of expected");
      out := arr.(!cursor) :: !out;
      incr cursor
    done
  in
  let consume tag s =
    if !cursor >= n then raise (Bad_patch (tag ^ " past end of expected"));
    if not (String.equal arr.(!cursor) s) then
      raise
        (Bad_patch
           (Printf.sprintf "%s %S but expected has %S" tag s arr.(!cursor)));
    incr cursor
  in
  List.iter
    (fun h ->
      copy_until (h.Diff.expected_start - 1);
      List.iter
        (function
          | Diff.Keep s ->
              consume "keep" s;
              out := s :: !out
          | Diff.Delete s -> consume "delete" s
          | Diff.Insert s -> out := s :: !out)
        h.Diff.lines)
    hunks;
  copy_until n;
  List.rev !out

let count_lines pred lines = List.length (List.filter pred lines)

let hunk_invariants_ok h =
  h.Diff.expected_count
  = count_lines
      (function Diff.Keep _ | Diff.Delete _ -> true | Diff.Insert _ -> false)
      h.Diff.lines
  && h.Diff.actual_count
     = count_lines
         (function
           | Diff.Keep _ | Diff.Insert _ -> true | Diff.Delete _ -> false)
         h.Diff.lines
  && h.Diff.expected_start >= 1 && h.Diff.actual_start >= 1

(* Within every run of changes, deletions must precede insertions. *)
let changes_ordered_ok lines =
  let rec go seen_insert = function
    | [] -> true
    | Diff.Keep _ :: rest -> go false rest
    | Diff.Delete _ :: rest -> (not seen_insert) && go false rest
    | Diff.Insert _ :: rest -> go true rest
  in
  go false lines

(* Reference edit distances, oracles for minimality. [with_sub] is unit-cost
   insert/delete/substitute (what refine's Wagner-Fischer minimizes);
   [no_sub] is insert/delete only (what a minimal Myers script minimizes). *)
let edit_distance ~with_sub equal a b =
  let a = Array.of_list a and b = Array.of_list b in
  let na = Array.length a and nb = Array.length b in
  let grid = Array.make_matrix (na + 1) (nb + 1) 0 in
  for i = 0 to na do
    for j = 0 to nb do
      grid.(i).(j) <-
        (if min i j = 0 then max i j
         else if equal a.(i - 1) b.(j - 1) then grid.(i - 1).(j - 1)
         else
           let base = min grid.(i - 1).(j) grid.(i).(j - 1) in
           1 + if with_sub then min base grid.(i - 1).(j - 1) else base)
    done
  done;
  grid.(na).(nb)

(* Deterministic pseudo-random stream for the randomized law sweeps. *)
let seed = ref 42

let rand n =
  seed := ((!seed * 1103515245) + 12345) land 0x3FFFFFFF;
  !seed mod n

let random_lines max_len alphabet =
  List.init
    (rand (max_len + 1))
    (fun _ -> List.nth alphabet (rand (List.length alphabet)))

(* Refinement helpers *)

let spans l = List.map (fun s -> (s.Diff.start, s.Diff.length)) l

let check_refine name ~expected ~actual pinned =
  (* Annotated: [seq_diff] carries same-named span fields, and being declared
     later it wins type-directed disambiguation. *)
  let show (r : Diff.refinement option) =
    let side l =
      String.concat ";"
        (List.map (fun (s, n) -> Printf.sprintf "%d+%d" s n) (spans l))
    in
    match r with
    | None -> "none"
    | Some r ->
        Printf.sprintf "e[%s] a[%s]"
          (side r.Diff.expected_spans)
          (side r.Diff.actual_spans)
  in
  check_string name ~expected:pinned
    ~actual:(show (Diff.refine ~expected ~actual))

(* [s] with the spanned ranges removed. *)
let remainder s span_list =
  let buf = Buffer.create (String.length s) in
  let rec go i = function
    | [] -> Buffer.add_substring buf s i (String.length s - i)
    | { Diff.start; length } :: rest ->
        Buffer.add_substring buf s i (start - i);
        go (start + length) rest
  in
  go 0 span_list;
  Buffer.contents buf

let utf8_alphabet = [| "a"; "\xc3\xa9"; "\xe2\x82\xac"; "\xf0\x9d\x84\x9e" |]

let boundaries s =
  let n = String.length s in
  let rec go i acc =
    if i >= n then List.rev (n :: acc)
    else
      let len = Uchar.utf_decode_length (String.get_utf_8_uchar s i) in
      go (i + len) (i :: acc)
  in
  go 0 []

let spans_ok s span_list =
  let bs = boundaries s in
  let rec go prev_end = function
    | [] -> true
    | { Diff.start; length } :: rest ->
        length > 0 && start > prev_end && List.mem start bs
        && List.mem (start + length) bs
        && go (start + length) rest
  in
  go (-1) span_list

(* Sequence-diff helpers *)

let get name = function
  | Some d -> d
  | None -> fail (name ^ ": sequences returned None")

let none name = function
  | (None : Diff.seq_diff option) -> ()
  | Some _ -> fail (name ^ ": expected None")

let render w v = Testable.to_string w v

(* Every case below wants the default (spans computed); the [?spans] switch
   and the guarded paths are exercised directly in [span_tests]. *)
let sequences ~expected ~actual = Diff.sequences ~expected ~actual ()

let hunk_tests =
  [
    test "pinned hunk cases" (fun () ->
        check "equal texts have no hunks"
          (Diff.hunks ~expected:"a\nb\n" ~actual:"a\nb\n" () = []);
        check "empty texts have no hunks"
          (Diff.hunks ~expected:"" ~actual:"" () = []);
        check "a single trailing newline is not significant"
          (Diff.hunks ~expected:"a" ~actual:"a\n" () = []);
        check_hunks "single line replaced" ~expected:"a\nb\n" ~actual:"a\nc\n"
          "@@ -1,2 +1,2 @@| a|-b|+c";
        check_hunks "minimal script keeps common lines" ~expected:"1\n2\n3\n"
          ~actual:"1\n3\n4\n" "@@ -1,3 +1,3 @@| 1|-2| 3|+4";
        check_hunks "deletions precede insertions in a change run"
          ~expected:"a\nb\n" ~actual:"x\ny\n" "@@ -1,2 +1,2 @@|-a|-b|+x|+y";
        check_hunks "insertion into empty text" ~expected:"" ~actual:"a\nb\n"
          "@@ -1,0 +1,2 @@|+a|+b";
        check_hunks "deletion to empty text" ~expected:"a\n" ~actual:""
          "@@ -1,1 +1,0 @@|-a";
        check_hunks "appended line" ~expected:"x\n" ~actual:"x\ny\n"
          "@@ -1,1 +1,2 @@| x|+y";
        check_hunks "repeated line at the difference boundary"
          ~expected:"a\nb\na\n" ~actual:"a\na\n" "@@ -1,3 +1,2 @@| a|-b| a");
    test "context grouping" (fun () ->
        check_hunks "context 0 trims to the change" ~context:0
          ~expected:"a\nb\nc\n" ~actual:"a\nx\nc\n" "@@ -2,1 +2,1 @@|-b|+x";
        let expected =
          text_of_lines (List.init 10 (fun i -> Printf.sprintf "l%d" (i + 1)))
        in
        let change l x lines =
          List.map (fun s -> if s = l then x else s) lines
        in
        let actual =
          text_of_lines
            (change "l2" "x2"
               (change "l9" "x9"
                  (List.init 10 (fun i -> Printf.sprintf "l%d" (i + 1)))))
        in
        check_hunks "distant changes become two hunks" ~context:1 ~expected
          ~actual
          "@@ -1,3 +1,3 @@| l1|-l2|+x2| l3\n@@ -8,3 +8,3 @@| l8|-l9|+x9| l10";
        check_hunks "nearby changes merge into one hunk" ~context:1 ~expected
          ~actual:
            (text_of_lines
               (change "l4" "x4"
                  (change "l6" "x6"
                     (List.init 10 (fun i -> Printf.sprintf "l%d" (i + 1))))))
          "@@ -3,5 +3,5 @@| l3|-l4|+x4| l5|-l6|+x6| l7";
        raises_match ~msg:"negative context is rejected" Exn.invalid_arg
          (fun () -> Diff.hunks ~context:(-1) ~expected:"a" ~actual:"b" ()));
    test "size guards fall back to a complete diff" (fun () ->
        (* Differing region of 2,400 lines: above the line guard, reported
           as all deletions then all insertions — complete, never omitted. *)
        let mid_e = List.init 1_200 (Printf.sprintf "e%d") in
        let mid_a = List.init 1_200 (Printf.sprintf "a%d") in
        let expected = text_of_lines (("top" :: mid_e) @ [ "bottom" ]) in
        let actual = text_of_lines (("top" :: mid_a) @ [ "bottom" ]) in
        let hs = Diff.hunks ~expected ~actual () in
        check "line guard: one hunk" (List.length hs = 1);
        (match hs with
        | [ h ] ->
            let deletes =
              count_lines
                (function Diff.Delete _ -> true | _ -> false)
                h.Diff.lines
            and inserts =
              count_lines
                (function Diff.Insert _ -> true | _ -> false)
                h.Diff.lines
            in
            check "line guard: every line reported"
              (deletes = 1_200 && inserts = 1_200);
            check "line guard: deletions precede insertions"
              (changes_ordered_ok h.Diff.lines);
            check "line guard: patch still reconstructs the actual text"
              (apply_hunks (split_lines expected) hs = split_lines actual)
        | _ -> ());
        (* 1,200 differing middle lines is under the line guard, but the
           minimal script needs 1,200 edits — above the edit cap, same
           complete fallback. *)
        let expected =
          text_of_lines ("s" :: List.init 600 (Printf.sprintf "e%d"))
        in
        let actual =
          text_of_lines ("s" :: List.init 600 (Printf.sprintf "a%d"))
        in
        let hs = Diff.hunks ~expected ~actual () in
        check "edit cap: patch reconstructs the actual text"
          (apply_hunks (split_lines expected) hs = split_lines actual);
        check "edit cap: ordering holds"
          (List.for_all (fun h -> changes_ordered_ok h.Diff.lines) hs));
    test "randomized hunk laws (300 cases)" (fun () ->
        let alphabet = [ "a"; "b"; "c" ] in
        for _ = 1 to 300 do
          let e_lines = random_lines 10 alphabet in
          let a_lines =
            if rand 2 = 0 then random_lines 10 alphabet
            else
              (* near copy: mutate up to two lines *)
              List.map
                (fun s -> if rand 5 = 0 then List.nth alphabet (rand 3) else s)
                e_lines
          in
          let expected = text_of_lines e_lines
          and actual = text_of_lines a_lines in
          let context = rand 4 in
          let hs = Diff.hunks ~context ~expected ~actual () in
          let flag name cond =
            if not cond then
              failf "%s\n  expected: %S\n  actual:   %S" name expected actual
          in
          flag "empty iff equal" (hs = [] = (e_lines = a_lines));
          flag "invariants" (List.for_all hunk_invariants_ok hs);
          flag "ordering"
            (List.for_all (fun h -> changes_ordered_ok h.Diff.lines) hs);
          flag "patch"
            (match apply_hunks e_lines hs with
            | reconstructed -> reconstructed = a_lines
            | exception Bad_patch _ -> false);
          (* Inputs are far below the guards, so the script must be minimal:
             exactly as many changed lines as the insert/delete edit
             distance. *)
          let changes =
            List.fold_left
              (fun n h ->
                n
                + count_lines
                    (function
                      | Diff.Delete _ | Diff.Insert _ -> true | _ -> false)
                    h.Diff.lines)
              0 hs
          in
          flag "minimality"
            (changes
            = edit_distance ~with_sub:false String.equal e_lines a_lines)
        done);
  ]

let refine_tests =
  [
    test "refinement: pinned cases" (fun () ->
        check_refine "equal strings have empty spans" ~expected:"abc"
          ~actual:"abc" "e[] a[]";
        check_refine "empty strings have empty spans" ~expected:"" ~actual:""
          "e[] a[]";
        check_refine "substitution marks both sides" ~expected:"abc"
          ~actual:"axc" "e[1+1] a[1+1]";
        check_refine "insertion marks only the actual side" ~expected:"ac"
          ~actual:"abc" "e[] a[1+1]";
        check_refine "deletion marks only the expected side" ~expected:"abc"
          ~actual:"ac" "e[1+1] a[]";
        check_refine "adjacent changes coalesce" ~expected:"abcd" ~actual:"axyd"
          "e[1+2] a[1+2]";
        check_refine "separate changes stay separate" ~expected:"abcde"
          ~actual:"xbcdy" "e[0+1;4+1] a[0+1;4+1]";
        check_refine "repeated element at the difference boundary"
          ~expected:"121" ~actual:"11" "e[1+1] a[]";
        check_refine "tail insertion" ~expected:"aaaa" ~actual:"aaaab"
          "e[] a[4+1]";
        check_refine "fully different strings are noise" ~expected:"abc"
          ~actual:"xyz" "none";
        check_refine "growing from empty is noise" ~expected:"" ~actual:"abc"
          "none";
        check_refine "exactly at the noise threshold still refines"
          ~expected:"abcde" ~actual:"vwxye" "e[0+4] a[0+4]";
        (* é is 2 bytes; the changed span covers the whole sequence on both
           sides. *)
        check_refine "multi-byte characters are never split"
          ~expected:"h\xc3\xa9llo" ~actual:"h\xc3\xa4llo" "e[1+2] a[1+2]";
        check_refine "multi-byte against single-byte" ~expected:"a\xc3\xa9"
          ~actual:"aa" "e[1+2] a[1+1]");
    test "refinement: cell guard" (fun () ->
        (* Differing region above the cell guard: refinement declines, even
           though only two characters differ. *)
        let mid = String.make 2_101 'm' in
        check "oversized differing region declines"
          (Diff.refine ~expected:("A" ^ mid ^ "B") ~actual:("C" ^ mid ^ "D")
          = None);
        (* Common-outer stripping keeps the same-sized inputs refinable when
           the differing region is small. *)
        check "stripping saves same-sized inputs"
          (Diff.refine ~expected:(mid ^ "A") ~actual:(mid ^ "B") <> None));
    test "refinement: unmarked characters agree, marks minimal (exhaustive <=4)"
      (fun () ->
        (* The spans are the changed regions, so the unmarked characters of
           the two sides must be equal, or the highlight lies. Exhaustive
           over all string pairs of length <= 4 on a 3-letter alphabet
           (121^2 = 14,641 pairs). *)
        let strings =
          let rec go n =
            if n = 0 then [ "" ]
            else
              let rest = go (n - 1) in
              rest
              @ List.concat_map
                  (fun s -> [ s ^ "a"; s ^ "b"; s ^ "c" ])
                  (List.filter (fun s -> String.length s = n - 1) rest)
          in
          go 4
        in
        List.iter
          (fun e ->
            List.iter
              (fun a ->
                match Diff.refine ~expected:e ~actual:a with
                | None -> ()
                | Some r ->
                    let re = remainder e r.Diff.expected_spans in
                    let ra = remainder a r.Diff.actual_spans in
                    (* One-byte alphabet: bytes = code points, so span
                       lengths count marked code points. A minimal script
                       marks at most [distance] code points per side. *)
                    let marked l =
                      List.fold_left (fun n s -> n + s.Diff.length) 0 l
                    in
                    let chars s = List.init (String.length s) (String.get s) in
                    let d =
                      edit_distance ~with_sub:true Char.equal (chars e)
                        (chars a)
                    in
                    if
                      (not (String.equal re ra))
                      || marked r.Diff.expected_spans > d
                      || marked r.Diff.actual_spans > d
                    then
                      failf
                        "BAD refine: e=%S a=%S unmarked-e=%S unmarked-a=%S d=%d"
                        e a re ra d)
              strings)
          strings);
    test "refinement: UTF-8 span safety (randomized, 300 cases)" (fun () ->
        let some_count = ref 0 in
        for _ = 1 to 300 do
          let base = List.init (3 + rand 8) (fun _ -> utf8_alphabet.(rand 4)) in
          let mutated =
            List.map
              (fun s -> if rand 4 = 0 then utf8_alphabet.(rand 4) else s)
              base
          in
          let expected = String.concat "" base in
          let actual = String.concat "" mutated in
          match Diff.refine ~expected ~actual with
          | None -> ()
          | Some r ->
              incr some_count;
              if
                not
                  (spans_ok expected r.Diff.expected_spans
                  && spans_ok actual r.Diff.actual_spans)
              then
                failf "bad spans\n  expected: %S\n  actual:   %S" expected
                  actual
        done;
        check "randomized refine exercised the Some path" (!some_count > 50));
  ]

let sequence_tests =
  [
    test "sequences: first mismatch of a long rendered list" (fun () ->
        let expected = List.init 100 (fun i -> i) in
        let actual =
          List.map
            (fun i -> if i = 37 || i = 70 || i = 71 then i + 1000 else i)
            expected
        in
        let d =
          get "first mismatch"
            (sequences
               ~expected:(render Testable.(list int) expected)
               ~actual:(render Testable.(list int) actual))
        in
        check "kind is list" (d.Diff.kind = `List);
        check_int "lengths: expected" ~expected:100
          ~actual:d.Diff.expected_length;
        check_int "lengths: actual" ~expected:100 ~actual:d.Diff.actual_length;
        check_int "differing count" ~expected:3 ~actual:d.Diff.differing;
        match d.Diff.first with
        | None -> check "first mismatch present" false
        | Some m ->
            check_int "first index" ~expected:37 ~actual:m.Diff.index;
            check_side "first expected element" ~expected:(Some "37")
              ~actual:m.Diff.expected;
            check_side "first actual element" ~expected:(Some "1037")
              ~actual:m.Diff.actual);
    test "sequences: arrays" (fun () ->
        let expected = Array.init 20 (fun i -> float_of_int i) in
        let actual = Array.copy expected in
        actual.(4) <- 99.5;
        let d =
          get "arrays"
            (sequences
               ~expected:(render Testable.(array float_exact) expected)
               ~actual:(render Testable.(array float_exact) actual))
        in
        check "kind is array" (d.Diff.kind = `Array);
        check_int "array differing" ~expected:1 ~actual:d.Diff.differing;
        match d.Diff.first with
        | Some m ->
            check_int "array first index" ~expected:4 ~actual:m.Diff.index;
            check_side "array first actual" ~expected:(Some "99.5")
              ~actual:m.Diff.actual
        | None -> check "array first present" false);
    test "sequences: wrap-insensitive comparison" (fun () ->
        (* Identical elements must compare equal wherever the renderer's
           line breaks fell: a one-element edit shifts every later wrap
           point. *)
        let expected = List.init 60 (fun i -> Printf.sprintf "field-%d" i) in
        let actual =
          List.map
            (fun s -> if s = "field-30" then "CHANGED-30" else s)
            expected
        in
        let re = render Testable.(list string) expected
        and ra = render Testable.(list string) actual in
        check "renderings actually wrap" (String.contains re '\n');
        check "wrap points differ"
          (String.length re <> String.length ra
          || re <> String.sub ra 0 (String.length re));
        let d = get "wrap-insensitive" (sequences ~expected:re ~actual:ra) in
        check_int "wrap-insensitive differing" ~expected:1
          ~actual:d.Diff.differing;
        (match d.Diff.first with
        | Some m ->
            check_int "wrap-insensitive index" ~expected:30 ~actual:m.Diff.index;
            check_side "wrap-insensitive expected element"
              ~expected:(Some {|"field-30"|}) ~actual:m.Diff.expected
        | None -> check "wrap-insensitive first present" false);
        (* Wrap points move {e inside} nested elements too: a long inner
           list breaks across lines at positions that shift with the edit,
           and canonicalization must still make the untouched elements
           compare equal. *)
        let w = Testable.(list (list int)) in
        let expected =
          List.init 10 (fun i -> List.init 20 (fun j -> (100 * i) + j))
        in
        let actual =
          List.mapi
            (fun i xs -> if i = 6 then List.map (fun x -> x + 1) xs else xs)
            expected
        in
        let re = render w expected and ra = render w actual in
        check "nested renderings actually wrap" (String.contains re '\n');
        let d = get "nested wrap" (sequences ~expected:re ~actual:ra) in
        check_int "nested wrap differing" ~expected:1 ~actual:d.Diff.differing;
        match d.Diff.first with
        | Some m ->
            check_int "nested wrap index" ~expected:6 ~actual:m.Diff.index
        | None -> check "nested wrap first present" false);
    test "sequences: length-only and empty differences" (fun () ->
        (* Aligned counting: a pure suffix insertion is that many differing
           elements — one each — and the first is the first unmatched one. *)
        let d =
          get "length difference"
            (sequences
               ~expected:(render Testable.(list int) [ 1; 2; 3 ])
               ~actual:(render Testable.(list int) [ 1; 2; 3; 4; 5 ]))
        in
        check_int "shorter length" ~expected:3 ~actual:d.Diff.expected_length;
        check_int "longer length" ~expected:5 ~actual:d.Diff.actual_length;
        check_int "inserted elements each count once" ~expected:2
          ~actual:d.Diff.differing;
        (match d.Diff.first with
        | Some m ->
            check_int "suffix-insertion first index" ~expected:3
              ~actual:m.Diff.index;
            check_side "suffix-insertion has no expected side" ~expected:None
              ~actual:m.Diff.expected;
            check_side "suffix-insertion actual side" ~expected:(Some "4")
              ~actual:m.Diff.actual
        | None -> check "suffix-insertion first present" false);
        let d =
          get "empty vs one"
            (sequences ~expected:"[]"
               ~actual:(render Testable.(list int) [ 7 ]))
        in
        check_int "empty length" ~expected:0 ~actual:d.Diff.expected_length;
        check_int "one length" ~expected:1 ~actual:d.Diff.actual_length;
        let d =
          get "empty arrays" (sequences ~expected:"[||]" ~actual:"[||]")
        in
        check "empty arrays kind" (d.Diff.kind = `Array);
        check_int "empty arrays differing" ~expected:0 ~actual:d.Diff.differing;
        (* Same elements, different layout: parses, but nothing differs —
           the renderer falls back rather than claiming a difference. *)
        let d =
          get "layout-only"
            (sequences ~expected:"[1; 2; 3]" ~actual:"[1;\n 2;\n 3]")
        in
        check_int "layout-only differing" ~expected:0 ~actual:d.Diff.differing;
        check "layout-only no first" (d.Diff.first = None));
    test "sequences: element grammar" (fun () ->
        let w = Testable.(list (pair string (list int))) in
        let expected = [ ("a; b", [ 1; 2 ]); ("c", [ 3 ]) ] in
        let actual = [ ("a; b", [ 1; 2 ]); ("c", [ 4 ]) ] in
        let d =
          get "nested"
            (sequences ~expected:(render w expected) ~actual:(render w actual))
        in
        check_int "separator inside %S string does not split" ~expected:2
          ~actual:d.Diff.expected_length;
        check_int "nested differing" ~expected:1 ~actual:d.Diff.differing;
        (match d.Diff.first with
        | Some m ->
            check_int "nested index" ~expected:1 ~actual:m.Diff.index;
            check_side "nested element rendering"
              ~expected:(Some {|("c", [4])|}) ~actual:m.Diff.actual
        | None -> check "nested first present" false);
        let w = Testable.(list string) in
        let d =
          get "escaped quote"
            (sequences
               ~expected:(render w [ {|say "hi"; bye|}; "x" ])
               ~actual:(render w [ {|say "hi"; bye|}; "y" ]))
        in
        check_int "escaped-quote element count" ~expected:2
          ~actual:d.Diff.expected_length;
        check_int "escaped-quote differing" ~expected:1 ~actual:d.Diff.differing;
        (* Strings whose rendering ends in an escaped backslash right before
           the closing quote: a lookbehind scanner ("a quote after a
           backslash never closes") reads past the real close and
           misparses; the forward escape scan must not. Also covers a
           backslash escape directly before an escaped quote mid-string. *)
        let d =
          get "escaped backslash"
            (sequences
               ~expected:(render w [ {|tail\|}; {|back\slash"|}; "b" ])
               ~actual:(render w [ {|tail\|}; {|back\slash"|}; "c" ]))
        in
        check_int "escaped-backslash element count" ~expected:3
          ~actual:d.Diff.expected_length;
        check_int "escaped-backslash differing" ~expected:1
          ~actual:d.Diff.differing;
        (match d.Diff.first with
        | Some m ->
            check_int "escaped-backslash index" ~expected:2 ~actual:m.Diff.index
        | None -> check "escaped-backslash first present" false);
        let w = Testable.(list char) in
        let expected = [ '['; ';'; '\n'; '\'' ] in
        let actual = [ '['; ';'; '\t'; '\'' ] in
        let d =
          get "char literals"
            (sequences ~expected:(render w expected) ~actual:(render w actual))
        in
        check_int "char element count" ~expected:4
          ~actual:d.Diff.expected_length;
        check_int "char differing" ~expected:1 ~actual:d.Diff.differing;
        match d.Diff.first with
        | Some m ->
            check_int "char index" ~expected:2 ~actual:m.Diff.index;
            check_side "char expected element" ~expected:(Some {|'\n'|})
              ~actual:m.Diff.expected
        | None -> check "char first present" false);
    test "sequences: alignment counts elements, not positions" (fun () ->
        (* The multiset truth of the slist fixture ([1..10] vs sorted
           [2..10; 42]): one element removed, one added — two differing
           elements, never "10 of 10" from the positional shift (D5 §3). *)
        let d =
          get "shifted equal-length lists"
            (sequences
               ~expected:
                 (render Testable.(list int) (List.init 10 (fun i -> i + 1)))
               ~actual:
                 (render
                    Testable.(list int)
                    (List.init 9 (fun i -> i + 2) @ [ 42 ])))
        in
        check_int "aligned differing count" ~expected:2 ~actual:d.Diff.differing;
        (match d.Diff.first with
        | Some m ->
            check_int "expected-only first index" ~expected:0
              ~actual:m.Diff.index;
            check_side "expected-only first: expected side" ~expected:(Some "1")
              ~actual:m.Diff.expected;
            check_side "expected-only first: no actual side" ~expected:None
              ~actual:m.Diff.actual
        | None -> check "expected-only first present" false);
        (* The mirror image: the unmatched element sits on the actual side. *)
        let d =
          get "actual-only first"
            (sequences
               ~expected:
                 (render Testable.(list int) (List.init 9 (fun i -> i + 2)))
               ~actual:
                 (render Testable.(list int) (List.init 10 (fun i -> i + 1))))
        in
        check_int "actual-only differing" ~expected:1 ~actual:d.Diff.differing;
        (match d.Diff.first with
        | Some m ->
            check_int "actual-only first index" ~expected:0 ~actual:m.Diff.index;
            check_side "actual-only first: no expected side" ~expected:None
              ~actual:m.Diff.expected;
            check_side "actual-only first: actual side" ~expected:(Some "1")
              ~actual:m.Diff.actual
        | None -> check "actual-only first present" false);
        (* A middle insertion is one difference, not a shifted disagreement
           at every later index. *)
        let expected = List.init 20 (fun i -> i) in
        let actual =
          List.init 10 (fun i -> i) @ [ 999 ] @ List.init 10 (fun i -> i + 10)
        in
        let d =
          get "middle insertion"
            (sequences
               ~expected:(render Testable.(list int) expected)
               ~actual:(render Testable.(list int) actual))
        in
        check_int "middle insertion is one difference" ~expected:1
          ~actual:d.Diff.differing;
        match d.Diff.first with
        | Some m ->
            check_int "middle insertion index" ~expected:10 ~actual:m.Diff.index;
            check_side "middle insertion side" ~expected:(Some "999")
              ~actual:m.Diff.actual
        | None -> check "middle insertion first present" false);
    test "sequences: conservative Nones" (fun () ->
        none "plain scalars" (sequences ~expected:"true" ~actual:"false");
        none "records" (sequences ~expected:"{x = 1}" ~actual:"{x = 2}");
        none "kind mismatch" (sequences ~expected:"[1; 2]" ~actual:"[|1; 2|]");
        none "one side not a sequence"
          (sequences ~expected:"[1; 2]" ~actual:"boom");
        none "unbalanced bracket in an element"
          (sequences ~expected:"[1; (2]" ~actual:"[1; 3]");
        none "unterminated string in an element"
          (sequences ~expected:{|["a; b]|} ~actual:"[1]");
        none "empty element" (sequences ~expected:"[1;; 2]" ~actual:"[1; 2]");
        none "trailing separator" (sequences ~expected:"[1; 2;]" ~actual:"[1]");
        none "empty strings" (sequences ~expected:"" ~actual:""));
  ]

(* Element-grain highlight spans.

   The law these pin: a span never crosses an element boundary. Character
   refinement over the same renderings is free to do exactly that (and did,
   before the spans existed) — [marks] below reads the marked substrings
   back out, so a regression shows up as a fragment like ["; 5]); (\"carol\",
   ["] instead of a whole element. *)

let marks s spans =
  String.concat "|"
    (List.map (fun { Diff.start; length } -> String.sub s start length) spans)

let check_marks name ~expected ~actual (pin_e, pin_a) =
  let d = get name (sequences ~expected ~actual) in
  check_string
    (name ^ ": expected marks")
    ~expected:pin_e
    ~actual:(marks expected d.Diff.expected_spans);
  check_string (name ^ ": actual marks") ~expected:pin_a
    ~actual:(marks actual d.Diff.actual_spans)

let span_tests =
  [
    test "spans: whole elements, never across a boundary" (fun () ->
        check_marks "replacement" ~expected:"[1; 2; 3]" ~actual:"[1; 2; 4]"
          ("3", "4");
        check_marks "insertion" ~expected:"[1; 2; 3]" ~actual:"[1; 9; 2; 3]"
          ("", "9");
        check_marks "deletion" ~expected:"[1; 2; 3; 4]" ~actual:"[1; 3; 4]"
          ("2", "");
        (* The case a character-minimal script calls "everything changed". *)
        check_marks "shift" ~expected:"[1; 2; 3; 4]" ~actual:"[2; 3; 4; 5]"
          ("1", "5");
        check_marks "append" ~expected:{|["alpha"; "beta"]|}
          ~actual:{|["alpha"; "beta"; "gamma"]|} ("", {|"gamma"|});
        check_marks "arrays" ~expected:"[|1; 2; 3|]" ~actual:"[|1; 5; 3|]"
          ("2", "5"));
    test "spans: nested elements stay whole" (fun () ->
        check_marks "nested" ~expected:{|[("alice", [1; 2; 3]); ("bob", [4])]|}
          ~actual:{|[("alice", [1; 2; 3]); ("bob", [4; 5]); ("carol", [])]|}
          ({|("bob", [4])|}, {|("bob", [4; 5])|("carol", [])|}));
    test "spans: refinement inside a replaced element" (fun () ->
        (* Localized and small: the mark narrows to the differing word. *)
        check_marks "one word of a long element"
          ~expected:{|["the quick brown fox"; "jumps over"; "the lazy dog"]|}
          ~actual:{|["the quick brown fox"; "jumps over"; "the lazy cat"]|}
          ("dog", "cat");
        (* Scattered and broad: refining ["None"] against ["Some 2"] clears
           the noise threshold but marks three quarters of the element, so
           the whole element wins instead. *)
        check_marks "scattered refinement is refused"
          ~expected:"[Some 1; None; Some 3]" ~actual:"[Some 1; Some 2; Some 3]"
          ("None", "Some 2");
        (* A pure insertion inside a replaced element would mark the actual
           side only, which is how an inserted element renders; both
           endpoints are marked instead. *)
        check_marks "one-sided refinement is refused" ~expected:"[4; 5; 6]"
          ~actual:"[4; 55; 6]" ("5", "55"));
    test "spans: no marks when the sequences agree" (fun () ->
        let d =
          get "layout only" (sequences ~expected:"[1; 2]" ~actual:"[1;\n 2]")
        in
        check_int "differing" ~expected:0 ~actual:d.Diff.differing;
        check "expected unmarked" (d.Diff.expected_spans = []);
        check "actual unmarked" (d.Diff.actual_spans = []));
    test "spans: obey the span contract" (fun () ->
        (* Same invariants [refine] spans carry: ascending, disjoint,
           non-empty, on code-point boundaries. *)
        let e = {|["é"; "€"; "a"]|} and a = {|["é"; "x"; "b"]|} in
        let d = get "utf8" (sequences ~expected:e ~actual:a) in
        check "expected spans well-formed" (spans_ok e d.Diff.expected_spans);
        check "actual spans well-formed" (spans_ok a d.Diff.actual_spans);
        (* Both pairs refine to the differing code point inside the quotes:
           coverage counts code points, so the multi-byte element is judged
           the same as the ASCII one (1 of 3 either way). *)
        check_string "utf8 expected marks" ~expected:"€|a"
          ~actual:(marks e d.Diff.expected_spans);
        check_string "utf8 actual marks" ~expected:"x|b"
          ~actual:(marks a d.Diff.actual_spans));
  ]

(* The guards on span computation.

   Per-element refinement is the expensive half, and before the budget
   existed each replaced pair got its own [dp_cell_limit], so a payload
   [refine] declines in milliseconds took seconds here — 3.5s for 32
   elements of 1900 characters, measured. These pin the three ways that
   cannot happen: the caller can decline spans outright, the pairs share
   one budget, and the size-guarded alignment carries no spans. *)

let long_sequence k w ~salt =
  let seed = ref salt in
  let rnd () =
    seed := ((!seed * 1103515245) + 12345) land 0x3FFFFFFF;
    !seed
  in
  let word () = String.init w (fun _ -> Char.chr (97 + (rnd () mod 26))) in
  "[" ^ String.concat "; " (List.init k (fun _ -> "\"" ^ word () ^ "\"")) ^ "]"

let guard_tests =
  [
    test "spans: the caller can decline them" (fun () ->
        let expected = "[1; 2; 3]" and actual = "[1; 2; 4]" in
        let with_spans = get "with" (Diff.sequences ~expected ~actual ()) in
        let without =
          get "without" (Diff.sequences ~spans:false ~expected ~actual ())
        in
        (* Declining changes only the spans; every summary field is the
           same, so a caller that will not display marks loses nothing. *)
        check "summary is unaffected"
          (without.Diff.differing = with_spans.Diff.differing
          && without.Diff.expected_length = with_spans.Diff.expected_length
          && without.Diff.actual_length = with_spans.Diff.actual_length
          && without.Diff.first = with_spans.Diff.first);
        check "declined spans are empty"
          (without.Diff.expected_spans = [] && without.Diff.actual_spans = []);
        check "requested spans are not" (with_spans.Diff.expected_spans <> []));
    test "spans: replaced pairs share one refinement budget" (fun () ->
        (* Every element replaced, each far too big to refine within the
           shared budget: marks fall back to whole elements rather than
           refining the leading pairs and marking the rest. *)
        let expected = long_sequence 32 1900 ~salt:1
        and actual = long_sequence 32 1900 ~salt:2 in
        let d = get "over budget" (Diff.sequences ~expected ~actual ()) in
        check_int "all elements differ" ~expected:32 ~actual:d.Diff.differing;
        check_int "one mark per element (expected)" ~expected:32
          ~actual:(List.length d.Diff.expected_spans);
        check_int "one mark per element (actual)" ~expected:32
          ~actual:(List.length d.Diff.actual_spans);
        (* Whole elements: each mark spans its element's rendering, quotes
           included, not some refined subset of it. *)
        check "marks are whole elements"
          (List.for_all
             (fun { Diff.length; _ } -> length = 1902)
             d.Diff.expected_spans);
        check "spans still obey the contract"
          (spans_ok expected d.Diff.expected_spans
          && spans_ok actual d.Diff.actual_spans));
    test "spans: a size-guarded alignment carries none" (fun () ->
        (* Past the element-count guard the alignment degrades to one
           delete-all/insert-all block. Its pairs are an artifact of the
           fallback, so marking from them would report "everything changed"
           — the exact failure element grain exists to prevent. *)
        let seq n from =
          "["
          ^ String.concat "; " (List.init n (fun i -> string_of_int (from + i)))
          ^ "]"
        in
        let expected = seq 1200 0 and actual = seq 1200 5000 in
        let d = get "guarded" (Diff.sequences ~expected ~actual ()) in
        check "guarded alignment reports no marks"
          (d.Diff.expected_spans = [] && d.Diff.actual_spans = []);
        (* Below the guard the same shape does carry marks. *)
        let expected = seq 100 0 and actual = seq 100 5000 in
        let d = get "unguarded" (Diff.sequences ~expected ~actual ()) in
        check "an unguarded alignment does carry marks"
          (d.Diff.expected_spans <> []));
  ]

(* Refinement's allocation per grid cell.

   [wagner_fischer] calls [cp_equal] once per cell, so anything [cp_equal]
   allocates is multiplied by the grid. A local [let rec] closing over the
   offsets used to put a closure there — ~8 minor words per cell, 194M words
   for the sweep below, against 9M once the loop was lifted to a top-level
   function. Timing is too machine-dependent to assert, but minor-word counts
   are deterministic, so this pins the shape: per-cell allocation stays O(1)
   words and well under the closure regime. The bound is loose on purpose —
   it is here to catch a reintroduced per-cell allocation, not to freeze the
   current figure. *)
let alloc_tests =
  [
    test "refine allocates no closure per grid cell" (fun () ->
        let sizes = [ (200, 20); (600, 3) ] in
        let cells =
          List.fold_left (fun acc (n, it) -> acc + (n * n * it)) 0 sizes
        in
        let before = Gc.minor_words () in
        List.iter
          (fun (n, iters) ->
            let a = String.make n 'a' in
            let b =
              String.mapi (fun i c -> if i mod 10 = 0 then 'b' else c) a
            in
            for _ = 1 to iters do
              ignore (Sys.opaque_identity (Diff.refine ~expected:a ~actual:b))
            done)
          sizes;
        let words = Gc.minor_words () -. before in
        let per_cell = words /. float_of_int cells in
        check
          (Printf.sprintf "per-cell minor words (%.2f) stays under 2" per_cell)
          (per_cell < 2.));
  ]

let tests =
  hunk_tests @ refine_tests @ sequence_tests @ span_tests @ guard_tests
  @ alloc_tests
