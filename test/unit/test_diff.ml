(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Diff = Windtrap.Private.Diff

let strf = Printf.sprintf
let of_lines = function [] -> "" | lines -> String.concat "\n" lines ^ "\n"

let numbered prefix ~first n =
  List.init n (fun i -> strf "%s%d" prefix (first + i))

(* [edited n ks] is the lines [l1] to [ln], with [lk] as [xk] for each [k] of
   [ks]. *)
let edited n ks =
  List.init n (fun i ->
      let k = i + 1 in
      strf "%s%d" (if List.mem k ks then "x" else "l") k)

let distance ~substitutes a b =
  let a = Array.of_list a and b = Array.of_list b in
  let na = Array.length a and nb = Array.length b in
  let d = Array.make_matrix (na + 1) (nb + 1) 0 in
  for i = 0 to na do
    for j = 0 to nb do
      d.(i).(j) <-
        (if min i j = 0 then max i j
         else if String.equal a.(i - 1) b.(j - 1) then d.(i - 1).(j - 1)
         else
           let edit = min d.(i - 1).(j) d.(i).(j - 1) in
           1 + if substitutes then min edit d.(i - 1).(j - 1) else edit)
    done
  done;
  d.(na).(nb)

(* [near_copy g xs] is [xs] with each element replaced by a draw of [g] one
   time in five. *)
let near_copy g xs =
  Gen.map
    (List.map2 (fun x edit -> Option.value edit ~default:x) xs)
    (Gen.list
       ~size:(Gen.constant (List.length xs))
       (Gen.frequency [ (4, Gen.constant None); (1, Gen.map Option.some g) ]))

(* Line hunks *)

let unified_line = function
  | Diff.Keep s -> " " ^ s
  | Delete s -> "-" ^ s
  | Insert s -> "+" ^ s

let head (h : Diff.hunk) =
  strf "@@ -%d,%d +%d,%d @@" h.expected_start h.expected_count h.actual_start
    h.actual_count

let unified (h : Diff.hunk) =
  head h ^ String.concat "" (List.map (fun l -> "|" ^ unified_line l) h.lines)

let hunks ?context expected actual =
  List.map unified (Diff.hunks ?context ~expected ~actual ())

let hunks_row (_, (context, expected, actual, rows)) =
  equal (list string) rows (hunks ?context expected actual)

let kind = function Diff.Keep _ -> 'k' | Delete _ -> 'd' | Insert _ -> 'i'
let kinds (h : Diff.hunk) = String.of_seq (List.to_seq (List.map kind h.lines))

(* [runs h] is [h]'s head, then its lines as runs of one kind. *)
let runs (h : Diff.hunk) =
  let word = function 'k' -> "kept" | 'd' -> "deleted" | _ -> "inserted" in
  let add acc c =
    match acc with
    | (c', n) :: acc when Char.equal c c' -> (c, n + 1) :: acc
    | acc -> (c, 1) :: acc
  in
  let runs = List.rev (String.fold_left add [] (kinds h)) in
  head h ^ " "
  ^ String.concat ", " (List.map (fun (c, n) -> strf "%d %s" n (word c)) runs)

let ends ~first ~last common = (first :: common) @ [ last ]

(* One changed line at each end of a region of [n] lines per side, and [m] on
   the actual side: four edits, or five when [m = n + 1]. *)
let ends_region n m =
  ( ends ~first:"e0" ~last:"e1" (numbered "c" ~first:1 (n - 2)),
    ends ~first:"a0" ~last:"a1" (numbered "c" ~first:1 (m - 2)) )

(* Two runs of [k] changed lines apart by ten kept ones: [4 * k] edits, one
   more with [extra]. *)
let two_runs ?(extra = []) k =
  let side a b =
    numbered a ~first:1 k @ numbered "c" ~first:1 10 @ numbered b ~first:1 k
  in
  (side "e" "f" @ extra, side "a" "g")

let around =
  let common prefix = numbered prefix ~first:1 5 in
  let side name = common "t" @ numbered name ~first:1 1_200 @ common "b" in
  (side "e", side "a")

let expected_side (h : Diff.hunk) =
  List.filter_map
    (function Diff.Keep s | Delete s -> Some s | Insert _ -> None)
    h.lines

let actual_side (h : Diff.hunk) =
  List.filter_map
    (function Diff.Keep s | Insert s -> Some s | Delete _ -> None)
    h.lines

(* [patch ~start ~count ~own ~other hunks lines] checks that the range of
   each hunk in [lines], its [own] side, holds its [own] lines, and replaces it
   by its [other] lines. *)
let patch ~start ~count ~own ~other hunks lines =
  let lines = Array.of_list lines in
  let sub first last = Array.to_list (Array.sub lines first (last - first)) in
  let rec go cursor acc = function
    | [] -> List.rev_append acc (sub cursor (Array.length lines))
    | h :: hunks ->
        let first = start h - 1 in
        let last = first + count h in
        at_least int ~than:cursor first;
        equal (list string) (own h) (sub first last);
        go last
          (List.rev_append (other h) (List.rev_append (sub cursor first) acc))
          hunks
  in
  go 0 [] hunks

let patches e a _ hs =
  let forward =
    patch
      ~start:(fun (h : Diff.hunk) -> h.expected_start)
      ~count:(fun h -> h.expected_count)
      ~own:expected_side ~other:actual_side hs e
  and backward =
    patch
      ~start:(fun (h : Diff.hunk) -> h.actual_start)
      ~count:(fun h -> h.actual_count)
      ~own:actual_side ~other:expected_side hs a
  in
  equal (list string) a forward;
  equal (list string) e backward

let bound_row (_, ((e, a), rows)) =
  let hs = Diff.hunks ~expected:(of_lines e) ~actual:(of_lines a) () in
  equal (list string) rows (List.map runs hs);
  patches e a () hs

(* Three letters, so that lines repeat and align often. *)
let lines_case =
  let line = Gen.of_list [ "a"; "b"; "c" ] in
  let lines = Gen.list ~size:(Gen.int_range 0 12) line in
  Gen.with_pp
    (fun ppf (e, a, context) ->
      Format.fprintf ppf "expected %S, actual %S, context %d" (of_lines e)
        (of_lines a) context)
    (let open Gen in
     let* e = lines in
     let+ a = one_of [ lines; near_copy line e ] and+ context = int_range 0 3 in
     (e, a, context))

(* Where a line repeats at the edge of the change, the common first and last
   lines could claim it twice. *)
let lines_examples =
  [
    ([ "a" ], [ "a"; "a" ], 3);
    ([ "a"; "a" ], [ "a" ], 3);
    ([ "a"; "b"; "a" ], [ "a"; "a" ], 3);
    ([], [ "a"; "b" ], 0);
    ([ "a" ], [], 0);
  ]

let lines_law name law =
  prop name ~count:500 ~examples:lines_examples lines_case
    (fun (e, a, context) ->
      law e a context
        (Diff.hunks ~context ~expected:(of_lines e) ~actual:(of_lines a) ()))

let leading s =
  let n = String.length s in
  let rec go i = if i < n && Char.equal s.[i] 'k' then go (i + 1) else i in
  go 0

let trailing s =
  let n = String.length s in
  let rec go i = if i > 0 && Char.equal s.[i - 1] 'k' then go (i - 1) else i in
  n - go n

(* [inner_runs k] is the lengths of the runs of kept lines between two
   changes of [k]. *)
let inner_runs k =
  let k = String.sub k (leading k) (String.length k - leading k - trailing k) in
  String.split_on_char 'd' (String.map (function 'i' -> 'd' | c -> c) k)
  |> List.filter_map (fun r ->
      if String.equal r "" then None else Some (String.length r))

let margins e _ context hs =
  let rec gaps = function
    | (h : Diff.hunk) :: (h' :: _ as hs) ->
        let between = h'.expected_start - h.expected_start - h.expected_count in
        greater int ~than:(2 * context)
          (trailing (kinds h) + between + leading (kinds h'));
        gaps hs
    | [ _ ] | [] -> ()
  in
  hs
  |> List.iter (fun (h : Diff.hunk) ->
      let k = kinds h in
      let at_end = h.expected_start + h.expected_count - 1 = List.length e in
      let lead = leading k and trail = trailing k in
      equal int
        (if h.expected_start = 1 then min context lead else context)
        lead;
      equal int (if at_end then min context trail else context) trail;
      List.iter (at_most int ~than:(2 * context)) (inner_runs k));
  gaps hs

let changed hs =
  let changes k =
    String.fold_left (fun n c -> if Char.equal c 'k' then n else n + 1) 0 k
  in
  List.fold_left (fun n h -> n + changes (kinds h)) 0 hs

let hunk_ties () =
  let tie (e, a) =
    strf "%s against %s: %s" (String.escaped e) (String.escaped a)
      (String.concat "; " (hunks e a))
  in
  expect
    (String.concat "\n"
       (List.map tie
          [
            ("a\n", "a\na\n"); ("a\nb\n", "b\na\n"); ("a\nb\nc\n", "c\nb\na\n");
          ]))
  @@ __POS_OF__
       {|
    a\n against a\na\n: @@ -1,1 +1,2 @@| a|+a
    a\nb\n against b\na\n: @@ -1,2 +1,2 @@|-a| b|+a
    a\nb\nc\n against c\nb\na\n: @@ -1,3 +1,3 @@|-a|-b| c|+b|+a
    |}

let line_hunks =
  group "Line hunks"
    [
      cases "hunks gives each changed region as a unified hunk" ~name:fst
        [
          ( "a line replaced",
            (None, "a\nb\n", "a\nc\n", [ "@@ -1,2 +1,2 @@| a|-b|+c" ]) );
          ( "a line appended",
            (None, "x\n", "x\ny\n", [ "@@ -1,1 +1,2 @@| x|+y" ]) );
          ( "a common line between two changes",
            (None, "1\n2\n3\n", "1\n3\n4\n", [ "@@ -1,3 +1,3 @@| 1|-2| 3|+4" ])
          );
          ( "a changed run deletes, then inserts",
            (None, "a\nb\n", "x\ny\n", [ "@@ -1,2 +1,2 @@|-a|-b|+x|+y" ]) );
          ( "a line repeated around the change",
            (None, "a\nb\na\n", "a\na\n", [ "@@ -1,3 +1,2 @@| a|-b| a" ]) );
          ( "a second trailing newline",
            (None, "a\n", "a\n\n", [ "@@ -1,1 +1,2 @@| a|+" ]) );
        ]
        hunks_row;
      cases "two lines are equal when their bytes are, trailing blanks included"
        ~name:fst
        [
          ( "a trailing space",
            (None, "a \n", "a\n", [ "@@ -1,1 +1,1 @@|-a |+a" ]) );
          ( "a trailing tab",
            (None, "a\n", "a\t\n", [ "@@ -1,1 +1,1 @@|-a|+a\t" ]) );
        ]
        hunks_row;
      cases "a side with no line in a hunk starts at its next line" ~name:fst
        [
          ( "an insertion into the empty text",
            (None, "", "a\nb\n", [ "@@ -1,0 +1,2 @@|+a|+b" ]) );
          ( "a deletion to the empty text",
            (None, "a\n", "", [ "@@ -1,1 +1,0 @@|-a" ]) );
          ( "a deletion before a replacement",
            ( Some 0,
              "x\na\nb\n",
              "a\nc\n",
              [ "@@ -1,1 +1,0 @@|-x"; "@@ -3,1 +2,1 @@|-b|+c" ] ) );
        ]
        hunks_row;
      cases "context unchanged lines surround a region, 3 by default" ~name:fst
        [
          ( "the default",
            ( None,
              of_lines (edited 12 []),
              of_lines (edited 12 [ 6 ]),
              [ "@@ -3,7 +3,7 @@| l3| l4| l5|-l6|+x6| l7| l8| l9" ] ) );
          ( "fewer at the start of the texts",
            ( None,
              of_lines (edited 12 []),
              of_lines (edited 12 [ 2 ]),
              [ "@@ -1,5 +1,5 @@| l1|-l2|+x2| l3| l4| l5" ] ) );
          ( "none under 0",
            (Some 0, "a\nb\nc\n", "a\nx\nc\n", [ "@@ -2,1 +2,1 @@|-b|+x" ]) );
        ]
        hunks_row;
      cases "two regions at most 2 * context unchanged lines apart are one hunk"
        ~name:fst
        [
          ( "one line apart",
            ( Some 1,
              of_lines (edited 10 []),
              of_lines (edited 10 [ 4; 6 ]),
              [ "@@ -3,5 +3,5 @@| l3|-l4|+x4| l5|-l6|+x6| l7" ] ) );
          ( "two lines apart",
            ( Some 1,
              of_lines (edited 10 []),
              of_lines (edited 10 [ 3; 6 ]),
              [ "@@ -2,6 +2,6 @@| l2|-l3|+x3| l4| l5|-l6|+x6| l7" ] ) );
          ( "three lines apart",
            ( Some 1,
              of_lines (edited 10 []),
              of_lines (edited 10 [ 3; 7 ]),
              [
                "@@ -2,3 +2,3 @@| l2|-l3|+x3| l4";
                "@@ -6,3 +6,3 @@| l6|-l7|+x7| l8";
              ] ) );
          ( "six lines apart",
            ( Some 1,
              of_lines (edited 10 []),
              of_lines (edited 10 [ 2; 9 ]),
              [
                "@@ -1,3 +1,3 @@| l1|-l2|+x2| l3";
                "@@ -8,3 +8,3 @@| l8|-l9|+x9| l10";
              ] ) );
        ]
        hunks_row;
      cases "hunks is [] iff the texts split into equal lines" ~name:fst
        [
          ("equal texts", ("a\nb\n", "a\nb\n"));
          ("the empty texts", ("", ""));
          ("one trailing newline apart", ("a", "a\n"));
        ]
        (fun (_, (e, a)) -> equal (list string) [] (hunks e a));
      test "hunks raises on a negative context" (fun () ->
          raises_match Exn.invalid_arg (fun () ->
              Diff.hunks ~context:(-1) ~expected:"a" ~actual:"b" ()));
      cases
        "a region is minimal up to 2000 lines and 1000 edits, and whole past \
         either"
        ~name:fst
        [
          ( "2000 lines",
            ( ends_region 1_000 1_000,
              [
                "@@ -1,4 +1,4 @@ 1 deleted, 1 inserted, 3 kept";
                "@@ -997,4 +997,4 @@ 3 kept, 1 deleted, 1 inserted";
              ] ) );
          ( "2001 lines",
            ( ends_region 1_000 1_001,
              [ "@@ -1,1000 +1,1001 @@ 1000 deleted, 1001 inserted" ] ) );
          ( "1000 edits",
            ( two_runs 250,
              [
                "@@ -1,253 +1,253 @@ 250 deleted, 250 inserted, 3 kept";
                "@@ -258,253 +258,253 @@ 3 kept, 250 deleted, 250 inserted";
              ] ) );
          ( "1001 edits",
            ( two_runs ~extra:[ "f251" ] 250,
              [ "@@ -1,511 +1,510 @@ 511 deleted, 510 inserted" ] ) );
          ( "a whole region keeps its context",
            ( around,
              [
                "@@ -3,1206 +3,1206 @@ 3 kept, 1200 deleted, 1200 inserted, 3 \
                 kept";
              ] ) );
        ]
        bound_row;
      lines_law
        "a hunk's lines of each side fill its range of that text, in order, \
         and replacing them by the other side's gives the other text"
        patches;
      lines_law
        "a hunk keeps context unchanged lines around its changes, fewer at an \
         end of the texts, and joins regions at most 2 * context lines apart"
        margins;
      lines_law "within a run of changes the deletions come first"
        (fun _ _ _ hs ->
          List.iter (fun h -> not_contains ~sub:"id" (kinds h)) hs);
      lines_law "hunks is [] iff the lines of any two texts are equal"
        (fun e a _ hs ->
          equal bool (List.equal String.equal e a) (List.is_empty hs));
      lines_law
        "below the bounds, the hunks change as many lines as the insertion and \
         deletion distance" (fun e a _ hs ->
          equal int (distance ~substitutes:false e a) (changed hs));
      test "hunks' choice among minimal differences is the baseline's" hunk_ties;
    ]

(* Character refinement *)

let spans l =
  String.concat ", "
    (List.map (fun (s : Diff.span) -> strf "%d+%d" s.start s.length) l)

let refined expected actual =
  match Diff.refine ~expected ~actual with
  | None -> "none"
  | Some r ->
      strf "expected [%s], actual [%s]" (spans r.expected_spans)
        (spans r.actual_spans)

let refined_row (_, (e, a, row)) = equal string row (refined e a)

(* [cells ma mb] is two strings whose differing region is [ma] code points
   of expected and [mb] of actual. *)
let cells ma mb =
  let side first n last = first ^ String.make (n - 2) 'm' ^ last in
  (side "A" ma "B", side "C" mb "D")

let code_points s =
  let rec go i acc =
    if i >= String.length s then List.rev acc
    else
      let n = Uchar.utf_decode_length (String.get_utf_8_uchar s i) in
      go (i + n) (String.sub s i n :: acc)
  in
  go 0 []

let boundaries s =
  let next (i, acc) cp =
    let i = i + String.length cp in
    (i, i :: acc)
  in
  snd (List.fold_left next (0, [ 0 ]) (code_points s))

let unmarked s spans =
  let b = Buffer.create (String.length s) in
  let last =
    List.fold_left
      (fun i (span : Diff.span) ->
        Buffer.add_substring b s i (span.start - i);
        span.start + span.length)
      0 spans
  in
  Buffer.add_substring b s last (String.length s - last);
  Buffer.contents b

let marked s spans =
  List.fold_left
    (fun n (span : Diff.span) ->
      n + List.length (code_points (String.sub s span.start span.length)))
    0 spans

(* Code points of one to four bytes. *)
let strings_case =
  let code_point = Gen.of_list [ "a"; "é"; "€"; "𝄞" ] in
  Gen.with_pp
    (fun ppf (e, a) -> Format.fprintf ppf "expected %S, actual %S" e a)
    (let open Gen in
     let* e = list ~size:(int_range 3 10) code_point in
     let+ a = near_copy code_point e in
     (String.concat "" e, String.concat "" a))

(* Every pair of strings of up to four letters of three. *)
let short_pairs =
  let rec strings n =
    if n = 0 then [ "" ]
    else
      let shorter = strings (n - 1) in
      shorter
      @ List.concat_map
          (fun s -> [ s ^ "a"; s ^ "b"; s ^ "c" ])
          (List.filter (fun s -> String.length s = n - 1) shorter)
  in
  let all = strings 4 in
  List.concat_map (fun e -> List.map (fun a -> (e, a)) all) all

let refine_law ?(examples = []) name law =
  prop name ~count:300 ~examples strings_case (fun (e, a) ->
      let r = Diff.refine ~expected:e ~actual:a in
      cover "refined" (Option.is_some r);
      Option.iter (law e a) r)

let apart_on_code_points s spans =
  let on = boundaries s in
  let code_point =
    satisfies ~claim:"an offset on a code point" int (fun i -> List.mem i on)
  in
  let rec from last = function
    | [] -> ()
    | (span : Diff.span) :: spans ->
        let stop = span.start + span.length in
        less int ~than:span.start last;
        greater int ~than:0 span.length;
        code_point span.start;
        code_point stop;
        from stop spans
  in
  from (-1) spans

let spans_law e a (r : Diff.refinement) =
  apart_on_code_points e r.expected_spans;
  apart_on_code_points a r.actual_spans

let minimal_law e a (r : Diff.refinement) =
  let d = distance ~substitutes:true (code_points e) (code_points a) in
  equal string (unmarked e r.expected_spans) (unmarked a r.actual_spans);
  at_most int ~than:d (marked e r.expected_spans);
  at_most int ~than:d (marked a r.actual_spans)

let refine_ties () =
  let tie (e, a) = strf "%s against %s: %s" e a (refined e a) in
  expect
    (String.concat "\n"
       (List.map tie
          [ ("aab", "abc"); ("aab", "aba"); ("aba", "bac"); ("aaaa", "aaaaa") ]))
  @@ __POS_OF__
       {|
    aab against abc: expected [1+1], actual [2+1]
    aab against aba: expected [2+1], actual [1+1]
    aba against bac: expected [0+1], actual [2+1]
    aaaa against aaaaa: expected [], actual [4+1]
    |}

(* Refinement compares two code points once per cell of its grid, so what the
   comparison allocates is multiplied by the grid. A count of minor words is
   deterministic, so one refinement per size measures it. Coverage's counters
   allocate nothing; instrumented for mutation, the module counts every site
   it passes and the figure means nothing. *)
let allocation () =
  let module Mutate = Windtrap_runtime.Mutate in
  let mutated (m : Mutate.mutant) =
    String.starts_with ~prefix:"lib/diff.ml:" (Mutate.id_to_string m.id)
  in
  if List.exists mutated (Mutate.catalogue ()) then
    skip ~reason:"lib/diff.ml is instrumented for mutation" ();
  let sizes = [ 200; 600 ] in
  let cells = List.fold_left (fun n len -> n + (len * len)) 0 sizes in
  let before = Gc.minor_words () in
  sizes
  |> List.iter (fun len ->
      let a = String.make len 'a' in
      let b = String.mapi (fun i c -> if i mod 10 = 0 then 'b' else c) a in
      ignore (Sys.opaque_identity (Diff.refine ~expected:a ~actual:b)));
  less float_exact ~than:2. ((Gc.minor_words () -. before) /. Float.of_int cells)

let character_refinement =
  group "Character refinement"
    [
      cases "refine marks the code points a minimal script changes"
        ~name:(fun (n, _) -> n)
        [
          ("equal strings", ("abc", "abc", "expected [], actual []"));
          ("the empty strings", ("", "", "expected [], actual []"));
          ("a substitution", ("abc", "axc", "expected [1+1], actual [1+1]"));
          ("an insertion", ("ac", "abc", "expected [], actual [1+1]"));
          ("a deletion", ("abc", "ac", "expected [1+1], actual []"));
          ( "adjacent changes, as one span",
            ("the abcd end", "the axyd end", "expected [5+2], actual [5+2]") );
          ( "separate changes, as two spans",
            ("abcde", "xbcdy", "expected [0+1, 4+1], actual [0+1, 4+1]") );
          ( "a code point repeated around the change",
            ("121", "11", "expected [1+1], actual []") );
          ( "an insertion at the end",
            ("aaaa", "aaaab", "expected [], actual [4+1]") );
          ( "a two-byte code point, whole",
            ("h\xc3\xa9llo", "h\xc3\xa4llo", "expected [1+2], actual [1+2]") );
          ( "a code point against a byte",
            ("aa\xc3\xa9zz", "aaazz", "expected [2+2], actual [2+1]") );
          ( "two stray bytes, one unit each",
            ("abc\xffdefgh", "abc\xfedefgh", "expected [3+1], actual [3+1]") );
          ( "a truncated sequence, one unit",
            ("ab\xc3", "ab\xc3\xa9", "expected [2+1], actual [2+2]") );
        ]
        refined_row;
      cases "refine is None when the marks cover half or more of a side"
        ~name:(fun (n, _) -> n)
        [
          ("13 against 14", ("13", "14", "none"));
          ("no common code point", ("abc", "xyz", "none"));
          ("from the empty string", ("", "abc", "none"));
          ("half of each side", ("abcd", "axyd", "none"));
          ("half of actual, none of expected", ("ab", "abcd", "none"));
          ("half of expected, none of actual", ("abcd", "ab", "none"));
          ( "under half of each side",
            ("abcdefg", "abcxyzg", "expected [3+3], actual [3+3]") );
          ( "under half of the code points, over half of the bytes",
            ("a\xf0\x9d\x84\x9ebc", "axbc", "expected [1+4], actual [1+1]") );
        ]
        refined_row;
      cases "refine is None when (ma + 1) * (mb + 1) is above 4000000"
        ~name:(fun (n, _) -> n)
        [
          ( "2000 by 2000 cells",
            let e, a = cells 1_999 1_999 in
            (e, a, "expected [0+1, 1998+1], actual [0+1, 1998+1]") );
          ( "2001 by 2000 cells",
            let e, a = cells 2_000 1_999 in
            (e, a, "none") );
          ( "2000 by 2001 cells",
            let e, a = cells 1_999 2_000 in
            (e, a, "none") );
          ( "a short region after a long common start",
            let mid = String.make 2_101 'm' in
            (mid ^ "A", mid ^ "B", "expected [2101+1], actual [2101+1]") );
        ]
        refined_row;
      refine_law ~examples:short_pairs
        "without their spans the two sides are the same string, and each side \
         marks at most the edit distance"
        minimal_law;
      refine_law
        "spans are non-empty, ascending, apart and on code point boundaries"
        spans_law;
      test "refine's choice among minimal scripts is the baseline's" refine_ties;
      test "refine allocates under 2 minor words per cell of its grid"
        allocation;
    ]

let () = exit (run "diff" [ line_hunks; character_refinement ])
