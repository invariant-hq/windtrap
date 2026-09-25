(*---------------------------------------------------------------------------
   Copyright (c) 2020-2021 Craig Ferguson
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   Line diffing adapts the Myers algorithm from windtrap v1's lib/myers;
   refinement adapts the Wagner-Fischer edit script from v1's lib/distance.ml,
   itself derived from Craig Ferguson's work on Alcotest
   (https://github.com/mirage/alcotest/pull/247). v3 merges the two behind a
   data-only interface (no styling, no truncation, renderers project the
   data) and adds size guards against pathological inputs.
  ---------------------------------------------------------------------------*)

type line = Keep of string | Delete of string | Insert of string

type hunk = {
  expected_start : int;
  expected_count : int;
  actual_start : int;
  actual_count : int;
  lines : line list;
}

(* Common ends *)

(* [common_ends a b] is the number of equal elements at the start of [a] and
   [b], then at their end. The end is counted in what the start leaves, so an
   element repeated at the boundary is not counted twice. *)
let common_ends a b =
  let la = Array.length a and lb = Array.length b in
  let n = min la lb in
  let rec prefix p =
    if p < n && String.equal a.(p) b.(p) then prefix (p + 1) else p
  in
  let p = prefix 0 in
  let rec suffix s =
    if s < n - p && String.equal a.(la - 1 - s) b.(lb - 1 - s) then
      suffix (s + 1)
    else s
  in
  (p, suffix 0)

(* Line hunks *)

(* Myers takes O((n + m) * d) time and O(d * (n + m)) memory for a region of
   [n + m] lines that takes [d] edits. *)
let myers_line_limit = 2_000
let myers_max_edits = 1_000
let keep lines = List.map (fun l -> Keep l) (Array.to_list lines)

(* [myers a b] is a shortest script from [a] to [b], or [None] past the
   bounds. [a] and [b] are not both empty, so [v] has a cell at
   [offset + 1].

   After [d] edits, the path on the diagonal [k = x - y] reaches the furthest
   [x] it can, [v.(offset + k)], by following equal lines after its last
   edit. [traces.(d)] keeps [v] after [d] edits, from which the backtrack
   recovers each edit of the path that reaches the end. *)
let myers a b =
  let n = Array.length a and m = Array.length b in
  if n + m > myers_line_limit then None
  else
    let max_d = min (n + m) myers_max_edits in
    let offset = max_d in
    let v = Array.make ((2 * max_d) + 1) (-1) in
    let traces = Array.make (max_d + 1) [||] in
    (* The path on diagonal [k] after [d] edits extends the one on [k + 1] by
       an insertion when this holds of the paths [v] after [d - 1] edits,
       and the one on [k - 1] by a deletion otherwise. *)
    let inserts v ~d k =
      k = -d || (k <> d && v.(offset + k - 1) < v.(offset + k + 1))
    in
    let rec slide x y =
      if x < n && y < m && String.equal a.(x) b.(y) then slide (x + 1) (y + 1)
      else x
    in
    let rec reaches_end d k =
      k <= d
      &&
      let x =
        if inserts v ~d k then v.(offset + k + 1) else v.(offset + k - 1) + 1
      in
      let x = slide x (x - k) in
      v.(offset + k) <- x;
      (x >= n && x - k >= m) || reaches_end d (k + 2)
    in
    let rec backtrack d x y script =
      if d = 0 then keep (Array.sub a 0 x) @ script
      else
        let k = x - y and paths = traces.(d - 1) in
        let insertion = inserts paths ~d k in
        let pk = if insertion then k + 1 else k - 1 in
        let px = paths.(offset + pk) in
        let py = px - pk in
        let edit, after =
          if insertion then (Insert b.(py), px) else (Delete a.(px), px + 1)
        in
        let kept = keep (Array.sub a after (x - after)) in
        backtrack (d - 1) px py ((edit :: kept) @ script)
    in
    let rec search d =
      if d > max_d then None
      else if reaches_end d (-d) then Some (backtrack d n m [])
      else begin
        traces.(d) <- Array.copy v;
        search (d + 1)
      end
    in
    v.(offset + 1) <- 0;
    search 0

(* [deletions_first script] is [script] with the deletions of each run of
   changes before its insertions. *)
let rec deletions_first = function
  | [] -> []
  | (Keep _ as line) :: script -> line :: deletions_first script
  | (Delete _ | Insert _) :: _ as script ->
      let rec run deletions insertions = function
        | (Delete _ as line) :: script ->
            run (line :: deletions) insertions script
        | (Insert _ as line) :: script ->
            run deletions (line :: insertions) script
        | ([] | Keep _ :: _) as script ->
            List.rev_append deletions
              (List.rev_append insertions (deletions_first script))
      in
      run [] [] script

(* [cut ~context script] is the hunks of [script], a script of the whole
   texts. *)
let cut ~context script =
  let lines = Array.of_list script in
  let n = Array.length lines in
  (* [expected_no.(i)] and [actual_no.(i)] number the line at which
     [lines.(i)] starts on each side; index [n] is past the last line. *)
  let expected_no = Array.make (n + 1) 1 and actual_no = Array.make (n + 1) 1 in
  lines
  |> Array.iteri (fun i line ->
      let e, a =
        match line with
        | Keep _ -> (1, 1)
        | Delete _ -> (1, 0)
        | Insert _ -> (0, 1)
      in
      expected_no.(i + 1) <- expected_no.(i) + e;
      actual_no.(i + 1) <- actual_no.(i) + a);
  let hunk (first, last) =
    let start = max 0 (first - context) and stop = min n (last + context + 1) in
    {
      expected_start = expected_no.(start);
      expected_count = expected_no.(stop) - expected_no.(start);
      actual_start = actual_no.(start);
      actual_count = actual_no.(stop) - actual_no.(start);
      lines = Array.to_list (Array.sub lines start (stop - start));
    }
  in
  (* Each changed region as the indices of its first and last change. *)
  let rec regions acc i =
    if i = n then List.rev acc
    else
      match (lines.(i), acc) with
      | Keep _, _ -> regions acc (i + 1)
      | (Delete _ | Insert _), (first, last) :: acc
        when i - last - 1 <= 2 * context ->
          regions ((first, i) :: acc) (i + 1)
      | (Delete _ | Insert _), acc -> regions ((i, i) :: acc) (i + 1)
  in
  List.map hunk (regions [] 0)

let hunks ?(context = 3) ~expected ~actual () =
  if context < 0 then invalid_arg "Diff.hunks: context is negative";
  let a = Array.of_list (Text.split_lines expected) in
  let b = Array.of_list (Text.split_lines actual) in
  let prefix, suffix = common_ends a b in
  let ma = Array.length a - prefix - suffix in
  let mb = Array.length b - prefix - suffix in
  if ma = 0 && mb = 0 then []
  else
    let mid_a = Array.sub a prefix ma and mid_b = Array.sub b prefix mb in
    let middle =
      match myers mid_a mid_b with
      | Some script -> deletions_first script
      | None ->
          List.map (fun l -> Delete l) (Array.to_list mid_a)
          @ List.map (fun l -> Insert l) (Array.to_list mid_b)
    in
    let before = keep (Array.sub a 0 prefix) in
    let after = keep (Array.sub a (prefix + ma) suffix) in
    cut ~context (before @ middle @ after)

(* Character refinement *)

type span = { start : int; length : int }
type refinement = { expected_spans : span list; actual_spans : span list }

let dp_cell_limit = 4_000_000

(* A highlight helps when it points at a small part of a mostly shared value.
   Once half of a side is marked, it draws the eye to coincidental alignments
   instead. Over a corpus of real failures, every informative highlight
   marked 11 to 21% of its side, and every uninformative one 50% or more. *)
let noise_threshold = 1. /. 2.

(* The code points of [s], each as its bytes, a malformed unit as
   [String.get_utf_8_uchar] decodes it. *)
let code_points s =
  let rec go i acc =
    if i >= String.length s then Array.of_list (List.rev acc)
    else
      let n = Uchar.utf_decode_length (String.get_utf_8_uchar s i) in
      go (i + n) (String.sub s i n :: acc)
  in
  go 0 []

(* [marks a b] is the indices of [a] and of [b] that a minimal script of
   deletions, insertions and substitutions changes, ascending. On a tie the
   backtrack prefers a deletion, an insertion, then a substitution, and
   passes an equal pair unless a deletion reaches a cheaper cell. Equal costs
   do not make a pair equal, so the pair itself is compared. *)
let marks a b =
  let na = Array.length a and nb = Array.length b in
  let equal i j = String.equal a.(i) b.(j) in
  let cost = Array.make_matrix (na + 1) (nb + 1) 0 in
  for i = 0 to na do
    for j = 0 to nb do
      cost.(i).(j) <-
        (if min i j = 0 then max i j
         else if equal (i - 1) (j - 1) then cost.(i - 1).(j - 1)
         else
           1 + min cost.(i - 1).(j) (min cost.(i).(j - 1) cost.(i - 1).(j - 1)))
    done
  done;
  let rec back i j marked_a marked_b =
    if i = 0 then (marked_a, List.init j Fun.id @ marked_b)
    else if j = 0 then (List.init i Fun.id @ marked_a, marked_b)
    else
      let deletion = cost.(i - 1).(j)
      and insertion = cost.(i).(j - 1)
      and substitution = cost.(i - 1).(j - 1) in
      if equal (i - 1) (j - 1) && deletion >= substitution then
        back (i - 1) (j - 1) marked_a marked_b
      else if deletion <= insertion && deletion <= substitution then
        back (i - 1) j ((i - 1) :: marked_a) marked_b
      else if insertion <= substitution then
        back i (j - 1) marked_a ((j - 1) :: marked_b)
      else back (i - 1) (j - 1) ((i - 1) :: marked_a) ((j - 1) :: marked_b)
  in
  back na nb [] []

(* [spans cps ~first marked] is the byte ranges of the code points
   [cps.(first + i)] for [i] in [marked], ascending, adjacent ones
   coalesced. *)
let spans cps ~first marked =
  let starts = Array.make (Array.length cps + 1) 0 in
  Array.iteri (fun i cp -> starts.(i + 1) <- starts.(i) + String.length cp) cps;
  let add spans i =
    let start = starts.(first + i) and stop = starts.(first + i + 1) in
    match spans with
    | last :: spans when last.start + last.length = start ->
        { last with length = stop - last.start } :: spans
    | spans -> { start; length = stop - start } :: spans
  in
  List.rev (List.fold_left add [] marked)

let refine ~expected ~actual =
  let a = code_points expected and b = code_points actual in
  let prefix, suffix = common_ends a b in
  let ma = Array.length a - prefix - suffix in
  let mb = Array.length b - prefix - suffix in
  if (ma + 1) * (mb + 1) > dp_cell_limit then None
  else
    let marked_a, marked_b =
      marks (Array.sub a prefix ma) (Array.sub b prefix mb)
    in
    let is_noise marked side =
      let n = List.length marked in
      n <> 0 && float n /. float (Array.length side) >= noise_threshold
    in
    if is_noise marked_a a || is_noise marked_b b then None
    else
      Some
        {
          expected_spans = spans a ~first:prefix marked_a;
          actual_spans = spans b ~first:prefix marked_b;
        }
