(*---------------------------------------------------------------------------
   Copyright (c) 2020-2021 Craig Ferguson
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   Line diffing adapts the Myers algorithm from windtrap v1's lib/myers;
   refinement adapts the Wagner-Fischer edit script from v1's lib/distance.ml,
   itself derived from Craig Ferguson's work on Alcotest
   (https://github.com/mirage/alcotest/pull/247). v3 merges the two behind a
   data-only interface — no styling, no truncation, renderers project the
   data — and adds size guards against pathological inputs.
  ---------------------------------------------------------------------------*)

type line = Keep of string | Delete of string | Insert of string

type hunk = {
  expected_start : int;
  expected_count : int;
  actual_start : int;
  actual_count : int;
  lines : line list;
}

(* Bounds (implementation constants, not contract) *)

(* Above this many differing lines (both sides summed, after stripping the
   common outer lines) the region is reported as delete-all/insert-all
   instead of running Myers. Bounds Myers' O((n + m) * d) time. *)
let myers_line_limit = 2_000

(* A minimal line script needing more than this many edits is noise; give up
   and fall back. Bounds the trace memory, O(d * (n + m)). *)
let myers_max_edits = 1_000

(* Above this many Wagner-Fischer cells (on the differing region) refinement
   is not attempted. *)
let dp_cell_limit = 4_000_000

(* A highlight earns its place by pointing at a small part of a mostly
   shared value. Once half of a side is marked, it stops doing that: the
   reader is looking at two values that differ, and scattering tildes over
   them draws the eye to coincidental character alignments instead —
   ["Some _"] against ["None"] marks [S], [m], [e ] against [N], [n], and
   says nothing a plain pair of lines would not. Measured over the corpus in
   examples/x-demo, every informative highlight marks 11-21% of its side and
   every uninformative one marks 50% or more.

   The bound is per side and strict, so an insertion that marks nothing on
   the expected side still refines, and two two-character values (["13"] and
   ["14"], marking half of each) do not — nobody needs a marker to find that
   difference. *)
let noise_threshold = 1. /. 2.

(* Line splitting and common-outer stripping *)

(* Text.split_lines owns the rule: a single trailing newline is not
   significant (documented in the .mli). *)
let split_lines s = Array.of_list (Text.split_lines s)

let common_prefix_len a b =
  let n = min (Array.length a) (Array.length b) in
  let i = ref 0 in
  while !i < n && String.equal a.(!i) b.(!i) do
    incr i
  done;
  !i

(* Suffix pass bounded by the prefix pass so a repeated line at the
   difference boundary cannot make the two overlap. *)
let common_suffix_len ~prefix a b =
  let la = Array.length a and lb = Array.length b in
  let n = min (la - prefix) (lb - prefix) in
  let i = ref 0 in
  while !i < n && String.equal a.(la - 1 - !i) b.(lb - 1 - !i) do
    incr i
  done;
  !i

(* Myers shortest edit script (adapted from v1 lib/myers) *)

exception Found of int * int array list

(* [myers ~max_edits a b] is the shortest edit script from [a] to [b] in text
   order, or [None] when it would take more than [max_edits] edits. Greedy
   forward D-path search with one furthest-reaching snapshot kept per step
   for the backtrack. Requires [max_edits >= 1]: the initial store below
   needs [offset + 1] in bounds when [a] and [b] are non-empty. *)
let myers ~max_edits (a : string array) (b : string array) =
  let n = Array.length a and m = Array.length b in
  let max_d = n + m in
  if max_d = 0 then Some []
  else begin
    let search_d = min max_d max_edits in
    let offset = search_d in
    let v = Array.make ((2 * search_d) + 1) (-1) in
    v.(offset + 1) <- 0;
    let traces_rev = ref [] in
    match
      for d = 0 to search_d do
        for k = -d to d do
          if (k + d) mod 2 = 0 then begin
            let x =
              if k = -d || (k <> d && v.(offset + k - 1) < v.(offset + k + 1))
              then v.(offset + k + 1)
              else if k = d then v.(offset + k - 1) + 1
              else
                let x1 = v.(offset + k - 1) + 1 in
                let x2 = v.(offset + k + 1) in
                if x1 > x2 then x1 else x2
            in
            let y = x - k in
            let x, y =
              let x = ref x in
              let y = ref y in
              while !x < n && !y < m && String.equal a.(!x) b.(!y) do
                incr x;
                incr y
              done;
              (!x, !y)
            in
            v.(offset + k) <- x;
            if x >= n && y >= m then begin
              traces_rev := Array.copy v :: !traces_rev;
              raise_notrace (Found (d, List.rev !traces_rev))
            end
          end
        done;
        traces_rev := Array.copy v :: !traces_rev
      done
    with
    | () -> None (* not reachable within [max_edits] edits *)
    | exception Found (d_final, traces) ->
        let traces = Array.of_list traces in
        let x = ref n in
        let y = ref m in
        let ops = ref [] in
        for d = d_final downto 1 do
          let k = !x - !y in
          let prev = traces.(d - 1) in
          let prev_k =
            if
              k = -d || (k <> d && prev.(offset + k - 1) < prev.(offset + k + 1))
            then k + 1
            else k - 1
          in
          let prev_x = prev.(offset + prev_k) in
          let prev_y = prev_x - prev_k in
          while !x > prev_x && !y > prev_y do
            ops := Keep a.(!x - 1) :: !ops;
            decr x;
            decr y
          done;
          if prev_k = k + 1 then ops := Insert b.(prev_y) :: !ops
          else ops := Delete a.(prev_x) :: !ops;
          x := prev_x;
          y := prev_y
        done;
        while !x > 0 && !y > 0 do
          ops := Keep a.(!x - 1) :: !ops;
          decr x;
          decr y
        done;
        while !x > 0 do
          ops := Delete a.(!x - 1) :: !ops;
          decr x
        done;
        while !y > 0 do
          ops := Insert b.(!y - 1) :: !ops;
          decr y
        done;
        Some !ops
  end

(* Hunk grouping *)

let is_change = function Keep _ -> false | Delete _ | Insert _ -> true

(* Reorder each maximal run of changes so deletions precede insertions,
   keeping text order otherwise. *)
let reorder_changes lines =
  let flush acc dels inss =
    let acc = List.fold_left (fun acc d -> d :: acc) acc (List.rev dels) in
    List.fold_left (fun acc i -> i :: acc) acc (List.rev inss)
  in
  let rec go acc dels inss = function
    | [] -> List.rev (flush acc dels inss)
    | (Keep _ as k) :: rest -> go (k :: flush acc dels inss) [] [] rest
    | (Delete _ as d) :: rest -> go acc (d :: dels) inss rest
    | (Insert _ as i) :: rest -> go acc dels (i :: inss) rest
  in
  go [] [] [] lines

(* Group the ops of a full-text edit script into hunks: changed regions
   separated by more than [2 * context] kept lines become separate hunks,
   each padded with up to [context] kept lines on both ends. *)
let group_hunks ~context ops =
  let ops = Array.of_list ops in
  let n = Array.length ops in
  (* Line number, on each side, of op [i] (1-based; for an op that consumes
     no line on a side, the next line number on that side). *)
  let exp_no = Array.make (max n 1) 1 and act_no = Array.make (max n 1) 1 in
  let e = ref 1 and a = ref 1 in
  for i = 0 to n - 1 do
    exp_no.(i) <- !e;
    act_no.(i) <- !a;
    match ops.(i) with
    | Keep _ ->
        incr e;
        incr a
    | Delete _ -> incr e
    | Insert _ -> incr a
  done;
  (* Coalesce change indices into groups. *)
  let groups =
    let rec go groups i =
      if i >= n then List.rev groups
      else if not (is_change ops.(i)) then go groups (i + 1)
      else
        match groups with
        | (first, last) :: rest when i - last - 1 <= 2 * context ->
            go ((first, i) :: rest) (i + 1)
        | _ -> go ((i, i) :: groups) (i + 1)
    in
    go [] 0
  in
  let hunk_of_group (first, last) =
    let hs = max 0 (first - context) in
    let he = min (n - 1) (last + context) in
    let lines = ref [] in
    let expected_count = ref 0 and actual_count = ref 0 in
    for i = he downto hs do
      lines := ops.(i) :: !lines;
      match ops.(i) with
      | Keep _ ->
          incr expected_count;
          incr actual_count
      | Delete _ -> incr expected_count
      | Insert _ -> incr actual_count
    done;
    {
      expected_start = exp_no.(hs);
      expected_count = !expected_count;
      actual_start = act_no.(hs);
      actual_count = !actual_count;
      lines = reorder_changes !lines;
    }
  in
  List.map hunk_of_group groups

let hunks ?(context = 3) ~expected ~actual () =
  if context < 0 then invalid_arg "Diff.hunks: context is negative";
  let a = split_lines expected and b = split_lines actual in
  let la = Array.length a and lb = Array.length b in
  let prefix = common_prefix_len a b in
  let suffix = common_suffix_len ~prefix a b in
  let ma = la - prefix - suffix and mb = lb - prefix - suffix in
  if ma = 0 && mb = 0 then []
  else
    let mid_a = Array.sub a prefix ma and mid_b = Array.sub b prefix mb in
    let middle =
      if ma + mb > myers_line_limit then None
      else myers ~max_edits:myers_max_edits mid_a mid_b
    in
    let middle =
      match middle with
      | Some ops -> ops
      | None ->
          (* Size guard: complete, non-minimal fallback. *)
          List.map (fun l -> Delete l) (Array.to_list mid_a)
          @ List.map (fun l -> Insert l) (Array.to_list mid_b)
    in
    let keeps arr from len = List.init len (fun i -> Keep arr.(from + i)) in
    let ops = keeps a 0 prefix @ middle @ keeps a (la - suffix) suffix in
    group_hunks ~context ops

(* Character refinement (adapted from v1 lib/distance.ml) *)

type span = { start : int; length : int }
type refinement = { expected_spans : span list; actual_spans : span list }

(* Middle-relative minimal edit commands; indices are element positions. *)
type edit_cmd = Del of int | Ins of int | Sub of int * int

(* [(byte_offset, byte_length)] of each code point of [s], malformed bytes
   decoding one replacement-sized unit at a time per String.get_utf_8_uchar.
   Spans built over these pairs can never split a UTF-8 sequence. *)
let code_points s =
  let n = String.length s in
  let rec go i acc =
    if i >= n then Array.of_list (List.rev acc)
    else
      let len = Uchar.utf_decode_length (String.get_utf_8_uchar s i) in
      go (i + len) ((i, len) :: acc)
  in
  go 0 []

(* Byte-faithful code-point equality.

   The byte loop is a top-level function taking everything it needs, not a
   local [let rec] closing over the offsets: [wagner_fischer] calls this once
   per grid cell, and a closure capturing five locals was being allocated on
   every one of them — ~8 minor words per cell, which dominated refinement's
   allocation. *)
let rec bytes_equal sa oa sb ob len k =
  k = len || (sa.[oa + k] = sb.[ob + k] && bytes_equal sa oa sb ob len (k + 1))

let cp_equal sa (oa, la) sb (ob, lb) = la = lb && bytes_equal sa oa sb ob la 0

(* Standard Wagner-Fischer: cost grid, then backtrack to a minimal edit
   script (ascending element indices). [equal_at] compares middle-relative
   positions. Tie-breaking is v1's: prefer no-cost diagonal, then deletion,
   then insertion, then substitution. Unlike v1, the no-cost diagonal
   requires the elements to actually be equal: equal grid values alone can
   also arise around an unequal pair, and following that diagonal silently
   aligns two different elements (found by the unmarked-remainder law in
   test_diff.ml). *)
let wagner_fischer ~equal_at na nb =
  let grid = Array.make_matrix (na + 1) (nb + 1) 0 in
  for i = 0 to na do
    for j = 0 to nb do
      let cost =
        if min i j = 0 then max i j
        else if equal_at (i - 1) (j - 1) then grid.(i - 1).(j - 1)
        else
          1 + min grid.(i - 1).(j) (min grid.(i).(j - 1) grid.(i - 1).(j - 1))
      in
      grid.(i).(j) <- cost
    done
  done;
  let rec aux acc = function
    | 0, 0 -> acc
    | i, 0 -> List.init i (fun k -> Del k) @ acc
    | 0, j -> List.init j (fun k -> Ins k) @ acc
    | i, j ->
        let delete_cost = grid.(i - 1).(j)
        and insert_cost = grid.(i).(j - 1)
        and subst_cost = grid.(i - 1).(j - 1) in
        if equal_at (i - 1) (j - 1) && delete_cost >= subst_cost then
          (* Elements equal: [grid.(i).(j) = subst_cost] by construction. *)
          aux acc (i - 1, j - 1)
        else if delete_cost <= insert_cost && delete_cost <= subst_cost then
          aux (Del (i - 1) :: acc) (i - 1, j)
        else if insert_cost <= subst_cost then
          aux (Ins (j - 1) :: acc) (i, j - 1)
        else aux (Sub (i - 1, j - 1) :: acc) (i - 1, j - 1)
  in
  aux [] (na, nb)

(* Ascending element indices to coalesced byte spans. *)
let spans_of_indices cps indices =
  let rec go acc = function
    | [] -> List.rev acc
    | i :: rest ->
        let o, l = cps.(i) in
        let acc =
          match acc with
          | { start; length } :: tl when start + length = o ->
              { start; length = length + l } :: tl
          | _ -> { start = o; length = l } :: acc
        in
        go acc rest
  in
  go [] indices

let refine ~expected ~actual =
  let ca = code_points expected and cb = code_points actual in
  let na = Array.length ca and nb = Array.length cb in
  let equal_abs i j = cp_equal expected ca.(i) actual cb.(j) in
  (* Strip the common outer code points before the O(n * m) phase; the
     suffix pass is bounded by the prefix pass (see common_suffix_len). *)
  let n = min na nb in
  let prefix = ref 0 in
  while !prefix < n && equal_abs !prefix !prefix do
    incr prefix
  done;
  let prefix = !prefix in
  let smax = min (na - prefix) (nb - prefix) in
  let suffix = ref 0 in
  while !suffix < smax && equal_abs (na - 1 - !suffix) (nb - 1 - !suffix) do
    incr suffix
  done;
  let suffix = !suffix in
  let ma = na - prefix - suffix and mb = nb - prefix - suffix in
  if ma = 0 && mb = 0 then Some { expected_spans = []; actual_spans = [] }
  else if (ma + 1) * (mb + 1) > dp_cell_limit then None
  else
    let equal_at i j = equal_abs (prefix + i) (prefix + j) in
    let script = wagner_fischer ~equal_at ma mb in
    let expected_indices, actual_indices =
      List.fold_left
        (fun (es, as_) cmd ->
          match cmd with
          | Del e -> ((prefix + e) :: es, as_)
          | Ins a -> (es, (prefix + a) :: as_)
          | Sub (e, a) -> ((prefix + e) :: es, (prefix + a) :: as_))
        ([], []) script
    in
    let signal marked total =
      total = 0 || float_of_int marked /. float_of_int total < noise_threshold
    in
    if
      not
        (signal (List.length expected_indices) na
        && signal (List.length actual_indices) nb)
    then None
    else
      Some
        {
          expected_spans = spans_of_indices ca (List.rev expected_indices);
          actual_spans = spans_of_indices cb (List.rev actual_indices);
        }
