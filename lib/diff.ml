(*---------------------------------------------------------------------------
   Copyright (c) 2020-2021 Craig Ferguson
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   Line diffing adapts the Myers algorithm from windtrap v1's lib/myers;
   refinement adapts the Wagner-Fischer edit script from v1's lib/distance.ml,
   itself derived from Craig Ferguson's work on Alcotest
   (https://github.com/mirage/alcotest/pull/247). v3 merges the two behind a
   data-only interface (RFC Law 4) and adds size guards (audit hazard 12).
  ---------------------------------------------------------------------------*)

type line = Keep of string | Delete of string | Insert of string

type hunk = {
  expected_start : int;
  expected_count : int;
  actual_start : int;
  actual_count : int;
  lines : line list;
}

(* ───── Bounds (implementation constants, not contract) ───── *)

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

(* When more than this fraction of the code points differ, a highlighted
   refinement is noise rather than signal. *)
let noise_threshold = 4. /. 5.

(* ───── Line splitting and common-outer stripping ───── *)

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

(* ───── Myers shortest edit script (adapted from v1 lib/myers) ───── *)

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

(* ───── Hunk grouping ───── *)

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

(* ───── Character refinement (adapted from v1 lib/distance.ml) ───── *)

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

(* Byte-faithful code-point equality. *)
let cp_equal sa (oa, la) sb (ob, lb) =
  la = lb
  &&
  let rec eq k = k = la || (sa.[oa + k] = sb.[ob + k] && eq (k + 1)) in
  eq 0

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

(* ───── Sequence elements (amendment B7) ───── *)

type mismatch = {
  index : int;
  expected : string option;
  actual : string option;
}

type seq_diff = {
  kind : [ `List | `Array ];
  expected_length : int;
  actual_length : int;
  differing : int;
  first : mismatch option;
}

exception Not_a_sequence

let is_ws = function ' ' | '\n' | '\t' | '\r' -> true | _ -> false

(* The canonical elements of the region [s.[first..last]] (inclusive): split
   on top-level [';'], with whitespace runs outside string and character
   literals collapsed to one space. Raises [Not_a_sequence] on anything the
   conservative grammar cannot account for: unbalanced brackets, an
   unterminated string, an empty element (as in ["[a;; b]"]). *)
let canonical_elements s ~first ~last =
  let buf = Buffer.create 32 in
  let out = ref [] in
  let depth = ref 0 in
  let pending_ws = ref false in
  let add c =
    if !pending_ws then begin
      if Buffer.length buf > 0 then Buffer.add_char buf ' ';
      pending_ws := false
    end;
    Buffer.add_char buf c
  in
  let flush () =
    if Buffer.length buf = 0 then raise_notrace Not_a_sequence;
    out := Buffer.contents buf :: !out;
    Buffer.clear buf;
    pending_ws := false
  in
  (* Copies the [%C]-style char literal starting at [i] (its opening quote)
     verbatim and returns the index of its closing quote, or [None] when no
     literal shape matches — the quote is then an ordinary character. Shapes:
     ['c'], ['\c'], and the four-character escapes ['\000'] / ['\xFF']. *)
  let char_literal i =
    let quote_at j = j <= last && s.[j] = '\'' in
    let close j =
      for k = i to j do
        Buffer.add_char buf s.[k]
      done;
      Some j
    in
    if i + 1 > last then None
    else if s.[i + 1] = '\\' then
      if quote_at (i + 3) then close (i + 3)
      else if quote_at (i + 5) then close (i + 5)
      else None
    else if quote_at (i + 2) then close (i + 2)
    else None
  in
  let i = ref first in
  while !i <= last do
    (match s.[!i] with
    | c when is_ws c -> pending_ws := true
    | '"' ->
        add '"';
        let rec copy j =
          if j > last then raise_notrace Not_a_sequence
          else
            match s.[j] with
            | '\\' ->
                if j + 1 > last then raise_notrace Not_a_sequence;
                Buffer.add_char buf '\\';
                Buffer.add_char buf s.[j + 1];
                copy (j + 2)
            | '"' ->
                Buffer.add_char buf '"';
                j
            | c ->
                Buffer.add_char buf c;
                copy (j + 1)
        in
        i := copy (!i + 1)
    | '\'' -> (
        (* Collapsing must not reach inside a char literal (['\n'] would
           otherwise lose its meaning), so literals copy verbatim. *)
        if !pending_ws && Buffer.length buf > 0 then Buffer.add_char buf ' ';
        pending_ws := false;
        match char_literal !i with
        | Some close -> i := close
        | None -> Buffer.add_char buf '\'')
    | ';' when !depth = 0 -> flush ()
    | ('(' | '[' | '{') as c ->
        incr depth;
        add c
    | (')' | ']' | '}') as c ->
        decr depth;
        if !depth < 0 then raise_notrace Not_a_sequence;
        add c
    | c -> add c);
    incr i
  done;
  if !depth <> 0 then raise_notrace Not_a_sequence;
  if Buffer.length buf > 0 then flush ()
  else if !out <> [] then raise_notrace Not_a_sequence (* trailing ';' *);
  Array.of_list (List.rev !out)

(* [Some (kind, elements)] when [s] (outer whitespace aside) is a bracketed
   sequence rendering; [None] otherwise. *)
let parse_sequence s =
  let n = String.length s in
  let a = ref 0 and b = ref (n - 1) in
  while !a < n && is_ws s.[!a] do
    incr a
  done;
  while !b > !a && is_ws s.[!b] do
    decr b
  done;
  if !b < !a + 1 || s.[!a] <> '[' || s.[!b] <> ']' then None
  else
    let kind, first, last =
      if !b - !a >= 3 && s.[!a + 1] = '|' && s.[!b - 1] = '|' then
        (`Array, !a + 2, !b - 2)
      else (`List, !a + 1, !b - 1)
    in
    match canonical_elements s ~first ~last with
    | elements -> Some (kind, elements)
    | exception Not_a_sequence -> None

(* Differences under the element-grain alignment: the line-diff machinery
   run over the canonical element arrays, so a single inserted or removed
   element is one difference, not a shifted disagreement at every later
   index. Within a run of changes, [min d i] of the [d] deletions and [i]
   insertions pair up as replacements and the rest are one-sided — [max d i]
   differing elements in total. [first] is the first non-aligned element; it
   sits after Keeps only, so its index reads the same on both sides (and on
   the one side that carries a one-sided element). *)
let aligned_differences ~prefix ops =
  let differing = ref 0 in
  let first = ref None in
  let kept = ref prefix in
  let run_del = ref [] and run_ins = ref [] in
  let flush_run () =
    let dels = List.rev !run_del and inss = List.rev !run_ins in
    let nd = List.length dels and ni = List.length inss in
    if nd + ni > 0 then begin
      differing := !differing + max nd ni;
      (if !first = None then
         let index = !kept in
         first :=
           match (dels, inss) with
           | e :: _, a :: _ ->
               Some { index; expected = Some e; actual = Some a }
           | e :: _, [] -> Some { index; expected = Some e; actual = None }
           | [], a :: _ -> Some { index; expected = None; actual = Some a }
           | [], [] -> None (* unreachable: nd + ni > 0 *));
      run_del := [];
      run_ins := []
    end
  in
  List.iter
    (function
      | Keep _ ->
          flush_run ();
          incr kept
      | Delete e -> run_del := e :: !run_del
      | Insert a -> run_ins := a :: !run_ins)
    ops;
  flush_run ();
  (!differing, !first)

let sequences ~expected ~actual =
  match (parse_sequence expected, parse_sequence actual) with
  | Some (ke, ee), Some (ka, ea) when ke = ka ->
      let ne = Array.length ee and na = Array.length ea in
      let prefix = common_prefix_len ee ea in
      let suffix = common_suffix_len ~prefix ee ea in
      let me = ne - prefix - suffix and ma = na - prefix - suffix in
      let ops =
        if me = 0 && ma = 0 then []
        else
          let mid_e = Array.sub ee prefix me
          and mid_a = Array.sub ea prefix ma in
          match
            if me + ma > myers_line_limit then None
            else myers ~max_edits:myers_max_edits mid_e mid_a
          with
          | Some ops -> ops
          | None ->
              (* Size guard: complete, non-minimal fallback (as [hunks]) —
                 one delete-all/insert-all run, counted as [max me ma]. *)
              List.map (fun e -> Delete e) (Array.to_list mid_e)
              @ List.map (fun a -> Insert a) (Array.to_list mid_a)
      in
      let differing, first = aligned_differences ~prefix ops in
      Some
        {
          kind = ke;
          expected_length = ne;
          actual_length = na;
          differing;
          first;
        }
  | _ -> None

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
    let ratio = float_of_int (List.length script) /. float_of_int (max na nb) in
    if ratio > noise_threshold then None
    else
      let expected_indices, actual_indices =
        List.fold_left
          (fun (es, as_) cmd ->
            match cmd with
            | Del e -> ((prefix + e) :: es, as_)
            | Ins a -> (es, (prefix + a) :: as_)
            | Sub (e, a) -> ((prefix + e) :: es, (prefix + a) :: as_))
          ([], []) script
      in
      Some
        {
          expected_spans = spans_of_indices ca (List.rev expected_indices);
          actual_spans = spans_of_indices cb (List.rev actual_indices);
        }
