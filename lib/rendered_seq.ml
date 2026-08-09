(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   Extracted from lib/diff.ml, where it shared a file with Myers line
   diffing and Wagner-Fischer refinement without sharing anything else.
  ---------------------------------------------------------------------------*)

type kind = [ `List | `Array ]
type extent = { start : int; length : int }
type element = { canonical : string; extent : extent }

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
  (* The element's byte range, tracked alongside the canonical form: [from]
     is its first non-whitespace byte and [upto] its last, so the extent
     excludes the separator and the whitespace the canonicalization dropped. *)
  let from = ref (-1) and upto = ref (-1) in
  let mark j =
    if !from < 0 then from := j;
    upto := j
  in
  let add c =
    if !pending_ws then begin
      if Buffer.length buf > 0 then Buffer.add_char buf ' ';
      pending_ws := false
    end;
    Buffer.add_char buf c
  in
  let flush () =
    if Buffer.length buf = 0 then raise_notrace Not_a_sequence;
    let extent = { start = !from; length = !upto - !from + 1 } in
    out := { canonical = Buffer.contents buf; extent } :: !out;
    Buffer.clear buf;
    pending_ws := false;
    from := -1;
    upto := -1
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
    | ';' when !depth = 0 -> flush ()
    | '"' ->
        mark !i;
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
        i := copy (!i + 1);
        mark !i
    | '\'' -> (
        (* Collapsing must not reach inside a char literal (['\n'] would
           otherwise lose its meaning), so literals copy verbatim. *)
        mark !i;
        if !pending_ws && Buffer.length buf > 0 then Buffer.add_char buf ' ';
        pending_ws := false;
        match char_literal !i with
        | Some close ->
            i := close;
            mark !i
        | None -> Buffer.add_char buf '\'')
    | ('(' | '[' | '{') as c ->
        mark !i;
        incr depth;
        add c
    | (')' | ']' | '}') as c ->
        mark !i;
        decr depth;
        if !depth < 0 then raise_notrace Not_a_sequence;
        add c
    | c ->
        mark !i;
        add c);
    incr i
  done;
  if !depth <> 0 then raise_notrace Not_a_sequence;
  if Buffer.length buf > 0 then flush ()
  else if !out <> [] then raise_notrace Not_a_sequence (* trailing ';' *);
  Array.of_list (List.rev !out)

let parse s =
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
