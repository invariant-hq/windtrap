(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Newlines *)

let normalize_newlines s =
  if not (String.contains s '\r') then s
  else
    let len = String.length s in
    let b = Buffer.create len in
    let rec loop i =
      if i >= len then ()
      else
        match s.[i] with
        | '\r' ->
            Buffer.add_char b '\n';
            if i + 1 < len && s.[i + 1] = '\n' then loop (i + 2)
            else loop (i + 1)
        | c ->
            Buffer.add_char b c;
            loop (i + 1)
    in
    loop 0;
    Buffer.contents b

let ensure_trailing_newline s =
  if String.ends_with ~suffix:"\n" s then s else s ^ "\n"

(* "a\nb\n" and "a\nb" both split to ["a"; "b"]: a single trailing newline
   terminates the last line instead of opening an empty one. *)
let split_lines s =
  match List.rev (String.split_on_char '\n' s) with
  | "" :: rest -> List.rev rest
  | parts -> List.rev parts

(* Lengths and cuts *)

let length_utf8 s =
  let len = String.length s in
  let rec count i n =
    if i >= len then n
    else count (i + Uchar.utf_decode_length (String.get_utf_8_uchar s i)) (n + 1)
  in
  count 0 0

(* The ellipsis is inside the bound: a display bound that lets the text run
   over wraps the line an erase then misses. *)
let truncate_utf8 max_chars s =
  let len = String.length s in
  if len <= max_chars || length_utf8 s <= max_chars then s
  else if max_chars <= 3 then String.sub "..." 0 (max 0 max_chars)
  else
    let rec cut i n =
      if i >= len || n = max_chars - 3 then i
      else cut (i + Uchar.utf_decode_length (String.get_utf_8_uchar s i)) (n + 1)
    in
    String.sub s 0 (cut 0 0) ^ "..."

(* A cut never lands before a continuation byte unless three precede it: a
   well-formed sequence has at most three, so a malformed one moves a cut by
   three bytes at most. *)
let continues s i = Char.code s.[i] land 0xC0 = 0x80

let rec boundary_before s i steps =
  if steps = 0 || i <= 0 || i >= String.length s || not (continues s i) then i
  else boundary_before s (i - 1) (steps - 1)

let rec boundary_after s i steps =
  if steps = 0 || i >= String.length s || not (continues s i) then i
  else boundary_after s (i + 1) (steps - 1)

type at = Head | Tail | Around of int

(* The byte after the [n]th newline, and the byte after the newline that
   precedes the last [n] lines, a final newline ending the last line. *)
let after_lines s n =
  let rec go i n =
    if n <= 0 then Some i
    else
      match String.index_from_opt s i '\n' with
      | Some j -> go (j + 1) (n - 1)
      | None -> None
  in
  go 0 n

let before_last_lines s n =
  let rec go i n =
    match String.rindex_from_opt s (i - 1) '\n' with
    | Some j -> if n = 1 then Some (j + 1) else go j (n - 1)
    | None -> None
  in
  let len = String.length s in
  if n <= 0 then Some len
  else go (if String.ends_with ~suffix:"\n" s then len - 1 else len) n

let window ?lines ~bytes at s =
  let len = String.length s and bytes = max 0 bytes in
  let start, stop =
    match at with
    | Head ->
        let stop = if len <= bytes then len else boundary_before s bytes 3 in
        let stop =
          match Option.bind lines (after_lines s) with
          | Some line_stop -> min line_stop stop
          | None -> stop
        in
        (0, stop)
    | Tail ->
        let start =
          if len <= bytes then 0 else boundary_after s (len - bytes) 3
        in
        let start =
          match Option.bind lines (before_last_lines s) with
          | Some line_start -> max line_start start
          | None -> start
        in
        (start, len)
    | Around _ when len <= bytes -> (0, len)
    | Around i ->
        let start = boundary_after s (max 0 (min i len - (bytes / 2))) 3 in
        let stop = start + bytes in
        (start, if stop >= len then len else boundary_before s stop 3)
  in
  if start = 0 && stop = len then (0, s)
  else (start, String.sub s start (stop - start))

let mark_truncated ~length kept =
  Printf.sprintf "%s... (truncated; %d bytes total)" kept length

let truncate_bytes_utf8 max_bytes s =
  if max_bytes <= 0 then "<truncated>"
  else if String.length s <= max_bytes then s
  else
    mark_truncated ~length:(String.length s)
      (snd (window ~bytes:max_bytes Head s))

(* Each side keeps at most half, its cut moved away from the middle. *)
let elide_middle max_bytes ~show s =
  if max_bytes < 0 then invalid_arg "Text.elide_middle: negative bound";
  let len = String.length s in
  if len <= max_bytes then show s
  else
    let head = boundary_before s (max_bytes / 2) 3 in
    let tail = boundary_after s (len - (max_bytes / 2)) 3 in
    Printf.sprintf "%s\u{2026} (%d bytes elided)%s"
      (show (String.sub s 0 head))
      (tail - head)
      (show (String.sub s tail (len - tail)))

(* Search *)

(* Naive scan: patterns are assertion- and filter-sized. *)
let first_occurrence ?(start = 0) ~pattern s =
  let n = String.length pattern and len = String.length s in
  if start < 0 || start > len then
    invalid_arg "Text.first_occurrence: start is outside the string";
  if n = 0 then Some start
  else
    let matches_at i =
      let rec go j = j = n || (s.[i + j] = pattern.[j] && go (j + 1)) in
      go 0
    in
    let rec scan i =
      if i + n > len then None else if matches_at i then Some i else scan (i + 1)
    in
    scan start

let contains_substring ~pattern s = first_occurrence ~pattern s <> None

(* Control bytes *)

let control c = (c < ' ' && c <> '\t') || c = '\127'

let escape_controls s =
  if not (String.exists control s) then s
  else begin
    let b = Buffer.create (String.length s + 8) in
    String.iter
      (fun c ->
        if control c then
          Buffer.add_string b (Printf.sprintf "\\x%02x" (Char.code c))
        else Buffer.add_char b c)
      s;
    Buffer.contents b
  end
