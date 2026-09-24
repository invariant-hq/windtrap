(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Adapted from windtrap 0.1's lib/text.ml; [escape_controls] and
   [ensure_trailing_newline] are new in v3. *)

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

(* UTF-8-aware operations *)

let length_utf8 s =
  let len = String.length s in
  let rec count byte_pos char_count =
    if byte_pos >= len then char_count
    else
      let decode = String.get_utf_8_uchar s byte_pos in
      count (byte_pos + Uchar.utf_decode_length decode) (char_count + 1)
  in
  count 0 0

(* The result is at most [max_chars] code points, ellipsis included. It used
   to be [max_chars - 1] code points PLUS ["..."] (two over the bound it was
   asked for) which is a display bound that does not bind: the live tail
   sized to the terminal wrapped, and the erase that follows it then left
   residue on the wrapped line. *)
let truncate_utf8 max_chars s =
  let len = String.length s in
  if len <= max_chars || length_utf8 s <= max_chars then s
  else if max_chars <= 3 then String.sub "..." 0 (max 0 max_chars)
  else
    let keep = max_chars - 3 in
    let rec find_cut_point byte_pos char_count =
      if byte_pos >= len then byte_pos
      else if char_count >= keep then byte_pos
      else
        let decode = String.get_utf_8_uchar s byte_pos in
        find_cut_point
          (byte_pos + Uchar.utf_decode_length decode)
          (char_count + 1)
    in
    let cut = find_cut_point 0 0 in
    String.sub s 0 cut ^ "..."

let prefix_bytes_utf8 max_bytes s =
  (* Walk forward character-by-character; [byte_pos] is always a
     character boundary, so landing exactly on [max_bytes] is a valid cut
     and a character straddling it is excluded. *)
  let len = String.length s in
  let rec find_safe_cut byte_pos =
    if byte_pos >= len then byte_pos
    else
      let decode = String.get_utf_8_uchar s byte_pos in
      let next_pos = byte_pos + Uchar.utf_decode_length decode in
      if next_pos > max_bytes then byte_pos else find_safe_cut next_pos
  in
  String.sub s 0 (find_safe_cut 0)

let mark_truncated ~length kept =
  Printf.sprintf "%s... (truncated; %d bytes total)" kept length

let truncate_bytes_utf8 max_bytes s =
  if max_bytes <= 0 then "<truncated>"
  else if String.length s <= max_bytes then s
  else mark_truncated ~length:(String.length s) (prefix_bytes_utf8 max_bytes s)

(* Each cut moves away from the middle, so neither side passes its half. A
   UTF-8 sequence has at most three continuation bytes. *)
let elide_middle max_bytes ~show s =
  if max_bytes < 0 then invalid_arg "Text.elide_middle: negative bound";
  let len = String.length s in
  if len <= max_bytes then show s
  else
    let continues i = Char.code s.[i] land 0xC0 = 0x80 in
    let rec boundary i step steps =
      if steps = 0 || i <= 0 || i >= len || not (continues i) then i
      else boundary (i + step) step (steps - 1)
    in
    let head = boundary (max_bytes / 2) (-1) 3 in
    let tail = boundary (len - (max_bytes / 2)) 1 3 in
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
