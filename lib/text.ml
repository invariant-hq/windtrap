(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let normalize_newlines s =
  let len = String.length s in
  let b = Buffer.create len in
  let rec loop i =
    if i >= len then ()
    else
      match s.[i] with
      | '\r' ->
          if i + 1 < len && s.[i + 1] = '\n' then (
            Buffer.add_char b '\n';
            loop (i + 2))
          else (
            Buffer.add_char b '\n';
            loop (i + 1))
      | c ->
          Buffer.add_char b c;
          loop (i + 1)
  in
  loop 0;
  Buffer.contents b

let length_utf8 s =
  let len = String.length s in
  let rec count byte_pos char_count =
    if byte_pos >= len then char_count
    else
      let decode = String.get_utf_8_uchar s byte_pos in
      let next_pos = byte_pos + Uchar.utf_decode_length decode in
      count next_pos (char_count + 1)
  in
  count 0 0

let truncate_utf8 max_chars s =
  let len = String.length s in
  if len <= max_chars then s
  else
    let rec find_cut_point byte_pos char_count =
      if byte_pos >= len then byte_pos
      else if char_count >= max_chars - 1 then byte_pos
      else
        let decode = String.get_utf_8_uchar s byte_pos in
        let next_pos = byte_pos + Uchar.utf_decode_length decode in
        find_cut_point next_pos (char_count + 1)
    in
    let cut = find_cut_point 0 0 in
    String.sub s 0 cut ^ "..."

let truncate_bytes_utf8 max_bytes s =
  if max_bytes <= 0 then "<truncated>"
  else if String.length s <= max_bytes then s
  else
    (* Walk forward character-by-character; if we overshoot, back up *)
    let rec find_safe_cut byte_pos =
      if byte_pos >= max_bytes then
        (* Walk backwards past any continuation bytes to a character start *)
        let rec backup pos =
          if pos <= 0 then 0
          else
            let c = Char.code s.[pos] in
            (* UTF-8 continuation bytes have form 10xxxxxx *)
            if c land 0xC0 <> 0x80 then pos else backup (pos - 1)
        in
        backup (max_bytes - 1)
      else
        let decode = String.get_utf_8_uchar s byte_pos in
        let next_pos = byte_pos + Uchar.utf_decode_length decode in
        if next_pos > max_bytes then byte_pos else find_safe_cut next_pos
    in
    let cut = find_safe_cut 0 in
    let prefix = String.sub s 0 cut in
    Pp.str "%s... (truncated; %d bytes total)" prefix (String.length s)

(* Uses local exceptions for early exit from nested loops -- raise_notrace
   avoids the overhead of capturing a backtrace on the hot path. *)
let contains_substring ~pattern s =
  let plen = String.length pattern in
  let slen = String.length s in
  if plen = 0 then true
  else if plen > slen then false
  else
    let exception Found in
    let exception Mismatch in
    try
      for i = 0 to slen - plen do
        try
          for j = 0 to plen - 1 do
            if s.[i + j] <> pattern.[j] then raise_notrace Mismatch
          done;
          raise_notrace Found
        with Mismatch -> ()
      done;
      false
    with Found -> true
