(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Adapted from windtrap 0.1's lib/text.ml; [strip_ansi] and
   [ensure_trailing_newline] are new in v3. *)

(* ───── Newlines ───── *)

let normalize_newlines s =
  let len = String.length s in
  let b = Buffer.create len in
  let rec loop i =
    if i >= len then ()
    else
      match s.[i] with
      | '\r' ->
          Buffer.add_char b '\n';
          if i + 1 < len && s.[i + 1] = '\n' then loop (i + 2) else loop (i + 1)
      | c ->
          Buffer.add_char b c;
          loop (i + 1)
  in
  loop 0;
  Buffer.contents b

let ensure_trailing_newline s =
  if s = "" then "\n"
  else if s.[String.length s - 1] = '\n' then s
  else s ^ "\n"

(* ───── UTF-8-aware operations ───── *)

let length_utf8 s =
  let len = String.length s in
  let rec count byte_pos char_count =
    if byte_pos >= len then char_count
    else
      let decode = String.get_utf_8_uchar s byte_pos in
      count (byte_pos + Uchar.utf_decode_length decode) (char_count + 1)
  in
  count 0 0

let truncate_utf8 max_chars s =
  let len = String.length s in
  if len <= max_chars || length_utf8 s <= max_chars then s
  else
    let rec find_cut_point byte_pos char_count =
      if byte_pos >= len then byte_pos
      else if char_count >= max_chars - 1 then byte_pos
      else
        let decode = String.get_utf_8_uchar s byte_pos in
        find_cut_point
          (byte_pos + Uchar.utf_decode_length decode)
          (char_count + 1)
    in
    let cut = find_cut_point 0 0 in
    String.sub s 0 cut ^ "..."

let truncate_bytes_utf8 max_bytes s =
  if max_bytes <= 0 then "<truncated>"
  else if String.length s <= max_bytes then s
  else
    (* Walk forward character-by-character; [byte_pos] is always a
       character boundary, so landing exactly on [max_bytes] is a valid
       cut and a character straddling it is excluded. *)
    let rec find_safe_cut byte_pos =
      if byte_pos >= max_bytes then byte_pos
      else
        let decode = String.get_utf_8_uchar s byte_pos in
        let next_pos = byte_pos + Uchar.utf_decode_length decode in
        if next_pos > max_bytes then byte_pos else find_safe_cut next_pos
    in
    let cut = find_safe_cut 0 in
    Printf.sprintf "%s... (truncated; %d bytes total)" (String.sub s 0 cut)
      (String.length s)

(* ───── Search ───── *)

(* Local exceptions with raise_notrace give early exit from the nested
   loops without the cost of capturing a backtrace. *)
let contains_substring ~pattern s =
  let plen = String.length pattern in
  let slen = String.length s in
  if plen = 0 then true
  else if plen > slen then false
  else begin
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
  end

(* ───── ANSI escapes ───── *)

let strip_ansi s =
  let len = String.length s in
  let b = Buffer.create len in
  let rec loop i =
    if i >= len then ()
    else
      match s.[i] with
      | '\027' ->
          if i + 1 >= len then () (* trailing ESC: drop *)
          else begin
            match s.[i + 1] with
            | '[' -> csi (i + 2)
            | ']' -> osc (i + 2)
            | _ -> loop (i + 2)
            (* two-byte escape: drop both *)
          end
      | c ->
          Buffer.add_char b c;
          loop (i + 1)
  and csi i =
    (* Skip parameter/intermediate bytes up to and including the final
       byte, which lies in 0x40..0x7e. *)
    if i >= len then ()
    else if s.[i] >= '\x40' && s.[i] <= '\x7e' then loop (i + 1)
    else csi (i + 1)
  and osc i =
    (* Terminated by BEL or by the two-byte string terminator ESC \. *)
    if i >= len then ()
    else
      match s.[i] with
      | '\007' -> loop (i + 1)
      | '\027' when i + 1 < len && s.[i + 1] = '\\' -> loop (i + 2)
      | _ -> osc (i + 1)
  in
  loop 0;
  Buffer.contents b
