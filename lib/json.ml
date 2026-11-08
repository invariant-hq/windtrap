(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf

type value =
  | Int of int
  | Float of float
  | Bool of bool
  | String of string
  | List of value list
  | Object of (string * value) list

(* A malformed sequence is U+FFFD, so the text is always UTF-8. *)
let add_string b s =
  Buffer.add_char b '"';
  let rec loop i =
    if i < String.length s then begin
      let d = String.get_utf_8_uchar s i in
      let n = Uchar.utf_decode_length d in
      (if not (Uchar.utf_decode_is_valid d) then Buffer.add_string b "\u{FFFD}"
       else
         match s.[i] with
         | '"' -> Buffer.add_string b "\\\""
         | '\\' -> Buffer.add_string b "\\\\"
         | '\n' -> Buffer.add_string b "\\n"
         | '\r' -> Buffer.add_string b "\\r"
         | '\t' -> Buffer.add_string b "\\t"
         | c when c < ' ' || c = '\127' ->
             Buffer.add_string b (strf "\\u%04x" (Char.code c))
         | _ -> Buffer.add_substring b s i n);
      loop (i + n)
    end
  in
  loop 0;
  Buffer.add_char b '"'

(* The shortest of the three that reads back. *)
let number f =
  if not (Float.is_finite f) then "null"
  else
    let reads p = float_of_string (strf "%.*g" p f) = f in
    strf "%.*g" (if reads 15 then 15 else if reads 16 then 16 else 17) f

let rec add b = function
  | Int n -> Buffer.add_string b (string_of_int n)
  | Float f -> Buffer.add_string b (number f)
  | Bool v -> Buffer.add_string b (string_of_bool v)
  | String s -> add_string b s
  | List vs ->
      Buffer.add_char b '[';
      List.iteri
        (fun i v ->
          if i > 0 then Buffer.add_string b ", ";
          add b v)
        vs;
      Buffer.add_char b ']'
  | Object fields ->
      Buffer.add_char b '{';
      List.iteri
        (fun i (k, v) ->
          if i > 0 then Buffer.add_string b ", ";
          add_string b k;
          Buffer.add_string b ": ";
          add b v)
        fields;
      Buffer.add_char b '}'

let to_string v =
  let b = Buffer.create 256 in
  add b v;
  Buffer.contents b
