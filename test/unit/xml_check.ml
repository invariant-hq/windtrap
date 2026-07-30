(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A minimal XML 1.0 well-formedness checker for the JUnit renderer tests:
   optional declaration, one root element, balanced tags, quoted attributes,
   known entity references, no raw '<'/'&' in character data, no control
   bytes. A test asset, deliberately not a dependency; not a general parser
   (no DOCTYPE, PIs, or CDATA — the renderer emits none). *)

exception Bad of string

let bad fmt = Printf.ksprintf (fun m -> raise (Bad m)) fmt

let check (s : string) : (unit, string) result =
  let len = String.length s in
  let pos = ref 0 in
  let peek () = if !pos < len then Some s.[!pos] else None in
  let next () =
    match peek () with
    | Some c ->
        incr pos;
        c
    | None -> bad "unexpected end of document"
  in
  let looking_at p =
    String.length p <= len - !pos && String.sub s !pos (String.length p) = p
  in
  let expect p =
    if looking_at p then pos := !pos + String.length p
    else bad "expected %S at byte %d" p !pos
  in
  let is_ws c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
  let skip_ws () =
    while match peek () with Some c -> is_ws c | None -> false do
      incr pos
    done
  in
  let name () =
    let start = !pos in
    let name_char c =
      (c >= 'a' && c <= 'z')
      || (c >= 'A' && c <= 'Z')
      || (c >= '0' && c <= '9')
      || c = '_' || c = '-' || c = '.' || c = ':'
    in
    while match peek () with Some c -> name_char c | None -> false do
      incr pos
    done;
    if !pos = start then bad "empty name at byte %d" start;
    String.sub s start (!pos - start)
  in
  let entity () =
    (* at '&' *)
    let start = !pos in
    incr pos;
    let stop = min len (start + 12) in
    let rec find i =
      if i >= stop then bad "unterminated entity at byte %d" start
      else if s.[i] = ';' then i
      else find (i + 1)
    in
    let semi = find !pos in
    let body = String.sub s !pos (semi - !pos) in
    (match body with
    | "lt" | "gt" | "amp" | "quot" | "apos" -> ()
    | _ when String.length body > 1 && body.[0] = '#' ->
        let digits = String.sub body 1 (String.length body - 1) in
        let num =
          if digits.[0] = 'x' then
            int_of_string_opt
              ("0x" ^ String.sub digits 1 (String.length digits - 1))
          else int_of_string_opt digits
        in
        if num = None then
          bad "bad character reference &%s; at byte %d" body start
    | _ -> bad "unknown entity &%s; at byte %d" body start);
    pos := semi + 1
  in
  let attributes () =
    let stop = ref false in
    while not !stop do
      skip_ws ();
      match peek () with
      | Some ('/' | '>') | None -> stop := true
      | Some _ ->
          ignore (name ());
          expect "=";
          let quote = next () in
          if quote <> '"' && quote <> '\'' then
            bad "unquoted attribute at byte %d" !pos;
          let finished = ref false in
          while not !finished do
            match next () with
            | c when c = quote -> finished := true
            | '<' -> bad "raw '<' in attribute at byte %d" (!pos - 1)
            | '&' ->
                decr pos;
                entity ()
            | _ -> ()
          done
    done
  in
  let rec element () =
    expect "<";
    let tag = name () in
    attributes ();
    if looking_at "/>" then expect "/>"
    else begin
      expect ">";
      content ();
      expect "</";
      let closing = name () in
      if closing <> tag then bad "mismatched </%s> for <%s>" closing tag;
      skip_ws ();
      expect ">"
    end
  and content () =
    let stop = ref false in
    while not !stop do
      if looking_at "</" then stop := true
      else
        match peek () with
        | None -> stop := true
        | Some '<' -> element ()
        | Some '&' -> entity ()
        | Some c when Char.code c < 0x20 && not (is_ws c) ->
            bad "control byte 0x%02x in text at byte %d" (Char.code c) !pos
        | Some _ -> incr pos
    done
  in
  match
    skip_ws ();
    if looking_at "<?xml" then begin
      let rec close i =
        if i + 1 >= len then bad "unterminated declaration"
        else if s.[i] = '?' && s.[i + 1] = '>' then i + 2
        else close (i + 1)
      in
      pos := close !pos
    end;
    skip_ws ();
    element ();
    skip_ws ();
    if !pos <> len then bad "trailing content at byte %d" !pos
  with
  | () -> Ok ()
  | exception Bad m -> Error m
