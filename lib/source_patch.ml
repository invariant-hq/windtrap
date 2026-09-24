(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The normalization and the payload formatting are ppx_expect's
   pretty-payload pipeline, moved here from the expect runtime so that
   the facade's [expect] and the rewriter's [%expect] compare and
   correct through one implementation. *)

(* Flexible text *)

(* Whitespace is Base.Char.is_whitespace — the set ppx_expect strips with. *)
let is_ws = function
  | ' ' | '\t' | '\n' | '\011' | '\012' | '\r' -> true
  | _ -> false

let rstrip s =
  let stop = ref (String.length s) in
  while !stop > 0 && is_ws s.[!stop - 1] do
    decr stop
  done;
  if !stop = String.length s then s else String.sub s 0 !stop

let strip s =
  let start = ref 0 and stop = ref (String.length s) in
  while !start < !stop && is_ws s.[!start] do
    incr start
  done;
  while !stop > !start && is_ws s.[!stop - 1] do
    decr stop
  done;
  String.sub s !start (!stop - !start)

let leading_spaces s =
  let n = ref 0 in
  while !n < String.length s && s.[!n] = ' ' do
    incr n
  done;
  !n

(* ppx_expect splits on '\n' treating "\r\n" as one separator; every
   consumer right-strips each line, which removes the '\r' of a "\r\n"
   pair, so a plain split suffices. A lone '\r' inside a line stays a
   literal byte, exactly as upstream. *)
let split_lines s = String.split_on_char '\n' s

let drop_blank_edges lines =
  let rec drop = function "" :: rest -> drop rest | lines -> lines in
  List.rev (drop (List.rev (drop lines)))

(* [(relative indent, stripped contents)] per line of pretty output.
   Indentation counts leading spaces only; contents are stripped of all
   whitespace, tabs included — ppx_expect's legacy rule, kept for
   byte-compatible matching and corrections. *)
let pretty_lines raw =
  let lines = drop_blank_edges (List.map rstrip (split_lines raw)) in
  let indented =
    List.map (fun line -> (leading_spaces line, strip line)) lines
  in
  let min_indent =
    List.fold_left
      (fun acc (indent, contents) ->
        if contents = "" then acc else min acc indent)
      max_int indented
  in
  List.map
    (fun (indent, contents) ->
      ((if contents = "" then 0 else max 0 (indent - min_indent)), contents))
    indented

let spaces n = String.make n ' '

(* The canonical (dedented) form of pretty output. Two flexible payloads
   match iff their normalizations are equal, which is exactly ppx_expect's
   rule of comparing both sides through its payload formatter: the
   formatter is [normalize] plus a uniform re-indent, so equality
   coincides. *)
let normalize s =
  String.concat "\n"
    (List.map
       (fun (indent, contents) ->
         if contents = "" then "" else spaces indent ^ contents)
       (pretty_lines s))

(* Literal rendering *)

type delimiter = Quote | Tag of string

(* Delimiter conflict fixing: grow the tag until neither delimiter occurs
   in the contents. *)
let fix_tag ~contents tag =
  let rec fix tag =
    if
      Text.contains_substring ~pattern:("{" ^ tag ^ "|") contents
      || Text.contains_substring ~pattern:("|" ^ tag ^ "}") contents
    then fix (tag ^ "xxx")
    else tag
  in
  fix tag

(* Format raw output as the contents of a flexible payload. [column] is
   the indentation of the line that holds the node, in leading spaces:
   multi-line contents are indented [column + 2], matching ppx_expect so
   first promotes after adoption produce no churn. *)
let format_flexible ~delimiter ~column raw =
  match pretty_lines raw with
  | [] -> ( match delimiter with Tag _ -> " " | Quote -> "")
  | [ (_, line) ] -> (
      match delimiter with Tag _ -> " " ^ line ^ " " | Quote -> line)
  | lines ->
      let contents_indent = column + 2 in
      let first, indentation, last =
        match delimiter with
        | Quote -> (" ", 1, " ")
        | Tag _ -> ("", contents_indent, spaces contents_indent)
      in
      let render (indent, contents) =
        if contents = "" then "" else spaces (indentation + indent) ^ contents
      in
      String.concat "\n" ((first :: List.map render lines) @ [ last ])

(* A tagged literal, with the string-extension head ([{%expect|…|}],
   [{%expect_exact tag|…|tag}]) kept when the source spelled one: the
   head is the node, so dropping it would drop the expectation. *)
let tagged ?ext ~tag contents =
  let tag = fix_tag ~contents tag in
  let head =
    match ext with
    | None -> ""
    | Some ext -> if tag = "" then ext else ext ^ " "
  in
  "{" ^ head ^ tag ^ "|" ^ contents ^ "|" ^ tag ^ "}"

let literal ~delimiter contents =
  match delimiter with
  | Tag tag -> tagged ~tag contents
  | Quote ->
      "\""
      ^ String.concat "\\n"
          (List.map String.escaped (String.split_on_char '\n' contents))
      ^ "\""

(* Patches *)

type style = Flexible | Exact

type patch = {
  pos : Loc.pos;
  literal : string;
  style : style;
  content : string;
}

let patch ~site ~literal ~style content =
  { pos = site; literal; style; content }

type error = No_literal of Loc.pos | Drifted of Loc.pos

let error_message = function
  | No_literal (file, line, _, _) ->
      Printf.sprintf "%s:%d: no string literal at the recorded position" file
        line
  | Drifted (file, line, _, _) ->
      Printf.sprintf
        "%s:%d: the literal differs from the value the binary was compiled \
         with; rebuild and rerun"
        file line

(* Lexing. The position names the [__POS_OF__ literal] expression, with
   or without its parentheses, the literal itself, or an [[%expect]] node
   whose payload the literal is: from its first byte the literal is the
   next token after any parentheses, the [__POS_OF__] identifier, the
   node's [[%ident] head and whitespace — never a slice of the recorded
   span, whose end column is measured from the start line. A node with no
   payload is the one shape with no literal to rewrite: its correction is
   inserted before the closing bracket. *)

(* The offset of the first byte of the 1-based [line]. *)
let line_offset source line =
  let rec go offset n =
    if n = line then Some offset
    else
      match String.index_from_opt source offset '\n' with
      | Some newline -> go (newline + 1) (n + 1)
      | None -> None
  in
  if line < 1 then None else go 0 1

let line_indent source bol =
  let n = ref 0 in
  while bol + !n < String.length source && source.[bol + !n] = ' ' do
    incr n
  done;
  !n

let position_token = "__POS_OF__"

let starts_with_at source i prefix =
  let n = String.length prefix in
  i + n <= String.length source && String.equal (String.sub source i n) prefix

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' -> true
  | _ -> false

let skip_ws source i =
  let len = String.length source in
  let i = ref i in
  while !i < len && is_ws source.[!i] do
    incr i
  done;
  !i

let skip_ident source i =
  let len = String.length source in
  let i = ref i in
  while !i < len && is_ident_char source.[!i] do
    incr i
  done;
  !i

(* [`Literal i] with the literal's first byte, or [`Bare i] with the
   offset of a payloadless node's closing bracket. *)
let rec find_literal source i =
  let len = String.length source in
  let i = skip_ws source i in
  if i >= len then None
  else
    match source.[i] with
    | '(' -> find_literal source (i + 1)
    | '"' | '{' -> Some (`Literal i)
    | '[' when starts_with_at source i "[%" ->
        let j = skip_ws source (skip_ident source (i + 2)) in
        if j < len && source.[j] = ']' then Some (`Bare j)
        else find_literal source j
    | _ when starts_with_at source i position_token ->
        find_literal source (i + String.length position_token)
    | _ -> None

(* A run of CRs before an LF is read as one LF, and a lone CR is an
   ordinary byte. OCaml 5.2 and later drop one CR before an LF and earlier
   versions none, so a literal that the compiler reads otherwise is refused
   as [Drifted]. *)
let decode_newlines s =
  if not (String.contains s '\r') then s
  else begin
    let b = Buffer.create (String.length s) in
    let n = String.length s in
    let i = ref 0 in
    while !i < n do
      if s.[!i] = '\r' then begin
        let j = ref !i in
        while !j < n && s.[!j] = '\r' do
          incr j
        done;
        if !j < n && s.[!j] = '\n' then i := !j
        else begin
          Buffer.add_char b '\r';
          incr i
        end
      end
      else begin
        Buffer.add_char b s.[!i];
        incr i
      end
    done;
    Buffer.contents b
  end

(* [{tag|…|tag}] or [{%ext tag|…|tag}] at [i]: the extension head if
   any, the delimiter, the decoded contents and the offset past the
   closing delimiter. *)
let read_tagged source i =
  let len = String.length source in
  let ext, tag_start =
    if i + 1 < len && source.[i + 1] = '%' then (
      let stop = skip_ident source (i + 2) in
      let start = ref stop in
      while !start < len && source.[!start] = ' ' do
        incr start
      done;
      (Some (String.sub source (i + 1) (stop - i - 1)), !start))
    else (None, i + 1)
  in
  let j = ref tag_start in
  while
    !j < len && match source.[!j] with 'a' .. 'z' | '_' -> true | _ -> false
  do
    incr j
  done;
  if !j >= len || source.[!j] <> '|' then None
  else
    let tag = String.sub source tag_start (!j - tag_start) in
    let close = "|" ^ tag ^ "}" in
    match Text.first_occurrence ~start:(!j + 1) ~pattern:close source with
    | None -> None
    | Some k ->
        Some
          ( ext,
            Tag tag,
            decode_newlines (String.sub source (!j + 1) (k - !j - 1)),
            k + String.length close )

let digit_value c =
  match c with
  | '0' .. '9' -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' -> Some (Char.code c - Char.code 'a' + 10)
  | 'A' .. 'F' -> Some (Char.code c - Char.code 'A' + 10)
  | _ -> None

(* [n] digits in base [radix] at [j], as a number. *)
let digits source j n radix =
  let rec go k acc =
    if k = n then Some acc
    else if j + k >= String.length source then None
    else
      match digit_value source.[j + k] with
      | Some d when d < radix -> go (k + 1) ((acc * radix) + d)
      | _ -> None
  in
  go 0 0

(* ["…"] at [i], decoded with OCaml's escapes. *)
let read_quoted source i =
  let len = String.length source in
  let buf = Buffer.create 64 in
  let rec go j =
    if j >= len then None
    else
      match source.[j] with
      | '"' -> Some (None, Quote, decode_newlines (Buffer.contents buf), j + 1)
      | '\\' when j + 1 < len -> (
          let c = source.[j + 1] in
          let literal_escape () =
            Buffer.add_char buf '\\';
            Buffer.add_char buf c;
            go (j + 2)
          in
          (* A numeric escape: [consumed] characters after the backslash. *)
          let code n ~consumed =
            match n with
            | Some n when n < 256 ->
                Buffer.add_char buf (Char.chr n);
                go (j + 1 + consumed)
            | _ -> literal_escape ()
          in
          match c with
          | '\\' | '"' | '\'' | ' ' ->
              Buffer.add_char buf c;
              go (j + 2)
          | 'n' ->
              Buffer.add_char buf '\n';
              go (j + 2)
          | 't' ->
              Buffer.add_char buf '\t';
              go (j + 2)
          | 'b' ->
              Buffer.add_char buf '\b';
              go (j + 2)
          | 'r' ->
              Buffer.add_char buf '\r';
              go (j + 2)
          | ('\n' | '\r') when c = '\n' || (j + 2 < len && source.[j + 2] = '\n')
            ->
              (* A line continuation, the newline spelled LF or CRLF as the
                 lexer accepts it: the newline and the next line's leading
                 blanks are not part of the value. *)
              let k = ref (if c = '\n' then j + 2 else j + 3) in
              while !k < len && (source.[!k] = ' ' || source.[!k] = '\t') do
                incr k
              done;
              go !k
          | '0' .. '9' -> code (digits source (j + 1) 3 10) ~consumed:3
          | 'x' -> code (digits source (j + 2) 2 16) ~consumed:3
          | 'o' -> code (digits source (j + 2) 3 8) ~consumed:4
          | 'u' when j + 2 < len && source.[j + 2] = '{' -> (
              match String.index_from_opt source (j + 3) '}' with
              | None -> literal_escape ()
              | Some close -> (
                  let n = close - (j + 3) in
                  match digits source (j + 3) n 16 with
                  | Some v when n > 0 && Uchar.is_valid v ->
                      Buffer.add_utf_8_uchar buf (Uchar.of_int v);
                      go (close + 1)
                  | _ -> literal_escape ()))
          | _ -> literal_escape ())
      | c ->
          Buffer.add_char buf c;
          go (j + 1)
  in
  go (i + 1)

let locate source (p : patch) =
  let _, line, column, _ = p.pos in
  let contents ~delimiter ~bol =
    match p.style with
    | Flexible ->
        format_flexible ~delimiter ~column:(line_indent source bol) p.content
    | Exact -> p.content
  in
  match line_offset source line with
  | None -> Error (No_literal p.pos)
  | Some bol -> (
      match find_literal source (bol + column) with
      | None -> Error (No_literal p.pos)
      | Some (`Bare i) ->
          (* A payloadless node compiles to the empty string; anything
             else means the source gained a payload since the build. *)
          if not (String.equal p.literal "") then Error (Drifted p.pos)
          else
            let delimiter = Tag "" in
            Ok (i, i, " " ^ literal ~delimiter (contents ~delimiter ~bol))
      | Some (`Literal i) -> (
          let read =
            if source.[i] = '{' then read_tagged source i
            else read_quoted source i
          in
          match read with
          | None -> Error (No_literal p.pos)
          | Some (ext, delimiter, value, stop) ->
              if not (String.equal value p.literal) then Error (Drifted p.pos)
              else
                let contents = contents ~delimiter ~bol in
                let text =
                  match delimiter with
                  | Tag tag -> tagged ?ext ~tag contents
                  | Quote -> literal ~delimiter contents
                in
                Ok (i, stop, text)))

let ( let* ) = Result.bind

let apply source patches =
  let* located =
    List.fold_left
      (fun acc p ->
        let* acc = acc in
        let* span = locate source p in
        Ok (span :: acc))
      (Ok []) patches
  in
  let located =
    List.stable_sort (fun (a, _, _) (b, _, _) -> compare a b) located
  in
  let buf = Buffer.create (String.length source + 256) in
  let cursor =
    List.fold_left
      (fun cursor (start, stop, text) ->
        if start < cursor then cursor (* the same literal twice: once *)
        else begin
          Buffer.add_substring buf source cursor (start - cursor);
          Buffer.add_string buf text;
          stop
        end)
      0 located
  in
  Buffer.add_substring buf source cursor (String.length source - cursor);
  Ok (Buffer.contents buf)
