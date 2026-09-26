(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Flexible text *)

(* Whitespace is Base.Char.is_whitespace, the set ppx_expect strips. *)
let is_ws = function
  | ' ' | '\t' | '\n' | '\011' | '\012' | '\r' -> true
  | _ -> false

(* The first offset at or after [i] whose byte does not satisfy [p]. *)
let skip p s i =
  let rec go i = if i < String.length s && p s.[i] then go (i + 1) else i in
  go i

let rstrip s =
  let rec stop i = if i > 0 && is_ws s.[i - 1] then stop (i - 1) else i in
  String.sub s 0 (stop (String.length s))

let spaces n = String.make n ' '

(* ppx_expect's pretty lines: right-stripped, without blank lines at either
   end, and dedented by the least indentation of a nonblank line. The
   indentation counts spaces, and the dedent drops all leading whitespace. *)
let pretty_lines s =
  let rec drop_blanks = function
    | "" :: lines -> drop_blanks lines
    | lines -> lines
  in
  let lines = List.map rstrip (Text.split_lines s) in
  let lines = List.rev (drop_blanks (List.rev (drop_blanks lines))) in
  let indent line = skip (Char.equal ' ') line 0 in
  let margin =
    List.fold_left
      (fun margin line ->
        if line = "" then margin else min margin (indent line))
      max_int lines
  in
  let dedent line =
    if line = "" then ""
    else
      let start = skip is_ws line 0 in
      spaces (indent line - margin)
      ^ String.sub line start (String.length line - start)
  in
  List.map dedent lines

(* ppx_expect compares both sides through its payload formatter, which is
   this form indented as a block, so equal forms are its match. *)
let normalize s = String.concat "\n" (pretty_lines s)

(* Patches *)

type style = Flexible | Exact

type rewrite = {
  site : Loc.pos;
  literal : string;
  style : style;
  content : string;
}

(* A [Trailing] site is an expect test's head, its line and start column,
   with the end of its body as end column, counted from the head's line. *)
type patch =
  | Rewrite of rewrite
  | Trailing of { site : Loc.pos; content : string }

let patch ~site ~literal ~style content =
  Rewrite { site; literal; style; content }

let trailing ~site content = Trailing { site; content }

type error = No_literal of Loc.pos | Drifted of Loc.pos

let error_message = function
  | No_literal _ -> "no string literal at the recorded position"
  | Drifted _ ->
      "the literal differs from the value the binary was compiled with; \
       rebuild and rerun"

(* Literal rendering *)

type delimiter = Quote | Tag of string

let rec fix_tag ~contents tag =
  let occurs delimiter = Text.contains_substring ~pattern:delimiter contents in
  if occurs ("{" ^ tag ^ "|") || occurs ("|" ^ tag ^ "}") then
    fix_tag ~contents (tag ^ "xxx")
  else tag

(* ppx_expect's layout, so a literal that ppx_expect wrote is corrected to
   the same bytes. *)
let format_flexible ~delimiter ~column raw =
  match (pretty_lines raw, delimiter) with
  | [], Quote -> ""
  | [], Tag _ -> " "
  | [ line ], Quote -> line
  | [ line ], Tag _ -> " " ^ line ^ " "
  | lines, _ ->
      let first, indent, last =
        match delimiter with
        | Quote -> (" ", 1, " ")
        | Tag _ ->
            let indent = column + 2 in
            ("", indent, spaces indent)
      in
      let indented line = if line = "" then "" else spaces indent ^ line in
      String.concat "\n" ((first :: List.map indented lines) @ [ last ])

(* The literal that holds [contents], after [ext], the head of a
   [{%ext|…|}] node. *)
let literal' ?ext ~delimiter contents =
  match delimiter with
  | Quote -> "\"" ^ String.escaped contents ^ "\""
  | Tag tag ->
      let tag = fix_tag ~contents tag in
      let head =
        match ext with
        | None -> ""
        | Some ext -> if tag = "" then ext else ext ^ " "
      in
      "{" ^ head ^ tag ^ "|" ^ contents ^ "|" ^ tag ^ "}"

let literal ~delimiter contents = literal' ~delimiter contents

(* Reading literals *)

(* What a position names: the literal from [start] to [stop], whose [value]
   is what the compiler reads, or the closing bracket of a node without
   payload. *)
type found =
  | Literal of {
      start : int;
      stop : int;
      ext : string option;
      delimiter : delimiter;
      value : string;
    }
  | Bare of int

let is_at source i token =
  let n = String.length token in
  i + n <= String.length source && String.equal (String.sub source i n) token

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' -> true
  | _ -> false

let is_tag_char = function 'a' .. 'z' | '_' -> true | _ -> false
let is_blank = function ' ' | '\t' -> true | _ -> false

(* The offset of the first byte of the 1-based [line]. *)
let line_offset source line =
  let rec go bol n =
    if n = line then Some bol
    else
      Option.bind (String.index_from_opt source bol '\n') (fun newline ->
          go (newline + 1) (n + 1))
  in
  go 0 1

(* A run of CRs before an LF reads as that LF, and a lone CR is an ordinary
   byte. OCaml 5.2 and later drop one CR before an LF and earlier versions
   none, so a literal that the compiler reads otherwise is [Drifted]. *)
let decode_newlines s =
  let len = String.length s in
  let b = Buffer.create len in
  let rec go i =
    if i < len then begin
      let lf = skip (Char.equal '\r') s i in
      if lf > i && is_at s lf "\n" then go lf
      else begin
        Buffer.add_char b s.[i];
        go (i + 1)
      end
    end
  in
  go 0;
  Buffer.contents b

(* [{tag|…|tag}] or [{%ext tag|…|tag}] at [i]. *)
let read_tagged source i =
  let ext, first =
    if is_at source i "{%" then
      let stop = skip is_ident_char source (i + 2) in
      ( Some (String.sub source (i + 1) (stop - i - 1)),
        skip (Char.equal ' ') source stop )
    else (None, i + 1)
  in
  let bar = skip is_tag_char source first in
  if not (is_at source bar "|") then None
  else
    let tag = String.sub source first (bar - first) in
    let close = "|" ^ tag ^ "}" in
    Text.first_occurrence ~start:(bar + 1) ~pattern:close source
    |> Option.map (fun k ->
        let value = String.sub source (bar + 1) (k - bar - 1) in
        Literal
          {
            start = i;
            stop = k + String.length close;
            ext;
            delimiter = Tag tag;
            value = decode_newlines value;
          })

(* The number that [digits] digits in base [radix] spell at [at]. *)
let number source ~at ~digits ~radix =
  let digit = function
    | '0' .. '9' as c -> Some (Char.code c - Char.code '0')
    | 'a' .. 'f' as c -> Some (Char.code c - Char.code 'a' + 10)
    | 'A' .. 'F' as c -> Some (Char.code c - Char.code 'A' + 10)
    | _ -> None
  in
  let rec go k n =
    if k = digits then Some n
    else if at + k >= String.length source then None
    else
      match digit source.[at + k] with
      | Some d when d < radix -> go (k + 1) ((n * radix) + d)
      | Some _ | None -> None
  in
  go 0 0

(* ["…"] at [i]. An escape that the lexer rejects is kept as written. *)
let read_quoted source i =
  let len = String.length source in
  let b = Buffer.create 64 in
  let rec go j =
    if j >= len then None
    else
      match source.[j] with
      | '"' ->
          let value = Buffer.contents b in
          Some
            (Literal
               { start = i; stop = j + 1; ext = None; delimiter = Quote; value })
      | '\\' when j + 1 < len -> escape (j + 1)
      (* Only a CR of the source is a line end: a [\r] escape is the byte. *)
      | '\r' ->
          let lf = skip (Char.equal '\r') source j in
          if is_at source lf "\n" then go lf else add '\r' (j + 1)
      | c -> add c (j + 1)
  and add c next =
    Buffer.add_char b c;
    go next
  (* [k] is the byte after the backslash. *)
  and escape k =
    let as_written () =
      Buffer.add_char b '\\';
      add source.[k] (k + 1)
    in
    let add_code n ~next =
      match n with
      | Some n when n < 256 -> add (Char.chr n) next
      | Some _ | None -> as_written ()
    in
    match source.[k] with
    | ('\\' | '"' | '\'' | ' ') as c -> add c (k + 1)
    | 'n' -> add '\n' (k + 1)
    | 't' -> add '\t' (k + 1)
    | 'b' -> add '\b' (k + 1)
    | 'r' -> add '\r' (k + 1)
    (* A line continuation drops the newline, LF or CR LF, and the next
       line's leading blanks. *)
    | '\n' -> go (skip is_blank source (k + 1))
    | '\r' when is_at source (k + 1) "\n" -> go (skip is_blank source (k + 2))
    | '0' .. '9' ->
        add_code (number source ~at:k ~digits:3 ~radix:10) ~next:(k + 3)
    | 'x' ->
        add_code (number source ~at:(k + 1) ~digits:2 ~radix:16) ~next:(k + 3)
    | 'o' ->
        add_code (number source ~at:(k + 1) ~digits:3 ~radix:8) ~next:(k + 4)
    | 'u' when is_at source (k + 1) "{" -> (
        let first = k + 2 in
        match String.index_from_opt source first '}' with
        | None -> as_written ()
        | Some close -> (
            let digits = close - first in
            match number source ~at:first ~digits ~radix:16 with
            | Some v when digits > 0 && Uchar.is_valid v ->
                Buffer.add_utf_8_uchar b (Uchar.of_int v);
                go (close + 1)
            | Some _ | None -> as_written ()))
    | _ -> as_written ()
  in
  go (i + 1)

let position_token = "__POS_OF__"

(* From the position, the literal is the next token after whitespace,
   parentheses, [__POS_OF__] and a node's [[%ext] head. The recorded span is
   never sliced: its end column counts from its start line. *)
let rec find_literal source i =
  let i = skip is_ws source i in
  if i >= String.length source then None
  else
    match source.[i] with
    | '(' -> find_literal source (i + 1)
    | '"' -> read_quoted source i
    | '{' -> read_tagged source i
    | '[' when is_at source i "[%" ->
        let j = skip is_ws source (skip is_ident_char source (i + 2)) in
        if is_at source j "]" then Some (Bare j) else find_literal source j
    | '_' when is_at source i position_token ->
        find_literal source (i + String.length position_token)
    | _ -> None

(* Applying *)

(* The span that the patch replaces in [source], and its new text. *)
let locate_rewrite source (p : rewrite) =
  let _, line, column, _ = p.site in
  match line_offset source line with
  | None -> Error (No_literal p.site)
  | Some bol -> (
      let contents delimiter =
        match p.style with
        | Exact -> p.content
        | Flexible ->
            let column = skip (Char.equal ' ') source bol - bol in
            format_flexible ~delimiter ~column p.content
      in
      (* A CR before an LF inside a tagged literal reads as LF, so exact
         contents that hold a CR are written quoted, where it is [\r]. *)
      let quoted =
        match p.style with
        | Exact -> String.contains p.content '\r'
        | Flexible -> false
      in
      match find_literal source (bol + column) with
      | None -> Error (No_literal p.site)
      | Some (Bare i) ->
          if not (String.equal p.literal "") then Error (Drifted p.site)
          else
            let delimiter = if quoted then Quote else Tag "" in
            Ok (i, i, " " ^ literal ~delimiter (contents delimiter))
      | Some (Literal { start; stop; ext; delimiter; value }) -> (
          if not (String.equal value p.literal) then Error (Drifted p.site)
          else
            match (quoted, ext) with
            | false, _ ->
                Ok (start, stop, literal' ?ext ~delimiter (contents delimiter))
            | true, None ->
                Ok (start, stop, literal ~delimiter:Quote (contents Quote))
            | true, Some ext ->
                (* A [{%ext|…|}] node has no quoted spelling. *)
                let node = literal ~delimiter:Quote (contents Quote) in
                Ok (start, stop, "[" ^ ext ^ " " ^ node ^ "]")))

let test_heads = [ "let%expect_test"; "[%%expect_test" ]

(* The node goes after the body, on a line of its own two columns right of
   the test's head, as ppx_expect inserts it. The head must still be at the
   site and the body must end on a token. *)
let locate_trailing source ~site content =
  let _, line, column, stop = site in
  match line_offset source line with
  | None -> Error (No_literal site)
  | Some bol ->
      let head = bol + column and at = bol + stop in
      if
        not
          (List.exists (is_at source head) test_heads
          && head < at
          && at <= String.length source
          && not (is_ws source.[at - 1]))
      then Error (Drifted site)
      else
        let indent = column + 2 in
        let delimiter = Tag "" in
        let payload =
          literal ~delimiter (format_flexible ~delimiter ~column:indent content)
        in
        Ok (at, at, ";\n" ^ spaces indent ^ "[%expect " ^ payload ^ "]")

let locate source = function
  | Rewrite p -> locate_rewrite source p
  | Trailing { site; content } -> locate_trailing source ~site content

(* [spans] are in the order of their starts. A span that starts before the
   cursor is the same literal patched twice, and is written once. *)
let splice source spans =
  let rec pieces cursor = function
    | [] -> [ String.sub source cursor (String.length source - cursor) ]
    | (start, _, _) :: spans when start < cursor -> pieces cursor spans
    | (start, stop, text) :: spans ->
        String.sub source cursor (start - cursor) :: text :: pieces stop spans
  in
  String.concat "" (pieces 0 spans)

let apply source patches =
  let rec locate_all spans = function
    | [] -> Ok spans
    | p :: patches ->
        Result.bind (locate source p) (fun span ->
            locate_all (span :: spans) patches)
  in
  let by_start (a, _, _) (b, _, _) = Int.compare a b in
  Result.map
    (fun spans -> splice source (List.stable_sort by_start spans))
    (locate_all [] patches)
