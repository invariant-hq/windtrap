(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The check of the rule catalogue against the fixtures.

   [check_rules.exe RULES.md] reads every row of the catalogue's tables, whose
   first cell is a rule id such as [C21] and whose last is "pinned by", and
   fails, naming the row, when:
   - the last cell names a fixture that does not exist or does not carry the
     rule's id. A fixture is a backquoted path under [coverage/], [mutate/] or
     [expect/]: [X.ml] as written, [X] for [X.ml], [X*] for every [.ml] file
     of the directory whose name starts with [X]. A path with another
     extension, such as a dune file, names no fixture.
   - the last cell says [unpinned] without a [STATED-NOT-TESTED] reason.
   - a fixture names a rule by its id and interface line, as in
     [(* C21, cov:51 *)], and the rule's row does not name that fixture, or
     no row has that id.

   Paths are relative to the directory of RULES.md. *)

let read_file path =
  In_channel.with_open_bin path (fun ic -> In_channel.input_all ic)

let starts_with ~prefix s = String.starts_with ~prefix s
let is_digit c = c >= '0' && c <= '9'
let is_word c = is_digit c || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

let is_id s =
  String.length s >= 2
  && (s.[0] = 'C' || s.[0] = 'M' || s.[0] = 'E')
  && String.for_all is_digit (String.sub s 1 (String.length s - 1))

(* The cells of a table row, split on the bars that are not escaped. *)
let cells line =
  let b = Buffer.create 80 and acc = ref [] in
  let n = String.length line in
  let i = ref 0 in
  while !i < n do
    (match line.[!i] with
    | '\\' when !i + 1 < n ->
        Buffer.add_char b line.[!i + 1];
        incr i
    | '|' ->
        acc := String.trim (Buffer.contents b) :: !acc;
        Buffer.clear b
    | c -> Buffer.add_char b c);
    incr i
  done;
  match List.rev !acc with _ :: cells -> cells | [] -> []

(* The backquoted spans of [s]. *)
let quoted s =
  let rec go acc i =
    match String.index_from_opt s i '`' with
    | None -> List.rev acc
    | Some a -> (
        match String.index_from_opt s (a + 1) '`' with
        | None -> List.rev acc
        | Some b -> go (String.sub s (a + 1) (b - a - 1) :: acc) (b + 1))
  in
  go [] 0

let families = [ "coverage/"; "mutate/"; "expect/" ]

(* The fixture files a backquoted path names, relative to [root]. *)
let fixtures_of ~root path =
  if not (List.exists (fun prefix -> starts_with ~prefix path) families) then []
  else if String.ends_with ~suffix:"*" path then
    let dir = Filename.dirname path in
    let stem = Filename.basename path in
    let stem = String.sub stem 0 (String.length stem - 1) in
    Sys.readdir (Filename.concat root dir)
    |> Array.to_list
    |> List.filter (fun f ->
        starts_with ~prefix:stem f && Filename.check_suffix f ".ml")
    |> List.sort String.compare
    |> List.map (fun f -> Filename.concat dir f)
  else if Filename.check_suffix path ".ml" then [ path ]
  else if Filename.extension path = "" then [ path ^ ".ml" ]
  else []

(* Whether [text] holds [word] with no word character on either side. *)
let has_word text word =
  let n = String.length text and k = String.length word in
  let rec go i =
    if i > n - k then false
    else if
      String.sub text i k = word
      && (i = 0 || not (is_word text.[i - 1]))
      && (i + k = n || not (is_word text.[i + k]))
    then true
    else go (i + 1)
  in
  go 0

(* The ids that [text] names with an interface line: an id, a comma, blanks,
   then a file name such as [cov] or [expect_test_config.mli], a colon and a
   digit. *)
let cited_ids text =
  let n = String.length text in
  let rec skip p i = if i < n && p text.[i] then skip p (i + 1) else i in
  let is_blank c = c = ' ' || c = '\n' in
  let is_name c = is_word c || c = '_' || c = '.' in
  let cites i =
    let digits = skip is_digit (i + 1) in
    let name = skip is_blank (digits + 1) in
    let colon = skip is_name name in
    digits > i + 1
    && digits < n
    && text.[digits] = ','
    && name > digits + 1
    && colon > name
    && colon + 1 < n
    && text.[colon] = ':'
    && is_digit text.[colon + 1]
  in
  let ids = ref [] in
  for i = 0 to n - 1 do
    if
      (text.[i] = 'C' || text.[i] = 'M' || text.[i] = 'E')
      && (i = 0 || not (is_word text.[i - 1]))
      && cites i
    then ids := String.sub text i (skip is_digit (i + 1) - i) :: !ids
  done;
  List.sort_uniq String.compare !ids

let rec ml_files ~root dir =
  Sys.readdir (Filename.concat root dir)
  |> Array.to_list |> List.sort String.compare
  |> List.concat_map (fun f ->
      let path = Filename.concat dir f in
      if Sys.is_directory (Filename.concat root path) then ml_files ~root path
      else if Filename.check_suffix f ".ml" then [ path ]
      else [])

let () =
  let catalogue =
    match Sys.argv with
    | [| _; catalogue |] -> catalogue
    | _ ->
        prerr_endline "usage: check_rules.exe RULES.md";
        exit 2
  in
  let root = Filename.dirname catalogue in
  let errors = ref 0 in
  let fail fmt =
    Printf.ksprintf
      (fun s ->
        incr errors;
        Printf.eprintf "%s: %s\n" catalogue s)
      fmt
  in
  let rows =
    String.split_on_char '\n' (read_file catalogue)
    |> List.filter_map (fun line ->
        if not (starts_with ~prefix:"| " line) then None
        else
          match cells line with
          | id :: rest when is_id id && rest <> [] ->
              Some (id, List.nth rest (List.length rest - 1))
          | _ -> None)
  in
  let pinned = Hashtbl.create 256 in
  List.iter
    (fun (id, cell) ->
      if has_word cell "unpinned" && not (has_word cell "STATED-NOT-TESTED")
      then fail "%s is unpinned and gives no STATED-NOT-TESTED reason." id;
      List.iter
        (fun path ->
          List.iter
            (fun fixture ->
              Hashtbl.replace pinned (id, fixture) ();
              let file = Filename.concat root fixture in
              if not (Sys.file_exists file) then
                fail "%s names %s, which does not exist." id fixture
              else if not (has_word (read_file file) id) then
                fail "%s names %s, which does not carry %s." id fixture id)
            (fixtures_of ~root path))
        (quoted cell))
    rows;
  let known = List.map fst rows in
  List.iter
    (fun family ->
      let dir = String.sub family 0 (String.length family - 1) in
      List.iter
        (fun fixture ->
          List.iter
            (fun id ->
              if not (List.mem id known) then
                fail "%s cites %s, which no row has." fixture id
              else if not (Hashtbl.mem pinned (id, fixture)) then
                fail "%s cites %s, whose row does not name it." fixture id)
            (cited_ids (read_file (Filename.concat root fixture))))
        (ml_files ~root dir))
    families;
  if !errors > 0 then exit 1
