(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Not mutated: the mutation loop scopes its mutants with it (see lib/dune). *)
[@@@mutate exclude_file]

let strf = Printf.sprintf

module Files = Map.Make (String)

(* Each file's ranges, sorted, disjoint and apart by at least one line. *)
type t = (int * int) list Files.t

let empty = Files.empty
let is_empty = Files.is_empty

(* A path as both a diff and an instrumenter spell it relative to the root:
   no leading [./]. *)
let rec normal file =
  if String.starts_with ~prefix:"./" file then
    normal (String.sub file 2 (String.length file - 2))
  else file

let rec insert (a, b) = function
  | [] -> [ (a, b) ]
  | (c, d) :: rest when b + 1 < c -> (a, b) :: (c, d) :: rest
  | (c, d) :: rest when d + 1 < a -> (c, d) :: insert (a, b) rest
  | (c, d) :: rest -> insert (min a c, max b d) rest

let add t ~file (a, b) =
  Files.update (normal file)
    (fun ranges -> Some (insert (a, b) (Option.value ranges ~default:[])))
    t

let union a b =
  Files.fold
    (fun file ranges t -> List.fold_left (fun t r -> add t ~file r) t ranges)
    b a

let mem t ~file ~line =
  match Files.find_opt (normal file) t with
  | None -> false
  | Some ranges -> List.exists (fun (a, b) -> a <= line && line <= b) ranges

let files t = List.map fst (Files.bindings t)

let to_string t =
  let range (a, b) = if a = b then string_of_int a else strf "%d-%d" a b in
  String.concat ","
    (List.map
       (fun (file, ranges) ->
         file ^ ":" ^ String.concat "," (List.map range ranges))
       (Files.bindings t))

(* [--lines] *)

let decimal s =
  s <> "" && String.for_all (function '0' .. '9' -> true | _ -> false) s

let range s =
  let line s =
    if decimal s then
      match int_of_string_opt s with Some n when n >= 1 -> Some n | _ -> None
    else None
  in
  match String.split_on_char '-' s with
  | [ a ] -> Option.map (fun a -> (a, a)) (line a)
  | [ a; b ] -> (
      match (line a, line b) with
      | Some a, Some b when a <= b -> Some (a, b)
      | _ -> None)
  | _ -> None

let expected = "FILE:A-B,... with 1 <= A <= B (e.g. lib/a.ml:3-7,12)"

(* An item that holds a colon starts a file, split at its last colon so a
   path may hold one; a bare range continues the file before it. *)
let of_spec spec =
  let item (t, file) s =
    match (String.rindex_opt s ':', file) with
    | Some i, _ -> (
        let file = String.sub s 0 i in
        match range (String.sub s (i + 1) (String.length s - i - 1)) with
        | Some r when file <> "" -> Ok (add t ~file r, Some file)
        | Some _ | None -> Error expected)
    | None, Some file -> (
        match range s with
        | Some r -> Ok (add t ~file r, Some file)
        | None -> Error expected)
    | None, None -> Error expected
  in
  let rec items acc = function
    | [] -> Ok (fst acc)
    | s :: rest -> Result.bind (item acc s) (fun acc -> items acc rest)
  in
  match String.split_on_char ',' (String.trim spec) with
  | [ "" ] -> Error expected
  | parts -> items (empty, None) parts

(* [git diff -U0] *)

(* The new side of a file header, without git's [b/] prefix. A quoted path
   keeps its bytes, as git quotes only the unusual ones. *)
let new_path line =
  let path = String.trim (String.sub line 4 (String.length line - 4)) in
  let path =
    if String.length path >= 2 && path.[0] = '"' then
      String.sub path 1 (String.length path - 2)
    else path
  in
  if path = "/dev/null" then None
  else if String.starts_with ~prefix:"b/" path then
    Some (String.sub path 2 (String.length path - 2))
  else Some path

(* [@@ -a,b +c,d @@]: the [d] lines from [c], one when [,d] is absent, none
   for a hunk that only deletes. *)
let hunk line =
  match String.split_on_char ' ' line with
  | "@@" :: _ :: added :: _ when String.starts_with ~prefix:"+" added -> (
      let added = String.sub added 1 (String.length added - 1) in
      let start, count =
        match String.split_on_char ',' added with
        | [ c ] -> (int_of_string_opt c, Some 1)
        | [ c; d ] -> (int_of_string_opt c, int_of_string_opt d)
        | _ -> (None, None)
      in
      match (start, count) with
      | Some c, Some d when d > 0 && c >= 1 -> Some (c, c + d - 1)
      | _ -> None)
  | _ -> None

(* A header is a [+++] line after a [---] one: an added line that starts
   with [++] reads [+++] too, and follows no [---]. *)
let of_diff text =
  let step (t, file, previous) line =
    let t, file =
      if
        String.starts_with ~prefix:"+++ " line
        && String.starts_with ~prefix:"--- " previous
      then (t, new_path line)
      else
        match (file, hunk line) with
        | Some file, Some r -> (add t ~file r, Some file)
        | _ -> (t, file)
    in
    (t, file, line)
  in
  let t, _, _ =
    List.fold_left step (empty, None, "") (String.split_on_char '\n' text)
  in
  t
