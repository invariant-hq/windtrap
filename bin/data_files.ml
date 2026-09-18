(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Instr = Windtrap_runtime.Instr

let spf = Printf.sprintf

(* Discovery *)

let is_dir path = Sys.file_exists path && Sys.is_directory path

(* Ancestor-scan fallback: the nearest ancestor (the current directory
   included) holding an estate — `dune exec windtrap` runs from wherever
   the user is in the checkout, and a tree built without dune keeps its
   files under _windtrap. Both layouts are the runtime's own rule, and
   both are searched, so a project can hold either. Candidates inside a
   sandbox are never roots: planted garbage under _build/.sandbox must
   not capture the scan. *)
let under_sandbox dir =
  String.map (function '\\' -> '/' | c -> c) dir
  |> String.split_on_char '/' |> List.mem ".sandbox"

let candidates format root =
  [
    Instr.data_dir format ~build_dir:(Filename.concat root "_build");
    Instr.standalone_data_dir format ~root;
  ]

let rec scan_ancestors format current =
  let present =
    if under_sandbox current then []
    else List.filter is_dir (candidates format current)
  in
  if present <> [] then Some (present, current)
  else
    let parent = Filename.dirname current in
    if parent = current then None else scan_ancestors format parent

(* The build-directory rule, shared with the runtimes' output path and
   the core's own root rule: the build directory dune names in
   INSIDE_DUNE — the context it is building in, [<root>/_build/default]
   or a private --build-dir's, exported to rule actions and to `dune
   exec` alike — else the one the current directory is inside, a binary
   run by hand from under a build directory. A value that is not such a
   path — a harness's INSIDE_DUNE=1 — names no build directory. Unlike
   the core's rule, this binary's own path is never consulted: an
   installed windtrap lives under dune's install tree, itself a
   _build, and says nothing about the project it is reporting on. *)
let build_dir cwd =
  let named =
    match Sys.getenv_opt "INSIDE_DUNE" with
    | Some context when context <> "" -> [ context ]
    | Some _ | None -> []
  in
  List.find_map (fun path -> Instr.build_dir ~path) (named @ [ cwd ])

(* Inside a build directory the estate is that directory's,
   unconditionally, and the root is its parent; only outside any does
   the ancestor scan run. *)
let estate format cwd =
  match build_dir cwd with
  | Some build_dir ->
      Some ([ Instr.data_dir format ~build_dir ], Filename.dirname build_dir)
  | None -> scan_ancestors format cwd

let is_data_file ~ext path = Filename.check_suffix path ("." ^ ext)

let rec files_under ~ext dir =
  match Sys.readdir dir with
  | exception Sys_error _ -> []
  | entries ->
      Array.fold_left
        (fun acc entry ->
          let path = Filename.concat dir entry in
          match Sys.is_directory path with
          | true -> files_under ~ext path @ acc
          | false -> if is_data_file ~ext path then path :: acc else acc
          | exception Sys_error _ -> acc)
        [] entries

(* Explicit arguments are a contract (see the .mli): a named file must
   exist and carry the suffix, loudly; directories keep the scan's
   tolerance and contribute however many files they contain. *)
let expand_path ~ext path =
  if not (Sys.file_exists path) then
    Error (spf "%s: no such file or directory" path)
  else if Sys.is_directory path then Ok (files_under ~ext path)
  else if is_data_file ~ext path then Ok [ path ]
  else Error (spf "%s: not a .%s file" path ext)

let discover (format : Instr.format) = function
  | [] ->
      let ext = format.ext in
      Ok
        (match estate format (Sys.getcwd ()) with
        | None -> ([], [ "." ])
        | Some (dirs, root) ->
            ( List.sort_uniq String.compare
                (List.concat_map (files_under ~ext) dirs),
              [ root ] ))
  | paths ->
      let ext = format.ext in
      List.fold_left
        (fun acc path ->
          Result.bind acc (fun files ->
              Result.map (fun found -> found @ files) (expand_path ~ext path)))
        (Ok []) paths
      |> Result.map (fun files ->
          (List.sort_uniq String.compare files, [ "." ]))

(* Freshness *)

type freshness = Fresh | Orphan of string | Stale of string

(* One digest per executable, however many dumps it wrote: every run of
   a cram-driven binary leaves a file naming the same executable, and
   hashing a test binary once per file made the aggregate's cost grow
   with invocations rather than with executables. The command runs once
   per process, so the memo is never stale. *)
let digests : (string, string option) Hashtbl.t = Hashtbl.create 64

let exe_digest exe_path =
  match Hashtbl.find_opt digests exe_path with
  | Some digest -> digest
  | None ->
      let digest = Instr.file_digest exe_path in
      Hashtbl.replace digests exe_path digest;
      digest

let freshness ~path identity =
  match (identity : Instr.identity option) with
  | None -> Fresh
  | Some { exe; digest } -> (
      let resolved =
        if not (Filename.is_relative exe) then Some exe
        else
          (* A relative identity is a path below a build directory; the
             file's own locates it — the same directory whether the file
             was discovered or named on the command line. *)
          Option.map
            (fun build_dir -> Filename.concat build_dir exe)
            (Instr.build_dir ~path)
      in
      match resolved with
      | None -> Fresh
      | Some exe_path -> (
          if not (Sys.file_exists exe_path) then Orphan exe
          else
            match exe_digest exe_path with
            | Some actual when actual <> digest -> Stale exe
            | Some _ | None -> Fresh))

let describe ~path = function
  | Fresh -> assert false
  | Orphan exe ->
      spf "%s: its executable (%s) no longer exists; excluding it" path exe
  | Stale exe ->
      spf
        "%s: not written by the executable now at %s (rebuilt since); \
         excluding it"
        path exe

(* A file or two excluded among many is worth a line each. A build without
   the instrumentation excludes every file of the project: the same line per
   test executable, in front of the remedy. *)
let detail_cap = 3

let warnings excluded =
  let more = List.length excluded - detail_cap in
  List.map
    (fun (path, freshness) -> describe ~path freshness)
    (List.filteri (fun i _ -> i < detail_cap) excluded)
  @ if more > 0 then [ spf "... and %d more like that" more ] else []

let all_excluded ~ext excluded =
  let total = List.length excluded in
  let orphans =
    List.length
      (List.filter
         (function Orphan _ -> true | Fresh | Stale _ -> false)
         excluded)
  in
  spf "found %d .%s file%s and every one is %s" total ext
    (if total = 1 then "" else "s")
    (if orphans = total then "orphaned"
     else if orphans = 0 then "stale"
     else spf "stale or orphaned (%d orphaned)" orphans)
