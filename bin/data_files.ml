(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Instr = Windtrap_runtime.Instr

let spf = Printf.sprintf

(* Discovery *)

let data_dir ~dir root = Filename.concat (Filename.concat root "_build") dir

(* Ancestor-scan fallback: the nearest ancestor (the current directory
   included) with a _build/<dir> directory — `dune exec windtrap` runs
   from wherever the user is in the checkout. Candidates inside a sandbox
   are never roots: planted garbage under _build/.sandbox must not
   capture the scan. *)
let under_sandbox dir =
  String.map (function '\\' -> '/' | c -> c) dir
  |> String.split_on_char '/' |> List.mem ".sandbox"

let rec scan_ancestors ~dir current =
  let candidate = data_dir ~dir current in
  if
    (not (under_sandbox current))
    && Sys.file_exists candidate && Sys.is_directory candidate
  then Some current
  else
    let parent = Filename.dirname current in
    if parent = current then None else scan_ancestors ~dir parent

(* The root rule, shared with the runtimes' output path: when the current
   directory is inside a _build — a dune rule action, sandboxed or not —
   the root is the parent of the topmost _build component,
   unconditionally; only outside _build does the ancestor scan run. *)
let project_root ~dir cwd =
  match Instr.build_root ~path:cwd with
  | Some root -> Some root
  | None -> scan_ancestors ~dir cwd

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

let discover ~dir ~ext = function
  | [] ->
      Ok
        (match project_root ~dir (Sys.getcwd ()) with
        | None -> ([], [ "." ])
        | Some root ->
            ( List.sort_uniq String.compare
                (files_under ~ext (data_dir ~dir root)),
              [ root ] ))
  | paths ->
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
          (* A relative identity is a path below _build; the file's own
             topmost-_build root locates that _build — the same root
             whether the file was discovered or named on the command
             line. *)
          match Instr.build_root ~path with
          | None -> None
          | Some root ->
              Some (Filename.concat (Filename.concat root "_build") exe)
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
