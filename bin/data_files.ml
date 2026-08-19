(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Instr = Windtrap_instr

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

let rec find_project_root ~dir current =
  let candidate = data_dir ~dir current in
  if
    (not (under_sandbox current))
    && Sys.file_exists candidate && Sys.is_directory candidate
  then Some current
  else
    let parent = Filename.dirname current in
    if parent = current then None else find_project_root ~dir parent

(* The root rule, shared with the runtimes' output path: when the current
   directory is inside a _build — a dune rule action, sandboxed or not —
   the root is the parent of the topmost _build component,
   unconditionally; only outside _build does the ancestor scan run. *)
let project_root ~dir cwd =
  match Instr.build_root ~path:cwd with
  | Some root -> Some root
  | None -> find_project_root ~dir cwd

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

(* This binary may itself be instrumented — windtrap's own is, in
   windtrap's own tree — and then merely running it registers points and
   dumps them at exit into the directory it just read, where the next
   build turns them stale and it warns about itself forever. A file
   whose recorded writer is this very executable is not data about the
   suite; it is this command's own exhaust, and is dropped before
   anything judges its freshness. Inert for an installed windtrap, which
   carries no instrumentation at all. *)
let self_written = function
  | None -> false
  | Some { Instr.exe; _ } -> exe = Instr.exe_identity ~exe:Sys.executable_name

type freshness = Fresh | Orphan of string | Stale of string

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
            match Instr.file_digest exe_path with
            | Some actual when actual <> digest -> Stale exe
            | Some _ | None -> Fresh))

let describe ~stale_hint ~path = function
  | Fresh -> assert false
  | Orphan exe -> spf "%s: its executable (%s) no longer exists" path exe
  | Stale exe ->
      spf "%s: not written by the executable now at %s - %s" path exe stale_hint
