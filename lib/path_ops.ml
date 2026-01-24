(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let file_exists path = try Sys.file_exists path with _ -> false

let is_build_dir dir =
  let base = Filename.basename dir in
  base = "_build" || String.starts_with ~prefix:"_build" base

let rec find_project_root_from dir =
  (* Skip _build directories - dune creates fake .git markers in sandboxes *)
  if is_build_dir dir then
    let parent = Filename.dirname dir in
    if parent = dir then None else find_project_root_from parent
  else
    let has marker = file_exists (Filename.concat dir marker) in
    if has "dune-project" || has "dune-workspace" || has ".git" then Some dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else find_project_root_from parent

let project_root () =
  match Env.project_root () with
  | Some r -> r
  | None ->
      let cwd = Sys.getcwd () in
      Option.value ~default:cwd (find_project_root_from cwd)

let default_log_dir () = Filename.concat (project_root ()) "_build/_tests"

let rec mkdir_p path =
  if path = "" || path = "." then ()
  else if Sys.file_exists path then ()
  else begin
    let parent = Filename.dirname path in
    if parent <> path then mkdir_p parent;
    try Unix.mkdir path 0o770 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

(* Converts arbitrary strings into safe filesystem path components.
   Long names are truncated to 40 chars + MD5 hash to stay within
   common filesystem limits while preserving uniqueness. *)
let sanitize_component s =
  let is_ok = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> true
    | _ -> false
  in
  let buf = Buffer.create (String.length s) in
  String.iter (fun c -> Buffer.add_char buf (if is_ok c then c else '_')) s;
  let out = Buffer.contents buf |> String.trim in
  let out = if out = "" || out = "." || out = ".." then "unnamed" else out in
  if String.length out <= 80 then out
  else
    let hash = Digest.to_hex (Digest.string out) in
    String.sub out 0 40 ^ "_" ^ hash

(* On Unix, prefer getpwuid over $HOME since $HOME can be overridden.
   On Windows, getpwuid is unavailable so $HOME is the only option. *)
let home_directory () =
  if Sys.win32 then try Some (Sys.getenv "HOME") with Not_found -> None
  else
    try Some (Unix.getpwuid (Unix.getuid ())).Unix.pw_dir
    with Not_found -> ( try Some (Sys.getenv "HOME") with Not_found -> None)

let collapse_home path =
  match home_directory () with
  | None -> path
  | Some home ->
      if String.starts_with ~prefix:home path then
        let home_len = String.length home in
        "~" ^ String.sub path home_len (String.length path - home_len)
      else path
