(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Adapted from windtrap 0.1's lib/path_ops.ml and the path helpers of
   its lib/snapshot.ml. v3 adds [reconstruct]: sandbox path
   reconstruction that fails when the result cannot be proven to lie under
   the project root. *)

let file_exists path = try Sys.file_exists path with _ -> false

(* Project root *)

let is_build_dir dir =
  String.starts_with ~prefix:"_build" (Filename.basename dir)

(* The marker walk must start above any _build tree, not merely skip the
   _build component when the ascent reaches it: dune materializes source
   files (dune-project included) under _build and sandboxes add decoy
   .git markers, so a marker found anywhere inside the build tree is a
   decoy. Returns the parent of the topmost _build component of [dir],
   or [dir] unchanged when there is none. *)
let above_build_tree dir =
  let rec loop d above =
    let parent = Filename.dirname d in
    let above = if is_build_dir d then Some parent else above in
    if parent = d then above else loop parent above
  in
  Option.value ~default:dir (loop dir None)

let find_project_root_from dir =
  let rec walk dir =
    let has marker = file_exists (Filename.concat dir marker) in
    if has "dune-project" || has "dune-workspace" || has ".git" then Some dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else walk parent
  in
  walk (above_build_tree dir)

let project_root () =
  match Env.project_root () with
  | Some r ->
      if Filename.is_relative r then Filename.concat (Sys.getcwd ()) r else r
  | None ->
      let cwd = Sys.getcwd () in
      Option.value ~default:cwd (find_project_root_from cwd)

let default_log_dir () = Filename.concat (project_root ()) "_build/_tests"

(* Sandbox reconstruction *)

let normalize_sep s = String.map (fun c -> if c = '\\' then '/' else c) s

let trim_trailing_slashes s =
  let rec last_non_slash i =
    if i < 0 then -1 else if s.[i] = '/' then last_non_slash (i - 1) else i
  in
  let i = last_non_slash (String.length s - 1) in
  if i < 0 then s else String.sub s 0 (i + 1)

let strip_build_prefix path =
  let p = normalize_sep path in
  let comps = String.split_on_char '/' p in
  let rec drop acc = function
    | "_build" :: _context :: rest -> List.rev_append acc rest
    | c :: rest -> drop (c :: acc) rest
    | [] -> List.rev acc
  in
  String.concat "/" (drop [] comps)

let is_drive c0 c1 =
  (('A' <= c0 && c0 <= 'Z') || ('a' <= c0 && c0 <= 'z')) && c1 = ':'

let is_absolute p =
  let n = String.length p in
  (n > 0 && p.[0] = '/') || (n >= 3 && is_drive p.[0] p.[1] && p.[2] = '/')

(* Splits an absolute '/'-separated path into an anchor ("" for Unix
   roots, "C:" for drives) and lexically normalized components. [None]
   when the path is not absolute or ".." escapes above the anchor. *)
let split_normalize p =
  if not (is_absolute p) then None
  else
    match String.split_on_char '/' p with
    | anchor :: rest ->
        let rec norm acc = function
          | [] -> Some (List.rev acc)
          | ("" | ".") :: rest -> norm acc rest
          | ".." :: rest -> (
              match acc with [] -> None | _ :: tl -> norm tl rest)
          | c :: rest -> norm (c :: acc) rest
        in
        Option.map (fun comps -> (anchor, comps)) (norm [] rest)
    | [] -> None

let rec is_prefix xs ys =
  match (xs, ys) with
  | [], _ -> true
  | _, [] -> false
  | x :: xs, y :: ys -> String.equal x y && is_prefix xs ys

let reconstruct ~root file =
  let root = trim_trailing_slashes (normalize_sep root) in
  let stripped = strip_build_prefix file in
  let candidate =
    if is_absolute stripped then stripped else root ^ "/" ^ stripped
  in
  match (split_normalize root, split_normalize candidate) with
  | Some (root_anchor, root_comps), Some (anchor, comps)
    when String.equal root_anchor anchor
         && is_prefix root_comps comps
         && List.length comps > List.length root_comps ->
      Ok (anchor ^ "/" ^ String.concat "/" comps)
  | _ -> Error candidate

(* Display paths *)

(* Path shown in reports and command hints: build-sandbox
   and project-root prefixes stripped, then lexically normalized — interior
   ["."] and empty segments dropped ([".."] untouched), so the printed path
   is byte-equal across every producer of the line class, the library and
   inline runners alike (dune runs tests with argv0 ["./t.exe"], whose
   concatenation would otherwise carry a ["/./"]). Best effort. *)
let display path =
  let root = project_root () in
  let path = strip_build_prefix path in
  let path =
    match String.split_on_char '/' path with
    | [] -> path
    | first :: rest ->
        String.concat "/"
          (first :: List.filter (fun seg -> seg <> "." && seg <> "") rest)
  in
  let prefix = root ^ "/" in
  if String.starts_with ~prefix path then
    String.sub path (String.length prefix)
      (String.length path - String.length prefix)
  else path

(* Path components *)

(* Long names are truncated to 40 bytes plus a digest to stay within
   common filesystem limits while preserving uniqueness. *)
let sanitize_component s =
  let is_ok = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> true
    | _ -> false
  in
  let buf = Buffer.create (String.length s) in
  String.iter (fun c -> Buffer.add_char buf (if is_ok c then c else '_')) s;
  let out = Buffer.contents buf in
  let out = if out = "" || out = "." || out = ".." then "unnamed" else out in
  if String.length out <= 80 then out
  else
    let hash = Digest.to_hex (Digest.string out) in
    String.sub out 0 40 ^ "_" ^ hash

(* Filesystem helpers *)

let rec mkdir_p path =
  if path = "" || path = "." then ()
  else if Sys.file_exists path then ()
  else begin
    let parent = Filename.dirname path in
    if parent <> path then mkdir_p parent;
    try Unix.mkdir path 0o770 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

(* On Unix, prefer getpwuid over $HOME since $HOME can be overridden. On
   Windows, getpwuid is unavailable so $HOME is the only option. *)
let home_directory () =
  if Sys.win32 then Sys.getenv_opt "HOME"
  else
    try Some (Unix.getpwuid (Unix.getuid ())).Unix.pw_dir
    with Not_found | Unix.Unix_error _ -> Sys.getenv_opt "HOME"

let collapse_home path =
  match home_directory () with
  | None -> path
  | Some home ->
      let home = trim_trailing_slashes home in
      let n = String.length home in
      (* The prefix must end at a component boundary: "/home/user2" is
         not under "/home/user". [n > 1] keeps a degenerate "/" home
         from collapsing every absolute path. *)
      if
        n > 1
        && String.starts_with ~prefix:home path
        && (String.length path = n || path.[n] = '/' || path.[n] = '\\')
      then "~" ^ String.sub path n (String.length path - n)
      else path
