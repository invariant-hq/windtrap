(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Adapted from windtrap 0.1's lib/path_ops.ml and the path helpers of
   its baseline layer. v3 adds [reconstruct]: sandbox path
   reconstruction that fails when the result cannot be proven to lie under
   the project root. *)

let file_exists path = try Sys.file_exists path with _ -> false
let normalize_sep s = String.map (fun c -> if c = '\\' then '/' else c) s

(* Project root and log root

   Two rules and no marker files: the override, else the build directory
   the process belongs to. Under dune INSIDE_DUNE is the build context —
   [<root>/_build/default], a private [--build-dir] likewise; a sandboxed
   action keeps that value and only moves its cwd under [_build/.sandbox]
   — and by hand the executable's own path names it. The root is the
   directory above the first component whose name starts with [_build];
   nothing is read from disk. Outside any build directory the root is
   the working directory: a non-dune binary run from a subdirectory of
   its project is the case the override variable exists for. *)

let is_build_dir dir =
  String.starts_with ~prefix:"_build" (Filename.basename dir)

let build_dir_of_path path =
  let rec go acc = function
    | [] -> None
    | c :: _ when is_build_dir c ->
        Some (String.concat "/" (List.rev (c :: acc)))
    | c :: rest -> go (c :: acc) rest
  in
  go [] (String.split_on_char '/' (normalize_sep path))

let absolute path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

(* INSIDE_DUNE first: dune exports the context it is building in, which
   is the one answer under a sandboxed action, where the executable may
   have been copied in from elsewhere, and under a private build
   directory. A value that is not such a path — a harness setting the
   variable to [1] — names no build directory and the executable's own
   path decides. *)
let build_dir () =
  List.find_map
    (fun path -> build_dir_of_path (absolute path))
    ((match Env.get_string "INSIDE_DUNE" with Some d -> [ d ] | None -> [])
    @ [ Sys.executable_name ])

let project_root () =
  match Env.project_root () with
  | Some root -> absolute root
  | None -> (
      match build_dir () with
      | Some dir -> Filename.dirname dir
      | None -> Sys.getcwd ())

(* The log root follows the build directory, not the project root: a
   private [--build-dir] then keeps its own capture logs and last-failed
   store, and a tree built without dune never grows a [_build]. *)
let default_log_dir () =
  match build_dir () with
  | Some dir -> Filename.concat dir "_tests"
  | None -> Filename.concat (Filename.get_temp_dir_name ()) "windtrap"

(* Sandbox reconstruction *)

let trim_trailing_slashes s =
  let rec last_non_slash i =
    if i < 0 then -1 else if s.[i] = '/' then last_non_slash (i - 1) else i
  in
  let i = last_non_slash (String.length s - 1) in
  if i < 0 then s else String.sub s 0 (i + 1)

(* Not exported: [reconstruct] and [display] are the two ways out of this
   module, and both prove or relativize the result. A bare strip is the
   unproven guess the module header refuses to hand out. *)
(* The components after a build directory and its context: a sandboxed
   action runs under [_build/.sandbox/<hash>/<context>/], an unsandboxed
   one under [_build/<context>/]. [None] when [comps] holds no build
   directory followed by a context. *)
let rec after_build_context = function
  | build :: ".sandbox" :: _hash :: _context :: rest when is_build_dir build ->
      Some rest
  | build :: _context :: rest when is_build_dir build -> Some rest
  | _ :: rest -> after_build_context rest
  | [] -> None

let strip_build_prefix path =
  let p = normalize_sep path in
  let comps = String.split_on_char '/' p in
  let rec drop acc = function
    | build :: _ as comps when is_build_dir build -> (
        match after_build_context comps with
        | Some rest -> List.rev_append acc rest
        | None -> List.rev_append acc comps)
    | c :: rest -> drop (c :: acc) rest
    | [] -> List.rev acc
  in
  String.concat "/" (drop [] comps)

let build_root dir =
  let comps = String.split_on_char '/' (normalize_sep dir) in
  match after_build_context comps with
  | None -> None
  | Some rest ->
      let kept = List.length comps - List.length rest in
      Some (String.concat "/" (List.filteri (fun i _ -> i < kept) comps))

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
(* Total, deliberately: root discovery reads the cwd, and a test that
   chdirs into a directory it then removes makes [Sys.getcwd] raise. These
   paths are printed from inside failure reports, where a raise would take
   down the whole run after the tests are already done — the report shows an
   absolute path rather than not showing up at all. *)
let relative_to_root path =
  match project_root () with
  | exception Sys_error _ -> path
  | root ->
      let prefix = root ^ "/" in
      if String.starts_with ~prefix path then
        String.sub path (String.length prefix)
          (String.length path - String.length prefix)
      else path

(* Relativized before the build prefix is stripped: a project root that
   itself lies inside a build tree (a scratch root under a sandbox) would
   otherwise never prefix its own paths. The build prefix is then
   stripped from the remainder, so a build copy under the root prints as
   its source. *)
let display path =
  let normalize path =
    let keep seg = seg <> "." && seg <> "" in
    match String.split_on_char '/' path with
    | "" :: rest -> "/" ^ String.concat "/" (List.filter keep rest)
    | segments -> (
        match List.filter keep segments with
        | [] -> "."
        | kept -> String.concat "/" kept)
  in
  let relative = relative_to_root path in
  if relative != path then normalize (strip_build_prefix relative)
  else relative_to_root (normalize (strip_build_prefix path))

let display_artifact = relative_to_root

(* Path components *)

(* Long names are truncated to 40 bytes plus a digest to stay within
   common filesystem limits while preserving uniqueness.

   Any name the mapping altered also carries a digest, of the name as given.
   Without it the mapping is many-to-one — ["parse: empty"] and
   ["parse, empty"] both become [parse__empty] — and two tests then share
   one capture log, which [Capture.with_capture] opens [O_TRUNC]: the second
   test destroys the first test's output while the first test's failure
   report still points at the file. The digest is of the original, so it is
   stable across runs and independent of execution order. *)
let sanitize_component s =
  let is_ok = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> true
    | _ -> false
  in
  let buf = Buffer.create (String.length s) in
  String.iter (fun c -> Buffer.add_char buf (if is_ok c then c else '_')) s;
  let mapped = Buffer.contents buf in
  let short = String.sub (Digest.to_hex (Digest.string s)) 0 8 in
  let out =
    if mapped = "" || mapped = "." || mapped = ".." then "unnamed-" ^ short
    else if String.equal mapped s then mapped
    else mapped ^ "-" ^ short
  in
  if String.length out <= 80 then out
  else String.sub out 0 40 ^ "_" ^ Digest.to_hex (Digest.string s)

(* Filesystem helpers *)

let rec mkdir_p path =
  if path = "" || path = "." then ()
  else if Sys.file_exists path then ()
  else begin
    let parent = Filename.dirname path in
    if parent <> path then mkdir_p parent;
    try Unix.mkdir path 0o770 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end
