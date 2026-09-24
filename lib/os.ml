(*---------------------------------------------------------------------------
   Copyright (c) 2015 The mtime programmers. All rights reserved.
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Monotonic clock *)

external elapsed_ns : unit -> int64 = "ocaml_windtrap_clock_elapsed_ns"

(* Force a call at module load time to initialize the C-side clock
   origin, so the int64 counters stay small and subtraction-safe. *)
let () = ignore (elapsed_ns ())

type counter = int64

let counter () = elapsed_ns ()
let count start = Int64.sub (elapsed_ns ()) start
let count_s start = Int64.to_float (count start) /. 1_000_000_000.

(* Environment variables *)

(* An empty value counts as unset: it lets callers clear a variable in
   environments without unsetenv, and `VAR= cmd` reads as "not set". *)
let getenv name =
  match Sys.getenv_opt name with Some "" | None -> None | Some s -> Some s

(* [Unix.putenv] is only the binding half, and binding to "" is not
   unbinding ([Sys.getenv_opt] answers [Some ""]). The unbinding half is
   C's (os_stubs.c), so [Run.setenv] can put back a variable the test
   found unset. The name check sits here so one bad name reads the same on
   every platform. *)
external unsetenv : string -> unit = "ocaml_windtrap_unsetenv"

let setenv name value =
  if name = "" || String.contains name '=' then
    invalid_arg
      (* No non-ASCII here: [Printexc.to_string] renders the payload with
         [%S], so anything outside ASCII reaches reports as escaped bytes. *)
      (Printf.sprintf
         "windtrap: %S is not a usable environment variable name: a name is \
          non-empty and contains no '='"
         name);
  match value with Some v -> Unix.putenv name v | None -> unsetenv name

(* The one boolean vocabulary: a mirror refuses anything outside it, so a
   typo cannot read as "off". *)
let bool_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "1" | "true" | "yes" | "y" | "on" -> Some true
  | "0" | "false" | "no" | "n" | "off" -> Some false
  | _ -> None

let bool_expected = "a boolean: 1/0, true/false, yes/no or on/off"

let split_comma s =
  String.split_on_char ',' s |> List.map String.trim
  |> List.filter (fun s -> s <> "")

(* Set-and-not-falsy: `CI=false` must not count as CI. *)
let is_flagged name =
  match getenv name with
  | None -> false
  | Some s -> ( match bool_of_string s with Some b -> b | None -> true)

let inside_dune () = is_flagged "INSIDE_DUNE"
let is_tty_stdout () = Unix.isatty Unix.stdout

(* TERM=dumb is the near-universal "no escape sequences" convention (git,
   cargo, Emacs M-x shell); the exact spelling, like git's check. *)
let term_dumb () =
  match getenv "TERM" with Some "dumb" -> true | Some _ | None -> false

(* One classification behind both predicates, so a GITHUB_ACTIONS without
   a CI cannot count as GitHub Actions. *)
type ci = Not_ci | Github_actions | Other_ci

let ci () =
  if not (is_flagged "CI") then Not_ci
  else if is_flagged "GITHUB_ACTIONS" then Github_actions
  else Other_ci

let in_ci () = ci () <> Not_ci
let in_github_actions () = ci () = Github_actions

type color_mode = Always | Never | Auto

let color_mode_of_string s =
  match String.lowercase_ascii s with
  | "always" -> Some Always
  | "never" -> Some Never
  | "auto" -> Some Auto
  | _ -> None

(* NO_COLOR is read here rather than passed: it is a fact about the
   environment, not about one sink, and every command must honour it. An
   explicit [Always] still wins: the user asked. [inside_dune] counts as a
   terminal because dune captures the output and renders its escape
   sequences back to the user. *)
let resolve_color mode ~tty ~inside_dune ~term_dumb =
  match mode with
  | Always -> true
  | Never -> false
  | Auto -> (tty || inside_dune) && (not term_dumb) && getenv "NO_COLOR" = None

(* Atomic file writes

   Exclusive temporary creation, EINTR-safe writes, and rename-based
   replacement; no fsync ceremony and no per-operation error catalogue:
   publication atomicity is the whole contract. *)

let temp_prefix = ".tmp-"
let is_temp_name name = String.starts_with ~prefix:temp_prefix name
let temp_serial = Atomic.make 0
let temp_attempts = 256

let describe = function
  | Unix.Unix_error (error, _, _) -> Unix.error_message error
  | Sys_error message | Failure message -> message
  | exception_value -> Printexc.to_string exception_value

let fail path step exception_value =
  raise
    (Sys_error
       (Printf.sprintf "%s: %s: %s" path step (describe exception_value)))

(* Runs one step, translating failures to the module's Sys_error.
   Asynchronous and resource-exhaustion exceptions pass through unwrapped
   so callers still observe interrupts as interrupts. *)
let step path name callback =
  try callback () with
  | (Sys.Break | Out_of_memory | Stack_overflow) as exception_value ->
      let backtrace = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace exception_value backtrace
  | exception_value -> fail path name exception_value

let rec open_temp path flags perm =
  try Unix.openfile path flags perm
  with Unix.Unix_error (Unix.EINTR, _, _) -> open_temp path flags perm

(* EINTR on close leaves the descriptor state formally unspecified, but
   every supported platform closes it; retrying could close a reused
   descriptor. *)
let close_fd fd =
  try Unix.close fd with Unix.Unix_error (Unix.EINTR, _, _) -> ()

let create_temp directory perm =
  let rec attempt remaining =
    let name =
      Printf.sprintf "%s%x-%x" temp_prefix (Unix.getpid ())
        (Atomic.fetch_and_add temp_serial 1)
    in
    let temp = Filename.concat directory name in
    match open_temp temp Unix.[ O_WRONLY; O_CREAT; O_EXCL; O_CLOEXEC ] perm with
    | fd -> (temp, fd)
    | exception Unix.Unix_error (Unix.EEXIST, _, _) when remaining > 1 ->
        attempt (remaining - 1)
  in
  attempt temp_attempts

let rec write_all fd contents offset =
  if offset < String.length contents then
    match
      Unix.single_write_substring fd contents offset
        (String.length contents - offset)
    with
    | 0 -> failwith "write returned zero"
    | written -> write_all fd contents (offset + written)
    | exception Unix.Unix_error (Unix.EINTR, _, _) ->
        write_all fd contents offset

let atomic_write ?(perm = 0o666) ~path contents =
  if perm land lnot 0o777 <> 0 then
    invalid_arg "Os.atomic_write: perm must contain only bits within 0o777";
  (* Renaming over a symlink would silently substitute a regular file for
     the link while the real target kept the old bytes. Publication never
     changes what kind of thing a path names: refuse before any write. *)
  (match Unix.lstat path with
  | { Unix.st_kind = Unix.S_LNK; _ } ->
      raise
        (Sys_error
           (path
          ^ ": is a symbolic link; atomic replacement would substitute a \
             regular file for the link, so it is refused"))
  | _ -> ()
  | exception Unix.Unix_error _ -> ());
  let directory = Filename.dirname path in
  let temp, fd =
    step path "cannot create temporary file" (fun () ->
        create_temp directory perm)
  in
  let closed = ref false in
  let close_once () =
    if not !closed then begin
      closed := true;
      close_fd fd
    end
  in
  let cleanup () =
    (try close_once () with _ -> ());
    try Unix.unlink temp with _ -> ()
  in
  let publish () =
    step path "cannot write" (fun () -> write_all fd contents 0);
    step path "cannot close" close_once;
    step path "cannot replace" (fun () -> Unix.rename temp path)
  in
  try publish ()
  with exception_value ->
    let backtrace = Printexc.get_raw_backtrace () in
    cleanup ();
    Printexc.raise_with_backtrace exception_value backtrace

(* Project root and log root

   Two rules and no marker files: the override, else the build directory
   the process belongs to. Under dune INSIDE_DUNE is the build context
   ([<root>/_build/default], a private [--build-dir] likewise; a sandboxed
   action keeps that value and only moves its cwd under [_build/.sandbox])
   and by hand the executable's own path names it. Outside any build
   directory the root is the working directory: a non-dune binary run from
   a subdirectory of its project is the case the override exists for. *)

let file_exists path = try Sys.file_exists path with _ -> false
let normalize_sep s = String.map (fun c -> if c = '\\' then '/' else c) s

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
   is the one answer under a sandboxed action and under a private build
   directory. The executable's directory, never its own name: a binary
   called [_build_x.exe] is not a build directory. A value that is no path (a harness's INSIDE_DUNE=1) is a
   relative path like any other, so it names a build directory only when
   the working directory lies under one. *)
let build_dir () =
  List.find_map
    (fun path -> build_dir_of_path (absolute path))
    ((match getenv "INSIDE_DUNE" with Some d -> [ d ] | None -> [])
    @ [ Filename.dirname Sys.executable_name ])

let project_root () =
  match getenv "WINDTRAP_PROJECT_ROOT" with
  | Some root -> absolute root
  | None -> (
      match build_dir () with
      | Some dir -> Filename.dirname dir
      | None -> Sys.getcwd ())

(* The log root follows the build directory, not the project root: a
   private [--build-dir] keeps its own logs, and a tree built without dune
   never grows a [_build]. *)
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

(* Not exported: [reconstruct] and [display_path] are the two ways out,
   and both prove or relativize the result. A bare strip is the unproven
   guess this module refuses to hand out. [Baseline.write] creates
   directories from a reconstructed path, so a guess would create them in
   the wrong place. *)
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

(* Display paths

   Total, deliberately: root discovery reads the cwd, and a test that
   chdirs into a directory it then removes makes [Sys.getcwd] raise.
   These paths are printed from inside failure reports, where a raise
   would take down the whole run after the tests are already done. *)

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
   otherwise never prefix its own paths. Interior ["."] and empty segments
   are dropped so the printed path is byte-equal across every producer
   (dune runs tests with argv0 ["./t.exe"]). *)
let display_path path =
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

(* Path components

   Any name the mapping altered carries a digest of the name as given:
   without it the mapping is many-to-one (["parse: empty"] and
   ["parse, empty"] both become [parse__empty]) and two tests share one
   capture log, which [Capture] opens [O_TRUNC]. Long names are truncated
   to 40 bytes plus a digest to stay within filesystem limits. *)
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

(* Standard error *)

(* Standard output is flushed first: a log that merges the two streams
   orders them by flush. A closed standard output does not cost the line. *)
let say message =
  (try
     Format.pp_print_flush Format.std_formatter ();
     flush stdout
   with Sys_error _ -> ());
  Format.pp_print_flush Format.err_formatter ();
  (* A control byte would restyle the terminal or garble the line; a line
     feed stays, since a diagnostic may span lines. *)
  let lines =
    List.map Text.escape_controls (String.split_on_char '\n' message)
  in
  prerr_string ("windtrap: " ^ String.concat "\n" lines ^ "\n");
  flush stderr

let warn message = say ("warning: " ^ message)
