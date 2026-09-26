(*---------------------------------------------------------------------------
   Copyright (c) 2015 The mtime programmers. All rights reserved.
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf

(* Monotonic clock *)

external elapsed_ns : unit -> int64 = "ocaml_windtrap_clock_elapsed_ns"

(* The first call fixes the origin of the clock, so counters stay small. *)
let () = ignore (elapsed_ns ())

type counter = int64

let counter = elapsed_ns
let count start = Int64.sub (elapsed_ns ()) start
let count_s start = Int64.to_float (count start) /. 1e9

(* Environment variables *)

(* An empty value counts as unset: [VAR= cmd] then reads as unset, and a
   platform without [unsetenv] can still clear a variable. *)
let getenv name =
  match Sys.getenv_opt name with
  | Some "" | None -> None
  | Some _ as value -> value

(* [Unix.putenv name ""] binds [name] to the empty string, so unbinding
   takes a C stub. *)
external unsetenv : string -> unit = "ocaml_windtrap_unsetenv"

(* The name is checked here, so a bad one fails alike on every platform. The
   message stays ASCII, since [Printexc.to_string] prints it with [%S]. *)
let setenv name value =
  if name = "" || String.contains name '=' then
    invalid_arg
      (strf
         "windtrap: %S is not a usable environment variable name: a name is \
          non-empty and contains no '='"
         name);
  match value with Some v -> Unix.putenv name v | None -> unsetenv name

let bool_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "1" | "true" | "yes" | "y" | "on" -> Some true
  | "0" | "false" | "no" | "n" | "off" -> Some false
  | _ -> None

let bool_expected = "a boolean: 1/0, true/false, yes/no or on/off"

let split_comma s =
  String.split_on_char ',' s |> List.map String.trim
  |> List.filter (fun s -> s <> "")

let is_flagged name =
  match getenv name with
  | None -> false
  | Some value -> Option.value (bool_of_string value) ~default:true

let inside_dune () = is_flagged "INSIDE_DUNE"
let is_tty_stdout () = Unix.isatty Unix.stdout

(* [TERM=dumb] is the common convention for a terminal without escape
   sequences, compared as it is spelled, as git does. *)
let term_dumb () =
  match getenv "TERM" with Some "dumb" -> true | Some _ | None -> false

let in_ci () = is_flagged "CI"
let in_github_actions () = in_ci () && is_flagged "GITHUB_ACTIONS"

type color_mode = Always | Never | Auto

let color_mode_of_string s =
  match String.lowercase_ascii s with
  | "always" -> Some Always
  | "never" -> Some Never
  | "auto" -> Some Auto
  | _ -> None

(* [NO_COLOR] describes the environment and not a sink, so it is read here for
   every caller. Dune replays the escape sequences of the output it captures,
   so [inside_dune] counts as a terminal. *)
let resolve_color mode ~tty ~inside_dune ~term_dumb =
  match mode with
  | Always -> true
  | Never -> false
  | Auto -> (tty || inside_dune) && (not term_dumb) && getenv "NO_COLOR" = None

(* Atomic file writes *)

let temp_prefix = ".tmp-"

(* With the pid, the serial names each temporary of a process apart. *)
let temp_serial = Atomic.make 0

(* An interrupt and exhausted resources pass through as themselves. *)
let step path failed f =
  try f () with
  | (Sys.Break | Out_of_memory | Stack_overflow) as exn ->
      Printexc.raise_with_backtrace exn (Printexc.get_raw_backtrace ())
  | exn ->
      let reason =
        match exn with
        | Unix.Unix_error (error, _, _) -> Unix.error_message error
        | Sys_error reason -> reason
        | exn -> Printexc.to_string exn
      in
      raise (Sys_error (strf "%s: %s: %s" path failed reason))

let on_failure ~undo f =
  try f ()
  with exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    (try undo () with _ -> ());
    Printexc.raise_with_backtrace exn backtrace

(* A name can be taken already: a crashed run whose pid is reused leaves its
   temporary behind. *)
let create_temp directory perm =
  let rec attempt remaining =
    let serial = Atomic.fetch_and_add temp_serial 1 in
    let name = strf "%s%x-%x" temp_prefix (Unix.getpid ()) serial in
    let temp = Filename.concat directory name in
    match
      Unix.openfile temp Unix.[ O_WRONLY; O_CREAT; O_EXCL; O_CLOEXEC ] perm
    with
    | fd -> (temp, fd)
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> attempt remaining
    | exception Unix.Unix_error (Unix.EEXIST, _, _) when remaining > 1 ->
        attempt (remaining - 1)
  in
  attempt 256

let rec write_all fd s first =
  let length = String.length s - first in
  if length > 0 then
    match Unix.single_write_substring fd s first length with
    | 0 -> raise (Sys_error "write returned zero")
    | written -> write_all fd s (first + written)
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> write_all fd s first

(* After an [EINTR] the descriptor is closed on every supported platform, and
   a retry could close one reused meanwhile. *)
let close_fd fd =
  try Unix.close fd with Unix.Unix_error (Unix.EINTR, _, _) -> ()

(* A rename over a symbolic link would replace the link, and its target would
   keep the old bytes. *)
let atomic_write ?(perm = 0o666) ~path contents =
  if perm land lnot 0o777 <> 0 then
    invalid_arg "Os.atomic_write: perm must contain only bits within 0o777";
  (match Unix.lstat path with
  | { Unix.st_kind = Unix.S_LNK; _ } ->
      raise
        (Sys_error
           (path
          ^ ": is a symbolic link; atomic replacement would substitute a \
             regular file for the link, so it is refused"))
  | _ | (exception Unix.Unix_error _) -> ());
  let temp, fd =
    step path "cannot create temporary file" (fun () ->
        create_temp (Filename.dirname path) perm)
  in
  on_failure ~undo:(fun () -> Unix.unlink temp) @@ fun () ->
  on_failure
    ~undo:(fun () -> close_fd fd)
    (fun () -> step path "cannot write" (fun () -> write_all fd contents 0));
  step path "cannot close" (fun () -> close_fd fd);
  step path "cannot replace" (fun () -> Unix.rename temp path)

(* Project root and log root *)

let normalize_sep path = String.map (function '\\' -> '/' | c -> c) path

(* The components of [path] before its first build directory, that
   directory, and the components after it. *)
let split_at_build_dir path =
  let rec split before = function
    | [] -> None
    | c :: after when String.starts_with ~prefix:"_build" c ->
        Some (List.rev before, c, after)
    | c :: after -> split (c :: before) after
  in
  split [] (String.split_on_char '/' (normalize_sep path))

let build_dir_of_path path =
  Option.map
    (fun (before, build, _) -> String.concat "/" (before @ [ build ]))
    (split_at_build_dir path)

let absolute path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

(* Dune exports its build context as [INSIDE_DUNE], which a sandboxed action
   keeps and which names a private [--build-dir]. A value that is no path (a
   harness's [INSIDE_DUNE=1]) is relative, so it names a build directory only
   when the working directory lies under one. *)
let build_dir () =
  List.find_map
    (fun path -> build_dir_of_path (absolute path))
    (Option.to_list (getenv "INSIDE_DUNE")
    @ [ Filename.dirname Sys.executable_name ])

let is_absolute p =
  String.starts_with ~prefix:"/" p
  || String.length p >= 3
     && p.[1] = ':'
     && p.[2] = '/'
     && match p.[0] with 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false

(* The anchor of an absolute path ([""] for [/], [C:] for a drive) and its
   components, with [.], [..] and empty ones resolved. [None] when [p] is
   relative or climbs above its anchor. *)
let normalized p =
  let rec resolve above = function
    | [] -> Some (List.rev above)
    | ("" | ".") :: rest -> resolve above rest
    | ".." :: rest -> (
        match above with [] -> None | _ :: above -> resolve above rest)
    | c :: rest -> resolve (c :: above) rest
  in
  match String.split_on_char '/' p with
  | anchor :: rest when is_absolute p ->
      Option.map (fun comps -> (anchor, comps)) (resolve [] rest)
  | _ -> None

let join (anchor, comps) = anchor ^ "/" ^ String.concat "/" comps

(* The root is normalized, since a display compares it as bytes: a [.] or a
   trailing [/] would make it prefix nothing. *)
let project_root () =
  match getenv "WINDTRAP_PROJECT_ROOT" with
  | Some root -> (
      let root = normalize_sep (absolute root) in
      match normalized root with Some path -> join path | None -> root)
  | None -> (
      match build_dir () with
      | Some dir -> Filename.dirname dir
      | None -> normalize_sep (Sys.getcwd ()))

(* The logs follow the build directory, so a private [--build-dir] keeps its
   own and a tree built without dune never grows a [_build]. *)
let default_log_dir () =
  match build_dir () with
  | Some dir -> Filename.concat dir "_tests"
  | None -> Filename.concat (Filename.get_temp_dir_name ()) "windtrap"

(* Source tree and build tree *)

(* The components of [path] around its build context: a sandboxed action runs
   under [_build/.sandbox/<hash>/<context>/], any other under
   [_build/<context>/]. *)
let build_context path =
  match split_at_build_dir path with
  | Some (before, build, ".sandbox" :: hash :: context :: after) ->
      Some (before, [ build; ".sandbox"; hash; context ], after)
  | Some (before, build, context :: after) ->
      Some (before, [ build; context ], after)
  | Some (_, _, []) | None -> None

let strip_build_prefix path =
  match build_context path with
  | Some (before, _, after) -> String.concat "/" (before @ after)
  | None -> normalize_sep path

let trim_trailing_slashes s =
  let rec stop i = if i > 0 && s.[i - 1] = '/' then stop (i - 1) else i in
  match stop (String.length s) with 0 -> s | n -> String.sub s 0 n

let rec is_strictly_under ~dirs comps =
  match (dirs, comps) with
  | [], _ :: _ -> true
  | dir :: dirs, comp :: comps ->
      String.equal dir comp && is_strictly_under ~dirs comps
  | _, [] -> false

let reconstruct ~root file =
  let root = trim_trailing_slashes (normalize_sep root) in
  let file = strip_build_prefix file in
  let candidate = if is_absolute file then file else root ^ "/" ^ file in
  match (normalized root, normalized candidate) with
  | Some (root_anchor, dirs), Some ((anchor, comps) as path)
    when String.equal root_anchor anchor && is_strictly_under ~dirs comps ->
      Ok (join path)
  | _ -> Error candidate

let build_root dir =
  Option.map
    (fun (before, context, _) -> String.concat "/" (before @ context))
    (build_context dir)

(* Display paths *)

let chop_prefix ~prefix s =
  if String.starts_with ~prefix s then
    let first = String.length prefix in
    Some (String.sub s first (String.length s - first))
  else None

(* [None] when the root cannot be read: a test can remove its own working
   directory, and these paths are printed after the run. The prefix is
   matched with backslashes read as separators, and the rest keeps the
   path's bytes: [normalize_sep] keeps every offset. *)
let chop_root path =
  match project_root () with
  | exception Sys_error _ -> None
  | root ->
      let prefix = root ^ "/" in
      if String.starts_with ~prefix (normalize_sep path) then
        let first = String.length prefix in
        Some (String.sub path first (String.length path - first))
      else None

(* The root goes first: a root inside a build tree (a scratch root under a
   sandbox) would not prefix the stripped path. Dune runs a test as
   [./t.exe], and dropping [.] segments spells its paths as every other
   producer does. *)
let display_path path =
  let clean path =
    let keep = function "" | "." -> false | _ -> true in
    match String.split_on_char '/' path with
    | "" :: rest -> "/" ^ String.concat "/" (List.filter keep rest)
    | segments -> (
        match List.filter keep segments with
        | [] -> "."
        | kept -> String.concat "/" kept)
  in
  match chop_root path with
  | Some relative -> clean (strip_build_prefix relative)
  | None ->
      let path = clean (strip_build_prefix path) in
      Option.value (chop_root path) ~default:path

let display_artifact path = Option.value (chop_root path) ~default:path

(* Path components *)

(* The digest keeps [parse: empty] and [parse, empty] apart, since two tests
   with one component would share a capture log, which [Capture] truncates on
   open. The bound of 80 bytes stays within filesystem limits. *)
let sanitize_component s =
  let safe = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> true
    | _ -> false
  in
  let mapped = String.map (fun c -> if safe c then c else '_') s in
  let digest = Digest.to_hex (Digest.string s) in
  let short = String.sub digest 0 8 in
  let component =
    match mapped with
    | "" | "." | ".." -> "unnamed-" ^ short
    | _ when String.equal mapped s -> mapped
    | _ -> mapped ^ "-" ^ short
  in
  if String.length component <= 80 then component
  else String.sub component 0 40 ^ "_" ^ digest

(* Filesystem helpers *)

let file_exists = Sys.file_exists

let rec mkdir_p path =
  match path with
  | "" | "." -> ()
  | path when file_exists path -> ()
  | path -> (
      let parent = Filename.dirname path in
      if parent <> path then mkdir_p parent;
      try Unix.mkdir path 0o770 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let failure_reason ~path = function
  | Sys_error message ->
      Option.value (chop_prefix ~prefix:(path ^ ": ") message) ~default:message
  | Unix.Unix_error (error, _, dir) ->
      strf "cannot create directory %s: %s" (display_path dir)
        (Unix.error_message error)
  | exn -> Printexc.to_string exn

(* Standard error *)

(* [Text.escape_controls] escapes a line feed too, so it sees each line
   alone. *)
let say message =
  (try
     Format.pp_print_flush Format.std_formatter ();
     flush stdout
   with Sys_error _ -> ());
  Format.pp_print_flush Format.err_formatter ();
  let lines =
    List.map Text.escape_controls (String.split_on_char '\n' message)
  in
  prerr_string ("windtrap: " ^ String.concat "\n" lines ^ "\n");
  flush stderr

let warn message = say ("warning: " ^ message)

(* Signals *)

(* The runtime blocks a signal while its handler runs: unblocked, one that the
   handler sends is delivered at once. *)
let default_signals signals =
  List.iter (fun signal -> Sys.set_signal signal Sys.Signal_default) signals;
  ignore (Unix.sigprocmask Unix.SIG_UNBLOCK signals)

let with_signals signals handle fn =
  if Sys.win32 then fn ()
  else begin
    let owner = Unix.getpid () in
    let handle signal =
      if Unix.getpid () <> owner then begin
        default_signals signals;
        Unix.kill (Unix.getpid ()) signal
      end
      else begin
        default_signals (List.filter (fun s -> s <> Sys.sigpipe) signals);
        handle signal
      end
    in
    let previous =
      List.map
        (fun signal -> (signal, Sys.signal signal (Sys.Signal_handle handle)))
        signals
    in
    List.iter
      (fun (signal, behavior) ->
        match behavior with
        | Sys.Signal_ignore -> Sys.set_signal signal behavior
        | Sys.Signal_default | Sys.Signal_handle _ -> ())
      previous;
    Fun.protect
      ~finally:(fun () ->
        List.iter
          (fun (signal, behavior) -> Sys.set_signal signal behavior)
          previous)
      fn
  end

let die_by signal =
  default_signals [ signal ];
  Unix.kill (Unix.getpid ()) signal;
  Unix._exit
    ((128
     +
     if signal = Sys.sighup then 1
     else if signal = Sys.sigint then 2
     else if signal = Sys.sigpipe then 13
     else 15)
     [@mutate off "reached only when the signal does not end the process"])
