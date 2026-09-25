(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Formats *)

type format = {
  magic : string;
  kind : string;
  dir : string;
  ext : string;
  remedy : string;
  who : string;
}

(* Identities *)

type identity = { exe : string; digest : string }

let is_hex = function '0' .. '9' | 'a' .. 'f' -> true | _ -> false

let validate_identity ~who { exe; digest } =
  if exe = "" then invalid_arg (who ^ ": empty identity exe");
  if String.length digest <> 32 || not (String.for_all is_hex digest) then
    invalid_arg (who ^ ": identity digest is not 32 hex characters")

let file_digest path =
  match Digest.to_hex (Digest.file path) with
  | digest -> Some digest
  | exception (Sys_error _ | End_of_file) -> None

(* Build Paths *)

let hex_hash s = Digest.to_hex (Digest.string s)

let absolute path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

(* One executable reached two ways is one executable, and the key below
   is what decides how many data files it gets. Dune spells the same
   binary [test/a.exe] here and [./test/a.exe] there, and a suite that
   spawns a sibling names it [../../bin/main.exe]; without this, each
   spelling files its own dump, every rebuild leaves all but the last
   one behind, and the report calls them stale for the rest of the
   build directory's life.

   Lexical, like [Os.display_path]: these are paths under [_build],
   which dune builds out of plain directories, so no [..] can mean
   something a symlink redefined. The first component is the root ("" for
   "/x", "C:" for "C:/x") and is never touched. *)
let canonical path =
  let path = String.map (function '\\' -> '/' | c -> c) (absolute path) in
  match String.split_on_char '/' path with
  | [] -> path
  | first :: rest ->
      let step above = function
        | "" | "." -> above
        | ".." -> ( match above with [] -> [] | _ :: outer -> outer)
        | c -> c :: above
      in
      String.concat "/" (first :: List.rev (List.fold_left step [] rest))

(* The one build-directory rule, the core's ([Os.build_dir_of_path])
   restated here because this library links no core: a component whose
   name starts with [_build] - dune's default and any private
   [--build-dir] alike. [Some (build_dir, below)] when [path] has one -
   [build_dir] the path cut after the first such component, [below] the
   path under it with any [.sandbox/<digest>] prefix stripped, so
   sandboxed and direct runs agree. *)
let is_build_component c = String.starts_with ~prefix:"_build" c

let split_build path =
  let components = String.split_on_char '/' (canonical path) in
  let rec split_at_build before = function
    | [] -> None
    | c :: below when is_build_component c ->
        Some (List.rev (c :: before), below)
    | c :: rest -> split_at_build (c :: before) rest
  in
  match split_at_build [] components with
  | None -> None
  | Some (build_dir, below) ->
      let below =
        match below with
        | ".sandbox" :: _digest :: rest -> rest
        | below -> below
      in
      Some (String.concat "/" build_dir, String.concat "/" below)

let build_dir ~path = Option.map fst (split_build path)
let build_root ~path = Option.map Filename.dirname (build_dir ~path)

(* An executable lies in a build directory when one of the directories
   above it is one; its own file name never is, so [below] always ends
   with that name and no identity is empty. *)
let split_exe exe =
  let path = canonical exe in
  let name = Filename.basename path in
  Option.map
    (fun (build_dir, below) ->
      (build_dir, if below = "" then name else below ^ "/" ^ name))
    (split_build (Filename.dirname path))

let exe_identity ~exe =
  match split_exe exe with Some (_, below) -> below | None -> canonical exe

(* Where a format's files live: beside the build directory's contexts,
   marked as not a context by the underscore, or - for a tree with no
   build directory, which must never grow one - under the project's own
   [_windtrap]. *)
let data_dir format ~build_dir = Printf.sprintf "%s/_%s" build_dir format.dir

let standalone_data_dir format ~root =
  Printf.sprintf "%s/_windtrap/%s" root format.dir

let output_stem format ~exe =
  let dir, key =
    match split_exe exe with
    | Some (build_dir, below) -> (data_dir format ~build_dir, below)
    | None -> (standalone_data_dir format ~root:(Sys.getcwd ()), canonical exe)
  in
  Printf.sprintf "%s/windtrap-%s" dir (hex_hash key)

let output_file format ~exe = output_stem format ~exe ^ "." ^ format.ext
let output_dir format ~exe = output_stem format ~exe

(* Errors *)

type error =
  | Unknown_format of { path : string; header : string }
  | Unreadable of { path : string; reason : string }
  | Corrupt of { path : string; reason : string }

let pp_error format ppf = function
  | Unknown_format { path; header } ->
      Format.fprintf ppf
        "%s: not a windtrap %s file (expected header %S, found \"%s\"); files \
         written by other windtrap versions are not readable - %s"
        path format.kind format.magic header format.remedy
  | Unreadable { path; reason } ->
      Format.fprintf ppf "%s: cannot read %s file: %s" path format.kind reason
  | Corrupt { path; reason } ->
      Format.fprintf ppf "%s: corrupt %s file: %s" path format.kind reason

(* The runtime links no core, so its warnings skip the report's escape of
   control bytes; what they print is identifiers and build paths. *)
let warn fmt =
  Printf.ksprintf (fun m -> Printf.eprintf "windtrap: warning: %s\n%!" m) fmt

(* Reading and Writing *)

let read_file path =
  match
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> really_input_string ic (in_channel_length ic))
  with
  | contents -> Ok contents
  | exception Sys_error reason -> Error (Unreadable { path; reason })
  | exception End_of_file ->
      Error (Corrupt { path; reason = "file changed while reading" })

let rec mkdir_p dir =
  if dir = "" || dir = "." || dir = "/" || Sys.file_exists dir then ()
  else begin
    mkdir_p (Filename.dirname dir);
    try Sys.mkdir dir 0o755 with Sys_error _ -> ()
  end

let temp_state = lazy (Random.State.make_self_init ())
let random_token () = Random.State.int (Lazy.force temp_state) 0x1000000

(* Exclusive creation of [name token], retried under a fresh token on
   collision: two processes writing concurrently can never interleave
   into a shared temp file - the loser of the last atomic rename simply
   overwrites, which is fine. A leftover [.tmp] from a crashed run is
   skipped, not reused. [tries] bounds the retries; the last failure
   propagates. *)
let create_exclusive name =
  let rec attempt tries =
    match
      let path = name (random_token ()) in
      ( path,
        open_out_gen
          [ Open_wronly; Open_creat; Open_excl; Open_binary ]
          0o644 path )
    with
    | reserved -> reserved
    | exception Sys_error _ when tries > 1 -> attempt (tries - 1)
  in
  attempt 10

(* [data] into the open [temp], then the atomic rename over [path]; the
   temp file never outlives a failure. *)
let commit_temp ~temp oc ~path data =
  (try
     Fun.protect
       ~finally:(fun () -> close_out_noerr oc)
       (fun () -> output_string oc data)
   with e ->
     (try Sys.remove temp with Sys_error _ -> ());
     raise e);
  try Sys.rename temp path
  with e ->
    (try Sys.remove temp with Sys_error _ -> ());
    raise e

let write_file path data =
  mkdir_p (Filename.dirname path);
  let temp, oc =
    create_exclusive (fun token -> Printf.sprintf "%s.%06x.tmp" path token)
  in
  commit_temp ~temp oc ~path data

(* A fresh [<prefix><token>.<ext>] in [dir]. The token is reserved by
   the exclusive creation of [<prefix><token>.tmp]: a concurrent writer
   holding the same token either still has its temp file (this creation
   fails and retries) or has already renamed it into place (the
   existence check fails and retries) - at every instant one of the two
   exists, so two writers never share a final name. *)
let write_new_file dir ~prefix ~ext data =
  mkdir_p dir;
  let temp, oc =
    create_exclusive (fun token ->
        let stem = Filename.concat dir (Printf.sprintf "%s%06x" prefix token) in
        if Sys.file_exists (stem ^ "." ^ ext) then
          raise (Sys_error (stem ^ ": name taken"))
        else stem ^ ".tmp")
  in
  let path = Filename.chop_suffix temp ".tmp" ^ "." ^ ext in
  commit_temp ~temp oc ~path data;
  path

(* The Header *)

let add_header format buffer identity =
  Buffer.add_string buffer format.magic;
  Buffer.add_char buffer '\n';
  match identity with
  | None -> ()
  | Some ({ exe; digest } as identity) ->
      validate_identity ~who:format.who identity;
      Printf.bprintf buffer "exe %s %d %s\n" digest (String.length exe) exe

(* Parser Scaffolding *)

type cursor = { input : string; len : int; mutable pos : int }

exception Parse_error of string

let parse_fail fmt = Printf.ksprintf (fun m -> raise (Parse_error m)) fmt

let first_line s =
  let line =
    match String.index_opt s '\n' with Some i -> String.sub s 0 i | None -> s
  in
  let line = if String.length line > 64 then String.sub line 0 64 else line in
  String.escaped line

let is_ws = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false

let start format ~path s =
  let len = String.length s in
  let has_magic =
    String.starts_with ~prefix:format.magic s
    && (len = String.length format.magic || is_ws s.[String.length format.magic])
  in
  if not has_magic then Error (Unknown_format { path; header = first_line s })
  else Ok { input = s; len; pos = String.length format.magic }

let skip_ws c =
  while c.pos < c.len && is_ws c.input.[c.pos] do
    c.pos <- c.pos + 1
  done

let read_int c what =
  skip_ws c;
  let start = c.pos in
  if c.pos < c.len && c.input.[c.pos] = '-' then c.pos <- c.pos + 1;
  while c.pos < c.len && is_digit c.input.[c.pos] do
    c.pos <- c.pos + 1
  done;
  if c.pos = start then parse_fail "expected %s at offset %d" what start;
  match int_of_string (String.sub c.input start (c.pos - start)) with
  | n -> n
  | exception Failure _ -> parse_fail "invalid %s at offset %d" what start

let read_nat c what =
  let n = read_int c what in
  if n < 0 then parse_fail "negative %s" what;
  n

(* A count of items that each take at least one byte cannot exceed the
   length of the input, so a larger one is corrupt. The bound is what keeps
   a corrupt count from sizing an [Array.make] in the parser of a format.
   The whole length is looser than what remains after the cursor, and it
   protects the allocation as well. *)
let read_count c what =
  let n = read_nat c what in
  if n > c.len then parse_fail "%s exceeds data" what;
  n

let read_name c what =
  let n = read_nat c (what ^ " length") in
  if c.pos >= c.len || c.input.[c.pos] <> ' ' then
    parse_fail "expected space before %s at offset %d" what c.pos;
  c.pos <- c.pos + 1;
  if n > c.len - c.pos then parse_fail "truncated %s" what;
  let name = String.sub c.input c.pos n in
  c.pos <- c.pos + n;
  name

let read_word c what =
  skip_ws c;
  let start = c.pos in
  while c.pos < c.len && not (is_ws c.input.[c.pos]) do
    c.pos <- c.pos + 1
  done;
  if c.pos = start then parse_fail "expected %s at offset %d" what start;
  String.sub c.input start (c.pos - start)

let read_identity c =
  skip_ws c;
  if
    c.pos + 3 <= c.len
    && c.input.[c.pos] = 'e'
    && c.input.[c.pos + 1] = 'x'
    && c.input.[c.pos + 2] = 'e'
  then begin
    c.pos <- c.pos + 3;
    skip_ws c;
    let start = c.pos in
    while c.pos < c.len && is_hex c.input.[c.pos] do
      c.pos <- c.pos + 1
    done;
    let digest = String.sub c.input start (c.pos - start) in
    if String.length digest <> 32 then
      parse_fail "identity digest is not 32 hex characters at offset %d" start;
    let exe = read_name c "executable identity" in
    if exe = "" then parse_fail "empty executable identity";
    Some { exe; digest }
  end
  else None

let finish c =
  skip_ws c;
  if c.pos <> c.len then parse_fail "trailing data at offset %d" c.pos
