(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf

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
let is_digest s = String.length s = 32 && String.for_all is_hex s

let file_digest path =
  match Digest.to_hex (Digest.file path) with
  | digest -> Some digest
  | exception (Sys_error _ | End_of_file) -> None

(* Build paths *)

let absolute path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let join = String.concat "/"

(* The root of [path] made absolute ("" for "/x", "C:" for "C:/x"), then its
   names with "." and empty ones dropped and ".." resolved. One executable
   spelled two ways is then one identity; otherwise each spelling files its
   own dump, and all but the last go stale. Lexical resolution is sound under
   [_build], which dune builds from plain directories. *)
let components path =
  let step above = function
    | "" | "." -> above
    | ".." -> ( match above with [] -> [] | _ :: outer -> outer)
    | name -> name :: above
  in
  let path = String.map (function '\\' -> '/' | c -> c) (absolute path) in
  match String.split_on_char '/' path with
  | root :: names -> root :: List.rev (List.fold_left step [] names)
  | [] -> assert false

(* The rule of [Os.build_dir_of_path], restated because the runtime links no
   core: the build directory and the components below it. *)
let split_build components =
  let rec split above = function
    | [] -> None
    | name :: below when String.starts_with ~prefix:"_build" name ->
        Some (join (List.rev (name :: above)), below)
    | name :: below -> split (name :: above) below
  in
  split [] components

let build_dir ~path = Option.map fst (split_build (components path))
let build_root ~path = Option.map Filename.dirname (build_dir ~path)

(* The build directory [exe] lies in, if any, and its identity. Only a
   directory above [exe] can be its build directory, and the
   [.sandbox/<digest>] right below that directory is no part of the
   identity. *)
let locate exe =
  let components = components exe in
  match split_build components with
  | Some (build_dir, ".sandbox" :: _ :: (_ :: _ as below))
  | Some (build_dir, (_ :: _ as below)) ->
      (Some build_dir, join below)
  | Some (_, []) | None -> (None, join components)

let exe_identity ~exe = snd (locate exe)

(* The files of a format sit beside the contexts of a build directory, the
   underscore marking them as no context. A tree without one must never grow
   one, so its files go under the project's own [_windtrap]. *)
let data_dir format ~build_dir = strf "%s/_%s" build_dir format.dir
let standalone_data_dir format ~root = strf "%s/_windtrap/%s" root format.dir

let output_dir format ~exe =
  let build_dir, identity = locate exe in
  let dir =
    match build_dir with
    | Some build_dir -> data_dir format ~build_dir
    | None -> standalone_data_dir format ~root:(Sys.getcwd ())
  in
  strf "%s/windtrap-%s" dir (Digest.to_hex (Digest.string identity))

let output_file format ~exe = output_dir format ~exe ^ "." ^ format.ext

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

(* Reading and writing *)

let read_file path =
  match
    In_channel.with_open_bin path (fun ic ->
        In_channel.really_input_string ic (in_channel_length ic))
  with
  | Some contents -> Ok contents
  | None -> Error (Corrupt { path; reason = "file changed while reading" })
  | exception Sys_error reason -> Error (Unreadable { path; reason })

(* A directory that a concurrent writer makes first is no failure, and one
   that cannot be made fails the creation of the file in it. *)
let rec mkdir_p dir =
  if not (dir = "" || dir = "." || dir = "/" || Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    try Sys.mkdir dir 0o755 with Sys_error _ -> ()
  end

let tokens = lazy (Random.State.make_self_init ())

(* [atomic_write names data] writes [data] to the temporary file of
   [names token], then renames it over the destination of that pair, which it
   returns. The temporary is created exclusively under a fresh token, ten
   times at most, so concurrent writers never interleave in one and a
   leftover of a crashed run is skipped; [names] refuses a token by raising
   [Sys_error]. The temporary never outlives a failure. *)
let atomic_write names data =
  let rec reserve tries =
    match
      let temp, path = names (Random.State.int (Lazy.force tokens) 0x1000000) in
      let flags = [ Open_wronly; Open_creat; Open_excl; Open_binary ] in
      (temp, path, open_out_gen flags 0o644 temp)
    with
    | reserved -> reserved
    | exception Sys_error _ when tries > 1 -> reserve (tries - 1)
  in
  let temp, path, oc = reserve 10 in
  match
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () -> output_string oc data);
    Sys.rename temp path
  with
  | () -> path
  | exception e ->
      (try Sys.remove temp with Sys_error _ -> ());
      raise e

let write_file path data =
  mkdir_p (Filename.dirname path);
  ignore
    (atomic_write (fun token -> (strf "%s.%06x.tmp" path token, path)) data)

(* A writer holding the same token either still has its temporary, and the
   exclusive creation fails, or has renamed it into place, and the existence
   check fails. One of the two exists at every instant, so two writers never
   share a destination. *)
let write_new_file dir ~prefix ~ext data =
  mkdir_p dir;
  atomic_write
    (fun token ->
      let stem = Filename.concat dir (strf "%s%06x" prefix token) in
      if Sys.file_exists (stem ^ "." ^ ext) then
        raise (Sys_error (stem ^ ": name taken"));
      (stem ^ ".tmp", stem ^ "." ^ ext))
    data

(* The header *)

let add_header format buffer identity =
  Buffer.add_string buffer format.magic;
  Buffer.add_char buffer '\n';
  match identity with
  | None -> ()
  | Some { exe; digest } ->
      if exe = "" then invalid_arg (format.who ^ ": empty identity exe");
      if not (is_digest digest) then
        invalid_arg (format.who ^ ": identity digest is not 32 hex characters");
      Printf.bprintf buffer "exe %s %d %s\n" digest (String.length exe) exe

(* Parsing *)

type cursor = { input : string; len : int; mutable pos : int }

exception Parse_error of string

let parse_fail fmt = Printf.ksprintf (fun m -> raise (Parse_error m)) fmt
let is_ws = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false

let start format ~path s =
  let len = String.length s in
  let magic = String.length format.magic in
  if
    String.starts_with ~prefix:format.magic s && (len = magic || is_ws s.[magic])
  then Ok { input = s; len; pos = magic }
  else
    let line = Option.value ~default:len (String.index_opt s '\n') in
    let header = String.escaped (String.sub s 0 (min line 64)) in
    Error (Unknown_format { path; header })

let skip_while p c =
  while c.pos < c.len && p c.input.[c.pos] do
    c.pos <- c.pos + 1
  done

let skip_ws c = skip_while is_ws c

(* A sign is read so that a negative number is refused as one. *)
let read_nat c what =
  skip_ws c;
  let start = c.pos in
  if c.pos < c.len && c.input.[c.pos] = '-' then c.pos <- c.pos + 1;
  skip_while (function '0' .. '9' -> true | _ -> false) c;
  if c.pos = start then parse_fail "expected %s at offset %d" what start;
  match int_of_string (String.sub c.input start (c.pos - start)) with
  | n when n < 0 -> parse_fail "negative %s" what
  | n -> n
  | exception Failure _ -> parse_fail "invalid %s at offset %d" what start

(* A count of items that each take a byte at least cannot exceed the length
   of the input, and the bound keeps a corrupt count from sizing the
   [Array.make] of a format's parser. The whole length is looser than what
   remains after the cursor, and it protects the allocation as well. *)
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
  skip_while (fun b -> not (is_ws b)) c;
  if c.pos = start then parse_fail "expected %s at offset %d" what start;
  String.sub c.input start (c.pos - start)

let read_identity c =
  skip_ws c;
  if not (c.pos + 3 <= c.len && String.sub c.input c.pos 3 = "exe") then None
  else begin
    c.pos <- c.pos + 3;
    skip_ws c;
    let start = c.pos in
    skip_while is_hex c;
    let digest = String.sub c.input start (c.pos - start) in
    if not (is_digest digest) then
      parse_fail "identity digest is not 32 hex characters at offset %d" start;
    let exe = read_name c "executable identity" in
    if exe = "" then parse_fail "empty executable identity";
    Some { exe; digest }
  end

let finish c =
  skip_ws c;
  if c.pos <> c.len then parse_fail "trailing data at offset %d" c.pos
