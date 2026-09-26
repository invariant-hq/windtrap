(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type mode = Check | Corrected | Update

type subject =
  | Literal of { pos : Loc.pos; value : string; exact : bool }
  | File of string

type key = Site of Loc.pos | Path of string

let key_of = function Literal { pos; _ } -> Site pos | File path -> Path path

let subject_file = function
  | Literal { pos = file, _, _, _; _ } -> file
  | File path -> path

(* A subject's file: [source] is under the project root, and [read] is the
   file a check reads, dune's copy inside a build action and [source]
   otherwise. *)
type where = { read : string; source : string }

type entry = {
  baseline : string option; (* in comparison form, read at the first check *)
  mutable accepted : string option; (* the content a correction recorded *)
}

(* A correction produces the file [output]. A literal's patch applies to the
   bytes of [input]. *)
type correction =
  | Patch of { input : string; output : string; patch : Source_patch.patch }
  | Content of { output : string; text : string }

type write =
  | Written of { path : string; literals : int }
  | Refused of { path : string; reason : string }

(* A check records its correction in [pending]; [settle] moves the attempt's
   corrections to [kept] or drops them, and [write] empties [kept]. *)
type t = {
  mode : mode;
  root : string;
  build_root : string option;
  entries : (key, entry) Hashtbl.t;
  mutable pending : (entry * correction) list; (* newest first *)
  mutable kept : correction list; (* newest first *)
  sources : (string, (string, string) result) Hashtbl.t;
      (* a source file's bytes as a check first read them, or why it could
         not *)
  mutable writes : write list;
}

let absolute path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

let create ?root ?cwd ~mode () =
  let root =
    match root with None -> Os.project_root () | Some r -> absolute r
  in
  let cwd = match cwd with None -> Sys.getcwd () | Some d -> absolute d in
  (* A build context of another workspace (a project root aimed elsewhere
     by WINDTRAP_PROJECT_ROOT) holds no copy of this root's files. *)
  let build_root =
    match Os.build_root cwd with
    | Some b when String.starts_with ~prefix:(root ^ "/") b -> Some b
    | Some _ | None -> None
  in
  {
    mode;
    root;
    build_root;
    entries = Hashtbl.create 16;
    pending = [];
    kept = [];
    sources = Hashtbl.create 4;
    writes = [];
  }

let mode t = t.mode

(* Checking *)

let resolve t subject =
  (* Dune's copy of a file has the file's path relative to the root. *)
  let copy source =
    match t.build_root with
    | None -> source
    | Some build ->
        let skip = String.length t.root + 1 in
        build ^ "/" ^ String.sub source skip (String.length source - skip)
  in
  Result.map
    (fun source -> { read = copy source; source })
    (Os.reconstruct ~root:t.root (subject_file subject))

let form subject text =
  match subject with
  | Literal { exact = true; _ } -> text
  | Literal { exact = false; _ } -> Source_patch.normalize text
  | File _ -> Text.ensure_trailing_newline (Text.normalize_newlines text)

let read_file path = In_channel.with_open_bin path In_channel.input_all

let read_baseline subject where =
  match subject with
  | Literal { value; _ } -> Some (form subject value)
  | File _ ->
      if Os.file_exists where.read then
        Some (form subject (read_file where.read))
      else None

(* The reason names no path: the message that carries it names the file. *)
let read_source path =
  match read_file path with
  | text -> Ok text
  | exception (Sys_error _ as e) ->
      Error ("the source file cannot be read: " ^ Os.failure_reason ~path e)

(* The bytes a correction applies to and the file it produces. Dune's [diff?]
   finds a [.corrected] file beside the copy that the action read. *)
let destination t where =
  match t.mode with
  | Corrected -> (where.read, where.read ^ ".corrected")
  | Update | Check -> (where.source, where.source)

let cached table key make =
  match Hashtbl.find_opt table key with
  | Some value -> value
  | None ->
      let value = make () in
      Hashtbl.add table key value;
      value

(* A literal's patch is tried alone on the bytes that [write] will patch:
   [Source_patch.apply] locates every patch of a file against the same
   original bytes, so one valid alone is valid with the others. *)
let correction t subject where ~actual ~accepted =
  let input, output = destination t where in
  match subject with
  | File _ -> Ok (Content { output; text = accepted })
  | Literal { pos = (_, line, _, _) as pos; value; exact } -> (
      let style = if exact then Source_patch.Exact else Source_patch.Flexible in
      let patch = Source_patch.patch ~site:pos ~literal:value ~style actual in
      let source = cached t.sources input (fun () -> read_source input) in
      let apply text =
        Result.map_error Source_patch.error_message
          (Source_patch.apply text [ patch ])
      in
      match Result.bind source apply with
      | Ok _ -> Ok (Patch { input; output; patch })
      | Error reason -> Error (Failure.Refused { line; reason }))

let check t ?loc ?(correct = true) subject actual =
  let fail ?withheld state =
    let baseline =
      match subject with
      | Literal { exact; _ } -> Failure.Literal { exact }
      | File path -> Failure.File path
    in
    let failure = Failure.baseline ?loc baseline state in
    raise
      (Failure.Check_failure
         (match withheld with
         | None -> failure
         | Some why -> Failure.with_withheld why failure))
  in
  match resolve t subject with
  | Error candidate -> fail (Failure.Unresolvable { candidate })
  | Ok where -> (
      let entry =
        cached t.entries (key_of subject) (fun () ->
            { baseline = read_baseline subject where; accepted = None })
      in
      let actual' = form subject actual in
      let mismatch expected =
        Failure.Mismatch
          { expected = Failure.text expected; actual = Failure.text actual' }
      in
      match (entry.accepted, entry.baseline) with
      | Some accepted, _ ->
          (* An accepted key records no second correction, so [write] has
             one content per key. *)
          if not (String.equal actual' accepted) then
            fail ~withheld:Failure.Conflict (mismatch accepted)
      | None, Some expected when String.equal expected actual' -> ()
      | None, baseline -> (
          let state =
            match baseline with
            | Some expected -> mismatch expected
            | None -> Failure.Missing { proposed = Failure.text actual' }
          in
          match if correct then t.mode else Check with
          | Check -> fail state
          | (Corrected | Update) as mode -> (
              match correction t subject where ~actual ~accepted:actual' with
              | Error refused -> fail ~withheld:refused state
              | Ok correction ->
                  entry.accepted <- Some actual';
                  t.pending <- (entry, correction) :: t.pending;
                  if mode = Corrected then fail state)))

let settle t ~keep =
  let pending = t.pending in
  t.pending <- [];
  if keep then begin
    t.kept <- List.map snd pending @ t.kept;
    List.length pending
  end
  else begin
    (* A dropped attempt (a retry) leaves no accepted content behind for
       the next attempt to agree with. *)
    List.iter (fun (entry, _) -> entry.accepted <- None) pending;
    0
  end

(* Writing *)

module String_map = Map.Make (String)

(* A file's every failure is its refusal, so the files after it are still
   written. *)
let publish t output ~literals contents =
  let outcome =
    match contents with
    | Error reason -> Refused { path = output; reason }
    | Ok text -> (
        try
          Os.mkdir_p (Filename.dirname output);
          Os.atomic_write ~path:output text;
          Written { path = output; literals }
        with (Sys_error _ | Unix.Unix_error _) as e ->
          Refused { path = output; reason = Os.failure_reason ~path:output e })
  in
  t.writes <- outcome :: t.writes

let write t =
  let kept = List.rev t.kept in
  t.kept <- [];
  let group (patched, contents) = function
    | Patch { input; output; patch } ->
        let earlier =
          match String_map.find_opt output patched with
          | Some (_, patches) -> patches
          | None -> []
        in
        (String_map.add output (input, patch :: earlier) patched, contents)
    | Content { output; text } -> (patched, String_map.add output text contents)
  in
  let patched, contents =
    List.fold_left group (String_map.empty, String_map.empty) kept
  in
  (* Every patch was valid alone on the bytes a check read: a refusal now is
     an edit since. *)
  let apply input patches =
    let changed error =
      "it changed during the run: " ^ Source_patch.error_message error
    in
    Result.bind (read_source input) (fun text ->
        Result.map_error changed (Source_patch.apply text patches))
  in
  String_map.iter
    (fun output (input, patches) ->
      publish t output ~literals:(List.length patches) (apply input patches))
    patched;
  String_map.iter
    (fun output text -> publish t output ~literals:0 (Ok text))
    contents

let path = function Written { path; _ } | Refused { path; _ } -> path
let writes t = List.sort (fun a b -> String.compare (path a) (path b)) t.writes
