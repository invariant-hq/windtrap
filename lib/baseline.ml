(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The v3 registry's one-accepted-content rule and atomic
   acceptance, re-keyed by position or path and split into a recording
   half (during the run) and a writing half (after it). *)

type mode = Check | Corrected | Update

type subject =
  | Literal of { pos : Loc.pos; value : string; exact : bool }
  | File of string

type key = Site of Loc.pos | Path of string

let key_of = function Literal { pos; _ } -> Site pos | File path -> Path path

(* Where a subject's file is: [read], the copy the run reads and — in
   Corrected mode — patches, which is dune's build copy inside a build
   action and the source otherwise; [source], the file under the project
   root that Update mode rewrites. *)
type where = { read : string; source : string }

(* One key: the content the run compares against, read once at the first
   check, and the content a correction established, if one did. *)
type entry = {
  where : where;
  baseline : string option;
  mutable accepted : string option;
}

type correction =
  | Patch of where * Source_patch.patch
  | Content of where * string

type written = { path : string; literals : int }

type t = {
  mode : mode;
  root : string;
  build_root : string option;
  entries : (key, entry) Hashtbl.t;
  mutable pending : (key * correction) list; (* this attempt's, newest first *)
  mutable kept : correction list; (* newest first *)
  mutable writes : written list;
  mutable refusals : (string * string) list;
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
    writes = [];
    refusals = [];
  }

let mode t = t.mode

(* Resolution *)

let subject_file = function
  | Literal { pos = file, _, _, _; _ } -> file
  | File path -> path

let resolve t subject =
  match Os.reconstruct ~root:t.root (subject_file subject) with
  | Error _ as e -> e
  | Ok source ->
      let read =
        match t.build_root with
        | None -> source
        | Some build ->
            let prefix = t.root ^ "/" in
            let relative =
              String.sub source (String.length prefix)
                (String.length source - String.length prefix)
            in
            build ^ "/" ^ relative
      in
      Ok { read; source }

(* Comparison forms and file reading *)

let canonicalize s = Text.ensure_trailing_newline (Text.normalize_newlines s)

let form subject actual =
  match subject with
  | Literal { exact = true; _ } -> actual
  | Literal { exact = false; _ } -> Source_patch.normalize actual
  | File _ -> canonicalize actual

let read_file path = In_channel.with_open_bin path In_channel.input_all

let read_baseline subject where =
  match subject with
  | Literal { value; exact; _ } ->
      Some (if exact then value else Source_patch.normalize value)
  | File _ ->
      if Os.file_exists where.read then
        Some (canonicalize (read_file where.read))
      else None

(* Checking *)

let record t key entry subject where actual accepted =
  let correction =
    match subject with
    | Literal { pos; value; exact } ->
        let style =
          if exact then Source_patch.Exact else Source_patch.Flexible
        in
        Patch (where, Source_patch.patch ~site:pos ~literal:value ~style actual)
    | File _ -> Content (where, accepted)
  in
  entry.accepted <- Some accepted;
  t.pending <- (key, correction) :: t.pending

let check t ?loc ?(correct = true) subject actual =
  let mode = if correct then t.mode else Check in
  let kind =
    match subject with
    | Literal _ -> Failure.Literal
    | File path -> Failure.File path
  in
  let fail state =
    raise (Failure.Check_failure (Failure.baseline ?loc kind state))
  in
  match resolve t subject with
  | Error candidate -> fail (Failure.Unresolvable { candidate })
  | Ok where -> (
      let key = key_of subject in
      let entry =
        match Hashtbl.find_opt t.entries key with
        | Some entry -> entry
        | None ->
            let entry =
              { where; baseline = read_baseline subject where; accepted = None }
            in
            Hashtbl.add t.entries key entry;
            entry
      in
      let actual' = form subject actual in
      match entry.accepted with
      | Some accepted ->
          if not (String.equal actual' accepted) then
            fail (Failure.Mismatch { expected = accepted; actual = actual' })
      | None -> (
          match entry.baseline with
          | Some expected when String.equal expected actual' -> ()
          | baseline -> (
              let state =
                match baseline with
                | Some expected ->
                    Failure.Mismatch { expected; actual = actual' }
                | None -> Failure.Missing { proposed = actual' }
              in
              match mode with
              | Check -> fail state
              | Corrected ->
                  record t key entry subject where actual actual';
                  fail state
              | Update -> record t key entry subject where actual actual')))

let settle t ~keep =
  let pending = t.pending in
  t.pending <- [];
  if keep then begin
    t.kept <- List.rev_append (List.rev_map snd pending) t.kept;
    List.length pending
  end
  else begin
    List.iter
      (fun (key, _) ->
        match Hashtbl.find_opt t.entries key with
        | Some entry -> entry.accepted <- None
        | None -> ())
      pending;
    0
  end

(* Writing *)

module String_map = Map.Make (String)

(* The destination of a correction: the bytes it applies to and the file
   it produces. *)
let destination t where =
  match t.mode with
  | Corrected -> (where.read, where.read ^ ".corrected")
  | Update | Check -> (where.source, where.source)

let write t =
  let refuse path reason = t.refusals <- (path, reason) :: t.refusals in
  let publish path contents =
    Os.mkdir_p (Filename.dirname path);
    Os.atomic_write ~path contents
  in
  (* Patches group by the file they rewrite, contents stand alone. *)
  let patches, contents =
    List.fold_left
      (fun (patches, contents) correction ->
        match correction with
        | Patch (where, patch) ->
            let input, output = destination t where in
            let existing =
              Option.value ~default:[] (String_map.find_opt output patches)
            in
            ( String_map.add output ((input, patch) :: existing) patches,
              contents )
        | Content (where, text) ->
            let _, output = destination t where in
            (patches, String_map.add output text contents))
      (String_map.empty, String_map.empty)
      (List.rev t.kept)
  in
  t.kept <- [];
  if t.mode <> Check then begin
    String_map.iter
      (fun output entries ->
        let input = fst (List.hd entries) in
        let patches = List.map snd entries in
        match Source_patch.apply (read_file input) patches with
        | Ok text ->
            publish output text;
            t.writes <-
              { path = output; literals = List.length patches } :: t.writes
        | Error error -> refuse output (Source_patch.error_message error)
        | exception Sys_error reason -> refuse output reason)
      patches;
    String_map.iter
      (fun output text ->
        match publish output text with
        | () -> t.writes <- { path = output; literals = 0 } :: t.writes
        | exception Sys_error reason -> refuse output reason)
      contents
  end

let writes t = List.sort (fun a b -> compare a.path b.path) t.writes
let refusals t = List.sort compare t.refusals
