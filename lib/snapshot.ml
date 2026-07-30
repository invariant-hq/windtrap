(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Rewritten for v3 from windtrap v1's lib/snapshot.ml, which contributed
   the colocated __snapshots__ layout and the canonicalize-both-sides rule.
   The per-run name registry, read-only checking, one-accepted-content rule
   and orphan logic are new in v3; path reconstruction lives in Path_ops and
   atomic writes in Atomic_file. *)

(* Modes and the CI guard *)

type mode = Check | Update
type mode_resolution = Mode of mode | Refused_in_ci

let resolve_mode ~ci = function
  | Env.No_update -> Mode Check
  | Env.Update -> if ci then Refused_in_ci else Mode Update
  | Env.Force_update -> Mode Update

(* Registries *)

type write_status = Created | Updated

(* One name in one baseline directory. [site] is the first check's site and
   [owner] the test that made it (duplicate detection needs no surviving
   call frame); [baseline] is the canonical content every later check
   of the name compares against — set at the first successful read (Check
   mode) or at acceptance (Update mode), [None] while the baseline is
   missing. *)
type entry = {
  site : Loc.t option;
  owner : string;
  path : string;
  mutable baseline : string option;
}

type t = {
  mode : mode;
  root : string;
  (* Baseline directory -> (lowercase name -> entry). The outer key set is
     the set of consulted directories, which scopes orphan scans. *)
  dirs : (string, (string, entry) Hashtbl.t) Hashtbl.t;
  mutable writes_rev : (string * write_status) list;
}

let create ?root ~mode () =
  let root =
    match root with
    | None -> Path_ops.project_root ()
    | Some r ->
        if Filename.is_relative r then Filename.concat (Sys.getcwd ()) r else r
  in
  { mode; root; dirs = Hashtbl.create 8; writes_rev = [] }

let mode t = t.mode

(* Names *)

let valid_name_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '.' | '_' | '-' -> true
  | _ -> false

let validate_name name =
  if
    name = ""
    || (not (String.for_all valid_name_char name))
    || Atomic_file.is_temp_name name
  then
    invalid_arg
      (Printf.sprintf
         "Windtrap.Snapshot.check: invalid snapshot name %S: names match \
          [A-Za-z0-9._-]+ and may not start with the reserved %S prefix"
         name Atomic_file.temp_prefix)

(* Canonicalization and file reading *)

let canonicalize s = Text.ensure_trailing_newline (Text.normalize_newlines s)

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

(* Scope resolution *)

let chop_extension name =
  try Filename.chop_extension name with Invalid_argument _ -> name

(* [<src_dir>/__snapshots__/<src_basename>/] — the frozen layout. *)
let baseline_dir t file =
  match Path_ops.reconstruct ~root:t.root file with
  | Error _ as e -> e
  | Ok abs ->
      let dir = Filename.dirname abs in
      let base = chop_extension (Filename.basename abs) in
      Ok (dir ^ "/__snapshots__/" ^ base)

(* Checking *)

let fail ?loc ~name ~path state =
  raise (Failure.Check_failure (Failure.snapshot ?loc ~name ~path state))

(* Accept [actual] for [entry]: write it atomically unless an equal
   baseline already exists, and record the write. Checking never reaches
   this function; only update-mode acceptance does. *)
let accept t entry actual =
  let existed = Path_ops.file_exists entry.path in
  let unchanged =
    existed && String.equal (canonicalize (read_file entry.path)) actual
  in
  if not unchanged then begin
    Path_ops.mkdir_p (Filename.dirname entry.path);
    Atomic_file.write ~path:entry.path actual;
    let status = if existed then Updated else Created in
    t.writes_rev <- (entry.path, status) :: t.writes_rev
  end;
  entry.baseline <- Some actual

let check t ?loc ~test ~scope ~name actual =
  validate_name name;
  match scope with
  | None -> fail ?loc ~name ~path:"" Failure.Unresolvable
  | Some file -> (
      match baseline_dir t file with
      | Error candidate -> fail ?loc ~name ~path:candidate Failure.Unresolvable
      | Ok dir -> (
          let table =
            match Hashtbl.find_opt t.dirs dir with
            | Some table -> table
            | None ->
                let table = Hashtbl.create 8 in
                Hashtbl.add t.dirs dir table;
                table
          in
          let actual = canonicalize actual in
          let updating = t.mode = Update in
          match Hashtbl.find_opt table (String.lowercase_ascii name) with
          | Some entry ->
              (* Recheck iff both sites are known and equal (loops, [cases]
                 families, retries), or a site is unknown and the same test
                 is checking; Duplicate otherwise — [owner] catches every
                 cross-test duplicate without a surviving call frame. *)
              let duplicate =
                match (entry.site, loc) with
                | Some first, Some second -> not (Loc.equal first second)
                | _ -> not (String.equal entry.owner test)
              in
              if duplicate then
                fail ?loc ~name ~path:entry.path
                  (Failure.Duplicate
                     { first = entry.site; first_test = entry.owner })
              else begin
                match entry.baseline with
                | Some expected ->
                    if not (String.equal expected actual) then
                      fail ?loc ~name ~path:entry.path
                        (Failure.Mismatch { expected; actual })
                | None ->
                    if updating then accept t entry actual
                    else
                      fail ?loc ~name ~path:entry.path
                        (Failure.Missing { proposed = actual })
              end
          | None ->
              let path = dir ^ "/" ^ name ^ ".snap" in
              let entry = { site = loc; owner = test; path; baseline = None } in
              Hashtbl.add table (String.lowercase_ascii name) entry;
              if updating then accept t entry actual
              else if not (Path_ops.file_exists path) then
                fail ?loc ~name ~path (Failure.Missing { proposed = actual })
              else begin
                let expected = canonicalize (read_file path) in
                entry.baseline <- Some expected;
                if not (String.equal expected actual) then
                  fail ?loc ~name ~path (Failure.Mismatch { expected; actual })
              end))

(* Acceptance report *)

let writes t = List.rev t.writes_rev

(* Orphans and pruning *)

let snap_suffix = ".snap"

let referenced table entry_name =
  let base =
    String.sub entry_name 0
      (String.length entry_name - String.length snap_suffix)
  in
  Hashtbl.mem table (String.lowercase_ascii base)

let orphans t =
  Hashtbl.fold
    (fun dir table acc ->
      let entries = try Sys.readdir dir with Sys_error _ -> [||] in
      Array.fold_left
        (fun acc entry_name ->
          if
            String.ends_with ~suffix:snap_suffix entry_name
            && (not (Atomic_file.is_temp_name entry_name))
            && not (referenced table entry_name)
          then (dir ^ "/" ^ entry_name) :: acc
          else acc)
        acc entries)
    t.dirs []
  |> List.sort String.compare

type prune_refusal = {
  not_update_run : bool;
  filtered : bool;
  skipped : int;
  failed : int;
  focused : int;
}

let prune t ~filtered ~skipped ~failed ~focused =
  if skipped < 0 || failed < 0 || focused < 0 then
    invalid_arg "Windtrap.Snapshot.prune: negative test count";
  let not_update_run = t.mode <> Update in
  if not_update_run || filtered || skipped > 0 || failed > 0 || focused > 0 then
    Error { not_update_run; filtered; skipped; failed; focused }
  else
    Ok
      (List.filter
         (fun path ->
           match Sys.remove path with
           | () -> true
           | exception Sys_error _ -> false)
         (orphans t))
