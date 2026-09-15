(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   File discovery and the staleness pass live in Data_files, shared with
   `windtrap coverage`: one rule for resolving the project root, one rule
   for detecting a file whose executable is gone or was rebuilt. The
   report layout lives in the library renderer (Render.mutation_report,
   via Windtrap.Private), so the loop's in-process report and this merged
   one cannot drift.
  ---------------------------------------------------------------------------*)

module Render = Windtrap.Private.Render
module Env = Windtrap.Private.Env
module Test_tree = Windtrap.Private.Test_tree
module M = Windtrap_runtime.Mutate
module V = Windtrap_runtime.Verdicts

let spf = Printf.sprintf

let usage =
  {|usage: windtrap mutants [PATH...]

Merges the .mutants verdict files written by mutation runs and reports the
mutants that survived every test executable. Without PATH arguments the files
are found under _build/_mutants, walking up from the current directory to the
enclosing project root; PATH arguments (.mutants files, or directories
searched recursively) replace that default.

Runs no tests and drives no build.
Exits 1 when any mutant survived every executable that reached it.

OPTIONS:
  -h, --help  Print this help and exit|}

(* The one remedy, spelled once, in words any build tool's user can act
   on: this command does not know how the suite is run, and a spelled-out
   command would be wrong everywhere but the tree it was written in. A
   verdict exists only where a suite was asked to test its mutants
   (WINDTRAP_MUTATE=1) in a build carrying them, and a run the build tool
   replays from its cache writes nothing. *)
let rerun =
  "re-run every suite with its mutants (WINDTRAP_MUTATE=1, instrumented with \
   ppx_windtrap.mutate, forcing the runs your build tool cached), then merge \
   again"

let remedy =
  rerun ^ "; delete _build/_mutants to drop leftovers of removed executables"

(* Flags *)

let parse_args args =
  let rec go paths = function
    | [] -> Ok (List.rev paths)
    | ("-h" | "--help" | "-help") :: _ -> Error `Help
    | arg :: _ when String.length arg > 0 && arg.[0] = '-' ->
        Error (`Usage (spf "unknown option '%s'" arg))
    | path :: rest -> go (path :: paths) rest
  in
  go [] args

(* Discovery: Data_files's, shared with `windtrap coverage` — the project
   root resolved as the runtime resolves its output path, explicit PATH
   arguments as a loud contract. A silently narrowed merge would be worse
   here than in coverage: under killed-anywhere-wins, dropping the file
   that holds the kill turns the mutant back into a survivor, which is
   the exact failure mode this command exists to prevent. [files] come
   back sorted for deterministic merge order and error attribution;
   [roots] are the source roots the survivor excerpts resolve against. *)

let discover paths = Data_files.discover ~dir:"_mutants" ~ext:"mutants" paths

(* The staleness pass

   Data_files.freshness's, judged from the identity each file records: a
   verdict file whose executable was deleted or renamed (an orphan), and
   one not written by the executable now on disk — a rebuild without the
   backend, or a run the build tool replayed from its cache.

   A flagged verdict file is excluded, never merged: a stale verdict can
   claim a kill the code no longer earns, and a false kill hides a live
   defect. Excluding is the only answer that cannot lie. *)

(* The executable a verdict file speaks for, as the witness column names
   it: the basename of the recorded identity (`test_slug.exe`). Dune's
   inline-test runner is `inline-test-runner.exe` in every library's
   `.<lib>.inline-tests` directory, so it is named by the library whose
   inline tests ran (`<lib>`) instead. A file that records no identity —
   hand-written, or a merge, which has no single writer — is named by its
   own basename. *)
let executable_label ~path identity =
  match (identity : V.identity option) with
  | None -> Filename.basename path
  | Some { exe; _ } ->
      let base = Filename.basename exe in
      let dir = Filename.basename (Filename.dirname exe) in
      let suffix = ".inline-tests" in
      if
        base = "inline-test-runner.exe"
        && String.length dir > String.length suffix + 1
        && dir.[0] = '.'
        && Filename.check_suffix dir suffix
      then String.sub dir 1 (String.length dir - 1 - String.length suffix)
      else base

(* Loads [files], drops the orphaned and stale ones loudly, and returns
   what is left, each collection labelled with its executable. Warnings
   and failure details go to stderr; [Error code] is the exit code (data
   problems are 1). *)
let load_fresh files =
  let loaded =
    List.fold_left
      (fun acc path ->
        Result.bind acc (fun entries ->
            Result.map
              (fun (t, identity) ->
                (path, t, identity, Data_files.freshness ~path identity)
                :: entries)
              (V.load path)))
      (Ok []) files
  in
  match loaded with
  | Error error ->
      Format.eprintf "windtrap mutants: %a@." V.pp_error error;
      Error 1
  | Ok entries ->
      let entries = List.rev entries in
      let kept, excluded =
        List.partition (fun (_, _, _, f) -> f = Data_files.Fresh) entries
      in
      (* One line per excluded file — the path, the executable and the
         reason — then the remedy once, however many there were. *)
      List.iter
        (fun (path, _, _, f) ->
          Printf.eprintf "windtrap mutants: %s\n%!"
            (Data_files.describe ~path f))
        excluded;
      if kept = [] then
        Printf.eprintf
          "windtrap mutants: every .mutants file was excluded, so there is \
           nothing to report\n\
           %!";
      if excluded <> [] then Printf.eprintf "windtrap mutants: %s\n%!" remedy;
      if kept = [] then Error 1
      else
        Ok
          (List.map
             (fun (path, t, identity, _) ->
               (executable_label ~path identity, t))
             kept)

(* Report data *)

(* Sources, best effort: a survivor whose file cannot be read still names
   its line in the head row. Recorded paths are workspace-relative, so
   they resolve against the roots discovery settled on. *)
let read_source ~roots =
  let cache = Hashtbl.create 16 in
  let read path =
    match open_in_bin path with
    | exception Sys_error _ -> None
    | ic ->
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () ->
            match really_input_string ic (in_channel_length ic) with
            | contents -> Some contents
            | exception (End_of_file | Sys_error _) -> None)
  in
  fun file ->
    match Hashtbl.find_opt cache file with
    | Some contents -> contents
    | None ->
        let contents =
          List.find_map (fun root -> read (Filename.concat root file)) roots
        in
        Hashtbl.add cache file contents;
        contents

(* The aggregate report, from the labelled collections and nothing else.

   The verdicts and the counts are the merge's: [V.merge] is
   killed-anywhere-wins, the one algebra of the file format, and this
   projection never re-derives it. What the merge cannot carry is who ran
   a witness — a merged survivor's witnesses are a union with the
   executables folded away — so those are collected beside it: every
   survived record's witnesses, tagged with its file's executable. A file
   that killed the mutant contributes none; its tests noticed. A merged
   survivor's witnesses are then the tagged union from every file that
   reported it survived, sorted by (executable, test), without
   duplicates. The ordering mirrors the loop's per-executable report: by
   witness count descending, then by identifier. *)
let render_data ~resolve_source files =
  let merged = List.fold_left (fun acc (_, t) -> V.merge acc t) V.empty files in
  let tagged = Hashtbl.create 64 in
  List.iter
    (fun (exe, t) ->
      List.iter
        (fun (r : V.record) ->
          match r.V.verdict with
          | V.Survived { witness; others } ->
              List.iter
                (fun path ->
                  Hashtbl.add tagged r.V.id (exe, Test_tree.path_to_string path))
                (witness :: others)
          | V.Killed | V.Unreached -> ())
        (V.records t))
    files;
  let mutant_of (r : V.record) : Render.mutant =
    {
      (* The identifier is spelled here, with the runtime's own function:
         Render carries it into the head row without re-spelling it. *)
      Render.id = M.id_to_string r.V.id;
      file = r.V.id.M.file;
      line = r.V.id.M.line;
      before = r.V.before;
      after = r.V.after;
      source = resolve_source r.V.id.M.file;
    }
  in
  let survivor_of (r : V.record) : Render.survivor =
    {
      Render.mutant = mutant_of r;
      witnesses =
        List.map
          (fun (exe, test) ->
            (* [loc = None] throughout: a test's declaration site lives
               in the test tree of the executable that ran it, and this
               command links none of them. The name is what a reader
               greps for, and it is in the report. *)
            { Render.test; loc = None; exe = Some exe })
          (List.sort_uniq compare (Hashtbl.find_all tagged r.V.id));
    }
  in
  let records = V.records merged in
  let survivors =
    List.filter_map
      (fun (r : V.record) ->
        match r.V.verdict with
        | V.Survived _ -> Some (survivor_of r)
        | V.Killed | V.Unreached -> None)
      records
  in
  (* [List.stable_sort] keeps identifier order within a count, as the
     loop's does. *)
  let survivors =
    List.stable_sort
      (fun (a : Render.survivor) (b : Render.survivor) ->
        compare (List.length b.witnesses) (List.length a.witnesses))
      survivors
  in
  (* The merge is the one report that can call a mutant unreached: a
     mutant no executable's tests evaluate is a finding here, where one
     executable's unreached mutant was merely not its own. *)
  let unreached =
    List.filter_map
      (fun (r : V.record) ->
        match r.V.verdict with
        | V.Unreached -> Some (mutant_of r)
        | V.Killed | V.Survived _ -> None)
      records
  in
  {
    (* The arming variable, spelled with the runtime's own function: the
       report and the runtime cannot disagree about what to type. *)
    Render.arm_variable = M.arm_variable;
    survivors;
    unreached;
    killed =
      List.length
        (List.filter (fun (r : V.record) -> r.V.verdict = V.Killed) records);
    scope = Render.Executables (List.length files);
    filter = None;
  }

let print_report report =
  let ansi =
    Env.resolve_color (Env.color_mode ()) ~tty:(Env.is_tty_stdout ())
      ~inside_dune:(Env.inside_dune ()) ~term_dumb:(Env.term_dumb ())
  in
  let renderer = Render.create ~out:Format.std_formatter ~ansi () in
  Render.mutation_report renderer report;
  Format.pp_print_flush Format.std_formatter ()

(* The command *)

let run args =
  match parse_args args with
  | Error `Help ->
      print_endline usage;
      0
  | Error (`Usage message) ->
      Printf.eprintf "windtrap mutants: %s\n%s\n" message usage;
      2
  | Ok paths -> (
      match discover paths with
      | Error message ->
          Printf.eprintf "windtrap mutants: %s\n" message;
          1
      | Ok (files, roots) -> (
          if files = [] then begin
            Printf.eprintf
              "windtrap mutants: no .mutants files found\n\
               Instrument the library under test with ppx_windtrap.mutate and \
               run every suite with its mutants (WINDTRAP_MUTATE=1) first; \
               every mutation run writes its verdicts under _build/_mutants.\n";
            1
          end
          else
            match load_fresh files with
            | Error code -> code
            | Ok files ->
                let report =
                  render_data ~resolve_source:(read_source ~roots) files
                in
                print_report report;
                (* A survivor is the project's failure: a fault every
                   executable that reached it let through. An unreached
                   mutant is a coverage-style finding, listed and not
                   scored. *)
                if report.Render.survivors = [] then 0 else 1))
