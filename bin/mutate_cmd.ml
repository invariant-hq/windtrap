(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   File discovery and the staleness pass live in Data_files, shared with
   `windtrap coverage`: one rule for resolving the project root, one rule
   for detecting a file whose executable is gone or was rebuilt. The
   report layout lives in the library's report sections
   (Report_sections.mutation_report, via Windtrap.Private), so the loop's
   in-process report and this merged one cannot drift.
  ---------------------------------------------------------------------------*)

module Sections = Windtrap.Private.Report_sections
module Os = Windtrap.Private.Os
module Cli = Windtrap.Private.Cli
module Test_tree = Windtrap.Private.Test_tree
module M = Windtrap_runtime.Mutate
module V = Windtrap_runtime.Verdicts

let spf = Printf.sprintf
let usage = "usage: windtrap mutants [PATH...]"

let help =
  "windtrap mutants - merge .mutants verdict files and report the survivors\n\n"
  ^ usage
  ^ {|

Merges the .mutants verdict files written by mutation runs and reports the
mutants that survived every test executable. Without PATH arguments the files
are found under the build directory's _mutants (or _windtrap/mutants in a tree
built without one), walking up from the current directory to the enclosing
project root; PATH arguments (.mutants files, or directories searched
recursively) replace that default.

Runs no tests and drives no build.
Exits 1 when any mutant survived every executable that reached it.

OPTIONS:
  -h, --help
      Print this help and exit.

ENVIRONMENT (no flag):
  WINDTRAP_COLOR
      Color output: always, never or auto.|}

(* The one remedy, spelled once, in words any build tool's user can act
   on: this command does not know how the suite is run, and a spelled-out
   command would be wrong everywhere but the tree it was written in. A
   verdict exists only where a suite was asked to test its mutants
   (--mutate) in a build carrying them, and a run the build tool replays
   from its cache writes nothing. *)
let rerun =
  "re-run every suite with its mutants (--mutate, instrumented with \
   ppx_windtrap.mutate, forcing the runs your build tool cached), then merge \
   again"

let remedy = rerun ^ "; delete the files whose executable no longer exists"

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

(* Discovery: Data_files's, shared with `windtrap coverage`, with the project
   root resolved as the runtime resolves its output path, explicit PATH
   arguments as a loud contract. A silently narrowed merge would be worse
   here than in coverage: under killed-anywhere-wins, dropping the file
   that holds the kill turns the mutant back into a survivor, which is
   the exact failure mode this command exists to prevent. [files] come
   back sorted for deterministic merge order and error attribution;
   [roots] are the source roots the survivor excerpts resolve against. *)

let discover paths = Data_files.discover V.format paths

(* The staleness pass

   Data_files.freshness's, judged from the identity each file records on
   its header: a verdict file whose executable was deleted or renamed (an
   orphan), and one not written by the executable now on disk, after a
   rebuild without the backend, or a run the build tool replayed from its
   cache.

   A flagged verdict file is excluded, never merged: a stale verdict can
   claim a kill the code no longer earns, and a false kill hides a live
   defect. Excluding is the only answer that cannot lie. The judgement
   comes before the load, so a corrupt leftover of another build is
   excluded like any other, while a file of this build must load: dropping
   it could drop the one kill of a mutant. *)

(* Dune's inline-test runner is [inline-test-runner.exe] in every
   library's [.<lib>.inline-tests] directory: the library is what tells
   one from another. *)
let inline_library exe =
  let dir = Filename.basename (Filename.dirname exe) in
  let suffix = ".inline-tests" in
  if
    Filename.basename exe = "inline-test-runner.exe"
    && String.length dir > String.length suffix + 1
    && dir.[0] = '.'
    && Filename.check_suffix dir suffix
  then Some (String.sub dir 1 (String.length dir - 1 - String.length suffix))
  else None

(* The executable a verdict file speaks for, as the witness column names
   it: the basename of the recorded identity ([test_slug.exe]), or the
   library whose inline tests ran. A file that records no identity
   (hand-written, or a merge, which has no single writer) is named by its
   own basename. *)
let executable_label ~path identity =
  match (identity : V.identity option) with
  | None -> Filename.basename path
  | Some { exe; _ } ->
      Option.value (inline_library exe) ~default:(Filename.basename exe)

(* How a reader runs that executable again. An identity is relative iff
   its executable was built under a dune build directory, its first
   component the build context, which [dune exec] does not take. Dune's
   inline runner takes its arguments from dune alone, and a file without
   an identity names no executable: both are reached through the build. *)
let invocation identity =
  match (identity : V.identity option) with
  | None -> `Mirrors
  | Some { exe; _ } when Option.is_some (inline_library exe) -> `Mirrors
  | Some { exe; _ } when Filename.is_relative exe ->
      let target =
        match String.index_opt exe '/' with
        | Some i -> String.sub exe (i + 1) (String.length exe - i - 1)
        | None -> exe
      in
      `Exe
        ("dune exec --instrument-with ppx_windtrap.mutate "
       ^ Sections.shell_word target ^ " --")
  | Some { exe; _ } -> `Exe (Sections.shell_word exe)

(* Judges [files] from their headers, drops the orphaned and stale ones
   loudly, loads the rest and returns them, each collection with its
   executable's label and how to run it again. Warnings and failure
   details go to stderr; [Error code] is the exit code (data problems are
   1). *)
let load_fresh files =
  let judged =
    List.fold_left
      (fun acc path ->
        Result.bind acc (fun (kept, excluded) ->
            Result.bind (Data_files.identity V.format path) (fun identity ->
                match Data_files.freshness ~path identity with
                | Data_files.Fresh ->
                    Result.map
                      (fun (t, identity) ->
                        ((path, t, identity) :: kept, excluded))
                      (V.load path)
                | (Data_files.Orphan _ | Data_files.Stale _) as freshness ->
                    Ok (kept, (path, freshness) :: excluded))))
      (Ok ([], []))
      files
  in
  match judged with
  | Error error ->
      Os.say (Format.asprintf "%a" V.pp_error error);
      Error 1
  | Ok (kept, excluded) ->
      let kept = List.rev kept and excluded = List.rev excluded in
      List.iter Os.say (Data_files.warnings excluded);
      if kept = [] then
        Os.say
          (Data_files.all_excluded ~ext:"mutants" (List.map snd excluded)
          ^ "\n\
            \  A verdict is written only by a run asked to test its mutants, \
             and it is\n\
            \  invalidated by any later build of the executable that wrote it."
          );
      if excluded <> [] then Os.say remedy;
      if kept = [] then Error 1
      else
        Ok
          (List.map
             (fun (path, t, identity) ->
               (executable_label ~path identity, invocation identity, t))
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

   Coverage merges by addition, so two executables over one file can only
   agree more. Verdicts do not add: a mutant killed by one suite and merely
   reached by another is killed, and the second suite's view alone is a
   false survivor. That is why a verdict file exists.

   The verdicts and the counts are the merge's: [V.merge] is
   killed-anywhere-wins, the one algebra of the file format, and this
   projection never re-derives it. What the merge cannot carry is who ran
   a witness (a merged survivor's witnesses are a union with the
   executables folded away), so those are collected beside it: every
   survived record's witnesses, tagged with its file's executable. What is
   tagged for a mutant that some file killed is never read, since such a
   mutant is no survivor of the merge. A merged survivor's witnesses are
   then the tagged union from every file that reported it survived, sorted
   by (executable, test), without duplicates. Survivors are ordered by
   reaching-test count descending, then by identifier: the one the most
   tests watched is the one a reader can act on soonest.

   The second component is how the executable of the first survivor's
   first reaching-test row is run again, for the report's one command: it
   is the first of [files] carrying that row's label that let the mutant
   survive. *)
let render_data ~resolve_source files =
  let merged =
    List.fold_left (fun acc (_, _, t) -> V.merge acc t) V.empty files
  in
  let tagged = Hashtbl.create 64 in
  List.iter
    (fun (exe, _, t) ->
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
  let survivor_of (r : V.record) : Sections.survivor =
    {
      Sections.mutant =
        {
          (* Spelled with the runtime's own function: the report carries
             the identifier into the title and the command as it is. *)
          Sections.id = M.id_to_string r.V.id;
          file = r.V.id.M.file;
          line = r.V.id.M.line;
          before = r.V.before;
          after = r.V.after;
          source = resolve_source r.V.id.M.file;
        };
      witnesses =
        List.map
          (fun (exe, test) ->
            (* [loc = None] throughout: a test's declaration site lives
               in the test tree of the executable that ran it, and this
               command links none of them. The name is what a reader
               greps for, and it is in the report. *)
            { Sections.test; loc = None; exe = Some exe })
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
  (* [List.stable_sort] keeps identifier order within a count. *)
  let survivors =
    List.stable_sort
      (fun (a : Sections.survivor) (b : Sections.survivor) ->
        compare (List.length b.witnesses) (List.length a.witnesses))
      survivors
  in
  let survived_in id t =
    List.exists
      (fun (r : V.record) ->
        match r.V.verdict with
        | V.Survived _ -> String.equal (M.id_to_string r.V.id) id
        | V.Killed | V.Unreached -> false)
      (V.records t)
  in
  let invocation =
    match survivors with
    | { mutant; witnesses = { exe = Some exe; _ } :: _ } :: _ ->
        List.find_map
          (fun (label, invocation, t) ->
            if String.equal label exe && survived_in mutant.id t then
              Some invocation
            else None)
          files
    | { witnesses = { exe = None; _ } :: _ | []; _ } :: _ | [] -> None
  in
  (* The merge is the one report that can call a mutant unreached by the
     project: a mutant no executable's tests evaluate. *)
  let unreached =
    List.filter_map
      (fun (r : V.record) ->
        match r.V.verdict with
        | V.Unreached -> Some (r.V.id.M.file, r.V.id.M.line)
        | V.Killed | V.Survived _ -> None)
      records
  in
  ( {
      Sections.survivors;
      unreached;
      killed =
        List.length
          (List.filter (fun (r : V.record) -> r.V.verdict = V.Killed) records);
      not_tested = 0;
      scope = Sections.Executables (List.length files);
    },
    Option.value invocation ~default:`Mirrors )

let print_report ~color ~invocation report =
  let ansi =
    Os.resolve_color color ~tty:(Os.is_tty_stdout ())
      ~inside_dune:(Os.inside_dune ()) ~term_dumb:(Os.term_dumb ())
  in
  Sections.print ~out:Format.std_formatter ~ansi
    (Sections.mutation_report ~invocation report)

(* The command *)

let run args =
  match parse_args args with
  | Error `Help ->
      print_endline help;
      0
  | Error (`Usage message) ->
      Os.say message;
      prerr_endline usage;
      2
  | Ok paths -> (
      (* No --color flag here, so WINDTRAP_COLOR is the whole colour
         decision: read through the runner's --color parser and refused on
         the same terms, never read as "auto" out of a typo. *)
      match (Cli.color_mode (), discover paths) with
      | Error error, _ ->
          Os.say (Cli.error_message error);
          2
      | Ok _, Error message ->
          Os.say message;
          1
      | Ok color, Ok (files, roots) -> (
          if files = [] then begin
            Os.say
              "no .mutants files found\n\
               Instrument the library under test with ppx_windtrap.mutate and \
               run every suite with its mutants (--mutate) first; every \
               mutation run writes its verdicts under the build directory's \
               _mutants or under _windtrap/mutants.";
            1
          end
          else
            match load_fresh files with
            | Error code -> code
            | Ok files ->
                let report, invocation =
                  render_data ~resolve_source:(read_source ~roots) files
                in
                print_report ~color ~invocation report;
                (* A survivor is the project's failure: a fault every
                   executable that reached it let through. An unreached
                   mutant is a coverage-style finding, listed and not
                   scored. *)
                if report.Sections.survivors = [] then 0 else 1))
