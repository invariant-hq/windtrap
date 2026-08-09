(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   File discovery and the staleness pass are bin/coverage_cmd.ml's, with
   _coverage/.coverage replaced by _mutants/.mutants: one rule for
   resolving the project root, one rule for detecting a dump whose
   executable is gone or was rebuilt. The report layout lives in the
   library renderer (Render.mutation_report, via Windtrap.Private), so
   the loop's in-process report and this merged one cannot drift.
  ---------------------------------------------------------------------------*)

module Render = Windtrap.Private.Render
module Env = Windtrap.Private.Env
module Test_tree = Windtrap.Private.Test_tree
module M = Windtrap_mutate

let spf = Printf.sprintf

let usage =
  {|usage: windtrap mutate [PATH...]

Merges the .mutants verdict files written by mutation runs and reports the
mutants that survived every test executable. Without PATH arguments the files
are found under _build/_mutants, walking up from the current directory to the
enclosing project root; PATH arguments (.mutants files, or directories
searched recursively) replace that default.

Runs no tests and drives no build. A survivor never fails the build.

OPTIONS:
  -h, --help  Print this help and exit|}

(* The re-run, spelled once. Every remedy this command prints names it,
   and it is the alias recipe from the manual with --force, which is
   load-bearing: a mutation run is not a cached artifact. *)
let rerun =
  "  WINDTRAP_MUTATE=1 dune build @mutants --force --instrument-with \
   ppx_windtrap.mutate"

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

(* Discovery *)

let mutants_dir_of root =
  Filename.concat (Filename.concat root "_build") "_mutants"

(* Ancestor-scan fallback: the nearest ancestor (the current directory
   included) with a _build/_mutants directory — `dune exec windtrap`
   runs from wherever the user is in the checkout. Candidates inside a
   sandbox are never roots: planted garbage under _build/.sandbox must
   not capture the scan. *)
let under_sandbox dir =
  String.map (function '\\' -> '/' | c -> c) dir
  |> String.split_on_char '/' |> List.mem ".sandbox"

let rec find_project_root dir =
  let candidate = mutants_dir_of dir in
  if
    (not (under_sandbox dir))
    && Sys.file_exists candidate && Sys.is_directory candidate
  then Some dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then None else find_project_root parent

(* The root rule, shared with the runtime's output path: when the current
   directory is inside a _build — a dune rule action, sandboxed or not —
   the root is the parent of the topmost _build component,
   unconditionally; only outside _build does the ancestor scan run. *)
let project_root cwd =
  match M.build_root ~path:cwd with
  | Some root -> Some root
  | None -> find_project_root cwd

let is_verdict_file path = Filename.check_suffix path ".mutants"

let rec files_under dir =
  match Sys.readdir dir with
  | exception Sys_error _ -> []
  | entries ->
      Array.fold_left
        (fun acc entry ->
          let path = Filename.concat dir entry in
          match Sys.is_directory path with
          | true -> files_under path @ acc
          | false -> if is_verdict_file path then path :: acc else acc
          | exception Sys_error _ -> acc)
        [] entries

(* Explicit arguments are a contract: a named file must exist and carry
   the .mutants suffix. A typo'd path or a wrong glob in CI that silently
   narrowed the merge would be worse here than in coverage: under
   killed-anywhere-wins, dropping the file that holds the kill turns the
   mutant back into a survivor, which is the exact failure mode this
   command exists to prevent. Directories keep the scan's tolerance. *)
let expand_path path =
  if not (Sys.file_exists path) then
    Error (spf "%s: no such file or directory" path)
  else if Sys.is_directory path then Ok (files_under path)
  else if is_verdict_file path then Ok [ path ]
  else Error (spf "%s: not a .mutants file" path)

(* [files] sorted for deterministic merge order and error attribution;
   [roots] are the source roots the survivor excerpts resolve against. *)
let discover = function
  | [] ->
      Ok
        (match project_root (Sys.getcwd ()) with
        | None -> ([], [ "." ])
        | Some root ->
            ( List.sort_uniq String.compare (files_under (mutants_dir_of root)),
              [ root ] ))
  | paths ->
      List.fold_left
        (fun acc path ->
          Result.bind acc (fun files ->
              Result.map (fun found -> found @ files) (expand_path path)))
        (Ok []) paths
      |> Result.map (fun files ->
          (List.sort_uniq String.compare files, [ "." ]))

(* The staleness pass

   Coverage's, detected the same way from the identity each file records:
   a verdict file whose executable was deleted or renamed (an orphan), and
   one not written by the executable now on disk — a re-run made without
   --instrument-with, or a run dune replayed from cache. The digest
   comparison is content-based because mtimes prove nothing: dune's
   shared cache restores rebuilt artifacts with their original
   timestamps. Files without an identity (hand-written or already merged)
   are never flagged.

   There is no --stale override here, and the asymmetry with coverage is
   deliberate. A stale coverage dump understates what the suite reaches;
   a stale verdict can claim a kill the code no longer earns, and a false
   kill hides a live defect. Excluding is the only answer that cannot
   lie. *)

type freshness = Fresh | Orphan of string | Stale of string

let freshness ~path identity =
  match (identity : M.identity option) with
  | None -> Fresh
  | Some { exe; digest } -> (
      let resolved =
        if not (Filename.is_relative exe) then Some exe
        else
          (* A relative identity is a path below _build; the file's own
             topmost-_build root locates that _build — the same root
             whether the file was discovered or named on the command
             line. *)
          match M.build_root ~path with
          | None -> None
          | Some root ->
              Some (Filename.concat (Filename.concat root "_build") exe)
      in
      match resolved with
      | None -> Fresh
      | Some exe_path -> (
          if not (Sys.file_exists exe_path) then Orphan exe
          else
            match Digest.to_hex (Digest.file exe_path) <> digest with
            | true -> Stale exe
            | false -> Fresh
            | exception (Sys_error _ | End_of_file) -> Fresh))

let describe ~path = function
  | Fresh -> assert false
  | Orphan exe -> spf "%s: its executable (%s) no longer exists" path exe
  | Stale exe ->
      spf
        "%s: not written by the executable now at %s - a re-run made without \
         --instrument-with ppx_windtrap.mutate, or a mutation run dune did not \
         repeat"
        path exe

(* Loads [files], drops the orphaned and stale ones loudly, and merges
   what is left. Warnings and failure details go to stderr; [Error code]
   is the exit code (data problems are 1). *)
let load_merged files =
  let loaded =
    List.fold_left
      (fun acc path ->
        Result.bind acc (fun entries ->
            Result.map
              (fun (t, identity) ->
                (path, t, freshness ~path identity) :: entries)
              (M.load path)))
      (Ok []) files
  in
  match loaded with
  | Error error ->
      Format.eprintf "windtrap mutate: %a@." M.pp_error error;
      Error 1
  | Ok entries ->
      let entries = List.rev entries in
      let kept = List.filter (fun (_, _, f) -> f = Fresh) entries in
      let excluded = List.filter (fun (_, _, f) -> f <> Fresh) entries in
      List.iter
        (fun (path, _, f) ->
          Printf.eprintf "windtrap mutate: %s; excluding it\n%!"
            (describe ~path f))
        excluded;
      (* Two exclusions, two remedies, and they are not interchangeable.
         A forced run rewrites a stale verdict; nothing rewrites an
         orphan, whose executable no longer exists — the file is a
         leftover and only deleting it removes it, so naming the re-run
         there would send the reader round a loop that cannot terminate.
         Skipped when everything was excluded: the epilogue below carries
         both. *)
      let any predicate = List.exists (fun (_, _, f) -> predicate f) excluded in
      if kept <> [] then begin
        if any (function Stale _ -> true | Fresh | Orphan _ -> false) then
          Printf.eprintf
            "windtrap mutate: a forced run rewrites stale verdicts:\n%s\n%!"
            rerun;
        if any (function Orphan _ -> true | Fresh | Stale _ -> false) then
          Printf.eprintf
            "windtrap mutate: delete the orphaned files; re-running cannot \
             replace a verdict whose executable is gone\n\
             %!"
      end;
      if kept = [] then begin
        Printf.eprintf
          "windtrap mutate: every .mutants file is orphaned or stale\n\
           Re-run the mutation tests:\n\
           %s\n\
           and delete leftovers of removed executables.\n"
          rerun;
        Error 1
      end
      else Ok (List.fold_left (fun acc (_, t, _) -> M.merge acc t) M.empty kept)

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

(* [loc = None] throughout: a test's declaration site lives in the test
   tree of the executable that ran it, and this command links none of
   them. The name is what a reader greps for, and it is in the file. *)
let survivor_of ~source (r : M.record) witnesses : Render.survivor =
  {
    Render.file = r.M.id.M.file;
    line = r.M.id.M.line;
    col = r.M.id.M.col;
    rewrite = r.M.id.M.rewrite;
    before = r.M.before;
    after = r.M.after;
    source = source r.M.id.M.file;
    witnesses =
      List.map
        (fun path ->
          { Render.test = Test_tree.path_to_string path; loc = None })
        witnesses;
  }

let unreached_lines records =
  let by_file = Hashtbl.create 16 in
  List.iter
    (fun (r : M.record) ->
      let file = r.M.id.M.file in
      let prior = Option.value ~default:[] (Hashtbl.find_opt by_file file) in
      Hashtbl.replace by_file file (r.M.id.M.line :: prior))
    records;
  Hashtbl.fold
    (fun file lines acc ->
      { Render.file; lines = List.sort_uniq compare lines } :: acc)
    by_file []
  |> List.sort (fun (a : Render.unreached) b -> compare a.file b.file)

let print_report ~roots collection =
  let records = M.records collection in
  let source = read_source ~roots in
  let survivors =
    List.filter_map
      (fun (r : M.record) ->
        match r.M.verdict with
        | M.Survived { witness; others } ->
            Some (survivor_of ~source r (witness :: others))
        | M.Killed _ | M.Unreached -> None)
      records
  in
  (* Ordered by reaching-test count descending, as the loop's report is:
     the survivor the most tests watched is the one whose block a reader
     can act on soonest. [List.stable_sort] keeps identifier order within
     a count. Nothing is capped — a project report a reader cannot page
     past would send them back to the per-executable one. *)
  let survivors =
    List.stable_sort
      (fun (a : Render.survivor) (b : Render.survivor) ->
        compare (List.length b.witnesses) (List.length a.witnesses))
      survivors
  in
  let unreached =
    List.filter (fun (r : M.record) -> r.M.verdict = M.Unreached) records
  in
  let killed =
    List.length
      (List.filter
         (fun (r : M.record) ->
           match r.M.verdict with M.Killed _ -> true | _ -> false)
         records)
  in
  let ansi =
    Env.resolve_color (Env.color_mode ()) ~tty:(Env.is_tty_stdout ())
      ~inside_dune:(Env.inside_dune ()) ~term_dumb:(Env.term_dumb ())
  in
  let renderer = Render.create ~out:Format.std_formatter ~ansi () in
  Render.mutation_report renderer
    {
      Render.survivors;
      survivors_total = List.length survivors;
      unreached = unreached_lines unreached;
      unreached_total = List.length unreached;
      killed;
      total = List.length records;
      (* The merge ran nothing and seeded nothing, and it is the project's
         view rather than one executable's — so no duration, no seed, and
         no sibling scoping. *)
      duration = None;
      seed = None;
      siblings = false;
    };
  Format.pp_print_flush Format.std_formatter ()

(* The command *)

let run args =
  match parse_args args with
  | Error `Help ->
      print_endline usage;
      0
  | Error (`Usage message) ->
      Printf.eprintf "windtrap mutate: %s\n%s\n" message usage;
      2
  | Ok paths -> (
      match discover paths with
      | Error message ->
          Printf.eprintf "windtrap mutate: %s\n" message;
          1
      | Ok (files, roots) -> (
          if files = [] then begin
            Printf.eprintf
              "windtrap mutate: no .mutants files found\n\
               Instrument the library under test\n\
              \  (instrumentation (backend ppx_windtrap.mutate))\n\
               and mutation-test it first:\n\
               %s\n"
              rerun;
            1
          end
          else
            match load_merged files with
            | Error code -> code
            | Ok collection ->
                print_report ~roots collection;
                0))
