(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   File discovery and the staleness pass live in Data_files, shared with
   `windtrap mutants`; the table and excerpt rendering live in the
   library renderer (Render, via Windtrap.Private) over section data
   built by the coverage seam's one builder (Driver.coverage_data), so
   the in-process report and this command share one layout and one
   projection.
  ---------------------------------------------------------------------------*)

module Render = Windtrap.Private.Render
module Driver = Windtrap.Private.Driver
module Env = Windtrap.Private.Env

let spf = Printf.sprintf

let usage =
  {|usage: windtrap coverage [OPTIONS] [PATH...]

Merges the .coverage files written by instrumented test executables and
reports expression coverage per source file. Without PATH arguments the files
are found under _build/_coverage, walking up from the current directory
to the enclosing project root; PATH arguments (.coverage files, or
directories searched recursively) replace that default.

OPTIONS:
  --min PCT             Exit 1 when total coverage is below PCT
  --json                Machine-readable report on standard output
  --lcov                LCOV tracefile on standard output (genhtml, Codecov,
                        Coveralls, GitLab, editor gutters)
  --expect PATH         Exit 1 unless every .ml/.mll/.mly under PATH (or PATH
                        itself) has coverage data; repeatable
  --do-not-expect PATH  Exempt PATH, a file or a directory, from --expect
  -u, --show-uncovered  Also render uncovered source excerpts
  -h, --help            Print this help and exit|}

(* Flags *)

type options = {
  min : float option;
  json : bool;
  lcov : bool;
  expect : string list;
  do_not_expect : string list;
  show_uncovered : bool;
  paths : string list;
}

let min_of_string value =
  match float_of_string_opt value with
  | Some pct when Float.is_finite pct && 0. <= pct && pct <= 100. -> Some pct
  | _ -> None

let parse_args args =
  let min_error value =
    Error
      (`Usage
         (spf "invalid value '%s' for --min: expected a percentage (0-100)"
            value))
  in
  let rec go acc = function
    | [] ->
        if acc.json && acc.lcov then
          Error (`Usage "--json and --lcov each own standard output; pick one")
        else
          Ok
            {
              acc with
              paths = List.rev acc.paths;
              expect = List.rev acc.expect;
              do_not_expect = List.rev acc.do_not_expect;
            }
    | ("-h" | "--help" | "-help") :: _ -> Error `Help
    | "--min" :: value :: rest -> (
        match min_of_string value with
        | Some pct -> go { acc with min = Some pct } rest
        | None -> min_error value)
    | [ "--min" ] -> Error (`Usage "option '--min' requires an argument")
    | arg :: rest when String.length arg >= 6 && String.sub arg 0 6 = "--min="
      -> (
        let value = String.sub arg 6 (String.length arg - 6) in
        match min_of_string value with
        | Some pct -> go { acc with min = Some pct } rest
        | None -> min_error value)
    | "--json" :: rest -> go { acc with json = true } rest
    | "--lcov" :: rest -> go { acc with lcov = true } rest
    | "--expect" :: path :: rest ->
        go { acc with expect = path :: acc.expect } rest
    | [ "--expect" ] -> Error (`Usage "option '--expect' requires an argument")
    | "--do-not-expect" :: path :: rest ->
        go { acc with do_not_expect = path :: acc.do_not_expect } rest
    | [ "--do-not-expect" ] ->
        Error (`Usage "option '--do-not-expect' requires an argument")
    | ("-u" | "--show-uncovered") :: rest ->
        go { acc with show_uncovered = true } rest
    | arg :: _ when String.length arg > 0 && arg.[0] = '-' ->
        Error (`Usage (spf "unknown option '%s'" arg))
    | path :: rest -> go { acc with paths = path :: acc.paths } rest
  in
  go
    {
      min = None;
      json = false;
      lcov = false;
      expect = [];
      do_not_expect = [];
      show_uncovered = false;
      paths = [];
    }
    args

(* Discovery: Data_files's, shared with `windtrap mutants` — the project
   root resolved as the runtime resolves its dump path, explicit PATH
   arguments as a loud contract (a silent narrowing of the merge would
   end in the no-data message and its wrong remedy). [files] come back
   sorted for deterministic merge order and error attribution; [roots]
   are the source roots for line mapping. *)

let discover paths = Data_files.discover ~dir:"_coverage" ~ext:"coverage" paths

(* The staleness pass

   "Up to date" is not "re-run": the holes are dumps whose executable
   was deleted or renamed (orphans, which would silently inflate the
   merge) and dumps not written by the executable now on disk — a
   rebuild without the backend (writes no fresh dump), or a test run the
   build tool replayed from its cache after sources reverted to an
   already-tested state (the dump on disk stays a different build's, and
   only a forced run heals it). Detection is Data_files.freshness's,
   from the identity recorded in each dump; the exclusion and the remedy
   are this command's. A flagged dump is always excluded and always
   named: there is no override, because a number computed from a dump
   known to describe another build can only mislead. The remedy is one
   sentence in words any build tool's user can act on: this command
   does not know how the suite is run, and a spelled-out command would
   be wrong everywhere but the tree it was written in. *)

(* The empty-estate message: no dump the merge could use. *)
let no_data =
  "no .coverage files found\n\
   Instrument the library under test with ppx_windtrap.coverage and run its \
   tests first; every instrumented test executable writes its dump under \
   _build/_coverage at exit.\n"

let remedy =
  "re-run the suite instrumented (forcing the runs your build tool cached), \
   then merge again; delete _build/_coverage to drop leftovers of removed \
   executables"

(* Loads [files], excludes the ones the freshness pass flagged, and
   merges the survivors. Warnings and failure details go to stderr;
   [Error code] is the exit code (data problems are 1). *)
let load_merged files =
  let loaded =
    List.fold_left
      (fun acc path ->
        Result.bind acc (fun entries ->
            Result.map
              (fun (t, exe) ->
                (path, t, Data_files.freshness ~path exe) :: entries)
              (Windtrap_runtime.Coverage.load path)))
      (Ok []) files
  in
  match loaded with
  | Error error ->
      Format.eprintf "windtrap coverage: %a@."
        Windtrap_runtime.Coverage.pp_error error;
      Error 1
  | Ok entries -> (
      let entries = List.rev entries in
      let kept, flagged =
        List.partition (fun (_, _, v) -> v = Data_files.Fresh) entries
      in
      (* One line per excluded dump — the path, the executable and the
         reason, which is what a reader goes and looks at — then the
         remedy once, however many there were. *)
      List.iter
        (fun (path, _, v) ->
          Printf.eprintf "windtrap coverage: %s\n%!"
            (Data_files.describe ~path v))
        flagged;
      if kept = [] then
        Printf.eprintf
          "windtrap coverage: every .coverage file was excluded, so there is \
           nothing to report\n\
           %!";
      if flagged <> [] then Printf.eprintf "windtrap coverage: %s\n%!" remedy;
      if kept = [] then Error 1
      else
        match
          List.fold_left
            (fun acc (_, t, _) ->
              Result.bind acc (fun acc -> Windtrap_runtime.Coverage.merge acc t))
            (Ok Windtrap_runtime.Coverage.empty) kept
        with
        | Ok collection -> Ok collection
        | Error error ->
            Format.eprintf "windtrap coverage: %a@."
              Windtrap_runtime.Coverage.pp_error error;
            Error 1)

(* JSON *)

let json_escape s =
  let buffer = Buffer.create (String.length s + 8) in
  String.iter
    (function
      | '"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\r' -> Buffer.add_string buffer "\\r"
      | '\t' -> Buffer.add_string buffer "\\t"
      | c ->
          if Char.code c < 32 then
            Buffer.add_string buffer (spf "\\u%04x" (Char.code c))
          else Buffer.add_char buffer c)
    s;
  Buffer.contents buffer

let json_ints lines =
  spf "[%s]" (String.concat "," (List.map string_of_int lines))

(* The CI artifact: summary + per-file visited/total/percentage +
   uncovered lines. Frozen keys; a file whose source is missing or stale
   reports an empty uncovered list. *)
let print_json ~source_roots collection =
  let summary = Windtrap_runtime.Coverage.summary collection in
  let reports =
    Windtrap_runtime.Coverage.file_reports ~source_roots collection
  in
  Printf.printf
    "{ \"summary\": { \"visited\": %d, \"total\": %d, \"percentage\": %.2f },\n\
    \  \"files\": ["
    summary.visited summary.total
    (Windtrap_runtime.Coverage.percentage summary);
  List.iteri
    (fun i (r : Windtrap_runtime.Coverage.file_report) ->
      Printf.printf
        "%s\n\
        \    { \"path\": \"%s\", \"visited\": %d, \"total\": %d,\n\
        \      \"percentage\": %.2f,\n\
        \      \"uncovered_lines\": %s }"
        (if i = 0 then "" else ",")
        (json_escape r.file) r.summary.visited r.summary.total
        (Windtrap_runtime.Coverage.percentage r.summary)
        (json_ints r.uncovered_lines))
    reports;
  Printf.printf " ] }\n%!"

(* LCOV *)

(* The tracefile every coverage service and gutter reads, and what
   genhtml renders: one record per file, [DA:<line>,<hits>] for every
   line a point touches (hits by the runtime's per-line rule, so an
   uncovered line is a 0), then the instrumented and hit line counts.
   Paths are as recorded, project-relative. A file whose source is
   missing or stale has no lines to speak of and is omitted, named on
   stderr: painting it would attribute hits to code the data does not
   describe. *)
let print_lcov ~source_roots collection =
  let reports =
    Windtrap_runtime.Coverage.file_reports ~source_roots collection
  in
  List.iter
    (fun (r : Windtrap_runtime.Coverage.file_report) ->
      match r.source with
      | None ->
          Printf.eprintf
            "windtrap coverage: %s: %s; omitted from the lcov output\n%!" r.file
            (if r.stale then "the source changed since the run"
             else "source not found")
      | Some _ ->
          Printf.printf "TN:\nSF:%s\n" r.file;
          List.iter
            (fun (line, hits) -> Printf.printf "DA:%d,%d\n" line hits)
            r.line_hits;
          let hit = List.filter (fun (_, hits) -> hits > 0) r.line_hits in
          Printf.printf "LF:%d\nLH:%d\nend_of_record\n"
            (List.length r.line_hits) (List.length hit))
    reports;
  flush stdout

(* Exhaustiveness *)

(* Project coverage is defined over instrumented, linked code, so a
   source file can be absent from the merge for reasons the report
   cannot show: a library without the stanza, a module no test
   executable links, a test executable nobody ran since the rebuild.
   --expect names the sources that must be present; a missing one is
   a loud failure rather than a silently smaller denominator. *)

(* The recorded names and the walked paths on one footing: lexical
   components without "" and ".", and the basename's extension chain
   stripped at its first dot, so lib/calc.ml, ./lib/calc.ml, dune's
   lib/calc.pp.ml, and the lib/calc.mll a lexer is generated from are
   one stem. *)
let stem path =
  let components =
    String.split_on_char '/' (String.map (function '\\' -> '/' | c -> c) path)
    |> List.filter (fun c -> c <> "" && c <> ".")
  in
  match List.rev components with
  | [] -> ""
  | base :: rev_dirs ->
      let base =
        match String.index_opt base '.' with
        | Some i -> String.sub base 0 i
        | None -> base
      in
      String.concat "/" (List.rev (base :: rev_dirs))

let is_source name =
  List.exists (Filename.check_suffix name) [ ".ml"; ".mll"; ".mly" ]

(* Build and switch directories, and dot-directories, are never sources. *)
let skipped_dir name =
  name = "_build" || name = "_opam" || (name <> "" && name.[0] = '.')

let rec sources_under dir =
  match Sys.readdir dir with
  | exception Sys_error _ -> []
  | entries ->
      Array.to_list entries |> List.sort String.compare
      |> List.concat_map (fun entry ->
          let path = Filename.concat dir entry in
          match Sys.is_directory path with
          | true -> if skipped_dir entry then [] else sources_under path
          | false -> if is_source entry then [ path ] else []
          | exception Sys_error _ -> [])

(* A named path is a contract, like a PATH argument: it must exist. *)
let expand_expectation path =
  if not (Sys.file_exists path) then
    Error (spf "%s: no such file or directory" path)
  else if Sys.is_directory path then Ok (sources_under path)
  else Ok [ path ]

let expand_expectations paths =
  List.fold_left
    (fun acc path ->
      Result.bind acc (fun found ->
          Result.map (fun more -> found @ more) (expand_expectation path)))
    (Ok []) paths

(* The sources named by [expect], less those named by [do_not_expect],
   that [present] (the merge's recorded file names) does not cover. *)
let missing_expectations ~expect ~do_not_expect present =
  match (expand_expectations expect, expand_expectations do_not_expect) with
  | Error message, _ | _, Error message -> Error message
  | Ok expected, Ok excluded ->
      let excluded = List.map stem excluded
      and present = List.map stem present in
      Ok
        (expected
        |> List.filter (fun path ->
            let s = stem path in
            not (List.mem s excluded || List.mem s present))
        |> List.sort_uniq String.compare)

let check_expectations ~expect ~do_not_expect collection =
  if expect = [] then 0
  else
    match
      missing_expectations ~expect ~do_not_expect
        (Windtrap_runtime.Coverage.files collection)
    with
    | Error message ->
        Printf.eprintf "windtrap coverage: %s\n%!" message;
        1
    | Ok [] -> 0
    | Ok missing ->
        List.iter
          (fun path ->
            Printf.eprintf
              "windtrap coverage: %s: expected source has no coverage data \
               (not instrumented, or linked into no test executable that ran)\n\
               %!"
              path)
          missing;
        1

(* The command *)

let report_table ~source_roots ~show_uncovered collection =
  let ansi =
    Env.resolve_color (Env.color_mode ()) ~tty:(Env.is_tty_stdout ())
      ~inside_dune:(Env.inside_dune ()) ~term_dumb:(Env.term_dumb ())
  in
  let renderer = Render.create ~out:Format.std_formatter ~ansi () in
  (* The section data comes from the coverage seam's one builder, so this
     table and the in-process report modes cannot drift. *)
  Render.coverage_report renderer
    ~mode:(if show_uncovered then `Full else `Report)
    (Driver.coverage_data ~source_roots collection);
  Format.pp_print_flush Format.std_formatter ()

(* The gate compares raw percentages. The verdict states the threshold
   as given and, on failure, the measurement exactly as the report line
   states it — a fraction of integers beside its rounding — so no printed
   sentence carries a comparison its own digits can contradict. *)
let check_min ~machine summary = function
  | None -> 0
  | Some min ->
      let pct = Windtrap_runtime.Coverage.percentage summary in
      let print = if machine then Printf.eprintf else Printf.printf in
      if pct >= min then begin
        print "minimum %g%%: ok\n%!" min;
        0
      end
      else begin
        print "minimum %g%%: FAILED \u{2014} %.1f%% (%d/%d points)\n%!" min pct
          summary.Windtrap_runtime.Coverage.visited
          summary.Windtrap_runtime.Coverage.total;
        1
      end

let run args =
  match parse_args args with
  | Error `Help ->
      print_endline usage;
      0
  | Error (`Usage message) ->
      Printf.eprintf "windtrap coverage: %s\n%s\n" message usage;
      2
  | Ok options -> (
      match discover options.paths with
      | Error message ->
          Printf.eprintf "windtrap coverage: %s\n" message;
          1
      | Ok (files, source_roots) -> (
          if files = [] then begin
            prerr_string ("windtrap coverage: " ^ no_data);
            1
          end
          else
            match load_merged files with
            | Error code -> code
            | Ok collection ->
                if options.json then print_json ~source_roots collection
                else if options.lcov then print_lcov ~source_roots collection
                else
                  report_table ~source_roots
                    ~show_uncovered:options.show_uncovered collection;
                (* Both gates run, so one run names everything wrong;
                   either failing is exit 1. A machine format owns
                   stdout; the verdict moves aside. *)
                let expectations =
                  check_expectations ~expect:options.expect
                    ~do_not_expect:options.do_not_expect collection
                in
                let gate =
                  check_min
                    ~machine:(options.json || options.lcov)
                    (Windtrap_runtime.Coverage.summary collection)
                    options.min
                in
                max expectations gate))
