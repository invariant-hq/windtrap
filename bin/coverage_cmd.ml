(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   File discovery and the staleness pass live in Data_files, shared with
   `windtrap mutants`; the table and excerpt rendering live in the
   library's report sections (Report_sections, via Windtrap.Private) over
   section data this command builds from what the runtime measured. This
   is the one coverage reporter: a run prints no number of its own.
  ---------------------------------------------------------------------------*)

module Coverage = Windtrap_runtime.Coverage
module Sections = Windtrap.Private.Report_sections
module Os = Windtrap.Private.Os
module Cli = Windtrap.Private.Cli
module Pp = Windtrap.Private.Pp

let strf = Printf.sprintf
let ( let* ) = Result.bind

(* What standard output carries: the report, or a document. *)
type output = Report | Json | Lcov

type options = {
  min : float option;
  output : output;
  mode : [ `Report | `Full ];
  expect : string list;
  do_not_expect : string list;
  paths : string list;
}

(* A step of [run] that ends the command says why on standard error and is
   [Error] of the exit code. *)

(* Files *)

let no_data =
  "no .coverage files found\n\
   Instrument the library under test with ppx_windtrap.coverage and run its \
   tests first; every instrumented test executable writes its dump at exit, \
   under the build directory's _coverage or under _windtrap/coverage."

let all_excluded excluded =
  Data_files.all_excluded ~ext:"coverage" (List.map snd excluded)
  ^ "\n\
    \  They were written by executables that no longer exist or have been \
     rebuilt since.\n\
    \  The usual cause is a build without the instrumentation flag."

(* The remedy is in words: this command does not know how the suite is run. *)
let remedy =
  "re-run the suite instrumented (forcing the runs your build tool cached), \
   then merge again; delete the files whose executable no longer exists"

(* No flag keeps a dump of another build: a number computed from it can only
   mislead. *)
let merged files =
  let say_error error =
    Os.say (Pp.to_string Coverage.pp_error error);
    Error 1
  in
  let rec judge kept excluded = function
    | [] -> Ok (List.rev kept, List.rev excluded)
    | path :: paths -> (
        let* identity =
          Data_files.identity Coverage.format path
          |> Result.map_error (fun error -> Coverage.Data error)
        in
        match Data_files.freshness ~path identity with
        | Data_files.Fresh ->
            let* t, _ = Coverage.load path in
            judge (t :: kept) excluded paths
        | (Data_files.Orphan _ | Data_files.Stale _) as freshness ->
            judge kept ((path, freshness) :: excluded) paths)
  in
  let merge acc t =
    let* acc = acc in
    Coverage.merge acc t
  in
  match judge [] [] files with
  | Error error -> say_error error
  | Ok (kept, excluded) -> (
      List.iter Os.say (Data_files.warnings excluded);
      match (kept, excluded) with
      | [], [] ->
          Os.say no_data;
          Error 1
      | [], excluded ->
          Os.say (all_excluded excluded);
          Os.say remedy;
          Error 1
      | kept, excluded -> (
          if excluded <> [] then Os.say remedy;
          match List.fold_left merge (Ok Coverage.empty) kept with
          | Ok collection -> Ok collection
          | Error error -> say_error error))

(* Gates *)

(* A backslash separates as a slash does, as in a name recorded on Windows. *)
let stem path =
  let parts =
    String.split_on_char '/' (String.map (function '\\' -> '/' | c -> c) path)
    |> List.filter (fun part -> part <> "" && part <> ".")
  in
  match List.rev parts with
  | [] -> ""
  | base :: dirs ->
      let base =
        match String.index_opt base '.' with
        | Some i -> String.sub base 0 i
        | None -> base
      in
      String.concat "/" (List.rev (base :: dirs))

let rec sources_under dir =
  match Sys.readdir dir with
  | exception Sys_error _ -> []
  | entries ->
      Array.to_list entries |> List.sort String.compare
      |> List.concat_map (fun entry ->
          let path = Filename.concat dir entry in
          match Sys.is_directory path with
          | true
            when entry = "_build" || entry = "_opam"
                 || String.starts_with ~prefix:"." entry ->
              []
          | true -> sources_under path
          | false
            when List.exists
                   (Filename.check_suffix entry)
                   [ ".ml"; ".mll"; ".mly" ] ->
              [ path ]
          | false -> []
          | exception Sys_error _ -> [])

let sources paths =
  match List.find_opt (fun path -> not (Sys.file_exists path)) paths with
  | Some path -> Error (strf "%s: no such file or directory" path)
  | None ->
      Ok
        (List.concat_map
           (fun path ->
             if Sys.is_directory path then sources_under path else [ path ])
           paths)

(* The sources that [--expect] names, less those that [--do-not-expect] names,
   whose stem no recorded name has. *)
let missing_sources o collection =
  let* expected = sources o.expect in
  let* exempt = sources o.do_not_expect in
  let known = List.map stem (exempt @ Coverage.files collection) in
  Ok
    (List.sort_uniq String.compare
       (List.filter (fun path -> not (List.mem (stem path) known)) expected))

let expect_gate o collection =
  (* Without [--expect], a path of [--do-not-expect] need not exist. *)
  if o.expect = [] then 0
  else
    match missing_sources o collection with
    | Error message ->
        Os.say message;
        1
    | Ok missing ->
        List.iter
          (fun path ->
            Os.say
              (strf
                 "%s: expected source has no coverage data (not instrumented, \
                  or linked into no test executable that ran)"
                 path))
          missing;
        if missing = [] then 0 else 1

(* A document leaves the outcome line out, so the gate says it. *)
let min_gate o ({ visited; total } : Coverage.summary) =
  match o.min with
  | None -> 0
  | Some min ->
      (match o.output with
      | Report -> ()
      | Json | Lcov ->
          Os.say
            (Sections.render ~ansi:false
               (Sections.coverage_line ~min:(Some min) ~visited ~total)));
      if Sections.percent ~visited ~total >= min then 0 else 1

(* Reports and documents *)

let percentage ({ visited; total } : Coverage.summary) =
  Sections.percent ~visited ~total

let json_string s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (function
      | ('"' | '\\') as c ->
          Buffer.add_char b '\\';
          Buffer.add_char b c
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | c when c < ' ' -> Buffer.add_string b (strf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let json (summary : Coverage.summary) reports =
  let file (r : Coverage.file_report) =
    strf
      "\n\
      \    { \"path\": %s, \"visited\": %d, \"total\": %d,\n\
      \      \"percentage\": %.2f,\n\
      \      \"uncovered_lines\": [%s] }"
      (json_string r.file) r.summary.visited r.summary.total
      (percentage r.summary)
      (String.concat "," (List.map string_of_int r.uncovered_lines))
  in
  strf
    "{ \"summary\": { \"visited\": %d, \"total\": %d, \"percentage\": %.2f },\n\
    \  \"files\": [%s ] }\n"
    summary.visited summary.total (percentage summary)
    (String.concat "," (List.map file reports))

(* A tracefile has no escape syntax, so the name is written as recorded. *)
let lcov_record (r : Coverage.file_report) =
  let hit = List.filter (fun (_, hits) -> hits > 0) r.line_hits in
  strf "TN:\nSF:%s\n%sLF:%d\nLH:%d\nend_of_record\n" r.file
    (String.concat ""
       (List.map (fun (line, hits) -> strf "DA:%d,%d\n" line hits) r.line_hits))
    (List.length r.line_hits) (List.length hit)

let coverage_data (summary : Coverage.summary) reports : Sections.coverage =
  let file (r : Coverage.file_report) : Sections.coverage_file =
    {
      file = r.file;
      visited = r.summary.visited;
      total = r.summary.total;
      uncovered = r.uncovered_lines;
      source = r.source;
      stale = r.stale;
    }
  in
  {
    visited = summary.visited;
    total = summary.total;
    files = List.map file reports;
  }

let print o ~ansi (summary : Coverage.summary) reports =
  (match o.output with
  | Report ->
      Sections.print ~out:Format.std_formatter ~ansi
        (Sections.coverage_report ~mode:o.mode ~min:o.min
           (coverage_data summary reports))
  | Json -> print_string (json summary reports)
  | Lcov ->
      (* A file without its source has no record: its lines would paint code
         the data does not describe. *)
      List.iter
        (fun (r : Coverage.file_report) ->
          match r.source with
          | Some _ -> print_string (lcov_record r)
          | None ->
              Os.say
                (strf "%s: %s; omitted from the lcov output" r.file
                   (if r.stale then "the source changed since the run"
                    else "source not found")))
        reports);
  flush stdout

(* Running *)

let usage = "usage: windtrap coverage [OPTIONS] [PATH...]"

let help =
  "windtrap coverage - merge .coverage files and report\n\n" ^ usage
  ^ {|

Merges the .coverage files written by instrumented test executables and
reports expression coverage per source file. Without PATH arguments the files
are found under the build directory's _coverage (or _windtrap/coverage in a
tree built without one), walking up from the current directory to the
enclosing project root; PATH arguments (.coverage files, or directories
searched recursively) replace that default.

OPTIONS:
  --min=PCT
      Exit 1 when total coverage is below PCT.

  --json
      Machine-readable report on standard output.

  --lcov
      LCOV tracefile on standard output (genhtml, Codecov, Coveralls, GitLab,
      editor gutters).

  --expect=PATH
      Exit 1 unless every .ml/.mll/.mly under PATH (or PATH itself) has
      coverage data; repeatable.

  --do-not-expect=PATH
      Exempt PATH, a file or a directory, from --expect.

  -u, --show-uncovered
      Also render uncovered source excerpts.

  -h, --help
      Print this help and exit.

ENVIRONMENT (no flag):
  WINDTRAP_COLOR
      Color output: always, never or auto.|}

(* [--flag=value] is [--flag value] for a flag that takes a value. *)
let split_inline arg =
  match String.index_opt arg '=' with
  | Some i
    when List.mem (String.sub arg 0 i)
           [ "--min"; "--expect"; "--do-not-expect" ] ->
      [ String.sub arg 0 i; String.sub arg (i + 1) (String.length arg - i - 1) ]
  | Some _ | None -> [ arg ]

let options args =
  let usage_error message =
    Os.say message;
    prerr_endline usage;
    Error 2
  in
  let rec parse o = function
    | [] ->
        Ok
          {
            o with
            expect = List.rev o.expect;
            do_not_expect = List.rev o.do_not_expect;
            paths = List.rev o.paths;
          }
    | ("-h" | "--help" | "-help") :: _ ->
        print_endline help;
        Error 0
    | "--min" :: value :: args -> (
        match float_of_string_opt value with
        | Some min when 0. <= min && min <= 100. ->
            parse { o with min = Some min } args
        | Some _ | None ->
            usage_error
              (strf
                 "invalid value '%s' for --min: expected a percentage (0-100)"
                 value))
    | "--expect" :: path :: args ->
        parse { o with expect = path :: o.expect } args
    | "--do-not-expect" :: path :: args ->
        parse { o with do_not_expect = path :: o.do_not_expect } args
    | [ (("--min" | "--expect" | "--do-not-expect") as flag) ] ->
        usage_error (strf "option '%s' requires an argument" flag)
    | (("--json" | "--lcov") as flag) :: args ->
        let output = if flag = "--json" then Json else Lcov in
        if o.output <> Report && o.output <> output then
          usage_error "--json and --lcov each own standard output; pick one"
        else parse { o with output } args
    | ("-u" | "--show-uncovered") :: args -> parse { o with mode = `Full } args
    | arg :: _ when String.starts_with ~prefix:"-" arg ->
        usage_error (strf "unknown option '%s'" arg)
    | path :: args -> parse { o with paths = path :: o.paths } args
  in
  parse
    {
      min = None;
      output = Report;
      mode = `Report;
      expect = [];
      do_not_expect = [];
      paths = [];
    }
    (List.concat_map split_inline args)

let ansi = function
  | Json | Lcov -> Ok false
  | Report -> (
      match Cli.color_mode () with
      | Error error ->
          Os.say (Cli.error_message error);
          Error 2
      | Ok color ->
          Ok
            (Os.resolve_color color ~tty:(Os.is_tty_stdout ())
               ~inside_dune:(Os.inside_dune ()) ~term_dumb:(Os.term_dumb ())))

let run args =
  let code =
    let* o = options args in
    let* ansi = ansi o.output in
    let* files, source_roots =
      match Data_files.discover Coverage.format o.paths with
      | Ok found -> Ok found
      | Error message ->
          Os.say message;
          Error 1
    in
    let* collection = merged files in
    let summary = Coverage.summary collection in
    print o ~ansi summary (Coverage.file_reports ~source_roots collection);
    let expected = expect_gate o collection in
    let met = min_gate o summary in
    Ok (max expected met)
  in
  match code with Ok code | Error code -> code
