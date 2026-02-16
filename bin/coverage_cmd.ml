(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* ───── File Discovery ───── *)

let is_coverage_file filename = Filename.check_suffix filename ".coverage"
let sort_unique xs = List.sort_uniq String.compare xs

let list_recursively filter directory =
  let rec aux directory acc =
    Sys.readdir directory
    |> Array.fold_left
         (fun acc entry ->
           let path = Filename.concat directory entry in
           match Sys.is_directory path with
           | true -> aux path acc
           | false -> if filter entry then path :: acc else acc
           | exception Sys_error _ -> acc)
         acc
  in
  aux directory []

let list_non_recursively filter directory =
  Sys.readdir directory |> Array.to_list
  |> List.filter_map (fun entry ->
      if filter entry then Some (Filename.concat directory entry) else None)

let find_coverage_files = function
  | [] ->
      let in_cwd =
        list_non_recursively is_coverage_file Filename.current_dir_name
      in
      let in_build =
        if
          Sys.file_exists "_build/_coverage"
          && Sys.is_directory "_build/_coverage"
        then list_recursively is_coverage_file "_build/_coverage"
        else []
      in
      sort_unique (in_cwd @ in_build)
  | paths ->
      paths
      |> List.concat_map (fun path ->
          if Sys.file_exists path then
            match Sys.is_directory path with
            | true -> list_recursively is_coverage_file path
            | false -> if is_coverage_file path then [ path ] else []
            | exception Sys_error _ -> []
          else [])
      |> sort_unique

let load_coverage files =
  let tbl : Windtrap_coverage.coverage = Hashtbl.create 17 in
  List.iter
    (fun path -> Windtrap_coverage.merge tbl (Windtrap_coverage.read_file path))
    files;
  tbl

(* ───── JSON Output ───── *)

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
          let code = Char.code c in
          if code < 32 then
            Buffer.add_string buffer (Printf.sprintf "\\u%04x" code)
          else Buffer.add_char buffer c)
    s;
  Buffer.contents buffer

let json_int_list values =
  values |> List.map string_of_int |> String.concat "," |> Printf.sprintf "[%s]"

let print_json ~skip_covered ~source_paths coverage =
  let overall = Windtrap_coverage.summarize coverage in
  let reports = Windtrap_coverage.reports ~source_paths coverage in
  let reports =
    if skip_covered then
      List.filter
        (fun report ->
          report.Windtrap_coverage.summary.visited
          < report.Windtrap_coverage.summary.total)
        reports
    else reports
  in
  Printf.printf "{\n";
  Printf.printf
    "  \"summary\": {\"visited\": %d, \"total\": %d, \"percentage\": %.2f},\n"
    overall.visited overall.total
    (Windtrap_coverage.percentage overall);
  Printf.printf "  \"files\": [\n";
  let last_index = List.length reports - 1 in
  reports
  |> List.iteri (fun index report ->
      let suffix = if index = last_index then "" else "," in
      let summary = report.Windtrap_coverage.summary in
      Printf.printf
        "    {\"path\": \"%s\", \"visited\": %d, \"total\": %d, \
         \"percentage\": %.2f, \"source_available\": %s, \
         \"uncovered_offsets\": %s, \"uncovered_lines\": %s}%s\n"
        (json_escape report.Windtrap_coverage.filename)
        summary.visited summary.total
        (Windtrap_coverage.percentage summary)
        (if report.Windtrap_coverage.source_available then "true" else "false")
        (json_int_list report.Windtrap_coverage.uncovered_offsets)
        (json_int_list report.Windtrap_coverage.uncovered_lines)
        suffix);
  Printf.printf "  ]\n}\n%!"

(* ───── CLI ───── *)

let help =
  {|windtrap coverage - coverage reporting

USAGE:
    windtrap coverage [OPTIONS] [PATH...]

DISPLAY:
    -u, --show-uncovered  Show uncovered source code snippets with context
    -C, --context N       Context lines around uncovered regions (default: 1)
    --skip-covered        Hide files with 100% coverage
    --summary-only        Show only the one-line coverage summary

OUTPUT:
    -j, --json            Machine-readable JSON report

PATHS:
    --coverage-path PATH  Search for .coverage files (repeatable)
    --source-path PATH    Source root for line mapping (repeatable)
    PATH...               Positional arguments treated as coverage paths

    -h, --help            Show this help|}

let split_equals args =
  List.concat_map
    (fun arg ->
      match String.index_opt arg '=' with
      | Some i when i > 0 && arg.[0] = '-' ->
          [
            String.sub arg 0 i;
            String.sub arg (i + 1) (String.length arg - i - 1);
          ]
      | _ -> [ arg ])
    args

let run args =
  let args = split_equals args in
  let show_uncovered = ref false in
  let context = ref 1 in
  let skip_covered = ref false in
  let summary_only = ref false in
  let json = ref false in
  let coverage_paths = ref [] in
  let source_paths = ref [] in
  let rec parse = function
    | [] -> ()
    | ("-u" | "--show-uncovered") :: rest ->
        show_uncovered := true;
        parse rest
    | ("-C" | "--context") :: n :: rest -> (
        match int_of_string_opt n with
        | Some v when v >= 0 ->
            context := v;
            parse rest
        | _ ->
            Printf.eprintf "Error: --context requires a non-negative integer\n";
            exit 1)
    | ("-C" | "--context") :: [] ->
        Printf.eprintf "Error: --context requires an argument\n";
        exit 1
    | "--skip-covered" :: rest ->
        skip_covered := true;
        parse rest
    | "--summary-only" :: rest ->
        summary_only := true;
        parse rest
    | ("-j" | "--json") :: rest ->
        json := true;
        parse rest
    | "--coverage-path" :: path :: rest ->
        coverage_paths := path :: !coverage_paths;
        parse rest
    | "--coverage-path" :: [] ->
        Printf.eprintf "Error: --coverage-path requires an argument\n";
        exit 1
    | "--source-path" :: path :: rest ->
        source_paths := path :: !source_paths;
        parse rest
    | "--source-path" :: [] ->
        Printf.eprintf "Error: --source-path requires an argument\n";
        exit 1
    | ("-h" | "--help" | "-help") :: _ ->
        print_endline help;
        exit 0
    | arg :: _ when String.length arg > 0 && arg.[0] = '-' ->
        Printf.eprintf "Unknown option: %s\n" arg;
        Printf.eprintf "Try --help for usage information.\n";
        exit 1
    | path :: rest ->
        coverage_paths := path :: !coverage_paths;
        parse rest
  in
  parse args;
  let files = find_coverage_files (List.rev !coverage_paths) in
  if files = [] then begin
    Printf.eprintf
      "Error: no *.coverage files found\n\
       Hint: Add (instrumentation (backend ppx_windtrap)) to your dune-project\n\
       and run: dune runtest --instrument-with ppx_windtrap\n";
    exit 1
  end;
  let coverage = load_coverage files in
  let source_paths =
    match List.rev !source_paths with [] -> [ "." ] | paths -> paths
  in
  if !json then print_json ~skip_covered:!skip_covered ~source_paths coverage
  else if !show_uncovered then
    Windtrap_coverage.print_uncovered ~context:!context ~source_paths coverage
  else
    Windtrap_coverage.print_summary ~per_file:(not !summary_only)
      ~skip_covered:!skip_covered ~source_paths coverage
