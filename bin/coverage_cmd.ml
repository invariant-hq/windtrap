(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* ───── File Discovery ───── *)

let is_coverage_file filename = Filename.check_suffix filename ".coverage"

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
        if Sys.file_exists "_build/_coverage"
           && Sys.is_directory "_build/_coverage"
        then list_recursively is_coverage_file "_build/_coverage"
        else []
      in
      in_cwd @ in_build
  | paths ->
      paths
      |> List.filter (fun p -> Sys.file_exists p && Sys.is_directory p)
      |> List.concat_map (list_recursively is_coverage_file)

let load_coverage files =
  let tbl : Windtrap_coverage.coverage = Hashtbl.create 17 in
  List.iter
    (fun path ->
      Windtrap_coverage.merge tbl (Windtrap_coverage.read_file path))
    files;
  tbl

(* ───── CLI ───── *)

let usage =
  {|Usage: windtrap coverage [options]

Options:
  --per-file        Show per-file breakdown
  --coverage-path   Directory to search for .coverage files (repeatable)
  --help            Show this help message|}

let run args =
  let per_file = ref false in
  let coverage_paths = ref [] in
  let rec parse = function
    | [] -> ()
    | "--per-file" :: rest ->
        per_file := true;
        parse rest
    | "--coverage-path" :: path :: rest ->
        coverage_paths := path :: !coverage_paths;
        parse rest
    | ("--help" | "-help" | "-h") :: _ ->
        print_endline usage;
        exit 0
    | arg :: _ ->
        Printf.eprintf "Error: unknown argument '%s'\n\n%s\n" arg usage;
        exit 1
  in
  parse args;
  let files = find_coverage_files (List.rev !coverage_paths) in
  if files = [] then begin
    Printf.eprintf
      "Error: no *.coverage files found\n\
       Hint: Add (instrumentation (backend ppx_windtrap --coverage)) to your\n\
       dune-project and run: dune runtest --instrument-with ppx_windtrap\n";
    exit 1
  end;
  let coverage = load_coverage files in
  Windtrap_coverage.print_summary ~per_file:!per_file coverage
