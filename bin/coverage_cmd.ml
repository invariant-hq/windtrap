(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   File discovery adapts windtrap v1's bin/coverage_cmd.ml; the table and
   excerpt rendering moved into the library renderer (Render, via
   Windtrap.Private) so the in-process WINDTRAP_COVERAGE modes and this
   command share one layout.
  ---------------------------------------------------------------------------*)

module Render = Windtrap.Private.Render
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
  --stale MODE          Dumps whose executable is gone or was rebuilt since:
                        exclude (default; warns), include, or fail
  --json                Machine-readable report on standard output
  -u, --show-uncovered  Also render uncovered source excerpts
  -h, --help            Print this help and exit|}

(* Flags *)

type stale_mode = Exclude | Include | Fail

type options = {
  min : float option;
  stale : stale_mode;
  json : bool;
  show_uncovered : bool;
  paths : string list;
}

let min_of_string value =
  match float_of_string_opt value with
  | Some pct when Float.is_finite pct && 0. <= pct && pct <= 100. -> Some pct
  | _ -> None

let stale_of_string = function
  | "exclude" -> Some Exclude
  | "include" -> Some Include
  | "fail" -> Some Fail
  | _ -> None

let parse_args args =
  let min_error value =
    Error
      (`Usage
         (spf "invalid value '%s' for --min: expected a percentage (0-100)"
            value))
  in
  let stale_error value =
    Error
      (`Usage
         (spf
            "invalid value '%s' for --stale: expected include, exclude or fail"
            value))
  in
  let rec go acc = function
    | [] -> Ok { acc with paths = List.rev acc.paths }
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
    | "--stale" :: value :: rest -> (
        match stale_of_string value with
        | Some stale -> go { acc with stale } rest
        | None -> stale_error value)
    | [ "--stale" ] -> Error (`Usage "option '--stale' requires an argument")
    | arg :: rest when String.length arg >= 8 && String.sub arg 0 8 = "--stale="
      -> (
        let value = String.sub arg 8 (String.length arg - 8) in
        match stale_of_string value with
        | Some stale -> go { acc with stale } rest
        | None -> stale_error value)
    | "--json" :: rest -> go { acc with json = true } rest
    | ("-u" | "--show-uncovered") :: rest ->
        go { acc with show_uncovered = true } rest
    | arg :: _ when String.length arg > 0 && arg.[0] = '-' ->
        Error (`Usage (spf "unknown option '%s'" arg))
    | path :: rest -> go { acc with paths = path :: acc.paths } rest
  in
  go
    {
      min = None;
      stale = Exclude;
      json = false;
      show_uncovered = false;
      paths = [];
    }
    args

(* Discovery *)

let coverage_dir_of root =
  Filename.concat (Filename.concat root "_build") "_coverage"

(* Ancestor-scan fallback: the nearest ancestor (the current directory
   included) with a _build/_coverage directory — `dune exec windtrap`
   runs from wherever the user is in the checkout. Candidates inside a
   sandbox are never roots: planted garbage under _build/.sandbox must
   not capture the scan. *)
let under_sandbox dir =
  String.map (function '\\' -> '/' | c -> c) dir
  |> String.split_on_char '/' |> List.mem ".sandbox"

let rec find_project_root dir =
  let candidate = coverage_dir_of dir in
  if
    (not (under_sandbox dir))
    && Sys.file_exists candidate && Sys.is_directory candidate
  then Some dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then None else find_project_root parent

(* The root rule, shared with the runtime's dump path: when the
   current directory is inside a _build — a dune rule action, sandboxed
   or not — the root is the parent of the topmost _build component,
   unconditionally; only outside _build does the ancestor scan run. *)
let project_root cwd =
  match Windtrap_coverage.build_root ~path:cwd with
  | Some root -> Some root
  | None -> find_project_root cwd

let is_coverage_file path = Filename.check_suffix path ".coverage"

let rec files_under dir =
  match Sys.readdir dir with
  | exception Sys_error _ -> []
  | entries ->
      Array.fold_left
        (fun acc entry ->
          let path = Filename.concat dir entry in
          match Sys.is_directory path with
          | true -> files_under path @ acc
          | false -> if is_coverage_file path then path :: acc else acc
          | exception Sys_error _ -> acc)
        [] entries

(* Explicit arguments are a contract: a named file must exist and carry
   the .coverage suffix — a typo'd path, a wrong glob in CI, or a
   renamed dump is a loud error, never a silent narrowing of the merge
   (which would end in the no-data message and its wrong remedy).
   Directories keep the scan's tolerance: they contribute however many
   .coverage files they contain. *)
let expand_path path =
  if not (Sys.file_exists path) then
    Error (spf "%s: no such file or directory" path)
  else if Sys.is_directory path then Ok (files_under path)
  else if is_coverage_file path then Ok [ path ]
  else Error (spf "%s: not a .coverage file" path)

(* [files] sorted for deterministic merge order and error attribution;
   [roots] are the source roots for line mapping. *)
let discover = function
  | [] ->
      Ok
        (match project_root (Sys.getcwd ()) with
        | None -> ([], [ "." ])
        | Some root ->
            ( List.sort_uniq String.compare (files_under (coverage_dir_of root)),
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

   The @cover alias forces every stanza up to date before the aggregate
   runs, but "up to date" is not "re-run": the holes are dumps whose
   executable was deleted or renamed (orphans, which would silently
   inflate the merge) and dumps not written by the executable now on
   disk — a re-run without --instrument-with (rebuilds the executable,
   writes no fresh dump), or a test action dune replayed from cache
   after sources reverted to an already-tested state (the dump on disk
   stays a different build's, and plain re-runs stay cache hits, so only
   a forced run heals it — hence the remedy below). All are detected
   from the identity recorded in each dump; the digest comparison is
   content-based because mtimes prove nothing here: dune's shared cache
   restores rebuilt artifacts with their original timestamps. Dumps
   without an identity (hand-written or merged files) are never
   flagged. *)

type verdict = Fresh | Orphan of string | Stale of string

let verdict ~path identity =
  match (identity : Windtrap_coverage.identity option) with
  | None -> Fresh
  | Some { exe; digest } -> (
      let resolved =
        if not (Filename.is_relative exe) then Some exe
        else
          (* A relative identity is a path below _build; the dump's own
             topmost-_build root locates that _build — the same root
             whether the dump was discovered or named on the command
             line. *)
          match Windtrap_coverage.build_root ~path with
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
         --instrument-with ppx_windtrap, or a cached test dune did not re-run"
        path exe

(* Loads [files], applies the --stale policy, and merges the survivors.
   Warnings and failure details go to stderr; [Error code] is the exit
   code (the matrix is unchanged: data problems are 1). *)
let load_merged ~stale files =
  let loaded =
    List.fold_left
      (fun acc path ->
        Result.bind acc (fun entries ->
            Result.map
              (fun (t, exe) -> (path, t, verdict ~path exe) :: entries)
              (Windtrap_coverage.load path)))
      (Ok []) files
  in
  match loaded with
  | Error error ->
      Format.eprintf "windtrap coverage: %a@." Windtrap_coverage.pp_error error;
      Error 1
  | Ok entries -> (
      let entries = List.rev entries in
      let flagged = List.filter (fun (_, _, v) -> v <> Fresh) entries in
      let kept =
        match stale with
        | Include | Fail -> entries
        | Exclude -> List.filter (fun (_, _, v) -> v = Fresh) entries
      in
      List.iter
        (fun (path, _, v) ->
          let action =
            match stale with
            | Exclude -> "excluding it (--stale=include overrides)"
            | Include -> "including it anyway (--stale=include)"
            | Fail -> "failing (--stale=fail)"
          in
          Printf.eprintf "windtrap coverage: %s; %s\n%!" (describe ~path v)
            action)
        flagged;
      let any_stale =
        List.exists
          (fun (_, _, v) -> match v with Stale _ -> true | _ -> false)
          flagged
      in
      (* A plain re-run cannot heal a stale dump whose test action is a
         dune cache hit; the forced run always rewrites it. Skipped when
         everything is excluded: the epilogue below carries the same
         command. *)
      if any_stale && kept <> [] then
        Printf.eprintf
          "windtrap coverage: a forced run rewrites stale dumps: dune build \
           @cover --force --instrument-with ppx_windtrap\n\
           %!";
      if stale = Fail && flagged <> [] then Error 1
      else if kept = [] then begin
        Printf.eprintf
          "windtrap coverage: every .coverage file is orphaned or stale\n\
           Re-run the instrumented tests:\n\
          \  dune build @cover --force --instrument-with ppx_windtrap\n\
           and delete leftovers of removed executables (or dune clean).\n";
        Error 1
      end
      else
        match
          List.fold_left
            (fun acc (_, t, _) ->
              Result.bind acc (fun acc -> Windtrap_coverage.merge acc t))
            (Ok Windtrap_coverage.empty) kept
        with
        | Ok collection -> Ok collection
        | Error error ->
            Format.eprintf "windtrap coverage: %a@." Windtrap_coverage.pp_error
              error;
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

let json_ranges ranges =
  spf "[%s]"
    (String.concat "," (List.map (fun (a, b) -> spf "[%d,%d]" a b) ranges))

(* The CI artifact: summary + per-file visited/total/
   percentage + uncovered lines and ranges. Frozen keys; a file whose
   source is missing or stale reports empty uncovered arrays. *)
let print_json ~source_roots collection =
  let summary = Windtrap_coverage.summary collection in
  let reports = Windtrap_coverage.file_reports ~source_roots collection in
  Printf.printf
    "{ \"summary\": { \"visited\": %d, \"total\": %d, \"percentage\": %.2f },\n\
    \  \"files\": ["
    summary.visited summary.total
    (Windtrap_coverage.percentage summary);
  List.iteri
    (fun i (r : Windtrap_coverage.file_report) ->
      Printf.printf
        "%s\n\
        \    { \"path\": \"%s\", \"visited\": %d, \"total\": %d,\n\
        \      \"percentage\": %.2f,\n\
        \      \"uncovered_lines\": %s,\n\
        \      \"uncovered_ranges\": %s }"
        (if i = 0 then "" else ",")
        (json_escape r.file) r.summary.visited r.summary.total
        (Windtrap_coverage.percentage r.summary)
        (json_ints r.uncovered_lines)
        (json_ranges (Windtrap_coverage.collapse_ranges r.uncovered_lines)))
    reports;
  Printf.printf " ] }\n%!"

(* The command *)

let report_table ~source_roots ~show_uncovered collection =
  let ansi =
    Env.resolve_color (Env.color_mode ()) ~tty:(Env.is_tty_stdout ())
      ~inside_dune:(Env.inside_dune ()) ~term_dumb:(Env.term_dumb ())
  in
  let renderer = Render.create ~out:Format.std_formatter ~ansi () in
  Render.coverage_report renderer ~source_roots
    ~mode:(if show_uncovered then `Full else `Report)
    collection;
  Format.pp_print_flush Format.std_formatter ()

(* The gate compares raw values; the verdict prints decimal renderings,
   and a printed "A% is below B%" is true exactly when A < B as printed.
   So the threshold prints with the fewest decimals (one at least) that
   parse back to the exact value the gate compared — --min 72.24 prints
   72.24, never a rounded 72.2 — and a failing percentage gains decimals
   until its rendering is numerically below that threshold. %.17g is the
   always-round-trips last resort for values no short rendering
   reaches. *)
let precisions = [ 1; 2; 4; 6; 9 ]

let shown_min min =
  let rec go = function
    | [] -> spf "%.17g" min
    | digits :: rest ->
        let s = spf "%.*f" digits min in
        if float_of_string s = min then s else go rest
  in
  go precisions

let shown_pct ~min pct =
  let rec go = function
    | [] -> spf "%.17g" pct
    | digits :: rest ->
        let s = spf "%.*f" digits pct in
        if float_of_string s < min then s else go rest
  in
  go precisions

let check_min ~json summary = function
  | None -> 0
  | Some min ->
      let pct = Windtrap_coverage.percentage summary in
      let print = if json then Printf.eprintf else Printf.printf in
      if pct >= min then begin
        print "minimum %s%%: ok\n%!" (shown_min min);
        0
      end
      else begin
        print "minimum %s%%: FAILED \u{2014} %s%% is below %s%%\n%!"
          (shown_min min) (shown_pct ~min pct) (shown_min min);
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
            Printf.eprintf
              "windtrap coverage: no .coverage files found\n\
               Instrument the library under test\n\
              \  (instrumentation (backend ppx_windtrap))\n\
               and run its tests first:\n\
              \  dune runtest --instrument-with ppx_windtrap\n";
            1
          end
          else
            match load_merged ~stale:options.stale files with
            | Error code -> code
            | Ok collection ->
                if options.json then print_json ~source_roots collection
                else
                  report_table ~source_roots
                    ~show_uncovered:options.show_uncovered collection;
                check_min ~json:options.json
                  (Windtrap_coverage.summary collection)
                  options.min))
