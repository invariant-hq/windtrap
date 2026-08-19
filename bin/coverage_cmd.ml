(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   File discovery and the staleness pass live in Data_files, shared with
   `windtrap mutate`; the table and excerpt rendering live in the
   library renderer (Render, via Windtrap.Private) over section data
   built by the coverage seam's one builder (Driver.coverage_data), so
   the in-process WINDTRAP_COVERAGE modes and this command share one
   layout and one projection.
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

(* Discovery: Data_files's, shared with `windtrap mutate` — the project
   root resolved as the runtime resolves its dump path, explicit PATH
   arguments as a loud contract (a silent narrowing of the merge would
   end in the no-data message and its wrong remedy). [files] come back
   sorted for deterministic merge order and error attribution; [roots]
   are the source roots for line mapping. *)

let discover paths = Data_files.discover ~dir:"_coverage" ~ext:"coverage" paths

(* The staleness pass

   The @cover alias forces every stanza up to date before the aggregate
   runs, but "up to date" is not "re-run": the holes are dumps whose
   executable was deleted or renamed (orphans, which would silently
   inflate the merge) and dumps not written by the executable now on
   disk — a re-run without --instrument-with (rebuilds the executable,
   writes no fresh dump), or a test action dune replayed from cache
   after sources reverted to an already-tested state (the dump on disk
   stays a different build's, and plain re-runs stay cache hits, so only
   a forced run heals it — hence the remedy below). Detection is
   Data_files.freshness's, from the identity recorded in each dump; the
   --stale policy and the wording of the remedies are this command's. *)

let stale_hint =
  "a re-run made without --instrument-with ppx_windtrap.coverage, or a cached test \
   dune did not re-run"

(* Loads [files], applies the --stale policy, and merges the survivors.
   Warnings and failure details go to stderr; [Error code] is the exit
   code (the matrix is unchanged: data problems are 1). *)
let load_merged ~stale files =
  let loaded =
    List.fold_left
      (fun acc path ->
        Result.bind acc (fun entries ->
            Result.map
              (fun (t, exe) ->
                (path, t, Data_files.freshness ~path exe) :: entries)
              (Windtrap_coverage.load path)))
      (Ok []) files
  in
  match loaded with
  | Error error ->
      Format.eprintf "windtrap coverage: %a@." Windtrap_coverage.pp_error error;
      Error 1
  | Ok entries -> (
      let entries = List.rev entries in
      let flagged =
        List.filter (fun (_, _, v) -> v <> Data_files.Fresh) entries
      in
      let kept =
        match stale with
        | Include | Fail -> entries
        | Exclude ->
            List.filter (fun (_, _, v) -> v = Data_files.Fresh) entries
      in
      (* Per-file detail is what a reader wants when a dump or two is
         stale among many: it names the executable and the reason, and
         the reader goes and looks. Forty of them is the same sentence
         forty times, and it buries the one fact that matters — that no
         instrumented run has happened since this build. So the detail
         is capped; the [kept = []] branch below adds the summary and
         the remedy, which is the case a reader reaches by simply
         forgetting the instrumentation flag. *)
      let detail_cap = 3 in
      let flagged_count = List.length flagged in
      List.iteri
        (fun i (path, _, v) ->
          if i < detail_cap then
            let action =
              match stale with
              | Exclude -> "excluding it (--stale=include overrides)"
              | Include -> "including it anyway (--stale=include)"
              | Fail -> "failing (--stale=fail)"
            in
            Printf.eprintf "windtrap coverage: %s; %s\n%!"
              (Data_files.describe ~stale_hint ~path v)
              action)
        flagged;
      if flagged_count > detail_cap then
        Printf.eprintf "windtrap coverage: ... and %d more like that\n%!"
          (flagged_count - detail_cap);
      let any_stale =
        List.exists
          (fun (_, _, v) ->
            match v with Data_files.Stale _ -> true | _ -> false)
          flagged
      in
      (* A plain re-run cannot heal a stale dump whose test action is a
         dune cache hit; the forced run always rewrites it. Skipped when
         everything is excluded: the epilogue below carries the same
         command. *)
      if any_stale && kept <> [] then
        Printf.eprintf
          "windtrap coverage: a forced run rewrites stale dumps: dune build \
           @cover --force --instrument-with ppx_windtrap.coverage\n\
           %!";
      if stale = Fail && flagged <> [] then Error 1
      else if kept = [] then begin
        let orphans =
          List.length
            (List.filter
               (fun (_, _, v) ->
                 match v with Data_files.Orphan _ -> true | _ -> false)
               flagged)
        in
        let total = List.length flagged in
        (* One sentence, not one per file. The commonest way to arrive
           here is not a subtle staleness problem at all — it is running
           the aggregate without the instrumentation flag, so the dumps
           on disk describe binaries the current build replaced. Lead
           with the remedy for that. *)
        Printf.eprintf
          "windtrap coverage: found %d .coverage file%s and every one is %s\n\
          \  They were written by executables that no longer exist or have \
           been rebuilt since.\n\
          \  The usual cause is a build without the instrumentation flag.\n\
           Re-run the instrumented tests, naming the backend your \
           (instrumentation) stanza uses:\n\
          \  dune build @cover --force --instrument-with ppx_windtrap.coverage\n\
           (--stale=include reads them anyway; dune clean removes leftovers \
           of deleted executables.)\n\
           %!"
          total
          (if total = 1 then "" else "s")
          (if orphans = total then "orphaned"
           else if orphans = 0 then "stale"
           else Printf.sprintf "stale or orphaned (%d orphaned)" orphans);
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

(* The CI artifact: summary + per-file visited/total/percentage +
   uncovered lines. Frozen keys; a file whose source is missing or stale
   reports an empty uncovered list. *)
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
        \      \"uncovered_lines\": %s }"
        (if i = 0 then "" else ",")
        (json_escape r.file) r.summary.visited r.summary.total
        (Windtrap_coverage.percentage r.summary)
        (json_ints r.uncovered_lines))
    reports;
  Printf.printf " ] }\n%!"

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
              \  (instrumentation (backend ppx_windtrap.coverage))\n\
               and run its tests first:\n\
              \  dune runtest --instrument-with ppx_windtrap.coverage\n";
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
