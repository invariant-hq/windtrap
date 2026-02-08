(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* ───── Types ───── *)

type t = {
  stream : bool option;
  format : string option;
  bail : int option;
  quick : bool option;
  filter : string option;
  exclude : string option;
  failed : bool option;
  list_only : bool option;
  output_dir : string option;
  update : bool option;
  junit : string option;
  seed : int option;
  timeout : float option;
  prop_count : int option;
  color : string option;
  tags : string list;
  exclude_tags : string list;
}

let empty =
  {
    stream = None;
    format = None;
    bail = None;
    quick = None;
    filter = None;
    exclude = None;
    failed = None;
    list_only = None;
    output_dir = None;
    update = None;
    junit = None;
    seed = None;
    timeout = None;
    prop_count = None;
    color = None;
    tags = [];
    exclude_tags = [];
  }

let version = "0.1.0"

(* ───── Help And Parsing ───── *)

let print_help prog_name =
  let name = Filename.basename prog_name in
  Pp.pr "@[<v>%s - windtrap test runner@,@," name;
  Pp.pr "USAGE:@,";
  Pp.pr "    %s [OPTIONS] [PATTERN]@,@," name;
  Pp.pr "COMMON OPTIONS:@,";
  Pp.pr "    -s, --stream           Stream test output (don't capture)@,";
  Pp.pr
    "    -v, --verbose          Verbose output (alias for --format verbose)@,";
  Pp.pr "    -q, --quick            Skip slow tests@,";
  Pp.pr
    "    -x, --fail-fast        Stop on first failure (alias for --bail 1)@,";
  Pp.pr "    --bail N               Stop after N failures@,";
  Pp.pr "    --failed               Rerun only tests that failed last time@,";
  Pp.pr "    -l, --list             List tests without running@,";
  Pp.pr "    -u, --update           Update snapshots@,";
  Pp.pr "    -f, --filter PATTERN   Filter tests by name@,";
  Pp.pr "    -e, --exclude PATTERN  Exclude tests by name@,";
  Pp.pr
    "    --tag LABEL            Run only tests with this label (repeatable)@,";
  Pp.pr "    --exclude-tag LABEL    Skip tests with this label (repeatable)@,@,";
  Pp.pr "OUTPUT:@,";
  Pp.pr "    --format FMT           Output: verbose, compact, tap, junit@,";
  Pp.pr "    --junit PATH           Write JUnit XML to file@,@,";
  Pp.pr "OTHER:@,";
  Pp.pr "    --seed N               Random seed for property tests@,";
  Pp.pr "    --timeout N            Default timeout in seconds@,";
  Pp.pr
    "    --prop-count N         Number of property test cases (default: 100)@,";
  Pp.pr
    "    --color MODE           Color output: always, never, auto (default: \
     auto)@,";
  Pp.pr "    -o, --output DIR       Directory for test logs@,";
  Pp.pr "    -V, --version          Show version and exit@,";
  Pp.pr "    -h, --help             Show this help@,@,";
  Pp.pr "ENVIRONMENT VARIABLES:@,";
  Pp.pr "    WINDTRAP_STREAM        Stream output (1/true/yes)@,";
  Pp.pr "    WINDTRAP_FORMAT        Output format (verbose/compact/tap/junit)@,";
  Pp.pr "    WINDTRAP_FILTER        Filter pattern@,";
  Pp.pr "    WINDTRAP_EXCLUDE       Exclude pattern@,";
  Pp.pr "    WINDTRAP_UPDATE        Update snapshots (1/true/yes)@,";
  Pp.pr "    WINDTRAP_SEED          Random seed for property tests@,";
  Pp.pr "    WINDTRAP_PROP_COUNT    Number of property test cases@,";
  Pp.pr "    WINDTRAP_TIMEOUT       Default timeout in seconds@,";
  Pp.pr "    WINDTRAP_COLOR         Color output (always/never/auto)@,";
  Pp.pr "    WINDTRAP_TAG           Required tags (comma-separated)@,";
  Pp.pr "    WINDTRAP_EXCLUDE_TAG   Excluded tags (comma-separated)@,@,";
  Pp.pr "SNAPSHOT ENVIRONMENT VARIABLES:@,";
  Pp.pr "    WINDTRAP_SNAPSHOT_DIR           Centralized snapshot directory@,";
  Pp.pr
    "    WINDTRAP_SNAPSHOT_DIFF_CONTEXT  Context lines in diffs (default: 3)@,";
  Pp.pr "    WINDTRAP_SNAPSHOT_MAX_BYTES     Truncation limit for snapshots@,";
  Pp.pr
    "    WINDTRAP_SNAPSHOT_REPORT        Report snapshot updates (1/true/yes)@,";
  Pp.pr "    WINDTRAP_PROJECT_ROOT          Project root for path resolution@,";
  Pp.pr "    WINDTRAP_COLUMNS               Override terminal width@,";
  Pp.pr "    WINDTRAP_TAIL_ERRORS           Max lines shown per failure@,";
  Pp.pr "@]%!"

let rec parse_args acc = function
  | [] -> acc
  (* Common options *)
  | ("-s" | "--stream") :: rest ->
      parse_args { acc with stream = Some true } rest
  | ("-v" | "--verbose") :: rest ->
      parse_args { acc with format = Some "verbose" } rest
  | ("-q" | "--quick") :: rest -> parse_args { acc with quick = Some true } rest
  | ("-x" | "--fail-fast") :: rest -> parse_args { acc with bail = Some 1 } rest
  | "--bail" :: n :: rest -> (
      match int_of_string_opt n with
      | Some b when b > 0 -> parse_args { acc with bail = Some b } rest
      | _ ->
          Pp.epr "Error: --bail requires a positive integer@.";
          exit 1)
  | "--bail" :: [] ->
      Pp.epr "Error: --bail requires an argument@.";
      exit 1
  | "--failed" :: rest -> parse_args { acc with failed = Some true } rest
  | ("-l" | "--list") :: rest ->
      parse_args { acc with list_only = Some true } rest
  | ("-u" | "--update") :: rest ->
      parse_args { acc with update = Some true } rest
  | ("-f" | "--filter") :: pat :: rest ->
      parse_args { acc with filter = Some pat } rest
  | "--filter" :: [] | "-f" :: [] ->
      Pp.epr "Error: --filter requires an argument@.";
      exit 1
  | ("-e" | "--exclude") :: pat :: rest ->
      parse_args { acc with exclude = Some pat } rest
  | "--exclude" :: [] | "-e" :: [] ->
      Pp.epr "Error: --exclude requires an argument@.";
      exit 1
  (* Tag filtering *)
  | "--tag" :: label :: rest ->
      parse_args { acc with tags = label :: acc.tags } rest
  | "--tag" :: [] ->
      Pp.epr "Error: --tag requires an argument@.";
      exit 1
  | "--exclude-tag" :: label :: rest ->
      parse_args { acc with exclude_tags = label :: acc.exclude_tags } rest
  | "--exclude-tag" :: [] ->
      Pp.epr "Error: --exclude-tag requires an argument@.";
      exit 1
  (* Output options *)
  | "--format" :: fmt :: rest -> parse_args { acc with format = Some fmt } rest
  | "--format" :: [] ->
      Pp.epr "Error: --format requires an argument@.";
      exit 1
  | "--junit" :: path :: rest -> parse_args { acc with junit = Some path } rest
  | "--junit" :: [] ->
      Pp.epr "Error: --junit requires an argument@.";
      exit 1
  (* Seed and timeout *)
  | "--seed" :: n :: rest -> (
      match int_of_string_opt n with
      | Some seed -> parse_args { acc with seed = Some seed } rest
      | None ->
          Pp.epr "Error: --seed requires an integer argument@.";
          exit 1)
  | "--seed" :: [] ->
      Pp.epr "Error: --seed requires an argument@.";
      exit 1
  | "--timeout" :: n :: rest -> (
      match float_of_string_opt n with
      | Some t when t > 0.0 -> parse_args { acc with timeout = Some t } rest
      | _ ->
          Pp.epr "Error: --timeout requires a positive number@.";
          exit 1)
  | "--timeout" :: [] ->
      Pp.epr "Error: --timeout requires an argument@.";
      exit 1
  | "--prop-count" :: n :: rest -> (
      match int_of_string_opt n with
      | Some c when c > 0 -> parse_args { acc with prop_count = Some c } rest
      | _ ->
          Pp.epr "Error: --prop-count requires a positive integer@.";
          exit 1)
  | "--prop-count" :: [] ->
      Pp.epr "Error: --prop-count requires an argument@.";
      exit 1
  (* Color *)
  | "--color" :: mode :: rest -> parse_args { acc with color = Some mode } rest
  | "--color" :: [] ->
      Pp.epr "Error: --color requires an argument (always, never, auto)@.";
      exit 1
  (* Other options *)
  | ("-o" | "--output") :: dir :: rest ->
      parse_args { acc with output_dir = Some dir } rest
  | "--output" :: [] | "-o" :: [] ->
      Pp.epr "Error: --output requires an argument@.";
      exit 1
  | ("-V" | "--version") :: _ ->
      Pp.pr "windtrap %s@." version;
      exit 0
  | ("-h" | "--help") :: _ ->
      print_help Sys.argv.(0);
      exit 0
  | "--" :: _ -> acc
  (* Equals-style arguments *)
  | arg :: rest when String.length arg > 9 && String.sub arg 0 9 = "--filter="
    ->
      let pat = String.sub arg 9 (String.length arg - 9) in
      parse_args { acc with filter = Some pat } rest
  | arg :: rest
    when String.length arg > 10 && String.sub arg 0 10 = "--exclude=" ->
      let pat = String.sub arg 10 (String.length arg - 10) in
      parse_args { acc with exclude = Some pat } rest
  | arg :: rest when String.length arg > 9 && String.sub arg 0 9 = "--format="
    ->
      let fmt = String.sub arg 9 (String.length arg - 9) in
      parse_args { acc with format = Some fmt } rest
  | arg :: rest when String.length arg > 8 && String.sub arg 0 8 = "--junit=" ->
      let path = String.sub arg 8 (String.length arg - 8) in
      parse_args { acc with junit = Some path } rest
  | arg :: rest when String.length arg > 9 && String.sub arg 0 9 = "--output="
    ->
      let dir = String.sub arg 9 (String.length arg - 9) in
      parse_args { acc with output_dir = Some dir } rest
  | arg :: rest when String.length arg > 7 && String.sub arg 0 7 = "--seed="
    -> (
      let n = String.sub arg 7 (String.length arg - 7) in
      match int_of_string_opt n with
      | Some seed -> parse_args { acc with seed = Some seed } rest
      | None ->
          Pp.epr "Error: --seed requires an integer argument@.";
          exit 1)
  | arg :: rest
    when String.length arg > 10 && String.sub arg 0 10 = "--timeout=" -> (
      let n = String.sub arg 10 (String.length arg - 10) in
      match float_of_string_opt n with
      | Some t when t > 0.0 -> parse_args { acc with timeout = Some t } rest
      | _ ->
          Pp.epr "Error: --timeout requires a positive number@.";
          exit 1)
  | arg :: rest
    when String.length arg > 13 && String.sub arg 0 13 = "--prop-count=" -> (
      let n = String.sub arg 13 (String.length arg - 13) in
      match int_of_string_opt n with
      | Some c when c > 0 -> parse_args { acc with prop_count = Some c } rest
      | _ ->
          Pp.epr "Error: --prop-count requires a positive integer@.";
          exit 1)
  | arg :: rest when String.length arg > 6 && String.sub arg 0 6 = "--tag=" ->
      let label = String.sub arg 6 (String.length arg - 6) in
      parse_args { acc with tags = label :: acc.tags } rest
  | arg :: rest when String.length arg > 7 && String.sub arg 0 7 = "--bail="
    -> (
      let n = String.sub arg 7 (String.length arg - 7) in
      match int_of_string_opt n with
      | Some b when b > 0 -> parse_args { acc with bail = Some b } rest
      | _ ->
          Pp.epr "Error: --bail requires a positive integer@.";
          exit 1)
  | arg :: rest
    when String.length arg > 14 && String.sub arg 0 14 = "--exclude-tag=" ->
      let label = String.sub arg 14 (String.length arg - 14) in
      parse_args { acc with exclude_tags = label :: acc.exclude_tags } rest
  | arg :: rest when String.length arg > 8 && String.sub arg 0 8 = "--color=" ->
      let mode = String.sub arg 8 (String.length arg - 8) in
      parse_args { acc with color = Some mode } rest
  (* Unknown flag *)
  | arg :: _ when String.length arg > 0 && arg.[0] = '-' ->
      Pp.epr "Unknown option: %s@." arg;
      Pp.epr "Try --help for usage information.@.";
      exit 1
  (* Positional argument becomes filter *)
  | arg :: rest when acc.filter = None ->
      parse_args { acc with filter = Some arg } rest
  | arg :: _ ->
      Pp.epr "Error: unexpected argument '%s' (filter already set to '%s')@."
        arg
        (Option.value ~default:"" acc.filter);
      exit 1

let parse argv =
  match Array.to_list argv with
  | [] -> empty
  | _ :: args -> parse_args empty args

let parse_format = function
  | "verbose" -> Progress.Verbose
  | "compact" -> Progress.Compact
  | "tap" -> Progress.Tap
  | "junit" -> Progress.Junit
  | s ->
      Pp.epr "Unknown format: %s. Use: verbose, compact, tap, junit@." s;
      exit 1

(* ───── Config Resolution ───── *)

(* Priority chain: programmatic arg > CLI flag > env var > default.
   Each layer is Option.t; first Some wins. *)
let resolve prog cli env default =
  prog
  |> Option.fold ~none:cli ~some:Option.some
  |> Option.fold ~none:(env ()) ~some:Option.some
  |> Option.value ~default

let resolve_opt prog cli env =
  prog
  |> Option.fold ~none:cli ~some:Option.some
  |> Option.fold ~none:(env ()) ~some:Option.some

let resolve_config ?quick ?bail ?fail_fast ?output_dir ?stream ?update
    ?snapshot_dir ?filter ?exclude ?failed ?format ?junit ?seed ?timeout
    ?prop_count ?tags ?exclude_tags (cli : t) =
  (* Apply --color override before anything that might emit styled output. *)
  Option.iter Env.override_color cli.color;
  let quick = resolve quick cli.quick (fun () -> None) false in
  let bail =
    match (bail, fail_fast) with
    | Some n, _ -> Some n
    | None, Some true -> Some 1
    | _ -> cli.bail
  in
  let output_dir =
    resolve output_dir cli.output_dir
      (fun () -> None)
      (Path_ops.default_log_dir ())
  in
  let update = resolve update cli.update Env.update false in
  let filter_pattern = resolve_opt filter cli.filter Env.filter in
  let exclude_pattern = resolve_opt exclude cli.exclude Env.exclude in
  let junit = resolve_opt junit cli.junit (fun () -> None) in

  let stream = resolve stream cli.stream Env.stream false in
  (* stream = true means "don't capture", so capture is the inverse *)
  let capture = not stream in

  (* Format needs a 3-way match because the programmatic arg is already a
     Progress.mode while CLI and env are raw strings that need parsing. *)
  let progress_mode =
    match (format, cli.format, Env.format ()) with
    | Some f, _, _ -> f
    | None, Some s, _ -> parse_format s
    | None, None, Some s -> parse_format s
    | None, None, None ->
        if Option.is_some junit then Progress.Junit else Progress.Compact
  in

  let snapshot_config =
    let base = Snapshot.Config.default () in
    let c =
      Option.fold ~none:base
        ~some:(fun d -> Snapshot.Config.create ~root_dir:d ())
        snapshot_dir
    in
    if update then Snapshot.Config.with_mode Snapshot.Update c else c
  in

  let seed = resolve_opt seed cli.seed Env.seed in
  let timeout =
    resolve_opt timeout cli.timeout (fun () ->
        Option.bind (Env.timeout ()) (fun t -> if t > 0.0 then Some t else None))
  in

  let prop_count = resolve_opt prop_count cli.prop_count Env.prop_count in

  let required_tags = Option.value ~default:[] tags @ cli.tags @ Env.tag () in
  let dropped_tags =
    Option.value ~default:[] exclude_tags
    @ cli.exclude_tags @ Env.exclude_tag ()
  in
  let use_failed =
    resolve (Option.map (fun b -> b) failed) cli.failed (fun () -> None) false
  in
  let failed_allowlist =
    if use_failed then
      let paths = Runner.read_last_failed output_dir in
      if paths = [] then begin
        Pp.epr "%a No previous failures recorded. Running all tests.@."
          (Pp.styled `Yellow Pp.string)
          "[INFO]";
        None
      end
      else Some paths
    else None
  in
  let base_filter =
    Runner.make_filter ~quick ~filter_pattern ~exclude_pattern ~required_tags
      ~dropped_tags
  in
  let filter =
    match failed_allowlist with
    | None -> base_filter
    | Some paths ->
        fun ~path tags ->
          if List.mem path paths then base_filter ~path tags else `Skip
  in
  {
    Runner.filter;
    progress_mode;
    log_dir = output_dir;
    capture;
    snapshot_config;
    bail;
    junit_file = junit;
    seed;
    default_timeout = timeout;
    prop_count;
  }
