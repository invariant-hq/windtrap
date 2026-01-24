(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* ───── Types ───── *)

type t = {
  stream : bool option;
  format : string option;
  fail_fast : bool option;
  quick : bool option;
  filter : string option;
  list_only : bool option;
  output_dir : string option;
  update : bool option;
  junit : string option;
  seed : int option;
  timeout : float option;
}

let empty =
  {
    stream = None;
    format = None;
    fail_fast = None;
    quick = None;
    filter = None;
    list_only = None;
    output_dir = None;
    update = None;
    junit = None;
    seed = None;
    timeout = None;
  }

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
  Pp.pr "    -x, --fail-fast        Stop on first failure@,";
  Pp.pr "    -l, --list             List tests without running@,";
  Pp.pr "    -u, --update           Update snapshots@,";
  Pp.pr "    -f, --filter PATTERN   Filter tests by name@,@,";
  Pp.pr "OUTPUT:@,";
  Pp.pr "    --format FMT           Output: verbose, compact, tap, junit@,";
  Pp.pr "    --junit PATH           Write JUnit XML to file@,@,";
  Pp.pr "OTHER:@,";
  Pp.pr "    --seed N               Random seed for property tests@,";
  Pp.pr "    --timeout N            Default timeout in seconds@,";
  Pp.pr "    -o, --output DIR       Directory for test logs@,";
  Pp.pr "    -h, --help             Show this help@,@,";
  Pp.pr "ENVIRONMENT VARIABLES:@,";
  Pp.pr "    WINDTRAP_STREAM        Stream output (1/true/yes)@,";
  Pp.pr "    WINDTRAP_FORMAT        Output format (verbose/compact/tap/junit)@,";
  Pp.pr "    WINDTRAP_FILTER        Filter pattern@,";
  Pp.pr "    WINDTRAP_UPDATE        Update snapshots (1/true/yes)@,";
  Pp.pr "    WINDTRAP_SEED          Random seed for property tests@,";
  Pp.pr "    WINDTRAP_COLOR         Color output (always/never/auto)@,";
  Pp.pr "@]%!"

let rec parse_args acc = function
  | [] -> acc
  (* Common options *)
  | ("-s" | "--stream") :: rest ->
      parse_args { acc with stream = Some true } rest
  | ("-v" | "--verbose") :: rest ->
      parse_args { acc with format = Some "verbose" } rest
  | ("-q" | "--quick") :: rest -> parse_args { acc with quick = Some true } rest
  | ("-x" | "--fail-fast") :: rest ->
      parse_args { acc with fail_fast = Some true } rest
  | ("-l" | "--list") :: rest ->
      parse_args { acc with list_only = Some true } rest
  | ("-u" | "--update") :: rest ->
      parse_args { acc with update = Some true } rest
  | ("-f" | "--filter") :: pat :: rest ->
      parse_args { acc with filter = Some pat } rest
  | "--filter" :: [] | "-f" :: [] ->
      Pp.epr "Error: --filter requires an argument@.";
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
  (* Other options *)
  | ("-o" | "--output") :: dir :: rest ->
      parse_args { acc with output_dir = Some dir } rest
  | "--output" :: [] | "-o" :: [] ->
      Pp.epr "Error: --output requires an argument@.";
      exit 1
  | ("-h" | "--help") :: _ ->
      print_help Sys.argv.(0);
      exit 0
  | "--" :: _ -> acc
  (* Equals-style arguments *)
  | arg :: rest when String.length arg > 9 && String.sub arg 0 9 = "--filter="
    ->
      let pat = String.sub arg 9 (String.length arg - 9) in
      parse_args { acc with filter = Some pat } rest
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

let resolve_config ?quick ?fail_fast ?output_dir ?stream ?update ?snapshot_dir
    ?filter ?format ?junit ?seed ?timeout (cli : t) =
  let quick = resolve quick cli.quick (fun () -> None) false in
  let fail_fast = resolve fail_fast cli.fail_fast (fun () -> None) false in
  let output_dir =
    resolve output_dir cli.output_dir
      (fun () -> None)
      (Path_ops.default_log_dir ())
  in
  let update = resolve update cli.update Env.update false in
  let filter_pattern = resolve_opt filter cli.filter Env.filter in
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
        if Option.is_some junit then Progress.Junit else Progress.Verbose
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

  let filter = Runner.make_filter ~quick ~filter_pattern in
  {
    Runner.filter;
    progress_mode;
    log_dir = output_dir;
    capture;
    snapshot_config;
    stop_on_error = fail_fast;
    junit_file = junit;
    seed;
    default_timeout = timeout;
  }
