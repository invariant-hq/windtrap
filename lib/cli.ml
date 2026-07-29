(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   The flag inventory and the programmatic > CLI > env > default precedence
   derive from windtrap v1's lib/cli.ml, rebuilt as one declarative flag
   table that generates parsing, --help, and the env-mirror listing.
  ---------------------------------------------------------------------------*)

(* ───── Parsed flags ───── *)

type parsed = {
  filter : string option;
  exclude : string option;
  tags : string list;
  exclude_tags : string list;
  shard : (int * int) option;
  quick : bool option;
  failed_only : bool option;
  list_only : bool option;
  bail : int option;
  stream : bool option;
  update : Env.update option;
  prune : bool option;
  seed : Seed.seed option;
  timeout : float option;
  slow_threshold : float option;
  prop_count : int option;
  output : [ `Quiet | `Verbose ] option;
  junit : string option;
  color : Env.color_mode option;
  coverage : [ `Summary | `Report | `Full | `Off ] option;
  log_dir : string option;
  help : bool;
  version : bool;
}

let empty =
  {
    filter = None;
    exclude = None;
    tags = [];
    exclude_tags = [];
    shard = None;
    quick = None;
    failed_only = None;
    list_only = None;
    bail = None;
    stream = None;
    update = None;
    prune = None;
    seed = None;
    timeout = None;
    slow_threshold = None;
    prop_count = None;
    output = None;
    junit = None;
    color = None;
    coverage = None;
    log_dir = None;
    help = false;
    version = false;
  }

(* ───── Errors ───── *)

type error =
  | Unknown_flag of string
  | Missing_value of string
  | Invalid_value of { source : string; value : string; expected : string }
  | Extra_positional of { filter : string; extra : string }

let error_message = function
  | Unknown_flag flag -> Pp.str "unknown option '%s'" flag
  | Missing_value flag -> Pp.str "option '%s' requires an argument" flag
  | Invalid_value { source; value; expected } ->
      Pp.str "invalid value '%s' for %s: expected %s" value source expected
  | Extra_positional { filter; extra } ->
      Pp.str "unexpected argument '%s': the filter is already '%s'" extra filter

(* ───── The flag table ───── *)

type arg =
  | Flag of (parsed -> parsed)
  | Value of {
      metavar : string;
      set : source:string -> parsed -> string -> (parsed, error) result;
    }

type entry = {
  short : string option;
  long : string;
  arg : arg;
  doc : string;
  mirror : string option; (* WINDTRAP_* environment mirror, for --help *)
}

let invalid ~source ~value ~expected =
  Error (Invalid_value { source; value; expected })

let set_string set = Value { metavar = "PATTERN"; set }

let set_positive_int store =
  Value
    {
      metavar = "N";
      set =
        (fun ~source acc value ->
          match int_of_string_opt value with
          | Some n when n > 0 -> Ok (store acc n)
          | _ -> invalid ~source ~value ~expected:"a positive integer");
    }

let seed_expected = "an s1: token with 16 lowercase hexadecimal digits"
let shard_expected = "K/N with 1 <= K <= N (e.g. 2/4)"
let coverage_expected = "summary, report, full or off"

let coverage_of_string value =
  match String.lowercase_ascii value with
  | "summary" -> Some `Summary
  | "report" -> Some `Report
  | "full" -> Some `Full
  | "off" -> Some `Off
  | _ -> None

(* "K/N" with K and N plain decimal numerals — the spelling is CI-facing
   and frozen, so int_of_string's 0x/0b/underscore/sign leniency is
   deliberately rejected — and 1 <= K <= N. *)
let shard_of_string value =
  let decimal s =
    s <> "" && String.for_all (function '0' .. '9' -> true | _ -> false) s
  in
  match String.index_opt value '/' with
  | None -> None
  | Some slash -> (
      let k = String.sub value 0 slash
      and n = String.sub value (slash + 1) (String.length value - slash - 1) in
      if not (decimal k && decimal n) then None
      else
        match (int_of_string_opt k, int_of_string_opt n) with
        | Some k, Some n when 1 <= k && k <= n -> Some (k, n)
        | _ -> None)

let table =
  [
    {
      short = Some "-f";
      long = "--filter";
      arg =
        set_string (fun ~source:_ acc value ->
            Ok { acc with filter = Some value });
      doc = "Run only tests whose path contains PATTERN";
      mirror = Some "WINDTRAP_FILTER";
    };
    {
      short = Some "-e";
      long = "--exclude";
      arg =
        set_string (fun ~source:_ acc value ->
            Ok { acc with exclude = Some value });
      doc = "Skip tests whose path contains PATTERN";
      mirror = Some "WINDTRAP_EXCLUDE";
    };
    {
      short = None;
      long = "--tag";
      arg =
        Value
          {
            metavar = "LABEL";
            set =
              (fun ~source:_ acc value ->
                Ok { acc with tags = acc.tags @ [ value ] });
          };
      doc = "Run only tests tagged LABEL (repeatable)";
      mirror = Some "WINDTRAP_TAG";
    };
    {
      short = None;
      long = "--exclude-tag";
      arg =
        Value
          {
            metavar = "LABEL";
            set =
              (fun ~source:_ acc value ->
                Ok { acc with exclude_tags = acc.exclude_tags @ [ value ] });
          };
      doc = "Skip tests tagged LABEL (repeatable)";
      mirror = Some "WINDTRAP_EXCLUDE_TAG";
    };
    {
      short = None;
      long = "--shard";
      arg =
        Value
          {
            metavar = "K/N";
            set =
              (fun ~source acc value ->
                match shard_of_string value with
                | Some shard -> Ok { acc with shard = Some shard }
                | None -> invalid ~source ~value ~expected:shard_expected);
          };
      doc = "Run only the Kth of N deterministic path-hash buckets";
      mirror = Some "WINDTRAP_SHARD";
    };
    {
      short = None;
      long = "--quick";
      arg = Flag (fun acc -> { acc with quick = Some true });
      doc = "Skip slow-tagged tests";
      mirror = None;
    };
    {
      short = None;
      long = "--failed";
      arg = Flag (fun acc -> { acc with failed_only = Some true });
      doc = "Rerun only the last run's failures";
      mirror = None;
    };
    {
      short = Some "-l";
      long = "--list";
      arg = Flag (fun acc -> { acc with list_only = Some true });
      doc = "List selected tests without running them";
      mirror = None;
    };
    {
      short = Some "-x";
      long = "--fail-fast";
      arg = Flag (fun acc -> { acc with bail = Some 1 });
      doc = "Stop after the first failure (same as --bail 1)";
      mirror = None;
    };
    {
      short = None;
      long = "--bail";
      arg = set_positive_int (fun acc n -> { acc with bail = Some n });
      doc = "Stop after N failures";
      mirror = None;
    };
    {
      short = None;
      long = "--timeout";
      arg =
        Value
          {
            metavar = "SECONDS";
            set =
              (fun ~source acc value ->
                match float_of_string_opt value with
                | Some limit when limit > 0. && Float.is_finite limit ->
                    Ok { acc with timeout = Some limit }
                | _ -> invalid ~source ~value ~expected:"a positive number");
          };
      doc = "Default per-test timeout in seconds";
      mirror = Some "WINDTRAP_TIMEOUT";
    };
    {
      short = None;
      long = "--slow-threshold";
      arg =
        Value
          {
            metavar = "SECONDS";
            set =
              (fun ~source acc value ->
                match float_of_string_opt value with
                | Some limit when limit >= 0. && Float.is_finite limit ->
                    Ok { acc with slow_threshold = Some limit }
                | _ -> invalid ~source ~value ~expected:"a non-negative number");
          };
      doc = "Warn when an untagged test runs longer than SECONDS (0 disables)";
      mirror = Some "WINDTRAP_SLOW_THRESHOLD";
    };
    {
      short = None;
      long = "--seed";
      arg =
        Value
          {
            metavar = "TOKEN";
            set =
              (fun ~source acc value ->
                match Seed.of_string value with
                | Ok seed -> Ok { acc with seed = Some seed }
                | Error _ -> invalid ~source ~value ~expected:seed_expected);
          };
      doc = "Root seed for property tests (s1:<16 hex>)";
      mirror = Some "WINDTRAP_SEED";
    };
    {
      short = None;
      long = "--prop-count";
      arg = set_positive_int (fun acc n -> { acc with prop_count = Some n });
      doc = "Generated cases per property";
      mirror = Some "WINDTRAP_PROP_COUNT";
    };
    {
      short = Some "-u";
      long = "--update";
      arg = Flag (fun acc -> { acc with update = Some Env.Update });
      doc = "Accept snapshot changes (refused under CI)";
      mirror = Some "WINDTRAP_UPDATE";
    };
    {
      short = None;
      long = "--prune";
      arg = Flag (fun acc -> { acc with prune = Some true });
      doc = "Delete orphaned baselines after a full, clean update run";
      mirror = Some "WINDTRAP_PRUNE";
    };
    {
      short = Some "-s";
      long = "--stream";
      arg = Flag (fun acc -> { acc with stream = Some true });
      doc = "Stream test output instead of capturing it";
      mirror = Some "WINDTRAP_STREAM";
    };
    {
      (* One verbosity axis, three levels: -q ⊂ default ⊂ -v. Both flags
         set the same [output] field, so repeating or mixing them is
         last-one-wins, like every other single-valued flag. *)
      short = Some "-v";
      long = "--verbose";
      arg = Flag (fun acc -> { acc with output = Some `Verbose });
      doc = "One status line per test";
      mirror = Some "WINDTRAP_VERBOSE";
    };
    {
      short = Some "-q";
      long = "--quiet";
      arg = Flag (fun acc -> { acc with output = Some `Quiet });
      doc = "Failures and summary only";
      mirror = Some "WINDTRAP_QUIET";
    };
    {
      short = None;
      long = "--junit";
      arg =
        Value
          {
            metavar = "PATH";
            set =
              (fun ~source:_ acc value -> Ok { acc with junit = Some value });
          };
      doc = "Also write a JUnit XML report to PATH";
      mirror = None;
    };
    {
      short = None;
      long = "--color";
      arg =
        Value
          {
            metavar = "MODE";
            set =
              (fun ~source acc value ->
                match String.lowercase_ascii value with
                | "always" -> Ok { acc with color = Some Env.Always }
                | "never" -> Ok { acc with color = Some Env.Never }
                | "auto" -> Ok { acc with color = Some Env.Auto }
                | _ -> invalid ~source ~value ~expected:"always, never or auto");
          };
      doc = "Color output: always, never or auto";
      mirror = Some "WINDTRAP_COLOR";
    };
    {
      short = None;
      long = "--coverage";
      arg =
        Value
          {
            metavar = "MODE";
            set =
              (fun ~source acc value ->
                match coverage_of_string value with
                | Some mode -> Ok { acc with coverage = Some mode }
                | None -> invalid ~source ~value ~expected:coverage_expected);
          };
      doc = "Coverage output: summary, report, full or off";
      mirror = Some "WINDTRAP_COVERAGE";
    };
    {
      short = Some "-o";
      long = "--output";
      arg =
        Value
          {
            metavar = "DIR";
            set =
              (fun ~source:_ acc value -> Ok { acc with log_dir = Some value });
          };
      doc = "Root directory for capture logs";
      mirror = None;
    };
    {
      short = Some "-V";
      long = "--version";
      arg = Flag (fun acc -> { acc with version = true });
      doc = "Print the version and exit";
      mirror = None;
    };
    {
      short = Some "-h";
      long = "--help";
      arg = Flag (fun acc -> { acc with help = true });
      doc = "Print this help and exit";
      mirror = None;
    };
  ]

(* Environment settings with no flag, listed by --help. *)
let env_only =
  [
    ("WINDTRAP_ALLOW_FOCUS", "Lift the CI guard on focused tests");
    ("WINDTRAP_COLUMNS", "Terminal width override for reports");
    ("WINDTRAP_TAIL_ERRORS", "Captured-output lines shown per failure");
    ("WINDTRAP_PROJECT_ROOT", "Project root for snapshot path resolution");
  ]

(* ───── Parsing ───── *)

let find_long name = List.find_opt (fun entry -> entry.long = name) table
let find_short name = List.find_opt (fun entry -> entry.short = Some name) table

(* Split "--flag=value" into the flag and its inline value. *)
let split_inline arg =
  match String.index_from_opt arg 2 '=' with
  | None -> (arg, None)
  | Some eq ->
      ( String.sub arg 0 eq,
        Some (String.sub arg (eq + 1) (String.length arg - eq - 1)) )

let ( let* ) = Result.bind

let add_positional acc value =
  match acc.filter with
  | None -> Ok { acc with filter = Some value }
  | Some filter -> Error (Extra_positional { filter; extra = value })

let rec parse_args acc = function
  | [] -> Ok acc
  | "--" :: rest ->
      List.fold_left
        (fun acc value ->
          let* acc = acc in
          add_positional acc value)
        (Ok acc) rest
  | arg :: rest when String.length arg > 2 && String.sub arg 0 2 = "--" ->
      let name, inline = split_inline arg in
      apply (find_long name) ~source:name ~inline acc rest
  | arg :: rest when String.length arg > 1 && arg.[0] = '-' ->
      apply (find_short arg) ~source:arg ~inline:None acc rest
  | arg :: rest ->
      let* acc = add_positional acc arg in
      parse_args acc rest

and apply entry ~source ~inline acc rest =
  match entry with
  | None -> Error (Unknown_flag source)
  | Some { arg = Flag set; _ } -> (
      match inline with
      | Some value -> invalid ~source ~value ~expected:"no argument"
      | None ->
          let acc = set acc in
          (* --help and --version win immediately; later flags are unread. *)
          if acc.help || acc.version then Ok acc else parse_args acc rest)
  | Some { arg = Value { set; _ }; _ } -> (
      match inline with
      | Some value ->
          let* acc = set ~source acc value in
          parse_args acc rest
      | None -> (
          match rest with
          | [] -> Error (Missing_value source)
          | value :: rest ->
              let* acc = set ~source acc value in
              parse_args acc rest))

let parse argv =
  match Array.to_list argv with
  | [] -> Ok empty
  | _prog :: args -> parse_args empty args

(* ───── Resolution ───── *)

let first_some higher lower =
  match higher with Some _ -> higher | None -> lower

(* The environment layer of the output level. Within the layer verbose
   wins over quiet — the variables carry no order to make last-one-wins
   meaningful, so the tie-break is fixed and documented. *)
let env_output () =
  match (Env.verbose (), Env.quiet ()) with
  | Some true, _ -> Some `Verbose
  | _, Some true -> Some `Quiet
  | _, _ -> None

let resolve_output ~overrides cli =
  match first_some overrides.output (first_some cli.output (env_output ())) with
  | Some `Quiet -> `Quiet
  | Some `Verbose -> `Verbose
  | None -> `Compact

let output_level ?(overrides = empty) cli = resolve_output ~overrides cli

(* [parse] validates command-line values, but programmatic overrides and the
   environment mirrors bypass it — WINDTRAP_TIMEOUT=-5 must not reach
   [Unix.setitimer]. [checked ~flag ~env] re-validates the value the
   precedence picked, naming the winning layer as the error source (the flag
   spelling stands in for a programmatic override — there is no typed flag
   to name). *)
let checked ~flag ~env ~valid ~render ~expected value env_value =
  match first_some value env_value with
  | Some v when not (valid v) ->
      let source = if value = None then env else flag in
      invalid ~source ~value:(render v) ~expected
  | picked -> Ok picked

(* Numeric mirrors validate like their flags (prop/F-4): when the
   environment is the winning layer — no programmatic or CLI value above it
   — a token that does not parse is an error naming the variable, exactly
   like the WINDTRAP_SEED and WINDTRAP_SHARD paths, never a silently
   defaulted run. A losing layer stays unread, so a valid CLI value shadows
   a malformed mirror. Tokens are trimmed before parsing, matching the Env
   module's numeric convention. *)
let env_numeric ~source ~parse ~expected raw higher =
  match higher with
  | Some _ -> Ok None
  | None -> (
      match raw with
      | None -> Ok None
      | Some value -> (
          match parse (String.trim value) with
          | Some v -> Ok (Some v)
          | None -> invalid ~source ~value ~expected))

let resolve ?(overrides = empty) cli =
  let pick over_field cli_field env_field =
    first_some over_field (first_some cli_field env_field)
  in
  let defaults = Run.default_config () in
  let* seed =
    match first_some overrides.seed cli.seed with
    | Some seed -> Ok seed
    | None -> (
        match Env.seed () with
        | None -> Ok defaults.Run.seed
        | Some value -> (
            match Seed.of_string value with
            | Ok seed -> Ok seed
            | Error _ ->
                invalid ~source:"WINDTRAP_SEED" ~value ~expected:seed_expected))
  in
  let* timeout =
    let higher = first_some overrides.timeout cli.timeout in
    let* env_value =
      env_numeric ~source:"WINDTRAP_TIMEOUT" ~parse:float_of_string_opt
        ~expected:"a positive number" (Env.timeout ()) higher
    in
    checked ~flag:"--timeout" ~env:"WINDTRAP_TIMEOUT"
      ~valid:(fun t -> Float.is_finite t && t > 0.)
      ~render:(Pp.str "%g") ~expected:"a positive number" higher env_value
  in
  let* slow_threshold =
    let higher = first_some overrides.slow_threshold cli.slow_threshold in
    let* env_value =
      env_numeric ~source:"WINDTRAP_SLOW_THRESHOLD" ~parse:float_of_string_opt
        ~expected:"a non-negative number" (Env.slow_threshold ()) higher
    in
    checked ~flag:"--slow-threshold" ~env:"WINDTRAP_SLOW_THRESHOLD"
      ~valid:(fun t -> Float.is_finite t && t >= 0.)
      ~render:(Pp.str "%g") ~expected:"a non-negative number" higher env_value
  in
  let positive_int ~flag ~env value env_value =
    checked ~flag ~env
      ~valid:(fun n -> n > 0)
      ~render:string_of_int ~expected:"a positive integer" value env_value
  in
  let* prop_count =
    let higher = first_some overrides.prop_count cli.prop_count in
    let* env_value =
      env_numeric ~source:"WINDTRAP_PROP_COUNT" ~parse:int_of_string_opt
        ~expected:"a positive integer" (Env.prop_count ()) higher
    in
    positive_int ~flag:"--prop-count" ~env:"WINDTRAP_PROP_COUNT" higher
      env_value
  in
  let* bail =
    positive_int ~flag:"--bail" ~env:"--bail" (* no environment mirror *)
      (first_some overrides.bail cli.bail)
      None
  in
  let* shard =
    (* Like WINDTRAP_SEED, a malformed winning token is an error, because a
       silently ignored shard reruns the whole suite in every bucket. *)
    let env_shard () =
      match Env.shard () with
      | None -> Ok None
      | Some value -> (
          match shard_of_string value with
          | Some shard -> Ok (Some shard)
          | None ->
              invalid ~source:"WINDTRAP_SHARD" ~value ~expected:shard_expected)
    in
    match first_some overrides.shard cli.shard with
    | Some (k, n) when not (1 <= k && k <= n) ->
        (* Programmatic overrides bypass [parse]'s validation. *)
        invalid ~source:"--shard" ~value:(Pp.str "%d/%d" k n)
          ~expected:shard_expected
    | Some _ as shard -> Ok shard
    | None -> env_shard ()
  in
  let env_update =
    match Env.update () with Env.No_update -> None | u -> Some u
  and env_flag read = if read () then Some true else None in
  Ok
    {
      Run.seed;
      filter = pick overrides.filter cli.filter (Env.filter ());
      exclude = pick overrides.exclude cli.exclude (Env.exclude ());
      tags = overrides.tags @ cli.tags @ Env.tags ();
      exclude_tags =
        overrides.exclude_tags @ cli.exclude_tags @ Env.exclude_tags ();
      shard;
      quick = Option.value (first_some overrides.quick cli.quick) ~default:false;
      failed_only =
        Option.value
          (first_some overrides.failed_only cli.failed_only)
          ~default:false;
      list_only =
        Option.value
          (first_some overrides.list_only cli.list_only)
          ~default:false;
      bail;
      stream =
        Option.value
          (pick overrides.stream cli.stream (Env.stream ()))
          ~default:false;
      update =
        Option.value
          (pick overrides.update cli.update env_update)
          ~default:Env.No_update;
      prune =
        Option.value
          (pick overrides.prune cli.prune (env_flag Env.prune))
          ~default:false;
      timeout;
      slow_threshold =
        Option.value slow_threshold ~default:defaults.Run.slow_threshold;
      prop_count;
      junit = first_some overrides.junit cli.junit;
      color =
        Option.value
          (first_some overrides.color cli.color)
          ~default:(Env.color_mode ());
      columns = Env.columns ();
      tail_errors = Env.tail_errors ();
      log_dir =
        Option.value
          (first_some overrides.log_dir cli.log_dir)
          ~default:defaults.Run.log_dir;
      allow_focus = Env.allow_focus ();
    }

(* The coverage mode resolves outside [resolve] because it is not run
   configuration: it is a rendering decision the facade's [run] applies
   after the run record is complete (RFC Law 12 — the run's [config]
   carries no coverage field). Same precedence and same loudness as the
   config mirrors: CLI beats [WINDTRAP_COVERAGE]; a malformed winning
   value is an error, never a silently defaulted mode. *)
let coverage_mode (cli : parsed) =
  match cli.coverage with
  | Some mode -> Ok mode
  | None -> (
      match Env.coverage () with
      | None -> Ok `Summary
      | Some value -> (
          match coverage_of_string value with
          | Some mode -> Ok mode
          | None ->
              invalid ~source:"WINDTRAP_COVERAGE" ~value
                ~expected:coverage_expected))

(* ───── Help ───── *)

let usage ~prog =
  Pp.str "usage: %s [OPTIONS] [PATTERN]" (Filename.basename prog)

let flag_heading entry =
  let names =
    match entry.short with
    | Some short -> Pp.str "%s, %s" short entry.long
    | None -> Pp.str "    %s" entry.long
  in
  match entry.arg with
  | Flag _ -> names
  | Value { metavar; _ } -> names ^ " " ^ metavar

let two_columns rows =
  let width =
    List.fold_left (fun w (head, _) -> max w (String.length head)) 0 rows
  in
  List.map
    (fun (head, doc) ->
      Pp.str "  %s%s  %s" head
        (String.make (width - String.length head) ' ')
        doc)
    rows

let help ~prog =
  let flag_rows = List.map (fun e -> (flag_heading e, e.doc)) table in
  let mirror_rows =
    List.filter_map
      (fun e ->
        Option.map (fun v -> (v, Pp.str "Mirror of %s" e.long)) e.mirror)
      table
    @ env_only
  in
  String.concat "\n"
    ([
       Pp.str "%s - windtrap test runner" (Filename.basename prog);
       "";
       usage ~prog;
       "";
       "A bare PATTERN runs only tests whose full path contains it (same as";
       "-f PATTERN). Under `dune runtest`, set options through their";
       "WINDTRAP_* environment mirrors instead.";
       "";
       "OPTIONS:";
     ]
    @ two_columns flag_rows @ [ ""; "ENVIRONMENT:" ] @ two_columns mirror_rows)
  ^ "\n"
