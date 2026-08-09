(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   The flag inventory and the programmatic > CLI > env > default precedence
   derive from windtrap v1's lib/cli.ml, rebuilt as one declarative flag
   table that generates parsing, --help, and the env-mirror listing.
  ---------------------------------------------------------------------------*)

(* Parsed flags *)

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
  max_shrink : int option;
  max_discard : int option;
  max_prop_count : int option;
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
    max_shrink = None;
    max_discard = None;
    max_prop_count = None;
    output = None;
    junit = None;
    color = None;
    coverage = None;
    log_dir = None;
    help = false;
    version = false;
  }

(* Errors *)

type error =
  | Unknown_flag of string
  | Missing_value of string
  | Invalid_value of { source : string; value : string; expected : string }
  | Extra_positional of { filter : string; extra : string }

(* The flag table *)

type arg =
  | Flag of (parsed -> parsed)
  | Value of {
      metavar : string;
      set : source:string -> parsed -> string -> (parsed, error) result;
    }

(* How a mirror's raw value is spelled. Every reader but [Own] turns it
   into tokens the flag's own [arg] consumes, which is what keeps a mirror
   from validating differently from the flag it mirrors: same parser, same
   range check, same [expected] text, only the error source differs.

   [Token] is the whole value as one token, shaped — [Fun.id] where it is
   read as written, [String.trim] for the numeric tokens. [Comma] is one
   token per comma-separated item, trimmed, empties dropped. [Truthy] is a
   boolean spelling: a truthy value applies a value-less flag. [Own] is a
   vocabulary the variable owns and [Env] parses — WINDTRAP_UPDATE's
   [force], WINDTRAP_COLOR's silent fall back to [Auto]. *)
type reader =
  | Token of (string -> string)
  | Comma
  | Truthy
  | Own of (parsed -> parsed)

(* A flag's WINDTRAP_* environment mirror, declared beside the flag it
   mirrors. [absent p] is [true] while no layer above the environment has
   decided this flag's field; it carries the precedence law for the mirror.
   A mirror whose flag already lost is never even parsed, so a valid
   [--timeout] shadows a malformed WINDTRAP_TIMEOUT instead of tripping over
   it; and because the layer is folded in table order, the first mirror to
   write a field keeps it — which is exactly the documented
   WINDTRAP_VERBOSE-over-WINDTRAP_QUIET tie-break. Additive fields ([--tag],
   [--exclude-tag]) are never closed: every layer contributes. *)
type mirror = { var : string; reader : reader; absent : parsed -> bool }

type entry = {
  short : string option;
  long : string;
  arg : arg;
  doc : string;
  mirror : mirror option;
}

let invalid ~source ~value ~expected =
  Error (Invalid_value { source; value; expected })

let mirrored var reader absent = Some { var; reader; absent }
let verbatim = Token Fun.id
let trimmed = Token String.trim
let additive _ = true
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

(* A discard budget of [0] is meaningful — "tolerate no discards at all" —
   so this knob is non-negative where the others are positive. *)
let set_non_negative_int store =
  Value
    {
      metavar = "N";
      set =
        (fun ~source acc value ->
          match int_of_string_opt value with
          | Some n when n >= 0 -> Ok (store acc n)
          | _ -> invalid ~source ~value ~expected:"a non-negative integer");
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
      mirror = mirrored "WINDTRAP_FILTER" verbatim (fun p -> p.filter = None);
    };
    {
      short = Some "-e";
      long = "--exclude";
      arg =
        set_string (fun ~source:_ acc value ->
            Ok { acc with exclude = Some value });
      doc = "Skip tests whose path contains PATTERN";
      mirror = mirrored "WINDTRAP_EXCLUDE" verbatim (fun p -> p.exclude = None);
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
      mirror = mirrored "WINDTRAP_TAG" Comma additive;
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
      mirror = mirrored "WINDTRAP_EXCLUDE_TAG" Comma additive;
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
      mirror = mirrored "WINDTRAP_SHARD" verbatim (fun p -> p.shard = None);
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
      mirror = mirrored "WINDTRAP_FAILED" Truthy (fun p -> p.failed_only = None);
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
      mirror = mirrored "WINDTRAP_BAIL" trimmed (fun p -> p.bail = None);
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
      mirror = mirrored "WINDTRAP_TIMEOUT" trimmed (fun p -> p.timeout = None);
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
      mirror =
        mirrored "WINDTRAP_SLOW_THRESHOLD" trimmed (fun p ->
            p.slow_threshold = None);
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
      mirror = mirrored "WINDTRAP_SEED" verbatim (fun p -> p.seed = None);
    };
    {
      short = None;
      long = "--prop-count";
      arg = set_positive_int (fun acc n -> { acc with prop_count = Some n });
      doc = "Generated cases per property";
      mirror =
        mirrored "WINDTRAP_PROP_COUNT" trimmed (fun p -> p.prop_count = None);
    };
    {
      short = None;
      long = "--max-shrink";
      arg = set_positive_int (fun acc n -> { acc with max_shrink = Some n });
      doc = "Accepted shrink steps per failing property";
      mirror =
        mirrored "WINDTRAP_MAX_SHRINK" trimmed (fun p -> p.max_shrink = None);
    };
    {
      short = None;
      long = "--max-prop-count";
      arg =
        set_positive_int (fun acc n -> { acc with max_prop_count = Some n });
      doc = "Ceiling on every property's case count";
      mirror =
        mirrored "WINDTRAP_MAX_PROP_COUNT" trimmed (fun p ->
            p.max_prop_count = None);
    };
    {
      short = None;
      long = "--max-discard";
      arg = set_non_negative_int (fun acc n -> { acc with max_discard = Some n });
      doc = "Discarded cases tolerated per property (default 2x the count)";
      mirror =
        mirrored "WINDTRAP_MAX_DISCARD" trimmed (fun p -> p.max_discard = None);
    };
    {
      short = Some "-u";
      long = "--update";
      arg = Flag (fun acc -> { acc with update = Some Env.Update });
      doc = "Accept snapshot changes (refused under CI)";
      mirror =
        mirrored "WINDTRAP_UPDATE"
          (Own
             (fun acc ->
               match Env.update () with
               | Env.No_update -> acc
               | mode -> { acc with update = Some mode }))
          (fun p -> p.update = None);
    };
    {
      short = None;
      long = "--prune";
      arg = Flag (fun acc -> { acc with prune = Some true });
      doc = "Delete orphaned baselines after a full, clean update run";
      mirror = mirrored "WINDTRAP_PRUNE" Truthy (fun p -> p.prune = None);
    };
    {
      short = Some "-s";
      long = "--stream";
      arg = Flag (fun acc -> { acc with stream = Some true });
      doc = "Stream test output instead of capturing it";
      mirror = mirrored "WINDTRAP_STREAM" Truthy (fun p -> p.stream = None);
    };
    {
      (* One verbosity axis, three levels: -q ⊂ default ⊂ -v. Both flags
         set the same [output] field, so repeating or mixing them is
         last-one-wins, like every other single-valued flag. *)
      short = Some "-v";
      long = "--verbose";
      arg = Flag (fun acc -> { acc with output = Some `Verbose });
      doc = "One status line per test";
      mirror = mirrored "WINDTRAP_VERBOSE" Truthy (fun p -> p.output = None);
    };
    {
      short = Some "-q";
      long = "--quiet";
      arg = Flag (fun acc -> { acc with output = Some `Quiet });
      doc = "Failures and summary only";
      mirror = mirrored "WINDTRAP_QUIET" Truthy (fun p -> p.output = None);
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
      mirror = mirrored "WINDTRAP_JUNIT" verbatim (fun p -> p.junit = None);
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
      mirror =
        mirrored "WINDTRAP_COLOR"
          (Own (fun acc -> { acc with color = Some (Env.color_mode ()) }))
          (fun p -> p.color = None);
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
      mirror =
        mirrored "WINDTRAP_COVERAGE" verbatim (fun p -> p.coverage = None);
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
      mirror = mirrored "WINDTRAP_OUTPUT" verbatim (fun p -> p.log_dir = None);
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

(* Did-you-mean

   Damerau-Levenshtein over the long flag names, bounded. Transposition
   counts as one edit because it is the typo people actually make:
   plain Levenshtein scores [--juint] two from both [--junit] and
   [--quiet], and the tie would be broken by table order.

   Long names only, and only for an input that looks like one. Any two
   short flags are one edit apart, so a suggestion for [-Z] would be
   arbitrary — and a confident wrong suggestion is worse than none. *)

let edit_distance a b =
  let la = String.length a and lb = String.length b in
  (* Three rows: the transposition case reads two rows back. *)
  let rows = Array.make_matrix (la + 1) (lb + 1) 0 in
  for i = 0 to la do
    rows.(i).(0) <- i
  done;
  for j = 0 to lb do
    rows.(0).(j) <- j
  done;
  for i = 1 to la do
    for j = 1 to lb do
      let substitution = if a.[i - 1] = b.[j - 1] then 0 else 1 in
      let best =
        min
          (min (rows.(i).(j - 1) + 1) (rows.(i - 1).(j) + 1))
          (rows.(i - 1).(j - 1) + substitution)
      in
      rows.(i).(j) <-
        (if
           i > 1 && j > 1
           && a.[i - 1] = b.[j - 2]
           && a.[i - 2] = b.[j - 1]
         then min best (rows.(i - 2).(j - 2) + 1)
         else best)
    done
  done;
  rows.(la).(lb)

let nearest_flag flag =
  if not (String.starts_with ~prefix:"--" flag) then None
  else
    (* A third of the name, floor two: beyond that it is a different word,
       not a slip. *)
    let budget = max 2 (String.length flag / 3) in
    let closer best entry =
      let d = edit_distance flag entry.long in
      match best with
      | Some (_, best_d) when best_d <= d -> best
      | _ when d <= budget -> Some (entry.long, d)
      | _ -> best
    in
    Option.map fst (List.fold_left closer None table)

let error_message = function
  | Unknown_flag flag -> (
      let base = Pp.str "unknown option '%s'" flag in
      match nearest_flag flag with
      | Some name -> Pp.str "%s; did you mean '%s'?" base name
      | None -> base)
  | Missing_value flag -> Pp.str "option '%s' requires an argument" flag
  | Invalid_value { source; value; expected } ->
      Pp.str "invalid value '%s' for %s: expected %s" value source expected
  | Extra_positional { filter; extra } ->
      Pp.str "unexpected argument '%s': the filter is already '%s'" extra filter


(* Environment settings with no flag, listed by --help. *)
let env_only =
  [
    ("WINDTRAP_ALLOW_FOCUS", "Lift the CI guard on focused tests");
    ("WINDTRAP_COLUMNS", "Terminal width override for reports");
    ("WINDTRAP_TAIL_ERRORS", "Captured-output lines shown per failure");
    ("WINDTRAP_PROJECT_ROOT", "Project root for snapshot path resolution");
  ]

(* Parsing *)

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

(* Resolution *)

let first_some higher lower =
  match higher with Some _ -> higher | None -> lower

(* The environment layer, folded out of the same table that drives parsing
   and [--help]: every WINDTRAP_* mirror is read here and nowhere else, and
   every value reaches [parsed] through its flag's own [arg]. That is what
   makes a mirror incapable of drifting from its flag — WINDTRAP_SHARD=9/2
   fails exactly as [--shard 9/2] does, because it runs the same [set], with
   the variable named as the source instead of the flag.

   [layers ~overrides cli] is [cli] with each mirror filled into the fields
   neither [overrides] nor an earlier entry closed: the CLI and environment
   layers already merged, for the caller to lay the programmatic layer over.
   A malformed value in a mirror that wins is [Error] naming the variable,
   and the fold stops there — never a silently defaulted run. *)
let layers ~overrides cli =
  let contribute acc entry mirror raw =
    let apply acc token =
      let* acc = acc in
      match entry.arg with
      | Flag set -> Ok (set acc)
      | Value { set; _ } -> set ~source:mirror.var acc token
    in
    match mirror.reader with
    | Own read -> Ok (read acc)
    | Token shape -> apply (Ok acc) (shape raw)
    | Comma -> List.fold_left apply (Ok acc) (Env.split_comma raw)
    | Truthy ->
        if Env.get_bool mirror.var = Some true then apply (Ok acc) raw
        else Ok acc
  in
  List.fold_left
    (fun acc entry ->
      let* acc = acc in
      match entry.mirror with
      | Some mirror when mirror.absent overrides && mirror.absent acc -> (
          match Env.get_string mirror.var with
          | Some raw -> contribute acc entry mirror raw
          | None -> Ok acc)
      | Some _ | None -> Ok acc)
    (Ok cli) table

let output_level ?(overrides = empty) cli =
  (* [layers] stops at the first malformed mirror, which may well be one the
     output level does not depend on. The caller resolves the configuration
     first and exits on that error, so the layers above the environment are
     answer enough when the fold did not finish. *)
  let below = Result.value (layers ~overrides cli) ~default:cli in
  match first_some overrides.output below.output with
  | Some `Quiet -> `Quiet
  | Some `Verbose -> `Verbose
  | None -> `Compact

(* [parse] checks every value the command line offers and the mirror readers
   check every value the environment offers, each naming its own source. A
   programmatic override goes through neither — [overrides] is a record the
   caller fills in directly — so the value the precedence picks is checked
   once more before it reaches the run: WINDTRAP_TIMEOUT=-5 must not reach
   [Unix.setitimer], and neither may a [-5.] written in OCaml. Only an
   override can fail here, which is why the source is always [flag]: the
   flag spelling is the nearest thing to a name a programmatic argument
   has. *)
let checked ~flag ~valid ~render ~expected value =
  match value with
  | Some v when not (valid v) ->
      invalid ~source:flag ~value:(render v) ~expected
  | picked -> Ok picked

let resolve ?(overrides = empty) cli =
  let* below = layers ~overrides cli in
  let defaults = Run.default_config () in
  let seconds ~flag ~valid ~expected value =
    checked ~flag ~valid ~render:(Pp.str "%g") ~expected value
  in
  let positive_int ~flag value =
    checked ~flag
      ~valid:(fun n -> n > 0)
      ~render:string_of_int ~expected:"a positive integer" value
  in
  let* timeout =
    seconds ~flag:"--timeout"
      ~valid:(fun t -> Float.is_finite t && t > 0.)
      ~expected:"a positive number"
      (first_some overrides.timeout below.timeout)
  in
  let* slow_threshold =
    seconds ~flag:"--slow-threshold"
      ~valid:(fun t -> Float.is_finite t && t >= 0.)
      ~expected:"a non-negative number"
      (first_some overrides.slow_threshold below.slow_threshold)
  in
  let* prop_count =
    positive_int ~flag:"--prop-count"
      (first_some overrides.prop_count below.prop_count)
  in
  let* max_shrink =
    positive_int ~flag:"--max-shrink"
      (first_some overrides.max_shrink below.max_shrink)
  in
  let* max_prop_count =
    positive_int ~flag:"--max-prop-count"
      (first_some overrides.max_prop_count below.max_prop_count)
  in
  let* max_discard =
    (* Non-negative, not positive: zero is a coherent budget — "tolerate no
       discards" — and the engine already accepted it. *)
    checked ~flag:"--max-discard"
      ~valid:(fun n -> n >= 0)
      ~render:string_of_int ~expected:"a non-negative integer"
      (first_some overrides.max_discard below.max_discard)
  in
  let* bail =
    positive_int ~flag:"--bail" (first_some overrides.bail below.bail)
  in
  let* shard =
    match first_some overrides.shard below.shard with
    | Some (k, n) when not (1 <= k && k <= n) ->
        invalid ~source:"--shard" ~value:(Pp.str "%d/%d" k n)
          ~expected:shard_expected
    | picked -> Ok picked
  in
  Ok
    {
      Run.seed =
        Option.value
          (first_some overrides.seed below.seed)
          ~default:defaults.Run.seed;
      filter = first_some overrides.filter below.filter;
      exclude = first_some overrides.exclude below.exclude;
      tags = overrides.tags @ below.tags;
      exclude_tags = overrides.exclude_tags @ below.exclude_tags;
      shard;
      quick =
        Option.value (first_some overrides.quick below.quick) ~default:false;
      failed_only =
        Option.value
          (first_some overrides.failed_only below.failed_only)
          ~default:false;
      list_only =
        Option.value
          (first_some overrides.list_only below.list_only)
          ~default:false;
      bail;
      stream =
        Option.value (first_some overrides.stream below.stream) ~default:false;
      update =
        Option.value
          (first_some overrides.update below.update)
          ~default:Env.No_update;
      prune =
        Option.value (first_some overrides.prune below.prune) ~default:false;
      timeout;
      slow_threshold =
        Option.value slow_threshold ~default:defaults.Run.slow_threshold;
      prop_count;
      max_shrink;
      max_discard;
      max_prop_count;
      junit = first_some overrides.junit below.junit;
      color =
        Option.value
          (first_some overrides.color below.color)
          ~default:defaults.Run.color;
      columns = Env.columns ();
      tail_errors = Env.tail_errors ();
      log_dir =
        (* Resolved against the cwd once, here, before any test body runs.
           A relative [-o DIR] otherwise follows the process around: a test
           that chdirs sends the rest of the run's capture logs somewhere
           else, or nowhere, and the failure reports point at paths that do
           not exist. The default is already absolute. *)
        (let dir =
           Option.value
             (first_some overrides.log_dir below.log_dir)
             ~default:defaults.Run.log_dir
         in
         if not (Filename.is_relative dir) then dir
         else
           match Sys.getcwd () with
           | cwd -> Filename.concat cwd dir
           | exception Sys_error _ -> dir);
      allow_focus = Env.allow_focus ();
    }

(* The coverage mode resolves outside [resolve] because it is not run
   configuration: it is a rendering decision the facade's [run] applies
   after the run record is complete — the run's [config] carries no
   coverage field. Same precedence and same loudness as the config mirrors,
   because it is the same environment layer: CLI beats [WINDTRAP_COVERAGE];
   a malformed winning value is an error, never a silently defaulted mode. *)
let coverage_mode (cli : parsed) =
  let* below = layers ~overrides:empty cli in
  Ok (Option.value below.coverage ~default:`Summary)

(* Help *)

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
        Option.map (fun m -> (m.var, Pp.str "Mirror of %s" e.long)) e.mirror)
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
