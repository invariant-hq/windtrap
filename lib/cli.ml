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
  failed_only : bool option;
  list_only : bool option;
  bail : int option;
  stream : bool option;
  update : Env.update option;
  seed : Seed.seed option;
  timeout : float option;
  slow_threshold : float option;
  prop_count : int option;
  max_shrink : int option;
  verbose : bool option;
  junit : string option;
  color : Env.color_mode option;
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
    failed_only = None;
    list_only = None;
    bail = None;
    stream = None;
    update = None;
    seed = None;
    timeout = None;
    slow_threshold = None;
    prop_count = None;
    max_shrink = None;
    verbose = None;
    junit = None;
    color = None;
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
   it. Additive fields ([--tag], [--exclude-tag]) are never closed: every
   layer contributes. *)
type mirror = { var : string; reader : reader; absent : parsed -> bool }

type entry = {
  short : string option;
  long : string;
  arg : arg;
  doc : string;
  mirror : mirror option;
}

(* A table row: a flag with its optional mirror, or a setting only the
   environment can spell. One inventory drives parsing, [--help]'s two
   sections, and the environment layer — the flagless rows once lived in
   a second, hand-maintained list that could drift from what resolution
   actually read. *)
type item =
  | Flag_entry of entry
  | Env_setting of { var : string; doc : string }

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
    Flag_entry
      {
        short = Some "-f";
        long = "--filter";
        arg =
          set_string (fun ~source:_ acc value ->
              Ok { acc with filter = Some value });
        doc = "Run only tests whose path contains PATTERN";
        mirror = mirrored "WINDTRAP_FILTER" verbatim (fun p -> p.filter = None);
      };
    Flag_entry
      {
        short = Some "-e";
        long = "--exclude";
        arg =
          set_string (fun ~source:_ acc value ->
              Ok { acc with exclude = Some value });
        doc = "Skip tests whose path contains PATTERN";
        mirror =
          mirrored "WINDTRAP_EXCLUDE" verbatim (fun p -> p.exclude = None);
      };
    Flag_entry
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
    Flag_entry
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
    Flag_entry
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
    Flag_entry
      {
        short = None;
        long = "--failed";
        arg = Flag (fun acc -> { acc with failed_only = Some true });
        doc = "Rerun only the last run's failures";
        mirror =
          mirrored "WINDTRAP_FAILED" Truthy (fun p -> p.failed_only = None);
      };
    Flag_entry
      {
        short = Some "-l";
        long = "--list";
        arg = Flag (fun acc -> { acc with list_only = Some true });
        doc = "List selected tests without running them";
        mirror = None;
      };
    Flag_entry
      {
        short = Some "-x";
        long = "--fail-fast";
        arg = Flag (fun acc -> { acc with bail = Some 1 });
        doc = "Stop after the first failure (same as --bail 1)";
        mirror = None;
      };
    Flag_entry
      {
        short = None;
        long = "--bail";
        arg = set_positive_int (fun acc n -> { acc with bail = Some n });
        doc = "Stop after N failures";
        mirror = mirrored "WINDTRAP_BAIL" trimmed (fun p -> p.bail = None);
      };
    Flag_entry
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
    Flag_entry
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
                  | _ ->
                      invalid ~source ~value ~expected:"a non-negative number");
            };
        doc = "Warn when an untagged test runs longer than SECONDS (0 disables)";
        mirror =
          mirrored "WINDTRAP_SLOW_THRESHOLD" trimmed (fun p ->
              p.slow_threshold = None);
      };
    Flag_entry
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
    Flag_entry
      {
        short = None;
        long = "--prop-count";
        arg = set_positive_int (fun acc n -> { acc with prop_count = Some n });
        doc = "Generated cases per property";
        mirror =
          mirrored "WINDTRAP_PROP_COUNT" trimmed (fun p -> p.prop_count = None);
      };
    Flag_entry
      {
        short = None;
        long = "--max-shrink";
        arg = set_positive_int (fun acc n -> { acc with max_shrink = Some n });
        doc = "Accepted shrink steps per failing property";
        mirror =
          mirrored "WINDTRAP_MAX_SHRINK" trimmed (fun p -> p.max_shrink = None);
      };
    Flag_entry
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
    Flag_entry
      {
        short = Some "-s";
        long = "--stream";
        arg = Flag (fun acc -> { acc with stream = Some true });
        doc = "Stream test output instead of capturing it";
        mirror = mirrored "WINDTRAP_STREAM" Truthy (fun p -> p.stream = None);
      };
    Flag_entry
      {
        short = Some "-v";
        long = "--verbose";
        arg = Flag (fun acc -> { acc with verbose = Some true });
        doc = "One status line per test";
        mirror = mirrored "WINDTRAP_VERBOSE" Truthy (fun p -> p.verbose = None);
      };
    Flag_entry
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
    Flag_entry
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
                  | _ ->
                      invalid ~source ~value ~expected:"always, never or auto");
            };
        doc = "Color output: always, never or auto";
        mirror =
          mirrored "WINDTRAP_COLOR"
            (Own (fun acc -> { acc with color = Some (Env.color_mode ()) }))
            (fun p -> p.color = None);
      };
    Flag_entry
      {
        short = Some "-o";
        long = "--output";
        arg =
          Value
            {
              metavar = "DIR";
              set =
                (fun ~source:_ acc value ->
                  Ok { acc with log_dir = Some value });
            };
        doc = "Root directory for capture logs";
        mirror = mirrored "WINDTRAP_OUTPUT" verbatim (fun p -> p.log_dir = None);
      };
    Flag_entry
      {
        short = Some "-V";
        long = "--version";
        arg = Flag (fun acc -> { acc with version = true });
        doc = "Print the version and exit";
        mirror = None;
      };
    Flag_entry
      {
        short = Some "-h";
        long = "--help";
        arg = Flag (fun acc -> { acc with help = true });
        doc = "Print this help and exit";
        mirror = None;
      };
    (* The settings no flag can set, after the flags so [--help] lists
       them where the mirror rows end. Each is read where its owner
       consumes it: the first two by the resolution below,
       WINDTRAP_PROJECT_ROOT by [Path_ops], WINDTRAP_COVERAGE by
       [coverage_enabled], WINDTRAP_COVERAGE_ONLY by [Driver]'s coverage
       seam, and the mutation knobs by [mutation] and [Mutate_loop]. The
       arm row spells the runtime's own constant, so this roster, the
       reader and the report's [arm] line cannot name three different
       variables. *)
    Env_setting
      { var = "WINDTRAP_COLUMNS"; doc = "Terminal width override for reports" };
    Env_setting
      {
        var = "WINDTRAP_TAIL_ERRORS";
        doc = "Captured-output lines shown per failure";
      };
    Env_setting
      {
        var = "WINDTRAP_PROJECT_ROOT";
        doc = "Project root for snapshot path resolution";
      };
    Env_setting
      { var = "WINDTRAP_COVERAGE"; doc = "Inline coverage line: on or off" };
    Env_setting
      {
        var = "WINDTRAP_COVERAGE_ONLY";
        doc = "Source prefixes the coverage number covers";
      };
    Env_setting
      {
        var = "WINDTRAP_MUTATE";
        doc = "Mutation testing: 1, admit or off";
      };
    Env_setting
      {
        var = Windtrap_mutate.arm_variable;
        doc = "Arm one mutant, by identifier";
      };
    Env_setting
      {
        var = "WINDTRAP_MUTATE_TRY";
        doc = "Faults an admit run tries per test (0 for all)";
      };
    Env_setting
      {
        var = "WINDTRAP_MUTATE_ONLY";
        doc = "Source prefixes whose mutants a run considers";
      };
  ]

(* The flagless settings the resolution itself consumes, read through
   [Env]'s generic readers beside their rows above. Their tolerance is
   the setting's vocabulary — a non-positive or unparseable width counts
   as unset — where a mirror refuses loudly: no flag exists here for a
   lenient reading to drift from. *)

let columns () =
  match Env.get_int "WINDTRAP_COLUMNS" with
  | Some n when n > 0 -> Some n
  | _ -> None

let tail_errors () = Env.get_int "WINDTRAP_TAIL_ERRORS"

(* Did-you-mean

   Damerau-Levenshtein over the long flag names, bounded. Transposition
   counts as one edit because it is the typo people actually make:
   plain Levenshtein scores [--juint] two from both [--junit] and
   [--update], and the tie would be broken by table order.

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
    let closer best = function
      | Env_setting _ -> best
      | Flag_entry entry -> (
          let d = edit_distance flag entry.long in
          match best with
          | Some (_, best_d) when best_d <= d -> best
          | _ when d <= budget -> Some (entry.long, d)
          | _ -> best)
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

(* Parsing *)

let find_long name =
  List.find_map
    (function Flag_entry e when e.long = name -> Some e | _ -> None)
    table

let find_short name =
  List.find_map
    (function Flag_entry e when e.short = Some name -> Some e | _ -> None)
    table

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

(* The environment layer, folded out of the same table that drives parsing
   and [--help]: every WINDTRAP_* mirror is read here and nowhere else, and
   every value reaches [parsed] through its flag's own [arg]. That is what
   makes a mirror incapable of drifting from its flag — WINDTRAP_SHARD=9/2
   fails exactly as [--shard 9/2] does, because it runs the same [set], with
   the variable named as the source instead of the flag.

   [layers cli] is [cli] with each mirror filled into the fields the command
   line left open — the CLI and environment layers merged, in that
   precedence. A malformed value in a mirror that wins is [Error] naming the
   variable, and the fold stops there — never a silently defaulted run. *)
let layers cli =
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
    (fun acc item ->
      let* acc = acc in
      match item with
      | Env_setting _ -> Ok acc
      | Flag_entry entry -> (
          match entry.mirror with
          | Some mirror when mirror.absent acc -> (
              match Env.get_string mirror.var with
              | Some raw -> contribute acc entry mirror raw
              | None -> Ok acc)
          | Some _ | None -> Ok acc))
    (Ok cli) table

(* One fold from the fully-layered record to the two resolved records —
   the runner's configuration and the renderer's settings, split along
   the line the architecture draws: after it, no field is consulted by
   both sides. Nothing is range-checked here: every value arrived through
   its flag's own parser, the command line's or the mirror's, and each
   named its own source when it refused. *)
let resolved below =
  let defaults = Run.default_config () in
  let render_defaults = Render.default_settings in
  ( {
      Run.seed = Option.value below.seed ~default:defaults.Run.seed;
      filter = below.filter;
      exclude = below.exclude;
      tags = below.tags;
      exclude_tags = below.exclude_tags;
      shard = below.shard;
      failed_only = Option.value below.failed_only ~default:false;
      bail = below.bail;
      stream = Option.value below.stream ~default:false;
      update = Option.value below.update ~default:Env.No_update;
      timeout = below.timeout;
      prop_count = below.prop_count;
      max_shrink = below.max_shrink;
      log_dir =
        (* Resolved against the cwd once, here, before any test body runs.
           A relative [-o DIR] otherwise follows the process around: a test
           that chdirs sends the rest of the run's capture logs somewhere
           else, or nowhere, and the failure reports point at paths that do
           not exist. The default is already absolute. *)
        (let dir = Option.value below.log_dir ~default:defaults.Run.log_dir in
         if not (Filename.is_relative dir) then dir
         else
           match Sys.getcwd () with
           | cwd -> Filename.concat cwd dir
           | exception Sys_error _ -> dir);
      (* No flag and no mirror: only a forked mutation child sets it,
         through [Run.for_subset]. *)
      allow_focus = false;
    },
    {
      Render.color =
        Option.value below.color ~default:render_defaults.Render.color;
      columns = columns ();
      tail_errors = tail_errors ();
      slow_threshold =
        Option.value below.slow_threshold
          ~default:render_defaults.Render.slow_threshold;
    } )

(* WINDTRAP_COVERAGE: whether a run prints its inline coverage line.
   Environment only, and a boolean — the truthy and falsy spellings are
   [Env]'s shared ones, so it accepts what every other boolean variable
   accepts. It resolves apart from [resolve] because it is not run
   configuration but a rendering decision the drivers apply after the
   run record is complete, and with the same loudness: an unrecognized
   value is an error naming the variable, never a silently defaulted
   mode. The retired mode words land there too, and the message says
   where their output went — the per-file table and the excerpts are the
   reporting command's, over the merge of every executable's dumps
   rather than this one's view. *)
let coverage_enabled () =
  match Env.get_string "WINDTRAP_COVERAGE" with
  | None -> Ok true
  | Some value -> (
      match Env.get_bool "WINDTRAP_COVERAGE" with
      | Some enabled -> Ok enabled
      | None ->
          invalid ~source:"WINDTRAP_COVERAGE" ~value
            ~expected:
              "on or off; the per-file report is `windtrap coverage', its \
               uncovered excerpts `windtrap coverage -u'")

(* The mutation knobs

   Environment only, and deliberately so: the inline runner's argument
   parser accepts dune's inline-test protocol and nothing else, so a flag
   would exist for half the users. They resolve apart from [resolve] for
   the reason [coverage_enabled] does — none is run configuration and
   nothing in the runner may read them — but with the same loudness: an
   unrecognized value is an error naming the variable, never a silently
   defaulted mode. WINDTRAP_MUTATE's truthy and falsy spellings come from
   [Env]'s shared boolean reader, so it accepts exactly what every other
   boolean variable accepts, plus the mode words. The variable a mutant
   identifier travels in is the runtime's own constant, so the roster
   above, this reader and the report's [arm] line cannot name three
   different variables. *)

type mutation = {
  mode : [ `Unset | `Off | `Loop | `Admit ];
  arm : string option;
  tries : int;
}

let default_mutate_tries = 25

let mutation () =
  let* mode =
    match Env.get_string "WINDTRAP_MUTATE" with
    | None -> Ok `Unset
    | Some value -> (
        match String.lowercase_ascii (String.trim value) with
        | "admit" -> Ok `Admit
        | _ -> (
            match Env.get_bool "WINDTRAP_MUTATE" with
            | Some true -> Ok `Loop
            | Some false -> Ok `Off
            | None ->
                invalid ~source:"WINDTRAP_MUTATE" ~value
                  ~expected:"1, admit or off"))
  in
  let* tries =
    match Env.get_string "WINDTRAP_MUTATE_TRY" with
    | None -> Ok default_mutate_tries
    | Some value -> (
        match int_of_string_opt (String.trim value) with
        | Some n when n >= 0 -> Ok n
        | _ ->
            invalid ~source:"WINDTRAP_MUTATE_TRY" ~value
              ~expected:"a non-negative integer (0 tries every fault)")
  in
  Ok { mode; arm = Env.get_string Windtrap_mutate.arm_variable; tries }

(* One invocation, one resolution pass. Both drivers want all four
   answers and neither wants four error paths to reach them, so the
   environment layer is folded once and every answer is a projection of
   it. The four stay separate values in the result: coverage, verbosity
   and the renderer settings are rendering decisions, and folding any of
   them into [Run.config] would let a display choice reach the runner. *)
type settings = {
  config : Run.config;
  render : Render.settings;
  coverage : bool;
  output_level : [ `Compact | `Verbose ];
  junit : string option;
}

let settings cli =
  let* below = layers cli in
  let config, render = resolved below in
  let* coverage = coverage_enabled () in
  let output_level =
    if below.verbose = Some true then `Verbose else `Compact
  in
  Ok { config; render; coverage; output_level; junit = below.junit }

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
  let flag_rows =
    List.filter_map
      (function
        | Flag_entry e -> Some (flag_heading e, e.doc) | Env_setting _ -> None)
      table
  in
  let mirror_rows =
    List.filter_map
      (function
        | Flag_entry e ->
            Option.map (fun m -> (m.var, Pp.str "Mirror of %s" e.long)) e.mirror
        | Env_setting { var; doc } -> Some (var, doc))
      table
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
