(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   The flag inventory and the CLI > env > default precedence derive from
   windtrap v1's lib/cli.ml, rebuilt as one declarative flag table that
   generates parsing, --help, and the environment layer.
  ---------------------------------------------------------------------------*)

(* Parsed flags *)

type parsed = {
  filter : string list;
  exclude : string list;
  tags : string list;
  exclude_tags : string list;
  shard : (int * int) option;
  failed_only : bool option;
  list_only : bool option;
  bail : bool option;
  stream : bool option;
  update : bool option;
  corrected : bool option;
  seed : Seed.seed option;
  timeout : float option;
  slow_threshold : float option;
  prop_count : int option;
  verbose : bool option;
  junit : string option;
  color : Os.color_mode option;
  log_dir : string option;
  mutate : string list option;
  arm : string option;
  help : bool;
  version : bool;
}

let empty =
  {
    filter = [];
    exclude = [];
    tags = [];
    exclude_tags = [];
    shard = None;
    failed_only = None;
    list_only = None;
    bail = None;
    stream = None;
    update = None;
    corrected = None;
    seed = None;
    timeout = None;
    slow_threshold = None;
    prop_count = None;
    verbose = None;
    junit = None;
    color = None;
    log_dir = None;
    mutate = None;
    arm = None;
    help = false;
    version = false;
  }

(* Errors *)

type error =
  | Unknown_flag of string
  | Missing_value of string
  | Invalid_value of { source : string; value : string; expected : string }
  | Incompatible_flags of string * string

(* The argument grammar *)

(* What a flag takes on the command line. [Flag] takes nothing. [Value]
   takes one argument — the next one, or inline as [--flag=value]. An
   [Optional_value] flag takes an inline value or none: [--flag] alone is
   the bare form and never consumes the next argument, so a value attaches
   only with [=] ([--mutate[=PREFIX,...]]). *)
type arg =
  | Flag of (parsed -> parsed)
  | Value of {
      metavar : string;
      set : source:string -> parsed -> string -> (parsed, error) result;
    }
  | Optional_value of {
      metavar : string;
      set : source:string -> parsed -> string option -> (parsed, error) result;
    }

(* How a flag's WINDTRAP_* mirror layers under the command line. [Single
   absent]: the first layer that gives the flag decides it — the mirror
   is read only while [absent p], and a mirror whose flag the command
   line already decided is never even parsed, which is what
   lets a valid [--timeout] shadow a malformed WINDTRAP_TIMEOUT.
   [Repeatable]: every layer contributes ([--tag], [--exclude-tag]), and
   the variable holds a comma-separated list, one token per item. *)
type layering = Single of (parsed -> bool) | Repeatable
type mirror = { var : string; layering : layering }

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

let mirrored var absent = Some { var; layering = Single absent }
let repeatable var = Some { var; layering = Repeatable }
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

let color_expected = "always, never or auto"

(* The one [--color] parser: the flag, its mirror and the flagless
   commands' read of WINDTRAP_COLOR all go through it, so an unknown word
   is refused everywhere alike — never read as [auto]. *)
let color_of_string ~source value =
  match Os.color_mode_of_string value with
  | Some mode -> Ok mode
  | None -> invalid ~source ~value ~expected:color_expected

let table =
  [
    (* The patterns repeat as the tags do, but their mirrors hold one
       pattern each, because a test name may hold a comma: the command
       line's patterns replace the mirror's instead of adding to it. *)
    Flag_entry
      {
        short = Some "-f";
        long = "--filter";
        arg =
          set_string (fun ~source:_ acc value ->
              Ok { acc with filter = acc.filter @ [ value ] });
        doc =
          "Run only tests whose path contains PATTERN (repeatable: any of \
           them).";
        mirror = mirrored "WINDTRAP_FILTER" (fun p -> p.filter = []);
      };
    Flag_entry
      {
        short = Some "-e";
        long = "--exclude";
        arg =
          set_string (fun ~source:_ acc value ->
              Ok { acc with exclude = acc.exclude @ [ value ] });
        doc = "Skip tests whose path contains PATTERN (repeatable).";
        mirror = mirrored "WINDTRAP_EXCLUDE" (fun p -> p.exclude = []);
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
        doc = "Run only tests tagged LABEL (repeatable: all of them).";
        mirror = repeatable "WINDTRAP_TAG";
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
        doc = "Skip tests tagged LABEL (repeatable).";
        mirror = repeatable "WINDTRAP_EXCLUDE_TAG";
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
        doc = "Run only the Kth of N deterministic path-hash buckets.";
        mirror = mirrored "WINDTRAP_SHARD" (fun p -> p.shard = None);
      };
    (* The three feedback-loop flags have no mirror: they want a command
       line. Under [dune runtest] a variable would not help: dune runs a
       failed action again on every invocation anyway, and it keeps a
       passed one cached whatever variable is set, since it tracks none it
       was not told of. A listing is not a test run. On a directly executed
       binary the loop is real. *)
    Flag_entry
      {
        short = None;
        long = "--failed";
        arg = Flag (fun acc -> { acc with failed_only = Some true });
        doc = "Rerun only the last run's failures.";
        mirror = None;
      };
    Flag_entry
      {
        short = Some "-l";
        long = "--list";
        arg = Flag (fun acc -> { acc with list_only = Some true });
        doc = "List selected tests without running them.";
        mirror = None;
      };
    Flag_entry
      {
        short = Some "-x";
        long = "--fail-fast";
        arg = Flag (fun acc -> { acc with bail = Some true });
        doc = "Stop after the first failure.";
        mirror = None;
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
        doc = "Default per-test timeout in seconds.";
        mirror = mirrored "WINDTRAP_TIMEOUT" (fun p -> p.timeout = None);
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
        doc =
          "Warn when an untagged test runs longer than SECONDS (0 disables).";
        mirror =
          mirrored "WINDTRAP_SLOW_THRESHOLD" (fun p -> p.slow_threshold = None);
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
        doc = "Root seed for property tests (s1:<16 hex>).";
        mirror = mirrored "WINDTRAP_SEED" (fun p -> p.seed = None);
      };
    Flag_entry
      {
        short = None;
        long = "--prop-count";
        arg = set_positive_int (fun acc n -> { acc with prop_count = Some n });
        doc = "Generated cases per property.";
        mirror = mirrored "WINDTRAP_PROP_COUNT" (fun p -> p.prop_count = None);
      };
    (* Acceptance has no mirror, deliberately: a build action must never
       accept a baseline because of a variable in its environment. Under
       dune the acceptance is a [--corrected] run followed by
       [dune promote]; [-u] is for a command line. *)
    Flag_entry
      {
        short = Some "-u";
        long = "--update";
        arg = Flag (fun acc -> { acc with update = Some true });
        doc = "Accept baseline changes in place (refused under CI).";
        mirror = None;
      };
    Flag_entry
      {
        short = None;
        long = "--corrected";
        arg = Flag (fun acc -> { acc with corrected = Some true });
        doc = "Write corrections as <file>.corrected, for dune promote.";
        mirror = None;
      };
    Flag_entry
      {
        short = Some "-s";
        long = "--stream";
        arg = Flag (fun acc -> { acc with stream = Some true });
        doc = "Stream test output instead of capturing it.";
        mirror = mirrored "WINDTRAP_STREAM" (fun p -> p.stream = None);
      };
    Flag_entry
      {
        short = Some "-v";
        long = "--verbose";
        arg = Flag (fun acc -> { acc with verbose = Some true });
        doc = "One status line per test.";
        mirror = mirrored "WINDTRAP_VERBOSE" (fun p -> p.verbose = None);
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
        doc = "Also write a JUnit XML report to PATH.";
        mirror = mirrored "WINDTRAP_JUNIT" (fun p -> p.junit = None);
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
                  match color_of_string ~source value with
                  | Ok mode -> Ok { acc with color = Some mode }
                  | Error _ as error -> error);
            };
        doc = "Color output: always, never or auto.";
        mirror = mirrored "WINDTRAP_COLOR" (fun p -> p.color = None);
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
        doc = "Root directory for capture logs.";
        mirror = mirrored "WINDTRAP_OUTPUT" (fun p -> p.log_dir = None);
      };
    (* The mutation switches. Bare, [--mutate] surveys every mutant this
       executable catalogues; with a value, only those whose recorded
       source path starts with one of the prefixes — the loop forks once
       per mutant, so the scope narrows the work. Its mirror reads both
       ways: WINDTRAP_MUTATE=1 is the bare flag, 0 its absence, and
       anything else the prefixes. [--arm] runs the suite once with one
       mutant armed, which is what a survivor block's reproduce line asks
       for; its identifier is handed to the runtime unparsed. Both at once
       is refused at resolution, whichever layer each arrived by. *)
    Flag_entry
      {
        short = None;
        long = "--mutate";
        arg =
          Optional_value
            {
              metavar = "PREFIX,...";
              set =
                (fun ~source:_ acc value ->
                  let prefixes =
                    match value with
                    | None -> []
                    | Some value -> Os.split_comma value
                  in
                  Ok { acc with mutate = Some prefixes });
            };
        doc = "Test this executable's mutants, all or those under PREFIX.";
        mirror = mirrored "WINDTRAP_MUTATE" (fun p -> p.mutate = None);
      };
    Flag_entry
      {
        short = None;
        long = "--arm";
        arg =
          Value
            {
              metavar = "ID";
              set =
                (fun ~source:_ acc value -> Ok { acc with arm = Some value });
            };
        doc = "Run once with mutant ID armed.";
        mirror = mirrored "WINDTRAP_MUTATE_ARM" (fun p -> p.arm = None);
      };
    Flag_entry
      {
        short = Some "-V";
        long = "--version";
        arg = Flag (fun acc -> { acc with version = true });
        doc = "Print the version and exit.";
        mirror = None;
      };
    Flag_entry
      {
        short = Some "-h";
        long = "--help";
        arg = Flag (fun acc -> { acc with help = true });
        doc = "Print this help and exit.";
        mirror = None;
      };
    (* The settings no flag can set, after the flags so [--help] lists
       them where the flag rows end. Each is read where its owner
       consumes it: WINDTRAP_PROJECT_ROOT by [Os], and
       WINDTRAP_COVERAGE_FILE by the coverage runtime when the first
       instrumented module registers. *)
    Env_setting
      {
        var = "WINDTRAP_PROJECT_ROOT";
        doc = "Project root that baseline paths resolve under.";
      };
    Env_setting
      {
        var = "WINDTRAP_COVERAGE_FILE";
        doc = "Where an instrumented run writes its coverage dump.";
      };
    (* Not windtrap's, and last for that reason: the de-facto standard
       every command-line tool honours. Rostered so --color's reader can
       find out here that something else can turn styling off. *)
    Env_setting
      {
        var = "NO_COLOR";
        doc = "Any value: never style output (--color auto).";
      };
  ]

let entries =
  List.filter_map
    (function Flag_entry e -> Some e | Env_setting _ -> None)
    table

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
        (if i > 1 && j > 1 && a.[i - 1] = b.[j - 2] && a.[i - 2] = b.[j - 1]
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
    Option.map fst (List.fold_left closer None entries)

let error_message = function
  | Unknown_flag flag -> (
      let base = Pp.str "unknown option '%s'" flag in
      match nearest_flag flag with
      | Some name -> Pp.str "%s; did you mean '%s'?" base name
      | None -> base)
  | Missing_value flag -> Pp.str "option '%s' requires an argument" flag
  | Invalid_value { source; value; expected } ->
      Pp.str "invalid value '%s' for %s: expected %s" value source expected
  | Incompatible_flags (first, second) ->
      Pp.str "options '%s' and '%s' cannot be combined" first second

(* Parsing *)

let find_long entries name = List.find_opt (fun e -> e.long = name) entries

let find_short entries name =
  List.find_opt (fun e -> e.short = Some name) entries

(* Split "--flag=value" into the flag and its inline value. *)
let split_inline arg =
  match String.index_from_opt arg 2 '=' with
  | None -> (arg, None)
  | Some eq ->
      ( String.sub arg 0 eq,
        Some (String.sub arg (eq + 1) (String.length arg - eq - 1)) )

let ( let* ) = Result.bind
let add_positional acc value = { acc with filter = acc.filter @ [ value ] }

let rec parse_args entries acc = function
  | [] -> Ok acc
  | "--" :: rest -> Ok (List.fold_left add_positional acc rest)
  | arg :: rest when String.length arg > 2 && String.sub arg 0 2 = "--" ->
      let name, inline = split_inline arg in
      apply entries (find_long entries name) ~source:name ~inline acc rest
  | arg :: rest when String.length arg > 1 && arg.[0] = '-' ->
      apply entries (find_short entries arg) ~source:arg ~inline:None acc rest
  | arg :: rest -> parse_args entries (add_positional acc arg) rest

and apply entries entry ~source ~inline acc rest =
  match entry with
  | None -> Error (Unknown_flag source)
  | Some { arg = Flag set; _ } -> (
      match inline with
      | Some value -> invalid ~source ~value ~expected:"no argument"
      | None ->
          let acc = set acc in
          (* --help and --version win immediately; later flags are unread. *)
          if acc.help || acc.version then Ok acc
          else parse_args entries acc rest)
  | Some { arg = Value { set; _ }; _ } -> (
      match inline with
      | Some value ->
          let* acc = set ~source acc value in
          parse_args entries acc rest
      | None -> (
          match rest with
          | [] -> Error (Missing_value source)
          | value :: rest ->
              let* acc = set ~source acc value in
              parse_args entries acc rest))
  | Some { arg = Optional_value { set; _ }; _ } ->
      (* The value attaches only inline: [--flag next] leaves [next] alone,
         so a bare flag before a positional keeps meaning what it says. *)
      let* acc = set ~source acc inline in
      parse_args entries acc rest

let parse_entries entries argv =
  match Array.to_list argv with
  | [] -> Ok empty
  | _prog :: args -> (
      let* parsed = parse_args entries empty args in
      (* Two acceptances at once say two different things about where
         the produced text goes. *)
      match (parsed.update, parsed.corrected) with
      | Some true, Some true -> Error (Incompatible_flags ("-u", "--corrected"))
      | _ -> Ok parsed)

let parse argv = parse_entries entries argv

(* Resolution *)

(* The environment layer: every WINDTRAP_* mirror is read here and nowhere
   else, and every value reaches [parsed] through its flag's own parser
   with the variable as the source. That is what makes a mirror incapable
   of drifting from its flag — WINDTRAP_SHARD=9/2 fails exactly as
   [--shard 9/2] does, because it runs the same [set].

   Two reading rules turn the variable into what the parser takes, plus
   the grammar addition. A value flag's mirror is one token, trimmed, so
   " 2/4 " works as a shard; a repeatable flag's is a comma-separated
   list, one token per item. A valueless flag's mirror is a boolean:
   [true] applies the flag, [false] is what an unset variable is, and
   anything else is refused — a typo must not read as "off". An
   optional-value flag's mirror reads both ways: a boolean is the bare
   flag or its absence, anything else is the value. *)
let contribute acc entry mirror raw =
  let source = mirror.var in
  match (entry.arg, mirror.layering) with
  | Flag set, _ -> (
      match Os.bool_of_string raw with
      | Some true -> Ok (set acc)
      | Some false -> Ok acc
      | None -> invalid ~source ~value:raw ~expected:Os.bool_expected)
  | Value { set; _ }, Single _ -> set ~source acc (String.trim raw)
  | Value { set; _ }, Repeatable ->
      List.fold_left
        (fun acc token ->
          let* acc = acc in
          set ~source acc token)
        (Ok acc) (Os.split_comma raw)
  | Optional_value { set; _ }, _ -> (
      match Os.bool_of_string raw with
      | Some true -> set ~source acc None
      | Some false -> Ok acc
      | None -> set ~source acc (Some (String.trim raw)))

(* [layer_entries entries cli] is [cli] with each mirror filled into the
   fields the command line left open — the CLI and environment layers
   merged, in that precedence. A malformed value in a mirror that wins is
   [Error] naming the variable, and the fold stops there — never a
   silently defaulted run. *)
let layer_entries entries cli =
  List.fold_left
    (fun acc entry ->
      let* acc = acc in
      let open_mirror =
        match entry.mirror with
        | None -> None
        | Some ({ layering = Single absent; _ } as mirror) ->
            if absent acc then Some mirror else None
        | Some ({ layering = Repeatable; _ } as mirror) -> Some mirror
      in
      match open_mirror with
      | None -> Ok acc
      | Some mirror -> (
          match Os.getenv mirror.var with
          | Some raw -> contribute acc entry mirror raw
          | None -> Ok acc))
    (Ok cli) entries

let layers cli = layer_entries entries cli

(* One fold from the fully-layered record to the one resolved record.
   Nothing is range-checked here: every value arrived through its flag's
   own parser, the command line's or the mirror's, and each named its own
   source when it refused. *)
let resolved below ~mutation =
  let defaults = Run.default_config () in
  {
    Run.seed = Option.value below.seed ~default:defaults.Run.seed;
    filter = below.filter;
    exclude = below.exclude;
    tags = below.tags;
    exclude_tags = below.exclude_tags;
    shard = below.shard;
    failed_only = Option.value below.failed_only ~default:false;
    bail = Option.value below.bail ~default:false;
    stream = Option.value below.stream ~default:false;
    baseline =
      (match (below.update, below.corrected) with
      | Some true, _ -> Baseline.Update
      | _, Some true -> Baseline.Corrected
      | _ -> Baseline.Check);
    timeout = below.timeout;
    prop_count = below.prop_count;
    log_dir =
      (* Resolved against the cwd once, here, before any test body runs.
           A relative [-o DIR] otherwise follows the process around: a test
           that chdirs sends the rest of the run's capture logs somewhere
           else, or nowhere, and the failure reports point at paths that do
           not exist. The default is already absolute. *)
      (let dir = Option.value below.log_dir ~default:(Os.default_log_dir ()) in
       if not (Filename.is_relative dir) then dir
       else
         match Sys.getcwd () with
         | cwd -> Filename.concat cwd dir
         | exception Sys_error _ -> dir);
    (* No flag and no mirror: only a forked mutation child sets it,
         through [Run.for_subset]. *)
    allow_focus = false;
    color = Option.value below.color ~default:defaults.Run.color;
    slow_threshold =
      Option.value below.slow_threshold ~default:defaults.Run.slow_threshold;
    verbose = below.verbose = Some true;
    junit = below.junit;
    mutation;
    github = Os.in_github_actions ();
    (* Computed from argv by the facade, which alone holds it. *)
    invocation = `Mirrors;
  }

(* The mutation switches, after both layers: the loop arms each mutant
   itself, so an armed parent would mutate its own dry run — asking for
   both is refused, whether each came from its flag or its mirror. *)
let mutation_of below =
  match (below.mutate, below.arm) with
  | None, None -> Ok Run.No_mutation
  | Some prefixes, None -> Ok (Run.Loop prefixes)
  | None, Some id -> Ok (Run.Armed id)
  | Some _, Some _ -> Error (Incompatible_flags ("--mutate", "--arm"))

(* WINDTRAP_COLOR for a command with no [--color] flag: the same parser
   as the flag and its mirror, so the variable means one thing everywhere
   and a bad value is refused everywhere. *)
let color_mode () =
  match Os.getenv "WINDTRAP_COLOR" with
  | None -> Ok Os.Auto
  | Some value -> color_of_string ~source:"WINDTRAP_COLOR" (String.trim value)

(* One invocation, one resolution pass: the environment layer is folded
   once and the one record is a projection of it. *)
let settings cli =
  let* below = layers cli in
  let* mutation = mutation_of below in
  Ok (resolved below ~mutation)

(* Help *)

let usage ~prog =
  Pp.str "usage: %s [OPTIONS] [PATTERN...]" (Filename.basename prog)

(* cmdliner's spelling, which [split_inline] accepts: a value follows its
   short name after a space and its long name after [=]. *)
let flag_heading entry =
  let names =
    match (entry.arg, entry.short) with
    | Flag _, Some short -> Pp.str "%s, %s" short entry.long
    | Value { metavar; _ }, Some short ->
        Pp.str "%s %s, %s=%s" short metavar entry.long metavar
    | Value { metavar; _ }, None -> Pp.str "%s=%s" entry.long metavar
    | Optional_value { metavar; _ }, Some short ->
        Pp.str "%s, %s[=%s]" short entry.long metavar
    | Optional_value { metavar; _ }, None -> Pp.str "%s[=%s]" entry.long metavar
    | Flag _, None -> entry.long
  in
  match entry.mirror with
  | None -> names
  | Some { var; _ } -> Pp.str "%s (env %s)" names var

(* [text] filled greedily into lines of at most 80 columns, each behind
   [indent] spaces. A word is never split. *)
let fill ~indent text =
  let margin = String.make indent ' ' in
  let lines, last =
    List.fold_left
      (fun (lines, line) word ->
        if line = "" then (lines, margin ^ word)
        else if Text.length_utf8 line + 1 + Text.length_utf8 word <= 80 then
          (lines, line ^ " " ^ word)
        else (line :: lines, margin ^ word))
      ([], "")
      (List.filter (fun word -> word <> "") (String.split_on_char ' ' text))
  in
  List.rev (if last = "" then lines else last :: lines)

(* One entry: its heading, its sentences indented under it, a blank line. *)
let described ~heading doc = (("  " ^ heading) :: fill ~indent:6 doc) @ [ "" ]

let help ~prog =
  let options =
    List.concat_map (fun e -> described ~heading:(flag_heading e) e.doc) entries
  in
  let settings =
    List.concat_map
      (function
        | Env_setting { var; doc } -> described ~heading:var doc
        | Flag_entry _ -> [])
      table
  in
  String.concat "\n"
    ([ Pp.str "%s - windtrap test runner" (Filename.basename prog); "" ]
    @ [ usage ~prog; "" ]
    @ fill ~indent:0
        "Each bare PATTERN is read as one -f PATTERN. (env VAR) after an \
         option names the variable that sets it for a run with no command \
         line, such as dune runtest."
    @ [ ""; "OPTIONS:" ] @ options
    @ ("ENVIRONMENT (no flag):" :: settings))
