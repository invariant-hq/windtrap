(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Command-line and environment resolution into the run configuration.

    One declarative flag table drives everything here: {!parse} reads an
    argument vector into a {!type:parsed} record of raw flag values, {!settings}
    merges programmatic overrides, parsed flags, and the [WINDTRAP_*]
    environment mirrors into a {!Run.config} and the two rendering decisions
    kept out of it — with the precedence {e programmatic > CLI > env > default}
    (under [dune runtest] the environment mirrors {e are} the CLI) — and {!help}
    renders the flag and variable inventory. {!settings} is the one call a
    driver makes; {!resolve}, {!coverage_mode} and {!output_level} are its
    layers, documented and testable on their own.

    A flag's mirror is declared in that table beside the flag, and its value is
    applied through the flag's own parser, so the two cannot drift: a variable
    accepts exactly what its flag accepts, refuses exactly what its flag
    refuses, with the same [expected] wording, and differs only in naming the
    variable rather than the flag as the source of a bad value. {!Env} is
    consulted for the reading, not for the inventory — the settings it still
    owns outright are the ones no flag can set ([WINDTRAP_ALLOW_FOCUS],
    [WINDTRAP_COLUMNS], [WINDTRAP_TAIL_ERRORS], [WINDTRAP_PROJECT_ROOT]) and the
    two vocabularies wider than their flag's ([WINDTRAP_UPDATE]'s [force],
    [WINDTRAP_COLOR]'s lenient fall back to {!Env.Auto}).

    Nothing in this module prints or exits: parse and resolution failures are
    returned as a typed {!type:error} — the caller renders {!error_message} and
    exits [2] — and [--help]/[--version] come back as flags on {!type:parsed}
    for the caller to act on. The flag inventory is v1's minus the cut
    [--format] axis — terminal verbosity is one three-level axis ([-q] ⊂ default
    ⊂ [-v], {!output_level}), not a format — plus [--quiet], [--verbose],
    [--prune], [--strict-snapshots] and [--shard]. *)

(** {1:parsed Parsed flags} *)

type parsed = {
  filter : string option;
      (** [-f PATTERN], [--filter PATTERN], or the positional argument: run only
          tests whose full path contains [PATTERN]. *)
  exclude : string option;
      (** [-e PATTERN], [--exclude PATTERN]: skip tests whose full path contains
          [PATTERN]. *)
  tags : string list;
      (** [--tag LABEL], repeatable: required tags, in the order given. *)
  exclude_tags : string list;
      (** [--exclude-tag LABEL], repeatable: dropped tags, in the order given.
      *)
  shard : (int * int) option;
      (** [--shard K/N]: run only tests whose path hashes into bucket [K] of
          [N]. [K] and [N] are plain decimal numerals; parses only with
          [1 <= K <= N]. *)
  quick : bool option;  (** [--quick]: skip slow-tagged tests. *)
  failed_only : bool option;
      (** [--failed]: rerun only the last run's recorded failures. *)
  list_only : bool option;
      (** [-l], [--list]: list selected tests without running them. *)
  bail : int option;
      (** [--bail N]: stop after [N] failures. [-x]/[--fail-fast] parse as
          [Some 1]. *)
  stream : bool option;
      (** [-s], [--stream]: run against the real descriptors instead of
          capturing. *)
  update : Env.update option;
      (** [-u], [--update]: parse as [Some Env.Update]. Forcing past the CI
          guard is spelled [WINDTRAP_UPDATE=force]. *)
  prune : bool option;
      (** [--prune]: delete orphaned baselines after a full, clean update run.
      *)
  strict_snapshots : bool option;
      (** [--strict-snapshots]: fail the run on a baseline still stale after a
          full, clean run. *)
  seed : Seed.seed option;
      (** [--seed TOKEN]: the root seed, an [s1:] token parsed by
          {!Seed.of_string}. *)
  timeout : float option;
      (** [--timeout SECONDS]: default per-test limit; must be positive. *)
  slow_threshold : float option;
      (** [--slow-threshold SECONDS]: seconds an untagged test may take before
          the compact renderer flags the run and warns; must be non-negative,
          [0] disables. *)
  prop_count : int option;
      (** [--prop-count N]: generated cases per property; must be positive. *)
  max_shrink : int option;
  max_discard : int option;
  max_prop_count : int option;
      (** [--max-shrink N]: accepted shrink steps per failing property; must be
          positive. *)
  output : [ `Quiet | `Verbose ] option;
      (** [-q]/[--quiet] parse as [Some `Quiet], [-v]/[--verbose] as
          [Some `Verbose]. One field for one axis: mixing or repeating the flags
          is last-one-wins, like every single-valued flag. [None] is the compact
          default (see {!output_level}). *)
  junit : string option;  (** [--junit PATH]: also write JUnit XML to [PATH]. *)
  color : Env.color_mode option;
      (** [--color MODE]: [always], [never], or [auto]. *)
  coverage : [ `Summary | `Report | `Full | `Off ] option;
      (** [--coverage MODE]: the coverage rendering mode, resolved by
          {!coverage_mode} — [summary], [report], [full], or [off]. *)
  log_dir : string option;
      (** [-o DIR], [--output DIR]: root directory for capture logs. *)
  help : bool;  (** [-h], [--help]: the caller prints {!help} and exits [0]. *)
  version : bool;
      (** [-V], [--version]: the caller prints its version and exits [0]. *)
}
(** The type for raw parse results: one field per flag, [None] (or [[]], or
    [false] for {!parsed.help} and {!parsed.version}) when the flag was absent.
    Also the shape of {!resolve}'s programmatic overrides. *)

val empty : parsed
(** [empty] is the record with every flag absent. *)

(** {1:errors Errors} *)

(** The type for parse and resolution errors. [source] names the flag as typed
    ([--seed]) or the environment variable ([WINDTRAP_SEED]) that carried the
    offending value. *)
type error =
  | Unknown_flag of string  (** The flag is not in the inventory. *)
  | Missing_value of string
      (** The flag requires an argument; none was left. *)
  | Invalid_value of { source : string; value : string; expected : string }
      (** The argument did not parse; [expected] describes the accepted form. *)
  | Extra_positional of { filter : string; extra : string }
      (** A second positional argument [extra] arrived with the filter already
          set to [filter]. *)

val error_message : error -> string
(** [error_message error] is a one-line description of [error] for users, naming
    the offending flag or variable. Not stable for programmatic matching. *)

(** {1:parsing Parsing} *)

val parse : string array -> (parsed, error) result
(** [parse argv] reads the argument vector [argv] — [argv.(0)] is the program
    name and is ignored — into a {!type:parsed} record, or is [Error error] on
    the first flag that fails.

    Repeated single-valued flags keep the last occurrence; [--tag] and
    [--exclude-tag] accumulate in order. Long flags also accept the
    [--flag=value] spelling. The first bare argument becomes {!parsed.filter} (a
    second one is {!Extra_positional}); arguments after a [--] separator are all
    treated as positionals. Parsing stops at [-h]/[--help] and [-V]/[--version]:
    flags after them are not validated. *)

(** {1:resolution Resolution} *)

val resolve : ?overrides:parsed -> parsed -> (Run.config, error) result
(** [resolve ~overrides cli] is the run configuration obtained by taking, for
    each field, the first value present in [overrides] (programmatic, defaults
    to {!empty}), then [cli], then the field's [WINDTRAP_*] environment mirror
    ({!Env}), then {!Run.default_config} — except [tags] and [exclude_tags],
    which are additive across all three layers, overrides first. The env-only
    settings ([WINDTRAP_ALLOW_FOCUS], [WINDTRAP_COLUMNS],
    [WINDTRAP_TAIL_ERRORS]) are filled from the environment alone.

    Effects: reads the environment, and draws a fresh root seed ({!Seed.random})
    when no layer provides one.

    [Error (Invalid_value _)] with source [WINDTRAP_SEED] when the seed falls
    through to a malformed environment token; a well-formed [overrides] or [cli]
    seed leaves the variable unparsed. [WINDTRAP_SHARD] and the numeric mirrors
    [WINDTRAP_TIMEOUT], [WINDTRAP_SLOW_THRESHOLD], [WINDTRAP_PROP_COUNT] and
    [WINDTRAP_MAX_SHRINK] are treated the same way, and by the same code: a
    mirror is read through its flag's parser, so a value the flag would reject
    is an error naming the variable, never silently ignored — a misread shard
    would silently rerun the whole suite in every bucket, and a misread count or
    limit would silently run with the default. A mirror whose flag a higher
    layer already decided is not even parsed.

    The winning [timeout], [prop_count], [max_shrink] and [bail] must be
    positive ([timeout] finite as well), the winning [slow_threshold] must be
    finite and non-negative, and the winning [shard] must satisfy [1 <= K <= N].
    {!parse} and the mirrors enforce this already, each naming its own source; a
    violation that arrives through [overrides] — the one layer with no parser
    between it and the run — is [Error (Invalid_value _)] naming the flag
    spelling, never a config that detonates mid-run. {!parsed.help} and
    {!parsed.version} are ignored — acting on them is the caller's job. *)

val coverage_mode :
  parsed -> ([ `Summary | `Report | `Full | `Off ], error) result
(** [coverage_mode cli] is the coverage rendering mode: [cli]'s
    {!parsed.coverage} when present, else the [WINDTRAP_COVERAGE] environment
    mirror, else [`Summary]. Resolved apart from {!resolve} because it is a
    rendering decision, not run configuration — {!Run.config} carries no
    coverage field, and enabling any mode never changes outcomes or exit codes.
    The caller applies it: [`Summary] renders the one-line percentage when the
    run was instrumented, [`Report] and [`Full] add the per-file detail, [`Off]
    renders nothing.

    Effects: reads the environment when {!parsed.coverage} is [None].
    [Error (Invalid_value _)] with source [WINDTRAP_COVERAGE] when the winning
    environment value is not one of [summary], [report], [full], [off]. It
    builds the same environment layer {!resolve} does, so a malformed value in
    any {e other} winning mirror is reported here too; callers resolve the
    configuration first and exit on that error, which is where it belongs. *)

type mutation = {
  mode : [ `Off | `Loop | `Report ];
      (** [WINDTRAP_MUTATE]: [`Loop] for a mutation run ([1] and the other
          truthy spellings), [`Report] for [report], [`Off] for a falsy spelling
          or an unset variable. *)
  arm : string option;
      (** [WINDTRAP_MUTATE_ARM]: the mutant identifier to arm, unparsed —
          {!Windtrap_mutate.selector_of_string} owns that grammar and reports
          its own errors. [None] when the variable is unset or empty. *)
  limit : int;
      (** [WINDTRAP_MUTATE_LIMIT]: survivor blocks to print, [0] for all.
          Defaults to [10]. *)
}
(** The type for the mutation knobs, which are environment variables only: the
    inline runner's argument parser accepts dune's inline-test protocol and
    nothing else, so a flag would exist for half the users. *)

val mutation : unit -> (mutation, error) result
(** [mutation ()] reads the three mutation variables. Resolved apart from
    {!resolve} like {!coverage_mode}, and for the same reason — none of them is
    run configuration, and nothing in the runner may read them — with the same
    loudness: [Error (Invalid_value _)] naming [WINDTRAP_MUTATE] or
    [WINDTRAP_MUTATE_LIMIT] when its value is not one the variable accepts,
    never a silently defaulted mode.

    [WINDTRAP_MUTATE_JOBS] and [WINDTRAP_MUTATE_TIMEOUT] are specified but do
    not ship yet, and are deliberately not read here: a knob that is read and
    ignored is worse than one that is not read.

    Effects: reads the environment. *)

val output_level :
  ?overrides:parsed -> parsed -> [ `Quiet | `Compact | `Verbose ]
(** [output_level ~overrides cli] is the terminal verbosity level: the first
    {!parsed.output} present in [overrides] (defaults to {!empty}) then [cli],
    else the [WINDTRAP_QUIET]/[WINDTRAP_VERBOSE] environment mirrors (boolean
    spellings, as [WINDTRAP_STREAM]; when both are truthy, verbose wins — the
    variables carry no order for last-one-wins), else [`Compact]. Resolved apart
    from {!resolve} like {!coverage_mode}, because it is a rendering decision,
    not run configuration — {!Run.config} carries no verbosity field. Levels
    never change outcomes or exit codes; the renderer projects the same run data
    at every level.

    Effects: reads the environment when no layer above it decides. Never errors:
    unparseable boolean values count as unset, and a malformed value in some
    other mirror — which stops the shared environment layer short — leaves the
    level at what the layers above the environment say, the caller's {!resolve}
    having reported that error already. *)

type settings = {
  config : Run.config;  (** The run configuration ({!resolve}). *)
  coverage_mode : [ `Summary | `Report | `Full | `Off ];
      (** The coverage rendering mode ({!coverage_mode}). *)
  output_level : [ `Quiet | `Compact | `Verbose ];
      (** The terminal verbosity level ({!output_level}). *)
}
(** The type for everything one invocation resolves to. Three fields, not one
    configuration: the two rendering decisions stay {e out} of {!Run.config},
    because neither can change outcomes or exit codes and nothing in the runner
    may read them. *)

val settings : ?overrides:parsed -> parsed -> (settings, error) result
(** [settings ~overrides cli] is {!resolve}, {!coverage_mode} and
    {!output_level} in one call — what a driver needs from one invocation, with
    one error to render instead of three. Resolution runs in that order, so a
    malformed environment layer is reported as {!resolve} reports it (the caller
    prints that error and exits [2]), and the fresh root seed {!resolve} may
    draw is drawn exactly once.

    [overrides] is the programmatic layer {!resolve} and {!output_level} take
    (defaults to {!empty}); the coverage mode has no programmatic layer and
    comes from [cli] and [WINDTRAP_COVERAGE] alone.

    Effects: the union of the three — reads the environment, and draws a fresh
    root seed when no layer provides one. *)

(** {1:help Help} *)

val usage : prog:string -> string
(** [usage ~prog] is the one-line usage summary
    (["usage: <prog> [OPTIONS] [PATTERN]"]); [prog] is shortened to its
    basename. Callers print it with {!error_message} before exiting [2]. *)

val help : prog:string -> string
(** [help ~prog] is the full help page: usage, the flag table with one line per
    flag, and the environment-variable inventory (flag mirrors and the env-only
    variables). Generated from the same table that drives {!parse}. *)
