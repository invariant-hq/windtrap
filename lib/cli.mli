(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Command-line and environment resolution into the run configuration.

    One declarative flag table drives everything here: {!parse} reads an
    argument vector into a {!type:parsed} record of raw flag values, {!resolve}
    merges programmatic overrides, parsed flags, and the [WINDTRAP_*]
    environment mirrors into a {!Run.config} with the precedence
    {e programmatic > CLI > env > default} (under [dune runtest] the environment
    mirrors {e are} the CLI), and {!help} renders the flag and variable
    inventory.

    Nothing in this module prints or exits: parse and resolution failures are
    returned as a typed {!type:error} — the caller renders {!error_message} and
    exits [2] — and [--help]/[--version] come back as flags on {!type:parsed}
    for the caller to act on. The flag inventory is v1's minus the cut
    [--format] axis — terminal verbosity is one three-level axis ([-q] ⊂ default
    ⊂ [-v], {!output_level}), not a format — plus [--quiet], [--verbose],
    [--prune] and [--shard]. *)

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
    seed leaves the variable unread. [WINDTRAP_SHARD] and the numeric mirrors
    [WINDTRAP_TIMEOUT], [WINDTRAP_PROP_COUNT], and [WINDTRAP_SLOW_THRESHOLD] are
    treated the same way: a token in the winning layer that does not parse is an
    error naming the variable, never silently ignored — a misread shard would
    silently rerun the whole suite in every bucket, and a misread count or limit
    would silently run with the default. The winning [timeout], [prop_count],
    and [bail] must be positive ([timeout] finite as well), the winning
    [slow_threshold] must be finite and non-negative, and the winning [shard]
    must satisfy [1 <= K <= N]: {!parse} already rejects such values on the
    command line, and a violation arriving through an environment mirror or a
    programmatic override is [Error (Invalid_value _)] naming that source —
    never a config that detonates mid-run. {!parsed.help} and {!parsed.version}
    are ignored — acting on them is the caller's job. *)

val coverage_mode :
  parsed -> ([ `Summary | `Report | `Full | `Off ], error) result
(** [coverage_mode cli] is the coverage rendering mode: [cli]'s
    {!parsed.coverage} when present, else the [WINDTRAP_COVERAGE] environment
    mirror, else [`Summary]. Resolved apart from {!resolve} because it is a
    rendering decision, not run configuration — {!Run.config} carries no
    coverage field (Law 12), and enabling any mode never changes outcomes or
    exit codes (Law 13). The caller applies it: [`Summary] renders the one-line
    percentage when the run was instrumented, [`Report] and [`Full] add the
    per-file detail, [`Off] renders nothing.

    Effects: reads the environment when {!parsed.coverage} is [None].
    [Error (Invalid_value _)] with source [WINDTRAP_COVERAGE] when the winning
    environment value is not one of [summary], [report], [full], [off]. *)

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
    unparseable boolean values count as unset. *)

(** {1:help Help} *)

val usage : prog:string -> string
(** [usage ~prog] is the one-line usage summary
    (["usage: <prog> [OPTIONS] [PATTERN]"]); [prog] is shortened to its
    basename. Callers print it with {!error_message} before exiting [2]. *)

val help : prog:string -> string
(** [help ~prog] is the full help page: usage, the flag table with one line per
    flag, and the environment-variable inventory (flag mirrors and the env-only
    variables). Generated from the same table that drives {!parse}. *)
