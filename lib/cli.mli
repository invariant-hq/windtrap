(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Command-line and environment resolution into the run configuration.

    One declarative table drives everything here — every knob is one row, a flag
    beside its optional [WINDTRAP_*] mirror or a setting only the environment
    can spell: {!parse} reads an argument vector into a {!type:parsed} record of
    raw flag values, {!settings} merges parsed flags and environment mirrors
    into a {!Run.config} and the three rendering decisions kept out of it — with
    the precedence {e CLI > env > default} (under [dune runtest] the environment
    mirrors {e are} the CLI) — and {!help} renders the flag and variable
    inventory from the same rows. {!settings} is the one call a driver makes,
    one pass over one environment layer.

    A flag's mirror is declared in that table beside the flag, and its value is
    applied through the flag's own parser, so the two cannot drift: a variable
    accepts exactly what its flag accepts, refuses exactly what its flag
    refuses, with the same [expected] wording, and differs only in naming the
    variable rather than the flag as the source of a bad value. The two
    variables whose vocabulary is wider than their flag's — [WINDTRAP_UPDATE]'s
    [force] and [WINDTRAP_COLOR]'s lenient fall back to {!Env.Auto} — are parsed
    beside their own rows all the same. {!Env} is consulted for the reading, not
    for the inventory: what it still owns outright are the variables read below
    this layer ([WINDTRAP_PROJECT_ROOT] and the coverage/mutation scopes).

    Nothing in this module prints or exits: parse and resolution failures are
    returned as a typed {!type:error} — the caller renders {!error_message} and
    exits [2] — and [--help]/[--version] come back as flags on {!type:parsed}
    for the caller to act on. *)

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
      (** [--max-shrink N]: accepted shrink steps per failing property; must be
          positive. *)
  verbose : bool option;
      (** [-v], [--verbose]: one status line per test. [None] is the compact
          default. *)
  junit : string option;  (** [--junit PATH]: also write JUnit XML to [PATH]. *)
  color : Env.color_mode option;
      (** [--color MODE]: [always], [never], or [auto]. *)
  log_dir : string option;
      (** [-o DIR], [--output DIR]: root directory for capture logs. *)
  help : bool;  (** [-h], [--help]: the caller prints {!help} and exits [0]. *)
  version : bool;
      (** [-V], [--version]: the caller prints its version and exits [0]. *)
}
(** The type for raw parse results: one field per flag, [None] (or [[]], or
    [false] for {!parsed.help} and {!parsed.version}) when the flag was absent.
*)

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

type mutation = {
  mode : [ `Unset | `Off | `Loop | `Admit ];
      (** [WINDTRAP_MUTATE]: [`Loop] for a mutation run ([1] and the other
          truthy spellings), [`Admit] for [admit] — the per-test admission run
          over the selection, or over every test the run executes when it makes
          none — [`Off] for a falsy spelling, and [`Unset] for an unset or
          empty variable. The last two differ: an instrumented build says what
          it could do unless it was told not to. *)
  arm : string option;
      (** [WINDTRAP_MUTATE_ARM]: the mutant identifier to arm, unparsed —
          {!Windtrap_mutate.selector_of_string} owns that grammar and reports
          its own errors. [None] when the variable is unset or empty. *)
  tries : int;
      (** [WINDTRAP_MUTATE_TRY]: faults an [`Admit] run tries per selected test
          before ruling it unjustified, [0] for all it reaches. Defaults to
          [25]. Read for every mode: a value the user set and misspelled must
          be loud in every build. *)
}
(** The type for the mutation knobs, which are environment variables only: the
    inline runner's argument parser accepts dune's inline-test protocol and
    nothing else, so a flag would exist for half the users. *)

val mutation : unit -> (mutation, error) result
(** [mutation ()] reads the three mutation variables. Resolved apart from
    {!settings} because none of them is run configuration and nothing in the
    runner may read them, but with the same loudness:
    [Error (Invalid_value _)] naming [WINDTRAP_MUTATE] or [WINDTRAP_MUTATE_TRY]
    when its value is not one the variable accepts, never a silently defaulted
    mode.

    Effects: reads the environment. *)

type settings = {
  config : Run.config;  (** The run configuration. *)
  render : Render.settings;
      (** The renderer settings: the presentation knobs — [--color], the
          [WINDTRAP_TAIL_ERRORS] override, [--slow-threshold] — resolved with
          the same precedence as [config] and handed to the driver's renderer
          construction. *)
  coverage : bool;
      (** Whether the inline coverage line prints ([WINDTRAP_COVERAGE], on
          unless the variable says otherwise). *)
  output_level : [ `Compact | `Verbose ];
      (** The terminal verbosity level: the compact transcript, or one status
          line per test. *)
  junit : string option;
      (** [--junit PATH]: also write a JUnit report there ({!Driver.t}). *)
}
(** The type for everything one invocation resolves to. Not one configuration:
    only [config] is what the runner reads — the rendering decisions and the
    JUnit sink stay {e out} of it, because none of them can change outcomes or
    exit codes and nothing in the runner may read them. *)

val settings : parsed -> (settings, error) result
(** [settings cli] is everything one invocation resolves to: [cli] with each
    field's [WINDTRAP_*] mirror filled into what the command line left open,
    then split into the run configuration and the three rendering decisions.
    [tags] and [exclude_tags] are additive across both layers; every other field
    is the first layer that decided it, else the default.

    A mirror is read through its own flag's parser, so a value the flag would
    reject is [Error (Invalid_value _)] naming the variable, never silently
    ignored — a misread [WINDTRAP_SHARD] would rerun the whole suite in every
    bucket, and a misread count or limit would run with the default. A mirror
    whose flag the command line already decided is not even parsed, so a valid
    [--timeout] shadows a malformed [WINDTRAP_TIMEOUT]. [WINDTRAP_COVERAGE] is
    read the same way, and errors the same way; its message names
    [windtrap coverage], where the retired [report] and [full] modes went.
    {!parsed.help} and {!parsed.version} are ignored — acting on them is the
    caller's job.

    Effects: reads the environment, and draws a fresh root seed ({!Seed.random})
    when no layer provides one. *)

(** {1:help Help} *)

val usage : prog:string -> string
(** [usage ~prog] is the one-line usage summary
    (["usage: <prog> [OPTIONS] [PATTERN]"]); [prog] is shortened to its
    basename. Callers print it with {!error_message} before exiting [2]. *)

val help : prog:string -> string
(** [help ~prog] is the full help page: usage, the flag table with one line per
    flag, and the variables no flag can spell. Generated from the same table
    that drives {!parse}. The mirrors get one sentence rather than a row each:
    the rule is mechanical, and twenty-four lines reading [Mirror of --x] said
    nothing the sentence does not. *)
