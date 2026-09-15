(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Command-line and environment resolution into the run configuration.

    One declarative table drives everything here — every knob is one row, a flag
    beside its optional [WINDTRAP_*] mirror or a setting only the environment
    can spell: {!parse} reads an argument vector into a {!type:parsed} record of
    raw flag values, {!settings} merges parsed flags and environment mirrors
    into one {!Run.config} — with the precedence {e CLI > env > default} (under
    [dune runtest] the environment mirrors {e are} the CLI) — and {!help}
    renders the flag and variable inventory from the same rows. {!settings} is
    the one call the facade makes, one pass over one environment layer.

    A flag's mirror is declared in that table beside the flag, and its value is
    applied through the flag's own parser, so the two cannot drift: a variable
    accepts exactly what its flag accepts, refuses exactly what its flag
    refuses, with the same [expected] wording, and differs only in naming the
    variable rather than the flag as the source of a bad value. Two reading
    rules turn a variable into what the parser takes: a plain value is one
    token, trimmed; a repeatable flag's is a comma-separated list, one token per
    item. A valueless flag's variable is a boolean ({!Env.bool_of_string}) that
    applies the flag when true and is refused when it spells neither, and an
    optional-value flag's variable reads both ways — a boolean is the bare flag
    or its absence, anything else is the value, trimmed. {!Env} is consulted for
    the reading, not for the inventory: what it still owns outright are the
    variables read below this layer ([WINDTRAP_PROJECT_ROOT] and the coverage
    and mutation scopes).

    Seven flags have no mirror. [-l], [--failed] and [-x] want a command line: a
    variable cannot help a cached action, and a listing is not a test run. [-u]
    and [--corrected] are acceptance, which under dune is [--corrected] in the
    stanza's own action and [dune promote], never a variable in a build action's
    environment. [-h] and [-V] are the two informational exits.

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
  bail : bool option;
      (** [-x], [--fail-fast]: stop after the first counted failure. *)
  stream : bool option;
      (** [-s], [--stream]: run against the real descriptors instead of
          capturing. *)
  update : bool option;
      (** [-u], [--update]: accept baseline changes in place
          ({!Baseline.Update}); refused under CI by the runner, with no
          override. *)
  corrected : bool option;
      (** [--corrected]: write every correction as [<file>.corrected]
          ({!Baseline.Corrected}), for a [diff?] action and [dune promote]. *)
  seed : Seed.seed option;
      (** [--seed TOKEN]: the root seed, an [s1:] token parsed by
          {!Seed.of_string}. *)
  timeout : float option;
      (** [--timeout SECONDS]: default per-test limit; must be positive. *)
  slow_threshold : float option;
      (** [--slow-threshold SECONDS]: seconds an untagged test may take before
          the report warns; must be non-negative, [0] disables. *)
  prop_count : int option;
      (** [--prop-count N]: generated cases per property; must be positive. *)
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
  | Incompatible_flags of string * string
      (** Both flags were given and they contradict each other: [-u] and
          [--corrected]. *)

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
    [--flag=value] spelling; a flag whose value is optional takes it {e only}
    that way — bare, it never consumes the next argument. The first bare
    argument becomes {!parsed.filter} (a second one is {!Extra_positional});
    arguments after a [--] separator are all treated as positionals. Parsing
    stops at [-h]/[--help] and [-V]/[--version]: flags after them are not
    validated. [-u] together with [--corrected] is {!Incompatible_flags}. *)

(** {1:resolution Resolution} *)

type mutation = {
  mode : [ `Unset | `Loop ];
      (** [WINDTRAP_MUTATE]: [`Loop] for a mutation run ([1] and the other
          truthy spellings), [`Unset] for an unset, empty or falsy variable —
          the boolean vocabulary every other switch accepts
          ({!Env.bool_of_string}). Any other value is an error. *)
  arm : string option;
      (** [WINDTRAP_MUTATE_ARM]: the mutant identifier to arm, unparsed —
          {!Windtrap_runtime.Mutate.id_of_string} owns that grammar and reports
          its own errors. [None] when the variable is unset or empty. *)
}
(** The type for the mutation switches. Environment variables with no flag yet:
    [--mutate[=PREFIX,...]] and [--arm ID] are the flags they become, with these
    variables as their mirrors, so the inline runner keeps reaching them. *)

val mutation : unit -> (mutation, error) result
(** [mutation ()] reads the two mutation variables. Resolved apart from
    {!settings} because they are the mutation loop's, not the run's, but with
    the same loudness: [Error (Invalid_value _)] naming [WINDTRAP_MUTATE] when
    its value is not one the variable accepts, never a silently defaulted mode.

    Effects: reads the environment. *)

val settings : parsed -> (Run.config, error) result
(** [settings cli] is the configuration one invocation resolves to: [cli] with
    each field's [WINDTRAP_*] mirror filled into what the command line left
    open, then every field of {!Run.config} — [tags] and [exclude_tags] additive
    across both layers, every other field the first layer that decided it, else
    {!Run.default_config}'s. [coverage] is [WINDTRAP_COVERAGE] (on unless the
    variable says otherwise), [github] is {!Env.in_github_actions}[ ()], and
    [invocation] is left [`Mirrors] for the facade to compute from [argv].

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

val color_mode : unit -> (Env.color_mode, error) result
(** [color_mode ()] is [WINDTRAP_COLOR] read through [--color]'s parser, for a
    command with no [--color] flag of its own ([windtrap coverage],
    [windtrap mutants]): {!Env.Auto} when the variable is unset, the mode it
    spells, or [Error (Invalid_value _)] naming the variable for a value the
    flag would refuse. The runner reads the variable as every other mirror, in
    {!settings}.

    Effects: reads the environment. *)

(** {1:help Help} *)

val usage : prog:string -> string
(** [usage ~prog] is the one-line usage summary
    (["usage: <prog> [OPTIONS] [PATTERN]"]); [prog] is shortened to its
    basename. Callers print it with {!error_message} before exiting [2]. *)

val help : prog:string -> string
(** [help ~prog] is the full help page: usage, the flag table with one line per
    flag, and the variables no flag can spell. Generated from the same table
    that drives {!parse}. The mirrors get one sentence rather than a row each:
    the rule is mechanical, and twenty-odd lines reading [Mirror of --x] said
    nothing the sentence does not. *)

(**/**)

(* The argument grammar, the two table-driven passes and the help heading
   a row renders to, exposed for the grammar's own tests: the row kind no
   flag uses yet ([Optional_value]) is pinned over a synthetic row, so the
   flag that adopts it adds a row and nothing else. Not an interface —
   every other caller goes through [parse], [settings] and [help]. *)

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

type layering = Single of (parsed -> bool) | Repeatable
type mirror = { var : string; layering : layering }

type entry = {
  short : string option;
  long : string;
  arg : arg;
  doc : string;
  mirror : mirror option;
}

val parse_entries : entry list -> string array -> (parsed, error) result
val layer_entries : entry list -> parsed -> (parsed, error) result
val flag_heading : entry -> string

(**/**)
