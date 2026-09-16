(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Command-line and environment resolution into the run configuration.

    One declarative table drives everything here: every knob is one row, a flag
    beside its optional [WINDTRAP_*] mirror. {!parse} reads an argument vector
    into a {!type:parsed} record of raw flag values, {!settings} merges parsed
    flags and environment mirrors into one {!Run.config} with the precedence CLI
    > environment > default, and {!help} renders the inventory from the same
    rows. A mirror is applied through its flag's own parser, so a variable
    accepts and refuses exactly what its flag does, with the same [expected]
    wording, naming the variable as the source of a bad value. A plain value is
    one token, trimmed; a repeatable flag's is a comma-separated list; a
    valueless flag's is a boolean ({!Os.bool_of_string}), refused when it spells
    neither; an optional-value flag's reads both ways, a boolean as the bare
    flag or its absence and anything else as the value, trimmed. [-l],
    [--failed], [-x], [-u], [--corrected], [-h] and [-V] have no mirror.

    Nothing here prints or exits: failures are returned as a typed {!type:error}
    for the caller to render with {!error_message} and exit [2], and
    [--help]/[--version] come back as flags on {!type:parsed}. *)

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
          ({!Baseline.Update}); refused under CI by the runner. *)
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
  verbose : bool option;  (** [-v], [--verbose]: one status line per test. *)
  junit : string option;  (** [--junit PATH]: also write JUnit XML to [PATH]. *)
  color : Os.color_mode option;
      (** [--color MODE]: [always], [never], or [auto]. *)
  log_dir : string option;
      (** [-o DIR], [--output DIR]: root directory for capture logs. *)
  mutate : string list option;
      (** [--mutate[=PREFIX,...]]: run the mutation loop over every mutant this
          executable catalogues ([Some []], the bare flag) or only those whose
          recorded source path starts with one of the prefixes. *)
  arm : string option;
      (** [--arm ID]: run once with mutant [ID] armed. The identifier is kept
          unparsed ({!Windtrap_runtime.Mutate.id_of_string} owns its grammar).
      *)
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
          [--corrected], or [--mutate] and [--arm]. *)

val error_message : error -> string
(** [error_message error] is a one-line description of [error] for users, naming
    the offending flag or variable. Not stable for programmatic matching. *)

(** {1:parsing Parsing} *)

val parse : string array -> (parsed, error) result
(** [parse argv] reads the argument vector [argv] ([argv.(0)] is ignored) into a
    {!type:parsed} record, or is [Error error] on the first flag that fails.
    Repeated single-valued flags keep the last occurrence; [--tag] and
    [--exclude-tag] accumulate in order. Long flags also accept [--flag=value];
    a flag whose value is optional takes it only that way. The first bare
    argument becomes {!parsed.filter} (a second one is {!Extra_positional});
    arguments after [--] are all positionals. Parsing stops at [-h]/[--help] and
    [-V]/[--version]. [-u] with [--corrected] is {!Incompatible_flags}. *)

(** {1:resolution Resolution} *)

val settings : parsed -> (Run.config, error) result
(** [settings cli] is the configuration one invocation resolves to: [cli] with
    each field's [WINDTRAP_*] mirror filled into what the command line left
    open, then {!Run.default_config}'s value; [tags] and [exclude_tags] are
    additive across both layers. [mutation] is {!Run.Loop} of {!parsed.mutate}'s
    prefixes or {!Run.Armed} of {!parsed.arm}'s identifier; both, whichever
    layer each arrived by, is [Error (Incompatible_flags _)]. [github] is
    {!Os.in_github_actions}[ ()]; [invocation] is left [`Mirrors]. A mirror
    value the flag would reject is [Error (Invalid_value _)] naming the
    variable; a mirror whose flag the command line decided is not parsed.
    [WINDTRAP_MUTATE] reads as the optional-value rule says: a truthy spelling
    is the bare [--mutate], a falsy one its absence, anything else its prefixes.
    {!parsed.help} and {!parsed.version} are ignored.

    Effects: reads the environment, and draws a root seed ({!Seed.random}) when
    no layer provides one. *)

val color_mode : unit -> (Os.color_mode, error) result
(** [color_mode ()] is [WINDTRAP_COLOR] read through [--color]'s parser, for a
    command with no [--color] flag ([windtrap coverage], [windtrap mutants]):
    {!Os.Auto} when unset, the mode it spells, or [Error (Invalid_value _)]
    naming the variable. Effects: reads the environment. *)

(** {1:help Help} *)

val usage : prog:string -> string
(** [usage ~prog] is the one-line usage summary
    (["usage: <prog> [OPTIONS] [PATTERN]"]); [prog] is shortened to its
    basename. Callers print it with {!error_message} before exiting [2]. *)

val help : prog:string -> string
(** [help ~prog] is the full help page: usage, one line per flag, and the
    variables no flag can spell, generated from the table that drives {!parse}.
    The mirrors are described in one sentence, not one row each. *)

(**/**)

(* The argument grammar, the two table-driven passes and the help heading
   a row renders to, exposed for the grammar's own tests, which pin each
   row kind over a synthetic row so that a flag adopting one adds a row
   and nothing else. Not an interface — every other caller goes through
   [parse], [settings] and [help]. *)

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
