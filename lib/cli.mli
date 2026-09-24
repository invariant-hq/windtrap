(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Command-line and environment resolution into the run configuration.

    {!parse} reads an argument vector into the {!type-parsed} flags, {!settings}
    resolves them with the environment and the defaults into a
    {!Run.type-config}, and {!val-help} is the help page. One table drives the
    three. A row is a flag beside its optional mirror, or a setting that only
    the environment spells, which {!val-help} lists and nothing here reads.

    Nothing here prints or exits, and where a text shows is its caller's
    contract. An error is returned as an {!type-error}, and [--help] and
    [--version] come back as fields of {!type-parsed} for the caller to act on.

    {b Mirrors.} A mirror is the [WINDTRAP_*] variable of a flag. Under
    [dune runtest] an executable gets no command line, and the mirrors are its
    command line. A mirror is named [WINDTRAP_] then the long flag in capitals,
    with [_] for [-], except that of [--arm], which is [WINDTRAP_MUTATE_ARM].

    A mirror is read by the parser of its flag, so a variable accepts and
    refuses what its flag does, with the same [expected] wording. An error names
    the variable as its source. {!settings} reads the mirrors in one pass, and a
    variable set to the empty string counts as unset.
    - The mirror of a flag that takes a value is one token, trimmed.
    - The mirror of a repeatable flag is a comma-separated list, whose items are
      trimmed and whose empty items are dropped.
    - The mirror of a flag that takes no value is a boolean
      ({!Os.bool_of_string}). True gives the flag and false is its absence. Any
      other word is refused.
    - The mirror of a flag whose value is optional reads both ways. A boolean is
      the bare flag or its absence, and any other word is the value, trimmed.

    Seven flags have no mirror, and none must be given one. [-l], [--failed] and
    [-x] serve a loop of runs by hand, which is a command line's, and a listing
    is not a run. [-u] and [--corrected] accept baselines, and a build action
    must never accept one because of a variable in its environment. [-h] and
    [-V] only print. *)

(** {1:parsed Parsed flags} *)

type parsed = {
  filter : string option;
      (** [-f PATTERN], [--filter PATTERN], or the positional argument. *)
  exclude : string option;  (** [-e PATTERN], [--exclude PATTERN]. *)
  tags : string list;
      (** [--tag LABEL], repeatable: the labels in the order given. *)
  exclude_tags : string list;
      (** [--exclude-tag LABEL], repeatable: the labels in the order given. *)
  shard : (int * int) option;
      (** [--shard K/N], with [1 <= K <= N]. [K] and [N] are plain decimal
          numerals, so a sign, [0x] and [_] are refused. *)
  failed_only : bool option;  (** [--failed]. *)
  list_only : bool option;
      (** [-l], [--list]. {!settings} ignores it, and the caller lists the
          selection (see {!Run.list_selection}). *)
  bail : bool option;  (** [-x], [--fail-fast]. *)
  stream : bool option;  (** [-s], [--stream]. *)
  update : bool option;  (** [-u], [--update], for {!Baseline.Update}. *)
  corrected : bool option;  (** [--corrected], for {!Baseline.Corrected}. *)
  seed : Seed.seed option;
      (** [--seed TOKEN]: an [s1:] token, read by {!Seed.of_string}. *)
  timeout : float option;
      (** [--timeout SECONDS]: a finite and positive number. *)
  slow_threshold : float option;
      (** [--slow-threshold SECONDS]: a finite and non-negative number. *)
  prop_count : int option;  (** [--prop-count N]: a positive integer. *)
  verbose : bool option;  (** [-v], [--verbose]. *)
  junit : string option;  (** [--junit PATH]. *)
  color : Os.color_mode option;
      (** [--color MODE]: [always], [never] or [auto], in any case. *)
  log_dir : string option;  (** [-o DIR], [--output DIR]. *)
  mutate : string list option;
      (** [--mutate[=PREFIX,…]]: [Some []] for the bare flag, and else the
          comma-separated prefixes. *)
  arm : string option;
      (** [--arm ID]: the identifier as typed. It is not parsed here, so a
          malformed one is no {!type-error}, and
          [Windtrap_runtime.Mutate.id_of_string] reports it. *)
  help : bool;  (** [-h], [--help]. *)
  version : bool;  (** [-V], [--version]. *)
}
(** The type for the flags of one command line, one field per flag, before the
    environment and the defaults. An absent flag is [None], [[]] for a
    repeatable one, and [false] for [help] and [version]. A [bool option] field
    is [None] or [Some true] and never [Some false]. {!Run.type-config} says
    what a run does with each. *)

val empty : parsed
(** [empty] is the record with every flag absent. *)

(** {1:errors Errors} *)

(** The type for the errors of {!parse} and {!settings}. *)
type error =
  | Unknown_flag of string
      (** The flag is not in the table. The payload is the flag as typed,
          without any [=value]. An argument of two bytes or more that starts
          with [-] is read as a flag, so [-1] and a bundled [-xv] are unknown
          flags. *)
  | Missing_value of string
      (** The flag takes a value and the command line ended. A flag takes the
          next argument whatever it looks like. *)
  | Invalid_value of { source : string; value : string; expected : string }
      (** [value] was refused. [source] is the flag as typed, or the variable
          that carried [value], and [expected] describes what is accepted. A
          flag that takes no value and is given one, as in [--verbose=1], is
          refused this way, with [expected = "no argument"]. *)
  | Extra_positional of { filter : string; extra : string }
      (** A second positional argument [extra] came when the filter was already
          [filter], from a positional argument or from [-f]. *)
  | Incompatible_flags of string * string
      (** Both flags were given, and they contradict each other. The payload is
          [("-u", "--corrected")] or [("--mutate", "--arm")], whatever spelling
          or mirror carried them. *)

val error_message : error -> string
(** [error_message error] is one sentence on [error] for a user, which names the
    flag or the variable concerned. It is not stable enough for a program to
    match.

    An unknown long flag ends with the nearest long flag when one is near, as in
    [; did you mean '--junit'?], and a short flag gets no suggestion. *)

(** {1:parsing Parsing} *)

val parse : string array -> (parsed, error) result
(** [parse argv] is the flags of [argv], or the first error from the left.
    [argv.(0)] is not read, and an empty [argv] is [Ok empty]. It reads no
    environment and never raises.
    - A repeated flag keeps its last value, and [--tag] and [--exclude-tag]
      accumulate.
    - A long flag also takes its value as [--flag=value]. A short flag does not,
      and [-f=x] and [-fx] are unknown flags.
    - A flag whose value is optional takes it only as [--flag=value]. Bare, it
      never takes the next argument.
    - The first bare argument is [filter], and a second is {!Extra_positional}.
      Every argument after [--] is positional, which is how a pattern that
      starts with [-] is given.
    - Parsing stops at [--help] and at [--version], so the flags after them are
      not checked. An error before them still wins.
    - [-u] with [--corrected] is {!Incompatible_flags}. It is checked after the
      scan, so it also wins over a [--help] that follows the two. *)

(** {1:resolution Resolution} *)

val settings : parsed -> (Run.config, error) result
(** [settings cli] is the configuration that one invocation resolves to. Each
    field is [cli]'s, else its mirror's, else that of {!Run.default_config}.
    - [tags] and [exclude_tags] add up, the command line's first and then the
      mirror's.
    - A mirror whose flag the command line gave is not read, so a valid
      [--timeout] hides a malformed [WINDTRAP_TIMEOUT]. Any other mirror that
      its flag would refuse is [Error (Invalid_value _)] naming the variable,
      and is never ignored. The mirrors are read in the order of {!val-help},
      and the first error ends the resolution.
    - [baseline] is {!Baseline.Update} under [update], else
      {!Baseline.Corrected} under [corrected], else {!Baseline.Check}.
    - [mutation] is {!Run.Loop} of [mutate], {!Run.Armed} of [arm], or
      {!Run.No_mutation}. Both at once is [Error (Incompatible_flags _)],
      whichever layer gave each, and it is checked after every mirror.
      [WINDTRAP_MUTATE=1] is the bare flag, [WINDTRAP_MUTATE=0] its absence, and
      [WINDTRAP_MUTATE=lib/calc.ml] a prefix. A prefix that spells a boolean
      cannot go through the variable.
    - [log_dir]: a relative [-o DIR] is made absolute against the working
      directory, and is kept as given when that directory cannot be read.
    - [github] is {!Os.in_github_actions}[ ()], [allow_focus] is [false] and
      [invocation] is [`Mirrors]. [list_only], [help] and [version] are ignored.

    It never raises. It reads the environment and the working directory, and on
    every call it draws a seed from {!Seed.random}, which the result carries
    when no layer gives one. *)

val color_mode : unit -> (Os.color_mode, error) result
(** [color_mode ()] is [WINDTRAP_COLOR] read by the parser of [--color], for a
    command that has no such flag. It is [Ok Os.Auto] when the variable is unset
    or empty, and [Error (Invalid_value _)] naming the variable for a word that
    the flag would refuse. The runner needs no such call, because {!settings}
    reads the variable as it reads every mirror. *)

(** {1:help Help} *)

val usage : prog:string -> string
(** [usage ~prog] is [usage: <prog> [OPTIONS] [PATTERN]], with the basename of
    [prog]. *)

val help : prog:string -> string
(** [help ~prog] is the help page. It gives each flag with its spellings, its
    mirror and its description, in the order of the table, then the settings
    that only a variable spells. It ends with a newline.

    Every line fits 80 columns when the basename of [prog] leaves the first two
    within them. *)

(**/**)

(* The argument grammar and the two passes over a table of rows, exported for
   the unit suite. Every other caller goes through [parse], [settings] and
   [help]. [lib/cli.ml] describes [arg] and [layering] at their definitions.
   [parse_entries entries] is [parse] over [entries]. [layer_entries entries
   cli] is [cli] with each mirror of [entries] filled into the fields that the
   command line left open, or the first error, and it reads the environment.
   [flag_heading entry] is the heading of [entry] in [help], without its
   indentation. *)

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
