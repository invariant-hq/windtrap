(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Command-line and environment resolution into the run configuration.

    {!parse} reads an argument vector into the {!type-parsed} flags, {!settings}
    resolves them with the environment and the defaults into a
    {!Run.type-config}, and {!val-help} is the help page. {!val-help} also lists
    the settings that only the environment spells, which nothing here reads.

    Nothing here prints or exits. An error is returned as an {!type-error}, and
    [--help] and [--version] come back as fields of {!type-parsed} for the
    caller to act on.

    {b Mirrors.} A mirror is the [WINDTRAP_*] variable of a flag. Under
    [dune runtest] an executable gets no command line, and the mirrors are its
    command line. A mirror is named [WINDTRAP_] then the long flag in capitals,
    with [_] for [-], except that of [--arm], which is [WINDTRAP_MUTATE_ARM].

    A mirror reaches every executable of a project alike, and a suite that
    cannot honour one is not in error. {!settings} therefore keeps the source of
    a value where it changes the outcome of a run.

    A mirror is read by the parser of its flag, so a variable accepts and
    refuses what its flag does, with the same [expected] wording. An error names
    the variable as its source. {!settings} reads the mirrors in one pass, and a
    variable set to the empty string counts as unset.
    - The mirror of a flag that takes a value is one token, trimmed.
    - The mirror of [--tag] or [--exclude-tag] is a comma-separated list, whose
      items are trimmed and whose empty items are dropped.
    - [-f] and [-e] repeat as well, but their mirrors are one token each,
      because a test name may hold a comma.
    - The mirror of a flag that takes no value is a boolean
      ({!Os.bool_of_string}). True gives the flag and false is its absence. Any
      other word is refused.
    - The mirror of a flag whose value is optional reads both ways. A boolean is
      the bare flag or its absence, and any other word is the value, trimmed.

    Seven flags have no mirror, and none must be given one. [-l], [--failed] and
    [-x] serve a loop of runs by hand, which is a command line's, and a listing
    is not a run. [-u] and [--corrected] accept baselines, and a build action
    must never accept one because of a variable in its environment. [-h] and
    [-V] only print.

    @canonical Windtrap.Private.Cli *)

(** {1:parsed Parsed flags} *)

type parsed = {
  config : Run.config;
      (** The configuration that the flags give over {!Run.default_config}: each
          flag sets the field that {!Run.type-config} documents for it, a
          repeatable one appends to it, and [--mutate] and [--arm] set
          [mutation]. Positional arguments are patterns of [filter]. *)
  given : string list;
      (** The long name of each flag given, once, in the order first given; a
          positional pattern counts as [--filter]. A field whose flag is not
          here has {!Run.default_config}'s value. *)
  lines : Lines.t option;
      (** [--lines FILE:A-B,…]: the line set that {!Lines.of_spec} reads. *)
  diff : string option;
      (** [--diff FILE]: the path of a [git diff -U0] output, as typed. It is
          read by {!settings}. *)
  case : string option;
      (** [--case PATH]: a path, read by {!settings}. It has no mirror. *)
  list_only : bool;
      (** [-l], [--list]. {!settings} ignores it, and the caller lists the
          selection (see {!Run.list_selection}). *)
  help : bool;  (** [-h], [--help]. *)
  version : bool;  (** [-V], [--version]. *)
}
(** The type for the flags of one command line, before the environment. A field
    of the configuration holds a flag's value, and [given] says which flags set
    one, so a field is never copied out of a record of its own. The other fields
    hold what no field of the configuration holds. [--arm]'s identifier is not
    parsed here, so a malformed one is no {!type-error}, and
    [Windtrap_runtime.Mutate.id_of_string] reports it. *)

(** {1:errors Errors} *)

(** The type for the errors of {!parse} and {!settings}. *)
type error =
  | Unknown_flag of string
      (** The flag is not in the table. The payload is the argument as typed,
          but for the [=value] of a long flag: [--bogus=1] gives [--bogus], and
          [-f=x] gives [-f=x]. An argument of two bytes or more that starts with
          [-] is read as a flag, so [-1] and a bundled [-xv] are unknown flags.
      *)
  | Missing_value of string
      (** The flag takes a value and the command line ended. A flag takes the
          next argument whatever it looks like. *)
  | Invalid_value of { source : string; value : string; expected : string }
      (** [value] was refused. [source] is the flag as typed, or the variable
          that carried [value], and [expected] describes what is accepted. A
          flag that takes no value and is given one, as in [--verbose=1], is
          refused this way, with [expected = "no argument"]. *)
  | Incompatible_flags of string * string
      (** Both flags were given, and they contradict each other. The payload is
          [("-u", "--corrected")] or [("--mutate", "--arm")], whatever spelling
          or mirror carried them. *)
  | Requires of string * string
      (** The command line gave the first flag, which scopes the second, and no
          layer gave the second: [("--lines", "--mutate")] or
          [("--diff", "--mutate")]. *)

val error_message : error -> string
(** [error_message error] is one sentence on [error] for a user, which names the
    flag or the variable concerned. It is not stable enough for a program to
    match.

    An unknown long flag ends with the nearest long flag when one is near, as in
    [; did you mean '--junit'?], and a short flag gets no suggestion. *)

(** {1:parsing Parsing} *)

val parse : string array -> (parsed, error) result
(** [parse argv] is the flags of [argv], or the first error from the left.
    [argv.(0)] is not read, and an empty [argv] gives no flag. It reads no
    mirror and never raises. Its configuration is {!Run.default_config}'s before
    the flags, so every call draws a seed and reads the default log directory.
    - A repeated flag keeps its last value, and [-f], [-e], [--tag] and
      [--exclude-tag] accumulate.
    - A long flag also takes its value as [--flag=value]. A short flag does not,
      and [-f=x] and [-fx] are unknown flags.
    - A flag whose value is optional takes it only as [--flag=value]. Bare, it
      never takes the next argument.
    - A bare argument is added to [filter], as [-f] adds one. Every argument
      after [--] is positional, which is how a pattern that starts with [-] is
      given.
    - Parsing stops at [--help] and at [--version], so the flags after them are
      not checked. An error before them still wins.
    - [-u] with [--corrected] is {!Incompatible_flags}. It is checked after the
      scan, so it also wins over a [--help] that follows the two. *)

(** {1:resolution Resolution} *)

val settings : parsed -> (Run.config, error) result
(** [settings cli] is the configuration that one invocation resolves to. Each
    field is [cli]'s, else its mirror's, else that of {!Run.default_config}: the
    mirrors set [cli.config] through their flags' parsers.
    - [tags] and [exclude_tags] add up, the command line's first and then the
      mirror's. [filter] and [exclude] are the command line's patterns when it
      gives one, and else the mirror's.
    - A mirror whose flag the command line gave is not read, so a valid
      [--timeout] hides a malformed [WINDTRAP_TIMEOUT]. Any other mirror that
      its flag would refuse is [Error (Invalid_value _)] naming the variable,
      and is never ignored. The mirrors are read in the order of {!val-help},
      and the first error ends the resolution.
    - [mutation] is {!Run.Loop} of [--mutate], {!Run.Armed} of [--arm], or
      {!Run.No_mutation}. The lines of the loop's scope are the union of [lines]
      and of what {!Lines.of_diff} reads in the file of [diff], whose relative
      path is made absolute as [-o]'s is. A file that cannot be read is
      [Error (Invalid_value _)], and [lines] or [diff] from the command line
      without [mutate] is [Error (Requires _)]; from their mirrors alone they
      are ignored. Both at once is [Error (Incompatible_flags _)], whichever
      layer gave each, and it is checked after every mirror. A prefix that
      spells a boolean cannot go through the variable.
    - A relative path of [--junit], [--json] or [-o] is made absolute, against
      the working directory when the command line gives it and against
      {!Os.project_root} when its mirror does. It is kept as given when that
      directory cannot be read.
    - [case_file] is the bytes of the file of [case], read whole through
      {!Choice.of_bytes}, its path relative to the working directory. A file
      that cannot be read is [Error (Invalid_value _)], and a replay token from
      either layer beside it is [Error (Incompatible_flags _)].
    - [seed] is the layers', and else {!mutation_seed} under [--mutate] and
      [--arm], so that a mutation run judges every mutant on the same cases, run
      after run of one binary.
    - [github] is {!Os.in_github_actions}[ ()] and [invocation] is [`Mirrors].
      [list_only], [help] and [version] are ignored.

    It never raises. It reads the environment and the working directory. The
    seed that {!parse} drew is the result's when no layer gives one and no
    mutation is asked for. *)

val mutation_seed : Seed.seed
(** [mutation_seed] is the root seed of a mutation run that no layer gives one,
    [s1:6d7574616e747321]. *)

val color_mode : unit -> (Os.color_mode, error) result
(** [color_mode ()] is the colour mode that [WINDTRAP_COLOR] gives, read by the
    parser of [--color], for a command whose one runner flag is [--color]
    ({!parse_color}). It is [Ok Os.Auto] when the variable is unset or empty,
    and [Error (Invalid_value _)] naming the variable for a word that the flag
    would refuse. *)

val parse_color : string -> (Os.color_mode, error) result
(** [parse_color value] is [value] read as the value of [--color] on the command
    line, and [Error (Invalid_value _)] naming [--color] for a word that the
    flag refuses. *)

(** {1:help Help} *)

val usage : prog:string -> string
(** [usage ~prog] is [usage: <prog> [OPTIONS] [PATTERN...]], with the basename
    of [prog]. *)

val help : prog:string -> string
(** [help ~prog] is the help page. It gives each flag with its spellings, its
    mirror and its description, in the order of the table, then the settings
    that only a variable spells. It ends with a newline.

    Every line fits 80 columns when the basename of [prog] leaves the first two
    within them. *)
