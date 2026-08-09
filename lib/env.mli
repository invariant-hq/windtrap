(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Environment variable reading and platform detection.

    This module owns {e how} the environment is read: the generic typed readers
    below, the value vocabularies they share (booleans, comma-separated lists,
    colour modes, snapshot update modes), platform and CI detection, and the few
    settings that have no command-line flag. It is not the inventory of
    variables: every [WINDTRAP_*] mirror of a runner flag is declared beside
    that flag in {!Cli}'s table and read through {!get_string}, {!get_bool} and
    {!split_comma} from there, which is what stops a mirror from parsing or
    validating differently from the flag it mirrors. Two further lookups live
    elsewhere by design: the coverage runtime reads its own
    [WINDTRAP_COVERAGE_FILE] (windtrap links the coverage library, not the
    reverse, so it cannot depend on this module), and {!Path_ops} consults
    [HOME] as a platform fallback when resolving the home directory.

    Readers are plain functions that re-read the environment on every call;
    nothing is cached. A variable set to the empty string counts as unset.

    Boolean variables accept [1], [true], [yes], [y], [on] and their negations,
    case-insensitively; unparseable values count as unset. The presence-style
    variables [CI], [GITHUB_ACTIONS] and [INSIDE_DUNE] are looser: they are
    conventionally set to arbitrary values by other tools, so any value other
    than an explicit falsy spelling counts as set.

    Precedence (programmatic > CLI > env > default) is resolved by the CLI
    layer, which is why most readers return an [option] rather than a default.
*)

(** {1:readers Readers}

    The typed lookups every variable goes through, named rather than mirrored: a
    caller passes the variable's name, so one reader serves any number of
    variables. *)

val get_string : string -> string option
(** [get_string var] is the value of [var], or [None] when it is unset or empty.
    Unparsed: a caller that owns a format ([WINDTRAP_SEED]'s token,
    [WINDTRAP_SHARD]'s [k/n]) validates it and reports failure naming [var],
    rather than reading a silent default out of a typo. *)

val get_bool : string -> bool option
(** [get_bool var] is [var] read as a boolean, [None] when it is unset, empty,
    or spelled in no accepted way. The value is trimmed before parsing. *)

val get_int : string -> int option
(** [get_int var] is [var] read as a decimal integer, [None] when it is unset or
    does not parse. The value is trimmed before parsing. *)

val split_comma : string -> string list
(** [split_comma value] splits [value] on commas, trims each item and drops the
    empty ones — the spelling the repeatable flags take in one variable, e.g.
    [WINDTRAP_TAG="a, b ,,c "] is [["a"; "b"; "c"]]. *)

(** {1:platform Platform detection} *)

val inside_dune : unit -> bool
(** [inside_dune ()] is [true] iff the [INSIDE_DUNE] variable is set to anything
    but a falsy spelling, i.e. the process was started by dune (e.g.
    [dune runtest]). *)

val is_tty_stdout : unit -> bool
(** [is_tty_stdout ()] is [true] iff standard output is a terminal. *)

val is_tty_stderr : unit -> bool
(** [is_tty_stderr ()] is [true] iff standard error is a terminal. *)

val term_dumb : unit -> bool
(** [term_dumb ()] is [true] iff [TERM] is set to exactly [dumb] — the
    conventional "no escape sequences" terminal. Disables ANSI styling in
    {!Auto} mode and gates the renderer's live tail off. *)

(** {1:ci CI detection} *)

(** The type for detected CI environments. *)
type ci =
  | Not_ci  (** [CI] is unset or falsy. *)
  | Github_actions  (** [CI] and [GITHUB_ACTIONS] are both set. *)
  | Other_ci  (** [CI] is set but [GITHUB_ACTIONS] is not. *)

val ci : unit -> ci
(** [ci ()] is the detected CI environment. [CI] set to a falsy value ([0],
    [false], ...) counts as {!Not_ci}. *)

val in_ci : unit -> bool
(** [in_ci ()] is [true] iff {!ci} is not {!Not_ci}. Gates focused-test commits,
    snapshot update refusal, and GitHub annotations. *)

val in_github_actions : unit -> bool
(** [in_github_actions ()] is [true] iff {!ci} is {!Github_actions}. *)

(** {1:color Color} *)

(** The type for color preferences, from [WINDTRAP_COLOR] or [--color]. *)
type color_mode =
  | Always  (** Emit ANSI styling unconditionally. *)
  | Never  (** Never emit ANSI styling. *)
  | Auto  (** Style when on a terminal or under dune, unless [TERM] is dumb. *)

val color_mode : unit -> color_mode
(** [color_mode ()] parses [WINDTRAP_COLOR] ([always], [never], [auto],
    case-insensitively). Unset or unrecognized values are {!Auto}. *)

val resolve_color :
  color_mode -> tty:bool -> inside_dune:bool -> term_dumb:bool -> bool
(** [resolve_color mode ~tty ~inside_dune ~term_dumb] is the ANSI decision for
    [mode] on a sink whose terminal status is [tty]: [Always] is [true], [Never]
    is [false], and [Auto] is [(tty || inside_dune) && not term_dumb] (dune
    captures output but renders escape codes back to the user; a dumb terminal —
    {!term_dumb} — renders none, so [Auto] never styles it, while an explicit
    [Always] still wins). Pure; shared with the [--color] flag. *)

val use_color_stdout : unit -> bool
(** [use_color_stdout ()] is {!resolve_color} of {!color_mode} for standard
    output. *)

val use_color_stderr : unit -> bool
(** [use_color_stderr ()] is {!resolve_color} of {!color_mode} for standard
    error. *)

(** {1:standalone Settings with no flag}

    The variables no runner flag can set, and which therefore have no entry in
    {!Cli}'s table to be read from. *)

val columns : unit -> int option
(** [columns ()] is [WINDTRAP_COLUMNS], a terminal width override. Non-positive
    or unparseable values count as unset. *)

val tail_errors : unit -> int option
(** [tail_errors ()] is [WINDTRAP_TAIL_ERRORS], the maximum number of
    captured-output lines shown per failure. *)

val allow_focus : unit -> bool
(** [allow_focus ()] is [true] iff [WINDTRAP_ALLOW_FOCUS] is truthy. Lifts the
    CI guard on focused tests. *)

val project_root : unit -> string option
(** [project_root ()] is [WINDTRAP_PROJECT_ROOT], overriding project-root
    discovery. *)

(** {1:snapshots Snapshot update modes} *)

(** The type for snapshot update modes, from [-u] or [WINDTRAP_UPDATE]. *)
type update =
  | No_update  (** Check against baselines (the default). *)
  | Update  (** Accept mismatches, refused when {!in_ci}. *)
  | Force_update  (** Accept mismatches even under CI. *)

val update : unit -> update
(** [update ()] parses [WINDTRAP_UPDATE]: truthy values are {!Update}, [force]
    (case-insensitively) is {!Force_update}, anything else (including unset) is
    {!No_update}. The vocabulary is the variable's own — the [-u] flag it
    mirrors has no way to spell [force] — so, unlike the mirrors read through
    {!get_string}, it is parsed here and the CLI layer defers to it. *)
