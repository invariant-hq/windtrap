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
    validating differently from the flag it mirrors. One further lookup lives
    elsewhere by design: the coverage runtime reads its own
    [WINDTRAP_COVERAGE_FILE] (windtrap links the coverage library, not the
    reverse, so it cannot depend on this module).

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
(** [is_tty_stdout ()] is [true] iff standard output is a terminal. Standard
    error has no counterpart: nothing in windtrap styles it. *)

val term_dumb : unit -> bool
(** [term_dumb ()] is [true] iff [TERM] is set to exactly [dumb] — the
    conventional "no escape sequences" terminal. Disables ANSI styling in
    {!Auto} mode and gates the renderer's live tail off. *)

(** {1:ci CI detection}

    Two predicates rather than a detected-environment value: nothing in the
    library distinguishes one CI from another beyond GitHub Actions, and both
    answers come from one classification, so a [GITHUB_ACTIONS] without a [CI]
    cannot answer them inconsistently. *)

val in_ci : unit -> bool
(** [in_ci ()] is [true] iff [CI] is set to anything but a falsy spelling ([0],
    [false], ...). Gates focused-test commits, snapshot update refusal, and
    GitHub annotations. *)

val in_github_actions : unit -> bool
(** [in_github_actions ()] is [true] iff {!in_ci} and [GITHUB_ACTIONS] is
    likewise set: the workflow variable alone, without [CI], is not GitHub
    Actions. *)

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
    [Always] still wins). Pure; shared with the [--color] flag.

    This is the whole of the colour decision: there is no reader that resolves
    it for a sink of its own choosing. A caller passes the mode that won its own
    precedence — [Run.config]'s for the runner, {!color_mode} for a command with
    no [--color] flag — together with the sink's terminal status, so the ANSI
    decision is always made where the sink is known. *)

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

val coverage_only : unit -> string list
(** [coverage_only ()] is [WINDTRAP_COVERAGE_ONLY] split on commas: the source
    path prefixes the run's coverage number is allowed to speak about, or [[]]
    (unset) for all of them.

    The in-process coverage registry holds every instrumented library linked
    into the executable, so a run that depends on an instrumented library
    reports {e its} points too and the percentage stops being a statement about
    the code under test. Naming a prefix scopes the inline line and the report
    modes back to it. The [.coverage] dump is deliberately {e not} scoped: the
    file is the raw material [windtrap coverage] merges across executables, and
    narrowing it would lose data no later step can recover. *)

val mutate_only : unit -> string list
(** [mutate_only ()] is [WINDTRAP_MUTATE_ONLY] split on commas: the source path
    prefixes whose mutants a run will consider, or [[]] (unset) for all of them.

    This is not coverage's reporting filter with a different name. The loop
    forks once per mutant, so narrowing the catalogue narrows the {e work}: an
    executable whose mutants all fall outside the prefixes has nothing to test
    and behaves exactly as an uninstrumented one, discovery line included.
    Mutation runs are expensive and a whole-project catalogue is rarely what a
    reader wants to spend an afternoon on; naming a file or a directory is how
    they spend it on the code they are actually working on. The scope binds at
    registration, so it also bounds explicit arming
    ({!Windtrap_mutate.arm_variable}): a mutant of an out-of-scope file was
    never registered and cannot be armed — the scope states what the run's
    mutation surface {e is}, not a view over a larger one. *)

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
