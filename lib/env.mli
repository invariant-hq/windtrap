(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Environment variable access and platform detection.

    This module owns {e how} the environment is read — the one raw lookup below,
    the value vocabularies mirrors share (booleans, comma-separated lists,
    colour modes), platform and CI detection, and the one setting that has no
    command-line flag — and, in {!set}, the one way it is written. It is not the
    inventory of variables: every [WINDTRAP_*] mirror of a runner flag is
    declared beside that flag in {!Cli}'s table, read from there through
    {!get_string} and parsed by the flag's own parser, which is what stops a
    mirror from accepting or refusing differently from the flag it mirrors. One
    further lookup lives elsewhere by design: the coverage runtime reads its own
    [WINDTRAP_COVERAGE_FILE] (windtrap links the runtime, not the reverse, so it
    cannot depend on this module).

    Readers are plain functions that re-read the environment on every call;
    nothing is cached. A variable set to the empty string counts as unset.

    The presence-style variables [CI], [GITHUB_ACTIONS] and [INSIDE_DUNE] are
    conventionally set to arbitrary values by other tools, so any value other
    than an explicit falsy spelling ({!bool_of_string}) counts as set. *)

(** {1:readers Reading}

    One raw lookup and the vocabularies a value is parsed with. A caller passes
    the variable's name, so one reader serves any number of variables, and
    parses the value where it knows what the variable means — reporting a bad
    one by naming the variable, rather than reading a silent default out of a
    typo. *)

val get_string : string -> string option
(** [get_string var] is the value of [var], or [None] when it is unset or empty.
    Unparsed and untrimmed. *)

val bool_of_string : string -> bool option
(** [bool_of_string s] is the boolean [s] spells, case-insensitively and after
    trimming: [Some true] for [1], [true], [yes], [y] and [on]; [Some false] for
    [0], [false], [no], [n] and [off]; [None] for anything else. The vocabulary
    of every boolean variable — a valueless flag's mirror, the switches with no
    flag — which refuse a [None] rather than reading it as unset. *)

val bool_expected : string
(** [bool_expected] describes the spellings {!bool_of_string} accepts — the
    [y]/[n] abbreviations aside — as an error message's [expected] clause. *)

val split_comma : string -> string list
(** [split_comma value] splits [value] on commas, trims each item and drops the
    empty ones — the spelling the repeatable flags take in one variable, e.g.
    [WINDTRAP_TAG="a, b ,,c "] is [["a"; "b"; "c"]]. *)

(** {1:writing Writing} *)

val set : string -> string option -> unit
(** [set name (Some value)] binds [name] to [value] in the process environment;
    [set name None] {e unbinds} it — [Sys.getenv_opt name] is then [None], not
    [Some ""], which is a different fact to every program that asks. [Unix]
    offers only the binding half ([putenv]); the unbinding half is POSIX
    [unsetenv(3)] through a C stub, or on Windows the empty assignment [_putenv]
    documents as deletion.

    The change is process-global, immediate, and visible to every reader — the
    lookups above, [Sys.getenv_opt], and any child process spawned after it.
    Nothing else in the library writes the environment: this is the primitive
    under {!Run.setenv}, which is what test bodies call, and which has the
    runner put the prior binding back at the attempt boundary.

    Raises [Invalid_argument] when [name] is empty or contains ['='] — the names
    POSIX refuses, checked here so one bad name reads the same on every platform
    — and [Unix.Unix_error] or [Sys_error] when the environment itself cannot be
    changed (allocation failure). *)

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
    [false], ...). Gates focused-test commits, baseline update refusal, and
    GitHub annotations. *)

val in_github_actions : unit -> bool
(** [in_github_actions ()] is [true] iff {!in_ci} and [GITHUB_ACTIONS] is
    likewise set: the workflow variable alone, without [CI], is not GitHub
    Actions. *)

(** {1:color Color} *)

(** The type for color preferences, from [--color] or [WINDTRAP_COLOR]. *)
type color_mode =
  | Always  (** Emit ANSI styling unconditionally. *)
  | Never  (** Never emit ANSI styling. *)
  | Auto  (** Style when on a terminal or under dune, unless [TERM] is dumb. *)

val color_mode_of_string : string -> color_mode option
(** [color_mode_of_string s] is the mode [s] spells — [always], [never] or
    [auto], case-insensitively — and [None] for anything else. The one
    vocabulary of [--color] and [WINDTRAP_COLOR]: the flag's parser reads both
    ([Cli.color_mode] for a command with no [--color] flag), so the variable
    accepts and refuses exactly what the flag does. *)

val resolve_color :
  color_mode -> tty:bool -> inside_dune:bool -> term_dumb:bool -> bool
(** [resolve_color mode ~tty ~inside_dune ~term_dumb] is the ANSI decision for
    [mode] on a sink whose terminal status is [tty]: [Always] is [true], [Never]
    is [false], and [Auto] styles iff [tty || inside_dune], the terminal is not
    dumb ({!term_dumb}), and [NO_COLOR] is unset (dune captures output but
    renders escape codes back to the user; a dumb terminal renders none). An
    explicit [Always] wins over all three: the user asked.

    This is the whole of the colour decision: there is no reader that resolves
    it for a sink of its own choosing. A caller passes the mode that won its own
    precedence — [Run.config.color] for the runner, [Cli.color_mode] for a
    command with no [--color] flag — together with the sink's terminal status,
    so the decision is made where the sink is known.

    Effects: reads [NO_COLOR] — the one input taken from the environment rather
    than the caller, because it is a fact about the environment and not about
    any one sink, and every command must honour it. Any non-empty value counts,
    whatever it says; an empty one reads as unset, as everywhere here. *)

(** {1:standalone The setting with no flag} *)

val project_root : unit -> string option
(** [project_root ()] is [WINDTRAP_PROJECT_ROOT], overriding project-root
    discovery. *)
