(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Environment variable reading and platform detection.

    Every environment variable windtrap consults is read here, so the full
    inventory is this interface. Readers are plain functions that re-read the
    environment on every call; nothing is cached. A variable set to the empty
    string counts as unset.

    Boolean variables accept [1], [true], [yes], [y], [on] and their negations,
    case-insensitively; unparseable values count as unset. The presence-style
    variables [CI], [GITHUB_ACTIONS] and [INSIDE_DUNE] are looser: they are
    conventionally set to arbitrary values by other tools, so any value other
    than an explicit falsy spelling counts as set.

    Precedence (programmatic > CLI > env > default) is resolved by the CLI
    layer, which is why most readers return an [option] rather than a default.
*)

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

(** {1:run Run control}

    Environment mirrors of runner CLI flags; under [dune runtest] these are the
    CLI. *)

val seed : unit -> string option
(** [seed ()] is [WINDTRAP_SEED], the root seed token (["s1:<16 hex>"]),
    unparsed — the seed module owns the format. *)

val filter : unit -> string option
(** [filter ()] is [WINDTRAP_FILTER], a substring pattern selecting tests by
    full path. *)

val exclude : unit -> string option
(** [exclude ()] is [WINDTRAP_EXCLUDE], a substring pattern excluding tests by
    full path. *)

val tags : unit -> string list
(** [tags ()] is [WINDTRAP_TAG] split on commas, trimmed, empties dropped. [[]]
    when unset. *)

val exclude_tags : unit -> string list
(** [exclude_tags ()] is [WINDTRAP_EXCLUDE_TAG] split like {!tags}. *)

val timeout : unit -> string option
(** [timeout ()] is [WINDTRAP_TIMEOUT], the default per-test timeout in seconds,
    unparsed — the CLI layer owns validation, like {!seed}'s: a malformed
    winning value is a usage error naming the variable, never a silent default.
*)

val prop_count : unit -> string option
(** [prop_count ()] is [WINDTRAP_PROP_COUNT], the number of generated cases per
    property, unparsed — the CLI layer owns validation, like {!seed}'s. *)

val stream : unit -> bool option
(** [stream ()] is [WINDTRAP_STREAM]: stream test output instead of capturing
    it. *)

val columns : unit -> int option
(** [columns ()] is [WINDTRAP_COLUMNS], a terminal width override. Non-positive
    or unparseable values count as unset. *)

val tail_errors : unit -> int option
(** [tail_errors ()] is [WINDTRAP_TAIL_ERRORS], the maximum number of
    captured-output lines shown per failure. *)

val allow_focus : unit -> bool
(** [allow_focus ()] is [true] iff [WINDTRAP_ALLOW_FOCUS] is truthy. Lifts the
    CI guard on focused tests. *)

(** {1:snapshots Snapshot control} *)

(** The type for snapshot update modes, from [WINDTRAP_UPDATE]. *)
type update =
  | No_update  (** Check against baselines (the default). *)
  | Update  (** Accept mismatches, refused when {!in_ci}. *)
  | Force_update  (** Accept mismatches even under CI. *)

val update : unit -> update
(** [update ()] parses [WINDTRAP_UPDATE]: truthy values are {!Update}, [force]
    (case-insensitively) is {!Force_update}, anything else (including unset) is
    {!No_update}. *)

val prune : unit -> bool
(** [prune ()] is [true] iff [WINDTRAP_PRUNE] is truthy: delete orphaned
    baselines on a full, unfiltered update run. *)

val project_root : unit -> string option
(** [project_root ()] is [WINDTRAP_PROJECT_ROOT], overriding project-root
    discovery. *)

(** {1:coverage Coverage} *)

val coverage : unit -> string option
(** [coverage ()] is the raw value of [WINDTRAP_COVERAGE] (e.g. [report],
    [full]); the coverage layer owns the value vocabulary. *)
