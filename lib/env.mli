(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Centralized environment variable handling.

    All environment variables read by Windtrap are parsed here. Boolean
    variables accept [1], [true], [yes], [y], [on] (and their negations),
    case-insensitively. *)

(** {1 Terminal Detection} *)

val is_tty_stdout : bool Lazy.t
(** [true] when stdout is connected to a TTY. *)

val is_tty_stderr : bool Lazy.t
(** [true] when stderr is connected to a TTY. *)

val inside_dune : bool Lazy.t
(** [true] when the [INSIDE_DUNE] variable is set. *)

(** {1 CI Detection} *)

type ci_mode =
  | Disabled  (** Not running in CI. *)
  | Github_actions  (** Both [CI] and [GITHUB_ACTIONS] are set. *)
  | Unknown_ci  (** [CI] is set but [GITHUB_ACTIONS] is not. *)

val ci_mode : ci_mode Lazy.t
(** Detected CI mode based on the [CI] and [GITHUB_ACTIONS] variables. *)

val in_ci : unit -> bool
(** [in_ci ()] returns [true] if running in any CI environment. *)

val in_github_actions : unit -> bool
(** [in_github_actions ()] returns [true] if running in GitHub Actions. *)

(** {1 Color Support} *)

val use_color : bool Lazy.t
(** Whether to emit ANSI colors on stdout. Determined by [WINDTRAP_COLOR]
    ([always], [never], [auto]), falling back to TTY detection or [INSIDE_DUNE].
*)

val use_color_stderr : bool Lazy.t
(** Same as {!use_color} but for stderr. *)

val setup_color : unit -> unit
(** [setup_color ()] configures the Fmt style renderers for stdout and stderr
    based on {!use_color} and {!use_color_stderr}. Call once at startup. *)

(** {1 WINDTRAP_* Variables} *)

val stream : unit -> bool option
(** [WINDTRAP_STREAM]: stream test output to the console instead of capturing
    it. *)

val format : unit -> string option
(** [WINDTRAP_FORMAT]: output format ([verbose], [compact], [tap], [junit]). *)

val columns : unit -> int option
(** [WINDTRAP_COLUMNS]: override terminal width. Non-positive values are
    ignored. *)

val tail_errors : unit -> int option
(** [WINDTRAP_TAIL_ERRORS]: maximum number of captured-output lines shown per
    failure. *)

val seed : unit -> int option
(** [WINDTRAP_SEED]: random seed for property-based testing. *)

val prop_count : unit -> int option
(** [WINDTRAP_PROP_COUNT]: number of test cases per property test. *)

val update : unit -> bool option
(** [WINDTRAP_UPDATE]: update snapshot files instead of comparing. *)

val snapshot_diff_context : unit -> int option
(** [WINDTRAP_SNAPSHOT_DIFF_CONTEXT]: number of context lines in unified diffs.
*)

val snapshot_max_bytes : unit -> int option
(** [WINDTRAP_SNAPSHOT_MAX_BYTES]: truncation limit for snapshot content. *)

val snapshot_report : unit -> bool option
(** [WINDTRAP_SNAPSHOT_REPORT]: report snapshot updates to the console. *)

val snapshot_dir : unit -> string option
(** [WINDTRAP_SNAPSHOT_DIR]: centralized directory for snapshot files. Overrides
    the default colocated layout. *)

val project_root : unit -> string option
(** [WINDTRAP_PROJECT_ROOT]: project root directory used for path resolution. *)

val filter : unit -> string option
(** [WINDTRAP_FILTER]: substring pattern for filtering tests by name. *)

val exclude : unit -> string option
(** [WINDTRAP_EXCLUDE]: substring pattern for excluding tests by name. *)

val timeout : unit -> float option
(** [WINDTRAP_TIMEOUT]: default timeout in seconds for all tests. *)

(** {1 Tag Filtering} *)

val tag : unit -> string list
(** [WINDTRAP_TAG]: comma-separated list of required tags. *)

val exclude_tag : unit -> string list
(** [WINDTRAP_EXCLUDE_TAG]: comma-separated list of excluded tags. *)
