(*---------------------------------------------------------------------------
   Copyright (c) 2015 The mtime programmers. All rights reserved.
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Operating-system access: the monotonic clock, the process environment,
    atomic file writes, and the paths a run resolves.

    Paths returned by this module use ['/'] as separator. *)

(** {1:clock Monotonic clock}

    Monotonic time never goes backwards and ignores system clock adjustments; it
    is the base of every duration windtrap reports. Derived from
    {{:https://erratique.ch/software/mtime}mtime}. *)

type counter
(** The type for points in monotonic time. *)

val counter : unit -> counter
(** [counter ()] samples the current monotonic time. Raises [Sys_error] if the
    platform's monotonic clock is unavailable. *)

val count : counter -> int64
(** [count start] is the number of nanoseconds elapsed since [start],
    non-negative. *)

val count_s : counter -> float
(** [count_s start] is [count start] in seconds. *)

(** {1:env Environment variables}

    Readers re-read the environment on every call; nothing is cached. A variable
    set to the empty string counts as unset. The [WINDTRAP_*] mirror of a runner
    flag is declared beside that flag in {!Cli}'s table and parsed by the flag's
    own parser; this section holds the raw lookup and the value vocabularies the
    parsers share. *)

val getenv : string -> string option
(** [getenv var] is the value of [var], or [None] when it is unset or empty.
    Unparsed and untrimmed. *)

val setenv : string -> string option -> unit
(** [setenv name (Some value)] binds [name] to [value] in the process
    environment; [setenv name None] unbinds it, so [Sys.getenv_opt name] is then
    [None], not [Some ""]. The change is process-global and immediate. The
    primitive under {!Run.setenv}; nothing else in the library writes the
    environment.

    Raises [Invalid_argument] when [name] is empty or contains ['='], and
    [Unix.Unix_error] or [Sys_error] when the environment cannot be changed. *)

val bool_of_string : string -> bool option
(** [bool_of_string s] is the boolean [s] spells, case-insensitively and after
    trimming: [Some true] for [1], [true], [yes], [y] and [on]; [Some false] for
    [0], [false], [no], [n] and [off]; [None] for anything else. Every boolean
    variable reads this vocabulary and refuses a [None]. *)

val bool_expected : string
(** [bool_expected] describes the spellings {!bool_of_string} accepts, for an
    error message's [expected] clause. *)

val split_comma : string -> string list
(** [split_comma value] splits [value] on commas, trims each item and drops the
    empty ones: [WINDTRAP_TAG="a, b ,,c "] is [["a"; "b"; "c"]]. *)

(** {2:platform Platform and CI detection}

    [CI], [GITHUB_ACTIONS] and [INSIDE_DUNE] are set to arbitrary values by
    other tools, so any value but a falsy spelling ({!bool_of_string}) counts as
    set. *)

val inside_dune : unit -> bool
(** [inside_dune ()] is [true] iff [INSIDE_DUNE] is set: the process was started
    by dune. *)

val is_tty_stdout : unit -> bool
(** [is_tty_stdout ()] is [true] iff standard output is a terminal. *)

val term_dumb : unit -> bool
(** [term_dumb ()] is [true] iff [TERM] is exactly [dumb]. *)

val in_ci : unit -> bool
(** [in_ci ()] is [true] iff [CI] is set. Gates focused-test commits, [-u] and
    GitHub annotations. *)

val in_github_actions : unit -> bool
(** [in_github_actions ()] is [true] iff {!in_ci} and [GITHUB_ACTIONS] is set;
    the workflow variable without [CI] is not GitHub Actions. *)

(** {2:color Colour} *)

(** The type for colour preferences, from [--color] or [WINDTRAP_COLOR]. *)
type color_mode =
  | Always  (** Emit ANSI styling unconditionally. *)
  | Never  (** Never emit ANSI styling. *)
  | Auto  (** Style on a terminal or under dune, unless [TERM] is dumb. *)

val color_mode_of_string : string -> color_mode option
(** [color_mode_of_string s] is the mode [s] spells, [always], [never] or [auto]
    case-insensitively, and [None] for anything else. *)

val resolve_color :
  color_mode -> tty:bool -> inside_dune:bool -> term_dumb:bool -> bool
(** [resolve_color mode ~tty ~inside_dune ~term_dumb] is the ANSI decision for
    [mode] on a sink whose terminal status is [tty]: [Always] is [true], [Never]
    is [false], and [Auto] styles iff [tty || inside_dune], not [term_dumb], and
    [NO_COLOR] is unset (any non-empty value counts). The caller names the sink;
    [NO_COLOR] is the one input read here. *)

(** {1:atomic Atomic file writes}

    {!atomic_write} writes a temporary sibling and renames it over the target,
    so no reader ever observes a partial file. Temporaries live in the target's
    directory under {!temp_prefix}; a directory scan skips {!is_temp_name}
    entries. Writes are atomic with respect to observers but not synced to
    stable storage. *)

val temp_prefix : string
(** [temp_prefix] is [".tmp-"], the reserved basename prefix of temporary files.
    Baseline names must not collide with it. *)

val is_temp_name : string -> bool
(** [is_temp_name name] is [true] iff the basename [name] starts with
    {!temp_prefix}. *)

val atomic_write : ?perm:int -> path:string -> string -> unit
(** [atomic_write ~path contents] atomically creates or replaces the file at
    [path] with exactly [contents]: the bytes go to a fresh {!temp_prefix}
    temporary in [path]'s directory, which is then renamed over [path]. On
    failure the temporary is removed (best effort) and [path] is untouched. A
    successful rename replaces [path]'s previous permissions with the
    temporary's. [perm] is the created file's permission bits, subject to the
    umask; defaults to [0o666]. A [path] that names a symbolic link is refused
    before any write.

    Raises [Sys_error], with a message that starts with [path] and names the
    failing step, on failure of any step; [Sys.Break], [Out_of_memory] and
    [Stack_overflow] pass through after the same cleanup. Raises
    [Invalid_argument] if [perm] has bits outside [0o777], before any
    file-system access. *)

(** {1:root Project root and log root} *)

val build_dir_of_path : string -> string option
(** [build_dir_of_path path] is [path] cut after its first component whose name
    starts with [_build], e.g. ["/w/_build"] for
    ["/w/_build/default/test/t.exe"], or [None] when no component does. Lexical;
    backslashes are read as separators. *)

val build_dir : unit -> string option
(** [build_dir ()] is the build directory this process belongs to:
    {!build_dir_of_path} of [INSIDE_DUNE] when that variable holds a path with a
    build component, else of [Sys.executable_name], else [None]. Relative paths
    are made absolute against the current directory. *)

val project_root : unit -> string
(** [project_root ()] is [WINDTRAP_PROJECT_ROOT] when set (made absolute against
    the current directory if relative), else the parent of {!build_dir} when
    there is one, else the current directory. No marker file is consulted. *)

val default_log_dir : unit -> string
(** [default_log_dir ()] is the root of capture logs and the last-failed store
    when [-o] does not name one: [<build_dir>/_tests] when {!build_dir} is
    found, else [<temporary directory>/windtrap]. *)

(** {1:reconstruction Sandbox reconstruction}

    A compile-time source path is mapped back to the project's source tree, and
    a path that cannot be proven to lie under the root is an error, never a
    guess. *)

val reconstruct : root:string -> string -> (string, string) result
(** [reconstruct ~root file] maps the compile-time source path [file] to an
    absolute path under [root]: strips a [_build/<context>/] segment (or the
    [_build/.sandbox/<hash>/<context>/] of a sandboxed action), resolves a
    relative path against [root], and lexically normalizes [.], [..] and
    repeated separators. It is [Ok abs] only when [abs] is proven to lie
    strictly under [root], otherwise [Error candidate] with the unproven path.
    [root] must be absolute. The proof is lexical: symlinks are not resolved and
    the target need not exist. *)

val build_root : string -> string option
(** [build_root dir] is the build context [dir] lies in, cut after its first
    build directory component and the context after it, e.g.
    ["/w/_build/default"] for ["/w/_build/default/test"], or [None]. Lexical.
    Dune's copy of a source file [f] under the project root is
    [<build root>/<f relative to the root>]. *)

(** {1:display Display paths} *)

val display_path : string -> string
(** [display_path path] is [path] as printed in reports and command hints: a
    leading {!project_root} prefix removed, a [_build/<context>/] segment
    stripped as {!reconstruct} strips it, interior ["."] and empty segments
    dropped ([".."] untouched). Best effort: a path outside the root is returned
    normalized, otherwise unchanged. *)

val display_artifact : string -> string
(** [display_artifact path] is [path] with a leading {!project_root} prefix
    removed and nothing else: the form for a real file under [_build], such as a
    capture log. Both display functions are total: when the current directory
    cannot be read, the path is returned as given. *)

(** {1:components Path components} *)

val sanitize_component : string -> string
(** [sanitize_component s] is [s] as a safe single path component:
    alphanumerics, ['-'], ['_'] and ['.'] are kept, every other character
    becomes ['_']. The mapping is injective: a name it altered, and ["."],
    [".."] and the empty string (which become ["unnamed"]), carry a short digest
    of [s]; an unaltered name is returned unchanged. Results longer than 80
    bytes are truncated to 40 bytes plus a full digest. *)

(** {1:fs Filesystem helpers} *)

val file_exists : string -> bool
(** [file_exists path] is [true] iff [path] exists; [false] on any error. *)

val mkdir_p : string -> unit
(** [mkdir_p path] creates [path] and any missing parents with permissions
    [0o770]. Existing components are left alone. *)
