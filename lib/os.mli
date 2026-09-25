(*---------------------------------------------------------------------------
   Copyright (c) 2015 The mtime programmers. All rights reserved.
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Operating-system access: the monotonic clock, the process environment,
    atomic file writes, the paths that a run resolves and prints, and standard
    error. *)

(** {1:clock Monotonic clock}

    Monotonic time never goes backwards and ignores the adjustments of the
    system clock. *)

type counter
(** The type for points in monotonic time. *)

val counter : unit -> counter
(** [counter ()] is the current point in monotonic time. Raises [Sys_error] if
    the clock of the platform is unavailable or fails. {!count_s} raises the
    same, and so does the initialization of this module, so on such a platform a
    program that links the library fails when it starts. *)

val count_s : counter -> float
(** [count_s start] is the time elapsed since [start], in seconds. It is never
    negative. *)

(** {1:env Environment variables}

    Every reader reads the environment on each call, and nothing is cached. A
    variable that is set to the empty string counts as unset, for every variable
    that is read here and for every mirror.

    The [WINDTRAP_*] mirror of a flag is declared beside that flag in the table
    of {!Cli}. The parser of the flag reads it, so a mirror accepts and refuses
    what its flag does. A caller of {!getenv} must likewise refuse a value that
    it cannot parse, naming the variable, and never read a default out of it. *)

val getenv : string -> string option
(** [getenv var] is the value of [var], or [None] when [var] is unset or empty.
    The value is neither parsed nor trimmed. *)

val setenv : string -> string option -> unit
(** [setenv name (Some value)] binds [name] to [value] in the environment of the
    process. [setenv name None] unbinds it, after which [Sys.getenv_opt name] is
    [None] and not [Some ""]. The change is immediate and belongs to the
    process, so the readers of this module, [Sys.getenv_opt] and every child
    process started afterwards see it. [setenv] restores nothing.

    Raises [Invalid_argument] if [name] is empty or contains [=], before any
    change. Raises [Unix.Unix_error] or [Sys_error] if the environment cannot be
    changed. *)

val bool_of_string : string -> bool option
(** [bool_of_string s] is the boolean that [s] spells, in any case and after
    trimming. It is [Some true] for [1], [true], [yes], [y] and [on],
    [Some false] for [0], [false], [no], [n] and [off], and [None] for any other
    word.

    The reader of a variable decides what a [None] means, and the
    {{!section-platform}presence variables} count it as set. *)

val bool_expected : string
(** [bool_expected] describes the spellings of {!bool_of_string} for the
    [expected] clause of an error, and leaves out [y] and [n]. *)

val split_comma : string -> string list
(** [split_comma value] is the items of [value] between its commas, each
    trimmed, without the empty ones. ["a, b ,,c "] gives [["a"; "b"; "c"]]. *)

(** {2:platform Detecting dune, a CI and the terminal}

    Other tools set [CI], [GITHUB_ACTIONS] and [INSIDE_DUNE] to arbitrary
    values. Each of the three therefore counts as set unless it is unset, empty,
    or a false spelling of {!bool_of_string}, so neither [CI=false] nor [CI=0]
    is a CI. *)

val inside_dune : unit -> bool
(** [inside_dune ()] is [true] iff [INSIDE_DUNE] is set in the sense above,
    which means that dune started the process. *)

val is_tty_stdout : unit -> bool
(** [is_tty_stdout ()] is [true] iff standard output is a terminal. *)

val term_dumb : unit -> bool
(** [term_dumb ()] is [true] iff [TERM] is [dumb], compared as it is. *)

val in_ci : unit -> bool
(** [in_ci ()] is [true] iff [CI] is set in the sense above. *)

val in_github_actions : unit -> bool
(** [in_github_actions ()] is [true] iff {!in_ci} and [GITHUB_ACTIONS] is set in
    the sense above. *)

(** {2:color Colour} *)

(** The type for colour preferences, from [--color] or [WINDTRAP_COLOR]. *)
type color_mode =
  | Always  (** Style, whatever the environment says. *)
  | Never  (** Never style. *)
  | Auto
      (** Style on a terminal or under dune, unless [TERM] is [dumb] or
          [NO_COLOR] is set (see {!resolve_color}). *)

val color_mode_of_string : string -> color_mode option
(** [color_mode_of_string s] is the mode that [s] spells, which is [always],
    [never] or [auto] in any case, and [None] for any other word. It does not
    trim. *)

val resolve_color :
  color_mode -> tty:bool -> inside_dune:bool -> term_dumb:bool -> bool
(** [resolve_color mode ~tty ~inside_dune ~term_dumb] is whether ANSI styling is
    emitted on a sink. [Always] is [true] and wins over [NO_COLOR] and [TERM],
    and [Never] is [false]. [Auto] is [(tty || inside_dune) && not term_dumb]
    when [NO_COLOR] is unset, and [false] when it is set, to any non-empty
    value. [NO_COLOR] is the one input that is read here.

    The caller must pass the mode that won its own precedence, whether the sink
    is a terminal, {!inside_dune} and {!term_dumb}. *)

(** {1:atomic Atomic file writes}

    {!atomic_write} writes a temporary file beside its target and renames it
    over the target, so no reader observes a partial file, not even that of a
    concurrent run or of a run that crashed. The name of a temporary starts with
    [.tmp-], and a scan of a directory that receives such writes must skip the
    entries of that prefix.

    A write is atomic to observers and is not synced to stable storage. A run
    that is killed before the rename leaves its temporary behind. *)

val atomic_write : ?perm:int -> path:string -> string -> unit
(** [atomic_write ?perm ~path contents] creates the file at [path], or replaces
    it, with [contents] and nothing else. The bytes go to a fresh temporary in
    the directory of [path], which is then renamed over [path]. That directory
    must exist, because none is created. A [path] that names a symbolic link is
    refused before any write. On a failure the temporary is removed, as far as
    it can be, and [path] is untouched.

    [perm] is the permission bits of the new file, subject to the umask, and
    defaults to [0o666]. A file that is replaced takes them too, whatever its
    own were.

    Raises [Sys_error] if a step fails or if [path] is a symbolic link, with a
    message that starts with [path] and, for a step, names it. [Sys.Break],
    [Out_of_memory] and [Stack_overflow] pass through after the same cleanup.
    Raises [Invalid_argument] if [perm] has bits outside [0o777], before any
    access to the file system. *)

(** {1:root Project root and log root}

    The build directory of a process is a path cut after its first component
    whose name starts with [_build]. It is [/w/_build] for
    [/w/_build/default/test/t.exe], and [/w/_build_ci] for
    [/w/_build_ci/.sandbox/3f/default]. The path is the value of [INSIDE_DUNE]
    when it holds such a path, and the directory of [Sys.executable_name]
    otherwise, so an executable whose own name starts with [_build] lies in
    none. A relative one is made absolute against the current directory. The
    rule is lexical, it reads every backslash of the path as a separator, on
    every platform, and it spells the directory with [/]. No marker file is
    consulted.

    Reading the current directory raises [Sys_error] when that directory is
    gone, as after a test that removes the directory that it moved into.
    {!project_root} and {!default_log_dir} let the exception pass. *)

val project_root : unit -> string
(** [project_root ()] is [WINDTRAP_PROJECT_ROOT] when it is set, made absolute
    against the current directory. It is else the parent of the build directory
    when there is one, and else the current directory.

    The value of the variable is normalized lexically: [.] and [..] segments,
    repeated separators and a trailing [/] are removed, and no symbolic link is
    resolved. A value whose [..] climbs above the root is kept as it is.

    Raises [Sys_error] if the current directory is needed and cannot be read. *)

val default_log_dir : unit -> string
(** [default_log_dir ()] is the root of the capture logs and of the last-failed
    store when [-o] names none. It is [_tests] under the build directory when
    there is one, and else [windtrap] under [Filename.get_temp_dir_name ()].
    Both are keyed by suite under that root, so two suites share the root and no
    file. Raises [Sys_error] as {!project_root} does. *)

(** {1:reconstruction Source tree and build tree}

    A compile-time source path, from [__POS__] or from debug information, names
    a file as the compiler saw it, which under dune is a copy under [_build]. *)

val reconstruct : root:string -> string -> (string, string) result
(** [reconstruct ~root file] maps the compile-time source path [file] to an
    absolute path under [root]. It proceeds in this order:
    + It reads every backslash of [file] and of [root] as a separator, on every
      platform.
    + It strips from [file] the first build directory component, whose name
      starts with [_build], together with the context after it. That is
      [_build/<context>/], or [_build/.sandbox/<hash>/<context>/] for a
      sandboxed action. What stands before the component is kept, and so is a
      build directory with no context after it. ["/w/_build/default/test/t.ml"]
      resolves as ["/w/test/t.ml"] does.
    + It resolves a relative path against [root].
    + It normalizes [.], [..] and repeated separators lexically.

    The result is [Ok abs] only when [abs] lies strictly under [root]. It is
    otherwise [Error candidate], where [candidate] is the unproven path, not
    normalized. [root] must be absolute, and every call is an [Error] when it is
    not.

    The proof is lexical, so no symbolic link is resolved and the target need
    not exist. On a POSIX system the proven path of a [file] that holds a
    backslash is not the path of that file. [reconstruct] never raises. *)

val build_root : string -> string option
(** [build_root dir] is the build context that [dir] lies in, which is [dir] cut
    after its first build directory component and the context after it, or
    [None] when [dir] has none. It is ["/w/_build/default"] for
    ["/w/_build/default/test"], and ["/w/_build/.sandbox/3f/default"] for a
    directory of a sandboxed action. It is lexical, and reads backslashes as
    {!reconstruct} does.

    The copy that dune makes of a source file [f] under the project root is
    [<build root>/<f relative to the root>]. *)

(** {1:display Display paths} *)

val display_path : string -> string
(** [display_path path] is [path] as reports and printed commands show it, which
    is relative to the project root and the same bytes from every producer.
    - A leading {!project_root} prefix is removed, and a build segment is then
      stripped from the rest as {!reconstruct} strips one.
    - For a [path] that is not under the root, the segment is stripped first and
      the prefix is removed from the result.
    - Empty segments and [.] segments are dropped, [..] is kept, and backslashes
      become [/].

    A path outside the root is returned normalized and otherwise unchanged.
    [display_path] never raises. When the current directory cannot be read, no
    prefix is removed and the rest is done. *)

val display_artifact : string -> string
(** [display_artifact path] is [path] without a leading {!project_root} prefix,
    and nothing else is changed. It is the form for a file that the build wrote
    under [_build], as a capture log is. {!display_path} would strip the build
    segment of such a path, which then does not open.

    [display_artifact] never raises. When the current directory cannot be read,
    [path] is returned as given. *)

(** {1:components Path components} *)

val sanitize_component : string -> string
(** [sanitize_component s] is [s] as one safe path component. ASCII letters,
    digits, [-], [_] and [.] are kept, and every other byte becomes [_].
    - A name that this changes ends in [-] and the first 8 hexadecimal digits of
      the MD5 digest of [s].
    - [.], [..] and the empty string become [unnamed], with the same ending.
    - A name that this does not change is returned as it is.
    - A result longer than 80 bytes, a changed one or not, is cut to its first
      40 bytes, [_] and the whole digest.

    The digest is of [s] as given, so a name maps to the same component in every
    run, whatever the order of execution. Two names that differ only in replaced
    bytes get different components unless their digests collide. The mapping is
    not injective beyond that, because an unchanged name can equal the component
    of another name. *)

(** {1:fs Filesystem helpers} *)

val file_exists : string -> bool
(** [file_exists path] is [true] iff [path] exists, and [false] on any error. *)

val mkdir_p : string -> unit
(** [mkdir_p path] creates the directory [path] and its missing parents, with
    permissions [0o770] under the umask. A component that exists is left alone,
    even when it is a file, and so is one that another process creates
    meanwhile. Raises [Unix.Unix_error] if a directory cannot be created. *)

val failure_reason : path:string -> exn -> string
(** [failure_reason ~path exn] is why an operation on the file [path] failed, in
    words that do not repeat [path], for a message that names it already.
    - A [Sys_error] is its message less a leading [path ^ ": "], which the
      message of {!atomic_write} and that of opening [path] begin with.
    - The [Unix.Unix_error] of {!mkdir_p} is
      [cannot create directory <dir>: <error>], with [<dir>] through
      {!display_path}.
    - Any other exception is its [Printexc.to_string]. *)

(** {1:stderr Standard error} *)

val say : string -> unit
(** [say message] writes ["windtrap: "], [message] and a newline on standard
    error. It is the one form of what windtrap says about itself, at every
    verbosity, never styled and no part of a report.
    - A [message] of several lines is anchored on its first, so the prefix
      prints once.
    - Each line of [message] is written through {!Text.escape_controls}, so a
      control byte other than LF and TAB prints as [\xNN].
    - Standard output is flushed first, [Format.std_formatter] and then the
      channel, so a log that merges the two streams keeps their order. A
      [Sys_error] from that flush is dropped, so a closed standard output does
      not cost the line. [Format.err_formatter] is flushed before the line, and
      standard error after it.

    A client that prints through a formatter of its own, or that draws a live
    line, must flush or erase it first. A [Sys_error] from standard error itself
    is not caught. *)

val warn : string -> unit
(** [warn message] is [say ("warning: " ^ message)]. It is for something that
    the run survives, with its outcome and its exit code unchanged. *)

(**/**)

(* Values with no client but the unit suite. [count start] is the nanoseconds
   elapsed since [start], never negative, and [count_s] is [count] in seconds.
   [temp_prefix] is [".tmp-"], the prefix of the temporaries of [atomic_write].
   [is_temp_name name] is [true] iff [name] starts with [temp_prefix], where
   [name] is a directory entry, a basename and no path. [build_dir_of_path
   path] is [path] cut after its first component whose name starts with
   [_build], or [None] when no component does. It is lexical, reads backslashes
   as separators, spells its result with [/], and lets any component qualify,
   the name of a file included. [build_dir ()] is the build directory of the
   process as the section on roots defines it, or [None], and raises
   [Sys_error] as [project_root] does. [lib/runtime/instr.ml] restates the rule
   of [build_dir_of_path], because the runtime links no core. *)

val count : counter -> int64
val temp_prefix : string
val is_temp_name : string -> bool
val build_dir_of_path : string -> string option
val build_dir : unit -> string option

(**/**)
