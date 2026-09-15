(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Filesystem path operations: the project root and the log root, dune sandbox
    path reconstruction with containment proof, and safe path components.

    Reconstruction serves the baseline layer: compile-time source paths (from
    [__POS__] or debug info) are mapped back to the source tree of the project,
    and a path that cannot be {e proven} to lie under the project root is an
    error, never a guess — update mode must not create directories from an
    unverified reconstruction.

    Paths returned by this module use ['/'] as separator. *)

(** {1:root Project root and log root} *)

val build_dir_of_path : string -> string option
(** [build_dir_of_path path] is the build directory [path] lies in — [path] cut
    after its first component whose name starts with [_build], e.g.
    ["/w/_build"] for ["/w/_build/default/test/t.exe"] and ["/w/_build_ci"] for
    ["/w/_build_ci/.sandbox/3f/default"] — or [None] when no component does.
    Lexical: nothing is checked on disk, and backslashes are read as separators.
*)

val build_dir : unit -> string option
(** [build_dir ()] is the build directory this process belongs to:
    {!build_dir_of_path} of [INSIDE_DUNE] when that variable holds a path with a
    build component — dune exports the build context, [<root>/_build/default] (a
    private [--build-dir] likewise, and a sandboxed action keeps the value and
    only moves its working directory under [_build/.sandbox]) — else of
    [Sys.executable_name], a binary run by hand from under a build directory;
    else [None]. Relative paths are made absolute against the current directory.
*)

val project_root : unit -> string
(** [project_root ()] is the project root directory: [WINDTRAP_PROJECT_ROOT]
    when set (made absolute against the current directory if relative), else the
    parent of {!build_dir} when there is one — which covers [dune runtest],
    [dune exec] from any directory, and a build binary run by hand — else the
    current directory. No marker file is consulted: a binary outside any build
    directory run from a subdirectory of its project is what the variable is
    for. *)

val default_log_dir : unit -> string
(** [default_log_dir ()] is the root directory for capture logs and the
    last-failed store when [-o] does not name one: [<build_dir>/_tests] when
    {!build_dir} is found — so a private build directory keeps its own logs —
    else [<temporary directory>/windtrap] ({!Filename.get_temp_dir_name}), so a
    tree built without dune never grows a [_build]. Both are keyed by suite
    below that root. *)

(** {1:reconstruction Sandbox reconstruction} *)

val reconstruct : root:string -> string -> (string, string) result
(** [reconstruct ~root file] maps the compile-time source path [file] to an
    absolute path under [root]: strips a [_build/<context>/] segment, resolves
    relative paths against [root], and lexically normalizes [.] , [..] and
    repeated separators. It is [Ok abs] only when [abs] is proven to lie
    strictly under [root]; otherwise [Error candidate], where [candidate] is the
    unproven path for the error report. [root] must be absolute. The proof is
    lexical: symlinks are not resolved, and the target need not exist (update
    mode creates it).

    The strip is of the {e first} build directory component (a basename starting
    with [_build]) and the context component after it — or, for a sandboxed
    action, the [.sandbox/<hash>/<context>] components after it — keeping any
    absolute prefix before it and normalizing separators to ['/']:
    ["/w/_build/default/test/t.ml"] resolves as ["/w/test/t.ml"] would. A build
    directory with no context after it is not a build prefix and is kept. There
    is no export for the strip alone: a reconstruction that is not proven to lie
    under the root is exactly what this module refuses to hand out. *)

val build_root : string -> string option
(** [build_root dir] is the build context [dir] lies in — [dir] cut after its
    first build directory component and the context after it, e.g.
    ["/w/_build/default"] for ["/w/_build/default/test"] and
    ["/w/_build/.sandbox/3f/default"] for a sandboxed action's directory — or
    [None] when [dir] holds no such prefix. Lexical: nothing is checked on disk.
    A run started inside a build context is a build action, and dune's copy of a
    source file [f] under the project root is
    [<build root>/<f relative to the root>]. *)

(** {1:display Display paths} *)

val display : string -> string
(** [display path] is [path] as printed in reports and command hints: a leading
    {!project_root} prefix removed and then a [_build/<context>/] segment
    stripped from the remainder, as {!reconstruct} strips it — or, for a path
    not under the root, the segment stripped first and the root prefix removed
    from the result; interior ["."] and empty segments dropped ([".."]
    untouched) — so the printed path is project-root relative and byte-identical
    across every producer of the line class, the library and inline runners
    alike. Best effort: a path outside the root is returned normalized,
    otherwise unchanged. *)

val display_artifact : string -> string
(** [display_artifact path] is [path] with a leading {!project_root} prefix
    removed and nothing else — the form for a path that names a real file under
    [_build], such as a capture log. {!display} is wrong for those: its
    [_build/<context>/] strip is there because dune shadow-copies {e sources}
    under [_build], and applying it to a genuine build artifact yields a path
    that does not open.

    Total, as is {!display}: both read the cwd to find the root, and a test that
    chdirs into a directory it then removes makes that raise. Since these are
    printed from inside failure reports, they fall back to the path as given
    rather than taking the run down after the tests have finished. *)

(** {1:components Path components} *)

val sanitize_component : string -> string
(** [sanitize_component s] is [s] as a safe single path component:
    alphanumerics, ['-'], ['_'] and ['.'] are kept, every other character
    becomes ['_'].

    The mapping is injective. A name it altered — and ["."], [".."], and the
    empty string, which become ["unnamed"] — carries a short digest of [s] as
    given, because the replacement alone is many-to-one: ["parse: empty"] and
    ["parse, empty"] would otherwise name one file, and {!Capture} opens that
    file [O_TRUNC]. A name it did not alter is returned unchanged, so ordinary
    identifiers stay readable. The digest is of the original, so it is stable
    across runs and independent of execution order.

    Results longer than 80 bytes are truncated to 40 bytes plus a full digest,
    to stay within filesystem limits. *)

(** {1:fs Filesystem helpers} *)

val file_exists : string -> bool
(** [file_exists path] is [true] iff [path] exists; [false] on any error (e.g.
    permissions). *)

val mkdir_p : string -> unit
(** [mkdir_p path] creates [path] and any missing parents with permissions
    [0o770]. Does nothing for components that already exist. *)
