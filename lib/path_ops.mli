(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Filesystem path operations: project-root discovery, dune sandbox path
    reconstruction with containment proof, and safe path components.

    Reconstruction serves the snapshot layer: compile-time source paths (from
    [__POS__] or debug info) are mapped back to the source tree of the project,
    and a path that cannot be {e proven} to lie under the project root is an
    error, never a guess — update mode must not create directories from an
    unverified reconstruction.

    Paths returned by this module use ['/'] as separator. *)

(** {1:root Project root} *)

val project_root : unit -> string
(** [project_root ()] is the project root directory: the [WINDTRAP_PROJECT_ROOT]
    variable when set (made absolute against the current directory if relative),
    otherwise the nearest ancestor of the current directory containing
    [dune-project], [dune-workspace], or [.git]. The marker walk starts above
    any [_build] component of the current directory (build trees contain copied
    marker files and sandbox decoys), so under [dune runtest] it finds the real
    workspace root. Falls back to the current directory when no marker exists.
*)

val default_log_dir : unit -> string
(** [default_log_dir ()] is [<project_root>/_build/_tests], the base directory
    for capture logs and the last-failed store. *)

(** {1:reconstruction Sandbox reconstruction} *)

val strip_build_prefix : string -> string
(** [strip_build_prefix path] removes a [_build/<context>/] segment from [path],
    if any: everything up to the first [_build] component and the context
    component after it is dropped from that point (a leading absolute prefix
    before [_build] is kept). Separators are normalized to ['/']. A trailing
    [_build] with no context component is kept. For example,
    [strip_build_prefix "/w/_build/default/test/t.ml"] is ["/w/test/t.ml"]. *)

val reconstruct : root:string -> string -> (string, string) result
(** [reconstruct ~root file] maps the compile-time source path [file] to an
    absolute path under [root]: strips any [_build/<context>/] segment, resolves
    relative paths against [root], and lexically normalizes [.] , [..] and
    repeated separators. It is [Ok abs] only when [abs] is proven to lie
    strictly under [root]; otherwise [Error candidate], where [candidate] is the
    unproven path for the error report. [root] must be absolute. The proof is
    lexical: symlinks are not resolved, and the target need not exist (update
    mode creates it). *)

(** {1:display Display paths} *)

val display : string -> string
(** [display path] is [path] as printed in reports and command hints: any
    [_build/<context>/] segment stripped ({!strip_build_prefix}), interior ["."]
    and empty segments dropped ([".."] untouched), and a leading {!project_root}
    prefix removed — so the printed path is project-root relative and
    byte-identical across every producer of the line class, the library and
    inline runners alike. Best effort: a path outside the root is returned
    normalized, otherwise unchanged. *)

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
    becomes ['_']. Empty strings, ["."] and [".."] become ["unnamed"]. Results
    longer than 80 bytes are truncated to 40 bytes plus a digest suffix to stay
    unique and within filesystem limits. *)

(** {1:fs Filesystem helpers} *)

val file_exists : string -> bool
(** [file_exists path] is [true] iff [path] exists; [false] on any error (e.g.
    permissions). *)

val mkdir_p : string -> unit
(** [mkdir_p path] creates [path] and any missing parents with permissions
    [0o770]. Does nothing for components that already exist. *)

val collapse_home : string -> string
(** [collapse_home path] replaces a leading home-directory prefix of [path] with
    ["~"] for display. The prefix must end at a path-component boundary:
    ["/home/user2"] is not under the home directory ["/home/user"]. [path]
    unchanged when the home directory is unknown or not such a prefix. *)
