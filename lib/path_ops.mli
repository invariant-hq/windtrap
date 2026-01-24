(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Filesystem path operations. *)

val file_exists : string -> bool
(** [file_exists path] returns [true] if [path] exists on the filesystem.
    Returns [false] on any exception (e.g., permission errors). *)

val project_root : unit -> string
(** [project_root ()] returns the project root directory.

    Walks up from the current directory looking for [dune-project],
    [dune-workspace], or [.git]. Skips [_build] directories to handle dune
    sandboxes correctly. Falls back to the current working directory if no
    marker is found.

    Can be overridden via [WINDTRAP_PROJECT_ROOT]. *)

val default_log_dir : unit -> string
(** [default_log_dir ()] returns [<project_root>/_build/_tests]. *)

val mkdir_p : string -> unit
(** [mkdir_p path] creates [path] and all parent directories with permissions
    [0o770]. No-op if the directory already exists. *)

val sanitize_component : string -> string
(** [sanitize_component s] converts [s] to a safe path component. Keeps
    alphanumerics, ['-'], ['_'], and ['.'], replacing all other characters with
    ['_']. Empty strings and [".."] become ["unnamed"]. Names longer than 80
    characters are truncated to 40 characters with an MD5 hash suffix. *)

val collapse_home : string -> string
(** [collapse_home path] replaces a leading home directory prefix with [~].
    Returns [path] unchanged if the home directory cannot be determined. *)
