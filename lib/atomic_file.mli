(*--------------------------------------------------------------------------
  Copyright (c) 2026 Thibaut Mattio. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(** Atomic file writes.

    {!write} publishes a file by writing a temporary sibling and renaming it
    over the target, so no reader — including a concurrent or crashed run — ever
    observes a partially written target (RFC Law 1). Snapshot acceptance and the
    last-failed store write through this module and nothing else.

    Temporaries live in the target's directory under the reserved {!temp_prefix}
    name prefix; directory scans such as snapshot orphan reporting and pruning
    must skip entries recognized by {!is_temp_name}. Writes are atomic with
    respect to observers but are not synced to stable storage: durability across
    power failure is out of scope for test artifacts. *)

(** {1:naming Reserved temporary names} *)

val temp_prefix : string
(** [temp_prefix] is [".tmp-"], the reserved basename prefix of this module's
    temporary files. The prefix is part of the on-disk contract: baseline names
    must not collide with it, and scanners must ignore it. *)

val is_temp_name : string -> bool
(** [is_temp_name name] is [true] iff [name] starts with {!temp_prefix}. [name]
    is a directory entry name (a basename), not a path. *)

(** {1:writing Writing} *)

val write : ?perm:int -> path:string -> string -> unit
(** [write ~path contents] atomically creates or replaces the file at [path]
    with exactly [contents].

    The bytes are first written to a fresh {!temp_prefix} temporary in [path]'s
    directory, then renamed over [path]; interrupted system calls are retried,
    and on failure the temporary is removed (best effort) and [path] is left
    untouched. A successful rename replaces [path]'s previous permissions with
    the temporary's.

    [perm] is the created file's permission bits, subject to the process umask
    exactly like any newly created file. Defaults to [0o666].

    On failure of any step, raises [Sys_error] with a message that starts with
    [path] and names the failing step. [Sys.Break], [Out_of_memory], and
    [Stack_overflow] pass through unwrapped, after the same best-effort cleanup.

    Raises [Invalid_argument] if [perm] has bits outside [0o777], before any
    file-system access. *)
