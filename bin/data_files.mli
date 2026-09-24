(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The files that the two reporting commands merge: where they are found, and
    whether each still speaks for the executable that wrote it.

    {!discover} finds the dumps or the verdict files of a
    {!Windtrap_runtime.Instr.format}, by the build-path rule under which the
    runtime wrote them or under the paths of the command line. {!identity} reads
    the writer identity that a file records on its header, {!val-freshness}
    judges the file from it, and {!warnings} and {!all_excluded} are the lines
    for the files that a command excludes.

    The module prints nothing and writes nothing. Each command says the strings
    of this module on standard error behind [windtrap:]. The module reads
    [INSIDE_DUNE], the current directory, the directories that it searches, the
    files whose identity it reads, and the bytes of the executables that it
    judges. *)

(** {1:discovery Discovery} *)

val discover :
  Windtrap_runtime.Instr.format ->
  string list ->
  (string list * string list, string) result
(** [discover format paths] is [Ok (files, roots)]. [files] is the files of
    [format] to merge, those whose name ends in [.] and [format.ext], sorted
    with [String.compare] and without duplicates. [roots] is the directories
    under which a report looks for a recorded source file.

    When [paths] is empty the files are searched for where the runtime writes
    them, so a reader finds what a writer wrote:
    - The build directory is that of the path in [INSIDE_DUNE] when it has one
      ({!Windtrap_runtime.Instr.build_dir}), and otherwise that of the current
      directory. Dune sets [INSIDE_DUNE] to its build context for a rule action
      and under [dune exec] alike. When there is a build directory, [files] is
      every file of [format] at any depth under its
      {!Windtrap_runtime.Instr.data_dir}, and [roots] is the parent of the build
      directory. No other directory is searched, so [files] is [[]] when that
      data directory does not exist.
    - When there is no build directory, the search goes to the nearest ancestor
      of the current directory, itself included, that holds a [_build/_<dir>] or
      a [_windtrap/<dir>] directory, where [<dir>] is [format.dir]. [files] is
      every file of [format] under those of the two that exist, and [roots] is
      that ancestor. The build directory must be named [_build] here. An
      ancestor whose path has a [.sandbox] component is never chosen. When no
      ancestor holds one, the result is [Ok ([], ["."])].

    When [paths] is not empty it replaces the search, and [roots] is [["."]]. A
    path that is a directory contributes every file of [format] under it, at any
    depth, which may be none. Any other path must be an existing file whose name
    ends in the extension of [format]. Otherwise the result is [Error message],
    which names the first such path of [paths] and the reason. A path that
    cannot be used thus fails the whole call and never gives a shorter [files].

    A directory that cannot be listed and an entry that cannot be inspected
    contribute nothing, without an error. *)

(** {1:freshness Freshness} *)

val identity :
  Windtrap_runtime.Instr.format ->
  string ->
  (Windtrap_runtime.Instr.identity option, Windtrap_runtime.Instr.error) result
(** [identity format path] is [Ok identity], the writer identity on the header
    of the file at [path], or [Ok None] when the header records none. The
    records that follow the header are not parsed, so a file whose records are
    corrupt has an identity. The result is the error of
    {!Windtrap_runtime.Instr.read_file} when the file cannot be read,
    [Error (Unknown_format _)] when it does not start with the magic line of
    [format], and [Error (Corrupt _)] when its identity line is malformed. *)

(** The type for the freshness of a file, judged from the
    {!Windtrap_runtime.Instr.identity} that it records. [Orphan] and [Stale]
    carry the [exe] of that identity as it was recorded, which is a path below a
    build directory or an absolute one. *)
type freshness =
  | Fresh
      (** The executable at the recorded path has the recorded digest, or
          nothing can be judged (see {!val-freshness}). *)
  | Orphan of string  (** No executable exists at the recorded path. *)
  | Stale of string
      (** The executable at the recorded path does not have the recorded digest,
          so another build wrote the file. *)

val freshness :
  path:string -> Windtrap_runtime.Instr.identity option -> freshness
(** [freshness ~path identity] is the freshness of the file at [path], which
    recorded [identity]. An absolute [identity.exe] is the path of the
    executable. A relative one is resolved below the build directory of [path]
    ({!Windtrap_runtime.Instr.build_dir}). The comparison is by
    {!Windtrap_runtime.Instr.file_digest} and never by modification time.

    Three files cannot be judged and are [Fresh]: one that records no identity,
    which was written by hand or merged, one whose identity is relative and
    which lies in no build directory, and one whose executable exists and cannot
    be read. *)

val warnings : (string * freshness) list -> string list
(** [warnings excluded] is the warning lines for the [excluded] files, each a
    path with its freshness, which must not be [Fresh]. There is one line for
    each of the first 3 files, in the order given. A line names the path and the
    recorded executable, and says why the file is excluded and that it is. When
    there are more files, a last line counts the rest. Raises [Assert_failure]
    if one of the first 3 files is [Fresh]. *)

val all_excluded : ext:string -> freshness list -> string
(** [all_excluded ~ext excluded] is the sentence for a merge that excluded every
    file. It counts the files, names them by [ext], and says whether they are
    stale, orphaned, or both, with the number of orphaned ones. [excluded] must
    not be empty and must hold no [Fresh], and neither is checked. *)

(**/**)

(* [describe] has no caller outside this module, because the two commands go
   through [warnings]. [describe ~path f] is the warning line of the excluded
   file at [path]: the path, the recorded executable, why the file is excluded
   and that it is. It raises [Assert_failure] on [Fresh]. *)

val describe : path:string -> freshness -> string

(**/**)
