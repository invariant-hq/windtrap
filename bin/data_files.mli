(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Locating and vetting the instrumentation data files.

    The [coverage] and [mutants] subcommands read the same kind of estate: a
    directory of data files, each written by an instrumented test executable
    where the runtime puts them, [_build/_coverage] beside a build directory's
    contexts or [_windtrap] in a tree without one. This module locates the
    estate, expands explicit [PATH] arguments and judges each file's freshness
    from the writer identity it records; each command prints {!describe}'s
    warning per excluded file and its own remedy sentence. *)

val discover :
  Windtrap_runtime.Instr.format ->
  string list ->
  (string list * string list, string) result
(** [discover format paths] is [Ok (files, roots)]: the data files of [format]
    to merge, sorted, and the source roots reports resolve files against.

    With [paths] empty, [files] is every file of [format]'s extension under the
    estate and [roots] is [[root]]. Inside a build directory (the one
    [INSIDE_DUNE] names when it holds a path with a component starting with
    [_build], else the one the current directory is inside) the estate is that
    directory's {!Windtrap_runtime.Instr.data_dir} and the root its parent.
    Outside any, the estate is the nearest ancestor of the current directory,
    itself included, holding a [_build/<dir>] or a [_windtrap/<dir>] (both when
    it holds both), never one inside a [.sandbox] component. No estate found is
    [Ok ([], ["."])].

    Explicit [paths] replace that default and [roots] is [["."]]. A file
    argument must exist and carry the format's extension, else the result is
    [Error message] naming the path and the reason; a directory argument
    contributes the files found under it at any depth, however many. *)

(** The type for a data file's freshness, judged from the
    {!Windtrap_runtime.Instr.identity} it records. [Orphan] and [Stale] carry
    the recorded executable identity. *)
type freshness =
  | Fresh
      (** The recorded executable wrote this file, or the file records no
          identity (hand-written or merged). *)
  | Orphan of string  (** The recorded executable no longer exists. *)
  | Stale of string
      (** The executable on disk is not the one that wrote the file. *)

val freshness :
  path:string -> Windtrap_runtime.Instr.identity option -> freshness
(** [freshness ~path identity] judges the data file at [path] against the
    [identity] it recorded. A relative identity is resolved below the file's own
    build directory ({!Windtrap_runtime.Instr.build_dir}); an identity nothing
    can locate is [Fresh]. The comparison is by content digest, never by mtime.
*)

val describe : path:string -> freshness -> string
(** [describe ~path f] is the one warning line for an excluded file: the path,
    the recorded executable and why it is excluded. Never call it on [Fresh]. *)
