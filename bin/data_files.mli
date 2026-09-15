(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Locating and vetting the instrumentation data files.

    The [coverage] and [mutants] subcommands read the same kind of estate: a
    directory of data files under a project's [_build], each written by an
    instrumented test executable. This module is their shared half — resolving
    the project root as the runtime resolves its output paths, walking
    directories, expanding explicit [PATH] arguments, and judging each file's
    freshness from the writer identity it records. Both commands exclude a
    flagged file with one warning line ({!describe}) and then say, once, what
    heals it; that remedy sentence stays with each command. *)

val discover :
  dir:string ->
  ext:string ->
  string list ->
  (string list * string list, string) result
(** [discover ~dir ~ext paths] is [Ok (files, roots)]: the data files to merge,
    sorted for deterministic merge order and error attribution, and the source
    roots reports resolve files against. [dir] is the directory under [_build]
    (e.g. ["_coverage"]) and [ext] the extension without its dot (e.g.
    ["coverage"]).

    With [paths] empty, [files] is every [.<ext>] file under
    [<root>/_build/<dir>] and [roots] is [[root]] — the project root, resolved
    as the runtimes resolve theirs: the parent of the topmost [_build] component
    of the current directory when inside one, else the nearest ancestor with a
    [_build/<dir>] directory (never inside a sandbox — planted garbage under
    [_build/.sandbox] must not capture the scan). No root found is
    [Ok ([], ["."])].

    Explicit [paths] replace that default and [roots] is [["."]]. They are a
    contract: a file argument must exist and carry the [.<ext>] suffix, and a
    violation is [Error message] naming the path and the reason — never a silent
    narrowing of the merge. A directory argument contributes the [.<ext>] files
    found under it at any depth, however many that is. *)

(** The type for a data file's freshness, judged from the
    {!Windtrap_runtime.Instr.identity} it records. [Orphan] and [Stale] carry
    the recorded executable identity. *)
type freshness =
  | Fresh
      (** The recorded executable wrote this file — or the file records no
          identity (hand-written or merged), which is never flagged. *)
  | Orphan of string  (** The recorded executable no longer exists. *)
  | Stale of string
      (** The executable on disk is not the one that wrote the file. *)

val freshness :
  path:string -> Windtrap_runtime.Instr.identity option -> freshness
(** [freshness ~path identity] judges the data file at [path] against the
    [identity] it recorded. A relative identity is a path below [_build]; the
    file's own topmost-[_build] root locates that [_build] — the same root
    whether the file was discovered or named on the command line — and an
    identity nothing can locate is [Fresh], merged rather than guessed about.
    The comparison is content-based (the digest), because mtimes prove nothing:
    dune's shared cache restores rebuilt artifacts with their original
    timestamps. *)

val describe : path:string -> freshness -> string
(** [describe ~path f] is the one warning line for an excluded file: the path,
    the recorded executable, why it is excluded, and that it was. Never call it
    on [Fresh]. *)
