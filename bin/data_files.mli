(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Locating and vetting the instrumentation data files.

    The [coverage] and [mutate] subcommands read the same kind of estate: a
    directory of data files under a project's [_build], each written by an
    instrumented test executable. This module is their shared half — resolving
    the project root as the runtimes resolve their output paths, walking
    directories, expanding explicit [PATH] arguments, and judging each file's
    freshness from the writer identity it records. Both commands exclude a
    flagged file and warn; the wording of the warning and of the remedy stays
    with each command. *)

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
    as the runtimes resolve theirs: the parent of the topmost [_build]
    component of the current directory when inside one, else the nearest
    ancestor with a [_build/<dir>] directory (never inside a sandbox — planted
    garbage under [_build/.sandbox] must not capture the scan). No root found
    is [Ok ([], ["."])].

    Explicit [paths] replace that default and [roots] is [["."]]. They are a
    contract: a file argument must exist and carry the [.<ext>] suffix, and a
    violation is [Error message] naming the path and the reason — never a
    silent narrowing of the merge. A directory argument contributes the
    [.<ext>] files found under it at any depth, however many that is. *)

val self_written : Windtrap_instr.identity option -> bool
(** [self_written identity] is [true] when [identity] names the running
    executable. A reporting binary that is itself instrumented dumps its own
    data, at exit, into the directory it just read; such a file is this
    command's exhaust, not the suite's data, and callers drop it before
    merging. Always [false] for an uninstrumented [windtrap], which writes
    nothing. *)

(** The type for a data file's freshness, judged from the
    {!Windtrap_instr.identity} it records. [Orphan] and [Stale] carry the
    recorded executable identity. *)
type freshness =
  | Fresh
      (** The recorded executable wrote this file — or the file records no
          identity (hand-written or merged), which is never flagged. *)
  | Orphan of string  (** The recorded executable no longer exists. *)
  | Stale of string
      (** The executable on disk is not the one that wrote the file. *)

val freshness : path:string -> Windtrap_instr.identity option -> freshness
(** [freshness ~path identity] judges the data file at [path] against the
    [identity] it recorded. A relative identity is a path below [_build]; the
    file's own topmost-[_build] root locates that [_build] — the same root
    whether the file was discovered or named on the command line — and an
    identity nothing can locate is [Fresh], merged rather than guessed about.
    The comparison is content-based (the digest), because mtimes prove
    nothing: dune's shared cache restores rebuilt artifacts with their
    original timestamps. *)

val describe : stale_hint:string -> path:string -> freshness -> string
(** [describe ~stale_hint ~path f] is the warning line for a flagged file:
    orphans name the missing executable, and [Stale] appends [stale_hint] —
    each command's own wording of the re-run that heals it. Never call it on
    [Fresh]. *)
