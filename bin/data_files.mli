(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Locating and vetting the instrumentation data files.

    The [coverage] and [mutants] subcommands read the same kind of estate: a
    directory of data files, each written by an instrumented test executable
    where the runtime's one rule puts them — beside a build directory's contexts
    ([_build/_coverage]), or under [_windtrap] in a tree that has no build
    directory. This module is their shared half — locating that estate as the
    runtime locates its output paths, walking directories, expanding explicit
    [PATH] arguments, and judging each file's freshness from the writer identity
    it records. Both commands exclude a flagged file with one warning line
    ({!describe}) and then say, once, what heals it; that remedy sentence stays
    with each command. *)

val discover :
  Windtrap_runtime.Instr.format ->
  string list ->
  (string list * string list, string) result
(** [discover format paths] is [Ok (files, roots)]: the data files of [format]
    to merge, sorted for deterministic merge order and error attribution, and
    the source roots reports resolve files against.

    With [paths] empty, [files] is every file of [format]'s extension under the
    estate and [roots] is [[root]], the project root, located as the runtime
    locates its output: inside a build directory — the one [INSIDE_DUNE] names
    when it holds a path with a component starting with [_build], dune's context
    under a rule action and under [dune exec] alike, a private [--build-dir]
    included; else the one the current directory is inside — the estate is that
    directory's {!Windtrap_runtime.Instr.data_dir} and the root its parent;
    outside any, the nearest ancestor of the current directory (itself included)
    holding a [_build/<dir>] or a [_windtrap/<dir>] — both when it holds both,
    and never inside a sandbox: planted garbage under [_build/.sandbox] must not
    capture the scan. No estate found is [Ok ([], ["."])].

    Explicit [paths] replace that default and [roots] is [["."]]. They are a
    contract: a file argument must exist and carry the format's extension, and a
    violation is [Error message] naming the path and the reason — never a silent
    narrowing of the merge. A directory argument contributes the files found
    under it at any depth, however many that is. *)

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
    [identity] it recorded. A relative identity is a path below a build
    directory; the file's own ({!Windtrap_runtime.Instr.build_dir}) locates it —
    the same directory whether the file was discovered or named on the command
    line — and an identity nothing can locate is [Fresh], merged rather than
    guessed about. The comparison is content-based (the digest), because mtimes
    prove nothing: dune's shared cache restores rebuilt artifacts with their
    original timestamps. *)

val describe : path:string -> freshness -> string
(** [describe ~path f] is the one warning line for an excluded file: the path,
    the recorded executable, why it is excluded, and that it was. Never call it
    on [Fresh]. *)
