(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The runtime library that instrumented code links.

    Code instrumented by [ppx_windtrap.coverage] calls {!Coverage}, and code
    instrumented by [ppx_windtrap.mutate] calls {!Mutate}, at module load and
    then at every point or site. The mutation loop of the [windtrap] library
    drives {!Mutate} and writes what it finds through {!Verdicts}, and the
    [windtrap] command merges the files of both formats. {!Instr} is what the
    two formats share.

    The library depends on the standard library only, so it cannot call the
    [windtrap] library. It writes its warnings itself, on the standard error of
    the instrumented process and behind the [windtrap:] prefix, whatever the
    flags of a run. It reads one environment variable, [WINDTRAP_COVERAGE_FILE],
    at the first {!Coverage.register} of the process. It never decides which
    mutants a run tests nor which one a process arms. Its caller does, with
    {!Mutate.val-catalogue} and {!Mutate.arm}.

    Loading the library has one effect, the registration of the printer of
    {!Mutate.Runaway}.

    {b Generated code.} The [ppx_windtrap] package depends on the [windtrap]
    package at its own version, so the rewriters and this library are released
    together. That pairing is why generated code may name {!Coverage.register},
    {!Coverage.visit} and the fields of {!Coverage.point}, and
    {!Mutate.register} and the fields of {!Mutate.site}. No stability of these
    names is promised from one version to the next. *)

module Instr = Instr
(** What the two file formats share: build paths, writer identities, atomic
    writes and the scanner. It also holds the warning line of the runtime. *)

module Coverage = Coverage
(** Expression coverage: the point registry, the dump written at exit and the
    report data. *)

module Mutate = Mutate
(** Mutation: the mutant catalogue, the arming guard and the reach map. *)

module Verdicts = Verdicts
(** Mutation verdicts and the [.mutants] file that carries them. *)
