(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The instrumentation runtime: what every instrumented closure links.

    Code instrumented by [ppx_windtrap.coverage] calls {!Coverage}, code
    instrumented by [ppx_windtrap.mutate] calls {!Mutate}; the windtrap core
    drives {!Mutate} from its mutation loop and writes verdicts through
    {!Verdicts}; the [windtrap] command merges the files of both formats. Stdlib
    only, and it reads no environment variable but [WINDTRAP_COVERAGE_FILE]. *)

module Instr = Instr
(** Shared plumbing of the two data-file formats: build paths, writer
    identities, the atomic write, and the parser scaffolding. *)

module Coverage = Coverage
(** Expression coverage: the point registry, the [.coverage] dump written at
    exit, and the report data. *)

module Mutate = Mutate
(** Mutation: the mutant catalogue, the arming guard, and the reach map. *)

module Verdicts = Verdicts
(** Mutation verdicts and the [.mutants] file the loop writes and
    [windtrap mutants] merges. *)
