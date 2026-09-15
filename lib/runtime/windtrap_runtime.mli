(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The instrumentation runtime: what every instrumented closure links.

    Code instrumented by [ppx_windtrap.coverage] calls {!Coverage} and code
    instrumented by [ppx_windtrap.mutate] calls {!Mutate}, at module load and at
    every point or site; the windtrap core drives {!Mutate} from the mutation
    loop and writes verdicts through {!Verdicts}; the [windtrap] command loads
    and merges the files of both formats. Stdlib only, and it reads no
    environment variable but [WINDTRAP_COVERAGE_FILE]: which mutants a run tests
    and which one it arms are decisions the core hands down. *)

module Instr = Instr
(** Shared plumbing of the two data-file formats: build paths, writer
    identities, the atomic write, and the parser scaffolding. *)

module Coverage = Coverage
(** Expression coverage: the point registry generated code visits, the
    [.coverage] dump written at exit, and the report data behind the table. *)

module Mutate = Mutate
(** Mutation: the mutant catalogue generated code registers, the arming guard,
    and the reach map the loop reads. *)

module Verdicts = Verdicts
(** Mutation verdicts: the killed-anywhere-wins lattice and the [.mutants] file
    the loop writes and [windtrap mutants] merges. *)
