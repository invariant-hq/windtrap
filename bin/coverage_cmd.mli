(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The [windtrap coverage] subcommand: coverage reporting.

    Finds the [.coverage] files instrumented test executables wrote under
    [_build/_coverage] (resolving the project root as the runtime does — the
    parent of the topmost [_build] component of the current directory, else the
    nearest ancestor with a [_build/_coverage]), or under explicit [PATH]
    arguments; excludes, with a warning naming each one, dumps whose recorded
    executable was deleted or rebuilt since the run; merges the rest — loudly
    rejecting foreign formats and mismatched point tables — and renders the
    merged per-file report through the library renderer. [--min] gates CI;
    [--json] is the machine-readable artifact.

    The exclusion has no override, deliberately: a total computed from a dump
    known to describe another build is a number that can only mislead. *)

val run : string list -> int
(** [run args] executes the subcommand on [args] (the arguments after
    [coverage]) and is the process exit code:

    - [0] — report rendered, and the [--min] threshold, when given, was met;
    - [1] — no [.coverage] files were found; an explicit [PATH] argument named a
      missing file or a file without the [.coverage] suffix; a file was
      unreadable, corrupt, of a foreign format version, or carried a mismatched
      point table; every file was orphaned or stale; or total coverage fell
      below [--min];
    - [2] — usage error (unknown flag, malformed [--min]).

    Explicit [PATH] arguments are a contract: a file argument must exist and
    carry the [.coverage] suffix, and a violation is an error naming the path
    and the reason — never a silent narrowing of the merge. A directory argument
    contributes the [.coverage] files found under it, however many that is.

    The gate compares raw percentages, never their renderings. A failed
    verdict states the threshold as given and the measurement as the report
    line states it — ["minimum 80%: FAILED — 75.1% (5527/7363 points)"] — so
    coverage that renders equal to the threshold can still fail, and the
    fraction says why.

    Reports and the [--min] verdict print on standard output; errors and
    staleness warnings print on standard error, as does the [--min] verdict
    under [--json], whose standard output is exactly the JSON document. *)
