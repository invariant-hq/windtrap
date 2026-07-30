(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The [windtrap coverage] subcommand: coverage reporting.

    Finds the [.coverage] files instrumented test executables wrote under
    [_build/_coverage] (resolving the project root as the runtime does — the
    parent of the topmost [_build] component of the current directory, else the
    nearest ancestor with a [_build/_coverage]), or under explicit [PATH]
    arguments; excludes dumps whose recorded executable was deleted or rebuilt
    since the run (overridable with [--stale]); merges the rest — loudly
    rejecting foreign formats and mismatched point tables — and renders the
    merged per-file report through the library renderer. [--min] gates CI;
    [--json] is the machine-readable artifact. *)

val run : string list -> int
(** [run args] executes the subcommand on [args] (the arguments after
    [coverage]) and is the process exit code:

    - [0] — report rendered, and the [--min] threshold, when given, was met;
    - [1] — no [.coverage] files were found; an explicit [PATH] argument named a
      missing file or a file without the [.coverage] suffix; a file was
      unreadable, corrupt, of a foreign format version, or carried a mismatched
      point table; every file was orphaned or stale; an orphaned or stale dump
      was found under [--stale fail]; or total coverage fell below [--min];
    - [2] — usage error (unknown flag, malformed [--min] or [--stale]).

    Explicit [PATH] arguments are a contract: a file argument must exist and
    carry the [.coverage] suffix, and a violation is an error naming the path
    and the reason — never a silent narrowing of the merge. A directory argument
    contributes the [.coverage] files found under it, however many that is.

    The [--min] verdict prints the threshold with the fewest decimals (one at
    least) that render the exact value the gate compared, and a failed verdict
    prints the actual percentage with the fewest decimals whose rendering is
    numerically below that printed threshold — the printed comparison is never
    false: [--min 72.24] over data at 72.222…% reads ["72.2% is below 72.24%"],
    never ["72.22% is below 72.2%"].

    Reports and the [--min] verdict print on standard output; errors and
    staleness warnings print on standard error, as does the [--min] verdict
    under [--json], whose standard output is exactly the JSON document. *)
