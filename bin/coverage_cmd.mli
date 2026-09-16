(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The [windtrap coverage] subcommand.

    Merges the [.coverage] files instrumented test executables wrote and renders
    expression coverage per source file through the library's report sections.
    Without [PATH] arguments the files are found as {!Data_files.discover} finds
    them; [PATH] arguments ([.coverage] files, or directories searched
    recursively) replace that default. A dump whose recorded executable was
    deleted or rebuilt since the run is excluded with a warning naming it, and
    there is no override. *)

val run : string list -> int
(** [run args] executes the subcommand on [args], the arguments after
    [coverage], and is the process exit code:

    - [0]: report rendered, and every gate given was met;
    - [1]: no [.coverage] file was found; a [PATH] named a missing file or a
      file without the [.coverage] suffix; a file was unreadable, corrupt, of a
      foreign format version or carried a mismatched point table; every file was
      orphaned or stale; total coverage fell below [--min]; or a source under an
      [--expect] path has no coverage data;
    - [2]: usage error (unknown flag, malformed [--min]), or a [WINDTRAP_COLOR]
      the runner's [--color] parser refuses.

    Flags: [--min PCT] gates the raw percentage, never its rendering, and a
    failed verdict states the threshold as given and the measurement as the
    report line states it; [--json] and [--lcov] are machine formats that own
    standard output; [--expect PATH] (repeatable) requires every [.ml], [.mll]
    and [.mly] under [PATH], or [PATH] itself, to have coverage data, and
    [--do-not-expect PATH] exempts a file or directory from it; [-u]
    ([--show-uncovered]) also renders uncovered source excerpts; [-h] prints the
    usage on standard output and returns [0]. Both gates run, so one run names
    everything wrong.

    The report and the [--min] verdict print on standard output; errors,
    exclusion warnings and, under a machine format, the verdict print on
    standard error. *)
