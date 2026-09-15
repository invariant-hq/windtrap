(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The [windtrap mutants] subcommand: project-level mutation reporting.

    Finds the [.mutants] verdict files that instrumented test executables wrote
    — under the build directory's [_mutants], or under [_windtrap/mutants] in a
    tree built without one, located as the runtime locates its output
    ({!Data_files.discover}) — or under explicit [PATH] arguments; excludes
    verdicts whose recorded executable was deleted or rebuilt since the run;
    merges the rest — loudly rejecting foreign formats — under
    {b killed anywhere wins}, and renders through the library renderer the
    survivors that survived {e everywhere}, the mutants no executable reached,
    and the project's summary line. A merge runs nothing and seeds nothing, so
    that line carries neither a duration nor a seed, and it never scopes itself
    to one executable the way a single run's does.

    A library covered by several test executables is the normal case, and
    mutation verdicts do not merge the way coverage counts do: coverage merges
    by addition, so two executables over one file can only agree more, while a
    mutant {e killed} by one suite and merely {e reached} by another is killed
    and the surviving suite's view alone is a false survivor. This command is
    why the verdict file exists.

    It runs no tests and drives no build, which is what makes it legitimate
    under Law 12 where a subcommand that {e drives} the run was rejected — the
    verb says so: it reports mutants, it does not mutate. It is the project's
    gate: a mutant that survived every executable that reached it fails the
    merge. *)

val run : string list -> int
(** [run args] executes the subcommand on [args] (the arguments after [mutants])
    and is the process exit code:

    - [0] — report rendered and no mutant survived the merge. Unreached mutants
      alone are not red: a mutant no executable's tests evaluate is a
      coverage-style finding, listed and not scored;
    - [1] — report rendered and at least one mutant survived every executable
      that reached it. There is deliberately no [--min] and no
      [--max-survivors]: one survivor is the failure, and an equivalent mutant
      is dismissed at its site with [[@mutate off]], not absorbed by a
      threshold;
    - [1] — no [.mutants] files were found; an explicit [PATH] argument named a
      missing file or a file without the [.mutants] suffix; a file was
      unreadable, corrupt or of a foreign format version; or every file was
      orphaned or stale;
    - [2] — usage error (unknown flag).

    Explicit [PATH] arguments are a contract: a file argument must exist and
    carry the [.mutants] suffix, and a violation is an error naming the path and
    the reason — never a silent narrowing of the merge, which under
    killed-anywhere-wins would turn another executable's kill back into a
    survivor. A directory argument contributes the [.mutants] files found under
    it at any depth, however many that is.

    Discovery and explicit arguments differ in one further way, because they
    settle different source roots: discovery knows the project root and resolves
    a survivor's excerpt against it, while explicit arguments resolve against
    the current directory, so a file named from outside its checkout reports its
    survivors without excerpts. Excerpts are best-effort throughout — a survivor
    whose source cannot be read still names its file, line and rewrite.

    An orphaned or outdated verdict file is excluded and warned about, never
    merged: a verdict from a previous build can claim a kill the code no longer
    earns, and a false kill hides a live defect where a false survivor merely
    wastes a reader's time. One warning line names each excluded file, and one
    sentence after them says what heals both cases — re-running the mutation
    tests rewrites an outdated verdict, deleting the directory drops an orphan.
    The check needs the file's own [_build] to resolve the executable it names:
    a verdict file copied out of one — a CI artifact, say — records an identity
    nothing can locate, and is merged rather than guessed about.

    The report prints on standard output; errors and staleness warnings print on
    standard error. Each witness names the executable that ran it — the basename
    of the identity its verdict file records, or the library's [.inline-tests]
    directory for dune's inline-test runner, whose basename is the same in every
    library — but is not located: a test's declaration site lives in the
    executable's test tree, which this command does not link. *)
