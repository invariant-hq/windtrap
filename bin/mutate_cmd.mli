(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The [windtrap mutate] subcommand: project-level mutation reporting.

    Finds the [.mutants] verdict files that instrumented test executables wrote
    under [_build/_mutants] (resolving the project root as the runtime does —
    the parent of the topmost [_build] component of the current directory, else
    the nearest ancestor with a [_build/_mutants]), or under explicit [PATH]
    arguments; excludes verdicts whose recorded executable was deleted or
    rebuilt since the run; merges the rest — loudly rejecting foreign formats —
    under {b killed anywhere wins}, and renders through the library renderer the
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
    under Law 12 where a subcommand that {e drives} the run was rejected. There
    is no gate in this release (Law 16e). *)

val run : string list -> int
(** [run args] executes the subcommand on [args] (the arguments after [mutate])
    and is the process exit code:

    - [0] — report rendered, {e whatever it found}: a survivor never fails a
      build in this release, because the equivalent-mutant rate is a prediction
      until it is measured. There is deliberately no [--min] and no
      [--max-survivors];
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
    wastes a reader's time. The warning names the remedy the exclusion actually
    has, and the two are not interchangeable: a forced re-run rewrites an
    {e outdated} verdict, while an {e orphan} — one whose recorded executable no
    longer exists — is a leftover that no run can replace and only deletion
    removes. The check needs the file's own [_build] to resolve the executable
    it names: a verdict file copied out of one — a CI artifact, say — records an
    identity nothing can locate, and is merged rather than guessed about.

    The report prints on standard output; errors and staleness warnings print on
    standard error. Witnesses are named but not located: a test's declaration
    site lives in the executable's test tree, which this command does not link.
*)
