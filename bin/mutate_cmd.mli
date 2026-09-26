(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The [windtrap mutants] command.

    The command merges the verdict files that [--mutate] runs wrote and reports
    on a whole project. It reports the mutants that survived every executable
    that reached them, and the mutants that no executable reached. It only reads
    files, so it runs no test and drives no build. A build gates on its exit
    code, since a [--mutate] run exits 0 whatever it finds.

    {!run} is the whole command.
    {!Windtrap.Private.Report_sections.mutation_report} states what the report
    holds. This interface states the {{!section-files}files} that are merged and
    what the {{!section-merge}merge} decides: the verdicts, the reaching tests,
    the order of the survivors and the launcher of the [reproduce:] command. *)

(** {1:files Files}

    The verdict files are those that {!Data_files.discover} finds for
    [Windtrap_runtime.Verdicts.format] and the [PATH] arguments. A [PATH] that
    cannot be used fails the command and is never skipped, because a file left
    out can hold the one kill of a mutant, which would then be reported as a
    survivor.

    A file is judged from the identity on its header ({!Data_files.identity})
    before it is loaded: a file of another build is excluded whatever its
    records hold, and a file of the current build must load. A file of another
    build is one whose {!Data_files.val-freshness} is not [Fresh]. It is
    excluded from the merge because a verdict of another build can claim a kill
    that the code no longer earns, and no flag keeps it. The first file that is
    not excluded and cannot be read or parsed, its header included, ends the
    command with its {!Windtrap_runtime.Verdicts.pp_error}, because leaving it
    out could turn a killed mutant into a survivor. On standard error the
    command says the {!Data_files.warnings} of the excluded files, then
    {!Data_files.all_excluded} when no file is left, and then once, in a
    sentence of its own, how to refresh them.

    The source of a mutated file is looked for under the roots of
    {!Data_files.discover}: the root of the project after a search, and the
    current directory under [PATH] arguments. A survivor whose source is not
    found keeps its identifier and its rewrite, and its block has no source
    line. *)

(** {1:merge The merge}

    The verdicts are those of [Windtrap_runtime.Verdicts.merge] over the files
    that are kept, in which a kill by one executable wins. A mutant that some
    file reached survives the merge iff it survived in each of those files, and
    a mutant that no file reached is unreached. The counts of the report are
    read off the merged verdicts, and its scope is
    {!Windtrap.Private.Report_sections.scope.Executables} with the number of
    files kept.

    {b Reaching tests.} The reaching tests of a survivor are the union of those
    of every file in which the mutant survived. Each is paired with the label of
    its file, and the pairs are sorted by label and then by test, without
    duplicates. The label is the base name of the executable that the file
    records. For the inline-test runner of dune, whose base name is the same in
    every library, it is the name of the library, [<lib>] in
    [.<lib>.inline-tests/inline-test-runner.exe]. For a file that records no
    identity it is the base name of the file. No reaching test has a location,
    because the command links no test tree.

    {b Order.} The survivors are sorted by their number of reaching tests, the
    most first, and then in the order of [Windtrap_runtime.Mutate.compare_id].

    {b Launcher.} The [reproduce:] command of the report arms the first
    survivor, and its launcher is spelled from one verdict file. That file is
    the first one kept, in path order, that bears the label of the first
    reaching test of that survivor and in which the mutant survived. The
    launcher is a {!type:Windtrap.Private.Run.invocation}:
    - [`Mirrors] when the file records no identity, or that of an inline-test
      runner, which takes its arguments from dune alone.
    - [`Exe] of [dune exec --instrument-with ppx_windtrap.mutate <target> --]
      when the identity is relative, which means an executable below a build
      directory. [<target>] is the identity without its first component, the
      build context, spelled by {!Windtrap.Private.Report_sections.dune_exec}.
    - [`Exe] of the path of the executable, as one such word, when the identity
      is absolute. *)

(** {1:running Running} *)

val run : string list -> int
(** [run args] executes the command on [args], the arguments that follow
    [mutants] on the command line, and is the exit code of the process. It never
    calls [exit].

    The command line is [windtrap mutants [PATH...]]. [-h], [--help] and [-help]
    print the help page on standard output. Every other argument that starts
    with [-] is refused, and the rest are [PATH]s. The command has no [--color]
    flag. [WINDTRAP_COLOR] is the whole colour decision, read by
    {!Windtrap.Private.Cli.color_mode} and resolved for standard output by
    {!Windtrap.Private.Os.resolve_color}.

    [run] parses the arguments, reads [WINDTRAP_COLOR], finds the
    {{!section-files}files}, judges and loads them, and prints the report of the
    {{!section-merge}merge} on standard output, in that order. A step that fails
    returns its code, so a failure prints no report. Every other line goes to
    standard error, through {!Windtrap.Private.Os.say} but for the usage line
    that follows a usage error.

    The result is:
    - [0] when the report was printed and no mutant survived. Unreached mutants
      are listed and do not fail the command. It is also [0] for the help page.
    - [1] when the report was printed and at least one mutant survived every
      executable that reached it. The command has no threshold, so one survivor
      fails it, and a mutant that is equivalent to the original is dismissed at
      its site with [[@mutate off]]. It is also [1], with no report, when a
      [PATH] cannot be used, when no verdict file is found, when a file that is
      not excluded cannot be read, is corrupt or has another format version, or
      when every file is excluded.
    - [2] for an unknown option, which is the one usage error, and for a
      [WINDTRAP_COLOR] that the [--color] flag of a runner would refuse. *)
