(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The runtime of the [ppx_windtrap] rewriter: the registry of inline tests,
    dune's inline-test-runner protocol, and the guard against registered tests
    that nothing runs.

    The rewriter turns a [let%test] and a [let%expect_test] into an {!add_test}
    call, and a [module%test] into an {!enter_group} and {!leave_group} pair
    around the module. The calls run while the library under test loads, before
    any run exists, so the registry is global to the process.

    This module keeps what those calls register and hands it to [Windtrap.run].
    It is a client of the [windtrap] library as any suite is, so matching,
    corrections and exit codes are [Windtrap.run]'s. The main that dune's
    [inline_tests] backend generates is [init Sys.argv] and then [exit ()].

    {b Generated code.} The rewriter and this library are one package, so
    generated code and this interface always have the same version. That is why
    generated code may name {!add_test}, {!enter_group}, {!leave_group}, {!init}
    and {!exit}. No stability of these names is promised from one version to the
    next. *)

(** {1:registration Registration}

    Generated module initializers call these values before any run starts. The
    first registration of no library installs the
    {{!section-undriven}undriven guard}.

    [library] is the library that the registration belongs to, which the
    rewriter reads from dune's [library-name] cookie. Code of no library leaves
    it out: the modules of an executable, and code compiled without the cookie.
    A library's registrations are driven by that library's runner alone, the
    process that {!init} starts as [inline-test-runner <library>]. Every other
    process that links the library, a black-box suite included, neither runs
    them nor is refused by them.

    [file] is the path of the source file as the rewriter read it. Its basename
    is the partition of the test. Its module name, the basename up to its first
    dot and capitalized, names the group that {!collect} puts the tests of the
    file under. Two files of one library that have the same basename share a
    partition and a group. [tags] are those of the [[@tags]] attributes.

    A name that its scope already holds gets [" (2)"], then [" (3)"], appended
    in the order of registration, so a functor that is applied several times
    runs each of its instances under a path of its own. The scope is the open
    group, or the top level of the file's module, and the name of a group
    follows the same rule. *)

val add_test :
  ?library:string ->
  file:string ->
  pos:Windtrap.pos ->
  tags:string list ->
  string ->
  (unit -> unit) ->
  unit
(** [add_test ?library ~file ~pos ~tags name fn] registers
    [Windtrap.test ~__POS__:pos ~tags name fn] in the innermost open group, or
    at the top level of [file] when no group is open. [pos] is the position of
    the extension point, and the [name] of an anonymous [let%test _] is
    [line_<N>], after its line. *)

val enter_group :
  ?library:string -> file:string -> tags:string list -> string -> unit
(** [enter_group ?library ~file ~tags name] opens a group for a [module%test].
    The registrations that follow nest under [name] until the matching
    {!leave_group}, and groups nest. While a group is open, the [library] and
    the [file] of an {!add_test} only record a partition, and the test lands in
    the open group whatever library and file it names. *)

val leave_group : unit -> unit
(** [leave_group ()] closes the innermost open group and registers it as
    [Windtrap.group ~tags name children], with its children in the order of
    registration. It lands in the enclosing group, or at the top level of the
    library and the file that {!enter_group} named. Raises [Invalid_argument] if
    no group is open. *)

(** {1:collecting Collecting} *)

val collect : unit -> Windtrap.test list
(** [collect ()] drains the registry into the tests of a suite. It keeps the
    registrations of no library and, once {!init} has read
    [inline-test-runner <lib>], those of [<lib>]. Once {!init} has read a
    [-partition], it keeps only the registrations of that file. The others are
    drained and discarded. Each module name gives one untagged [Windtrap.group],
    [My_file] for [my_file.ml], and the groups come in the order of their first
    registration. A second call is [[]] until new registrations arrive.

    Draining claims the registry for the {{!section-undriven}undriven guard}, so
    a hand-written main passes the result to [Windtrap.run].

    Raises [Invalid_argument] if a group is still open, and the registry is
    claimed by then. *)

(** {1:protocol The runner protocol}

    Dune's [inline_tests] backend runs the generated main once as
    [inline-test-runner <lib> -list-partitions], and then once for each
    partition as [inline-test-runner <lib> -partition <file>]. *)

val init : string array -> unit
(** [init argv] reads the protocol out of [argv]. [inline-test-runner <lib>]
    sets the runner mode and the name of the library, [-partition <file>] sets
    the partition, and [-list-partitions] asks for the listing. Every other
    argument is ignored, so no flag of [Windtrap.run] is read here. [argv.(0)]
    becomes the program name of the run that {!exit} makes.

    Every call reads [argv] afresh and claims the registry for the
    {{!section-undriven}undriven guard}. *)

val exit : unit -> 'a
(** [exit ()] ends the process.
    - Outside the runner mode it exits [0]. That is the case when {!init} saw no
      [inline-test-runner] and when it was never called, so the generated main
      does nothing when it is run by hand.
    - Under [-list-partitions] it prints on standard output the basenames of the
      source files whose registrations {!collect} keeps without a partition,
      sorted, without duplicates and one on each line, and exits [0]. A
      registration that {!collect} has drained still counts.
    - Otherwise it exits with the code of
      [Windtrap.run ~argv:[| argv0; "--corrected" |] suite (collect ())], where
      [suite] is [<lib>/<file>] under [-partition] and [<lib>] without.

    The run gets no other argument, so the [WINDTRAP_*] mirrors are the only way
    to filter, tag or seed an inline run.

    The suite is named after the partition, so the processes of two partitions
    share no directory of capture logs and no record of the last failed tests.
    Each also has a JUnit file of its own when the target of [--junit] is a
    directory. A target that ends in [.xml] is one file, which every partition
    replaces.

    The exit code is the one of [Windtrap.run] under [--corrected]. That flag
    does not change the code of a usage error, such as a malformed mirror, nor
    that of a partition that declares no test, and both exit [2]. *)

(** {1:undriven The undriven guard}

    Test code preprocessed with [ppx_windtrap] registers its tests while it
    loads, in any stanza. An [(executable)] or [(test)] stanza has no inline
    runner, so nothing runs the registrations of its own modules, and without
    the guard such a program would exit [0] having run nothing.

    The first registration of no library installs an [at_exit] function. If the
    process ends and nothing has claimed the registry, the function writes a
    diagnostic on standard error, whatever the flags of a run, and exits [2].
    That is the code of a run in which no test ran. The diagnostic names the
    files of no library that registered, a library with [(inline_tests)] as the
    place for the tests, and the runner protocol. The code becomes [2] whatever
    code the process was ending with, so a process that was exiting [1] with an
    unclaimed registry exits [2].

    A library's registrations never install the function. A library without
    [(inline_tests)] has no runner, and nothing reports that its registrations
    never run.

    {b The claim rule.} {!init} claims the registry in every mode. {!collect}
    claims it too, because whoever drains the registry owns the execution of
    what it took, which covers a hand-written main that calls [Windtrap.run]
    itself. A claim holds for the life of the process. Running a suite claims
    nothing. An executable that runs its own suite and also registers tests of
    no library that it never drains prints its report, then the diagnostic, and
    exits [2], because no invocation of that executable can run those tests.

    The guard stays silent in a forked child that leaves through [Stdlib.exit].
    It exits through [Stdlib.exit], so the [at_exit] functions that remain still
    run, the dump of a coverage build included. *)
