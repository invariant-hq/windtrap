(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The ordinary-OCaml half of [ppx_windtrap]: the module-load registry, the
    inline-test-runner protocol, and the undriven-registration guard.

    The rewriter desugars [let%test] and [let%expect_test] into {!add_test}
    calls that run as the test library loads, [module%test] into an
    {!enter_group} / {!leave_group} pair around the module, and every
    [[%expect]] node into a [Windtrap.expect] call inside the body. This module
    only keeps what those calls register, grouped per source file, and hands it
    to [Windtrap.run] — a client of the public API and nothing more: matching,
    corrections and exit codes are the core's.

    {b Life cycle.} Registration is module-global by nature, since module
    initializers run before any run exists. The generated runner main is
    [init Sys.argv; exit ()]: under dune's [inline_tests] backend the argument
    vector carries the [inline-test-runner <lib> -partition <file>] protocol,
    and {!exit} runs the partition under [--corrected] and terminates with the
    run's exit code. *)

(** {1:registration Registration}

    Called from generated module initializers, before any run starts. [file] is
    the compile-time source path; its basename is the test's partition and its
    capitalized module name becomes the grouping group. [tags] come from
    [[@tags]] attributes.

    A name already registered in the same scope — the enclosing [module%test]
    group, or the file's top level — is renamed by appending [" (2)"], [" (3)"],
    …, so that a functor instantiated several times runs every instance under a
    path the runner's uniqueness law accepts. *)

val add_test :
  file:string ->
  pos:Windtrap.pos ->
  tags:string list ->
  string ->
  (unit -> unit) ->
  unit
(** [add_test ~file ~pos ~tags name fn] registers
    [Windtrap.test ~__POS__:pos ~tags name fn] under the group opened by
    {!enter_group} when one is open and at [file]'s top level otherwise. [pos]
    is the extension point's position. *)

val enter_group : file:string -> tags:string list -> string -> unit
(** [enter_group ~file ~tags name] opens a [module%test] group: subsequent
    registrations nest under [name] until the matching {!leave_group}. Groups
    nest freely. *)

val leave_group : unit -> unit
(** [leave_group ()] closes the innermost open group, registering it as
    [Windtrap.group ~tags name children].

    Raises [Invalid_argument] if no group is open. *)

(** {1:collecting Collecting} *)

val collect : unit -> Windtrap.test list
(** [collect ()] drains the registry into a suite: registrations grouped per
    source file under the file's module name ([my_file.ml] → [My_file]), files
    in first-registration order, restricted to the [-partition] file when
    {!init} parsed one. A second call is [[]] until new registrations arrive.
    Draining claims the registry (see {!section:undriven}).

    Raises [Invalid_argument] if a group opened by {!enter_group} was never
    closed. *)

val partitions : unit -> string list
(** [partitions ()] is the sorted basenames of the source files seen by
    registration — the [-list-partitions] answer. *)

(** {1:protocol The runner protocol}

    The backend invokes the generated runner as
    [inline-test-runner <lib> -partition <file>], and once with
    [-list-partitions] to enumerate partitions. *)

val init : string array -> unit
(** [init argv] parses the inline-test-runner protocol out of [argv]:
    [inline-test-runner <lib>] (runner mode and the library name),
    [-partition <file>] and [-list-partitions]. Unrecognized arguments are
    ignored. Every call parses afresh and claims the registry (see
    {!section:undriven}). *)

val exit : unit -> 'a
(** [exit ()] runs the inline suite and terminates the process.

    Not in runner mode — {!init} saw no [inline-test-runner] — it exits [0]: the
    generated runner does nothing when invoked by hand. With [-list-partitions]
    it prints {!partitions}, one per line, and exits [0]. Otherwise it exits
    with [Windtrap.run ~argv:[| argv0; "--corrected" |] lib (collect ())]: one
    runner, whose [WINDTRAP_*] mirrors are the rest of the command line and
    whose exit code under [--corrected] is dune's promotion protocol — a test
    whose failures are all recorded corrections leaves it alone, and a partition
    the mirrors' selection empties exits [0], so the [diff?] that follows is the
    verdict; every other failure exits [1]. Corrections land beside dune's copy
    of the source, where the backend's [diff?] looks. *)

(** {1:undriven The undriven-registration guard}

    The silent success this closes: preprocessed test code inside a plain
    [(executable)] or [(test)] stanza registers its tests at module load, and
    with no [(inline_tests)] stanza nothing ever drives the registry — the
    binary exits [0] having run nothing. So a process that terminates normally
    with registrations no driving path ever claimed prints a diagnostic on
    [stderr] — naming the registered files, the missing stanza and the runner
    protocol — and exits [2], the nothing-ran code.

    {b The claim rule.} The registry is claimed, once for the process's life, by
    {!init} in every mode, and by {!collect}, since whoever drains the registry
    owns the execution of what they took — which covers a hand-rolled main
    driving [Windtrap.run] itself.

    Running a suite claims nothing by itself. A standalone [Windtrap.run]
    executable that also links preprocessed test code it never drains dies with
    the diagnostic, because those registrations can run under no invocation of
    that executable — which is the defect, not a false positive.

    The guard is best-effort, against the silent [0] only: death by signal and
    [Unix._exit] bypass [at_exit], and those endings are already loud or
    deliberate. *)
