(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Runs recorded at module initialisation and judged in tests.

    [Run.execute] and [Run.list_selection] refuse to start while a run is
    active, so a suite about them makes its calls through {!execute} and
    {!list_selection} before its own [Windtrap.run], and its tests judge the
    projections below with windtrap's verbs. A recorded call never raises: the
    exception that escapes it is recorded, and each projection of that call
    fails the test that reads it while the rest of the suite runs. Every
    recorded call has a fresh scratch log directory, removed when the process
    exits, and the root seed {!seed}. *)

(** {1:environment The stated environment}

    A recorded call starts with every [WINDTRAP_*] variable, [CI],
    [GITHUB_ACTIONS], [INSIDE_DUNE], [NO_COLOR] and [TERM] unset, and then the
    [env] bindings of the call set. The process gets them back when the call
    ends. *)

val seed : Windtrap.Private.Seed.seed
(** [seed] is the root seed of every recorded call. *)

(** {1:calls Recorded calls} *)

type 'a t
(** The type for recorded calls that return ['a]: what the call returned or the
    exception that escaped it, and the bytes it wrote to the two output streams.
*)

val returned : 'a t -> 'a
(** [returned t] is what [t]'s call returned. It fails the test, naming the
    exception, when one escaped the call. *)

val escaped : 'a t -> exn option
(** [escaped t] is the exception that escaped [t]'s call, [None] when the call
    returned. Every exception is recorded, [Out_of_memory] and [Sys.Break]
    included. *)

val out : 'a t -> string
(** [out t] is what [t]'s call wrote to standard output. Both output streams go
    to files, descriptors included, for the extent of the call. *)

val err : 'a t -> string
(** [err t] is what [t]'s call wrote to standard error. *)

val log_dir : 'a t -> string
(** [log_dir t] is the scratch log directory of [t]'s call. It does not exist
    before the call, which makes it if it writes there. *)

(** {1:execute Recorded executions} *)

type execution =
  (Windtrap.Private.Run.outcome, Windtrap.Private.Run.startup_error) result t
(** The type for recorded [Run.execute] calls. *)

val execute :
  ?env:(string * string) list ->
  ?config:(Windtrap.Private.Run.config -> Windtrap.Private.Run.config) ->
  ?on_event:(Windtrap.Private.Run.event -> unit) ->
  ?allowlist:string list ->
  ?suite:string ->
  Windtrap.test list ->
  execution
(** [execute tests] is the record of [Run.execute] over [tests].
    - [env] is set in the stated environment. Defaults to [[]].
    - [config] edits the configuration, which is [Run.default_config ()] with
      the scratch log directory and {!seed}. Defaults to the identity.
    - [on_event] and [allowlist] are [Run.execute]'s.
    - [suite] defaults to ["suite"]. *)

val list_selection :
  ?env:(string * string) list ->
  ?config:(Windtrap.Private.Run.config -> Windtrap.Private.Run.config) ->
  ?suite:string ->
  Windtrap.test list ->
  (string list, Windtrap.Private.Run.startup_error) result t
(** [list_selection tests] is the record of [Run.list_selection] over [tests],
    with [env], [config] and [suite] as {!execute} takes them. *)

(** {1:projections Projections}

    Each fails the test when the run was refused, with the startup message, and
    as {!returned} does when an exception escaped it. A [path] is a test's group
    names then its own name. *)

val outcome : execution -> Windtrap.Private.Run.outcome
(** [outcome t] is the outcome of [t]. *)

val row : execution -> string list -> string
(** [row t path] is the row of the test at [path], as one of:
    - [pass];
    - [skip] or [skip <reason>];
    - [fail <phases>] for a failure that counts, and [xfail <phases>] for an
      expected one, where [<phases>] are the phases of its failures in order,
      separated by [", "], as in [fail body, teardown].

    It fails the test when no test at [path] executed. *)

val failures : execution -> string list -> Windtrap.Private.Failure.t list
(** [failures t path] is the failures of the test at [path], [[]] for a test
    that passed or skipped. It fails the test when no test at [path] executed.
*)

val executed : execution -> string list
(** [executed t] is the path strings of the executed tests, in the order of
    execution. *)

val exit_code : execution -> int
(** [exit_code t] is the exit code of [t]'s outcome. *)
