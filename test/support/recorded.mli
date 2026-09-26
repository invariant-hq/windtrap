(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Runs recorded at module initialisation and judged in tests.

    [Run.execute] refuses to start while a run is active, so a suite about it
    runs its scenarios through {!execute} before its own [Windtrap.run], and its
    tests judge the projections below with windtrap's verbs. Every recorded run
    has a fresh scratch log directory, removed when the process exits, and the
    root seed {!seed}. *)

(** {1:environment The stated environment}

    A recorded run starts with every [WINDTRAP_*] variable, [CI],
    [GITHUB_ACTIONS], [INSIDE_DUNE], [NO_COLOR] and [TERM] unset, and then the
    [env] bindings of the call set. The process gets them back when the run
    returns or raises. *)

val seed : Windtrap.Private.Seed.seed
(** [seed] is the root seed of every recorded run. *)

(** {1:execute Recorded executions} *)

type t
(** The type for recorded [Run.execute] calls. *)

val execute :
  ?env:(string * string) list ->
  ?config:(Windtrap.Private.Run.config -> Windtrap.Private.Run.config) ->
  ?on_event:(Windtrap.Private.Run.event -> unit) ->
  ?allowlist:string list ->
  ?suite:string ->
  Windtrap.test list ->
  t
(** [execute tests] is the record of [Run.execute] over [tests].
    - [env] is set in the stated environment. Defaults to [[]].
    - [config] edits the configuration, which is [Run.default_config ()] with
      the scratch log directory and {!seed}. Defaults to the identity.
    - [on_event] and [allowlist] are [Run.execute]'s.
    - [suite] defaults to ["suite"].

    Raises what [Run.execute] raises. *)

val result :
  t -> (Windtrap.Private.Run.outcome, Windtrap.Private.Run.startup_error) result
(** [result t] is what [Run.execute] returned. *)

val outcome : t -> Windtrap.Private.Run.outcome
(** [outcome t] is the outcome of [t]. It fails the test with the startup
    message when the run was refused. *)

val log_dir : t -> string
(** [log_dir t] is the scratch log directory of [t]. *)

(** {1:projections Projections}

    Each fails the test as {!outcome} does when the run was refused. A [path] is
    a test's group names then its own name. *)

val row : t -> string list -> string
(** [row t path] is the row of the test at [path], as one of:
    - [pass];
    - [skip] or [skip <reason>];
    - [fail <phases>] for a failure that counts, and [xfail <phases>] for an
      expected one, where [<phases>] are the phases of its failures in order,
      separated by [", "], as in [fail body, teardown].

    It fails the test when no test at [path] executed. *)

val failures : t -> string list -> Windtrap.Private.Failure.t list
(** [failures t path] is the failures of the test at [path], [[]] for a test
    that passed or skipped. It fails the test when no test at [path] executed.
*)

val executed : t -> string list
(** [executed t] is the path strings of the executed tests, in the order of
    execution. *)

val exit_code : t -> int
(** [exit_code t] is the exit code of [t]'s outcome. *)
