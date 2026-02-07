(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Test suite execution.

    Runs a list of {!Test.t} values, applies filtering, captures output, reports
    progress, and returns aggregate results. Exits early on duplicate test
    names. *)

(** {1 Types} *)

type result = { passed : int; failed : int; skipped : int }
(** Aggregate counts from a test run. *)

type filter = path:string -> Tag.t -> [ `Run | `Skip ]
(** Decides whether to run a test based on its full path and effective tags. *)

type config = {
  filter : filter;  (** Test filter applied before each case. *)
  progress_mode : Progress.mode;  (** Output format for progress reporting. *)
  log_dir : string;  (** Directory for captured stdout/stderr logs. *)
  capture : bool;  (** When [false], streams output to the console. *)
  snapshot_config : Snapshot.Config.t;  (** Snapshot testing settings. *)
  bail : int option;
      (** Stop the suite after this many failures. [None] runs all tests. *)
  junit_file : string option;  (** Path for JUnit XML output, if any. *)
  seed : int option;
      (** Random seed for property tests. Propagated to {!Windtrap_prop.Prop}.
      *)
  default_timeout : float option;
      (** Default timeout in seconds, used when a test has no per-test timeout.
      *)
  prop_count : int option;
      (** Override property test count. Applied globally when no per-test config
          is set. *)
}

(** {1 Running} *)

val default_config : unit -> config
(** [default_config ()] returns a config that runs all tests in
    {!Progress.Verbose} mode with output capture enabled. *)

val run : ?config:config -> string -> Test.t list -> result
(** [run ?config name tests] executes [tests] under suite [name].

    Aborts with exit code 1 if any test names are duplicated. Each test runs
    with an isolated random state (seed 137) for reproducibility. Setup and
    teardown hooks on groups are called around their children. *)

val list_tests : string -> Test.t list -> unit
(** [list_tests name tests] prints all test paths to stdout in sorted order
    without running them. *)

(** {1 Filtering} *)

val make_filter :
  quick:bool ->
  filter_pattern:string option ->
  required_tags:string list ->
  dropped_tags:string list ->
  filter
(** [make_filter ~quick ~filter_pattern ~required_tags ~dropped_tags] builds a
    filter that skips:
    - slow tests when [quick] is [true],
    - tests with the ["disabled"] label,
    - tests whose path does not contain [filter_pattern] (substring match),
    - tests missing any of [required_tags],
    - tests having any of [dropped_tags]. *)
