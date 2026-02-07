(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Progress reporting during test execution.

    Provides real-time feedback as tests run. Supports multiple output formats
    and emits GitHub Actions annotations when running in CI. *)

(** {1 Output Mode} *)

type mode =
  | Compact
      (** One character per test: [.] pass, [F] fail, [S] skip. Failures are
          collected and printed at the end. *)
  | Verbose
      (** Tree-style output with group headers, indented test names, and
          per-test timing. *)
  | Tap  (** TAP version 13 (Test Anything Protocol) format. *)
  | Junit
      (** JUnit XML format. Silent during execution; output is written by
          {!print_summary}. *)

(** {1 Test Result} *)

type test_result =
  | Pass of { time : float; attempts : int }
      (** Elapsed time in seconds. [attempts] is [1] when no retries occurred.
      *)
  | Fail of {
      time : float;
      failure : Failure.t;
      output_file : string option;
      attempts : int;
    }
      (** [output_file] is the path to captured stdout/stderr, if capture was
          enabled. *)
  | Skip of { reason : string option }

(** {1 Reporter} *)

type t
(** A mutable progress reporter instance. Not thread-safe. *)

val create : mode:mode -> total_tests:int -> t
(** [create ~mode ~total_tests] returns a fresh reporter with zero counters.
    [total_tests] is the expected number of tests for progress display. Pass [0]
    when the total is unknown. *)

val set_junit_file : t -> string -> unit
(** [set_junit_file t path] configures JUnit XML output to [path]. The file is
    written when {!print_summary} is called. *)

val print_header : t -> name:string -> run_id:string -> unit
(** [print_header t ~name ~run_id] prints the suite header. For {!Tap}, emits
    the TAP version line. Silent for {!Junit}. *)

val report_start : t -> path:string -> groups:string list -> unit
(** [report_start t ~path ~groups] signals that a test is about to run. *)

val report_result :
  t -> path:string -> groups:string list -> test_result -> unit
(** [report_result t ~path ~groups result] records and displays the outcome of a
    single test. Failures are accumulated for display by {!finish}. *)

val finish : t -> unit
(** [finish t] flushes any pending output and prints collected failure details.
    In GitHub Actions, emits [::error] annotations for each failure. *)

val total_time : t -> float
(** [total_time t] returns the cumulative execution time across all reported
    tests, in seconds. *)

val print_summary :
  t -> passed:int -> failed:int -> skipped:int -> time:float -> unit -> unit
(** [print_summary t ~passed ~failed ~skipped ~time ()] prints the final summary
    line. Writes JUnit XML if configured via {!set_junit_file}. For {!Tap},
    emits the plan line ([1..N]). Silent for {!Junit} on the console. *)
