(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Runtime support for ppx_windtrap.

    Provides the runtime functions called by PPX-generated code for test
    execution, output capture, expectation checking, and corrected file
    generation.

    Do not call these functions directly. They are invoked by code generated
    from [let%expect_test] and [[%expect]] syntax. *)

(** {1 Initialization} *)

val init : string array -> unit
(** [init argv] parses command-line arguments from [argv]. Called once by the
    generated test runner. Recognized flags: [-source-tree-root], [-diff-cmd],
    [-partition], [-list-partitions]. Unrecognized flags are silently ignored.
    Subsequent calls are silently ignored. *)

val exit : unit -> unit
(** [exit ()] runs all registered tests, writes pending corrections, and
    terminates the process. Exits with code 0 on success or when corrections are
    written (to allow dune's diff action to run), and code 1 on test failures
    without corrections. If not running as the test runner, exits immediately
    with code 0. *)

val am_test_runner : unit -> bool
(** [am_test_runner ()] returns [true] if running as the inline test runner
    (i.e., [init] received the [inline-test-runner] argument). *)

(** {1 Test Registration} *)

val set_lib : string -> unit
(** Set the current library name being tested. *)

(** {1 Expectation Checking} *)

type location = { file : string; line : int; start_col : int; end_col : int }
(** Source location for an expect node. *)

val expect : loc:location -> expected:string option -> unit
(** [expect ~loc ~expected] checks captured output against [expected] using
    normalized comparison (trailing whitespace and blank lines are trimmed).
    Pass [None] for [expected] to assert that no output was produced. In inline
    test mode, records a correction for [dune promote]; in executable mode, just
    raises on mismatch.

    @raise Failure.Check_failure if output does not match. *)

val expect_exact : loc:location -> expected:string option -> unit
(** [expect_exact ~loc ~expected] checks captured output against [expected]
    using exact byte-for-byte comparison. Otherwise behaves like {!expect}.

    @raise Failure.Check_failure if output does not match. *)

val output : unit -> string
(** [output ()] returns and consumes raw captured output since the last
    {!expect}, {!expect_exact}, or [output] call. Returns [""] if no output was
    captured. *)

val run_expect_test :
  must_reach_count:int ->
  trailing_loc:location ->
  (unit -> unit) ->
  unit
(** [run_expect_test ~must_reach_count ~trailing_loc fn] executes [fn] with
    must-reach and trailing-output checks. In inline mode, unmatched trailing
    output is recorded as an inserted [[%expect]] correction at [trailing_loc].
*)

(** {1 Module-based Test Syntax}

    These functions support the [module%test] and [let%test] syntax for building
    test trees using a stack-based group nesting model. *)

val add_test :
  ?file:string -> ?tags:string list -> string -> (unit -> unit) -> unit
(** [add_test ?file ?tags name fn] adds a test case to the current group, or to
    the top-level if no group is open. [tags] are attached as label tags. [file]
    is used for inline-test partitioning. *)

val enter_group : ?tags:string list -> string -> unit
(** [enter_group ?tags name] starts a new test group. *)

val leave_group : unit -> unit
(** [leave_group ()] closes the current test group and adds it to its parent
    group (or to the top level).

    @raise Failure if no group is currently open. *)

val run_tests : string -> unit
(** [run_tests name] runs all registered tests under the suite [name].

    This call is optional in executable mode — if omitted, tests auto-execute
    when the program exits via an [at_exit] handler with the default suite name
    ["Tests"]. Use [[%%run_tests "Name"]] to set a custom suite name.

    In inline test mode, defers execution to {!exit} which handles test running
    and correction file generation.

    @raise Failure if any test groups are still open (missing {!leave_group}).
*)
