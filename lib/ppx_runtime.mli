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
    [-partition]. Unrecognized flags are silently ignored.

    @raise Failure if called more than once. *)

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

(** {1 Test Execution} *)

type location = { file : string; line : int; start_col : int; end_col : int }
(** Source location for an expect node. *)

val run_test :
  file:string -> line:int -> name:string option -> fn:(unit -> unit) -> unit
(** [run_test ~file ~line ~name ~fn] registers an expect test. Despite its name,
    the test is not executed immediately; all registered tests run when {!exit}
    is called. *)

(** {1 Expectation Checking} *)

val expect : loc:location -> expected:string option -> unit
(** [expect ~loc ~expected] checks captured output against [expected] using
    normalized comparison (trailing whitespace and blank lines are trimmed).
    Pass [None] for [expected] to assert that no output was produced. Records a
    correction and raises on mismatch.

    @raise Failure.Check_failure if output does not match. *)

val expect_exact : loc:location -> expected:string option -> unit
(** [expect_exact ~loc ~expected] checks captured output against [expected]
    using exact byte-for-byte comparison. Otherwise behaves like {!expect}.

    @raise Failure.Check_failure if output does not match. *)

val output : unit -> string
(** [output ()] returns and consumes raw captured output since the last
    {!expect}, {!expect_exact}, or [output] call. Returns [""] if no output was
    captured. *)

(** {1 Module-based Test Syntax}

    These functions support the [module%test] and [let%test] syntax for building
    test trees using a stack-based group nesting model. *)

val add_test : string -> (unit -> unit) -> unit
(** [add_test name fn] adds a test case to the current group, or to the
    top-level if no group is open. *)

val enter_group : string -> unit
(** [enter_group name] starts a new test group. *)

val leave_group : unit -> unit
(** [leave_group ()] closes the current test group and adds it to its parent
    group (or to the top level).

    @raise Failure if no group is currently open. *)

val run_tests : string -> unit
(** [run_tests name] runs all registered tests under the suite [name], then
    terminates the process. Exits with code 0 on success, code 1 on failure. If
    [--list] was passed on the command line, prints test paths and exits without
    running.

    @raise Failure if any test groups are still open (missing {!leave_group}).
*)
