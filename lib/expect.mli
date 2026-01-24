(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Expect-style output testing.

    Compares captured stdout/stderr against expected strings. Used internally by
    the test runner via {!Log_trap}; the public API is re-exported through
    {!Windtrap}.

    All comparison functions raise {!Failure.Check_failure} on mismatch. *)

(** {1 Internal} *)

val set_trap : Log_trap.t -> unit
(** [set_trap trap] sets the active log trap for output capture. Called by the
    runner before each test. *)

val clear_trap : unit -> unit
(** [clear_trap ()] removes the active log trap. Called by the runner after each
    test. *)

(** {1 Normalization} *)

val normalize : string -> string
(** [normalize s] normalizes output for comparison. Strips trailing whitespace
    per line, removes leading/trailing blank lines, and dedents by the minimum
    common indentation of non-empty lines. *)

val output : unit -> string
(** [output ()] returns and consumes all stdout/stderr captured since the last
    {!expect}, {!expect_exact}, or {!output} call. Use for post-processing
    non-deterministic output before comparison.

    Returns [""] if no log trap is active. *)

val expect : string -> unit
(** [expect expected] compares captured stdout/stderr (normalized) against
    [expected]. Both [expected] and the captured output are normalized before
    comparison. Consumes the captured output.

    @raise Failure.Check_failure if the normalized strings differ. *)

val expect_exact : string -> unit
(** [expect_exact expected] compares captured stdout/stderr byte-for-byte
    against [expected] with no normalization. Consumes the captured output.

    @raise Failure.Check_failure if the strings differ. *)

val capture : (unit -> 'a) -> string -> 'a
(** [capture fn expected] runs [fn] with stdout/stderr redirected to a temporary
    file, then compares the captured output (normalized) against [expected].
    Returns the result of [fn].

    Unlike {!expect}, this uses independent fd-level redirection rather than the
    active log trap, so it works outside of the test runner.

    @raise Failure.Check_failure if the normalized outputs differ. *)

val capture_exact : (unit -> 'a) -> string -> 'a
(** [capture_exact fn expected] is like {!capture} but compares byte-for-byte
    with no normalization.

    @raise Failure.Check_failure if the strings differ. *)
