(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The body-side client surface: what code running {e inside a test} may use.

    The inline (ppx) runtime's expect machinery executes inside test bodies, and
    this module names the whole of what it reaches there: the ambient operations
    below, each dispatching through {!Run}'s one documented slot, plus the
    shared vocabulary modules ([Failure], [Loc]) whose values cross this
    boundary. Ambient-reading wrappers on the facade's own pattern — the slot is
    read here, the semantics live in [Run] and [Capture] — and nothing else: no
    behavior of its own, no state.

    This interface {e is} the specification of the body-side client surface (Law
    12), as {!Windtrap_driver} is of the drive side. Widening it is a design
    act, recorded here.

    {b The census} (the expect machinery's body-side reach, item by item):

    - {!add_failure} — an expect node's mismatch, an unreached node, and
      unmatched trailing output are recorded into the executing test's frame as
      ordinary failures.
    - {!captured_output} — every [[%expect]] node and [[%expect.output]] read
      consumes the capture cursor.
    - {!current_path} — the correction-coverage table is keyed by the executing
      test's path.
    - {!failure_count} — the runtime reads the frame's failure count before and
      after an expect body to tell the body's own failures from the expect
      resolutions recorded afterward: a body that already failed must not be
      reported as covered by its corrections (Law 11, masked assertion
      failures). Widened beyond the original sketch for exactly this read.

    Not here, deliberately: [subtest] — the original sketch listed it, but
    sub-cases are a public-API affair ([Windtrap.subtest], which generated
    bodies call like any user code); the expect machinery never reaches it.

    Each operation raises the assertions-outside-run error ([Invalid_argument],
    {!Run.current_frame}) when no test is running.

    Private-stable: this surface moves with co-versioned clients only. Whether
    it someday gets a public spelling is deliberately undecided; until then it
    ships under [Windtrap.Private] like the modules it cuts. *)

val add_failure : Failure.t -> unit
(** [add_failure failure] appends [failure] to the executing attempt's failure
    list ({!Run.add_failure} on the current frame): the test fails at the end
    with every recorded entry, and recording is not raising — the body carries
    on. *)

val current_path : unit -> string list
(** [current_path ()] is the executing test's full path, groups first
    ({!Run.path} of the current frame). Stable across attempts of the same test.
*)

val failure_count : unit -> int
(** [failure_count ()] is the number of failures recorded so far against the
    executing attempt ({!Run.failures} of the current frame). Two reads bracket
    a suspect region; the difference says whether it recorded any. *)

val captured_output : ?pos:Loc.pos -> unit -> string
(** [captured_output ()] consumes and returns the bytes the current attempt
    captured since the previous consumption ({!Capture.output} through the run's
    capture state). Under [--stream] there is nothing to consume: raises
    {!Failure.Check_failure} located at [pos], failing the calling test. *)
