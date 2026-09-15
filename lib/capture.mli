(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Per-test output capture: fd-level redirection into per-test log files.

    A {!t} value holds one run's capture state: the log-file layout and the byte
    cursor that {!output} advances. The runner owns the value and threads it
    explicitly; this module keeps no global state.

    Capture is fd-level: {!with_capture} redirects file descriptors 1 and 2 with
    [dup2], so writes that bypass OCaml channels — C stubs, subprocesses
    inheriting the descriptors — are captured too. fd-level capture sees only
    bytes that reach the descriptors, so every drain (at capture entry and exit,
    and in {!output} / {!output_tail}) flushes C stdio ([fflush] of the C
    [stdout]/[stderr] streams, via this module's stub) alongside the [Format]
    and channel buffers: output printed from a C stub without an explicit flush
    is attributed to the consumption point that follows it. Capture is per
    {e attempt}: every {!with_capture} call truncates the test's log file and
    resets the consumption cursor, so a retried test starts from an empty file
    and its report shows the final attempt's output.

    Log files live at [<log_dir>/<suite>/<groups...>/<test>.output], every
    component made filesystem-safe with {!Path_ops.sanitize_component}. The path
    is a function of the test's identity alone, so it is the same on every run
    and a rerun overwrites the previous one's logs.

    Under [--stream] capture is {!disabled}: tests run against the real
    descriptors, and {!output} — the one operation whose meaning requires
    captured bytes — fails the calling test with a typed failure instead of
    comparing against silence.

    Reports are bounded, files are not: the capture file holds the complete
    output, and {!output_tail} reads back only {!Failure.tail_bytes} final bytes
    as {!type:Failure.tail} data — a failing test's captured output appears in
    its report, bounded, with the full-log path. *)

(** {1:state Capture state} *)

type t
(** The type for one run's capture state. Values are either enabled — capture
    into per-test files under a log directory — or {!disabled} ([--stream]).
    Enabled values are mutable (the current test's file and the consumption
    cursor) and not thread-safe; the runner is sequential. *)

val create : log_dir:string -> suite:string -> unit -> t
(** [create ~log_dir ~suite ()] is enabled capture state writing under
    [log_dir/<suite>], where [log_dir] is the log root (usually
    {!Path_ops.default_log_dir}) and [suite] is the suite name sanitized into
    one path component. Nothing is written until {!with_capture} runs a test. *)

val disabled : t
(** [disabled] is the capture state for [--stream] runs: {!with_capture} runs
    bodies with the real descriptors, {!output_tail} reports no data, and
    {!output} raises (see below). *)

(** {1:capturing Capturing} *)

val with_capture :
  t -> groups:string list -> test_name:string -> (unit -> 'a) -> 'a
(** [with_capture t ~groups ~test_name fn] runs one attempt of the test named
    [test_name] under group path [groups] and is [fn ()]. When [t] is
    {!disabled} it is exactly [fn ()].

    Otherwise the attempt writes to
    [<log_dir>/<suite>/<groups...>/<test_name>.output] (each component
    sanitized, directories created), truncated on entry with the {!output}
    cursor reset — one call is one attempt, so a retry starts from an empty
    file. Descriptors 1 and 2 are redirected into it for the duration of [fn]
    and restored on every exit, return or raise; both edges drain the [Format]
    formatters, the channels and C stdio, so output buffered before the attempt
    is not attributed to it and output buffered at the end still reaches the
    file. Restoration happens even when the closing drain fails, and that error
    still propagates. The saved originals and the log descriptor are
    close-on-exec: a subprocess [fn] spawns writes through the redirected 1 and
    2 and inherits neither the real descriptors nor the log, so a child that
    outlives the run cannot hold a piped reader open past the summary.

    The file remains [t]'s current log — readable with {!output} and
    {!output_tail} — until the next [with_capture] call.

    Calls must not be nested; the runner runs one attempt at a time. Raises
    [Unix.Unix_error] if the log file cannot be created; the runner's per-test
    boundary turns that into a test failure. A failed setup leaves the
    descriptors untouched and no attempt readable: {!output} is [""] and
    {!output_tail} is [None] until the next call — never the previous attempt's
    output. *)

(** {1:reading Reading captured output} *)

val output : ?__POS__:Loc.pos -> t -> string
(** [output t] consumes captured output incrementally: it flushes the
    formatters, channels, and C stdio, returns the bytes the current attempt
    captured since the previous [output] call (or since the attempt started),
    and advances the cursor past them. It is [""] when everything so far has
    been consumed, and also when no test has been captured yet.

    When [t] is {!disabled}, raises {!Failure.Check_failure} with the message
    ["this test requires capture; rerun without --stream"], failing the calling
    test at the call site — under [--stream] there are no captured bytes to
    return, and v1's silent [""] made expect tests pass vacuously. The failure's
    location is [Loc.resolve ?__POS__ ()]: the facade passes none and lets
    {!Loc.capture} find its caller, while the ppx runtime passes the [[%expect]]
    node's position — its own compilation unit is not one {!Loc.own_unit} knows,
    so a capture would name [ppx_runtime.ml]. *)

val output_tail : t -> Failure.tail option
(** [output_tail t] is the bounded suffix of the current attempt's {e entire}
    captured output — from the start of the file, regardless of the {!output}
    cursor — as failure-report data, or [None] when [t] is {!disabled} or no
    test has been captured. The runner attaches it to failures with
    {!Failure.with_output_tail}.

    The tail retains at most {!Failure.tail_bytes} final bytes — the bound
    reports carry — read without loading the rest of the file; bytes before the
    retained suffix are counted in [omitted_bytes], and [log_path] is the
    capture file holding the complete output. A cut that lands inside a UTF-8
    sequence is moved past it (the skipped bytes count as omitted; a best effort
    — invalid UTF-8 is kept verbatim). *)
