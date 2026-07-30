(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Per-test output capture: fd-level redirection into per-test log files.

    A {!t} value holds one run's capture state: the log-file layout, the run's
    identity inside the log directory, and the byte cursor that {!output}
    advances. The runner owns the value and threads it explicitly; this module
    keeps no global state (RFC Law 9).

    Capture is fd-level: {!with_capture} redirects file descriptors 1 and 2 with
    [dup2], so writes that bypass OCaml channels — C stubs, subprocesses
    inheriting the descriptors — are captured too. fd-level capture sees only
    bytes that reach the descriptors, so every drain (at capture entry and exit,
    and in {!output} / {!output_tail}) flushes C stdio ([fflush] of the C
    [stdout]/[stderr] streams, via this module's stub) alongside the [Format]
    and channel buffers: output printed from a C stub without an explicit flush
    is attributed to the consumption point that follows it, as ppx_expect's
    collector attributes it. Capture is per {e attempt}: every {!with_capture}
    call truncates the test's log file and resets the consumption cursor, so a
    retried test starts from an empty file and its report shows the final
    attempt's output.

    Log files live at [<log_dir>/<suite>/<run-id>/<groups...>/<test>.output],
    every component made filesystem-safe with {!Path_ops.sanitize_component};
    {!link_latest} points [latest] symlinks at the newest run.

    Under [--stream] capture is {!disabled}: tests run against the real
    descriptors, and {!output} — the one operation whose meaning requires
    captured bytes — fails the calling test with a typed failure instead of
    comparing against silence.

    Reports are bounded, files are not: the capture file holds the complete
    output, and {!output_tail} reads back only a bounded suffix as
    {!type:Failure.tail} data (RFC Law 5: a failing test's captured output
    appears in its report, bounded, with the full-log path). {!type:limits} is
    also the seam for the deferred file-level prefix + tail bounding (RFC
    "Capture, expect, and the PPX"). *)

(** {1:limits Retention limits} *)

type limits
(** The type for report-side retention limits. In windtrap 3.0 the limits bound
    what failure reports carry ({!output_tail}); the capture file itself is
    never bounded. *)

val limits : tail_bytes:int -> limits
(** [limits ~tail_bytes] retains at most [tail_bytes] of a test's final output
    bytes in its failure report.

    Raises [Invalid_argument] if [tail_bytes < 0]. *)

val default_limits : limits
(** [default_limits] is [limits ~tail_bytes:8_192], matching the bound
    {!Failure.tail} applies to report payloads. *)

val tail_bytes : limits -> int
(** [tail_bytes limits] is the retained-suffix quota in bytes. *)

(** {1:state Capture state} *)

type t
(** The type for one run's capture state. Values are either enabled — capture
    into per-test files under a log directory — or {!disabled} ([--stream]).
    Enabled values are mutable (the current test's file and the consumption
    cursor) and not thread-safe; the runner is sequential (RFC "The runner"). *)

val create : ?limits:limits -> log_dir:string -> suite:string -> unit -> t
(** [create ~log_dir ~suite ()] is enabled capture state writing under
    [log_dir/<suite>/<run-id>] where:

    - [log_dir] is the log root, usually {!Path_ops.default_log_dir}.
    - [suite] is the suite name, sanitized into one path component.
    - [<run-id>] is a fresh 8-character base-36 identifier drawn from
      operating-system entropy ({!Seed.random}), distinguishing concurrent and
      successive runs of the same suite.
    - [limits] bounds {!output_tail}. Defaults to {!default_limits}.

    Nothing is written until {!with_capture} runs a test. *)

val disabled : t
(** [disabled] is the capture state for [--stream] runs: {!with_capture} runs
    bodies with the real descriptors, {!run_dir} and {!output_tail} report no
    data, {!link_latest} does nothing, and {!output} raises (see below). *)

val run_dir : t -> string option
(** [run_dir t] is the run's log directory [<log_dir>/<suite>/<run-id>], or
    [None] when [t] is {!disabled}. The directory exists once a test has been
    captured. *)

(** {1:capturing Capturing} *)

val with_capture :
  t -> groups:string list -> test_name:string -> (unit -> 'a) -> 'a
(** [with_capture t ~groups ~test_name fn] runs one attempt of the test named
    [test_name] under group path [groups] and is [fn ()]. When [t] is
    {!disabled} it is exactly [fn ()]. Otherwise it:

    - resolves the test's log file to [<run_dir>/<groups...>/<test_name>.output]
      (each component sanitized), creating directories as needed and truncating
      the file — each call is one attempt, so a retry starts from an empty file;
    - resets the {!output} cursor to the start of the file;
    - flushes the [Format] std/err formatters, the [stdout]/[stderr] channels,
      and C stdio, so output buffered before the attempt is not attributed to
      it, then redirects descriptors 1 and 2 into the file with [dup2]. The
      saved originals are close-on-exec: a subprocess spawned by [fn] inherits
      the redirected descriptors (its output is captured), never the real ones —
      a child that outlives the run cannot hold the runner's stdout or stderr
      open, so a piped reader sees end-of-file when the runner exits;
    - on every exit of [fn] — return or raise — flushes the formatters,
      channels, and C stdio again ({e before} restoring, so buffered output
      reaches the file), restores both descriptors, and closes the file. The
      descriptors are restored even when that flush fails; the flush error still
      propagates.

    The file remains [t]'s current log — readable with {!output} and
    {!output_tail} — until the next [with_capture] call.

    Calls must not be nested; the runner runs one attempt at a time. Raises
    [Unix.Unix_error] if the log file cannot be created; the runner's per-test
    boundary turns that into a test failure. A failed setup leaves the
    descriptors untouched and no attempt readable: {!output} is [""] and
    {!output_tail} is [None] until the next call — never the previous attempt's
    output. *)

(** {1:reading Reading captured output} *)

val output : ?pos:Loc.pos -> t -> string
(** [output t] consumes captured output incrementally: it flushes the
    formatters, channels, and C stdio, returns the bytes the current attempt
    captured since the previous [output] call (or since the attempt started),
    and advances the cursor past them. It is [""] when everything so far has
    been consumed, and also when no test has been captured yet.

    When [t] is {!disabled}, raises {!Failure.Check_failure} with the message
    ["this test requires capture; rerun without --stream"], failing the calling
    test at the call site (RFC "Capture, expect, and the PPX") — under
    [--stream] there are no captured bytes to return, and v1's silent [""] made
    expect tests pass vacuously. The failure's location is
    [Loc.resolve ?pos ()]. *)

val output_tail : t -> Failure.tail option
(** [output_tail t] is the bounded suffix of the current attempt's {e entire}
    captured output — from the start of the file, regardless of the {!output}
    cursor — as failure-report data, or [None] when [t] is {!disabled} or no
    test has been captured. The runner attaches it to failures with
    {!Failure.with_output_tail}.

    The tail retains at most the state's {!tail_bytes} final bytes, read without
    loading the rest of the file; bytes before the retained suffix are counted
    in [omitted_bytes], and [log_path] is the capture file holding the complete
    output. A cut that lands inside a UTF-8 sequence is moved past it (the
    skipped bytes count as omitted; a best effort — invalid UTF-8 is kept
    verbatim). The {!Failure.tail} constructor further bounds the text by its
    own constant, so a larger [tail_bytes] cannot bloat reports. *)

(** {1:links Latest links} *)

val link_latest : t -> unit
(** [link_latest t] points the [latest] symlinks at this run:
    [<log_dir>/<suite>/latest] to the run's directory and [<log_dir>/latest] to
    [<suite>/<run-id>], replacing existing links. The runner calls it once per
    run so [.../latest/...] always names the newest logs.

    Best effort: does nothing when [t] is {!disabled} or on Windows (symlinks
    need elevated privileges there), and filesystem errors leave the links
    unchanged rather than raise. *)
