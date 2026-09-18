(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Per-test output capture: fd-level redirection into per-test log files.

    A {!t} holds one run's capture state; the runner owns it and threads it
    explicitly. {!with_capture} redirects file descriptors 1 and 2 with [dup2],
    so C stubs and subprocesses inheriting the descriptors are captured too;
    every drain (at capture entry and exit, and in {!output} and {!output_tail})
    flushes the [Format] and channel buffers and C stdio. Capture is per
    attempt: each {!with_capture} call truncates the test's log file and resets
    the {!output} cursor, so a retried test starts from an empty file and its
    report shows the final attempt's output. Log files live at
    [<log_dir>/<suite>/<groups...>/<test>.output], every component made
    filesystem-safe with {!Os.sanitize_component}, so a rerun overwrites the
    previous run's logs. Under [--stream] capture is {!disabled}. The file holds
    the complete output; {!output_tail} reads back a bounded suffix. *)

(** {1:state Capture state} *)

type t
(** The type for one run's capture state: enabled, into per-test files under a
    log directory, or {!disabled}. Enabled values are mutable and not
    thread-safe. *)

val create : log_dir:string -> suite:string -> unit -> t
(** [create ~log_dir ~suite ()] is enabled capture state writing under
    [log_dir/<suite>], [suite] sanitized into one path component. Nothing is
    written until {!with_capture} runs a test. *)

val disabled : t
(** [disabled] is the capture state for [--stream] runs: {!with_capture} runs
    bodies with the real descriptors, {!output_tail} is [None], and {!output}
    raises. *)

(** {1:capturing Capturing} *)

val drain : unit -> unit
(** [drain ()] forces buffered output through to descriptors 1 and 2: [Format]'s
    two standard formatters, the [stdout] and [stderr] channels, then C stdio.
    Every capture edge does it; a [--stream] run does it at each boundary
    between a test's bytes and the report's, so that a row never precedes bytes
    its test wrote. *)

val with_capture :
  t -> groups:string list -> test_name:string -> (unit -> 'a) -> 'a
(** [with_capture t ~groups ~test_name fn] runs one attempt of the test
    [test_name] under group path [groups] and is [fn ()]; exactly [fn ()] when
    [t] is {!disabled}. Otherwise the attempt's log file (directories created)
    is truncated on entry with the {!output} cursor reset, descriptors 1 and 2
    are redirected into it for the duration of [fn] and restored on every exit,
    return or raise, both edges drained; restoration happens even when the
    closing drain fails, and that error still propagates. The saved originals
    and the log descriptor are close-on-exec: a subprocess writes through the
    redirected 1 and 2 and inherits neither. The file remains [t]'s current log
    until the next call. Calls must not be nested.

    Raises [Unix.Unix_error] if the log file cannot be created, leaving the
    descriptors untouched and no attempt readable: {!output} is [""] and
    {!output_tail} is [None] until the next call; the runner's per-test boundary
    turns that into a test failure. *)

val abandon : t -> unit
(** [abandon t] ends the redirection of an attempt in flight, from outside
    {!with_capture}: what the test buffered is drained into its log, then
    descriptors 1 and 2 are the real ones again. For a run that stops inside a
    test and will not return to it (a signal); a no-op when nothing is
    redirected or [t] is {!disabled}. Raises [Unix.Unix_error] if the
    descriptors cannot be restored. *)

(** {1:reading Reading captured output} *)

val output : ?__POS__:Loc.pos -> t -> string
(** [output t] drains the buffers and is the bytes the current attempt captured
    since the previous [output] call (or since the attempt started), advancing
    the cursor past them; [""] when everything has been consumed or no test has
    been captured yet. When [t] is {!disabled}, raises {!Failure.Check_failure}
    with the message ["this test requires capture; rerun without --stream"],
    located at [Loc.resolve ?__POS__ ()]. *)

val output_tail : t -> Failure.tail option
(** [output_tail t] is the bounded suffix of the current attempt's entire
    captured output, from the start of the file regardless of the {!output}
    cursor, as {!type:Failure.tail} data; [None] when [t] is {!disabled} or no
    test has been captured. At most {!Failure.tail_bytes} final bytes are
    retained, read without loading the rest of the file; bytes before them are
    counted in [omitted_bytes], and [log_path] is the capture file. A cut inside
    a UTF-8 sequence is moved past it, the skipped bytes counted as omitted;
    invalid UTF-8 is kept verbatim. *)
