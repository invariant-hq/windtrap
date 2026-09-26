(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Per-test output capture into log files.

    {!with_capture} redirects file descriptors 1 and 2 into the log file of one
    attempt of a test. {!val-output} reads the file back incrementally, and
    {!output_tail} reads the bounded end of it that a failure carries. {!create}
    is the state of a run that captures, and {!disabled} that of a run under
    [--stream], which captures nothing.

    The redirection is a [dup2] of the descriptors, so the writes of C stubs and
    of the subprocesses that inherit them are captured too. Both descriptors
    write through one open file, so a log holds standard output and standard
    error as one text, in the order in which the bytes reached the descriptors.
    A buffer that holds bytes back changes that order.

    The log of a test is [<log_dir>/<suite>/<groups...>/<test>.output], with
    every component passed through {!Os.sanitize_component}. The path depends on
    the identity of the test alone, so a run overwrites the logs of the run
    before it. Each attempt truncates the log, so it holds the whole output of
    the last attempt.

    The module keeps no state outside a {!t}, but descriptors 1 and 2 and the
    buffers that a drain flushes belong to the process. *)

(** {1:state Capture state} *)

type t
(** The type for the capture state of one run. A value either captures into the
    log files under a directory or is {!disabled}. A capturing value is mutable
    and not thread-safe. *)

val create : log_dir:string -> suite:string -> unit -> t
(** [create ~log_dir ~suite ()] is a state that captures under
    [log_dir/<suite>], with [suite] made one path component by
    {!Os.sanitize_component}. [log_dir] is the log directory of the run, taken
    as given ({!Os.default_log_dir} is the default of a run). Nothing is created
    or written before {!with_capture} runs an attempt. *)

val disabled : t
(** [disabled] is the state of a run under [--stream]. {!with_capture} then runs
    its function on the real descriptors and writes no file, {!output_tail} is
    [None], {!abandon} does nothing and {!val-output} raises. *)

(** {1:capturing Capturing} *)

val drain : unit -> unit
(** [drain ()] forces buffered output through to descriptors 1 and 2. It flushes
    the two standard formatters of [Format], the [stdout] and [stderr] channels
    and the C stdio streams of the same names. It reaches no other buffer, such
    as a formatter of the user's or the buffers of a child process.

    Raises [Sys_error] if a flush fails, as on a closed descriptor. *)

val with_capture :
  t -> groups:string list -> test_name:string -> (unit -> 'a) -> 'a
(** [with_capture t ~groups ~test_name fn] is [fn ()], run as one attempt of the
    test [test_name] under the groups [groups].

    When [t] captures, it creates the directories of the log, truncates the log
    and resets the cursor of {!val-output}, so a retry starts from an empty
    file. Descriptors 1 and 2 write to the log while [fn] runs, and they are the
    real ones again when [with_capture] returns or raises. It drains at the
    start, so that what was buffered before the attempt stays out of its log,
    and at the end, so that what the attempt left in a buffer reaches it.

    The log and the saved descriptors are close-on-exec, so a subprocess writes
    to the log through descriptors 1 and 2 and inherits neither. The log stays
    the current log of [t] until the next call. Calls on one state must not be
    nested. Nothing checks it, and after a nested call descriptors 1 and 2 never
    return to the real ones.

    Raises [Unix.Unix_error] if the directories or the log cannot be created or
    a descriptor cannot be duplicated, and [Sys_error] if the first drain fails.
    [fn] has not run, the descriptors are as they were, and [t] has no current
    log, never that of the attempt before.

    Raises the [Sys_error] of the last drain, after the descriptors are
    restored, if [fn] returned and the drain fails. What [fn] raised wins over a
    failed last drain. Raises [Unix.Unix_error] if the descriptors cannot be
    restored. *)

val abandon : t -> unit
(** [abandon t] ends the redirection of an attempt that is still running, from
    outside {!with_capture}, for a run that stops inside a test and never
    returns to it. It drains what the test buffered into the log, ignoring a
    [Sys_error] of the drain, and descriptors 1 and 2 are then the real ones
    again. The log stays the current log of [t].

    When nothing is redirected it restores nothing.

    Raises [Unix.Unix_error] if a descriptor cannot be restored. *)

(** {1:reading Reading captured output} *)

val output : ?__POS__:Loc.pos -> t -> string
(** [output t] is the bytes that the current attempt wrote since the previous
    call, or since it started. It drains first, and moves its cursor past what
    it returns.

    It is [""] when everything was read already and when [t] has no current log.

    Raises [Failure.Check_failure] when no bytes can be returned, with a
    {!Failure.Message} located at [Loc.resolve ?__POS__ ()]:
    - when [t] is {!disabled}, the message reads
      [this test requires capture; rerun without --stream];
    - when the log can no longer be opened, the message reads
      [this test's captured output cannot be read: <reason>], where [<reason>]
      is the [Sys_error] of the open, which names the log. The cursor stays.

    [__POS__] is read in these cases only. Raises [Sys_error] as {!drain} does.
*)

val output_tail : t -> Failure.tail option
(** [output_tail t] is the end of what the last attempt wrote after the cursor
    of {!val-output}, as the {!type:Failure.tail} that a failure carries. The
    bytes that {!val-output} returned are not in the tail, so a tail is [""]
    when the attempt wrote nothing after its last {!val-output}. It reads the
    log as {!with_capture} left it and drains nothing, so a call inside an
    attempt misses what a buffer still holds.

    The tail keeps at most the last {!Failure.tail_bytes} bytes after the cursor
    and reads no more than that. [omitted_bytes] counts the bytes between the
    cursor and them, and [log_path] is the log, which holds every byte. A cut
    inside a UTF-8 sequence moves past it, the bytes skipped counted as omitted,
    and invalid UTF-8 is kept as it is.

    It is [None] when [t] has no current log, and when the log can no longer be
    opened. *)
