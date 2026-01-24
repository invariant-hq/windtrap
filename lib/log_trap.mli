(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Log capture for test output (internal).

    Redirects stdout and stderr at the file-descriptor level during test
    execution, writing captured output to files organized by test suite, run ID,
    and test hierarchy. Used by the runner and {!Expect} module. *)

type t
(** A mutable log trap handle. Tracks the current capture file and consumed byte
    offset. *)

val create :
  root:string -> suite_name:string -> run_id:string -> enabled:bool -> t
(** [create ~root ~suite_name ~run_id ~enabled] creates a log trap.

    - [root]: base directory for all test output (e.g., ["_build/_tests"])
    - [suite_name]: test suite name, used as a subdirectory
    - [run_id]: unique identifier for this run
    - [enabled]: when [false], all capture operations become no-ops *)

val log_dir : t -> string
(** [log_dir t] returns the full path to the current run's log directory:
    [root/suite_name/run_id]. *)

val test_output_path : t -> groups:string list -> test_name:string -> string
(** [test_output_path t ~groups ~test_name] returns the file path for a test's
    captured output. Each element of [groups] becomes a subdirectory under
    {!log_dir}. Group and test names are sanitized for filesystem safety. *)

val with_capture :
  t -> groups:string list -> test_name:string -> (unit -> 'a) -> 'a
(** [with_capture t ~groups ~test_name fn] runs [fn] with stdout and stderr
    redirected to a log file via [dup2]. The original file descriptors are
    restored after [fn] returns or raises. Flushes all OCaml formatters before
    restoring.

    When capture is disabled, runs [fn] directly with no redirection. *)

val setup_symlinks : t -> unit
(** [setup_symlinks t] creates ["latest"] symlinks pointing to the current run
    directory at both the suite and root levels. No-op on Windows or if capture
    is disabled. *)

val print_location : t -> unit
(** [print_location t] prints the log directory path to stdout. No-op if capture
    is disabled. *)

(** {1 Expect Support}

    These functions support incremental output comparison by {!Expect}. The trap
    maintains a byte offset into the current capture file; output before the
    offset is considered "consumed" and ignored by subsequent reads. *)

val reset_for_test : t -> unit
(** [reset_for_test t] resets the capture file path and consumed offset for a
    new test. *)

val get_unconsumed : t -> string
(** [get_unconsumed t] flushes all OCaml formatters, then returns stdout/stderr
    captured since the last {!consume} call (or since test start). Returns [""]
    if no capture file is active or if all output has been consumed. *)

val consume : t -> unit
(** [consume t] advances the consumed offset to the end of the current capture
    file. Subsequent calls to {!get_unconsumed} return only output produced
    after this point. *)
