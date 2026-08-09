(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Source locations for failures, test declarations, and snapshot scoping.

    A location names a point in user source. It comes from exactly two places,
    in this order of authority: an explicit [?pos:pos] argument ([__POS__] at
    the call site), or a best-effort walk of the current call stack
    ({!capture}). [?pos] always wins; when neither yields a location there is
    none — a report without a location beats a report with a wrong one.
    {!resolve} packages that rule for failure sites.

    Capture is a provisional heuristic: it takes the first call-stack slot whose
    compilation unit is neither windtrap's nor the standard library's, inlined
    slots included, and returns [None] rather than guess. The walk never crosses
    a {!delimit} frame: the runner runs every user callback under one, so a
    failing call whose own frame was consumed by tail calls yields [None] —
    never the line that called the runner. *)

(** {1:types Types} *)

type pos = string * int * int * int
(** The type of [__POS__]: file, line, start column, end column. *)

type t = { file : string; line : int; column : int }
(** The type for source locations. [file] is as recorded at compile time
    (usually relative to the project root, e.g. ["test/test_users.ml"]); [line]
    is 1-based; [column] is 0-based. *)

(** {1:constructors Constructors} *)

val of_pos : pos -> t
(** [of_pos p] is the location of [p], keeping its start column. *)

val capture : unit -> t option
(** [capture ()] walks the current call stack (bounded, via
    {!Printexc.get_callstack} — immune to user-level re-raise) and returns the
    location of the first slot, inlined slots included, whose compilation unit
    is neither windtrap's nor the standard library's. The walk stops with [None]
    at the nearest {!delimit} frame: reaching it means every frame since the
    failing call was windtrap machinery, so any user frame beyond it is the
    runner's caller, not the failure site. [None] also when no eligible slot has
    a location or the program lacks debug information. Cheap enough to call at
    every failure construction. *)

val delimit : (unit -> 'a) -> 'a
(** [delimit fn] is [fn ()], run under a capture delimiter: a {!capture} during
    [fn] never walks past this call's frame. The runner wraps every
    user-callback invocation in [delimit], so an assertion in tail position —
    whose caller's frame is gone at raise time — reports no location rather than
    the runner's caller; recording then falls back to the test's declaration
    location (see [Run.add_failure]). The frame is recognized by the function's
    debug name and pinned: never inlined, and the call to [fn] is not a tail
    call. Raises whatever [fn] raises, backtrace preserved. *)

val resolve : ?pos:pos -> unit -> t option
(** [resolve ?pos ()] is [Some (of_pos p)] when [pos] is [Some p], and
    [capture ()] otherwise — the one location rule for every failure site. *)

val own_unit : string -> bool
(** [own_unit defname] is [true] iff the compilation unit of [defname] — a
    {!Printexc.Slot} debug name such as ["Windtrap__Check.raises"] — is one of
    windtrap's own: the [Windtrap] alias unit, a [Windtrap__]-wrapped module, or
    the coverage runtime. Whole unit names are matched, so a user library named
    [Windtrap_helpers] is not windtrap's. Backtrace rendering uses it to drop
    the runner's own trailing frames; {!capture} uses the wider notion that also
    covers the standard library. *)

(** {1:observers Observers} *)

val to_string : t -> string
(** [to_string loc] is [loc] spelled ["file:line"] — the one form reports print
    a location in. The column is deliberately absent: it is identity data (see
    {!equal}), not something an editor jump needs. *)

val equal : t -> t -> bool
(** [equal a b] is structural equality, column included — two checks on one line
    are two distinct sites (snapshot duplicate detection turns on this). *)
