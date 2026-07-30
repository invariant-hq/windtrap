(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Failure as data: typed failure records, per-test outcomes, and the control
    exceptions.

    Every failure site constructs one {!t}: a typed {!kind} payload, a {!phase},
    an optional {!Loc.t}, and an optional bounded captured-output {!tail}. A
    test's result is an {!outcome} carrying a failure {e list} — a body failure
    and a teardown failure are two entries, never merged.

    Failures are data; renderers are projections. Nothing here contains ANSI
    styling, acceptance or replay command text, or any other rendered transcript
    fragment: renderers derive all of those from the typed payloads. The strings
    stored here are pp-rendered {e values} — the one thing that cannot outlive
    the failure site — and every payload string is bounded at construction with
    an explicit truncation marker stating the original size (the bound is an
    implementation constant, currently 64 KiB).

    Construct failures with {!equality}, {!containment}, {!raised}, {!snapshot},
    {!property}, and {!message}; the runner reclassifies with {!with_phase} and
    attaches captured output with {!with_output_tail}. Assertion verbs raise
    {!Check_failure}; {!Skip_test}, {!Timeout}, and {!Exit_attempt} are the
    other control exceptions the runner understands. *)

(** {1:types Types} *)

(** The type for execution phases. Identifies which part of a test a failure
    interrupted; the runner assigns phases when it classifies outcomes
    (constructors default to {!Body}). *)
type phase =
  | Body  (** The test body. *)
  | Setup  (** A [bracket]'s setup function. *)
  | Teardown  (** A [bracket]'s teardown function. *)
  | Release  (** A fixture release at end of run. *)

type tail = {
  text : string;
      (** The verbatim final bytes of the test's captured output. Bounded by an
          implementation constant (currently 8 KiB); never starts inside a UTF-8
          sequence. *)
  omitted_bytes : int;
      (** Bytes of captured output preceding [text] that are not retained. [0]
          means [text] is the complete output. This field, not a marker inside
          [text], is the explicit truncation record; renderers show it together
          with [log_path]. *)
  log_path : string option;
      (** The per-test log file holding the full output, when capture wrote one.
      *)
}
(** The type for bounded captured-output tails: a failing test's captured output
    appears in its failure report, bounded, with the full-log path. *)

(** The type for snapshot failure states, as recorded by snapshot checking. See
    {!Snapshot.check} for how each state arises. *)
type snapshot_state =
  | Missing of { proposed : string }
      (** No baseline exists; [proposed] is the content the check would accept.
      *)
  | Mismatch of { expected : string; actual : string }
      (** The baseline [expected] differs from the produced [actual]. Renderers
          compute the diff from these payloads. *)
  | Unresolvable
      (** No source file could be resolved to scope the snapshot name. *)
  | Duplicate of { first : Loc.t option; first_test : string }
      (** The name was already registered by an earlier check this run: [first]
          is that check's site when one is known, [first_test] the test that
          made it. The failure's own [loc] is the second check's site. *)

(** The type for the claim an {!Equality} failure asserted. Every claim projects
    through [expected]/[actual] — a renderer that knows no claim beyond {!Equal}
    still renders truthfully from those two strings; the other cases carry the
    machine-readable payload that lets a claim-aware renderer word and excerpt
    the failure precisely. *)
type claim =
  | Equal
      (** A plain (in)equality: [equal], [not_equal], the boolean verbs, and the
          unwrapping verbs. [expected] and [actual] are pp-rendered values or
          constructor descriptions (["Some _"], ["Error <abstract>"]). *)
  | Contains of {
      needle : string;
          (** The needle, verbatim (bounded like every payload string). *)
      found_at : int option;
          (** The byte offset of the needle's first occurrence in the haystack:
              [None] for a failed [contains] (the needle does not occur),
              [Some _] for a failed [not_contains] (it does). This field, not
              [not_], records which of the two verbs failed. *)
      haystack_length : int;  (** The haystack's total byte length. *)
      excerpt_offset : int;
          (** The byte offset of the stored excerpt ([actual]) within the
              haystack; renderers derive the omitted byte counts on either side
              from it together with [haystack_length]. *)
    }
      (** A containment assertion ([contains]/[not_contains]) failed. [actual]
          is a bounded excerpt of the haystack — around the first occurrence
          when [found_at] is [Some _], the head otherwise — and [expected] is a
          one-line description of the claim. *)
  | Satisfies
      (** A [satisfies] assertion failed: [actual] is the rendered value the
          predicate rejected; [expected] is a one-line description. *)
  | Matches
      (** A [require_match] assertion failed: [actual] is the rendered scrutinee
          (or ["<abstract>"] without a printer); [expected] is a one-line
          description. *)

(** The type for typed failure payloads. Never a stringly key-value bag: each
    assertion family has its own case, and renderers pattern match on it. *)
type kind =
  | Equality of {
      expected : string;
      actual : string;
      not_ : bool;
      claim : claim;
    }
      (** A comparison assertion failed. [expected] and [actual] are the
          pp-rendered values, expected first (v1's order). [not_] is [true] for
          a negated assertion ([not_equal]): both strings then render the same
          value and renderers print it once ([not_] is never [true] for a claim
          other than {!Equal}). [claim] refines the payload with the assertion
          family; see {!type:claim}. *)
  | Raise of {
      expected : string option;
      actual : string option;
      predicate : bool;
      backtrace : string option;
      same_constructor : bool;
      expected_message : string option;
      actual_message : string option;
    }
      (** An exception assertion failed. [expected] is the rendered expected
          exception (or a predicate description), [None] when the assertion only
          demanded {e some} exception; [actual] is the rendered raised
          exception, [None] when nothing was raised; [backtrace] is the raised
          exception's backtrace when one was recorded.

          The remaining fields let renderers diff exception {e messages}:
          [same_constructor] is [true] iff both exceptions were present and
          raised with the same exception constructor (payloads aside);
          [expected_message]/[actual_message] are the corresponding exception's
          message payload when it carries one (the stdlib's string-carrying
          exceptions: [Invalid_argument], [Failure], [Sys_error]). When
          [same_constructor] is [true] and both messages are present, a renderer
          can diff the messages instead of repeating the constructor in both
          renderings.

          [predicate] is [true] iff the assertion was [raises_match]: an
          exception was raised and a user predicate rejected it. [false] with no
          [expected] side records an exception nobody expected — the
          uncaught-exception case — and renderers word the two differently. This
          field, not the absent [expected], records which failure it was. *)
  | Snapshot of { name : string; path : string; state : snapshot_state }
      (** A snapshot check failed. [name] is the snapshot name, [path] the
          resolved baseline path; both are stored unmodified — renderers derive
          acceptance commands from them. *)
  | Property of {
      rendered : string;
      case_index : int;
      shrink_steps : int;
      timed_out : float option;
      root : Seed.seed;
      count : int option;
      examples : bool;
      inner : t option;
    }
      (** A property failed. [rendered] is the printed (shrunk) counterexample;
          [case_index] the zero-based failing case; [shrink_steps] how many
          shrinks led to it; [timed_out] is [Some limit] when the per-test
          timeout expired during the shrink search — the counterexample is the
          best found within the budget and may not be minimal. It is [None] on
          every other path: a timeout before any case has failed times out the
          whole test instead, so [timed_out] is never set when [examples] is
          [true]. [root] is the run's root seed — renderers derive the replay
          line from it; [count] is the effective case count when run
          configuration ([--prop-count] / [WINDTRAP_PROP_COUNT]) supplied it,
          and [None] when the declaration site fixed the count or the engine
          default applied — renderers restate it in the replay line exactly when
          present, because replaying a late case needs at least as many cases as
          the failing run generated; [examples] is [true] when the case came
          from the explicit examples list (such cases are never seeded or
          shrunk); [inner] is the assertion failure raised by the property body
          at the shrunk counterexample, when it was a {!Check_failure}. *)
  | Message of string  (** A direct failure ([fail], [failf], and kin). *)

and t = {
  kind : kind;
  phase : phase;
  loc : Loc.t option;  (** [None] renders without a location header. *)
  msg : string option;  (** The user's [?msg] annotation, when given. *)
  output_tail : tail option;
      (** Attached by the runner after the test completes; [None] until
          {!with_output_tail}. *)
}
(** The type for structured test failures. *)

(** {1:exceptions Control exceptions} *)

exception Check_failure of t
(** Raised by every assertion verb on failure. *)

exception Skip_test of string option
(** Raised to skip the current test; the payload is the reason. *)

exception Timeout of float
(** Raised when a test exceeds its timeout; the payload is the limit in seconds.
*)

exception Exit_attempt
(** Raised by the runner's exit guard when code under test calls [Stdlib.exit]
    while a run is active: the raise cancels the exit ([exit] runs [at_exit]
    handlers before terminating, and an exception from one propagates to
    [exit]'s caller), so the attempt surfaces at the nearest failure boundary
    instead of killing the process. Carries no payload — an [at_exit] handler
    cannot observe the requested exit code. Registered with a [Printexc] printer
    so every stringification site renders it identically. *)

(** {1:boundaries Boundary rules}

    The two exception rules every failure boundary shares: which raised
    exceptions must propagate untouched, and how a raised exception's backtrace
    is captured for the ones that are recorded. *)

val is_fatal : exn -> bool
(** [is_fatal exn] is [true] iff [exn] is one of the exceptions no failure
    boundary may swallow — [Sys.Break], [Out_of_memory], [Stack_overflow]. Catch
    sites re-raise these instead of recording a failure: an interrupt or a
    resource exhaustion must stop the run, not fail one test. *)

val recorded_backtrace : unit -> string option
(** [recorded_backtrace ()] is the backtrace of the most recently raised
    exception when the runtime recorded one, and [None] when backtrace recording
    is off or the recorded backtrace is empty. Read it before anything else can
    raise. *)

(** {1:constructors Constructors}

    Constructors default [phase] to {!Body} and bound every payload string (see
    the module preamble); snapshot names and paths are stored unmodified because
    renderers derive acceptance commands from them.

    None of them captures a location: pass [?loc:(Loc.resolve ?pos ())] at
    failure sites — {!Loc.resolve} is the one location rule — and omit [loc]
    where a location would be a guess. *)

val equality :
  ?loc:Loc.t ->
  ?msg:string ->
  ?not_:bool ->
  ?claim:claim ->
  expected:string ->
  actual:string ->
  unit ->
  t
(** [equality ~expected ~actual ()] is an {!Equality} failure over the two
    rendered values. [not_] defaults to [false] and [claim] to {!Equal}.
    Containment claims are built with {!containment}, which owns the excerpt
    policy — never with this constructor.

    Raises [Invalid_argument] if [not_] is [true] with a claim other than
    {!Equal}: negation and claim refinement never combine (see {!kind}). *)

val containment :
  ?loc:Loc.t ->
  ?msg:string ->
  ?found_at:int ->
  expected:string ->
  needle:string ->
  haystack:string ->
  unit ->
  t
(** [containment ~expected ~needle ~haystack ()] is an {!Equality} failure whose
    claim is {!Contains}: [expected] is the claim's one-line description and the
    stored [actual] is a bounded excerpt of [haystack] — a window around
    [found_at] when given (the failed-[not_contains] case), the head of
    [haystack] otherwise (the failed-[contains] case). The excerpt is cut on
    UTF-8 code-point boundaries and bounded by an implementation constant
    (currently 8 KiB, the captured-output tail bound); the claim records the
    excerpt's offset and the haystack's total length so renderers can state what
    was omitted.

    Raises [Invalid_argument] if [found_at] is negative or past the end of
    [haystack]. *)

val raised :
  ?loc:Loc.t ->
  ?msg:string ->
  ?expected:string ->
  ?actual:string ->
  ?predicate:bool ->
  ?backtrace:string ->
  ?same_constructor:bool ->
  ?expected_message:string ->
  ?actual_message:string ->
  unit ->
  t
(** [raised ()] is a {!Raise} failure. All payload fields default to absent
    ([same_constructor] and [predicate] to [false]); see {!kind} for what each
    one means. *)

val snapshot : ?loc:Loc.t -> name:string -> path:string -> snapshot_state -> t
(** [snapshot ~name ~path state] is a {!Snapshot} failure for the snapshot
    [name] whose baseline is [path]. *)

val property :
  ?loc:Loc.t ->
  ?inner:t ->
  ?timed_out:float ->
  ?count:int ->
  rendered:string ->
  case_index:int ->
  shrink_steps:int ->
  root:Seed.seed ->
  examples:bool ->
  unit ->
  t
(** [property ~rendered ~case_index ~shrink_steps ~root ~examples ()] is a
    {!Property} failure; see {!kind} for the payload semantics. [timed_out] and
    [count] default to [None]. *)

val message : ?loc:Loc.t -> string -> t
(** [message text] is a {!Message} failure carrying [text]. *)

(** {1:updating Updating}

    Runner-side: failures are constructed where they happen, then classified and
    completed at the per-test boundary. *)

val with_phase : phase -> t -> t
(** [with_phase phase f] is [f] with its phase replaced — e.g. a failure caught
    while running a teardown becomes a {!Teardown}-phase entry. *)

val with_output_tail : tail -> t -> t
(** [with_output_tail tail f] is [f] carrying [tail] as its captured-output
    tail. *)

val tail : ?log_path:string -> ?omitted_bytes:int -> string -> tail
(** [tail text] is a bounded {!tail} retaining the final bytes of [text]: at
    most the implementation bound (currently 8 KiB), cut so the retained suffix
    never starts inside a UTF-8 sequence. Bytes cut here are added to
    [omitted_bytes], which records bytes the capture layer already dropped
    before calling (defaults to [0]).

    Raises [Invalid_argument] if [omitted_bytes < 0]. *)

(** {1:outcomes Per-test outcomes} *)

(** The type for per-test results. A failed test carries a failure {e list}: the
    runner appends one entry per phase that failed (body and teardown failures
    are two entries; [Fail []] never occurs). Run bookkeeping — timing, attempt
    counts — lives in the run record, not here. *)
type outcome =
  | Pass
  | Fail of t list  (** Non-empty, in the order the failures occurred. *)
  | Skip of string option  (** Skipped, with the reason from {!Skip_test}. *)
