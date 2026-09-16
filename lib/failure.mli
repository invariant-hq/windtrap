(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Failure as data: typed failure records, per-test outcomes, and the control
    exceptions.

    Every failure site constructs one {!t}: a typed {!kind}, a {!phase}, an
    optional {!Loc.t} with its {!attribution}, and an optional captured-output
    {!tail}. A test's {!outcome} carries a failure {e list}: a body failure and
    a teardown failure are two entries. Failures hold no styling or command text
    (guarantee 4); they hold pp-rendered values, each bounded at construction
    with a marker stating the original size (64 KiB). Assertion verbs raise
    {!Check_failure}; {!Skip_test}, {!Timeout} and {!Exit_attempt} are the other
    control exceptions the runner understands. *)

(** {1:types Types} *)

(** The type for execution phases; constructors default to {!Body} and the
    runner reclassifies with {!with_phase}. *)
type phase =
  | Body  (** The test body. *)
  | Setup
      (** A [bracket]'s setup function, or a [scoped] test's scope before it
          reached the body. *)
  | Teardown
      (** A [bracket]'s teardown function, or a [scoped] test's scope after the
          body returned. *)
  | Release  (** A fixture release at end of run. *)

type tail = {
  text : string;
      (** The verbatim final bytes of the test's captured output, bounded by
          {!tail_bytes}; never starts inside a UTF-8 sequence. *)
  omitted_bytes : int;
      (** Bytes of captured output preceding [text] that are not retained; [0]
          means [text] is the complete output. *)
  log_path : string option;
      (** The per-test log file holding the full output, when capture wrote one.
      *)
}
(** The type for bounded captured-output tails (guarantee 5). *)

(** The type for what a baseline check compared against. *)
type baseline =
  | Literal
      (** The literal at the failure's location ([expect], [expect_exact]). *)
  | File of string
      (** The file at this path, relative to the project root ([expect_file]),
          stored as the call named it. *)

(** The type for baseline failure states; see {!Baseline.check}. *)
type baseline_state =
  | Missing of { proposed : string }
      (** No baseline exists; [proposed] is the content the check would accept.
      *)
  | Mismatch of { expected : string; actual : string }
      (** The baseline [expected] differs from the produced [actual], both in
          their comparison form. *)
  | Unresolvable of { candidate : string }
      (** The baseline's path cannot be proven to lie under the project root;
          [candidate] is the unproven path. *)

type message_diff = {
  constructor : string;  (** The exception constructor both sides share. *)
  expected_message : string;  (** The expected exception's message payload. *)
  actual_message : string;  (** The raised exception's message payload. *)
}
(** The type for a right-constructor, wrong-message exception failure: both
    exceptions carry the same constructor and a message payload
    ([Invalid_argument], [Failure], [Sys_error]) and the two messages differ.
    Recorded only when all three hold. *)

(** The type for what a containment assertion demanded of the needle beyond
    occurrence. *)
type containment_demand =
  | Anywhere
      (** One occurrence, anywhere: [contains], [not_contains] and the affix
          verbs, whose position demand lives in their claim. *)
  | Ordered of { index : int; resumed_at : int }
      (** [in_order]: the needle is the chain element at zero-based [index] and
          its search began at byte [resumed_at], the end of the previous
          element's match. [found_at] keeps its plain meaning, the needle's
          first occurrence anywhere. *)

(** The type for typed failure payloads; renderers pattern match on it. *)
type kind =
  | Equality of {
      expected : string;
      actual : string;
      not_ : bool;
      diffable : bool;
    }
      (** Two sides that should have matched did not: [equal], [not_equal], the
          boolean, unwrapping and predicate verbs. [expected] and [actual] are
          the rendered values or constructor descriptions (["Some _"]), expected
          first. [not_] is [true] for a negated assertion, both strings then
          rendering the same value. [diffable] is [false] when [expected] is a
          claim sentence ({!predicate}) rather than a rendering: renderers
          refine neither side against the other. *)
  | Containment of {
      claim : string;
          (** A one-line description of what was asserted
              ([string containing "eof"]), never diffed. *)
      needle : string;  (** The needle, verbatim, bounded. *)
      found_at : int option;
          (** The byte offset of the needle's first occurrence in the haystack:
              [None] for a failed [contains], [Some _] for a failed
              [not_contains]. *)
      haystack_length : int;  (** The haystack's total byte length. *)
      excerpt : string;
          (** A bounded window of the haystack centred on the {!Ordered} cursor,
              else on [found_at] when [Some _], else the haystack's head; see
              {!containment}. Renderers show it whole. *)
      excerpt_offset : int;
          (** The byte offset of [excerpt] within the haystack. *)
      demand : containment_demand;  (** What was demanded beyond occurrence. *)
    }
      (** A containment assertion ([contains], [not_contains], the affix verbs,
          [in_order]) failed. *)
  | Raise of {
      expected : string option;
      actual : string option;
      predicate : bool;
      backtrace : string option;
      message_diff : message_diff option;
    }
      (** An exception assertion failed. [expected] is the rendered expected
          exception (or a predicate description), [None] when only {e some}
          exception was demanded; [actual] the rendered raised exception, [None]
          when nothing was raised; [backtrace] the raised exception's backtrace
          when recorded; [message_diff] is [Some _] exactly for a
          right-constructor, wrong-message failure. [predicate] is [true] iff
          the assertion was [raises_match]; [false] with no [expected] records
          an uncaught exception. *)
  | Baseline of { baseline : baseline; state : baseline_state }
      (** A baseline check failed: what was compared against and how the
          comparison ended. The acceptance command is spelled by renderers from
          the invocation. *)
  | Property of {
      rendered : string;
      case_index : int;
      shrink_steps : int;
      shrink_exhausted : bool;
          (** [true] iff the shrink search stopped (budget spent, or a candidate
              raised) rather than converging on a minimum. *)
      timed_out : float option;
      root : Seed.seed;
      count : int option;
      examples : bool;
      rendering : rendering;
          (** What [rendered] is; renderers mark a pre-image as such and name
              [Gen.with_pp] under a placeholder. *)
      inner : t option;
    }
      (** A property failed. [rendered] is the printed (shrunk) counterexample,
          [case_index] the zero-based failing case, [shrink_steps] how many
          shrinks led to it, [inner] the {!Check_failure} the body raised at it.
          [examples] is [true] when the case came from the explicit examples
          list, never seeded or shrunk. [timed_out] is [Some limit] when the
          timeout expired during the shrink search, never alongside [examples].
          [root] and [count] are the replay line's ingredients: the root seed,
          and the case count when run configuration supplied it. *)
  | Message of string  (** A direct failure ([fail], [failf], and kin). *)

(** The type for what a {!Property} failure's [rendered] text is. *)
and rendering =
  | Value  (** The counterexample, through its generator's printer. *)
  | Pre_image
      (** What a printerless [map] or [bind] computed the counterexample from,
          printed by the generators that drew it. *)

(** The type for how a failure's [loc] was obtained. *)
and attribution =
  | Recorded
      (** [loc] is what the failure site recorded (an explicit [?__POS__], a
          captured frame, or the site a runner-made failure names for itself: a
          timeout, an uncaught exception or an [xfail] that passed names the
          enclosing test's declaration, a restoration that could not happen the
          call that made the change), or the site recorded none and nothing
          filled it. *)
  | Declaration
      (** The failure site recorded no location, the failing call having sat in
          tail position, and the runner filled [loc] with the enclosing test's
          declaration site. Renderers print a hint naming [~__POS__] under such
          a location, except for a {!Property} failure, an uncaught-exception
          {!Raise} and a {!File} baseline. *)

and t = {
  kind : kind;
  phase : phase;
  loc : Loc.t option;  (** [None] renders without a location header. *)
  attribution : attribution;
      (** How [loc] was obtained; {!Recorded} from every constructor. *)
  msg : string option;  (** The user's [?msg] annotation, when given. *)
  subtest : string list;
      (** The sub-case label's components (the test's leaf name, then the
          enclosing subtest names outermost first) when the failure was recorded
          inside {!Run.subtest}; [[]] otherwise. Classification reads this
          field, never [msg]. *)
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
    while a run is active: the raise from the [at_exit] handler cancels the
    exit, so the attempt surfaces at the nearest failure boundary. Carries no
    payload, an [at_exit] handler cannot observe the exit code. Registered with
    a [Printexc] printer. *)

(** {1:boundaries Boundary rules} *)

val is_fatal : exn -> bool
(** [is_fatal exn] is [true] iff [exn] is [Sys.Break], [Out_of_memory] or
    [Stack_overflow]: the exceptions no failure boundary may swallow. *)

val backtrace_to_string : Printexc.raw_backtrace -> string
(** [backtrace_to_string raw] is [raw] rendered for a report: the one conversion
    every transport uses. It is {!Printexc.raw_backtrace_to_string} minus the
    trailing run of windtrap's own frames ({!Loc.own_unit}); only a trailing run
    is dropped, and a backtrace that never crossed user code is kept whole.
    Frames keep their original positions. *)

val recorded_backtrace : unit -> string option
(** [recorded_backtrace ()] is {!backtrace_to_string} of the most recently
    raised exception's backtrace, or [None] when recording is off or the
    backtrace is empty. Read it before anything else can raise. *)

(** {1:constructors Constructors}

    Constructors default [phase] to {!Body}, bound every payload string, and
    capture no location: pass [?loc:(Loc.resolve ?__POS__ ())] at failure sites
    and omit [loc] where it would be a guess. *)

val equality :
  ?loc:Loc.t ->
  ?msg:string ->
  ?not_:bool ->
  expected:string ->
  actual:string ->
  unit ->
  t
(** [equality ~expected ~actual ()] is a diffable {!Equality} failure over the
    two rendered values; [not_] defaults to [false]. *)

val containment :
  ?loc:Loc.t ->
  ?msg:string ->
  ?found_at:int ->
  ?demand:containment_demand ->
  claim:string ->
  needle:string ->
  haystack:string ->
  unit ->
  t
(** [containment ~claim ~needle ~haystack ()] is a {!Containment} failure
    storing [claim], [needle] and [demand] (default {!Anywhere}) as given, and a
    bounded excerpt of [haystack]: a window around an {!Ordered} demand's
    cursor, else around [found_at] when given, else the head of [haystack]. An
    anchored window is bounded by {!tail_bytes}; a head window by the first 10
    lines or 1 KiB, whichever comes first; both are cut on UTF-8 code-point
    boundaries, so an anchored window may exceed its bound by up to three bytes.
    The bound is applied here, once.

    Raises [Invalid_argument] if [found_at], or an {!Ordered} demand's
    [resumed_at], is negative or past the end of [haystack]. *)

val predicate : ?loc:Loc.t -> ?msg:string -> claim:string -> string -> t
(** [predicate ~claim value] is an {!Equality} failure with [diffable] unset:
    [claim] describes what the assertion demanded and takes the expected side,
    [value] is the rendered value that failed it. *)

val raised :
  ?loc:Loc.t ->
  ?msg:string ->
  ?expected:string ->
  ?actual:string ->
  ?predicate:bool ->
  ?backtrace:string ->
  ?message_diff:message_diff ->
  unit ->
  t
(** [raised ()] is a {!Raise} failure. All payload fields default to absent
    ([predicate] to [false]); see {!kind}. Pass [message_diff] only when it
    holds. *)

val baseline : ?loc:Loc.t -> baseline -> baseline_state -> t
(** [baseline b state] is a {!Baseline} failure of the baseline [b] in [state].
*)

val property :
  ?loc:Loc.t ->
  ?inner:t ->
  ?timed_out:float ->
  ?count:int ->
  rendered:string ->
  case_index:int ->
  shrink_steps:int ->
  ?shrink_exhausted:bool ->
  root:Seed.seed ->
  examples:bool ->
  ?rendering:rendering ->
  unit ->
  t
(** [property ~rendered ~case_index ~shrink_steps ~root ~examples ()] is a
    {!Property} failure; see {!kind}. [timed_out] and [count] default to [None],
    [rendering] to {!Value}. *)

val message : ?loc:Loc.t -> string -> t
(** [message text] is a {!Message} failure carrying [text]. *)

(** {1:updating Updating} *)

val with_phase : phase -> t -> t
(** [with_phase phase f] is [f] with its phase replaced. *)

val with_output_tail : tail -> t -> t
(** [with_output_tail tail f] is [f] carrying [tail] as its captured-output
    tail. *)

val tail : ?log_path:string -> ?omitted_bytes:int -> string -> tail
(** [tail text] is a {!tail} retaining the final {!tail_bytes} bytes of [text],
    cut so the retained suffix never starts inside a UTF-8 sequence. Bytes cut
    here are added to [omitted_bytes] (default [0]), the bytes the capture layer
    already dropped.

    Raises [Invalid_argument] if [omitted_bytes < 0]. *)

val tail_bytes : int
(** [tail_bytes] is the number of final bytes {!tail} retains (8 KiB). Readers
    of captured output size their reads by it. *)

(** {1:outcomes Per-test outcomes} *)

(** The type for per-test results. A failed test carries one entry per phase
    that failed; [Fail []] never occurs. Timing and attempt counts live in the
    run record. *)
type outcome =
  | Pass
  | Fail of t list  (** Non-empty, in the order the failures occurred. *)
  | Skip of string option  (** Skipped, with the reason from {!Skip_test}. *)
