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

Failures are data; renderers are projections. Nothing here holds ANSI
    styling or command text — renderers derive those. What it does hold is
    pp-rendered {e values}, the one thing that cannot outlive the failure site,
    each bounded at construction with a truncation marker stating the original
    size (currently 64 KiB).

    Construct failures with {!equality}, {!containment}, {!predicate},
    {!raised}, {!snapshot}, {!property}, and {!message}; the runner reclassifies
    with {!with_phase} and attaches captured output with {!with_output_tail}.
    Assertion verbs raise {!Check_failure}; {!Skip_test}, {!Timeout}, and
    {!Exit_attempt} are the other control exceptions the runner understands. *)

(** {1:types Types} *)

(** The type for execution phases. Identifies which part of a test a failure
    interrupted; the runner assigns phases when it classifies outcomes
    (constructors default to {!Body}). *)
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

type message_diff = {
  constructor : string;
      (** The exception constructor both sides share, named where the exceptions
          themselves were in hand — never recovered from a rendering. *)
  expected_message : string;  (** The expected exception's message payload. *)
  actual_message : string;  (** The raised exception's message payload. *)
}
(** The type for a right-constructor, wrong-message exception failure: both
    exceptions carry the same constructor, both carry a message payload (the
    stdlib's string-carrying exceptions: [Invalid_argument], [Failure],
    [Sys_error]), and the two messages differ. It is recorded only when all
    three hold, because that conjunction is the only question a renderer asks of
    it; a renderer therefore branches on the option and nothing else. *)

(** The type for what a containment assertion demanded of the needle's
    occurrences, beyond the "occurs / does not occur" that [found_at] already
    records. The containment verbs share one payload; this field is how a
    renderer tells them apart. *)
type containment_demand =
  | Anywhere
      (** One occurrence, anywhere: [contains], [not_contains], and the affix
          verbs (whose position demand lives in their claim). *)
  | Ordered of { index : int; resumed_at : int }
      (** [in_order]: the needle is the chain element at zero-based [index],
          and the search for it began at byte [resumed_at] — the end of the
          previous element's match. [found_at] keeps its plain meaning, the
          needle's first occurrence {e anywhere}, so a renderer distinguishes
          "not in the string at all" from "in the string, but before the
          cursor" — the out-of-order bug — exactly as it does for
          [starts_with]. *)

(** The type for typed failure payloads. Never a stringly key-value bag: each
    assertion family has its own case, and renderers pattern match on it. *)
type kind =
  | Equality of {
      expected : string;
      actual : string;
      not_ : bool;
      diffable : bool;
    }
      (** Two sides that should have matched did not: [equal], [not_equal], the
          boolean verbs, the unwrapping verbs, and the predicate verbs
          ([satisfies], [require_match]). [expected] and [actual] are the
          pp-rendered values or constructor descriptions (["Some _"],
          ["Error <abstract>"]), expected first (v1's order).

          [not_] is [true] for a negated assertion ([not_equal]): both strings
          then render the same value and renderers print it once.

          [diffable] is [false] when [expected] is a {e description} rather
          than a rendering — {!predicate}'s claim sentence
          (["value satisfying the predicate"], ["a match"]). Renderers word
          such a failure exactly as they word an equality, and refine neither
          side against the other: there is nothing for a character diff of a
          sentence against a value to point at. *)
  | Containment of {
      claim : string;
          (** A one-line description of what was asserted
              ([string containing "eof"]) — a description, never diffed against
              the haystack. *)
      needle : string;
          (** The needle, verbatim (bounded like every payload string). *)
      found_at : int option;
          (** The byte offset of the needle's first occurrence in the haystack:
              [None] for a failed [contains] (the needle does not occur),
              [Some _] for a failed [not_contains] (it does). This field records
              which of the two verbs failed. *)
      haystack_length : int;  (** The haystack's total byte length. *)
      excerpt : string;
          (** A bounded window of the haystack, centred on the offset the
              failure is about: the {!Ordered} cursor when there is one — the
              remaining region is what that search was reading — else
              [found_at] when it is [Some _]. With neither, the window is the
              haystack's head, bounded to what a reader scans past to reach
              the verdict. Renderers show what is stored, whole; see
              {!containment} for the bounds. *)
      excerpt_offset : int;
          (** The byte offset of [excerpt] within the haystack; renderers derive
              the omitted byte counts on either side from it together with
              [haystack_length]. *)
      demand : containment_demand;
          (** What the assertion demanded beyond mere occurrence; {!Anywhere}
              for the verbs that demand nothing more. *)
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
          exception (or a predicate description), [None] when the assertion only
          demanded {e some} exception; [actual] is the rendered raised
          exception, [None] when nothing was raised; [backtrace] is the raised
          exception's backtrace when one was recorded.

          [message_diff] is [Some _] exactly when the failure is a
          right-constructor, wrong-message one and a renderer can therefore diff
          the messages instead of repeating the constructor in both renderings;
          see {!type:message_diff}. It is [None] on every other path, so a
          renderer decides on the option alone.

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
      shrink_exhausted : bool;
          (** [true] iff the shrink search {e stopped} rather than converging:
              it spent its step budget, or forcing a candidate raised and left
              the siblings behind it unreachable. The two outcomes are
              otherwise indistinguishable in a report — both read "shrunk N
              steps" — and they mean different things: a converged search
              reports the minimal counterexample, a stopped one reports the
              best it reached. *)
      timed_out : float option;
      root : Seed.seed;
      count : int option;
      max_shrink : int option;
      examples : bool;
      printerless : bool;
          (** [true] iff the generator carries no printer, so [rendered] is a
              placeholder — [<no printer>], or [<example k>] — rather than the
              value. Renderers name the remedy ([Gen.with_pp]) exactly once,
              under the counterexample, instead of each placeholder shape
              carrying its own advice. *)
      inner : t option;
    }
      (** A property failed. [rendered] is the printed (shrunk)
          counterexample, [case_index] the zero-based failing case,
          [shrink_steps] how many shrinks led to it, and [inner] the assertion
          failure the body raised at that counterexample when it raised a
          {!Check_failure}. [examples] is [true] when the case came from the
          explicit examples list, which is never seeded or shrunk.

          [timed_out] is [Some limit] when the per-test timeout expired {e
          during} the shrink search, and [None] on every other path — a timeout
          before any case failed times out the whole test — so it is never set
          alongside [examples].

          [root], [count] and [max_shrink] are the replay line's three
          ingredients: the run's root seed, and the two run-configuration knobs
          when configuration supplied them ([None] when the declaration site or
          the engine default did). A renderer restates exactly the ones that
          are present: a replay under a different case count or shrink budget
          reaches a different case, or stops the descent at a different node,
          and reports something else. *)
  | Message of string  (** A direct failure ([fail], [failf], and kin). *)
  | Stale_baselines of string list
      (** Baselines still stale at the end of a full, clean run, failing it
          under [--strict-snapshots]: the offending baseline paths, non-empty,
          sorted as {!Snapshot.orphans} reports them and stored unmodified —
          renderers derive the display spelling and the removal-hint command
          from them. Runner-side ({!Run.Stale_baselines} rows), never raised by
          an assertion verb. *)

and t = {
  kind : kind;
  phase : phase;
  loc : Loc.t option;  (** [None] renders without a location header. *)
  msg : string option;  (** The user's [?msg] annotation, when given. *)
  subtest : string list;
      (** The sub-case label's components — the test's leaf name, then the
          enclosing subtest names outermost first — when the failure was
          recorded inside {!Run.subtest}; [[]] for plain failures. Renderers
          derive the displayed [leaf › name] label from it; classification
          reads the field, never the [msg] text, so a user annotation can
          never dress a plain failure as a sub-case. *)
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

val backtrace_to_string : Printexc.raw_backtrace -> string
(** [backtrace_to_string raw] is [raw] rendered for a report — the one
    conversion, so that nothing reaches a payload through
    {!Printexc.raw_backtrace_to_string} directly and the terminal, JUnit and
    GitHub reports show the same frames.

    It is {!Printexc.raw_backtrace_to_string} minus the trailing run of
    windtrap's own frames ({!Loc.own_unit}): the delimiter, the attempt guard
    and the raising verb sit under every backtrace windtrap records, name none
    of the reader's code, and on a short one outnumber it. Only a trailing run —
    a user callback windtrap invoked keeps both itself and the frames below it —
    and a backtrace that never crossed user code is kept whole rather than
    emptied. Frames keep their original positions, so the first line still reads
    ["Raised at"]. *)

val recorded_backtrace : unit -> string option
(** [recorded_backtrace ()] is {!backtrace_to_string} of the most recently
    raised exception's backtrace when the runtime recorded one, and [None] when
    backtrace recording is off or the recorded backtrace is empty. Read it
    before anything else can raise. *)

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
  expected:string ->
  actual:string ->
  unit ->
  t
(** [equality ~expected ~actual ()] is a diffable {!Equality} failure over the
    two rendered values; [not_] defaults to [false]. A containment failure is
    not an equality: build it with {!containment}, which owns the excerpt
    policy. A claim against a value is {!predicate}. *)

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
    storing [claim], [needle] and [demand] as given ([demand] defaults to
    {!Anywhere}) and a bounded excerpt of [haystack]: a window around an
    {!Ordered} demand's cursor when there is one, else around [found_at] when
    given (the failed-[not_contains] case), else the head of [haystack] (the
    failed-[contains] case).

    An anchored window is bounded by an implementation constant (currently
    8 KiB, the captured-output tail bound) — its surroundings are the evidence
    for the offset the verdict names. A head window has no offset to be
    evidence for, so it is bounded to a readable head instead (currently the
    first 10 lines or 1 KiB, whichever comes first). Both are cut on UTF-8
    code-point boundaries, and an anchored window may therefore exceed its
    bound by the up to three bytes that complete a sequence. The bound is
    applied once, here: renderers show the stored excerpt whole. The failure
    records the excerpt's offset and the haystack's total length so they can
    state what was omitted.

    Raises [Invalid_argument] if [found_at], or an {!Ordered} demand's
    [resumed_at], is negative or past the end of [haystack]. *)

val predicate : ?loc:Loc.t -> ?msg:string -> claim:string -> string -> t
(** [predicate ~claim value] is an {!Equality} failure with [diffable] unset:
    [claim] describes in one line what the assertion demanded and takes the
    expected side, [value] is the rendered value that failed it. The two
    predicate verbs share it deliberately — the difference between them {e is}
    the claim sentence, and that is already in the payload. *)

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
    ([predicate] to [false]); see {!kind} for what each one means. Pass
    [message_diff] only when it holds — the failure site owns that decision,
    having the exceptions themselves. *)

val snapshot : ?loc:Loc.t -> name:string -> path:string -> snapshot_state -> t
(** [snapshot ~name ~path state] is a {!Snapshot} failure for the snapshot
    [name] whose baseline is [path]. *)

val property :
  ?loc:Loc.t ->
  ?inner:t ->
  ?timed_out:float ->
  ?count:int ->
  ?max_shrink:int ->
  rendered:string ->
  case_index:int ->
  shrink_steps:int ->
  ?shrink_exhausted:bool ->
  root:Seed.seed ->
  examples:bool ->
  ?printerless:bool ->
  unit ->
  t
(** [property ~rendered ~case_index ~shrink_steps ~root ~examples ()] is a
    {!Property} failure; see {!kind} for the payload semantics. [timed_out],
    [count], and [max_shrink] default to [None], and [printerless] to [false] —
    the caller states that [rendered] is a placeholder, since only it knows. *)

val message : ?loc:Loc.t -> string -> t
(** [message text] is a {!Message} failure carrying [text]. *)

val stale_baselines : string list -> t
(** [stale_baselines paths] is a {!Stale_baselines} failure over the stale
    baseline paths [paths], stored unmodified.

    Raises [Invalid_argument] if [paths] is empty: an empty verdict is no
    verdict, and the runner records none. *)

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
(** [tail text] is a bounded {!tail} retaining the final {!tail_bytes} bytes of
    [text], cut so the retained suffix never starts inside a UTF-8 sequence.
    Bytes cut here are added to [omitted_bytes], which records bytes the capture
    layer already dropped before calling (defaults to [0]).

    Raises [Invalid_argument] if [omitted_bytes < 0]. *)

val tail_bytes : int
(** [tail_bytes] is the number of final bytes {!tail} retains (currently 8 KiB).
    Readers of captured output size their reads by it: a reader supplying fewer
    bytes silently under-fills a report, and one supplying more has the excess
    discarded here. *)

(** {1:outcomes Per-test outcomes} *)

(** The type for per-test results. A failed test carries a failure {e list}: the
    runner appends one entry per phase that failed (body and teardown failures
    are two entries; [Fail []] never occurs). Run bookkeeping — timing, attempt
    counts — lives in the run record, not here. *)
type outcome =
  | Pass
  | Fail of t list  (** Non-empty, in the order the failures occurred. *)
  | Skip of string option  (** Skipped, with the reason from {!Skip_test}. *)
