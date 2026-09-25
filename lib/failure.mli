(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Failure as data: the failure record, the outcome of a test and the control
    exception.

    Failures are data and renderers are projections (guarantee 4 of
    [doc/dev/architecture.md]). A failure site builds one {!t} with a
    {{!section-constructors}constructor} and raises it in a {!Check_failure}, or
    records it when the site is the runner's. A renderer reads it and changes
    nothing. Nothing here prints.

    A failure holds no styling and no command. A renderer spells the [replay:]
    and [accept:] commands from the payload and from the invocation of the run.
    What a failure holds is the text that cannot be made again after the failure
    site: printed values, messages and a backtrace, each a bounded {!type-text}.
*)

(** {1:texts Texts} *)

type text = private {
  kept : string;
      (** The whole text or, past 64 KiB, its longest prefix of at most 65 536
          bytes that ends on a code-point boundary. It holds no marker. *)
  length : int;  (** The length of the whole text in bytes. *)
}
(** The type for a text of a payload: a printed value, a message, a backtrace.
    It is cut iff [String.length kept < length]. A consumer must never take a
    cut text for a whole one: two texts are known equal only when neither is cut
    and their [kept] are equal. *)

val text : string -> text
(** [text s] is [s], bounded. It is the only way to make a {!type-text}, so
    every writer of a payload field keeps the bound. *)

val is_cut : text -> bool
(** [is_cut t] is [String.length t.kept < t.length]. *)

(** {1:types Types} *)

(** The type for the part of a test that a failure interrupted. *)
type phase =
  | Body  (** The body of the test. *)
  | Setup  (** The setup of a bracket, or a scope before it called back. *)
  | Teardown
      (** The teardown of a bracket, or a scope after the body left the
          callback, by returning or by raising. It is also the phase of a
          working directory or an environment binding that the runner could not
          restore after the attempt. *)
  | Release  (** The release of a fixture at the end of the run. *)

type tail = {
  text : string;
      (** The last bytes that the test wrote, at most {!tail_bytes} of them. A
          value that {!val-tail} builds from well-formed UTF-8 never starts
          inside a sequence. *)
  omitted_bytes : int;
      (** The bytes of output before [text], which the tail does not keep. This
          field is the record of the cut, and [text] holds no marker. *)
  log_path : string option;
      (** The log file that holds the whole output, when the capture wrote one.
      *)
}
(** The type for the bounded end of a test's captured output. A failing test's
    captured output is in its report, bounded, with the full log's path
    (guarantee 5, see {!Report.result}). *)

(** The type for what a baseline check compared against. *)
type baseline =
  | Literal of { exact : bool }
      (** The literal at the location of the failure, compared byte for byte iff
          [exact]. A renderer takes its source file from [loc]. *)
  | File of string
      (** The file at this path, stored as the expectation spelled it and never
          bounded, since a renderer names the file from it. *)

(** The type for how a baseline check failed (see {!Baseline.check}). *)
type baseline_state =
  | Missing of { proposed : text }
      (** No baseline exists, and [proposed] is the content that the check would
          accept. *)
  | Mismatch of { expected : text; actual : text }
      (** The baseline [expected] differs from the produced [actual], both in
          their comparison form. A renderer computes the diff from them. *)
  | Unresolvable of { candidate : string }
      (** The path cannot be proven to lie under the project root. [candidate]
          is the unproven path, stored whole. *)

(** The type for why a baseline failure offers no correction (see
    {{!Run.section-corrections}corrections}). *)
type withheld =
  | Failed_outside
      (** The attempt also has a failure that is not a baseline failure, whether
          or not it skipped as well. *)
  | Skipped
      (** Every failure of the attempt is a baseline failure, and the attempt
          skipped. *)
  | Refused of { line : int; reason : string }
      (** The source cannot take the correction of the literal at [line] of its
          file. [reason] is one sentence that names no path. *)
  | Conflict
      (** An earlier check of the same baseline in the run recorded a correction
          to a different text. *)

type message_diff = {
  constructor : string;
      (** The constructor that both exceptions share, which the producer names
          from the exceptions and never from a rendering. *)
  expected_message : text;  (** The message of the expected exception. *)
  actual_message : text;  (** The message of the raised exception. *)
}
(** The type for an exception failure with the right constructor and the wrong
    message. A producer must record one only when both exceptions share a
    constructor that carries a message ([Invalid_argument], [Failure],
    [Sys_error]) and their messages differ. {!raised} checks none of it, and a
    renderer decides on the presence of the value alone. *)

(** The type for what a containment assertion demanded of its needle beyond an
    occurrence, which [found_at] records. *)
type containment_demand =
  | Anywhere
      (** An occurrence, for [contains], or none, for [not_contains]. [found_at]
          tells which. *)
  | Prefix  (** An occurrence at byte [0], for [starts_with]. *)
  | Suffix  (** An occurrence that ends the haystack, for [ends_with]. *)
  | Ordered of { index : int; resumed_at : int }
      (** The needle is element [index], from zero, of an [in_order] chain, and
          its search started at byte [resumed_at]. [found_at] is still the first
          occurrence anywhere, so [None] says that the needle is not in the
          string, and [Some _] that it is there before [resumed_at]. *)

(** The type for failure payloads. A renderer matches on it without a wildcard,
    so a new constructor is a design amendment that every renderer answers. *)
type kind =
  | Equality of { expected : text; actual : text; not_ : bool; diffable : bool }
      (** Two sides that had to match did not. [expected] and [actual] are
          printed values, or descriptions of a constructor such as [Some _].
          Under [not_], a negated assertion, both must hold one rendering, and a
          renderer prints [expected] once and never reads [actual]. [diffable]
          is [false] when [expected] is a claim in words ({!predicate}), and a
          renderer then computes no diff and marks neither side against the
          other. *)
  | Containment of {
      needle : text;  (** The needle. *)
      found_at : int option;
          (** The byte offset of the first occurrence of the needle in the
              haystack, if any. Under {!Prefix} and {!Suffix} [Some _] says
              where the needle is instead (see {!Ordered} for [in_order]). *)
      haystack_length : int;  (** The length of the whole haystack in bytes. *)
      excerpt : string;
          (** A window of the haystack, which {!containment} bounds and a
              renderer shows whole. [found_at] can lie outside it, and the
              needle can run past its end. *)
      excerpt_offset : int;
          (** The byte offset of [excerpt] in the haystack. With
              [haystack_length] it gives the bytes left out on each side. *)
      demand : containment_demand;
          (** What was demanded beyond an occurrence. *)
    }  (** A containment assertion failed. *)
  | Raise of {
      expected : text option;
      actual : text option;
      predicate : bool;
      backtrace : text option;
      message_diff : message_diff option;
    }
      (** An exception assertion failed, or an exception was raised that nothing
          expected. [expected] and [actual] are the expected and the raised
          exception, printed. [predicate] is [true] iff the assertion was a
          [raises_match], whether its function returned or raised. [backtrace]
          is that of the raised exception, as {!backtrace_to_string} gives it,
          when one was recorded.

          A renderer tells four shapes apart, in this order.
          + A [message_diff]: the right constructor with the wrong message.
          + An [expected]: a [raises] named this exception, and its function
            raised another one, which is [actual], or returned.
          + An [actual] alone: an exception that a predicate rejected when
            [predicate] is [true], and an uncaught exception otherwise.
          + Neither: an exception that was demanded and never raised. *)
  | Baseline of {
      baseline : baseline;
      state : baseline_state;
      withheld : withheld option;
          (** [Some _] iff this failure offers no correction. *)
    }
      (** A baseline check failed: what it compared against and how it ended. *)
  | Property of {
      rendered : text;
          (** The counterexample as printed, after shrinking, or the text that
              stands for one (see {!Property.outcome}). *)
      summary : text option;
          (** [Some line] iff [rendered] is a table whose first line names the
              columns. [line] then says in one line what the table holds, as
              [2 calls, last: get] does for a stateful program. A renderer must
              print [line] where a one-line counterexample goes, and the table
              under it with its first line as the header row. *)
      case_index : int;
          (** The zero-based index of the failing case: the position of an
              example among the examples, or for a generated case the index that
              {!Seed.derive} took, which counts the discarded cases too. *)
      shrink_steps : int;
          (** The accepted shrink steps that led to [rendered]. *)
      shrink_end : shrink_end;
          (** How the shrink search ended. [rendered] is the last node that it
              accepted. It is {!Converged} beside [examples]. *)
      root : Seed.seed;  (** The root seed of the run. *)
      count : int option;
          (** The case count when the configuration of the run gave it, and
              [None] when the declaration or the default of the engine did. A
              renderer restates it in the [replay:] command only when it is
              present, since a replay under another count reaches another case.
          *)
      examples : bool;
          (** [true] iff the case is one of the explicit examples, which are
              never seeded or shrunk, so such a failure has no [replay:]
              command. *)
      rendering : rendering;
          (** What [rendered] is, which the producer states. *)
      inner : t option;
          (** The failure of the law on the reported counterexample: the payload
              of the {!Check_failure} that the law raised, or for any other
              exception a [Raise] failure with no [expected], the exception as
              printed, its backtrace and no location. A failure that
              {!Property.run} builds always has one. *)
    }  (** A property failed. *)
  | Timeout of { limit : float; case : timed_case option }
      (** The test's limit, in seconds, expired. [case] is the case of a
          property that was running then, when no case had failed before it, and
          [None] for any other timeout. *)
  | Message of text
      (** A direct failure: the text of a [fail], or a failure that the library
          words itself, as it does an intercepted [exit]. *)

and timed_case = {
  case_index : int;
      (** The index of the case, as the [case_index] of a
          {!constructor-Property} failure. *)
  examples : bool;  (** [true] iff the case is one of the explicit examples. *)
  passed : int;  (** The cases that had passed, the examples included. *)
  root : Seed.seed;  (** The root seed of the run. *)
  count : int option;
      (** The case count when the configuration of the run gave it, as the
          [count] of a {!constructor-Property} failure. *)
}
(** The type for the case of a property that a timeout interrupted. *)

(** The type for how a shrink search ended. Every case but {!Converged} says
    that the counterexample may not be minimal. *)
and shrink_end =
  | Converged  (** No candidate of the last node was accepted. *)
  | Budget_spent  (** The search took its budget of accepted steps. *)
  | Candidate_raised of text
      (** Forcing a candidate raised the exception printed here, and the
          siblings behind it were unreachable. *)
  | Timed_out of float
      (** The test's limit, in seconds, expired while shrinking, and the test
          did not time out. *)

(** The type for what the [rendered] of a {!constructor-Property} failure is. *)
and rendering =
  | Value
      (** The text of the counterexample itself: a value through the printer of
          its generator, a failing example, or a placeholder that says why no
          value is printed. *)
  | Pre_image
      (** What a printerless [map] or [bind] computed the counterexample from,
          printed by the generators that drew it (see [Gen.Engine.render]). It
          is the input of the mapping functions and not the value that the law
          received, and a renderer must mark it as such. *)

and t = {
  kind : kind;  (** The payload. *)
  phase : phase;  (** The phase that the failure interrupted. *)
  loc : Loc.t option;
      (** The location of the failure, when it has one: that of the failing
          call, or the declaration site of the test where the runner found none.
          Nothing in the record tells the two apart. The [loc] of a
          {!constructor-Property} failure is the declaration of the property,
          and the site of the assertion is on its [inner]. *)
  msg : text option;
      (** The [?msg] of the assertion, when given. For the failure of a call,
          {!Stateful} writes the label of the call before it. *)
  subtest : string list;
      (** The label of a subtest, for a failure that {!Run.subtest} recorded:
          the name of the test, then the names of the open subtests, outermost
          first. It is [[]] otherwise. A renderer classifies a subtest failure
          by this field and never by [msg], so an annotation of the user cannot
          pass a plain failure off as one. It derives the displayed label from
          this field too. *)
  output_tail : tail option;
      (** The captured output of the attempt. [None] until {!with_output_tail}.
      *)
}
(** The type for failures. The record is concrete, and two clients write fields
    past the constructors. {!Run} writes [loc] and [subtest], and {!Stateful}
    writes the [msg] and the [loc] of the failure of a call. *)

(** {1:control Control} *)

type control =
  [ `Skip of string option  (** Skip the running test, with the reason. *)
  | `Timeout of float
    (** The test's limit, in seconds, expired. The runner raises it from a
        signal handler, so it can surface at any allocation or poll point of the
        code that runs then, the library's own included. *)
  | `Exit
    (** Code under test called [Stdlib.exit] while a run is active, in the
        process that owns the run. The exit guard of {!Run} raises it, which
        cancels the exit, and it travels from the call to [exit] as any other
        exception does (see {{!Run.section-exits}exits}). It carries no code,
        because the guard cannot see it. *)
  | `Discard  (** Discard the running case of a property. *) ]
(** The type for a statement about the running test or case. The runner acts on
    the first three and the property engine on [`Discard]. *)

exception Check_failure of t
(** Raised to fail the running test with a finished failure. The assertion verbs
    raise it, and so do {!Baseline.check}, {!Capture.val-output} when nothing is
    captured, and {!Stateful} for the failure of a call. *)

exception Control of control
(** Raised through the user's code to the site that owns the control. This
    module registers its [Printexc] printer, so every site that prints it gives
    [windtrap skip: REASON] (or [windtrap skip] with no reason),
    [windtrap timeout after 1.5s],
    [Exit_attempt (code under test called exit; intercepted by windtrap)] and
    [windtrap discard (assume or reject outside a property)]. *)

(** {1:catching Catching the user's code}

    Every site that calls the user's code calls it through {!catch}, and keeps
    one rule: it records or converts a {!type-fault} and raises every control
    again, to its owner. Two owners consume more:
    - The runner's attempt consumes every control (see
      {{!Run.section-attempts}attempts}).
    - The property engine consumes [`Discard] everywhere below a law. Once a
      case has failed, it also consumes every control of the shrink search, so
      nothing replaces the failure found: a [`Timeout] ends the search, any
      other control rejects the candidate, and a printer turns what it raises
      into text.

    [Sys.Break] and [Out_of_memory] never reach a site, since an interrupt or an
    exhausted memory must stop the run and not fail one test. A [Stack_overflow]
    is an exception as any other. A handler of the user's that catches every
    exception still swallows a control. *)

type fault = [ `Assertion of t | `Exception of exn * Printexc.raw_backtrace ]
(** The type for what the user's code raised about itself: a {!Check_failure},
    or any other exception with its backtrace. *)

type caught = [ fault | control ]
(** The type for what {!catch} returns of a raise. *)

val catch : (unit -> 'a) -> ('a, caught) result
(** [catch f] is [Ok (f ())], or [Error c] where [c] classifies what [f ()]
    raised. It raises [Sys.Break] and [Out_of_memory] again with their backtrace
    and never returns them. A [Fun.Finally_raised] that carries a {!Control} or
    one of those two is unwrapped first, so a finally cut by the timeout is a
    [`Timeout]. *)

val reraise : [< caught ] -> 'a
(** [reraise c] raises again what {!catch} returned: the same exception, with
    its backtrace for an [`Exception]. *)

val exn_to_string : exn -> string
(** [exn_to_string e] is [Printexc.to_string e] with the prefix [Dune__exe__]
    removed from each name that starts with it, so an exception that an
    executable defines prints under the name its source gives it: [M.E], never
    [Dune__exe__M.E]. A producer must convert an exception with it, and never
    with [Printexc.to_string], so that every report names it the same way. *)

val caught_to_string : [< caught ] -> string
(** [caught_to_string c] is {!exn_to_string} of the exception that [c]
    classifies. *)

(** {1:backtraces Backtraces} *)

val backtrace_to_string : Printexc.raw_backtrace -> string
(** [backtrace_to_string raw] is [raw] as the text of a payload. A producer must
    convert with it, and never with [Printexc.raw_backtrace_to_string], so that
    every report shows the same frames.

    The result is the text of [Printexc.raw_backtrace_to_string] without the
    trailing run of windtrap's own frames ({!Loc.own_unit}). Only a trailing run
    is dropped, so a callback of the user that windtrap called keeps its frame
    and the frames below it, and a backtrace that never crossed code of the user
    is kept whole. The frames keep their positions, so the first line still
    reads [Raised at]. An empty backtrace gives [""], which {!raised} stores as
    no backtrace. *)

(** {1:constructors Constructors}

    A constructor builds a {!Body} failure with no subtest label and no captured
    output. It captures no location, so a failure site passes
    [?loc:(Loc.resolve ?__POS__ ())] and leaves [loc] out where it would be a
    guess.

    {b Bounds.} A constructor makes each text that it takes as a string, [msg]
    included, with {!val-text}. The path of a {!File}, the [candidate] of an
    {!Unresolvable} and the [constructor] of a {!type-message_diff} are strings
    stored whole, and an [excerpt] has the bounds of {!containment}. *)

val equality :
  ?loc:Loc.t ->
  ?msg:string ->
  ?not_:bool ->
  expected:string ->
  actual:string ->
  unit ->
  t
(** [equality ~expected ~actual ()] is a diffable {!Equality} failure over two
    printed values. [not_] defaults to [false]. A claim against a value takes
    {!predicate}, and a containment failure takes {!containment}. *)

val containment :
  ?loc:Loc.t ->
  ?msg:string ->
  ?found_at:int ->
  demand:containment_demand ->
  needle:string ->
  haystack:string ->
  unit ->
  t
(** [containment ~demand ~needle ~haystack ()] is a {!Containment} failure with
    [needle], [found_at] and [demand] as given and a bounded excerpt of
    [haystack].

    The excerpt is cut here, once. Its anchor is the [resumed_at] of an
    {!Ordered} demand, or else [found_at]. With an anchor the excerpt is a
    window of at most {!tail_bytes} bytes around it. Without one it is the head
    of the haystack, its first 10 lines or its first 1 KiB, whichever ends
    first, and under {!Suffix} its end, its last 10 lines or its last 1 KiB,
    whichever starts last. Every cut falls on a code-point boundary, within the
    bound.

    Raises [Invalid_argument] if [found_at], or the [resumed_at] of an
    {!Ordered} demand, is negative or greater than the length of [haystack]. *)

val predicate : ?loc:Loc.t -> ?msg:string -> claim:string -> string -> t
(** [predicate ~claim value] is an {!Equality} failure that is not diffable.
    [claim] says in one line what the assertion demanded and takes the expected
    side, and [value] is the printed value that failed it. *)

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
(** [raised ()] is a {!Raise} failure. Every text defaults to absent and
    [predicate] to [false]. A failure site must pass [message_diff] only when it
    holds (see {!type-message_diff}), and must make [backtrace] with
    {!backtrace_to_string}. Neither is checked. A [backtrace] of [""] is stored
    as none. *)

val baseline : ?loc:Loc.t -> baseline -> baseline_state -> t
(** [baseline b state] is a {!constructor-Baseline} failure of [b] in [state],
    with nothing withheld. *)

val property :
  ?loc:Loc.t ->
  ?inner:t ->
  ?count:int ->
  ?summary:string ->
  rendered:string ->
  case_index:int ->
  shrink_steps:int ->
  ?shrink_end:shrink_end ->
  root:Seed.seed ->
  examples:bool ->
  ?rendering:rendering ->
  unit ->
  t
(** [property ~rendered ~case_index ~shrink_steps ~root ~examples ()] is a
    {!constructor-Property} failure with the fields given. [inner], [count] and
    [summary] default to [None], [shrink_end] to {!Converged} and [rendering] to
    {!Value}. Nothing is validated, so the invariants that {!type-kind} states
    are the producer's to keep. *)

val timeout : ?loc:Loc.t -> ?case:timed_case -> float -> t
(** [timeout ?case limit] is a {!constructor-Timeout} failure for [limit]
    seconds, in [case] when given. *)

val message : ?loc:Loc.t -> string -> t
(** [message text] is a {!Message} failure that carries [text]. *)

(** {1:updating Updating}

    A failure is built where it happens, and the runner classifies and completes
    it at the boundary of the attempt with these three. *)

val with_phase : phase -> t -> t
(** [with_phase phase f] is [f] with [phase] as its phase. *)

val with_output_tail : tail -> t -> t
(** [with_output_tail tail f] is [f] with [tail] as its captured output, in
    place of any that it had. *)

val with_withheld : withheld -> t -> t
(** [with_withheld why f] is [f] with its correction withheld for [why] when [f]
    is a {!constructor-Baseline} failure, in any state, and [f] otherwise. A
    failure already marked {!Refused} or {!Conflict} keeps that mark, because it
    holds whatever the rest of the attempt does. It does not reach the [inner]
    of a {!constructor-Property} failure. *)

(** {1:tails Captured-output tails} *)

val tail : ?log_path:string -> ?omitted_bytes:int -> string -> tail
(** [tail text] is a {!type-tail} that keeps the last {!tail_bytes} bytes of
    [text]. A [text] of at most that many bytes is kept as given. Otherwise the
    cut moves forward to a code-point boundary, by at most three bytes, and the
    bytes cut are added to [omitted_bytes].
    - [omitted_bytes] is the bytes that the caller had dropped before [text].
      Defaults to [0].
    - [log_path] is the log file of the whole output. Defaults to none.

    Raises [Invalid_argument] if [omitted_bytes] is negative. *)

val tail_bytes : int
(** [tail_bytes] is [8_192], 8 KiB: the bytes that a {!type-tail} keeps, and the
    bound of an anchored excerpt of {!containment}. A reader of captured output
    must size its read by it, since {!val-tail} discards what is beyond it and a
    shorter read leaves a report short. *)

(** {1:outcomes Per-test outcomes} *)

(** The type for the outcome of a test, and of the release of a fixture. It does
    not say whether the test counts as failed, which {!Run.result} does. *)
type outcome =
  | Pass
  | Fail of t list
      (** Never empty, in the order in which the failures were recorded. The
          entries are never merged, so a body failure and a teardown failure are
          two entries, and one phase can add several, as each failing subtest
          and each failing expectation does. *)
  | Skip of string option
      (** Skipped, with the first reason that the attempt gave. An attempt that
          skipped and also recorded a failure is a [Fail]. *)
