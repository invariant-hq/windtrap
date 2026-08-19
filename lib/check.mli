(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The assertion verbs.

    Internal. {!Windtrap} re-exports every verb below flat and states the
    contract a test author reads; this interface states what a {e renderer}
    author needs, which is the payload each verb builds. Summaries here, the
    contract there, the prose in [doc/manual/assertions.md].

    Two rules hold for all of them. A failing verb constructs one
    {!Failure.t} — a typed kind, an optional location, the [?msg] annotation
    when given — and raises {!Failure.Check_failure}; verbs never print, never
    diff, and never touch run state. And the location is [?pos] when given,
    else a best-effort call-stack capture, else none ({!Loc.resolve} is the
    rule).

    {!skip} is not a failure: it raises {!Failure.Skip_test}. *)

(** {1:types Types} *)

type pos = Loc.pos
(** The type of [__POS__] payloads: file, line, start column, end column. *)

type 'a printer = Format.formatter -> 'a -> unit
(** The type for value printers, as taken by [?pp], [?pp_error], [?pp_ok]. *)

type 'a testable = 'a Testable.t
(** The type for assertion witnesses; see {!Testable}. *)

(** {1:equalities Equalities}

    Every verb here builds a diffable {!Failure.equality} over two rendered
    values or constructor descriptions, expected first. The witness renders
    only on failure. *)

val equal : ?pos:pos -> ?msg:string -> 'a testable -> 'a -> 'a -> unit
(** [equal t expected actual] is [()] iff [Testable.equal t expected actual]. *)

val not_equal : ?pos:pos -> ?msg:string -> 'a testable -> 'a -> 'a -> unit
(** [not_equal t a b] is [()] iff [a] and [b] are {e not} equal under [t]. The
    payload sets [not_] and stores one rendering — [a]'s — on both sides, which
    renderers print once. *)

val is_true : ?pos:pos -> ?msg:string -> bool -> unit
(** [is_true b] is [()] iff [b]. The payload compares ["true"] against
    ["false"]. *)

val is_false : ?pos:pos -> ?msg:string -> bool -> unit
(** [is_false b] is [()] iff [not b]. *)

val is_none : ?pos:pos -> ?msg:string -> ?pp:'a printer -> 'a option -> unit
(** [is_none o] is [()] iff [o] is [None]. The payload compares ["None"]
    against ["Some " ^ pp v], [pp] defaulting to {!Pp.abstract}. *)

val is_some : ?pos:pos -> ?msg:string -> 'a option -> unit
(** [is_some o] is [()] iff [o] is [Some _]. No [?pp]: the failing side is
    [None]. Payload-identical to {!require_some}'s. *)

(** {1:unwrapping Unwrapping}

    Assert the constructor and return the payload. The rejected side renders
    with the caller's printer, {!Pp.abstract} without one, and only on
    failure. *)

val require_some : ?pos:pos -> ?msg:string -> 'a option -> 'a
(** [require_some o] is [v] iff [o] is [Some v]. *)

val require_ok :
  ?pos:pos -> ?msg:string -> ?pp_error:'e printer -> ('a, 'e) result -> 'a
(** [require_ok r] is [v] iff [r] is [Ok v]. *)

val require_error :
  ?pos:pos -> ?msg:string -> ?pp_ok:'a printer -> ('a, 'e) result -> 'e
(** [require_error r] is [e] iff [r] is [Error e]. *)

val require_match :
  ?pos:pos -> ?msg:string -> ?pp:'a printer -> ('a -> 'b option) -> 'a -> 'b
(** [require_match extract v] is [b] iff [extract v] is [Some b]. Its payload
    is {!Failure.predicate}'s, with ["a match"] as the claim. An exception
    raised by [extract] propagates unchanged. *)

(** {1:predicates Predicates}

    Both build a {!Failure.predicate} payload: a claim sentence on the expected
    side, a rendered value on the actual side, and no diff between them —
    a description is not a rendering. *)

val satisfies :
  ?pos:pos ->
  ?msg:string ->
  ?claim:string ->
  'a testable ->
  ('a -> bool) ->
  'a ->
  unit
(** [satisfies t pred v] is [()] iff [pred v]. [claim] takes the expected side
    and defaults to ["value satisfying the predicate"]; [t]'s equality is never
    consulted. [pred] must be total. *)

val mem : ?pos:pos -> ?msg:string -> 'a testable -> 'a -> 'a list -> unit
(** [mem t x xs] is [()] iff [xs] has an element equal to [x] under [t]. The
    claim names [x], the value is [xs], both through [t]'s printer. Membership
    over bytes is {!contains}, whose payload is byte offsets and cannot be
    shared. *)

(** {1:containment String containment}

    Every verb here builds a {!Failure.containment} payload: the needle, the
    haystack's length, the byte offset of the needle's first occurrence
    anywhere in it when there is one, and a bounded excerpt whose policy
    {!Failure.containment} owns. The [demand] field is how a renderer tells the
    verbs apart. *)

val contains : ?pos:pos -> ?msg:string -> sub:string -> string -> unit
(** [contains ~sub s] is [()] iff [s] contains [sub] as a byte substring; the
    empty needle is contained in every string. *)

val not_contains : ?pos:pos -> ?msg:string -> sub:string -> string -> unit
(** [not_contains ~sub s] is [()] iff [s] does {e not} contain [sub] — so it
    always fails when [sub] is empty. *)

val starts_with : ?pos:pos -> ?msg:string -> affix:string -> string -> unit
(** [starts_with ~affix s] is [()] iff [s] begins with [affix]. Recording the
    affix's first occurrence when it has one is the point: a report separates
    "not there at all" from "there, but not at the start". *)

val ends_with : ?pos:pos -> ?msg:string -> affix:string -> string -> unit
(** [ends_with ~affix s] is [()] iff [s] ends with [affix]; the payload is
    {!starts_with}'s. *)

val in_order : ?pos:pos -> ?msg:string -> subs:string list -> string -> unit
(** [in_order ~subs s] is [()] iff every element of [subs] occurs in [s], each
    match beginning at or after the {e end} of the previous element's match;
    matches are leftmost, so [["aa"; "aa"]] needs four [a]s. An empty element
    matches at the cursor without advancing it.

    The failing element is the needle, and a {!Failure.Ordered} demand carries
    its zero-based index and the byte the search resumed from. [found_at] keeps
    its plain meaning — the element's first occurrence {e anywhere} — so a
    report separates "not in the string" from "in the string, but before the
    cursor". The excerpt windows on the cursor: the region the search was
    reading.

    Raises [Invalid_argument] if [subs] is empty: an assertion that demands
    nothing is a programmer error, not a passing test. *)

(** {1:exceptions Exceptions}

    Both verbs re-raise {!Failure.Check_failure}, {!Failure.Skip_test} and
    {!Failure.Timeout} from inside the thunk unchanged: without that guard a
    [raises] over code that itself asserts would swallow the assertion failure
    and report "wrong exception". Consequently the control exceptions cannot be
    asserted. Both build a {!Failure.raised} payload. *)

val raises : ?pos:pos -> ?msg:string -> exn -> (unit -> 'a) -> unit
(** [raises e f] is [()] iff [f ()] raises an exception structurally equal to
    [e]. The payload records the expected exception alone when [f ()] returned,
    and both plus the raised one's backtrace otherwise — with a
    {!Failure.message_diff} when the two share a constructor and differ only in
    a message. The verb holds both exceptions, so it names that constructor
    rather than leaving a renderer to guess it from a rendering.

    Structural equality is [Stdlib.( = )]: a payload it cannot compare (a
    functional value) makes the comparison itself raise [Invalid_argument],
    which propagates — assert such exceptions with {!raises_match}. *)

val raises_match :
  ?pos:pos -> ?msg:string -> (exn -> bool) -> (unit -> 'a) -> unit
(** [raises_match pred f] is [()] iff [f ()] raises an exception satisfying
    [pred], which must be total. A predicate has no rendering, so the payload's
    expected side is absent and its [predicate] flag is set — which is what
    separates a rejection from an uncaught exception. *)

(** Exception predicates for {!raises_match}: a constructor check and, with
    [~substring], a byte-substring check on the message (the empty string
    always matches). A whole message is {!raises}' job — it holds both
    exceptions, so it reports a message diff. *)
module Exn : sig
  val invalid_arg : ?substring:string -> exn -> bool
  (** [invalid_arg e] is [true] iff [e] is [Invalid_argument m] and [m]
      contains [substring], if given. *)

  val failure : ?substring:string -> exn -> bool
  (** [failure e] is [true] iff [e] is [Failure m] and [m] contains
      [substring], if given. *)

  val sys_error : ?substring:string -> exn -> bool
  (** [sys_error e] is [true] iff [e] is [Sys_error m] and [m] contains
      [substring], if given. Completes the set: these three are exactly the
      message-carrying exceptions {!raises} diffs by message. *)
end

(** {1:escapes Escape hatches} *)

val fail : ?pos:pos -> string -> 'a
(** [fail msg] raises {!Failure.Check_failure} carrying a {!Failure.message}
    payload. It never returns. *)

val failf : ?pos:pos -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [failf fmt ...] is {!fail} with a [Format] message. It never returns. *)

val skip : ?reason:string -> unit -> 'a
(** [skip ()] raises {!Failure.Skip_test} with [reason]; the runner reports the
    current test as skipped. It never returns. *)
