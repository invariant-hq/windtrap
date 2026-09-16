(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The assertion verbs.

    {!Windtrap} re-exports every verb flat and states the contract a test author
    reads; this interface states the payload each verb builds. A failing verb
    constructs one {!Failure.t} (a typed kind, an optional location, the [?msg]
    annotation) and raises {!Failure.Check_failure}; verbs never print, diff or
    touch run state. The location is [?__POS__] when given, else a best-effort
    call-stack capture, else none ({!Loc.resolve}). {!skip} is not a failure: it
    raises {!Failure.Skip_test}. *)

(** {1:types Types} *)

type pos = Loc.pos
(** The type of [__POS__] payloads: file, line, start column, end column. *)

type 'a printer = Format.formatter -> 'a -> unit
(** The type for value printers, as taken by [?pp]. *)

type 'a testable = 'a Testable.t
(** The type for assertion witnesses; see {!Testable}. *)

(** {1:equalities Equalities}

    Each verb builds a diffable {!Failure.equality} over two rendered values or
    constructor descriptions, expected first. The witness renders only on
    failure. *)

val equal : ?__POS__:pos -> ?msg:string -> 'a testable -> 'a -> 'a -> unit
(** [equal t expected actual] is [()] iff [Testable.equal t expected actual]. *)

val not_equal : ?__POS__:pos -> ?msg:string -> 'a testable -> 'a -> 'a -> unit
(** [not_equal t a b] is [()] iff [a] and [b] are not equal under [t]. The
    payload sets [not_] and stores [a]'s rendering on both sides. *)

val is_true : ?__POS__:pos -> ?msg:string -> bool -> unit
(** [is_true b] is [()] iff [b]. The payload compares ["true"] against
    ["false"]. *)

val is_false : ?__POS__:pos -> ?msg:string -> bool -> unit
(** [is_false b] is [()] iff [not b]. *)

val is_none : ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> 'a option -> unit
(** [is_none o] is [()] iff [o] is [None]. The payload compares ["None"] against
    ["Some " ^ pp v], [pp] defaulting to {!Pp.abstract}. *)

val is_some : ?__POS__:pos -> ?msg:string -> 'a option -> unit
(** [is_some o] is [()] iff [o] is [Some _]. Payload-identical to
    {!require_some}'s. *)

val is_ok :
  ?__POS__:pos -> ?msg:string -> ?pp:'e printer -> ('a, 'e) result -> unit
(** [is_ok r] is [()] iff [r] is [Ok _]. Payload-identical to {!require_ok}'s.
*)

val is_error :
  ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> ('a, 'e) result -> unit
(** [is_error r] is [()] iff [r] is [Error _]. Payload-identical to
    {!require_error}'s. *)

(** {1:unwrapping Unwrapping}

    Assert the constructor and return the payload. The rejected side renders
    with the caller's printer, {!Pp.abstract} without one, only on failure. *)

val require_some : ?__POS__:pos -> ?msg:string -> 'a option -> 'a
(** [require_some o] is [v] iff [o] is [Some v]. *)

val require_ok :
  ?__POS__:pos -> ?msg:string -> ?pp:'e printer -> ('a, 'e) result -> 'a
(** [require_ok r] is [v] iff [r] is [Ok v]; [pp] renders the rejected [Error]
    payload. *)

val require_error :
  ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> ('a, 'e) result -> 'e
(** [require_error r] is [e] iff [r] is [Error e]; [pp] renders the rejected
    [Ok] payload. *)

val require_match :
  ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> ('a -> 'b option) -> 'a -> 'b
(** [require_match extract v] is [b] iff [extract v] is [Some b]. Its payload is
    {!Failure.predicate}'s with ["a match"] as the claim. An exception raised by
    [extract] propagates unchanged. *)

(** {1:predicates Predicates}

    Both build a {!Failure.predicate} payload: a claim sentence on the expected
    side, a rendered value on the actual side, no diff between them. *)

val satisfies :
  ?__POS__:pos ->
  ?msg:string ->
  ?claim:string ->
  'a testable ->
  ('a -> bool) ->
  'a ->
  unit
(** [satisfies t pred v] is [()] iff [pred v]. [claim] takes the expected side
    and defaults to ["value satisfying the predicate"]; [t]'s equality is never
    consulted. [pred] must be total. *)

val mem : ?__POS__:pos -> ?msg:string -> 'a testable -> 'a -> 'a list -> unit
(** [mem t x xs] is [()] iff [xs] has an element equal to [x] under [t]. The
    claim names [x], the value is [xs], both through [t]'s printer. *)

(** {1:orders Orders}

    The four verbs compare under [t]'s order ({!Testable.compare}) and build a
    {!Failure.predicate} payload whose claim is the relation and the bound
    rendered by [t] (["less than 3"]) and whose value is [v] rendered by [t].
    [t]'s equality is never consulted. All four raise [Invalid_argument], naming
    the verb and [Testable.with_compare], when [t] carries no order. *)

val less : ?__POS__:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** [less t ~than v] is [()] iff [v] ranks strictly below [than]. *)

val at_most :
  ?__POS__:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** [at_most t ~than v] is [()] iff [v] ranks below or the same as [than]. *)

val greater :
  ?__POS__:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** [greater t ~than v] is [()] iff [v] ranks strictly above [than]. *)

val at_least :
  ?__POS__:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** [at_least t ~than v] is [()] iff [v] ranks above or the same as [than]. *)

(** {1:containment String containment}

    Each verb builds a {!Failure.containment} payload: the needle, the
    haystack's length, the byte offset of the needle's first occurrence anywhere
    when there is one, and a bounded excerpt. The [demand] field tells the verbs
    apart. *)

val contains : ?__POS__:pos -> ?msg:string -> sub:string -> string -> unit
(** [contains ~sub s] is [()] iff [s] contains [sub] as a byte substring; the
    empty needle is contained in every string. *)

val not_contains : ?__POS__:pos -> ?msg:string -> sub:string -> string -> unit
(** [not_contains ~sub s] is [()] iff [s] does not contain [sub]; it always
    fails when [sub] is empty. *)

val starts_with : ?__POS__:pos -> ?msg:string -> affix:string -> string -> unit
(** [starts_with ~affix s] is [()] iff [s] begins with [affix]. The payload
    records the affix's first occurrence when it has one. *)

val ends_with : ?__POS__:pos -> ?msg:string -> affix:string -> string -> unit
(** [ends_with ~affix s] is [()] iff [s] ends with [affix]; the payload is
    {!starts_with}'s. *)

val in_order : ?__POS__:pos -> ?msg:string -> subs:string list -> string -> unit
(** [in_order ~subs s] is [()] iff every element of [subs] occurs in [s], each
    match beginning at or after the end of the previous element's match; matches
    are leftmost, so [["aa"; "aa"]] needs four [a]s, and an empty element
    matches at the cursor without advancing it. The failing element is the
    needle; a {!Failure.Ordered} demand carries its zero-based index and the
    byte the search resumed from, and the excerpt windows on that cursor.

    Raises [Invalid_argument] if [subs] is empty. *)

(** {1:exceptions Exceptions}

    Both verbs re-raise {!Failure.Check_failure}, {!Failure.Skip_test} and
    {!Failure.Timeout} from inside the thunk unchanged, so the control
    exceptions cannot be asserted. Both build a {!Failure.raised} payload. *)

val raises : ?__POS__:pos -> ?msg:string -> exn -> (unit -> 'a) -> unit
(** [raises e f] is [()] iff [f ()] raises an exception structurally equal to
    [e] under [Stdlib.( = )]. The payload records the expected exception alone
    when [f ()] returned, and both plus the raised one's backtrace otherwise,
    with a {!Failure.message_diff} when the two share a constructor and differ
    only in a message. A payload [( = )] cannot compare (a functional value)
    makes the comparison raise [Invalid_argument], which propagates; use
    {!raises_match} for such exceptions. *)

val raises_match :
  ?__POS__:pos -> ?msg:string -> (exn -> bool) -> (unit -> 'a) -> unit
(** [raises_match pred f] is [()] iff [f ()] raises an exception satisfying
    [pred], which must be total. The payload's expected side is absent and its
    [predicate] flag set. *)

(** Exception predicates for {!raises_match}: a constructor check and, with
    [~substring], a byte-substring check on the message (the empty string always
    matches). *)
module Exn : sig
  val invalid_arg : ?substring:string -> exn -> bool
  (** [invalid_arg e] is [true] iff [e] is [Invalid_argument m] and [m] contains
      [substring], if given. *)

  val failure : ?substring:string -> exn -> bool
  (** [failure e] is [true] iff [e] is [Failure m] and [m] contains [substring],
      if given. *)

  val sys_error : ?substring:string -> exn -> bool
  (** [sys_error e] is [true] iff [e] is [Sys_error m] and [m] contains
      [substring], if given. *)
end

(** {1:escapes Escape hatches} *)

val fail : ?__POS__:pos -> string -> 'a
(** [fail msg] raises {!Failure.Check_failure} carrying a {!Failure.message}
    payload. Never returns. *)

val failf : ?__POS__:pos -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [failf fmt ...] is {!fail} with a [Format] message. Never returns. *)

val skip : ?reason:string -> unit -> 'a
(** [skip ()] raises {!Failure.Skip_test} with [reason]; the runner reports the
    current test as skipped. Never returns. *)
