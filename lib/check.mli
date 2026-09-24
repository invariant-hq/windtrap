(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The assertion verbs.

    A verb returns when its claim holds. When it does not, the verb builds one
    {!Failure.t} with a {{!Failure.section-constructors}constructor} of
    {!Failure}, which bounds every text that it is given. The failure is located
    by {!Loc.resolve}, and the verb raises it in a {!Failure.Check_failure}.
    This interface states the payload that each verb builds, for whoever reads
    or renders a failure. {!skip} raises a {!Failure.Control} and builds no
    failure.

    A verb's [?msg] is the failure's [msg], and its [?__POS__] is the failure's
    site in place of the one {!Loc.resolve} finds. A verb prints nothing,
    computes no diff and reads no state of the run. A passing verb resolves no
    location and calls no printer. Only {!raises} and {!raises_match} catch an
    exception, and only from the function that they run. What the equality, the
    order or the printer of a witness, a [?pp], a predicate or an [extract]
    raises escapes the verb as it is. *)

(* The facade declares every value below again, with the contract that a test's
   author reads ([{1:assertions}] in windtrap.mli). A change to one text is a
   change to the other. *)

(** {1:types Types} *)

type pos = Loc.pos
(** The type for [__POS__] values. *)

type 'a printer = Format.formatter -> 'a -> unit
(** The type for value printers, as a [?pp] takes one. Without its [?pp] a verb
    puts {!Pp.abstract}, [<abstract>], in the payload. *)

type 'a testable = 'a Testable.t
(** The type for witnesses (see {!Testable}). {!equal}, {!not_equal} and {!mem}
    read the equality and the printer of their witness, and {!satisfies} its
    printer only. The {{!section-orders}ordering verbs} read its order and its
    printer. *)

(** {1:equalities Equalities}

    Each verb builds a diffable {!Failure.equality}, the expected side first.
    {!require_some}, {!require_ok} and {!require_error} build the same one. *)

val equal : ?__POS__:pos -> ?msg:string -> 'a testable -> 'a -> 'a -> unit
(** [equal t expected actual] is [()] iff [Testable.equal t expected actual].
    The payload holds both values through [t]'s printer. *)

val not_equal : ?__POS__:pos -> ?msg:string -> 'a testable -> 'a -> 'a -> unit
(** [not_equal t a b] is [()] iff [Testable.equal t a b] is [false]. The payload
    sets [not_] and holds the rendering of [a] on both sides. [b] is never
    printed. *)

val is_true : ?__POS__:pos -> ?msg:string -> bool -> unit
(** [is_true b] is [()] iff [b]. The payload has ["true"] as [expected] and
    ["false"] as [actual]. *)

val is_false : ?__POS__:pos -> ?msg:string -> bool -> unit
(** [is_false b] is [()] iff [not b]. The payload has ["false"] as [expected]
    and ["true"] as [actual]. *)

val is_none : ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> 'a option -> unit
(** [is_none o] is [()] iff [o] is [None]. The payload has ["None"] as
    [expected], and as [actual] ["Some "] followed by the value through [pp]. *)

val is_some : ?__POS__:pos -> ?msg:string -> 'a option -> unit
(** [is_some o] is [()] iff [o] is [Some _]. Its payload is that of
    {!require_some}. It takes no [?pp], since the failing side is [None]. *)

val is_ok :
  ?__POS__:pos -> ?msg:string -> ?pp:'e printer -> ('a, 'e) result -> unit
(** [is_ok r] is [()] iff [r] is [Ok _]. Its payload is that of {!require_ok}.
*)

val is_error :
  ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> ('a, 'e) result -> unit
(** [is_error r] is [()] iff [r] is [Error _]. Its payload is that of
    {!require_error}. *)

(** {1:unwrapping Unwrapping}

    Each verb asserts a constructor and returns the value under it. *)

val require_some : ?__POS__:pos -> ?msg:string -> 'a option -> 'a
(** [require_some o] is [v] iff [o] is [Some v]. The payload has ["Some _"] as
    [expected] and ["None"] as [actual]. *)

val require_ok :
  ?__POS__:pos -> ?msg:string -> ?pp:'e printer -> ('a, 'e) result -> 'a
(** [require_ok r] is [v] iff [r] is [Ok v]. The payload has ["Ok _"] as
    [expected], and as [actual] ["Error "] followed by the error through [pp].
*)

val require_error :
  ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> ('a, 'e) result -> 'e
(** [require_error r] is [e] iff [r] is [Error e]. The payload has ["Error _"]
    as [expected], and as [actual] ["Ok "] followed by the value through [pp].
*)

val require_match :
  ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> ('a -> 'b option) -> 'a -> 'b
(** [require_match extract v] is [b] iff [extract v] is [Some b]. The payload is
    a {!Failure.predicate} whose claim is ["a match"] and whose value is [v],
    the input of [extract], through [pp]. *)

(** {1:predicates Predicates}

    Each verb builds a {!Failure.predicate}: a claim in words on the expected
    side, a printed value on the actual side, and no diff between them. The
    claim completes the word [expected], as in [expected less than 3]. The
    {{!section-orders}ordering verbs} and {!require_match} build the same
    payload. *)

val satisfies :
  ?__POS__:pos ->
  ?msg:string ->
  ?claim:string ->
  'a testable ->
  ('a -> bool) ->
  'a ->
  unit
(** [satisfies t pred v] is [()] iff [pred v]. [pred] must be total. The payload
    holds [claim] and [v] through [t]'s printer. [claim] defaults to
    ["value satisfying the predicate"]. Nothing ties it to [pred], so the
    expected side of such a payload is the caller's own text. *)

val mem : ?__POS__:pos -> ?msg:string -> 'a testable -> 'a -> 'a list -> unit
(** [mem t x xs] is [()] iff [List.exists (Testable.equal t x) xs], so [x] takes
    the expected side of [t]'s equality. The claim is [a list containing <x>]
    and the value is [xs] through [Testable.list t]. {!contains} is membership
    over bytes. *)

(** {1:orders Orders}

    The four verbs compare [v] with [than] under the order of [t]
    ({!Testable.compare}), and read the sign of [compare v than]. They build a
    {!Failure.predicate} whose claim is the relation and the bound through [t]'s
    printer, as [less than 3], and whose value is [v] through the same printer.
    The relations are [less than], [at most], [greater than] and [at least]. The
    claim is built from the relation and the bound that the verb compares, so it
    cannot differ from the comparison. The equality of [t] is never read, so a
    witness with a tolerance orders without it.

    Each verb raises [Invalid_argument] when [t] carries no order, whether or
    not its claim holds. The message names the verb and [Testable.with_compare].
*)

val less : ?__POS__:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** [less t ~than v] is [()] iff [compare v than < 0]. *)

val at_most :
  ?__POS__:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** [at_most t ~than v] is [()] iff [compare v than <= 0]. *)

val greater :
  ?__POS__:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** [greater t ~than v] is [()] iff [compare v than > 0]. *)

val at_least :
  ?__POS__:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** [at_least t ~than v] is [()] iff [compare v than >= 0]. *)

(** {1:containment String containment}

    The five verbs compare bytes. Each builds a {!Failure.containment} from a
    claim in one line, the needle and the whole haystack, and
    {!Failure.containment} owns the excerpt and its bounds. [found_at] is always
    the first occurrence of the needle from byte [0] of the haystack, when it
    has one.

    Three fields tell the failures apart. [demand] is {!Failure.Ordered} for
    {!in_order} and {!Failure.Anywhere} for the other four. [found_at] tells a
    failed {!contains} from a failed {!not_contains} (see
    {!Failure.Containment}). Only the claim tells {!starts_with} and
    {!ends_with} from those two and from each other, and no renderer shows a
    claim. *)

val contains : ?__POS__:pos -> ?msg:string -> sub:string -> string -> unit
(** [contains ~sub s] is [()] iff [sub] occurs in [s]. The empty string occurs
    in every string. *)

val not_contains : ?__POS__:pos -> ?msg:string -> sub:string -> string -> unit
(** [not_contains ~sub s] is [()] iff [sub] does not occur in [s], so it always
    fails when [sub] is empty. *)

val starts_with : ?__POS__:pos -> ?msg:string -> affix:string -> string -> unit
(** [starts_with ~affix s] is [()] iff [String.starts_with ~prefix:affix s]. *)

val ends_with : ?__POS__:pos -> ?msg:string -> affix:string -> string -> unit
(** [ends_with ~affix s] is [()] iff [String.ends_with ~suffix:affix s]. The
    payload is built as {!starts_with} builds it, with a claim of its own, so
    [found_at] is the leftmost occurrence and not the one nearest the end. *)

val in_order : ?__POS__:pos -> ?msg:string -> subs:string list -> string -> unit
(** [in_order ~subs s] is [()] iff every element of [subs] occurs in [s], each
    match starting at or after the end of the match before it. A match is the
    leftmost one from there, so [["aa"; "aa"]] needs four [a]s, and an empty
    element matches where the search stands without moving it.

    The needle of the payload is the first element that has no such match, under
    a {!Failure.Ordered} demand, where [found_at] keeps its meaning.

    Raises [Invalid_argument] if [subs] is empty, whatever [s] is, because an
    assertion that demands nothing is a mistake and not a passing test. *)

(** {1:exceptions Exceptions}

    Both verbs call the function through {!Failure.catch} and compare only an
    [`Exception], before [pred] is applied. They raise again, untouched, a
    {!Failure.Check_failure} and every {!Failure.Control}, so an assertion that
    fails inside the function is not reported as the wrong exception, an
    intercepted [exit] is never accepted, and an [assume] inside the function
    discards the case.

    Both build a {!Failure.raised}, and hold an exception as
    [Printexc.to_string] gives it and its backtrace as
    {!Failure.backtrace_to_string} gives it. *)

val raises : ?__POS__:pos -> ?msg:string -> exn -> (unit -> 'a) -> unit
(** [raises e f] is [()] iff [f ()] raises an exception equal to [e] under
    [Stdlib.( = )].
    - When [f ()] returns, the payload holds [e] as [expected] and nothing else.
    - When [f ()] raises another exception, it holds both exceptions and the
      backtrace of the raised one, when one was recorded. It also holds a
      {!Failure.message_diff} when both are an [Invalid_argument], both a
      [Failure] or both a [Sys_error], and their messages differ.

    [predicate] is [false] in both. When [( = )] meets a functional value in the
    two exceptions it raises [Invalid_argument], which escapes [raises] in place
    of what [f] raised. {!raises_match} takes such an exception. *)

val raises_match :
  ?__POS__:pos -> ?msg:string -> (exn -> bool) -> (unit -> 'a) -> unit
(** [raises_match pred f] is [()] iff [f ()] raises an exception that [pred]
    accepts. [pred] must be total.

    The payload has no [expected] and sets [predicate], which is what tells a
    rejected exception from an uncaught one. It holds the raised exception, with
    its backtrace when one was recorded, when [pred] rejected one, and nothing
    when [f ()] returned. It never holds a {!Failure.message_diff}, which only
    {!raises} can build. *)

module Exn : sig
  (** Predicates on exceptions, for {!raises_match}. The three constructors are
      those whose messages {!raises} compares. A predicate returns [false] on
      any other exception and never raises. *)

  val invalid_arg : ?substring:string -> exn -> bool
  (** [invalid_arg e] is [true] iff [e] is [Invalid_argument m] and [substring],
      when given, occurs in [m] as bytes. The empty string occurs in every
      message. *)

  val failure : ?substring:string -> exn -> bool
  (** [failure e] is {!invalid_arg} for [Failure m], the exception of [Stdlib].
  *)

  val sys_error : ?substring:string -> exn -> bool
  (** [sys_error e] is {!invalid_arg} for [Sys_error m]. *)
end

(** {1:escapes Escape hatches} *)

val fail : ?__POS__:pos -> string -> 'a
(** [fail msg] raises a {!Failure.Check_failure} whose payload is [msg], built
    by {!Failure.message}. The text is the payload and not an annotation, so the
    [msg] of the failure is [None]. It never returns. *)

val failf : ?__POS__:pos -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [failf fmt ...] is {!fail} with a message that [Format] builds. It never
    returns. *)

val skip : ?reason:string -> unit -> 'a
(** [skip ?reason ()] raises [Failure.Control (`Skip reason)]. It never returns.
*)
