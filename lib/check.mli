(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The assertion verbs.

    A verb returns when its claim holds. When it does not, the verb builds one
    {!Failure.t} with a {{!Failure.section-constructors}constructor} of
    {!Failure}, which bounds every text that it is given. The failure is located
    by {!Loc.resolve}, and the verb raises it with {!Failure.refuse}, which
    records it before it raises a {!Failure.Check_failure}. {!skip} raises a
    {!Failure.Control} and builds no failure.

    A verb's [?msg] is the failure's [msg], and its [?__POS__] is the failure's
    site in place of the one {!Loc.resolve} finds. A verb prints nothing,
    computes no diff and reads no state of the run, and a passing one resolves
    no location. Only {!raises}, {!raises_match} and {!eventually} catch an
    exception, and only from the function that they run: what the equality, the
    order or the printer of a witness, a [?pp], a predicate or an [extract]
    raises escapes the verb as it is. *)

(* The facade ([{1:assertions}] in windtrap.mli) states each verb's contract
   for a test's author. The docs here state only what it does not: the payload
   each verb builds and how it raises. *)

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
(** [equal] records a comparison under a tolerance as
    {{!section-tolerances}tolerances} states. *)

val not_equal : ?__POS__:pos -> ?msg:string -> 'a testable -> 'a -> 'a -> unit
(** The payload of [not_equal t a b] sets [not_] and holds the rendering of [a]
    on both sides. *)

val is_true : ?__POS__:pos -> ?msg:string -> bool -> unit
(** See the facade's [is_true]. *)

val is_false : ?__POS__:pos -> ?msg:string -> bool -> unit
(** See the facade's [is_false]. *)

val is_none : ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> 'a option -> unit
(** The payload of [is_none o] has ["None"] as [expected], and as [actual]
    ["Some "] followed by the value through [pp]. *)

val is_some : ?__POS__:pos -> ?msg:string -> 'a option -> unit
(** The payload of [is_some o] is that of {!require_some}. *)

val is_ok :
  ?__POS__:pos -> ?msg:string -> ?pp:'e printer -> ('a, 'e) result -> unit
(** The payload of [is_ok r] is that of {!require_ok}. *)

val is_error :
  ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> ('a, 'e) result -> unit
(** The payload of [is_error r] is that of {!require_error}. *)

(** {1:unwrapping Unwrapping} *)

val require_some : ?__POS__:pos -> ?msg:string -> 'a option -> 'a
(** See the facade's [require_some]. *)

val require_ok :
  ?__POS__:pos -> ?msg:string -> ?pp:'e printer -> ('a, 'e) result -> 'a
(** The payload of [require_ok r] has ["Ok _"] as [expected], and as [actual]
    ["Error "] followed by the error through [pp]. *)

val require_error :
  ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> ('a, 'e) result -> 'e
(** The payload of [require_error r] has ["Error _"] as [expected], and as
    [actual] ["Ok "] followed by the value through [pp]. *)

val require_match :
  ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> ('a -> 'b option) -> 'a -> 'b
(** The payload of [require_match extract v] is a {!Failure.predicate} whose
    claim is ["a match"] and whose value is [v], the input of [extract], through
    [pp]. *)

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
(** The payload of [satisfies t pred v] holds [claim] and [v] through [t]'s
    printer. Nothing ties [claim] to [pred], so the expected side of such a
    payload is the caller's own text. *)

val mem : ?__POS__:pos -> ?msg:string -> 'a testable -> 'a -> 'a list -> unit
(** The claim of [mem t x xs] is [a list containing <x>] and the value is [xs]
    through [Testable.list t]. *)

(** {1:orders Orders}

    The four verbs read the sign of [compare v than] under the order of [t]
    ({!Testable.compare}). They build a {!Failure.predicate} whose claim is the
    relation and the bound through [t]'s printer, as [less than 3], and whose
    value is [v] through the same printer. *)

val less : ?__POS__:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** See the facade's [less]. *)

val at_most :
  ?__POS__:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** See the facade's [at_most]. *)

val greater :
  ?__POS__:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** See the facade's [greater]. *)

val at_least :
  ?__POS__:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** See the facade's [at_least]. *)

(** {1:containment String containment}

    The five verbs compare bytes. Each builds a {!Failure.containment} from its
    demand, the needle and the whole haystack, and {!Failure.containment} owns
    the excerpt and its bounds. [found_at] is always the first occurrence of the
    needle from byte [0] of the haystack, when it has one.

    The demand is {!Failure.Prefix} for {!starts_with}, {!Failure.Suffix} for
    {!ends_with}, {!Failure.Ordered} for {!in_order} and {!Failure.Anywhere} for
    {!contains} and {!not_contains}, which [found_at] tells apart. *)

val contains : ?__POS__:pos -> ?msg:string -> sub:string -> string -> unit
(** See the facade's [contains]. *)

val not_contains : ?__POS__:pos -> ?msg:string -> sub:string -> string -> unit
(** See the facade's [not_contains]. *)

val starts_with : ?__POS__:pos -> ?msg:string -> prefix:string -> string -> unit
(** See the facade's [starts_with]. *)

val ends_with : ?__POS__:pos -> ?msg:string -> suffix:string -> string -> unit
(** The [found_at] of [ends_with ~suffix s] is the leftmost occurrence and not
    the one nearest the end. *)

val in_order : ?__POS__:pos -> ?msg:string -> subs:string list -> string -> unit
(** [in_order ~subs s] matches each element at its leftmost occurrence from the
    end of the match before it. The needle of the payload is the first element
    that has no such match, under a {!Failure.Ordered} demand, where [found_at]
    keeps its meaning. *)

(** {1:exceptions Exceptions}

    Both verbs call the function through {!Failure.catch} and compare only an
    [`Exception], before [pred] is applied. They raise again with
    {!Failure.reraise} an [`Assertion], a failure the function caught included,
    and every {!Failure.Control}.

    Both build a {!Failure.raised}, and hold an exception as
    {!Failure.exn_to_string} gives it and its backtrace as
    {!Failure.backtrace_to_string} gives it. *)

val raises : ?__POS__:pos -> ?msg:string -> exn -> (unit -> 'a) -> unit
(** [raises e f] compares with [Stdlib.( = )].
    - When [f ()] returns, the payload holds [e] as [expected] and nothing else.
    - When [f ()] raises another exception, it holds both exceptions and the
      backtrace of the raised one, when one was recorded. It also holds a
      {!Failure.message_diff} when both are an [Invalid_argument], both a
      [Failure] or both a [Sys_error], and their messages differ.

    [predicate] is [false] in both. *)

val raises_match :
  ?__POS__:pos -> ?msg:string -> (exn -> bool) -> (unit -> 'a) -> unit
(** The payload of [raises_match pred f] has no [expected] and sets [predicate].
    It holds the raised exception, with its backtrace when one was recorded,
    when [pred] rejected one, and nothing when [f ()] returned. It never holds a
    {!Failure.message_diff}, which only {!raises} can build. *)

module Exn : sig
  (** See the facade's [Exn]. *)

  val invalid_arg : ?sub:string -> exn -> bool
  (** See the facade's [Exn.invalid_arg]. *)

  val failure : ?sub:string -> exn -> bool
  (** See the facade's [Exn.failure]. *)

  val sys_error : ?sub:string -> exn -> bool
  (** See the facade's [Exn.sys_error]. *)
end

(** {1:polling Polling} *)

val eventually : within:float -> (unit -> 'a) -> 'a
(** [eventually ~within f] calls [f] through {!Failure.catch}. The fault of the
    last call is raised again with {!Failure.reraise}, so the enclosing scope
    records an assertion. Every call but the last runs in its own scope, so what
    it records is not seen outside. *)

(** {1:escapes Escape hatches} *)

val fail : ?__POS__:pos -> string -> 'a
(** [fail msg] refuses ({!Failure.refuse}) the failure whose payload is [msg],
    built by {!Failure.message}. The text is the payload and not an annotation,
    so the [msg] of the failure is [None]. *)

val failf : ?__POS__:pos -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** See the facade's [failf]. *)

val skip : ?reason:string -> unit -> 'a
(** [skip ?reason ()] raises [Failure.Control (`Skip reason)]. *)

(** {1:tolerances Tolerances}

    Each call of {!equal} whose witness compared under a tolerance
    ({{!Testable.observed}observed}) records, at its call site, the comparison
    whose error was the largest share of the bound it allowed: a test reads, for
    each site, how close its values came to its tolerance, and a tolerance far
    looser than the errors it saw says the test is weaker than it reads. The
    calls of every domain count, and a site is the location of the call, [None]
    where none resolves (see {!tolerances}). *)

type tolerance = {
  site : Loc.t option;  (** The call site of {!equal}. *)
  comparisons : int;
      (** The calls at [site] that compared under a tolerance. *)
  error : float;
      (** The error of the comparison that came closest to its bound, as a share
          of it. *)
  tolerance : float;  (** The bound that comparison allowed. *)
}
(** The type for what one site of {!equal} saw under a tolerance. *)

val tolerances : ?at:Loc.t -> unit -> tolerance list
(** [tolerances ?at ()] is every site recorded since the last [tolerances] or
    {!reset_tolerances}, ordered by site, and forgets them. A call with no
    location, as one in tail position without [~__POS__] has, is recorded at
    [at], as a failure without one takes the declaration of its test, and merged
    with what [at] recorded. The runner reads them as an attempt ends, with [at]
    the test's declaration. *)

val reset_tolerances : unit -> unit
(** [reset_tolerances ()] forgets every site recorded. The runner calls it as an
    attempt starts. *)
