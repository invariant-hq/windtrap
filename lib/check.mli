(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The assertion verbs.

    Twenty-seven verbs and the {!Exn} predicates, each verb raising one
    structured failure: a failing verb constructs a single {!Failure.t} — a
    typed kind, an optional location, the [?msg] annotation when given — and
    raises {!Failure.Check_failure}. Verbs never print, never diff, and never
    touch run state: renderers project failure data into reports, computing
    diffs from the rendered values stored in the payload.

    Comparisons go through a {!Testable.t} witness: {!equal} and {!not_equal}
    use its equality and, on failure only, render the values with its printer
    into the payload — expected precedes actual, always. The {!require_some},
    {!require_ok}, and {!require_error} verbs assert {e and unwrap}, so the
    happy path keeps its value:

    {[
      let user = require_some (Store.find store "alice") in
      let config = require_ok ~pp_error:Config.pp_error (Config.parse src) in
      equal Testable.string "alice" user.name
    ]}

    A failure's location is [?pos] ([__POS__] at the call site) when given, else
    a best-effort call-stack capture; when neither yields a location the failure
    has none ({!Loc.resolve} is the rule).

    {!skip} is not a failure: it raises {!Failure.Skip_test}, and the runner
    reports the test as skipped. *)

(** {1:types Types} *)

type pos = Loc.pos
(** The type of [__POS__] payloads: file, line, start column, end column. *)

type 'a printer = Format.formatter -> 'a -> unit
(** The type for value printers, as taken by [?pp_error] and [?pp_ok]. *)

type 'a testable = 'a Testable.t
(** The type for assertion witnesses; see {!Testable}. *)

(** {1:comparisons Comparisons} *)

val equal : ?pos:pos -> ?msg:string -> 'a testable -> 'a -> 'a -> unit
(** [equal t expected actual] is [()] iff [Testable.equal t expected actual].
    Otherwise it raises {!Failure.Check_failure} with both values rendered by
    [t]'s printer, expected first. Values are rendered only on failure. *)

val not_equal : ?pos:pos -> ?msg:string -> 'a testable -> 'a -> 'a -> unit
(** [not_equal t a b] is [()] iff [a] and [b] are {e not} equal under [t].
    Otherwise it raises {!Failure.Check_failure} carrying one rendering — [a]'s,
    stored on both sides of the payload — which renderers print once
    ([both sides equal: <v>]). *)

(** {1:booleans Booleans} *)

val is_true : ?pos:pos -> ?msg:string -> bool -> unit
(** [is_true b] is [()] iff [b]. The failure payload compares [true] against
    [false]. *)

val is_false : ?pos:pos -> ?msg:string -> bool -> unit
(** [is_false b] is [()] iff [not b]. *)

(** {1:containment String containment} *)

val contains : ?pos:pos -> ?msg:string -> sub:string -> string -> unit
(** [contains ~sub s] is [()] iff [s] contains [sub] as a byte substring; the
    empty needle is contained in every string. Otherwise it raises
    {!Failure.Check_failure} with a {!Failure.Containment} payload carrying
    [sub] and a bounded excerpt of [s]'s head (see {!Failure.containment} for
    the excerpt policy). *)

val not_contains : ?pos:pos -> ?msg:string -> sub:string -> string -> unit
(** [not_contains ~sub s] is [()] iff [s] does {e not} contain [sub] as a byte
    substring — so it always fails when [sub] is empty. Otherwise it raises
    {!Failure.Check_failure} with a {!Failure.Containment} payload carrying
    [sub], the byte offset of its first occurrence, and a bounded excerpt of [s]
    around that occurrence. *)

val in_order : ?pos:pos -> ?msg:string -> subs:string list -> string -> unit
(** [in_order ~subs s] is [()] iff every element of [subs] occurs in [s], each
    match beginning at or after the {e end} of the previous element's match;
    matches are leftmost. The chain never re-uses bytes, so [["aa"; "aa"]]
    needs four [a]s and not three.

    Otherwise it raises {!Failure.Check_failure} with a
    {!Failure.Containment} payload whose needle is the element that broke the
    chain and whose {!Failure.Ordered} demand carries that element's
    zero-based index and the byte offset the search resumed from; [found_at]
    is that element's first occurrence anywhere in [s], so a report separates
    "not in the string at all" from "in the string, but before the cursor" —
    the out-of-order bug — as {!starts_with} does. The excerpt windows on the
    cursor: what it shows is the region the search was reading.

    An empty element matches at the cursor without advancing it, {!contains}
    holding the empty needle to occur in every string.

    Raises [Invalid_argument] if [subs] is empty: an assertion that demands
    nothing is a programmer error, not a passing test. *)

val starts_with : ?pos:pos -> ?msg:string -> affix:string -> string -> unit
(** [starts_with ~affix s] is [()] iff [s] begins with [affix]. Otherwise it
    raises {!Failure.Check_failure} with a {!Failure.Containment} payload
    carrying [affix], a bounded excerpt of [s], and — when [affix] occurs
    somewhere in [s] — the offset of that occurrence, so a report distinguishes
    "not there at all" from "there, but not at the start". *)

val ends_with : ?pos:pos -> ?msg:string -> affix:string -> string -> unit
(** [ends_with ~affix s] is [()] iff [s] ends with [affix]; the payload is
    {!starts_with}'s. *)

(** {1:predicates Predicates} *)

val satisfies :
  ?pos:pos ->
  ?msg:string ->
  ?claim:string ->
  'a testable ->
  ('a -> bool) ->
  'a ->
  unit
(** [satisfies t pred v] is [()] iff [pred v]. Otherwise it raises
    {!Failure.Check_failure} with a {!Failure.predicate} payload carrying
    [claim] and [v] rendered by [t]'s printer — [t]'s equality is not consulted.
    [claim] is the sentence on the expected side and defaults to
    ["value satisfying the predicate"]. [pred] must be total; it runs on every
    call, the printer only on failure. *)

(** {1:options Options}

    The shape assertion, for when the value is not wanted. It takes the same
    optional printer the unwrapping verbs do rather than a {!Testable.t}: a
    witness carries an equality it never consults, and demanding one for a type
    the assertion does not inspect is what drives call sites to
    [equal (option pass) None x]. *)

val is_none : ?pos:pos -> ?msg:string -> ?pp:'a printer -> 'a option -> unit
(** [is_none o] is [()] iff [o] is [None]. On [Some v] it raises
    {!Failure.Check_failure} comparing [None] against [Some <v>], with [v]
    rendered by [pp] when given and as [<abstract>] otherwise; the printer runs
    only on failure. *)

(** {1:unwrapping Unwrapping}

    Each verb asserts the constructor and returns the payload, so the value
    flows on without a rebind. The rejected side of a [result] prints via
    [?pp_error]/[?pp_ok] when given and as [<abstract>] otherwise; printers run
    only on failure. *)

val require_some : ?pos:pos -> ?msg:string -> 'a option -> 'a
(** [require_some o] is [v] iff [o] is [Some v]. On [None] it raises
    {!Failure.Check_failure} comparing [Some _] against [None]. *)

val require_ok :
  ?pos:pos -> ?msg:string -> ?pp_error:'e printer -> ('a, 'e) result -> 'a
(** [require_ok r] is [v] iff [r] is [Ok v]. On [Error e] it raises
    {!Failure.Check_failure} comparing [Ok _] against [Error <e>], with [e]
    rendered by [pp_error] when given and as [<abstract>] otherwise. *)

val require_error :
  ?pos:pos -> ?msg:string -> ?pp_ok:'a printer -> ('a, 'e) result -> 'e
(** [require_error r] is [e] iff [r] is [Error e]. On [Ok v] it raises
    {!Failure.Check_failure} comparing [Error _] against [Ok <v>], with [v]
    rendered by [pp_ok] when given and as [<abstract>] otherwise. *)

val require_match :
  ?pos:pos -> ?msg:string -> ?pp:'a printer -> ('a -> 'b option) -> 'a -> 'b
(** [require_match extract v] is [b] iff [extract v] is [Some b] — the
    match-and-unwrap counterpart of {!require_some} for values that are not
    already options:

    {[
      let port =
        require_match ~pp:Uri.pp (function Tcp p -> Some p | _ -> None) addr
    ]}

    On [None] it raises {!Failure.Check_failure} with a {!Failure.predicate}
    payload whose claim is ["a match"], carrying [v] rendered by [pp] when given
    and as [<abstract>] otherwise; the printer runs only on failure. An
    exception raised by [extract] propagates unchanged. *)

(** {1:exceptions Exceptions}

    Both verbs run their thunk and re-raise the control exceptions
    {!Failure.Check_failure}, {!Failure.Skip_test}, and {!Failure.Timeout}
    unchanged, so an assertion failing (or a skip) {e inside} the thunk reports
    itself rather than being mistaken for a wrong exception. Consequently the
    control exceptions themselves cannot be asserted. *)

val raises : ?pos:pos -> ?msg:string -> exn -> (unit -> 'a) -> unit
(** [raises e f] is [()] iff [f ()] raises an exception structurally equal to
    [e]. It raises {!Failure.Check_failure} when [f ()] returns — the payload
    then records the expected exception alone — or when it raises a different
    exception, the payload then carrying both exceptions rendered by
    [Printexc.to_string], the raised one's backtrace when the runtime recorded
    one, and — when the two exceptions share their constructor and differ only
    in a message payload — a {!Failure.message_diff}, so a wrong-message failure
    reads as a message diff rather than two near-identical renderings. The verb
    holds the exceptions themselves, so it names the shared constructor instead
    of leaving a renderer to guess it from a rendering.

    Structural equality compares the exception's constructor and payload with
    [Stdlib.( = )]; a payload it cannot compare (a functional value) makes the
    comparison itself raise [Invalid_argument], which propagates out of the verb
    — assert such exceptions with {!raises_match}. *)

val raises_match :
  ?pos:pos -> ?msg:string -> (exn -> bool) -> (unit -> 'a) -> unit
(** [raises_match pred f] is [()] iff [f ()] raises an exception satisfying
    [pred]. It raises {!Failure.Check_failure} when [f ()] returns or when
    [pred] rejects the raised exception; a predicate has no rendering, so the
    payload's expected side is absent, but its [predicate] flag is set —
    distinguishing the rejection from an uncaught exception (see
    {!Failure.kind}) — and the rejected exception is carried rendered. [pred]
    must be total. {!Exn} provides the common predicates:

    {[
      raises_match (Exn.invalid_arg ~substring:"unhandled op") (fun () ->
          Machine.step m op)
    ]} *)

(** Exception predicates for {!raises_match}.

    Each predicate checks the exception's constructor and, optionally, its
    message: without [~substring] any message passes; with it the message must
    contain the given byte substring (the empty string always matches). A whole
    message is asserted with {!raises}, which holds both exceptions and so
    reports a message diff. *)
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
      message-carrying exceptions {!raises} diffs by message rather than by
      rendering (see [exn_message]). *)
end

(** {1:escapes Escape hatches} *)

val fail : ?pos:pos -> string -> 'a
(** [fail msg] raises {!Failure.Check_failure} carrying [msg] as a direct
    message failure. It never returns. *)

val failf : ?pos:pos -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [failf fmt ...] is {!fail} with a [Format] message. It never returns. *)

val skip : ?reason:string -> unit -> 'a
(** [skip ()] raises {!Failure.Skip_test} with [reason]; the runner reports the
    current test as skipped. It never returns. *)
