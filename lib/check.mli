(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The assertion verbs.

    Twenty-three verbs and the {!Exn} predicates, each verb raising one structured
    failure: a failing verb constructs a single {!Failure.t} — a typed kind, an
    optional location, the [?msg] annotation when given — and raises
    {!Failure.Check_failure}. Verbs never print, never diff, and never touch run
    state: renderers project failure data into reports, computing diffs from the
    rendered values stored in the payload.

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

val mem : ?pos:pos -> ?msg:string -> 'a testable -> 'a -> 'a list -> unit
(** [mem t x xs] is [()] iff [xs] has an element equal to [x] under [t].
    Otherwise it raises {!Failure.Check_failure} with a {!Failure.Predicate}
    payload whose claim names [x] and whose value is [xs], both rendered by
    [t]'s printer — the data an [is_true (List.mem x xs)] would have thrown
    away. Elements are rendered only on failure.

    Membership over bytes is {!contains}; this is membership over a witnessed
    element type, so the two cannot share a payload. *)

(** {1:predicates Predicates} *)

val satisfies :
  ?pos:pos -> ?msg:string -> 'a testable -> ('a -> bool) -> 'a -> unit
(** [satisfies t pred v] is [()] iff [pred v]. Otherwise it raises
    {!Failure.Check_failure} with a {!Failure.Predicate} payload whose claim is
    ["value satisfying the predicate"], carrying [v] rendered by [t]'s printer —
    [t]'s equality is not consulted. [pred] must be total; it runs on every
    call, the printer only on failure. Use [?msg] to name the predicate:
    [satisfies ~msg:"positive" Testable.int (fun n -> n > 0) n]. *)

(** {1:ordering Ordering}

    The verbs [is_true (n > 0)] stands in for. A comparison consumes both
    numbers and yields a boolean, so its failure can only say
    [expected true / actual false]; these keep the bound as the claim and the
    value as the value, and read [expected greater than 0 / actual 0].

    The ordering comes from the witness ({!Testable.with_order}), which is what
    keeps the call shorter than the [is_true] it replaces. Every base-type
    witness carries one. A witness that does not raises [Invalid_argument] at
    the call — a programmer error, reported where it is made, inside the running
    test's boundary. *)

val greater : ?pos:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** [greater t ~than:bound v] is [()] iff [v] is strictly greater than [bound]
    under [t]'s ordering. Otherwise it raises {!Failure.Check_failure} with a
    {!Failure.Predicate} payload whose claim names [bound] and whose value is
    [v], both rendered by [t]'s printer. *)

val greater_equal :
  ?pos:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** [greater_equal t ~than:bound v] is {!greater} with equality allowed. *)

val less : ?pos:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** [less t ~than:bound v] is [()] iff [v] is strictly less than [bound] under
    [t]'s ordering. *)

val less_equal : ?pos:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** [less_equal t ~than:bound v] is {!less} with equality allowed. *)

(** {1:options Options}

    The shape assertions, for when the value is not wanted. They take the same
    optional printer the unwrapping verbs do rather than a {!Testable.t}: a
    witness carries an equality these never consult, and demanding one for a
    type the assertion does not inspect is what drives call sites to
    [equal (option pass) None x]. *)

val is_none : ?pos:pos -> ?msg:string -> ?pp:'a printer -> 'a option -> unit
(** [is_none o] is [()] iff [o] is [None]. On [Some v] it raises
    {!Failure.Check_failure} comparing [None] against [Some <v>], with [v]
    rendered by [pp] when given and as [<abstract>] otherwise; the printer runs
    only on failure. *)

val is_some : ?pos:pos -> ?msg:string -> 'a option -> unit
(** [is_some o] is [()] iff [o] is [Some _] — {!require_some} for callers that
    want the assertion and not the value, instead of discarding it. There is no
    [?pp]: the failing side is [None], which has nothing to render. *)

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

    On [None] it raises {!Failure.Check_failure} with a {!Failure.Predicate}
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
    message: with neither constraint any message passes; [~substring] requires
    the message to contain the given byte substring (the empty string always
    matches); [~exact] requires exact equality. The constraints are mutually
    exclusive — supplying both is a programmer error that raises
    [Invalid_argument] as soon as the predicate is built, before it examines any
    exception. *)
module Exn : sig
  val invalid_arg : ?substring:string -> ?exact:string -> exn -> bool
  (** [invalid_arg e] is [true] iff [e] is [Invalid_argument m] and [m]
      satisfies the constraint, if any. *)

  val failure : ?substring:string -> ?exact:string -> exn -> bool
  (** [failure e] is [true] iff [e] is [Failure m] and [m] satisfies the
      constraint, if any. *)

  val sys_error : ?substring:string -> ?exact:string -> exn -> bool
  (** [sys_error e] is [true] iff [e] is [Sys_error m] and [m] satisfies the
      constraint, if any. Completes the set: these three are exactly the
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
