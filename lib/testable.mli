(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The assertion-side witness: printing, equality, and an optional order.

    An ['a t] tells the equality assertions how to compare values of type ['a]
    and how to render them into failure payloads, and tells the ordering
    assertions — when it carries an order — how to rank them. Every function
    must be total: they run on every assertion, and a raising printer or
    equality turns a report into a crash.

    Build witnesses with {!make} (equality is required — polymorphic equality is
    the explicitly named {!structural}) and give them an order with
    {!with_compare}; compose them with the container instances and {!contramap},
    and ignore components with {!pass}. Diffing is not here: renderers compute
    diffs from the printed values, so every type gets highlighted diffs from its
    [pp] alone. Generation is not here either: random generation lives in [Gen],
    the property-side witness. The two never merge. *)

(** {1:types Types} *)

type 'a t
(** The type for witnesses over values of type ['a]: a printer and an equality,
    both total, and optionally a total order. The equality is applied [expected]
    first, [actual] second — see {!make} before writing an asymmetric one. The
    order, when present, is what the ordering verbs consult; the equality verbs
    never read it, and the ordering verbs never read the equality. *)

(** {1:constructors Constructors} *)

val make :
  pp:(Format.formatter -> 'a -> unit) -> equal:('a -> 'a -> bool) -> 'a t
(** [make ~pp ~equal] is a witness comparing with [equal], printing with [pp],
    and carrying no order — see {!with_compare}. Both functions must be total
    over the values the tests exercise. A module with the conventional trio is
    [make ~pp:M.pp ~equal:M.equal].

    {b [equal] receives [expected] first, [actual] second.} The equality verbs
    apply it in their own argument order ([Check.equal t expected actual] calls
    [equal expected actual]), so an asymmetric [equal] treats the two sides
    differently — and the side it favours is fixed by the caller's spelling, not
    by anything visible in the witness.

    Prefer a symmetric [equal], especially for tolerances. A relative tolerance
    scaled by its {e second} argument scales by the computed value, so a wrong
    answer that is large buys itself a proportionally large tolerance and the
    assertion quietly stops testing anything. {!float_rel} is the worked
    example: it scales by [Float.max (abs_float a) (abs_float b)], which is
    symmetric by construction. Wrapping a library's [allclose] — most of which
    are asymmetric in exactly this way — needs the same treatment. *)

val with_compare : ('a -> 'a -> int) -> 'a t -> 'a t
(** [with_compare compare w] is [w] ordered by [compare], replacing any order it
    had. [compare] follows [Stdlib.compare]'s contract — negative when its first
    argument ranks below the second, zero when they rank the same, positive when
    above — and must be total. A module with the conventional trio is
    [make ~pp:M.pp ~equal:M.equal |> with_compare M.compare].

    The order is what the ordering verbs consult, and it is never reconciled
    with [w]'s equality: a tolerance equality and an exact order coexist in the
    float witnesses. A witness without one makes every ordering verb raise
    [Invalid_argument] naming this function. *)

val structural : pp:(Format.formatter -> 'a -> unit) -> 'a t
(** [structural ~pp] is [make ~pp] with polymorphic structural equality
    [Stdlib.( = )] and polymorphic structural order [Stdlib.compare] — the
    choice is explicit in the name. Both raise on functional values and loop on
    cyclic ones; give such types a real [equal] via {!make}. *)

val of_equal : ('a -> 'a -> bool) -> 'a t
(** [of_equal equal] is a witness comparing with [equal], printing every value
    as ["<abstract>"], and carrying no order.

    {b Warning.} The footgun is deliberate and visible: failures involving this
    witness show [<abstract>] on both sides and cannot show a diff. Reach for
    {!make} as soon as the type has any printable rendering. *)

val contramap : ('a -> 'b) -> 'b t -> 'a t
(** [contramap f w] compares, orders and prints values of type ['a] through
    their image under [f]: equality is [w]'s on [f a] and [f b], the order —
    when [w] has one — is [w]'s on the images too, and failures render [f a],
    not [a]. So [contramap String.length int] orders strings by length.

    {[
    type user = { id : int; name : string }

    let user_id = Testable.(contramap (fun u -> u.id) int)
    ]} *)

val pass : 'a t
(** [pass] considers all values equal, prints ["<pass>"], and carries no order.
    Use it to ignore a component of a composed witness, e.g. [pair string pass].
*)

(** {1:observers Observers} *)

val pp : 'a t -> Format.formatter -> 'a -> unit
(** [pp w ppf v] formats [v] with [w]'s printer. *)

val equal : 'a t -> 'a -> 'a -> bool
(** [equal w a b] is [true] iff [w]'s equality considers [a] and [b] equal. The
    verbs pass [expected] as [a] and [actual] as [b]; a witness whose equality
    is asymmetric therefore treats the two sides differently (see {!make}). *)

val compare : 'a t -> ('a -> 'a -> int) option
(** [compare w] is [w]'s order when it carries one — the base-type instances,
    {!structural}, anything passed through {!with_compare}, and a {!contramap}
    of any of those — and [None] otherwise. The ordering verbs read it here and
    raise [Invalid_argument] on [None]. *)

val to_string : 'a t -> 'a -> string
(** [to_string w v] is [v] printed with [w]'s printer as a string — the
    rendering assertion verbs store in failure payloads. *)

(** {1:instances Instances}

    Base-type witnesses print source-like renderings ([%S] for strings, [%C] for
    chars, [%g] for the tolerance float witnesses), so failure payloads read
    like OCaml values. Each carries its module's order ([Int.compare],
    [String.compare], …). *)

val unit : unit t
val bool : bool t
val char : char t

val string : string t
(** [string] prints with [%S]: quoted, with escapes, on one line. *)

val text : string t
(** [text] is {!string} printed verbatim: no quotes, no escapes, newlines kept.
    Because the rendering spans lines, failures diff it line by line — a unified
    diff naming the changed lines — where {!string} would show two escaped
    one-liners with the difference buried in [\\n] soup. It is the witness for
    multi-line text: rendered output, serialized documents, logs.

    Prefer {!string} for single-line values, where the quotes are what
    distinguish [""], [" "] and ["\t"]. Trailing whitespace stays visible under
    [text] regardless: the diff marks it. *)

val bytes : bytes t
val int : int t
val int32 : int32 t
val int64 : int64 t
val nativeint : nativeint t

(** The three float witnesses share one IEEE 754 story. Under {!float} and
    {!float_rel}, NaN is equal to nothing, itself included; under
    {!float_exact}, every NaN equals every NaN — the only witness that can
    assert a NaN result. Under all three an infinity is equal only to an
    infinity of the same sign: no tolerance bridges an infinite and a finite
    value. [0.] and [-0.] are equal under the tolerance witnesses and distinct
    under {!float_exact}. All three order with [Float.compare], so NaN sorts
    below every float and tolerance plays no part in the order. *)

val float_exact : float t
(** [float_exact] compares floats bit for bit, with all NaNs identified (see
    above). Failures print the shortest decimal that round-trips to the exact
    value ([0.1 +. 0.2] prints ["0.30000000000000004"]; [-0.] prints ["-0"]), so
    unequal floats never render identically. *)

val float : float -> float t
(** [float eps] compares with absolute tolerance: [a] and [b] are equal when
    [a = b] or [|a -. b| <= eps].

    Raises [Invalid_argument] if [eps] is not strictly positive (NaN included):
    such an [eps] is exact equality in a tolerance's syntax, and exactness is
    spelled {!float_exact}. *)

val float_rel : rel:float -> abs:float -> float t
(** [float_rel ~rel ~abs] compares with combined tolerance: [a] and [b] are
    equal when [a = b], when their absolute difference is within [abs]
    (near-zero values), or when it is within
    [rel *. Float.max (abs_float a) (abs_float b)] (large values, and symmetric
    by construction).

    Raises [Invalid_argument] if either bound is negative or NaN, or if both are
    zero. One zero bound is meaningful — [~rel:0.] is a purely absolute
    tolerance, [~abs:0.] a purely relative one — but both zero is exact equality
    in a tolerance's syntax, and exactness is spelled {!float_exact}. *)

(** {1:containers Containers}

    No container witness carries an order, whatever its components carry: an
    option or a list admits several ([None] first or last, lexicographic or by
    length) and a guessed one would be accepted silently. Spell the one you mean
    with {!with_compare}. *)

val option : 'a t -> 'a option t
val result : 'a t -> 'e t -> ('a, 'e) result t
val either : 'a t -> 'b t -> ('a, 'b) Either.t t
val list : 'a t -> 'a list t
val array : 'a t -> 'a array t

val slist : 'a t -> ('a -> 'a -> int) -> 'a list t
(** [slist w cmp] is [contramap (List.sort cmp) (list w)]: lists compared, and
    printed, as multisets. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
