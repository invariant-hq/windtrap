(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(** Random value generators with integrated shrinking and printing.

    An ['a t] couples three inseparable concerns: drawing a value from a
    {!Seed.state}, the lazy tree of shrink candidates for the drawn value, and
    how values print in counterexamples. There is no user-written shrinker
    anywhere: every generator shrinks, and shrink candidates satisfy the same
    constraints as generated values — an {!int_range} candidate stays in bounds,
    a {!such_that} candidate satisfies its predicate.

    This module, {!Seed} and {!Shrink_tree} ship as [windtrap.gen] — a
    standalone, stdlib-only library, usable without the runner. [windtrap]
    depends on it and re-exports this module as [Windtrap.Gen], so a property
    suite needs nothing beyond [windtrap] in its stanza.

    Start from primitives ({!int}, {!float}, {!string}, ...), combine with
    containers ({!list}, {!pair}, ...) and choice ({!of_list}, {!one_of},
    {!frequency}), transform with {!map}, {!bind}, or the binding operators, and
    attach a printer with {!with_pp}.

    {b Printing.} A counterexample renders with the generator's printer, and
    printers derive by composition — the law is:
    {e a composite generator prints exactly when all of its components print}.

    {b The law stops at {!map} and {!bind}.} They cannot derive a printer,
    because none for the result type can be inferred from the argument's — and
    that is not a corner case: [let+], [and+] and [let*] {e are} [map] and
    [bind], so the idiomatic spelling

    {[
      let* shape = gen_shape in
      let+ a = gen_f32 shape and+ b = gen_f32 shape in
      (a, b)
    ]}

    is printerless however well its components print. Expect to attach
    {!with_pp} at the top of any generator written this way; the alternative is
    to keep the composition inside the deriving combinators ({!pair}, {!list},
    {!one_of}, ...), which do carry printers through.

    Primitives print out of the box — {!string_of} and {!bytes_of} always print,
    quoted, whatever their character generator; {!list}, {!array}, {!option},
    {!result}, {!pair}, {!triple}, {!quad}, {!one_of}, and {!frequency} derive
    their printer from their components' printers; {!map}, {!bind}, and {!sized}
    produce printerless generators as above, and {!constant}, {!pure}, and
    {!of_list} print only when given [?pp], because their values are arbitrary.
    Those two are worth passing: they are the leaves that most often sit under a
    deriving combinator, and one printerless leaf forfeits the derived printer
    of the whole composition. {!such_that} keeps its generator's printer. A
    printerless counterexample renders as [<no printer>], and the failure
    report names the remedy ({!with_pp}) once, under the counterexample.

    {b Validation.} Generator constructors never raise: malformed arguments
    ([one_of []], [int_range 3 1]) are reported by raising [Invalid_argument]
    when the generator first samples, inside the running test's exception
    boundary — a test list that constructs is a test list that runs.

    Callbacks passed to {!map}, {!bind}, {!such_that}, and generator-valued
    functions given to {!sized} must be pure: the shrink search runs them —
    memoized, at most once per tree node — when it forces candidates. *)

(** {1:generators Generators} *)

type 'a t
(** The type for generators of values of type ['a]: generation from a
    {!Seed.state}, integrated shrinking, and counterexample printing,
    inseparable. *)

(** {1:numeric Numeric generators} *)

val int : int t
(** [int] generates a uniformly distributed integer over the full [int] range.
    Candidates shrink toward [0]. *)

val nat : int t
(** [nat] generates a natural number below [10_000], biased toward small values:
    50% below [10], 25% below [100], 20% below [1_000], 5% below [10_000].
    Candidates shrink toward [0]. Use it for sizes, lengths, and counts. *)

val small_int : int t
(** [small_int] generates an integer whose magnitude follows {!nat} — inside
    \[[-9_999];[9_999]\], biased toward small magnitudes, either sign.
    Candidates shrink toward [0]. Use it instead of {!int} when full-range
    values would overflow the arithmetic under test. *)

val int_range : int -> int -> int t
(** [int_range low high] generates an integer in \[[low];[high]\], uniformly.
    Candidates shrink toward the in-range point closest to [0] and stay in
    range.

    Sampling raises [Invalid_argument] if [high < low]. *)

val int32 : int32 t
(** [int32] generates a uniformly distributed [int32] over the full 32-bit
    range. Candidates shrink toward [0l]. *)

val int64 : int64 t
(** [int64] generates a uniformly distributed [int64] over the full 64-bit
    range. Candidates shrink toward [0L]. *)

val float : float t
(** [float] generates a finite float by drawing uniform IEEE 754 bit patterns
    and rejecting non-finite ones, so magnitudes spread over the full exponent
    range, including subnormals. Candidates shrink toward [0.]. *)

val float_any : float t
(** [float_any] is like {!float} without the finiteness rejection: uniform over
    all IEEE 754 bit patterns, including NaNs, infinities, and signed zeros.
    Candidates shrink toward [0.]. *)

val float_range : float -> float -> float t
(** [float_range low high] generates a float in \[[low];[high]\], uniformly.
    Candidates shrink toward the in-range point closest to [0.] and stay in
    range.

    Sampling raises [Invalid_argument] if [high < low], if either bound is not
    finite, or if [high -. low] overflows to infinity. *)

(** {1:base Unit, booleans, characters, strings} *)

val unit : unit t
(** [unit] generates [()], with no shrink candidates, and prints [()]. It is not
    [pure ()]: {!pure} without [?pp] carries no printer, so a deriving
    composition over it — a variant arm for a nullary operation, say — would
    forfeit its own printer too. *)

val bool : bool t
(** [bool] generates [true] or [false] with equal probability. [true] shrinks to
    [false]. *)

val char : char t
(** [char] generates a uniformly distributed byte: each of the 256 characters —
    the NUL byte ['\x00'] and bytes above 127 included — appears with
    probability 1/256. Candidates shrink toward ['a']. Use {!char_range} or
    {!of_list} for character subsets. *)

val char_range : char -> char -> char t
(** [char_range low high] generates a character in \[[low];[high]\] (byte
    order), uniformly. Candidates shrink toward the in-range character closest
    to ['a'] and stay in range: [char_range 'a' 'z'] shrinks toward ['a'],
    [char_range 'A' 'Z'] toward ['Z'], and [char_range '0' '9'] toward ['9'].

    Sampling raises [Invalid_argument] if [high < low]. *)

val string : string t
(** [string] is [string_of char]: a string whose length follows {!nat}'s
    distribution and whose characters follow {!char} — NUL and non-ASCII bytes
    included. Shrinking removes chunks of characters — the empty string is the
    first candidate — then shrinks characters individually toward ['a']. Use
    {!string_of} to control the length or character distribution.

    {b Note.} The natural spelling
    [string : ?size:int t -> ?char:char t -> string t] is unavailable: optional
    arguments on a value are unerasable (warning 16), so the knobs live on
    {!string_of} instead, aligned with {!list}. *)

val string_of : ?size:int t -> char t -> string t
(** [string_of ?size char] generates a string whose length follows the size
    generator, [size] defaulting to {!nat}, and whose characters are drawn from
    [char]. The result always prints, as a quoted string, even when [char] is
    printerless.

    Shrinking follows {!list}'s rule. With the default size, candidates first
    shrink the structure — the empty string, then removal of contiguous chunks —
    and then shrink characters individually with [char]'s own candidates. With
    an explicit [size], lengths follow [size]'s shrink candidates — a length
    constraint such as [~size:(int_range 2 5)] holds for every candidate — and
    characters shrink individually.

    Sampling raises [Invalid_argument] if [size] produces a negative length. *)

val bytes : bytes t
(** [bytes] is [bytes_of char]: {!string} converted to [bytes] — same length
    distribution, uniform bytes, same shrinking. *)

val bytes_of : ?size:int t -> char t -> bytes t
(** [bytes_of ?size char] is [string_of ?size char] converted to [bytes]. *)

(** {1:containers Containers} *)

val list : ?size:int t -> 'a t -> 'a list t
(** [list gen] generates a list of [gen] values whose length follows the size
    generator, [size] defaulting to {!nat}.

    Shrinking depends on [size]. With the default, candidates first shrink the
    structure — the empty list, then removal of contiguous chunks of descending
    power-of-two length — and then shrink elements individually, left to right.
    With an explicit [size], list lengths follow [size]'s own shrink candidates
    — so a length constraint such as [~size:(int_range 2 5)] holds for every
    candidate — and elements shrink individually.

    Sampling raises [Invalid_argument] if [size] produces a negative length. *)

val array : ?size:int t -> 'a t -> 'a array t
(** [array ?size gen] is [list ?size gen] converted to an array. *)

val option : 'a t -> 'a option t
(** [option gen] generates [None] with probability 0.15 and [Some] of a [gen]
    value otherwise. The first candidate of every [Some] is [None]; the payload
    then shrinks with [gen]. *)

val result : 'a t -> 'e t -> ('a, 'e) result t
(** [result ok err] generates [Ok] of an [ok] value with probability 0.75 and
    [Error] of an [err] value otherwise. Payloads shrink with their generator; a
    candidate never crosses constructors. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
(** [pair a b] generates both components. Candidates shrink the left component
    first, then the right (see {!Shrink_tree.pair}). *)

val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
(** [triple a b c] is like {!pair} for three components, shrinking
    left-to-right. *)

val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
(** [quad a b c d] is like {!pair} for four components, shrinking left-to-right.
*)

(** {1:choice Choice and structure} *)

val constant : ?pp:(Format.formatter -> 'a -> unit) -> 'a -> 'a t
(** [constant v] always generates [v], with no shrink candidates. Its values are
    arbitrary, so no printer can be inferred — supply one with [?pp] and the
    generator prints like a primitive, on its own and through the printers that
    {!list}, {!pair}, {!one_of}, ... derive. Without [?pp] it prints nothing,
    and a composition built over it can only be rescued by {!with_pp} at the
    top. *)

val pure : ?pp:(Format.formatter -> 'a -> unit) -> 'a -> 'a t
(** [pure] is {!constant}. *)

val of_list : ?pp:(Format.formatter -> 'a -> unit) -> 'a list -> 'a t
(** [of_list values] generates a value of [values], each with equal probability.
    Candidates shrink toward the head: the first candidate of any value is the
    head of [values], then values at intermediate positions — order [values]
    with the simplest value first.

    Like {!constant}, its values are arbitrary and it takes [?pp] for the same
    reason. Without one, counterexamples render as [<no printer>].

    Sampling raises [Invalid_argument] if [values] is empty. *)

val one_of : 'a t list -> 'a t
(** [one_of gens] picks one generator from [gens] uniformly and generates with
    it. The choice shrinks toward earlier generators — a candidate may
    re-generate from an earlier branch using the same random capital, and a
    branch whose re-generation is rejected is skipped — and the chosen value
    shrinks with its own generator.

    When every generator of [gens] prints, the choice prints and counterexamples
    render as values (branches generate the same type, so their printers are
    expected to agree; the first branch's is used). Otherwise counterexamples
    render as [<no printer>].

    Sampling raises [Invalid_argument] if [gens] is empty. *)

val frequency : (int * 'a t) list -> 'a t
(** [frequency weighted] picks a generator with probability proportional to its
    weight and generates with it. The choice itself does not shrink; the chosen
    value shrinks with its generator. Printing derives as in {!one_of}: when
    every branch prints, counterexamples render as values; otherwise they render
    as [<no printer>].

    Sampling raises [Invalid_argument] if [weighted] is empty, if any weight is
    negative, or if the weights sum to less than [1]. *)

val sized : (int -> 'a t) -> 'a t
(** [sized f] draws a size with {!nat} and generates with [f size]. Shrinking
    first re-generates at smaller sizes, then shrinks the generated value — the
    usual way to bound recursive generators. Like {!bind}, the result has no
    printer. [f] must be pure. *)

val such_that : ?max_tries:int -> ('a -> bool) -> 'a t -> 'a t
(** [such_that p gen] generates [gen] values satisfying [p], re-sampling up to
    [max_tries] times (default [100]); shrink candidates are filtered by [p], so
    every candidate satisfies it. The result keeps [gen]'s printer. If no draw
    satisfies [p], sampling raises {!Rejected} and the property engine counts
    the case as a discard.

    [p] is for rare, cheap conditions; when the constraint is structural, build
    a generator that satisfies it by construction instead.

    Sampling raises [Invalid_argument] if [max_tries < 1]. *)

(** {1:composition Composition} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f gen] generates [f v] for [v] generated by [gen], shrinking wherever
    [gen] shrinks. The result has no printer; its counterexamples render as
    [<no printer>] (see {!with_pp}). [f] must be pure: the shrink search applies
    it, memoized, when forcing candidates. *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** [bind gen f] generates [v] with [gen], then generates with [f v]. Candidates
    first shrink [v] — re-generating with [f] on the same random capital — then
    shrink the inner value; a candidate whose re-generation is rejected by a
    {!such_that} is skipped. The result has no printer. [f] must be pure. *)

val with_pp : (Format.formatter -> 'a -> unit) -> 'a t -> 'a t
(** [with_pp pp gen] is [gen] printing with [pp] — the same printer type the
    assertion vocabulary uses, so one printer feeds both worlds. *)

val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
(** [let+ x = gen in e] is [map (fun x -> e) gen]. *)

val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
(** [gen1 and+ gen2] is [pair gen1 gen2]. *)

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
(** [let* x = gen in e] is [bind gen (fun x -> e)]. *)

(** {1:engine Engine interface}

    Low-level API for the property engine and windtrap's own tests. The facade
    re-exports it with the rest of this module — public, but advanced: beyond
    the frozen value stream ({!Seed}) it carries no stability promise. *)

val list_exact : ?keep:('a list -> bool list) -> int -> 'a t -> 'a list t
(** [list_exact n gen] generates a list of exactly [n] [gen] values and shrinks
    it with {!list}'s default-size move set — the empty list, then removal of
    contiguous chunks of descending power-of-two length, then elements
    individually, left to right. [n] fixes the {e drawn} length only: no
    candidate is longer than the root, every shorter length down to the empty
    list is reachable, and reducing an element preserves the length it was
    given. Use it where [list ~size:(constant n)] would fix the same drawn
    length but offer element-wise shrinking only, with no move that shortens the
    list.

    [keep values] is a mask as long as [values]; an element flagged [false] is
    dropped. It runs on the drawn values {e before} the tree is assembled, so a
    dropped element contributes no subtree: no candidate anywhere holds it, or a
    value it would have shrunk to, and the root is a fixed point of the mask
    rather than [n] elements long. It then runs on every node of the assembled
    tree, so an element that a deletion or a reduction elsewhere invalidates is
    dropped in the same candidate — which can leave a candidate shorter than the
    structural move that produced it. That is how a {e state-dependent}
    well-formedness condition (the calls of a command sequence a precondition
    admits, the unique keys of an association list) holds at every node, where
    {!such_that} would drop a rejected candidate with its whole subtree and
    [assume] would spend the property engine's discard budget.

    [keep] must be total and idempotent.

    - {b Total.} It is handed the drawn list, the sublists the search reaches,
      and the empty list. An exception escapes {!sample} at the root; on a
      candidate it is memoized on that node and swallowed by the engine's
      candidate loop, which abandons the remaining siblings and reports as
      converged a search that stopped early.
    - {b Idempotent.} The second pass rewrites the root along with the
      candidates, so a mask that drops from a list it has already accepted drops
      from the root twice.

    The printer derives from [gen]'s, exactly as {!list}'s does.

    Sampling raises [Invalid_argument] if [n < 0], or if [keep] returns a mask
    whose length is not that of the list it was given — at sample time for the
    drawn list, at forcing time for a candidate. *)

exception Rejected
(** Raised by {!sample} when a {!such_that} filter exhausts its [max_tries]
    budget — a generation-time discard. Forcing shrink candidates never raises
    it: a candidate whose re-generation is rejected is skipped and the search
    continues with its siblings. *)

val sample : 'a t -> Seed.state -> 'a Shrink_tree.t
(** [sample gen state] draws one value and its shrink tree from [state]. The
    result is a pure function of [state] and [gen]. Only the root is drawn
    eagerly; candidates are forced — memoized, user callbacks included — by
    traversing the tree.

    Raises {!Rejected} on a generation-time discard and [Invalid_argument] on
    malformed generator arguments. Forcing candidates can raise
    [Invalid_argument] too, but never {!Rejected}: rejected candidates are
    skipped. *)

val prints : 'a t -> bool
(** [prints gen] is [true] iff [gen] carries a printer, attached with {!with_pp}
    or derived from its components — that is, iff {!render} yields the value
    rather than [<no printer>]. The property engine records it on a failure so
    the report can name the remedy ({!with_pp}) once, instead of each
    printerless rendering carrying its own advice. *)

val render : 'a t -> 'a -> string
(** [render gen v] is the counterexample text for [v]: the attached or derived
    printer's output when {!prints} holds, and [<no printer>] otherwise. Never
    raises: a printer that raises renders as [<printer raised ...>]. *)

val render_value : 'a t -> 'a -> string option
(** [render_value gen v] is [Some text], [v] rendered by [gen]'s attached or
    derived printer, or [None] when [gen] has none — used where the caller
    supplies its own placeholder, as the engine does for [~examples] values.
    Like {!render}, a raising printer yields [<printer raised ...>] rather than
    an exception. *)
