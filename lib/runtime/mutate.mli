(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Mutation runtime: the mutant catalogue and the arming guard.

    Instrumented code calls {!register} once per source file at module load and
    binds the guard closure it returns; every mutation site in that file
    evaluates the guard with its own file-local index. The catalogue is the
    binary: no side file is written and it cannot be stale. The mutation loop
    reads it ({!catalogue}), builds the reach map with {!next_epoch} and
    {!drain} during a dry run, then forks one child per mutant, which {!arm}s a
    single site. Verdicts live in {!Verdicts}.

    A mutant changes meaning only in a forked child, only when armed, and only
    in a build that asked (guarantee 12). Nothing here touches the disk, reads
    the environment or installs an [at_exit] handler; with no mutant armed the
    guard answers [false] at every site. *)

(** {1:identity Mutant identity} *)

type id = { file : string; line : int; col : int; rewrite : string }
(** The type for mutant identifiers: the source path as recorded at
    instrumentation, the 1-based line and 0-based column of the first byte of
    the mutated expression, and the rewrite's name from {!rewrites}. The
    instrumenter emits at most one site per [(line, col, rewrite)] in a file,
    later colliders (which rewriters such as [[@@deriving]] can produce by
    duplicating locations) being dropped rather than instrumented; {!arm}
    reports a collision as {!Ambiguous} rather than arming one candidate. *)

val rewrites : string list
(** [rewrites] is the closed vocabulary of rewrite names: ["not"], the
    comparisons ["lt"], ["le"], ["gt"], ["ge"], ["eq"], ["neq"], the arithmetic
    ["add"], ["sub"], ["fadd"], ["fsub"], the connectives ["and"], ["or"], and
    the statement deletion ["drop"]. A name outside it is rejected wherever it
    appears. *)

val id_to_string : id -> string
(** [id_to_string id] is the canonical spelling ["lib/calc.ml:9:12:add"]:
    [file], [line], [col], [rewrite], separated by colons. It is what [--arm]
    accepts and verdict files record. *)

val compare_id : id -> id -> int
(** [compare_id a b] orders identifiers by [file], then [line], [col] and
    [rewrite]. {!catalogue} and {!Verdicts} use this order. *)

(** {1:catalogue Sites and registration}

    The contract [ppx_windtrap.mutate] generates against: per instrumented file
    one binding,
    [let ___windtrap_armed___ = Mutate.register ~file ~sites:[| … |]], and at
    every site a guard on [___windtrap_armed___ i], [i] the site's index in
    [sites]. *)

type site = {
  line : int;  (** 1-based line of the mutated expression's first byte. *)
  col : int;  (** 0-based column of the mutated expression's first byte. *)
  rewrite : string;  (** The replacement's name, from {!rewrites}. *)
  before : string;  (** The original expression's source text. *)
  after : string;  (** The armed expression's source text. *)
  dismissed : string option;
      (** [Some reason] when the site carries [[@mutate off]]: never forked,
          never counted in a denominator. *)
}
(** The type for mutation sites, one entry of a file's site table. *)

val register : file:string -> sites:site array -> int -> bool
(** [register ~file ~sites] records [file]'s site table and is its guard
    closure. Site indices are file-local; [sites] is kept, not copied, and must
    not be mutated afterwards. An executable catalogues exactly the instrumented
    files it links.

    The guard, applied to a site index [i], does three things, in this order:
    increments site [i]'s reach count (saturating at [max_int]); if the site's
    epoch differs from the current one, sets it and marks the site for the next
    {!drain}; and is [true] iff site [i] is the armed one. It raises {!Runaway}
    when the armed site's reach count passes the budget given to {!arm}, and
    [Invalid_argument] if [i] is outside [sites]. With nothing armed it
    allocates nothing after a site's first evaluation in an epoch. It is not
    synchronized: a site evaluated from several domains at once can lose hits
    and, at an epoch boundary, its mark, so the reach map is a lower bound.

    Registering [file] again with an equal site table (one source compiled into
    two modules) arms both registrations together and reports each mutant once.
    A registration whose table differs from an earlier one for the same [file]
    is dropped with a warning on [stderr] and its guard is inert.

    Raises [Invalid_argument] if any site has [line < 1], [col < 0], or a
    [rewrite] outside {!rewrites}. *)

type mutant = {
  id : id;  (** This mutant's identifier. *)
  before : string;  (** The original expression's source text. *)
  after : string;  (** The armed expression's source text. *)
  dismissed : string option;  (** The [[@mutate off]] reason, if any. *)
}
(** The type for catalogued mutants: a {!type:site} with its file. *)

val compare_mutant : mutant -> mutant -> int
(** [compare_mutant a b] is {!compare_id} on their identifiers. *)

val catalogue : unit -> mutant list
(** [catalogue ()] is every mutant registered in this executable, ordered by
    {!compare_mutant} and without duplicates, independent of link order.
    Complete only after module initialization. *)

(** {1:arming Arming}

    At most one mutant is armed per process, and arming is loud: an identifier
    that matches several sites, or none of a catalogued file's, is an error
    naming the candidates, never a silent no-op. {!Uncatalogued} is the one case
    that is not a mistake: one identifier is normally handed to every test
    executable of a project, and most of them hold no site of that file. *)

(** The type for arming errors, all recoverable; the loop refuses to start on
    all but {!Uncatalogued}. *)
type arm_error =
  | Malformed of { spec : string; reason : string }
      (** [spec] is not a mutant identifier; [reason] says why. *)
  | Uncatalogued of { id : id }
      (** This executable catalogues no site at all in [id]'s file, or no binary
          does, nothing having been built with the mutation backend. Nothing is
          armed and nothing is concealed. *)
  | Unmatched of { id : id; candidates : mutant list }
      (** [id]'s file is catalogued here but no site matches its line, column
          and rewrite; [candidates] are that file's mutants, never empty. A
          wrong or stale identifier. *)
  | Ambiguous of { id : id; candidates : mutant list }
      (** [id] matches more than one site, one entry per site in [candidates]: a
          rewriter duplicated a location. The remedy is [[@mutate off]] on the
          expression or excluding the file. *)

val pp_arm_error : Format.formatter -> arm_error -> unit
(** [pp_arm_error ppf e] formats a message for [e], naming the candidates and
    the likely fix. *)

val id_of_string : string -> (id, arm_error) result
(** [id_of_string s] parses a mutant identifier in its canonical spelling, and
    is [Error (Malformed _)] if [s] has the wrong shape, a number is missing or
    negative, [line < 1], [col < 0], or the rewrite is not in {!rewrites}.
    [id_of_string (id_to_string id)] is [Ok id] for every [id] this module
    produces. *)

val arm : ?budget:int -> id -> (mutant, arm_error) result
(** [arm id] arms the single mutant [id] names and is that mutant, or
    {!Uncatalogued}, {!Unmatched} or {!Ambiguous} as above. Any previously armed
    mutant is disarmed first, whether or not [id] resolves. From then on the
    guard of that site, in every module registering an equal table for its file,
    answers [true].

    [budget] (default none) caps the armed site's reach count, the guard raising
    {!Runaway} on the evaluation that would pass it. The count is the site's
    since the last {!reset_reach}, not since this call, so a child that
    inherited the dry run's counts calls {!reset_reach} after arming.

    Raises [Invalid_argument] if [budget] is not positive; nothing is disarmed
    then. *)

val disarm : unit -> unit
(** [disarm ()] clears the armed mutant and the runaway budget. *)

val armed : unit -> mutant option
(** [armed ()] is the currently armed mutant, [None] when none is. *)

val armed_hits : unit -> int
(** [armed_hits ()] is how many times the armed site has been evaluated since
    the last {!reset_reach}, summed over every module registering its file, and
    [0] when nothing is armed. *)

exception Runaway of { id : id; hits : int; budget : int }
(** Raised by the guard when the armed site [id] has been evaluated [hits] times
    since the last {!reset_reach} and [hits > budget]. It escapes into the
    mutated program, where the child reduces it to a killed verdict: a mutant
    that turns a terminating loop into a spinning one is killed by its budget,
    not by the clock. The count keeps rising, so swallowing the exception and
    evaluating the site again raises it again. *)

(** {1:reach The reach map}

    Which tests evaluate which mutants is measured during the dry run. The loop
    calls {!drain} then {!next_epoch} when a test starts (whatever accumulated
    in between was evaluated outside any test), {!drain} when it finishes (the
    set of mutants that test evaluated, with hit counts), and {!drain} once more
    at run end. Cost is proportional to the sites the test touched. *)

type reached = {
  mutant : mutant;  (** The mutant evaluated. *)
  hits : int;
      (** How many times its guard was evaluated during the drained window,
          saturating at [max_int]. *)
}
(** The type for one entry of a drained window. *)

val next_epoch : unit -> unit
(** [next_epoch ()] opens a fresh observation window: sites evaluated after it
    mark themselves for the next {!drain}, whether or not they were evaluated
    before. *)

val drain : unit -> reached list
(** [drain ()] is the mutants marked since the previous [drain], ordered by
    {!compare_mutant} and without duplicates, each with the hits accumulated
    since it was marked; it then empties the mark list, so [drain ()]
    immediately after another [drain ()] is [[]]. A mutant registered by two
    modules for one file appears once, hits added. A site marks itself the first
    time it is evaluated in an epoch, so hits a teardown adds to a site the test
    already evaluated are reported nowhere; a budget derived from a drained
    count wants headroom. *)

val reset_reach : unit -> unit
(** [reset_reach ()] zeroes every site's reach count, empties the mark list and
    opens a fresh epoch. It does not disarm. *)
