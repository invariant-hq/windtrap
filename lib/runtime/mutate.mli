(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Mutation runtime: the mutant catalogue and the arming guard.

    Instrumented code (produced by [ppx_windtrap.mutate]) calls {!register} once
    per source file at module load time and binds the guard closure it returns;
    every mutation site in that file evaluates the guard with its own file-local
    index. The catalogue therefore {e is} the binary: no side file is written,
    and a catalogue can never be stale with respect to the code it describes.

    The mutation loop drives this module from the parent process: it reads the
    catalogue ({!catalogue}), builds the reach map from {!next_epoch} and
    {!drain} while the dry run executes, then forks one child per mutant, which
    {!arm}s a single site and runs the tests that reached it. What the run made
    of each mutant is not recorded here: verdicts and their file format live
    beside this module in {!Verdicts}, written by the loop and read by
    [windtrap mutants]; instrumented code never holds one.

    A mutant changes meaning only in a forked child, only when armed, and only
    in a build that asked for it. Nothing here touches the disk, nothing reads
    the environment, nothing installs an [at_exit] handler, and with no mutant
    armed the guard's answer is [false] at every site. Which mutants a run tests
    and which one a process arms are the core's decisions, handed to {!arm} and
    read off {!catalogue}. *)

(** {1:identity Mutant identity}

    A mutant is named by the source position of the expression it rewrites and
    by the rewrite applied there. That name is what travels: through the core's
    arming knob (see {!arm_variable}), through verdict files, and into the
    report. *)

type id = { file : string; line : int; col : int; rewrite : string }
(** The type for mutant identifiers. [file] is the source path as recorded at
    instrumentation, [line] (1-based) and [col] (0-based) the position of the
    first byte of the mutated expression, and [rewrite] the name of the
    replacement, drawn from {!rewrites}.

    The instrumenter guarantees that at most one site per file carries a given
    [line], [col] and [rewrite] — later colliders (which rewriters such as
    [[@@deriving]] can produce by duplicating locations) are dropped rather than
    instrumented. This module does not trust that guarantee: {!arm} reports a
    collision as {!Ambiguous} rather than arming one of the candidates, and a
    site it cannot name unambiguously is never armed and therefore never
    receives a verdict. *)

val rewrites : string list
(** [rewrites] is the closed vocabulary of rewrite names: ["not"], the
    comparisons ["lt"], ["le"], ["gt"], ["ge"], ["eq"], ["neq"], the arithmetic
    ["add"], ["sub"], ["fadd"], ["fsub"], the connectives ["and"], ["or"], and
    the statement deletion ["drop"]. A name outside this list is rejected
    wherever it appears — in a site table, in an identifier handed to {!arm}, in
    a verdict file — because a rewrite nobody can render is a report nobody can
    act on. *)

val id_to_string : id -> string
(** [id_to_string id] is [id] in the canonical spelling ["lib/calc.ml:9:12:add"]
    — [file], [line], [col], [rewrite], separated by colons. This is the
    spelling the core's arming knob accepts and verdict files record. *)

val compare_id : id -> id -> int
(** [compare_id a b] orders identifiers lexicographically by [file], then
    [line], then [col], then [rewrite]. This is the order {!catalogue} uses, and
    the order {!Verdicts} serializes verdict collections in. *)

(** {1:catalogue Sites and registration}

    The functions of this section are the contract [ppx_windtrap.mutate]
    generates against; user code and the windtrap core never call them. The
    generated code per instrumented file is {b exactly one binding} —
    [let ___windtrap_armed___ = Mutate.register ~file ~sites:[| … |]] — and
    every site in that file expands to a guard on [___windtrap_armed___ i],
    where [i] is the site's index in [sites]. The instrumenter names no array
    and allocates nothing: the fewer literals it emits, the fewer ways it can be
    wrong. *)

type site = {
  line : int;  (** 1-based line of the mutated expression's first byte. *)
  col : int;  (** 0-based column of the mutated expression's first byte. *)
  rewrite : string;  (** The replacement's name, from {!rewrites}. *)
  before : string;  (** The original expression's source text. *)
  after : string;  (** The armed expression's source text. *)
  dismissed : string option;
      (** [Some reason] when the site carries [[@mutate off]]: the loop skips
          the site, and no report counts it in a denominator. [None] otherwise.
      *)
}
(** The type for mutation sites: one entry of a file's site table. *)

val register : file:string -> sites:site array -> int -> bool
(** [register ~file ~sites] records [file]'s site table and is that file's guard
    closure. The reach and epoch arrays are allocated here, not by the caller,
    and are captured by the closure — so {b site indices are file-local}. A
    single global array indexed by an absolute identifier would be indexed
    before every file had registered (link order decides) and would read out of
    bounds; there is no absolute index anywhere in this interface.

    [sites] is kept, not copied — {!catalogue} and {!drain} read it — so the
    generated literal must not be mutated afterwards. Registration happens at
    module load, and the linker drops modules the binary never references, so an
    executable catalogues exactly the instrumented files it links.

    The guard, applied to a site index [i], does three things, in this order:

    + increments site [i]'s reach count (saturating at [max_int]);
    + if site [i]'s epoch differs from the current epoch, sets it and marks the
      site for the next {!drain} — this is how the reach map is built, at one
      epoch compare per evaluated site;
    + is [true] iff site [i] is the armed one.

    It raises {!Runaway} when the armed site's reach count passes the budget
    given to {!arm}, and [Invalid_argument] if [i] is outside [sites] (an
    instrumenter bug — the index is a literal the instrumenter emits beside the
    table). With nothing armed it allocates nothing after each site's first
    evaluation in an epoch, and its answer is [false].

    The guard is not synchronized. A site evaluated from several domains at once
    can lose hits and, at an epoch boundary, its mark — an under-report, so a
    window can miss a mutant but can never invent one. Windtrap spawns no
    domains; code under test that does gets a reach map that is a lower bound.

    Registering [file] again with a site table equal to the previous one is
    allowed (the same source compiled into two modules): both registrations arm
    together, the catalogue counts the file's mutants once, and {!drain} reports
    each mutant once. A registration whose table {e differs} from an earlier one
    for the same [file] means the executable links two incompatible
    instrumentations of one source — arming it would be meaningless, so it is
    dropped with a warning on [stderr] and the returned guard is inert ([false]
    at every index). It is not an exception because [register] runs at module
    load inside the user's program.

    Raises [Invalid_argument] if any site has [line < 1], [col < 0], or a
    [rewrite] outside {!rewrites} — a malformed table can only come from a
    broken instrumenter, and fails fast. *)

type mutant = {
  id : id;  (** This mutant's identifier. *)
  before : string;  (** The original expression's source text. *)
  after : string;  (** The armed expression's source text. *)
  dismissed : string option;  (** The [[@mutate off]] reason, if any. *)
}
(** The type for catalogued mutants: a {!type:site} with its file, as everything
    outside the instrumenter sees it. *)

val compare_mutant : mutant -> mutant -> int
(** [compare_mutant a b] is {!compare_id} on their identifiers, which name the
    same rewrite of the same expression exactly when they are equal. *)

val catalogue : unit -> mutant list
(** [catalogue ()] is every mutant registered in this executable, ordered by
    {!compare_mutant} and without duplicates. The order does not depend on link
    order, so two runs of the same binary enumerate mutants identically. This is
    the population the loop iterates: it is complete only after module
    initialization, since registration happens at module load and the linker
    drops modules the binary never references. *)

(** {1:arming Arming}

    At most one mutant is armed per process. Arming is loud: an identifier that
    matches more than one site, or that names a site of a file this executable
    {e does} catalogue and matches none of them, is an error naming the
    candidates and never a silent no-op — a silently ignored arming turns a
    green run into a false survivor.

    The one case that is not a mistake is {!Uncatalogued}, and it is separated
    from {!Unmatched} here because only the registry can tell the two apart. One
    identifier is normally handed to {e every} test executable of a project at
    once — the aggregate report's reproduce line arms one identifier across a
    whole re-run of the suite, because a command that links no test executable
    has no single binary to name — and in a project with several test
    executables most of them were built from other sources entirely. Such an
    executable holds no such mutant, produces no verdict, and hides nothing by
    running on. Whether that is worth refusing over is the caller's decision —
    the mutation loop makes it, and lets such a run proceed — but only this
    module can say which of the two cases the identifier is in. *)

(** The type for arming errors. All are recoverable: the loop prints them via
    {!pp_arm_error}, and refuses to start on all but {!Uncatalogued}. *)
type arm_error =
  | Malformed of { spec : string; reason : string }
      (** [spec] is not a mutant identifier; [reason] says why. *)
  | Uncatalogued of { id : id }
      (** This executable catalogues no site at all in [id]'s file, so the
          identifier is about some other binary — or about no binary, if nothing
          was built with the mutation backend. Nothing is armed and nothing is
          concealed: an executable that catalogues none of a file's sites cannot
          produce a verdict about them either way. *)
  | Unmatched of { id : id; candidates : mutant list }
      (** [id]'s file is catalogued here, but no site in it matches the line,
          column and rewrite. [candidates] are that file's mutants and are never
          empty — the empty case is {!Uncatalogued}. This is a wrong or stale
          identifier: the caller believes it named a mutant of {e this}
          executable and it did not. *)
  | Ambiguous of { id : id; candidates : mutant list }
      (** [id] matches more than one site; [candidates] lists them, one entry
          per site. The instrumenter cannot emit such a table, so this is a
          rewriter that duplicated a location: no identifier can separate the
          sites, and the remedy is [[@mutate off]] on the expression or
          excluding the file. *)

val pp_arm_error : Format.formatter -> arm_error -> unit
(** [pp_arm_error ppf e] formats a human-readable message for [e], naming the
    candidates and, where there is one, the likely fix. *)

val arm_variable : string
(** [arm_variable] is ["WINDTRAP_MUTATE_ARM"]: the name of the environment
    variable the windtrap core reads a mutant identifier from, and the name the
    report's reproduce line spells. A name, not a reader — this module reads no
    environment; the core parses the value with {!id_of_string} and hands it to
    {!arm}. Deliberately not ["WINDTRAP_MUTANT"]: two variables differing by two
    characters and meaning unrelated things is a defect. *)

val id_of_string : string -> (id, arm_error) result
(** [id_of_string s] parses a mutant identifier in its canonical spelling. It is
    [Error (Malformed _)] if [s] has the wrong shape, if a number is missing or
    negative, if [line < 1], if [col < 0], or if the rewrite is not in
    {!rewrites} — an unrecognized rewrite is a parse error and never an
    identifier that silently matches nothing.

    Round trip: [id_of_string (id_to_string id)] is [Ok id] for every [id] this
    module produces. *)

val arm : ?budget:int -> id -> (mutant, arm_error) result
(** [arm id] arms the single mutant [id] names and is that mutant, and is
    {!Uncatalogued} when this executable holds no site of [id]'s file at all,
    {!Unmatched} when it holds that file's sites but none [id] names, and
    {!Ambiguous} when [id] names several. Any previously armed mutant is
    disarmed first, whether or not [id] resolves. From then on the guard of that
    site — and of every module registering an equal table for its file — answers
    [true]. At most one mutant is armed per process, so this is the only way the
    answer is ever [true].

    [budget] caps the armed site's reach count: the guard raises {!Runaway} on
    the evaluation that would take the count past [budget]. The count is the
    site's, {e since the last} {!reset_reach} — not since this call — so a child
    that inherited the dry run's counts must {!reset_reach} after arming, which
    is what the loop does. The loop sets [budget] from the hit count the dry run
    measured for the site, which is what catches a mutant that spins without
    consuming wall clock in a place a timer can see. It defaults to no cap.

    Two sites of one file that agree on position and rewrite are {!Ambiguous},
    not an arming: no identifier separates them and one of them would stay live,
    reporting a false survivor for code reached through it.

    Raises [Invalid_argument] if [budget] is not positive; nothing is disarmed
    in that case. *)

val disarm : unit -> unit
(** [disarm ()] clears the armed mutant and the runaway budget. The guard is
    [false] at every site afterwards. *)

val armed : unit -> mutant option
(** [armed ()] is the currently armed mutant, [None] when none is. A process
    whose [armed ()] is [None] is observationally the original program. *)

val armed_hits : unit -> int
(** [armed_hits ()] is how many times the armed site has been evaluated since
    the last {!reset_reach} — the same count {!arm}'s [budget] caps, summed over
    every module registering the site's file — and [0] when nothing is armed. It
    is what lets a run that armed a mutant and stayed green tell "the tests
    prove nothing about this site" from "no test ran it": a caller wanting the
    run's own count calls {!reset_reach} after arming, as the loop's children
    do. *)

exception Runaway of { id : id; hits : int; budget : int }
(** Raised by the guard when the armed site [id] has been evaluated [hits] times
    since the last {!reset_reach} and [hits > budget]. It escapes into the
    mutated program, where the child's top-level wrapper reduces it to a killed
    verdict: a mutant that turns a terminating loop into a spinning one is
    killed by its budget, not by the clock. The site's count keeps rising, so a
    caller that swallows the exception and evaluates the site again gets it
    again. *)

(** {1:reach The reach map}

    Which tests evaluate which mutants is measured, not guessed, and it is what
    reduces the loop from [M × T] to something a laptop finishes. The runtime
    provides two total operations over the epoch counter and the dirty list, and
    takes no position on what an epoch means; the loop supplies that by calling
    them from the runner's events:

    - on [Test_started]: {!drain} — whatever accumulated since the previous
      drain was evaluated {e outside} any test (module initialization before the
      first test, fixture release after the previous one) and belongs to no test
      — then {!next_epoch};
    - on [Test_finished]: {!drain}, whose result is the set of mutants that test
      evaluated, with their hit counts;
    - at run end: {!drain} once more, for the last test's teardown.

    Cost is [O(sites that test touched)] per test, never [O(total sites)]. *)

type reached = {
  mutant : mutant;  (** The mutant evaluated. *)
  hits : int;
      (** How many times its guard was evaluated during the drained window,
          saturating at [max_int]. *)
}
(** The type for one entry of a drained window. *)

val next_epoch : unit -> unit
(** [next_epoch ()] opens a fresh observation window. Sites evaluated after it
    mark themselves for the next {!drain}, whether or not they were evaluated
    before. *)

val drain : unit -> reached list
(** [drain ()] is the mutants marked since the previous [drain], ordered by
    {!compare_mutant} and without duplicates, each with the hits it accumulated
    since it was marked; it then empties the mark list. A mutant registered by
    two modules for one source file appears once, with its hit counts added.
    [drain ()] immediately after another [drain ()] is [[]].

    A site marks itself the {e first} time it is evaluated in an epoch, so under
    the protocol above a drain at [Test_finished] is exactly the set that test
    evaluated. A consequence worth knowing: a site the test already evaluated
    and its teardown evaluates again is not marked a second time, so it is
    reported for the test (which is where it belongs — it is armable) and the
    teardown's extra hits are reported nowhere. A budget derived from a drained
    hit count therefore wants headroom. *)

val reset_reach : unit -> unit
(** [reset_reach ()] zeroes every site's reach count, empties the dirty list and
    opens a fresh epoch. A forked child calls it before its first test, so that
    the runaway budget measures the child's own hits and not the dry run's
    accumulated ones. It does not disarm. *)
