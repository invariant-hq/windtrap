(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Stateful testing against a reference.

    A {{!type-command}command} pairs a reference's function with a system's
    under a {{!type-fn}signature}: which arguments are drawn, which are values
    of {{!type-abstract}abstract types} that earlier calls made, and how the two
    outcomes compare. A {{!type-program}program} is a drawn sequence of calls.
    {!execute} runs it and records what ran, and the record is what the program
    prints. {!Property} and the renderers know a program only as that table and
    its one-line {!summary}, so seeds, replay, [--prop-count], the shrink
    budget, labels, tags and timeouts are those of any property.

    {b Drawing is structural.} No reference runs while a program is drawn. A
    command is drawn only when every abstract type it takes has a value that an
    earlier drawn call makes, and an abstract argument is drawn as one of the
    earlier calls that make its type. The shrink tree is
    [Gen.Engine.Shrink_tree.list] over the drawn calls, with no repair. A
    candidate deletes calls or reduces one argument or one choice, and never
    turns one command into another.

    {b Legality is decided when the program runs.} {!execute} resolves each
    call's abstract arguments among the values that the calls before it made,
    and asks the call's [pre] of the reference as the run left it. A call that
    does not resolve or whose [pre] fails is skipped on both sides and is absent
    from the record. A candidate can therefore run the calls its parent ran, and
    {!Property.run}, which compares failures and never programs, accepts it as a
    step. Every accepted step descends one level of a tree that is finite in
    depth when the argument trees are, so the search ends.

    {b Values.} Only a call whose signature ends in {!makes} makes a value, when
    its system returns. A value holds the reference's side and the system's. It
    is named by its type's prefix and a count per prefix, from [1], in the order
    of the calls that ran. A run starts with no value. *)

(** {1:abstract Abstract types} *)

type ('r, 's) abstract
(** The type for abstract types of an API, whose values only calls make. A value
    holds the reference's side ['r] and the system's side ['s]. *)

val abstract :
  ?pp:(Format.formatter -> 'r -> unit) ->
  ?invariant:('r -> 's -> unit) ->
  ?release:('s -> unit) ->
  string ->
  ('r, 's) abstract
(** [abstract prefix] is a new abstract type, distinct from every other, whose
    values are named [prefix] and a count.
    - [pp] prints a reference side in the [reference before] column of a record
      (see {!execute}).
    - [invariant r s] runs on the two sides of every value of the type after
      every call (see {!execute}).
    - [release s] releases a system side of the type when a run ends (see
      {!execute}).

    Nothing is checked here. {!val-program} and {!stateful} raise
    [Invalid_argument] if [prefix] is not a lowercase OCaml identifier, if it
    ends with a digit, or if two abstract types of one command list have it. *)

(** {1:signatures Signatures} *)

type ('r, 's, 'p) fn
(** The type for signatures. ['r] is the type of the reference's function, ['s]
    the system's and ['p] the precondition's, the reference's arguments to
    [bool]. A signature is one or more arrows ended by one result form, and the
    left operand of an arrow is a generator or an abstract type, so a result
    form never stands in argument position. *)

val ( @-> ) : 'a Gen.t -> ('r, 's, 'p) fn -> ('a -> 'r, 'a -> 's, 'a -> 'p) fn
(** [gen @-> fn] takes an argument drawn from [gen], the same value on both
    sides and in every run of the program, so neither side may mutate it. It
    shrinks with [gen], and the record prints it as its sample renders (see
    {!Gen.Engine.render}), a pre-image included. *)

val ( ^-> ) :
  ('ra, 'sa) abstract -> ('r, 's, 'p) fn -> ('ra -> 'r, 'sa -> 's, 'ra -> 'p) fn
(** [t ^-> fn] takes a value of [t]: its reference side for the reference and
    for [pre], its system side for the system. It is drawn as one of the earlier
    calls that make a value of [t]. When the call runs, it resolves to the value
    that this call made, or, when it made none, to the newest value of [t] that
    the calls before it made. A choice shrinks toward the newest such call.
    Deleting other calls never moves it off the value its call made. The record
    prints the value's name. *)

val returns : 'a Testable.t -> ('a, 'a, bool) fn
(** [returns w] ends a signature whose two results compare under [w]. *)

val makes : ('r, 's) abstract -> ('r, 's, bool) fn
(** [makes t] ends a signature whose system's result is a new value of [t] when
    the system returns, and whose reference's result is that value's reference
    side. *)

val chooses : 'a Testable.t -> (('a, exn) result -> 'a, 'a, bool) fn
(** [chooses w] ends a signature whose outcome the API leaves open. The system's
    outcome, [Ok v] or [Error e], is the reference's last argument. The
    reference returns or raises the outcome it accepts, which compares with the
    system's under [w] and the exception rule (see {!execute}). *)

(** {1:commands Commands} *)

type command
(** The type for one operation of an API, on the reference and on the system.
    Its signature is existential, so one list holds commands of every signature.
*)

val command :
  ?__POS__:Loc.pos ->
  ?pre:('a -> 'p) ->
  string ->
  ('a -> 'r, 'b -> 's, 'a -> 'p) fn ->
  ('a -> 'r) ->
  ('b -> 's) ->
  command
(** [command name fn reference system] is the operation [name] of signature
    [fn], [reference] being the reference's function and [system] the system's.
    The type demands at least one argument, since the third parameter of every
    result form is [bool], which is not an arrow.
    - [pre] is whether a call is legal, over the reference's arguments. It must
      not change them. Defaults to legal everywhere.
    - [name] identifies the command in the record, the summary and the label of
      a failing call. Its newlines become spaces.
    - [__POS__] is the location of a failure of its calls that recorded none. It
      defaults to a capture at this call (see {!Loc.resolve}), never at the
      failure. Nothing is checked at declaration. *)

(** {1:programs Programs} *)

type program
(** The type for drawn programs: the calls of one case, the commands that case
    draws from, and the record of the program's last run. *)

val program : ?steps:int -> command list -> program Gen.t
(** [program commands] generates the programs over [commands]. [steps] is the
    number of calls drawn, and defaults to [20]. Fewer are drawn when no command
    of the case can be drawn.

    Each case draws from a subset of [commands] (swarm testing). When a command
    of the subset takes an abstract type, every command that makes the type is
    in the subset. A command listed twice is drawn twice as often, and is one
    command. Which subset, how a choice is drawn and the order of the candidates
    are not part of the contract.

    The generator prints a program as its record (see {!execute}), and never as
    a pre-image. A program that has not run prints [(not run)].

    Sampling raises [Invalid_argument] if [commands] is empty, if [steps] is
    negative, or if the prefixes break the rules of {!val-abstract}, under
    [~steps:0] too. It raises [Invalid_argument] when an argument's sample has
    nothing to print, as that of a {!Gen.constant} or a {!Gen.of_list} without
    {!Gen.with_pp}:
    [push: argument 2 has no printer; attach one with Gen.with_pp], arguments
    counted from one, abstract ones included. *)

val summary : program -> string option
(** [summary program] is the record of [program] in one line, as
    [5 calls, last: elements], and [None] for a record without calls and for a
    program that has not run. The last call of a failing run is the call that
    failed, or the call after which an invariant failed. *)

val execute : program -> unit
(** [execute program] runs [program] from no value and records what ran in
    [program], in place of any earlier record. It returns [()] iff no call
    failed, no invariant failed and no release failed.

    {b A call.} Each drawn call runs in this order, and the first failure ends
    the run:
    + Its abstract arguments resolve. A call that does not resolve is skipped.
    + Its [pre] is asked of the reference's arguments. A call whose [pre] is
      [false] is skipped.
    + The reference side of each abstract argument whose type has a [pp] is
      printed, for the record.
    + The system runs. Under {!makes}, a system that returns makes a value.
    + The reference judges the system's outcome. Under {!returns} and {!makes}
      it runs and the two outcomes compare, and under {!makes} its result is the
      value's reference side. Under {!chooses} it receives the system's outcome,
      and the outcome it accepts compares with the system's.
    + The invariant of each abstract type runs on every value of the type, in
      the order the values were made.

    {b Outcomes.} An outcome is a result or a raised exception. Two results
    compare under the witness, which is applied to the reference's first. Two
    exceptions are equal iff their constructor names, as [Printexc] gives them,
    are equal once the module path is removed, so [Stdlib.Queue.Empty] equals
    [Ring.Empty]. Their payloads are printed and never compared. A result never
    equals an exception.

    {b Never outcomes.} A verb's failure, [Assert_failure], [Match_failure] and
    every [Failure.Control] are never outcomes.
    - From a system function, a verb's failure, a broken contract or a discard
      fails the run at that call, and the reference does not run.
    - From a function of the reference, the same breaks the reference, and so
      does anything that a [pre] raises: {!execute} raises
      {!Property.Oracle_failure}.
    - From a {!chooses} reference, a verb's failure or a broken contract fails
      the run at that call, as a mismatch. A discard there breaks the reference.
    - Every other control passes as it is, so a skip, a timeout and an [exit]
      keep the meaning they have in any law.

    A discard fails with the message
    [assume or reject in a command; a call's legality is its ~pre].

    {b Failures.} A failure is raised as a [Failure.Check_failure], or a
    [Property.Oracle_failure] for a broken reference, whose [msg] starts with a
    label, followed by the failure's own [msg] after ["; "], flattened to one
    line:
    - [call 3 of 3: push q1 0] for a call's mismatch or a never-outcome of its
      system or {!chooses} reference;
    - [reference of call 3 of 3: pop q1] for a broken reference function;
    - [~pre of call 3 of 3: pop q1] for a broken [pre];
    - [after call 3 of 3, on s2] for an invariant;
    - [release of q1] for a release.

    [N] in [of N] counts the calls that the run executed, the failing one
    included, so the failing call is the last of the record. A mismatch of two
    results is an equality failure over the witness's printing. A mismatch
    involving an exception is a [Failure.Raise] failure, with the reference's
    exception, if any, as [expected] and the system's, if any, as [actual], its
    backtrace included. A failure that recorded no location gets its command's.
    An invariant's and a release's keep their own, and none when they recorded
    none. Any other exception of an invariant or a release is a [Failure.Raise]
    failure with its backtrace.

    {b Release.} When the run ends, whether it passed, failed or raised a
    control, [release] runs once per physically distinct system side of its type
    that the run made, newest first by the call that first made it. Sides are
    told apart within a type only, so a system side that two types hold is
    released by each. Reference sides are never released. A release that fails
    over a passing run fails it. Over a failing run it is dropped, unless it
    raises a control, which replaces the failure. A fatal exception, [Sys.Break]
    or [Out_of_memory], skips the releases and the record, as it skips a
    teardown.

    {b The record.} The record is a table with one header row: [#], the call's
    number among the calls that ran, then [reference before] when a row has a
    cell, then [call]. A call reads [name a1 … an], [let v = name a1 … an] when
    it made the value [v]. A drawn argument is its sample's rendering on one
    line, cut at 200 bytes, in parentheses when it holds a space or starts with
    [-]. An abstract argument is its value's name. A [reference before] cell
    holds the printed reference sides of the call's abstract arguments whose
    type has a [pp], joined by [", "] and cut at 60 code points. A [pp] that
    raises costs its own cell, [<pp raised EXN>]. A record of more than 40 calls
    prints its first and last 20. A record without calls prints [(no calls)]. *)

(** {1:declaring Declaring} *)

val stateful :
  ?__POS__:Loc.pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?count:int ->
  ?steps:int ->
  string ->
  command list ->
  Test_tree.t
(** [stateful name commands] is the property test [name] over the programs of
    [commands]. Its body checks [commands] and [steps] as {!val-program} does,
    then runs {!Run.property} over [program ?steps commands] with {!execute} as
    its law and {!summary} as its summary, then judges the commands never
    called.
    - [timeout] and [count] are {!Run.prop}'s, and so is [--prop-count].
    - [__POS__] is the declaration site, resolved once at this call. It is the
      site of the test.
    - ["prop"] and ["stateful"] are always added to [tags].

    {b A broken reference.} {!execute} raises {!Property.Oracle_failure} for a
    broken reference, so {!Property.run} shrinks a case that broke the reference
    among the candidates that break it too, and rejects them in the search of a
    case that failed otherwise.

    {b Commands never called.} When {!Run.property} returns, every case has
    passed. If at least one did, a command that some passing case's subset held
    and that no passing case ran fails the test with a [Failure.Check_failure]
    at the declaration site, whose message names every such command in the order
    of [commands]:
    [never called: "pop", "peek" (over 100 passing cases); a call runs only
     where its arguments resolve and its ~pre holds]. A command is a value of
    [commands], compared physically.

    It takes no [examples], since a program is drawn, no [max_discard], the
    budget being {!Property.run}'s default, and no [retries], since a second
    attempt would replay the same programs from the root seed.

    Raises [Invalid_argument] if [timeout] is given and is not finite and
    positive. The body raises [Invalid_argument], inside the running test and
    before any case, under [~count:0] too, where {!val-program}'s sampling
    would. *)
