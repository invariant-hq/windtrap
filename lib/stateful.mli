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
    earlier drawn call makes, for an {!among} type a value of the type it lists.
    An abstract argument is drawn as one of the earlier calls that make its
    type, and an element as a position. The shrink tree is
    [Gen.Engine.Shrink_tree.list] over the drawn calls, with no repair. A
    candidate deletes calls, reduces one argument or one choice, or moves a
    parallel call out of its branch, and never turns one command into another.

    {b Legality is decided when the program runs.} {!execute} resolves each
    call's abstract arguments among the values that the calls before it made,
    takes its elements, and asks the call's [pre] of the reference as the run
    left it. A call that does not resolve, whose value lists no element or whose
    [pre] fails is skipped on both sides and is absent from the record. A
    candidate can therefore run the calls its parent ran, and {!Property.run},
    which compares failures and never programs, accepts it as a step. Every
    accepted step descends one level of a tree that is finite in depth when the
    argument trees are, except below an element, where every candidate takes an
    earlier index than its parent or is discarded (see [^->]); the shrink budget
    ends that search.

    {b Values.} Only a call whose signature ends in {!makes} makes a value, when
    its system returns. A value holds the reference's side and the system's. It
    is named by its type's prefix and a count per prefix, from [1], in the order
    of the calls that ran. A run starts with no value.

    {b Several domains.} A program for [n] domains is a prefix, [n] branches and
    a suffix. The branches' calls run at once, on worker domains, and {!execute}
    judges their outcomes against the orders of the calls replayed on the
    reference (see {!judge}). Only the prefix has one reference state, so only
    the prefix makes values, asks a [pre] and takes elements. *)

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
      every call on the test's domain while one reference state exists (see
      {!execute}).
    - [release s] releases a system side of the type when a run ends (see
      {!execute}).

    Nothing is checked here. {!val-program} and {!stateful} raise
    [Invalid_argument] if [prefix] is not a lowercase OCaml identifier, if it
    ends with a digit, or if two abstract types of one command list have it. *)

val among :
  'a Testable.t -> ('r, 's) abstract -> ('r -> 'a list) -> ('a, 'a) abstract
(** [among w t candidates] is a new abstract type whose values are the elements
    that a value of [t] lists: [candidates r] of its reference side [r]. A call
    takes an element with [^->] from a value of [t] of its own signature: the
    nearest before the element, else the first after it. No call makes one.
    [candidates] must not change [r]. An element is the same on both sides. It
    has no name, no invariant and no release, and the record prints it through
    [w], whose equality is not used.

    Nothing is checked here. {!val-program} and {!stateful} raise
    [Invalid_argument] if a command makes an element of the type,
    [Windtrap.stateful: pick makes an element of 'd'; an element is listed by a
     value, never made], or takes one without a value of [t]:
    [Windtrap.stateful: get takes an element of 'd' without a value of 'd'; an
     element is listed by a value its call takes], and [an element of 'd'] in
    place of [a value of 'd'] when [t] is itself an {!among} type. *)

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
    prints the value's name.

    When [t] is an {!among} type, [t ^-> fn] takes an element that a value of
    the call lists (see {!among}), the same on both sides. It is drawn as a
    position [k] below 2{^ 30}. When the call runs it takes the candidate at
    [(k * n) lsr 30] of the [n] that the value lists, so deleting another call
    keeps its relative place. A shrink candidate takes, among the [n] of its own
    run, one of the first eight indices that its parent's index shrinks through
    as an integer, or the index before it, and no two candidates take the same
    one; a candidate that changes the element alone runs with its parent's [n].
    A candidate left without an index, past those or below index [0], takes
    nothing, and {!execute} raises [Failure.Control `Discard] at its call, so no
    candidate takes its parent's index or a sibling's. The record prints the
    element through its witness, as a drawn argument prints. *)

val returns : 'a Testable.t -> ('a, 'a, bool) fn
(** [returns w] ends a signature whose two results compare under [w]. *)

val makes : ('r, 's) abstract -> ('r, 's, bool) fn
(** [makes t] ends a signature whose system's result is a new value of [t] when
    the system returns, and whose reference's result is that value's reference
    side. *)

val judges : 'a Testable.t -> (('a, exn) result -> unit, 'a, bool) fn
(** [judges w] ends a signature whose outcome the reference rules on instead of
    predicting. The system's outcome, [Ok v] or [Error e], is the reference's
    last argument. The reference returns to accept it, and rejects it with a
    verb's failure, a broken contract or the system's own exception raised
    again, the same value (see {!execute}). The record prints the outcome
    through [w]. *)

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

val program : ?steps:int -> ?domains:int -> command list -> program Gen.t
(** [program commands] generates the programs over [commands]. [steps] is the
    number of calls drawn, and defaults to [20]. Fewer are drawn when no command
    of the case can be drawn. [domains] defaults to [1].

    With [domains = n] above [1], a program is a prefix, [n] branches and a
    suffix. The prefix and the suffix hold at most [steps] calls between them.
    Each branch holds at most five calls for two domains, three for three, two
    for four and one from five domains. After the prefix a command is drawn only
    when it makes no value, has no [pre] and takes no element, so a branch and
    the suffix choose among the prefix's values. A branch call's first
    candidates move it to the end of the prefix, then to the start of the
    suffix. How the lengths are drawn is not part of the contract.

    Each case draws from a subset of [commands] (swarm testing). When a command
    of the subset takes an abstract type, every command that makes the type is
    in the subset. A command listed twice is drawn twice as often, and is one
    command. Which subset, how a choice is drawn and the order of the candidates
    are not part of the contract.

    The generator prints a program as its record (see {!execute}), and never as
    a pre-image. A program that has not run prints [(not run)].

    Sampling raises [Invalid_argument] if [commands] is empty, if [steps] is
    negative, if [domains] is below [1], if the prefixes break the rules of
    {!val-abstract} or of {!among}, under [~steps:0] too, or if [domains] is
    above [1] and every command makes a value, has a [pre] or takes an element:
    [Windtrap.stateful: on several domains every command makes a value, has a
     ~pre or takes an element, so no call can run after the prefix]. It raises
    [Invalid_argument] when an argument's sample has nothing to print, as that
    of a {!Gen.constant} or a {!Gen.of_list} without [~pp] or {!Gen.with_pp}:
    [push: argument 2 has no printer; attach one with Gen.with_pp], arguments
    counted from one, abstract ones included. *)

val summary : program -> string option
(** [summary program] is the record of [program] in one line, as
    [5 calls, last: elements], or [4 calls, 2 in parallel] for a record with
    parallel calls, and [None] for a record without calls and for a program that
    has not run. The last call of a failing run on one domain is the call that
    failed, or the call after which an invariant failed. *)

val execute : ?workers:Workers.t -> program -> unit
(** [execute program] runs [program] from no value and records what ran in
    [program], in place of any earlier record. It returns [()] iff no call
    failed, no invariant failed and no release failed. [workers] run the
    branches of a program on several domains (see
    {{!section-several}several domains}). Without them the branches run one
    after the other on the calling domain. They are ignored on one domain.

    {b A call.} Each drawn call runs in this order, and the first failure ends
    the run:
    + Its abstract arguments resolve. A call that does not resolve is skipped.
    + Its elements are taken, each from the [candidates] of the reference side
      of the value it reads (see {!among}). A call whose value lists no element
      is skipped. A shrink candidate whose element is left without an index
      raises [Failure.Control `Discard] here, once the releases ran (see [^->]).
    + Its [pre] is asked of the reference's arguments. A call whose [pre] is
      [false] is skipped.
    + The reference side of each abstract argument whose type has a [pp] is
      printed, for the record.
    + The system runs. Under {!makes}, a system that returns makes a value.
    + The reference judges the system's outcome. Under {!returns} and {!makes}
      it runs and the two outcomes compare, and under {!makes} its result is the
      value's reference side. Under {!judges} it receives the system's outcome
      and accepts or rejects it.
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
      does anything that a [pre] or the [candidates] of an {!among} type raise:
      {!execute} raises {!Property.Oracle_failure}.
    - From a {!judges} reference, a verb's failure or a broken contract fails
      the run at that call, and so does the system's own exception raised again,
      recognised by physical equality, as a [Failure.Raise] failure with the
      system's exception as [actual] and its backtrace. Any other exception, and
      a discard, break the reference.
    - Every other control passes as it is, so a skip, a timeout and an [exit]
      keep the meaning they have in any law.

    A discard fails with the message
    [assume or reject in a command; a call's legality is its ~pre].

    {b Failures.} A failure is raised as a [Failure.Check_failure], or a
    [Property.Oracle_failure] for a broken reference, whose [msg] starts with a
    label, followed by the failure's own [msg] after ["; "], flattened to one
    line:
    - [call 3 of 3: push q1 0] for a call's mismatch, a never-outcome of its
      system, or a rejection by its {!judges} reference;
    - [reference of call 3 of 3: pop q1] for a broken reference function, and
      [reference of call 3 of 3: get d1 _] for broken [candidates], [_] being
      the element they did not give;
    - [reference of call 2 of 3, in the order 2 then 3: pop q1] for a reference
      function that broke while the judge replayed an order on several domains
      (see {{!section-several}several domains});
    - [~pre of call 3 of 3: pop q1] for a broken [pre];
    - [after call 3 of 3, on s2] for an invariant;
    - [release of q1] for a release;
    - on several domains, two lines for outcomes that no order explains (see
      {{!section-several}several domains}).

    [N] in [of N] counts the calls that the run executed, the failing one
    included, so on one domain the failing call is the last of the record. On
    several domains a failing parallel call is followed by the other branches'
    calls. A mismatch of two results is an equality failure over the witness's
    printing. A mismatch involving an exception is a [Failure.Raise] failure,
    with the reference's exception, if any, as [expected] and the system's, if
    any, as [actual], its backtrace included. A failure that recorded no
    location gets its command's. An invariant's and a release's keep their own,
    and none when they recorded none. Any other exception of an invariant or a
    release is a [Failure.Raise] failure with its backtrace.

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
    [-]. An abstract argument is its value's name. An element is its witness's
    printing, under a drawn argument's rules, [<pp raised EXN>] when the printer
    raises, and [_] when its call failed before taking it. A [reference before]
    cell holds the printed reference sides of the call's abstract arguments
    whose type has a [pp], joined by [", "] and cut at 60 code points. A [pp]
    that raises costs its own cell, [<pp raised EXN>]. A record of more than 40
    calls prints its first and last 20. A record without calls prints
    [(no calls)].

    A record with a parallel call has two more columns: [domain], before [call],
    the branch of a parallel call and blank for the others, and [result], after
    it, the system's outcome in the run: a result as its witness prints it, cut
    at 60 code points, or [exception E], and blank for a call that made a value.
    A record with a call whose signature ends in {!judges} has the [result]
    column too. Only the prefix's rows have [reference before] cells. No line
    ends on a blank. *)

(** {2:several Several domains}

    On a program with branches, {!execute} runs the program [50] times with
    [workers], once without, each run from no value, and it fails at the first
    run that fails. Each run after the first starts with
    {!Run.restart_law_output}, so a failure shows the output of the run that
    failed. A run goes as follows.
    + The prefix runs as a program on one domain does, invariants included.
    + {b The branches.} Their calls resolve among the prefix's values, and a
      call that does not is skipped. Branch [i] runs on worker [i]: only the
      system functions, one after the other. A system's failure ends its branch,
      and the other branches run on. The run then fails at the first call, in
      program order, that failed. What else a system raised, a control,
      [Sys.Break] and [Out_of_memory] included, is raised again on the calling
      domain, with its backtrace, the first branch's first. Without [workers]
      the first such raise ends the run.
    + {b The suffix.} Its calls resolve among the prefix's values, and only
      their systems run, on the calling domain.
    + {b The judge} (see {!judge}) looks for an order of the calls that ran,
      each branch in its order, then the suffix, whose replay on the reference
      gives every outcome the system gave where the reference predicts it, and
      is accepted where it judges it. A replay starts from a reference replayed
      along the prefix. A {!judges} reference receives the system's recorded
      outcome in every replay, and its rejection rules the order out. When no
      order explains the outcomes, the run fails with
      [no order of the calls gives these results], then
      [the closest order, 2 then 3, differs at call 4: length q1], naming the
      parallel calls of the order whose first difference comes latest, over that
      difference's own failure. With no parallel call there is one order, and
      the failure reads as a call's does on one domain.

    A replay of the prefix that gives one of its calls another outcome than the
    run's breaks the reference, [reference of call 2 of 5: get m1], with the
    message
    [a replay of the reference differs from this run; the reference must behave
     the same from run to run], since the judge would otherwise blame the system
    for it.

    A reference function that breaks while the judge replays an order ends the
    run as a broken reference, even when another order would explain the
    outcomes. With a parallel call, its label names the order the judge
    replayed, completed as the closest order is:
    [reference of call 2 of 3, in the order 2 then 3: pop q1].

    No invariant runs after the prefix: several orders may explain a run, and no
    one reference state exists after the branches. Labels count in the prefix's
    run and in one replay of the order the judge accepted in the first run;
    every other replay of the reference counts nothing ({!Run.without_labels}).
    The releases run as on one domain.

    When the test's limit expires while the branches run, the calling domain
    waits for them at most one more limit ({!Workers.run}'s [grace]). Should a
    call still run then, the run releases nothing, the test times out and
    {!Run.stop} ends the run of the suite after it: the worker would run the
    test's code inside the next test. *)

(** {1:judging Judging} *)

(** The type for the verdicts of {!judge}. *)
type verdict =
  | Explained of int list
      (** The order accepted, as the numbers of its calls: the branches' calls
          interleaved, then the suffix's. *)
  | Unexplained of { order : int list; at : int; failure : Failure.t }
      (** No order explains the outcomes. [order] is the closest, the order
          whose first difference comes latest, the first such found, completed
          with the calls it did not reach: each branch in turn, then the suffix.
          [at] is the number of its first differing call, and [failure] that
          difference. *)

val judge :
  fresh:(unit -> 'a) ->
  branches:(int * ('a -> Failure.t option)) list list ->
  suffix:(int * ('a -> Failure.t option)) list ->
  verdict
(** [judge ~fresh ~branches ~suffix] looks for an order of the calls of
    [branches], each branch in its own order, followed by [suffix], in which
    every call is [None], and is the first found. A call is a number and a
    function that runs the call on a reference state, and is [None] when the
    call's outcome is the system's, and its difference otherwise.

    The search is depth first, over program order and never real time: the first
    branch's next call is tried first, and a call that differs ends the orders
    that start with the path to it. The first order of a branch point continues
    on the state that the call before it left. Every other starts from
    [fresh ()], on which the path is replayed, its calls' results ignored, so a
    state need be neither persistent nor copyable. With [n] calls in two
    branches, at most the binomial [n] choose the first branch's length orders
    are tried. What a call raises leaves [judge]. *)

(** {1:declaring Declaring} *)

val stateful :
  ?__POS__:Loc.pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?count:int ->
  ?steps:int ->
  ?domains:int ->
  string ->
  command list ->
  Test_tree.t
(** [stateful name commands] is the property test [name] over the programs of
    [commands]. Its body checks [commands], [steps] and [domains] as
    {!val-program} does, then runs {!Run.property} over
    [program ?steps ?domains commands] with [execute ?workers] as its law and
    {!summary} as its summary, then judges the commands never called.
    - [timeout] and [count] are {!Run.prop}'s, and so is [--prop-count].
    - [__POS__] is the declaration site, resolved once at this call. It is the
      site of the test.
    - ["prop"] and ["stateful"] are always added to [tags], and ["parallel"]
      when [domains] is above [1].

    {b Several domains.} With [domains] above [1], the body spawns [domains]
    workers ({!Workers.spawn}) before the first case, outside the property, and
    joins them when it ends, however it ends. A spawn that fails fails the test
    with a [Failure.Check_failure] at the declaration site,
    [cannot spawn a worker domain: <message>], and no counterexample. Each run
    of the law costs [50] of the shrink budget ({!Property.run}'s [cost]), and
    one that discards costs [1]: only an element discards, and it is taken in
    the prefix of the first repetition. The law is not [deterministic], so a
    counterexample does not run again. The test takes [~retries:0], so a group's
    retries do not apply. Under [--mutate] or [--arm] ([config.mutation] is not
    {!Run.No_mutation}) the body spawns nothing, each program runs once on the
    test's domain, and a counterexample runs again.

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
     where its arguments resolve and its ~pre holds]. When one of them takes an
    element of an {!among} type, the hint reads
    [where its arguments resolve, its value lists an element and its ~pre holds]
    instead. A command is a value of [commands], compared physically.

    It takes no [examples], since a program is drawn, no [max_discard], the
    budget being {!Property.run}'s default, and no [retries], since a second
    attempt would replay the same programs from the root seed.

    Raises [Invalid_argument] if [timeout] is given and is not finite and
    positive. The body raises [Invalid_argument], inside the running test and
    before any case, under [~count:0] too, where {!val-program}'s sampling
    would. *)
