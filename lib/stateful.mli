(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Stateful testing against a reference.

    A {{!type-command}command} pairs a reference's function with a system's
    under a {{!type-fn}signature}: which arguments are drawn, which are values
    of {{!type-abstract}abstract types} that earlier calls made, and how the two
    outcomes compare. A {{!type-program}program} is a drawn sequence of calls.
    {!execute} runs it and records what ran, and a failing run raises its
    {{!type-record}record} with its failure ({!Property.Run_failure}).
    {!Property} and the renderers know a failing program only as that table and
    its one-line {!summary}, so seeds, replay, [--prop-count], the shrink
    budget, labels, tags and timeouts are those of any property.

    {b Drawing is structural.} No reference runs while a program is drawn. A
    program is parsed from a sequence of choices (see {!Choice}), as every
    generator is. Each call is one span: a choice to go on, the command's
    position in the list, then its arguments. A command is drawn only when every
    abstract type it takes has a value that an earlier call makes, for an
    {!among} type a value of the type it lists. An abstract argument is drawn as
    the [k]th newest of the earlier calls that make its type, and an element as
    a position. The reducer of {!Shrink} shrinks a program as any value:
    deleting a span deletes a call, lowering a choice reduces an argument, turns
    a command into an earlier one of the list, or, on several domains, moves a
    parallel call out of its branch. A deletion lowers the [k] of each later
    abstract argument by the makers it deleted among the [k] newer, so the
    argument keeps its value. Nothing repairs a parse.

    {b Legality is decided when the program runs.} {!execute} resolves each
    call's abstract arguments among the values that the calls before it made,
    takes its elements, and asks the call's [pre] of the reference as the run
    left it. A call that does not resolve, whose value lists no element or whose
    [pre] fails is skipped on both sides and is absent from the record. Every
    sequence parses, so every edit of the reducer is a program, and
    {!Property.run}, which compares failures and never programs, keeps an edit
    that fails as its parent did.

    {b Several domains.} A program for [n] domains is a prefix, [n] branches and
    a suffix. The branches' calls run at once, on worker domains, and {!execute}
    judges their outcomes against the orders of the calls replayed on the
    reference (see {!judge}). Only the prefix has one reference state, so only
    the prefix makes values, asks a [pre] and takes elements. *)

(* The facade ([{1:stateful_tests}] in windtrap.mli) states each value's
   contract for a test's author. The docs here state only what it does not: how
   a program is drawn and parsed, what a run checks in which order, the failures
   and the record it builds, and how the body runs the property. *)

(** {1:abstract Abstract types} *)

type ('r, 's) abstract
(** See the facade's [abstract]. *)

val abstract :
  ?pp:(Format.formatter -> 'r -> unit) ->
  ?invariant:('r -> 's -> unit) ->
  ?release:('s -> unit) ->
  string ->
  ('r, 's) abstract
(** See the facade's [abstract]. Nothing is checked here: {!val-program} raises
    on the prefixes as {!stateful} does. *)

val among :
  'a Testable.t -> ('r, 's) abstract -> ('r -> 'a list) -> ('a, 'a) abstract
(** See the facade's [among]. Nothing is checked here: {!val-program} raises on
    its rules as {!stateful} does, and the message reads [an element of 'd'] in
    place of [a value of 'd'] when [t] is itself an {!among} type. *)

(** {1:signatures Signatures} *)

type ('r, 's, 'p) fn
(** See the facade's [fn]. *)

val ( @-> ) : 'a Gen.t -> ('r, 's, 'p) fn -> ('a -> 'r, 'a -> 's, 'a -> 'p) fn
(** See the facade's [( @-> )]. The record prints the argument as its sample
    renders (see {!Gen.Engine.render}). *)

val ( ^-> ) :
  ('ra, 'sa) abstract -> ('r, 's, 'p) fn -> ('ra -> 'r, 'sa -> 's, 'ra -> 'p) fn
(** See the facade's [( ^-> )]. A value is drawn as the [k]th newest maker, as
    the preamble says, and a deletion of the call that made it leaves [k]. An
    element is drawn as a position [k] below 2{^ 30}, its choice: when the call
    runs it takes the candidate at [(k * n) lsr 30] of the [n] that the value
    lists. *)

val returns : 'a Testable.t -> ('a, 'a, bool) fn
(** See the facade's [returns]. *)

val makes : ('r, 's) abstract -> ('r, 's, bool) fn
(** See the facade's [makes]. *)

val judges : 'a Testable.t -> (('a, exn) result -> unit, 'a, bool) fn
(** See the facade's [judges]. *)

(** {1:commands Commands} *)

type command
(** See the facade's [command]. Its signature is existential. *)

val command :
  ?__POS__:Loc.pos ->
  ?pre:('a -> 'p) ->
  string ->
  ('a -> 'r, 'b -> 's, 'a -> 'p) fn ->
  ('a -> 'r) ->
  ('b -> 's) ->
  command
(** See the facade's [command]. The type demands at least one argument, since
    the third parameter of every result form is [bool], which is not an arrow.
    [name] also names the command in the {!summary}, and [__POS__] is resolved
    at this call by {!Loc.resolve}. Nothing is checked at declaration. *)

(** {1:programs Programs} *)

type program
(** The type for drawn programs: the calls of one case, and the commands that
    case draws from. *)

val program : ?steps:int -> ?domains:int -> command list -> program Gen.t
(** [program commands] generates the programs over [commands]. [steps] is the
    number of calls drawn, and defaults to [20]. Fewer are drawn when no command
    of the case can be drawn. [domains] defaults to [1].

    With [domains = n] above [1], a program is a prefix, [n] branches and a
    suffix. The prefix and the suffix hold at most [steps] calls between them.
    Each branch holds at most as many calls as the facade's [stateful] says.
    After the prefix a command is drawn only when it makes no value, has no
    [pre] and takes no element, so a branch and the suffix choose among the
    prefix's values. A call after the prefix reads its place first: [0] ends the
    prefix, [1] starts the suffix and [i + 1] is branch [i], so lowering it
    moves a parallel call to the end of the prefix, to the start of the suffix
    or to an earlier branch, and never moves a call of the prefix or of the
    suffix into a branch. A parse that places a command after the prefix that
    cannot run there, or one call more than a branch holds, raises
    [Failure.Control `Discard]. How the lengths are drawn is not part of the
    contract.

    Each case draws from a subset of [commands] (swarm testing). When a command
    of the subset takes an abstract type, every command that makes the type is
    in the subset. The subset only steers drawing: it is drawn from the stream
    and recorded in no choice, so a replay holds every command, and a command
    appended to the list changes no program. A command listed twice is drawn
    twice as often, and is one command. Which subset and how a choice is drawn
    are not part of the contract.

    The generator prints a program as it was drawn, one call a line,
    [let v = name a1 … an] for a call that makes a value, a value being named as
    its maker would name it if every call made one, and a branch's calls after
    the branch's number, as [1: push q1 0]. A program without calls prints
    [(no calls)]. The digest of a replay token is of this printing; a failing
    program's counterexample prints as its record (see {!execute}).

    Sampling raises [Invalid_argument] where the facade's [stateful] says its
    body raises, under [~steps:0] too. On several domains the message reads
    [Windtrap.stateful: on several domains every command makes a value, has a
     ~pre or takes an element, so no call can run after the prefix]. Sampling
    also raises it with the facade's [( @-> )] message when an argument's sample
    has nothing to print, arguments counted from one, abstract ones included. *)

type record
(** The type for the record of a run: the calls that ran, in order, with what
    the report prints of each. *)

val pp_record : Format.formatter -> record -> unit
(** [pp_record] prints a record as the table that {!execute} describes. *)

val summary : record -> string option
(** [summary record] is [record] in one line, as [5 calls, last: elements], or
    [4 calls, 2 in parallel] for a record with parallel calls, and [None] for a
    record without calls. The last call of a failing run on one domain is the
    call that failed, or the call after which an invariant failed. *)

val execute : ?workers:Workers.t -> program -> record
(** [execute program] runs [program] once from no value, and is the record of
    what ran when no call failed, no invariant failed and no release failed. A
    run that fails raises {!Property.Run_failure}, its [fault] the failure, its
    [record] the record printed by {!pp_record} and its [summary] {!summary}'s.
    A control and a fatal exception leave as they are, with no record. [workers]
    run the branches of a program on several domains (see
    {{!section-several}several domains}). Without them the branches run one
    after the other on the calling domain. They are ignored on one domain.

    {b A call} runs as the facade's preamble says, in this order: its values
    resolve, its elements are taken, its [pre] is asked, the reference sides of
    its [reference before] cell print, the system runs, the reference judges,
    and the invariants run, each type's on its values in the order they were
    made. The first failure ends the run.

    {b Outcomes} compare as the facade says, the witness applied to the
    reference's result first, an exception's constructor name as [Printexc]
    gives it. Of the never outcomes, a broken reference makes {!execute} raise
    {!Property.Oracle_failure}, and the system's own exception that a {!judges}
    reference raises again is a [Failure.Raise] failure with the system's
    exception as [actual] and its backtrace. A discard fails with the message
    [assume or reject in a command; a call's legality is its ~pre].

    {b Failures.} A failure is the [fault] of a {!Property.Run_failure}:
    [`Assertion f], or [`Exception (Property.Oracle_failure f, _)] for a broken
    reference, where [f]'s [msg] starts with a label, followed by the failure's
    own [msg] after ["; "], flattened to one line:
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

    {b Release} runs as the facade's [abstract] says, newest first by the call
    that first made a side. Over a failing run a release that raises a control
    is not dropped: the control replaces the failure. A fatal exception,
    [Sys.Break] or [Out_of_memory], skips the record too.

    {b The record.} A record prints as a table with one header row: [#], the
    call's number among the calls that ran, then [reference before] when a row
    has a cell, then [call]. A call reads [name a1 … an], [let v = name a1 … an]
    when it made the value [v]. A drawn argument is its sample's rendering on
    one line, cut at 200 bytes, in parentheses when it holds a space or starts
    with [-]. An abstract argument is its value's name. An element is its
    witness's printing, under a drawn argument's rules, [<pp raised EXN>] when
    the printer raises, and [_] when its call failed before taking it. A
    [reference before] cell holds the printed reference sides of the call's
    abstract arguments whose type has a [pp], joined by [", "] and cut at 60
    code points. A [pp] that raises costs its own cell, [<pp raised EXN>]. A
    record of more than 40 calls prints its first and last 20. A record without
    calls prints [(no calls)].

    A record with a parallel call has two more columns: [domain], before [call],
    the branch of a parallel call and blank for the others, and [result], after
    it, the system's outcome in the run: a result as its witness prints it, cut
    at 60 code points, or [exception E], and blank for a call that made a value.
    A record with a call whose signature ends in {!judges} has the [result]
    column too. Only the prefix's rows have [reference before] cells. No line
    ends on a blank. *)

(** {2:several Several domains}

    On a program with branches a run goes as follows.
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
      each branch in its order and a call whose system returned before another's
      began first, then the suffix, whose replay on the reference gives every
      outcome the system gave where the reference predicts it, and is accepted
      where it judges it. A replay starts from a reference replayed along the
      prefix. A {!judges} reference receives the system's recorded outcome in
      every replay, and its rejection rules the order out. When no order
      explains the outcomes, the run fails with
      [no order of the calls gives these results], then
      [the closest order, 2 then 3, differs at call 4: length q1], naming the
      parallel calls of the order whose first difference comes latest, over that
      difference's own failure. With no parallel call there is one order, and
      the failure reads as a call's does on one domain.

    {b Real time.} Each call records when its system began and returned, on the
    monotonic clock ({!Os.counter}), read on the domain that runs it: no domain
    writes anything shared to take a stamp. Only a call that returned strictly
    before another began orders the two, so calls that overlap, or that the
    clock's resolution cannot tell apart, may come in either order. Without
    [workers] the branches run one after the other, and real time leaves them
    that one order.

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
    run and in one replay of the order the judge accepted; every other replay of
    the reference counts nothing ({!Run.without_labels}). The releases run as on
    one domain.

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
  precedes:(int -> int -> bool) ->
  branches:(int * ('a -> Failure.t option)) list list ->
  suffix:(int * ('a -> Failure.t option)) list ->
  verdict
(** [judge ~fresh ~precedes ~branches ~suffix] looks for an order of the calls
    of [branches], each branch in its own order, in which a call [a] comes
    before every call [b] of another branch that [precedes a b], followed by
    [suffix], in which every call is [None], and is the first found. A call is a
    number and a function that runs the call on a reference state, and is [None]
    when the call's outcome is the system's, and its difference otherwise.
    [precedes a b] is whether call [a] returned before call [b] began. It must
    hold of no two calls of one branch out of their order, and hold of a later
    call of a branch whenever it holds of an earlier one.

    The search is depth first: the first branch's next call that no other
    branch's next call precedes is tried first, and a call that differs ends the
    orders that start with the path to it. The first order of a branch point
    continues on the state that the call before it left. Every other starts from
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
(** See the facade's [stateful]. Its body checks [commands], [steps] and
    [domains] as {!val-program} does, then runs {!Run.property} over
    [program ?steps ?domains commands] with [execute ?workers] as its law, then
    judges the commands never called. [timeout] and [count] are {!Run.prop}'s.
    [__POS__] is the declaration site, resolved once at this call.

    {b Several domains.} With [domains] above [1], the body spawns the workers
    ({!Workers.spawn}) before the first case, outside the property, and joins
    them however it ends. A spawn that fails is a [Failure.Check_failure] at the
    declaration site, with no counterexample. Each case runs [50] times
    ({!Property.run}'s [runs]) and fails at the first run that fails, so it
    costs [50] of the shrink budget. The test takes [~retries:0]. Under
    [--mutate] or [--arm] ([config.mutation] is not {!Run.No_mutation}) the body
    spawns nothing.

    {b A broken reference} is a {!Property.Oracle_failure}, which
    {!Property.run} shrinks and rejects as the facade says.

    {b Commands never called.} When {!Run.property} returns, every case has
    passed. If at least one did, a command that some passing case's subset held
    and that no passing case ran makes the test vacuous, with a
    [Failure.Check_failure] of a {!Failure.Vacuous} failure at the declaration
    site, whose text names every such command in the order of [commands]:
    [never called: "pop", "peek" (over 100 passing cases); a call runs only
     where its arguments resolve and its ~pre holds]. When one of them takes an
    element of an {!among} type, the hint reads
    [where its arguments resolve, its value lists an element and its ~pre holds]
    instead. A command is a value of [commands], compared physically.

    It takes no [examples], since a program is drawn, no [max_discard], the
    budget being {!Property.run}'s default, and no [retries] of its own.

    Raises [Invalid_argument] if [timeout] is given and is not finite and
    positive. The body raises [Invalid_argument], inside the running test and
    before any case, under [~count:0] too, where {!val-program}'s sampling
    would. *)
