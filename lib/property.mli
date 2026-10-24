(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(** The property engine.

    {!run} checks a law over explicit examples, then over cases generated under
    one derived seed each. It counts the discards, accumulates the labels and
    shrinks a failing case. It returns an {!outcome}.

    {b Determinism.} The engine draws no randomness of its own. Every case
    samples from a seed derived from the root seed, the test's path and the
    index of the case (see {!run}). An outcome is then a pure function of
    {!run}'s arguments, provided the law and the functions of the generator are
    pure.

    {b Laws.} A law returns [()] to pass and raises to fail. The engine calls
    it, the generator and the printers through [Failure.catch], and keeps
    {{!Failure.section-catching}its rule} as the owner of [`Discard]. A failure
    is a [Failure.fault] of one of three classes, an assertion, a broken oracle
    ({!Oracle_failure}) or any other exception, and the shrink search keeps to
    the class of the first failure (see {!run}). It runs the law again on
    candidates, and once more on the counterexample, so a law must be
    deterministic, or say through [runs] that it is not.
    - A [`Discard] discards the case.
    - A [`Timeout] while no case has failed, during an example, a generation or
      the first run of a case, ends the run in a [Fail] whose failure is a
      [Failure.Timeout] that names the case. Any other control is raised again
      through {!run} then.

    Nothing in this module is global. Labels go through the {!context} that
    {!run} gives to the law. *)

(** {1:discarding Discarding}

    A case is discarded by a [Failure.Control `Discard], raised by the law or at
    generation time. {!run} counts the discard and moves to the next case. *)

val assume : bool -> unit
(** [assume cond] is [()] if [cond] holds, and raises [Failure.Control `Discard]
    otherwise. *)

val reject : unit -> 'a
(** [reject ()] raises [Failure.Control `Discard]. *)

(** {1:oracles Broken oracles} *)

exception Oracle_failure of Failure.t
(** Raised by a law whose oracle broke, with its failure. The oracle is the part
    of the law that decides what the code under test must do, as the reference
    of a stateful test. A broken oracle is a class of failure of its own, so the
    shrink search of a case that raised it keeps to the candidates that raise
    it, and the search of any other failure rejects them (see {!run}). *)

(** {1:records Runs that print themselves} *)

exception
  Run_failure of {
    fault : Failure.fault;
    record : string;
    summary : string option;
  }
(** Raised by a law whose case prints as the record of what its run did, as a
    stateful program prints the table of the calls that its run made: [fault] is
    the failure of the run, [record] the text that stands for the case, and
    [summary] says in one line what [record] holds, as {!run}'s [summary] would.
    {!run} takes the failure as [fault] and prints a counterexample whose run
    raised it as [record], so the record of the run that failed prints, never a
    run of the case drawn again. *)

(** {1:labelling Labelling}

    The marks of a case accumulate in the {!context} and commit when the case
    passes, so a discarded case and a failing case commit nothing. The shrink
    search marks a scratch context, which is thrown away. *)

type context
(** The type for the label accumulator of one {!run}. {!run} creates one per
    call and gives it to the law for the cases of the run. Nothing detects a
    context used after its run, and nothing reads the marks made then. *)

val collect : context -> string -> unit
(** [collect ctx label] marks [label] for the current case. A committed case
    counts a label once, however many times it marked it. The distribution is
    the [collected] of {!type-stats}. *)

val classify : context -> string -> bool -> unit
(** [classify ctx label cond] is [collect ctx label] if [cond] holds and [()]
    otherwise. *)

val scratch : unit -> context
(** [scratch ()] is a context that no run reads: what is marked or demanded in
    it counts nowhere. A law gives it to code whose labels must not count, as
    the orders of a stateful test that its judge rejects. *)

val cover : context -> string -> bool -> unit
(** [cover ctx label cond] is [classify ctx label cond] with the demand that at
    least one passing case marks [label].

    The demand registers when [cover] is first called, whether or not [cond]
    holds. It belongs to the run and is never rolled back, so a [cover] reached
    in a case that is then discarded or fails stays registered, and only its
    mark is dropped. A [cover] that no case reaches registers nothing, and an
    empty table of demands is a satisfied one.

    The demands are judged once, when the run has passed its full case count,
    and an unmarked label then makes the outcome {!Vacuous} with {!Uncovered}.
*)

(** {1:outcomes Outcomes} *)

type cover_status = {
  label : string;  (** The demanded label. *)
  hits : int;  (** The passing cases that marked it. *)
  satisfied : bool;  (** Whether [hits] is above zero. *)
}
(** The type for the state of one {!cover} label when the outcome was decided.
*)

type stats = {
  cases : int;
      (** The cases that ran the law to completion and passed, the examples
          included. *)
  examples : int;  (** The examples among [cases]. *)
  discards : int;
      (** The discarded cases, the examples included, whether the law or the
          generation discarded them. *)
  whole : int option;
      (** [Some n] when the domain was checked whole, its [n] values (see
          {!run}). *)
  count : int;
      (** The generated cases that the run demands pass: [count] of {!run}. *)
  collected : (string * int) list;
      (** The distribution of the labels over the passing cases, sorted by
          label. A {!cover} label counts here too. *)
  coverage : cover_status list;
      (** One entry per {!cover} label, sorted by label. *)
}
(** The type for the bookkeeping of a run, as it stood when the outcome was
    decided. Every outcome carries it. *)

(** The type for the results of {!run}, whose consumer is {!Run.prop}.

    The [failure] of a [Fail] is a {!Failure.Property} failure located at
    {!run}'s [loc]. {!Failure.Property} documents its payload, and {!run}
    decides the following:
    - [rendered] is the final case of the shrink search, parsed again (see
      {!run}), through [Gen.Engine.render], a failing example before its run
      through [Gen.Engine.render_value], or
      [<generator raised before producing a value>]. [rendering] can be [Ocaml]
      or a pre-image in the first case only, and is [Ocaml] when
      [Gen.Engine.literal] holds of the case.
    - [token] is the final case's replay token, its choices and the digest of
      [rendered] (see {!Choice.token}), for a generator that parses choices, and
      [None] otherwise.
    - [case_index] counts the discarded cases. For a failing example it is the
      zero-based position in [examples].
    - [shrink_steps] is [0] for an example and for a generator that raised.
    - [count] is [Some n] for a [`Config n] count, and [None] otherwise.
    - [inner] is the failure of the law on the reported counterexample, the
      final node of the search.
    - [failed_again] is what the law did when it ran once more on the reported
      counterexample, drawn again (see {!run}).

    Its [output_tail] is the captured output of the run of the law that failed
    on the reported counterexample, as [output] gave it, and [None] for a
    generator that raised or when the test's limit expired before [output]
    returned on the counterexample.

    The [failure] of a [Fail] that a timeout ended before any case failed is
    instead a [Failure.Timeout] located at [loc]. Its case is that of the
    example, the generation or the run that the limit cut, with [passed] the
    [cases] of the stats, and [root] and [count] as a {!Failure.Property}
    failure has them.

    The [stats] of a [Fail] are those of the moment the case failed, and an
    unmarked {!cover} label never turns a [Fail] into another outcome. *)
type outcome =
  | Pass of stats
      (** At least one case passed, every case passed, and every {!cover} label
          was marked. *)
  | Fail of { failure : Failure.t; stats : stats }  (** A case failed. *)
  | Vacuous of { why : vacuity; stats : stats }
      (** No case failed, and the run checked less than it claims. *)

(** The type for why a run that refuted nothing is vacuous. *)
and vacuity =
  | Gave_up  (** More than [max_discard] cases were discarded. *)
  | Discarded
      (** No case passed, and at least one was discarded, or the domain checked
          whole has no value. *)
  | No_case
      (** No case and no example ran: a [count] of [0] and no [examples]. *)
  | Uncovered
      (** Every case passed, and an entry of [coverage] is unsatisfied. *)

(** {1:running Running} *)

val default_count : int
(** [default_count] is the count of a run given none, [100]. *)

val shrink_budget : int
(** [shrink_budget] is the number of times a shrink search may run the law,
    [10_000]. Every candidate the law runs on counts, accepted or rejected, so
    the budget bounds any search. It is fixed, so a replay under the same root
    seed shrinks to the same counterexample, whatever the configuration of the
    run.

    A search that has spent the budget ends its failure with
    [Failure.Budget_spent] when it reaches a further candidate. A search that
    reaches a fixed point is [Failure.Converged], whatever it spent. *)

val run :
  ?loc:Loc.t ->
  ?count:[ `Declared of int | `Config of int ] ->
  ?max_discard:int ->
  ?examples:'a list ->
  ?summary:('a -> string option) ->
  ?runs:int ->
  ?output:(unit -> Failure.tail option) ->
  ?shrinking:(int -> unit) ->
  ?replay:string ->
  ?replayed_by:string ->
  ?case:string * int64 array ->
  ?only:(int -> bool) ->
  root:Seed.seed ->
  path:string ->
  'a Gen.t ->
  (context -> 'a -> unit) ->
  outcome
(** [run ~root ~path gen law] checks [law] over [gen] and returns the
    {!outcome}.
    - [root] is the root seed of the run and [path] the test's path in the
      suite. Generated case [index] samples [gen] from
      [Seed.make (Seed.derive ~root ~path ~index)].
    - [loc] is the declaration site of the property. It locates the failure of a
      [Fail].
    - [count] is the number of generated cases that must pass. Defaults to
      [100]. A [`Config n] count comes from the configuration of the run. A
      [`Declared n] count is the test's own and replays by itself.
    - [max_discard] is the number of discarded cases past which the run gives
      up. Defaults to [2 * count], clamped to [max_int].
    - [examples] are the inputs that run before any generated case. Defaults to
      [[]].
    - [summary v] is the [summary] of the failure whose counterexample is [v],
      the value that [rendered] describes. It must be [Some line] iff [gen]
      prints [v] as a table (see {!Failure.Property}), and must not raise.
      Defaults to [Fun.const None].
    - [runs] is the number of times [law] runs on each case, for a law that need
      not fail in every run of a failing case, as one that races on several
      domains. The case fails at the first run that fails, and passes when every
      run passes. Its labels are the marks of all its runs, and [output] gives
      the output of the run that failed. Defaults to [1].
    - [output ()] is the captured output of the run of [law] that has just
      ended. [run] calls it right after each run that fails on a case that
      becomes the counterexample: a failing example, the first failing case, and
      each candidate that the search accepts. It must not raise, though the
      test's limit can expire while it runs. Defaults to [Fun.const None].
    - [shrinking steps] tells how far the shrink search has gone: [run] calls it
      before each run of [law] on a candidate, with the number of candidates
      that the search has accepted. It is first called with [0], and never for
      an example or for a case whose sample has no candidate. It must not raise,
      though the test's limit can expire while it runs. Defaults to [ignore].

    - [replay] is a replay token (see {!Choice.token}). When given, the run
      checks its one case alone, in place of the examples and the generated
      cases: it parses the token's choices, and the law runs on the value when
      the parse read exactly those choices and the value prints with the token's
      digest. A failure is then reported as an example is, with [case_index]
      [0], [examples] [true] and the token. A token that does not parse, a token
      whose parse reads other choices, prints another value, discards or raises,
      and a case the law discards each fail the run with a message that names
      the token, a stale token as stale.
    - [replayed_by] opens that message, as what gave the token. Defaults to
      ["--replay"].
    - [case] is [(name, choices)], the choices of a case file (see
      {!Choice.of_bytes}) and what gave them, as ["--case crash"]. When given,
      the run checks the one case that [choices] replay, in place of [replay],
      the examples and the generated cases: whatever value the parse reads,
      unseeded and unshrunk. Every sequence is a case, so nothing is stale. A
      failure is reported as a replayed one is, with [case_index] [0],
      [examples] [true] and the token of the choices the parse read, which
      [replay] then checks alone. A generator that raises fails as a generated
      case's does. A timeout is a [Failure.Timeout] with no case, since the file
      holds the case. A case that the generator or the law discards is outside
      the law's domain and checks nothing: [run] raises
      [Failure.Control (`Skip (Some reason))], the reason opening with [name].
      [only] does not apply.
    - [only i] says whether the value at position [i] of a domain checked whole
      runs (see below), as a table rerunning its failed rows asks. A value it
      leaves out runs no law and counts nowhere, and its position still numbers
      the others. Unless it keeps every value, [stats.whole] is [None]. It
      changes nothing for a domain that is not checked whole. Defaults to
      keeping every value.

    {b Examples.} The examples run first, in order, unshrunk and without a seed.
    A passing example commits its labels and counts in [cases], a discarding one
    counts in [discards], and a failing one ends the run with [examples = true].
    A [`Timeout] while [output] runs on a failing example leaves that failure
    without output.

    An example is formatted, and given to [summary], at the start of its run,
    before [law], which can change it. A failing example thus prints as it was
    given, unless its run raised a {!Run_failure}. A control that the printer
    raises acts as one that [law] raises.

    {b Finite domains.} After the examples, unless [count] is [0], the run
    parses [gen]'s sequences in lexicographic order, each reading zeros past its
    prefix, to find whether its domain has at most [count] values; a sequence
    that discards or that {!Choice.respelled} is no value. It stops as soon as
    the domain proves larger, endless or raising (see the bounds in
    [property.ml]). A domain of at most [count] values is checked whole, in
    place of the generated cases: the law runs on every value in that order,
    [stats.whole] is [Some n], and when values fail the [Fail]'s failure is the
    first, with [domain] [Some n], [case_index] its position, no shrink step,
    and the others in [also]. Discards and demands are judged as after generated
    cases.

    {b Generated cases.} [index] counts from zero and counts the discarded
    cases, so a discarded seed is never drawn again. The run ends when [count]
    cases have passed. It gives up, {!Vacuous} with {!Gave_up}, as soon as more
    than [max_discard] cases are discarded, the examples included, so a
    [max_discard] of [0] gives up on the first discard. It gives up even when
    [count] is already met, as under [~count:0] with examples that discard past
    the budget.

    {b Vacuity.} A run that refuted nothing and passed no case, an example
    included, checked nothing, and is {!Vacuous} whatever its count: with
    {!Discarded} when a case was discarded or the domain checked whole has no
    value, and with {!No_case} when nothing ran, as under [~count:0] without
    [examples]. A case read from bytes ([case]) is never vacuous: bytes from
    elsewhere that the law discards are outside its domain and say nothing of
    the property, so the run skips instead.

    A [gen] that discards discards the case, and one that raises any other
    control raises it through [run]. A fault of [gen] fails the case unshrunk,
    with [<generator raised before producing a value>] as its counterexample and
    the fault as its [inner].

    {b Shrinking.} A generated case that fails is shrunk by {!Shrink.shrink}
    over the choices it read. A candidate is kept when [law] fails on it in the
    class of the first failure, a [Failure.Check_failure], an {!Oracle_failure}
    or any other exception, and its choices are shortlex smaller. The two
    failures need not be equal. A candidate that passes or raises a control
    other than [`Timeout] is rejected. A candidate whose parse discards or
    raises a fault is rejected at the cost of a discard, and one whose parse
    reads no smaller sequence runs no law.

    The search ends at a fixed point, [Failure.Converged], or when its cases,
    each costing [runs], or [1] when it discards, have spent {!shrink_budget},
    [Failure.Budget_spent].

    A [`Timeout] raised anywhere in the search ends it as well,
    [Failure.Timed_out] with the limit. The failure then describes the last
    accepted case, a case being accepted once [output] has returned on it, and
    the test does not time out. The search starts with [output] on the failing
    case, which a [`Timeout] there reports unshrunk and without output.
    [case_index] is always that of the first failure, so a replay takes the same
    steps, and a timeout changes only where among them the search stops.

    {b Printing.} The runs of the search can change the value of the final case,
    so the counterexample prints parsed again from its choices. A pure [gen]
    builds the counterexample as it was first generated, whatever a run of [law]
    did to the value it was given, and nothing runs on the value before it is
    printed. The final case prints as the search left it when the parse fails,
    which an impure [gen] or the test's limit can cause. A case whose run raised
    a {!Run_failure} prints as its [record], with its [summary], in place of the
    value and of [summary]; its token's digest is still of the value's printing.

    {b Running again.} After the search, a law of one run per case runs once
    more on the counterexample parsed again as for printing. One draw serves
    both, and the run comes after the printing. The run's labels go to a
    {!scratch} context. The [failed_again] of the failure is [Some true] when
    that run raises a [Failure.fault], in any class, [Some false] when it
    returns or raises a control other than [`Timeout], and [None] when the
    test's limit expires during the draw or the run. The law does not run again,
    and [failed_again] is [None], when [runs] is above [1], after a search that
    a timeout ended, for a failing example, which no seed draws, and for a
    generator that raised.

    Raises [Invalid_argument] if [count] or [max_discard] is negative, or if
    [runs] is not positive, inside the running test, where [run] executes.
    Raises a [Failure.Control] other than [`Discard] and [`Timeout] when [law]
    or [gen] raises it outside the search, or [gen]'s printer on an example, and
    no outcome then exists. A control delivered while the final node is
    formatted does not leave [run], since the guard of [Gen.Engine.render] turns
    it into text. *)
