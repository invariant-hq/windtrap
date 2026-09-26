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
    is a [Failure.fault] of one of two classes, an assertion or any other
    exception, and the shrink search keeps to the class of the first failure
    (see {!run}). It runs the law again on candidates, so a law must be
    deterministic.
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

val cover : context -> string -> bool -> unit
(** [cover ctx label cond] is [classify ctx label cond] with the demand that at
    least one passing case marks [label].

    The demand registers when [cover] is first called, whether or not [cond]
    holds. It belongs to the run and is never rolled back, so a [cover] reached
    in a case that is then discarded or fails stays registered, and only its
    mark is dropped. A [cover] that no case reaches registers nothing, and an
    empty table of demands is a satisfied one.

    The demands are judged once, when the run has passed its full case count,
    and an unmarked label then makes the outcome {!Coverage_failed}. *)

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
  discards : int;
      (** The discarded cases, the examples included, whether the law or the
          generation discarded them. *)
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
    - [rendered] is the final node of the shrink search through
      [Gen.Engine.render], a failing example through [Gen.Engine.render_value],
      or [<generator raised before producing a value>]. [rendering] can be a
      pre-image in the first case only.
    - [case_index] counts the discarded cases. For a failing example it is the
      zero-based position in [examples].
    - [shrink_steps] is [0] for an example and for a generator that raised.
    - [count] is [Some n] for a [`Config n] count, and [None] otherwise.
    - [inner] is the failure of the law on the reported counterexample, the
      final node of the search.

    The [failure] of a [Fail] that a timeout ended before any case failed is
    instead a [Failure.Timeout] located at [loc]. Its case is that of the
    example, the generation or the run that the limit cut, with [passed] the
    [cases] of the stats, and [root] and [count] as a {!Failure.Property}
    failure has them.

    The [stats] of a [Fail] are those of the moment the case failed, and an
    unmarked {!cover} label never turns a [Fail] into another outcome. *)
type outcome =
  | Pass of stats  (** Every case passed and every {!cover} label was marked. *)
  | Fail of { failure : Failure.t; stats : stats }  (** A case failed. *)
  | Coverage_failed of stats
      (** The full case count passed and an entry of [coverage] is unsatisfied.
      *)
  | Gave_up of stats  (** More than [max_discard] cases were discarded. *)

(** {1:running Running} *)

val shrink_budget : int
(** [shrink_budget] is the number of times a shrink search may run the law,
    [10_000]. Every candidate probed counts, accepted or rejected, so the budget
    bounds a search over any tree. It is fixed, so a replay under the same root
    seed descends the same path to the same node, whatever the configuration of
    the run.

    A search that has spent the budget ends its failure with
    [Failure.Budget_spent] when it reaches a further candidate. A search that
    reaches a node with no candidate left is [Failure.Converged], whatever it
    spent. *)

val run :
  ?loc:Loc.t ->
  ?count:[ `Declared of int | `Config of int ] ->
  ?max_discard:int ->
  ?examples:'a list ->
  ?summary:('a -> string option) ->
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
      the final node of the search or the failing example. It must be
      [Some line] iff [gen] prints [v] as a table (see {!Failure.Property}).
      Defaults to [Fun.const None].

    {b Examples.} The examples run first, in order, unshrunk and without a seed.
    A passing example commits its labels and counts in [cases], a discarding one
    counts in [discards], and a failing one ends the run with [examples = true].

    {b Generated cases.} [index] counts from zero and counts the discarded
    cases, so a discarded seed is never drawn again. The run ends when [count]
    cases have passed. It gives up as soon as more than [max_discard] cases are
    discarded, the examples included, so a [max_discard] of [0] gives up on the
    first discard. It gives up even when [count] is already met, as under
    [~count:0] with examples that discard past the budget.

    A [gen] that discards discards the case, and one that raises any other
    control raises it through [run]. A fault of [gen] fails the case unshrunk,
    with [<generator raised before producing a value>] as its counterexample and
    the fault as its [inner].

    {b Shrinking.} A generated case that fails is shrunk by a search that
    descends the tree of its sample. At each node the search runs [law] on the
    candidates in order and moves to the first that fails in the class of the
    first failure, either a [Failure.Check_failure] or any other exception. The
    two failures need not be equal. A candidate that passes or raises a control
    other than [`Timeout] is rejected.

    The search ends at a node with no accepted candidate, [Failure.Converged].
    It also ends when it has run [law] {!shrink_budget} times,
    [Failure.Budget_spent], and when the forcing of a candidate raises anything
    but a [`Timeout]: [Failure.Candidate_raised] with that exception as
    [Failure.exn_to_string] prints it.

    A [`Timeout] raised anywhere in the search ends it as well,
    [Failure.Timed_out] with the limit. The failure then describes the last
    accepted node, and the test does not time out. [case_index] is always that
    of the first failure, so a replay descends the same path, and a timeout
    changes only where on that path the descent stops.

    Raises [Invalid_argument] if [count] or [max_discard] is negative, inside
    the running test, where [run] executes. Raises a [Failure.Control] other
    than [`Discard] and [`Timeout] when [law] or [gen] raises it outside the
    search, and no outcome then exists. A control delivered while the
    counterexample is formatted does not leave [run], since the guard of
    [Gen.Engine.render] turns it into text. *)
