(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC

  The case loop structure and label bookkeeping derive from windtrap v1's
  prop/prop.ml, rebuilt over Seed (per-case derivation), Gen (integrated
  shrinking), and Failure (typed counterexamples).
  --------------------------------------------------------------------------*)

(** The property engine: run one property over a generator.

    {!run} drives a single property: explicit examples first, then generated
    cases under per-case derived seeds, with discard bookkeeping, greedy
    integrated shrinking, and per-case label accumulation. It returns a typed
    {!outcome} — a counterexample is a {!Failure.t} with a
    [Failure.kind.Property] payload — which the runner classifies and renderers
    project (RFC Law 4).

    {b Determinism.} The engine performs no random operation of its own. Case
    [index] of the test named [path] generates from
    [Seed.derive ~root ~path ~index] (RFC Law 7), so an outcome is a pure
    function of {!run}'s arguments whenever the generator's callbacks and the
    body are pure — suite composition never perturbs a property's stream, and a
    recorded root token replays every failure.

    {b Bodies.} A body is [unit]-returning and uses the ordinary assertion
    vocabulary; raising [Failure.Check_failure] fails the case, as does any
    other exception. The control exception [Failure.Skip_test] is re-raised
    unchanged, skipping the whole test; a shrink candidate that raises it is
    instead a rejected candidate — a candidate skip never turns a recorded
    failure into a skip (see {!run}, Shrinking). [Failure.Timeout] is re-raised
    unchanged only while the engine is still searching for a counterexample
    (examples, generation, a case's first body run), timing out the whole test;
    once a generated case has failed, a [Failure.Timeout] ends the shrink search
    instead, and the failure in hand is reported with its [timed_out] mark (see
    {!run}, Shrinking). The shrink search re-runs the body on candidate inputs,
    so bodies must be deterministic and repeatable.

    {b Ambient wiring (plan).} This module keeps the labelling {!context}
    explicit: {!run} passes it to the body, and {!collect}, {!classify}, and
    {!cover} take it as their first argument. The public [('a -> unit)] body
    surface arrives with the run record (stage G4): the runner stores the
    running context in the run's single ambient slot and forwards the public
    [collect]/[classify]/[cover] to the functions here. No state in this module
    is global (RFC Law 9): every context belongs to one {!run} invocation. *)

(** {1:discarding Discarding} *)

exception Discard
(** Raised inside a property body to discard the current case; the engine counts
    it and moves to the next case. Prefer {!assume} and {!reject} to raising
    directly. Generation-time discards use [Gen.Rejected] instead; the engine
    counts both kinds together. *)

val assume : bool -> unit
(** [assume cond] is [()] if [cond] and raises {!Discard} otherwise. Discarding
    is for rare, cheap preconditions: heavy discarding exhausts the discard
    budget and the property {e gives up} (see {!run}). When the precondition is
    structural, constrain the generator instead. *)

val reject : unit -> 'a
(** [reject ()] raises {!Discard}, unconditionally discarding the current case.
*)

(** {1:labelling Labelling}

    Labels report the distribution of generated inputs; coverage requirements
    turn a distribution expectation into a failure. Per-case marks accumulate in
    the engine's {!context} and commit when the case {e passes}: discarded and
    failing cases contribute nothing, and shrink re-runs accumulate into a
    scratch context that is thrown away. *)

type context
(** The type for one {!run}'s label accumulator. Created by {!run} for each
    invocation and passed to the body; it is invalid outside that run. *)

val collect : context -> string -> unit
(** [collect ctx label] marks [label] for the current case. Each committed case
    counts a label at most once, however many times it was collected; the
    distribution is reported in {!stats.collected}. *)

val classify : context -> string -> bool -> unit
(** [classify ctx label cond] is [collect ctx label] when [cond] and [()]
    otherwise. *)

val cover : context -> label:string -> at_least:float -> bool -> unit
(** [cover ctx ~label ~at_least cond] requires [label] to be marked in at least
    [at_least] percent of the run's passing cases, and marks it (as {!collect}
    does, plus a coverage hit) when [cond] is true. The requirement registers
    even when [cond] is false — a label that never hits still reports.
    Thresholds are evaluated once, at the end of a run that completes its case
    count: unsatisfied requirements make the outcome {!Coverage_failed}.

    Raises [Invalid_argument] if [at_least] is not in \[[0.];[100.]\] (NaN
    included) or if [label] was already registered in this run with a different
    threshold. The engine treats an [Invalid_argument] from the body like any
    other exception: the case fails. *)

(** {1:outcomes Outcomes} *)

type cover_status = {
  label : string;  (** The requirement's label. *)
  required : float;  (** The [at_least] threshold, in percent. *)
  actual : float;
      (** The achieved percentage: [hits] over the passing cases counted so far
          ([0.] when no case passed). *)
  hits : int;  (** Passing cases that marked the label. *)
  satisfied : bool;
      (** Whether [actual] meets [required], up to a small tolerance ([1e-9]
          percentage points, absorbing float division noise). *)
}
(** The type for the end-of-run state of one {!cover} requirement. *)

type stats = {
  cases : int;
      (** Cases that ran the body to completion and passed — committed examples
          included. *)
  discards : int;
      (** Discarded cases: {!Discard} from the body plus [Gen.Rejected] at
          generation time, examples included. *)
  collected : (string * int) list;
      (** The label distribution over passing cases, sorted by label. Coverage
          hits appear here too. *)
  coverage : cover_status list;
      (** One entry per {!cover} label, sorted by label. *)
}
(** The type for a run's bookkeeping tables. Every outcome carries the tables as
    of the moment it was decided; percentages in [coverage] are judged only on
    the {!Pass}/{!Coverage_failed} boundary. *)

(** The type for engine results, consumed by the runner. *)
type outcome =
  | Pass of stats  (** Every case passed and every coverage threshold held. *)
  | Fail of { failure : Failure.t; stats : stats }
      (** A case failed. [failure] carries a [Failure.kind.Property] payload:
          the rendered (shrunk) counterexample, the failing case index, the
          shrink step count, the root seed, whether the case was an explicit
          example, and the inner failure — the body's own [Check_failure]
          payload when it raised one, or a [Failure.kind.Raise] payload (no
          expected side) rendering the uncaught exception and its backtrace
          otherwise. *)
  | Coverage_failed of stats
      (** The full case count passed but at least one [coverage] entry is
          unsatisfied. *)
  | Gave_up of stats
      (** More than [max_discard] cases were discarded before the case count was
          reached. *)

(** {1:running Running} *)

val run :
  ?loc:Loc.t ->
  ?count:int ->
  ?max_discard:int ->
  ?max_shrink:int ->
  ?examples:'a list ->
  root:Seed.seed ->
  path:string ->
  'a Gen.t ->
  (context -> 'a -> unit) ->
  outcome
(** [run ~root ~path gen body] checks [body] over [gen] and returns the
    {!outcome}. [path] is the test's path in the suite; [root] is the run's root
    seed. [loc] is the property's declaration site, stamped on the failure when
    one is produced. Defaults: [count] is [100], [max_discard] is [2 * count]
    (clamped to [max_int]), [max_shrink] is [100], [examples] is [[]].

    {b Examples first.} The [examples] values run before any generation,
    unshrunk (they are already the reviewed minimal form), and are numbered
    separately from generated cases. A failing example fails fast with
    [shrink_steps = 0], [examples = true], [case_index] its zero-based position
    in [examples], and [rendered] the generator's printing of the value when
    [gen] has a printer and [<example k>] (1-based [k], matching the
    [example k of n] report numbering) otherwise. Examples consume no seeds and
    are never recorded for replay; passing examples commit their labels and
    count in {!stats.cases} — the coverage denominator includes them — and a
    discarding example counts in {!stats.discards}.

    {b Generated cases.} Case [index] — zero-based, counting every generation
    attempt, discarded attempts included, so that each attempt draws fresh
    values — samples [gen] from [Seed.make (Seed.derive ~root ~path ~index)].
    The run succeeds when [count] generated cases pass; it {e gives up} when
    more than [max_discard] cases have been discarded — body discards and
    generation-time rejections together, discarding [examples] included — so
    [max_discard] of [0] gives up on the first discard. The budget is checked
    before the case-count goal: a run whose examples alone exceed it gives up
    even when [count] is [0]. A generator that raises [Gen.Rejected] (a
    [such_that] budget exhausted) discards the attempt; a generator that raises
    anything else fails the case with
    [<generator raised before producing a value>] as the rendered counterexample
    and the exception as the inner failure.

    {b Shrinking.} On a failing generated case the engine descends the sample's
    shrink tree greedily: at each node it re-runs the body on the candidates in
    order and moves to the first candidate that fails {e in the same way} — a
    case that failed by [Check_failure] accepts candidates failing by any
    [Check_failure], and a case that failed by any other exception accepts
    candidates failing by any non-assertion exception (v1 semantics: the failure
    need not be equal, only of the same kind). Passing, discarded, and skipping
    candidates are rejected. Descent stops at a node with no accepted candidate,
    after [max_shrink] accepted steps, or when forcing a candidate raises. A
    [Failure.Timeout] raised anywhere in the search — a candidate's body, a
    candidate's forcing, or the search's own bookkeeping — also stops it, at the
    last accepted node: the counterexample in hand is reported rather than
    discarded, and the failure's [timed_out] field carries the limit, marking
    the counterexample as possibly not minimal. The reported counterexample,
    inner failure, and [shrink_steps] describe the final node. [case_index]
    stays the original failing index: replaying the root reproduces the case and
    the search re-derives the same descent path — a timeout moves only the
    stopping point along that path, so a replay reports an equally or further
    shrunk value from the same descent, never a different case.

    Raises [Invalid_argument] if [count], [max_discard], or [max_shrink] is
    negative — inside the running test's boundary, since [run] executes there.
    Re-raises [Failure.Skip_test] from the body unchanged, and [Failure.Timeout]
    from everywhere except the shrink search, where it ends the search and marks
    the reported failure instead (see Shrinking). *)
