(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(** The property engine: run one property over a generator.

    {!run} checks a body over explicit examples, then over generated cases under
    per-case derived seeds, with discard bookkeeping, greedy integrated
    shrinking and label accumulation, and returns a typed {!outcome}. The engine
    draws no randomness of its own: case [index] of the test at [path] generates
    from [Seed.derive ~root ~path ~index], so an outcome is a pure function of
    {!run}'s arguments whenever the generator's callbacks and the body are pure
    (guarantee 7).

    A body returns [()] to pass and raises to fail; [Failure.Skip_test] skips
    the whole test, and a [Failure.Timeout] raised while a counterexample is
    still being searched for times it out. The shrink search re-runs the body on
    candidate inputs, so bodies must be deterministic. Nothing in this module is
    global: labels go through the {!context} {!run} passes to the body. *)

(** {1:discarding Discarding} *)

exception Discard
(** Raised inside a property body to discard the current case; the engine counts
    it and moves on. Prefer {!assume} and {!reject}. Generation-time discards
    raise [Gen.Private.Rejected] instead; both count together. *)

val assume : bool -> unit
(** [assume cond] is [()] if [cond] and raises {!Discard} otherwise. For rare,
    cheap preconditions: heavy discarding exhausts the discard budget and the
    property gives up (see {!run}). *)

val reject : unit -> 'a
(** [reject ()] raises {!Discard}. *)

(** {1:labelling Labelling}

    Per-case marks accumulate in the {!context} and commit when the case passes;
    discarded and failing cases contribute nothing, and shrink re-runs
    accumulate into a scratch context that is thrown away. *)

type context
(** The type for one {!run}'s label accumulator, passed to the body and invalid
    outside that run. *)

val collect : context -> string -> unit
(** [collect ctx label] marks [label] for the current case. A committed case
    counts a label at most once; the distribution is {!stats.collected}. *)

val classify : context -> string -> bool -> unit
(** [classify ctx label cond] is [collect ctx label] when [cond] and [()]
    otherwise. *)

val cover : context -> string -> bool -> unit
(** [cover ctx label cond] is {!classify}[ ctx label cond] plus the demand that
    [label] be marked by at least one passing case. The demand registers on the
    first call, whether or not [cond] holds, and is judged at the end of a run
    that completes its case count: an unmarked label makes the outcome
    {!Coverage_failed}. A [cover] the run never reaches registers nothing. *)

(** {1:outcomes Outcomes} *)

type cover_status = {
  label : string;  (** The demanded label. *)
  hits : int;  (** Passing cases that marked it. *)
  satisfied : bool;  (** Whether [hits] is above zero. *)
}
(** The type for the end-of-run state of one {!cover} label. *)

type stats = {
  cases : int;
      (** Cases that ran the body to completion and passed, committed examples
          included. *)
  discards : int;
      (** Discarded cases, from the body and from generation, examples included.
      *)
  collected : (string * int) list;
      (** The label distribution over passing cases, sorted by label; {!cover}
          labels mark here too. *)
  coverage : cover_status list;
      (** One entry per {!cover} label, sorted by label. *)
}
(** The type for a run's bookkeeping, as of the moment the outcome was decided;
    [coverage] is judged only on the {!Pass}/{!Coverage_failed} boundary. *)

(** The type for engine results. *)
type outcome =
  | Pass of stats  (** Every case passed and every {!cover} label was hit. *)
  | Fail of { failure : Failure.t; stats : stats }
      (** A case failed. [failure] carries a [Failure.kind.Property] payload:
          the rendered (shrunk) counterexample, the failing case index, the
          shrink step count, the root seed, whether the case was an explicit
          example, and the inner failure: the body's own [Check_failure]
          payload, or a [Failure.kind.Raise] payload rendering an uncaught
          exception and its backtrace. *)
  | Coverage_failed of stats
      (** The full case count passed but a [coverage] entry is unsatisfied. *)
  | Gave_up of stats
      (** More than [max_discard] cases were discarded before the case count was
          reached. *)

(** {1:running Running} *)

val shrink_budget : int
(** [shrink_budget] is the accepted steps a shrink search may take, [10_000].
    Fixed, so a replay under the same root reaches the same node; a search that
    spends it is reported as stopped ([shrink_exhausted]), not minimal. *)

val run :
  ?loc:Loc.t ->
  ?count:[ `Declared of int | `Config of int ] ->
  ?max_discard:int ->
  ?examples:'a list ->
  root:Seed.seed ->
  path:string ->
  'a Gen.t ->
  (context -> 'a -> unit) ->
  outcome
(** [run ~root ~path gen body] checks [body] over [gen] and is the {!outcome}.
    [path] is the test's path in the suite, [root] the run's root seed, [loc]
    the property's declaration site, stamped on a failure. [count] is the number
    of generated cases and where it came from: a [`Config n] count is restated
    in the failure's replay line, a [`Declared n] count replays by itself.
    Defaults: [count] is [100], [max_discard] is [2 * count] (clamped to
    [max_int]), [examples] is [[]].

    {b Examples} run first, unshrunk, numbered from zero separately from
    generated cases; a failing example has [shrink_steps = 0], [examples = true]
    and [rendered] the value through the generator's printer (the placeholder
    when it has none). Examples consume no seeds; passing ones commit their
    labels and count in {!stats.cases}, discarding ones in {!stats.discards}.

    {b Generated case} [index], zero-based and counting discarded attempts,
    samples [gen] from [Seed.make (Seed.derive ~root ~path ~index)]. The run
    succeeds when [count] generated cases pass and gives up when more than
    [max_discard] cases have been discarded, examples included; the budget is
    checked before the case-count goal. A generator raising
    [Gen.Private.Rejected] discards the attempt; one raising anything else fails
    the case with [<generator raised before producing a value>] as the
    counterexample and the exception as the inner failure.

    {b Shrinking} descends the failing sample's tree greedily: at each node the
    body re-runs on the candidates in order and the search moves to the first
    one that fails in the same way, a [Check_failure] for a [Check_failure] and
    any other exception for any other. Passing, discarded and skipping
    candidates are rejected. Descent stops at a node with no accepted candidate,
    after {!shrink_budget} accepted steps, when forcing a candidate raises, or
    on a [Failure.Timeout] anywhere in the search, which reports the node in
    hand with the failure's [timed_out] field set. The reported counterexample,
    inner failure and [shrink_steps] describe the final node; [case_index] stays
    the original failing index, so a replay re-derives the same descent.

    Raises [Invalid_argument] if [count] or [max_discard] is negative, inside
    the running test's boundary. Re-raises [Failure.Skip_test] from the body
    unchanged, and [Failure.Timeout] from everywhere but the shrink search. *)
