(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Stateful property testing: a property over a generated sequence of calls.

    A {{!type:command}command} is one operation of a system under test, and
    bundles four things: its argument generator, its precondition, its pure
    transition on a {e model} of the state, and a body that calls the system and
    asserts with windtrap's ordinary verbs. {!stateful} declares a property over
    {e programs} — sequences of calls drawn from a command list. It is
    [Runner.prop] — the seam behind {!Windtrap.prop} — over a derived generator
    with a derived body: the property engine, the failure payload, and every
    renderer are unchanged, so seeds and replay, [--prop-count], [--max-shrink],
    tags, timeouts, capture, [xfail] and the CI reporters all apply as they do
    to any property.

    Add [windtrap.stateful] next to [windtrap] in the stanza —
    [(libraries windtrap windtrap.stateful)] — and open this module beside
    [Windtrap] or spell {!stateful} qualified; [doc/manual/stateful-testing.md]
    is the worked chapter.

    {b The pipeline.} A program is drawn at a fixed length ([?steps]) from one
    weight-1 [Gen.frequency] branch per command, {e repaired} against the model
    before the shrink tree is assembled ([Gen.list_exact]'s [?keep]), and shrunk
    by that tree's structural move set. Repair keeps a call iff its [~pre] holds
    in the model the calls before it produced, and threads [~next] through the
    calls it keeps. Two consequences are the design: the program shown is the
    program that ran — a call whose precondition does not hold is not in the
    program at all, not skipped at runtime — and shrinking only deletes calls
    and reduces arguments. It never substitutes one command for another and
    never invents one: every call of every candidate, everywhere in the tree, is
    a call the drawn program made, with its argument only reduced (see
    {!program} for the exact strength of that guarantee).

    {b [~pre] and [~next] must be pure, and ['model] must be persistent.} The
    model trajectory is folded three times per case — by repair when the program
    is drawn, by {!execute} when it runs, and by the printer when a
    counterexample renders — and the three must agree. A model implemented as a
    mutable structure returned unchanged corrupts generation before the test
    runs; if the state is genuinely a hashtable, model it as a [Map].

    {b Repair is total.} It evaluates user code on states the program will never
    execute, and an exception escaping it would escape the {e generator}, where
    both of the engine's escape hatches destroy the report: sampling fails the
    case with [<generator raised before producing a value>] and nothing shrinks,
    and forcing a candidate abandons the entire remaining sibling sequence while
    the report claims a converged, minimal counterexample. So a [~pre] or
    [~next] that raises {e poisons} the program instead — repair stops at that
    step, keeps it as the program's last call, and records the command, the
    step, the phase and the original exception. Executing a poisoned program
    runs the prefix and then fails, in the assertion class, naming all four; the
    search therefore minimises the specification bug.

    Only three things escape repair, and all three are about the {e run} rather
    than about the model: [Failure.Timeout], [Failure.Exit_attempt], and the
    three [Failure.is_fatal] exceptions. Everything else poisons — including
    [Failure.Check_failure], [Failure.Skip_test] and [Property.Discard], which
    {!execute} does {e not} convert when a body raises them. The asymmetry is
    deliberate: a body runs on the program that was drawn, where an assertion is
    the point, a skip means the run is unsupported and [assume] declines a case.
    Repair runs at generation time over states nothing may ever execute, and
    there none of the three means what it says — each is the model being written
    wrong, which is what poisoning reports. Assert in a body, where the report
    is made for it. *)

module Gen := Windtrap_gen.Gen
module Loc := Windtrap.Private.Loc

(** {1:commands Commands} *)

type ('model, 'sut) command
(** The type for one operation of the system under test: an argument generator,
    a precondition, a model transition, and a body. Abstract, and heterogeneous
    in its argument type — a command list holds commands whose arguments have
    different types. *)

val command :
  ?pos:Windtrap.pos ->
  ?pre:('model -> 'arg -> bool) ->
  string ->
  'arg Gen.t ->
  next:('model -> 'arg -> 'model) ->
  ('model -> 'arg -> 'sut -> unit) ->
  ('model, 'sut) command
(** [command name gen ~next body] is the call named [name] whose argument comes
    from [gen], which moves the model as [next] says, and which does [body].
    Every function takes the model first, then the argument, then (for [body])
    the system: expected precedes actual, always. [body] sees the model
    {e before} its own transition — the pre-state, which is what a postcondition
    needs.

    - [pre m arg] is whether the call is legal in model [m]. Defaults to
      [fun _ _ -> true], which is true by construction for most commands. A
      precondition does not only exclude illegal calls, it {e selects} a rare
      state: a command interesting only at capacity is generated only at
      capacity, so a system that should raise on an illegal call is a command
      whose [~pre] selects the illegal state and whose body asserts the raise.
    - [next m arg] is the model after the call. It is required, because its
      absence would be the claim {e this call does not change the model}, and
      that claim is false silently and vacuously: with an identity transition
      the model never grows, every other command's precondition fails, and the
      test becomes a green sequence of one command asserting nothing. Read-only
      commands say so with [~next:Fun.const].
    - [body m arg sut] calls the system and asserts. A body that asserts nothing
      is checked only by [stateful]'s [?invariant].

    [name] identifies the command in reports and nowhere else; newlines in it
    are replaced by spaces so that a step stays one row.

    [pos] is the declaration site, defaulting to a best-effort capture here
    rather than at the failure. A body is idiomatically one assertion in tail
    position, whose frame is gone by the time it raises, so a capture there
    answers [None] and the step reports no location at all; the command's own
    site is both available and the line a reader wants. It fills in only where
    the assertion recorded none — a body that did keep its own site keeps it,
    being nearer the failure. *)

val call :
  ?pos:Windtrap.pos ->
  ?pre:('model -> bool) ->
  string ->
  next:('model -> 'model) ->
  ('model -> 'sut -> unit) ->
  ('model, 'sut) command
(** [call] is {!command} for an operation with no generated argument — most of
    them, in most APIs. It is {!command} at ['arg = unit] over [Gen.unit], and
    its step prints as its name alone. Read-only commands spell [~next:Fun.id].
*)

(** {1:programs Programs}

    A program is a repaired sequence of calls together with the model they start
    from. {!stateful} wires the three functions below together; they are
    separate so that generation, printing and execution can be tested one at a
    time. *)

type ('model, 'sut) program
(** The type for one repaired program: the initial model, the calls it makes in
    order, and — when a [~pre] or [~next] raised — the poison mark that
    truncated it. Every call in a program is one whose precondition held in the
    model its predecessors produced. *)

val program :
  ?steps:int ->
  ?pp_model:(Format.formatter -> 'model -> unit) ->
  model:'model ->
  ('model, 'sut) command list ->
  ('model, 'sut) program Gen.t
(** [program ~model commands] generates programs over [commands] starting from
    [model].

    [steps] is how many calls are {e drawn} per program; repair removes the ones
    whose precondition does not hold, so a program makes at most [steps] calls.
    It defaults to [20] — a work budget, not a fact about state machines: a
    failing test re-runs a whole program per shrink candidate and a node offers
    roughly [2 * steps] of them, so command executions on the failing path grow
    with the square of [steps]. The length is fixed rather than drawn because a
    drawn length would be a second shrink dimension competing for the same
    budget, and chunk deletion subsumes it.

    {b Monotonicity, and how far it reaches.} Repair runs on the drawn calls
    {e before} the shrink tree is assembled, so only executable calls have
    subtrees at all and the drawn program is a fixed point of repair. Its
    immediate candidates are therefore strictly monotone: each is the drawn
    program with calls deleted and arguments reduced, none repeats it, and none
    is longer. Deeper, one clause of that goes: repair is a causal left fold
    applied afresh at every node, so under a node whose own repair dropped a
    call, deleting an earlier call can re-legalise the dropped one, and reducing
    a dropped call's argument changes nothing at all. A deep candidate can
    therefore repeat its parent's program or make a call its parent did not —
    and the engine compares failure classes, never programs, so a repeat is
    accepted as a shrink step and the reported [shrunk N steps] counts it. Every
    call is still one the drawn program made with its argument only reduced, so
    the search never leaves that program's vocabulary. Closing the gap needs the
    program re-assembled at every node, which is a different design.

    The generator prints, always: [Gen.prints] holds for the result, so a
    printerless stateful counterexample is unreachable and the report's
    [Gen.with_pp] remedy line never fires here. A program renders as a summary
    line — ["5 calls, last: pop"], or ["(no commands)"] for the empty program —
    followed by one numbered line per step: the command's name and its argument
    through [Gen.render_value], preceded by the model {e before} the step when
    [pp_model] is given. The printer bounds itself and emits hard newlines only:
    an argument that renders as ["()"] is omitted, arguments are cut at 200
    bytes (with a marker stating the original size) and model cells at 60 code
    points (each flattened to one line), a [pp_model] that raises costs its own
    cell and no more, and a program longer than 40 steps prints its first and
    last 20 with a ["… (N steps omitted)"] line between. Both columns are
    measured over the rows that print. An argument whose own generator has no
    printer renders as ["<no printer>"]; the step names and the program shape
    survive.

    Sampling raises [Invalid_argument] if [commands] is empty or if [steps] is
    negative — inside the running test's exception boundary, where every other
    malformed generator argument is reported.

    Sampling and forcing candidates propagate whatever [~pre] and [~next] raise
    from the set {!execute} does not convert; every other exception poisons the
    program instead of escaping (see the module preamble). *)

val command_names : ('model, 'sut) program -> string list
(** [command_names program] is the name of each call [program] makes, in order.
    It is the repair fold's result read directly, without the printer. *)

val execute :
  ?loc:Loc.t ->
  ?invariant:('model -> 'sut -> unit) ->
  ?teardown:('sut -> unit) ->
  setup:(unit -> 'sut) ->
  ('model, 'sut) program ->
  unit
(** [execute ~setup program] runs [program] against a system built by [setup],
    and returns [()] iff every body, every invariant check and [teardown]
    succeeded. [setup] runs once per [execute] — so once per generated case
    {e and} once per shrink candidate, since the search re-runs the program and
    a shared system would make it meaningless — and [teardown] releases on every
    path [execute] leaves.

    [invariant m sut] checks the state itself, as opposed to what a call
    returns: it runs on the fresh system before step 1, which is what makes the
    empty program a real test, and after every step. Absent, it means
    {e I make no claim about the state} — weaker than a false claim, so it is
    optional where [~next] is not.

    {b Failure class.} A body's exception is re-raised as a
    [Failure.Check_failure] carrying the payload the property engine would have
    built for it, so a descent never has to cross the engine's two acceptance
    classes and stall. Untouched, in bodies and in invariants alike:
    [Failure.Check_failure], [Failure.Skip_test], [Failure.Timeout],
    [Failure.Exit_attempt], [Property.Discard], and the three [Failure.is_fatal]
    exceptions. [Failure.Check_failure] is already the class the narrowing aims
    at; the rest are statements about the run rather than about this program —
    converting a skip would make it a reported counterexample, converting a
    discard would break [assume] inside a body, and converting a timeout would
    defeat the shrink search's deadline.

    {b Attribution.} A [Failure.Check_failure] leaving a step is re-raised with
    its [msg] slot naming the step: ["step 3 of 5: pop"], or
    ["invariant after step 3 of 5: pop"] for the check that follows the step, or
    ["step 3 of 3: close — ~pre raised"] for a poisoned program's last step, or
    ["invariant on the fresh system"]. A user [?msg] is flattened to one line
    and joined onto it, since the slot renders as one line. [loc] is stamped on
    the poisoned-program failure only — every other failure carries the site of
    the assertion that produced it, and a poisoned program's has none, so
    callers pass the [stateful] declaration site.

    {b The poisoned step.} A [~pre] poison means the call is not known to be
    legal, so its body does not run. A [~next] poison means [~pre] held and only
    the model {e after} the call is unknown, so the body does run, under the
    step's own attribution and before the poison is reported — and a failure of
    that body is the reported failure, the poison surfacing on some other case
    instead. No invariant check follows a poisoned step: there is no model to
    check it against.

    {b Teardown.} [teardown] is never composed with the body through
    [Fun.protect]: that raises [Fun.Finally_raised] {e in place of} the work
    exception, replacing a counterexample's assertion with a cleanup error and
    hiding a [Failure.Timeout] from the engine's shrink acceptance — an alarm
    delivered inside a candidate's teardown would then be accepted as a shrink
    step and reported as a converged, minimal counterexample. So a teardown
    failure is reported only when the body succeeded; on the failing path the
    teardown's own exception is dropped, except for [Failure.Timeout] and the
    [Failure.is_fatal] set, which end the run and outrank the failure in hand. A
    [setup] that raises propagates unconverted, and no teardown is owed. *)

(** {1:declaring Declaring} *)

val stateful :
  ?pos:Windtrap.pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?count:int ->
  ?steps:int ->
  ?pp_model:(Format.formatter -> 'model -> unit) ->
  ?invariant:('model -> 'sut -> unit) ->
  ?teardown:('sut -> unit) ->
  string ->
  model:'model ->
  setup:(unit -> 'sut) ->
  ('model, 'sut) command list ->
  Windtrap.test
(** [stateful name ~model ~setup commands] declares a property test: for every
    generated program over [commands], executing it against a system built by
    [setup] must leave every body's assertions and every [?invariant] check
    satisfied.

    It is [Runner.prop] over {!program} with {!execute} as its law, so
    [timeout], [count] and the run's [--prop-count] / [--max-shrink] /
    [--max-discard] knobs behave exactly as on a property; [steps], [pp_model]
    are {!program}'s and [invariant], [teardown] are {!execute}'s. The declared
    tags are extended with ["prop"] — so [--tag prop] selects stateful tests
    with every other property, and the run header prints the root seed — and
    ["stateful"], so a suite can select or exclude them on their own cost
    profile. [pos] fixes the declaration site, which is where a poisoned
    program's failure is reported: [command] records no position of its own, and
    a command's name is its identity in the report.

    [setup] should mint what it needs and [teardown] remove it:
    {!Windtrap.temp_dir} is {e test}-scoped and the wrong tool here, because a
    failing test builds one system per shrink candidate.

    There is no [?examples]: the program type is abstract, so a user cannot
    spell one, and a shrunk counterexample is copied back as a plain test. There
    is no [?retries] either: the shrink search re-runs the same program dozens
    of times and reports the one it converged on, so a system whose behaviour
    varies run to run is out of contract — retrying would hide that rather than
    settle it.

    The test fails at its first sample with [Invalid_argument] if [commands] is
    empty or [steps] is negative. *)
