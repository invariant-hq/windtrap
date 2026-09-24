(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Stateful property testing.

    A {{!type-command}command} is one operation of a system. It holds a
    generator of its argument, a precondition and a pure transition on a model
    of the state, and a body that calls the system and asserts. A
    {{!type-program}program} is a sequence of calls, each legal in the model
    that the calls before it produced. {!Property} and the renderers know a
    program only as a printed table and its one-line summary, so seeds, replay,
    [--prop-count], the shrink budget, tags and timeouts are those of any
    property.

    {b Repair.} {!val-program} draws a fixed number of calls and repairs them
    against the model. A call is kept iff its [pre] holds in the model that the
    kept calls before it produced, and [next] threads through the kept calls
    only. Repair runs on the drawn calls before the shrink tree is assembled,
    and again at every node of the tree. {!execute} evaluates no [pre], so the
    program that a failure shows is the program that ran.

    {b Purity.} [pre] and [next] must be pure, and ['model] persistent. The
    trajectory of the model is folded when a program is drawn, at every node of
    its shrink tree, when it runs, and when a failing one is printed. The folds
    must agree, since {!execute} and the printer apply [next] unguarded to the
    models that repair already applied it to. A mutable model that [next]
    returns unchanged corrupts generation before any program runs, so a hash
    table is modelled as a [Map].

    {b Exceptions.} In a body and in an invariant, [Failure.Check_failure], a
    skip and a discard keep the meaning they have in any law (see {!execute}).
    [pre] and [next] run at generation time, over models that no program may
    ever run in, and there none of the three means what it says. Raised by [pre]
    or [next], they are wrapped as any other exception is (see {!val-program}).

    {b Output.} This module prints nothing. The text of a program and its
    summary ride the {!Failure.Property} payload, and the label of the failing
    call rides the [msg] of the inner failure. *)

(** {1:commands Commands} *)

type ('model, 'sut) command
(** The type for one operation of a system ['sut] modelled by ['model]. The type
    of the argument is existential, so one list holds commands whose arguments
    differ in type. *)

val command :
  ?__POS__:Loc.pos ->
  ?pre:('model -> 'arg -> bool) ->
  string ->
  'arg Gen.t ->
  next:('model -> 'arg -> 'model) ->
  ('model -> 'arg -> 'sut -> unit) ->
  ('model, 'sut) command
(** [command name gen ~next body] is the operation [name], whose argument [gen]
    draws. Every function takes the model first, then the argument, then, for
    [body], the system.
    - [pre m arg] is whether the call is legal in the model [m]. Defaults to
      [fun _ _ -> true].
    - [next m arg] is the model after the call.
    - [body m arg sut] calls the system and asserts. [m] is the model before the
      call. A body that asserts nothing is checked by {!stateful}'s [invariant]
      alone.
    - [name] identifies the command in the printed program, in its summary and
      in the label of the failing call. Its newlines become spaces.
    - [__POS__] is the declaration site. It defaults to a capture at this call
      (see {!Loc.resolve}), never at the failure. A failing call reports it when
      its failure recorded no location, as a body that is one assertion in tail
      position records none. Nothing is checked at declaration. *)

val call :
  ?__POS__:Loc.pos ->
  ?pre:('model -> bool) ->
  string ->
  next:('model -> 'model) ->
  ('model -> 'sut -> unit) ->
  ('model, 'sut) command
(** [call name ~next body] is {!val-command} at ['arg = unit] over {!Gen.unit}.
    [pre], [next] and [body] take no argument. The printer of programs omits an
    argument whose text is [()], which is how {!Gen.unit} prints, so a call of
    it prints as its name alone. [__POS__] is forwarded. Without it,
    {!val-command}'s capture walks past the frames of this function and lands on
    the caller. *)

(** {1:programs Programs}

    {!stateful} wires the three functions below together. *)

type ('model, 'sut) program
(** The type for a repaired program: an initial model and the calls made from
    it, in order, each legal in the model that the calls before it produced. It
    has no constructor. A program comes from {!val-program} only. *)

val program :
  ?steps:int ->
  ?pp_model:(Format.formatter -> 'model -> unit) ->
  model:'model ->
  ('model, 'sut) command list ->
  ('model, 'sut) program Gen.t
(** [program ~model commands] generates the programs over [commands] that start
    from [model].
    - [steps] is the number of calls drawn. Defaults to [20].
    - [pp_model] adds a column to the printed program, the model before each
      call.

    Every call picks its command with equal probability, whatever the order of
    [commands].

    {b Shrinking.} The shrink tree is [Gen.Engine.Shrink_tree.list] over the
    trees of the kept calls, with repair applied again at every node. A
    candidate deletes calls or reduces one argument. The choice of a command
    does not shrink (see {!Gen.frequency}), so every call of a candidate is one
    that the drawn program made, its argument at most reduced.

    No immediate candidate of the drawn program is longer than it, and the
    guarantee stops there. Under a node whose repair dropped a call, a candidate
    that deletes that call or reduces its argument can repeat the program of its
    parent. A candidate that deletes an earlier call can make the dropped call
    legal again, and then be longer than its parent. {!Property.run} compares
    failures and never programs, so it accepts such a repeat as a step, and
    [shrunk N steps] counts it.

    A search runs one whole program, and calls [scope] once, per candidate. A
    program of [n] calls has about [2 * n] structural candidates, so the calls
    that a search executes grow with the square of [steps].

    {b Printing.} The generator always prints, so a program is never a
    pre-image. The empty program prints as [(no commands)]. Any other prints as
    a table: the header row [ #  model before  call], or [ #  call] without
    [pp_model], then one row per call with its number, the model before it, and
    the name of the command followed by its argument.
    - An argument prints through [Gen.Engine.render_value]. It has no pre-image,
      and without a printer it is the placeholder, the argument of a {!Gen.map}
      included. It is flattened to one line and cut at [200] bytes, with a
      marker that gives its size. An argument that prints as [()] is omitted.
    - A model cell is flattened to one line and cut at [60] code points, the
      last three being [...]. A [pp_model] that raises, whatever the exception,
      costs its own cell, which reads [<pp_model raised EXN>].
    - A program of more than [40] calls prints its first [20] and its last [20]
      around the line [… (N calls omitted)].

    Sampling raises [Invalid_argument] if [commands] is empty, under [~steps:0]
    too, or if [steps] is negative. Both messages name [Windtrap.stateful].

    A [pre] or a [next] that raises escapes repair wrapped in an exception that
    this interface does not export. It prints as
    [call 3: close, ~pre raised Failure("nth")], the number counting the kept
    calls from one, and it keeps the original backtrace. A [Failure.Control] of
    [`Timeout] or [`Exit] and the [Failure.is_fatal] exceptions escape as
    themselves. The exception escapes [Gen.Engine.sample] when the drawn program
    is repaired, and the forcing of a candidate when a candidate is (see
    {!Property.run} for what becomes of each). *)

val summary : ('model, 'sut) program -> string option
(** [summary program] is [program]'s table in one line, as in
    [5 calls, last: pop] or [1 call, last: pop], and [None] for the empty
    program, which prints no table. It names the last call and never the failing
    one, since a program does not know which call failed. The label of
    {!execute} names that call. *)

val execute :
  ?loc:Loc.t ->
  ?invariant:('model -> 'sut -> unit) ->
  scope:(('sut -> unit) -> unit) ->
  ('model, 'sut) program ->
  unit
(** [execute ~scope program] runs [program] against the system that [scope]
    hands to its callback. It returns [()] iff every body, every check of the
    invariant and [scope] itself succeeded.
    - [scope] must call its callback once, with a fresh system. It runs once per
      generated case and once per shrink candidate, so the system must behave
      the same from run to run.
    - [invariant m sut] runs on the initial model and the fresh system before
      the first call, which makes the empty program a test. It then runs after
      every call, on the model that follows it.
    - [loc] locates the failure of a scope that never called back.

    Every call runs its body on the model before it, then applies [next].

    {b Failures.} An exception of a body or of the invariant is raised as a
    [Failure.Check_failure], which keeps it in the acceptance class of an
    assertion failure. Its payload is the one that {!Property.run} builds for an
    exception. [Failure.Check_failure], [Failure.Control] and the
    [Failure.is_fatal] exceptions pass as they are.

    The [msg] of a [Failure.Check_failure] gets a label that names the call:
    [call 3 of 5: pop], [invariant after call 3 of 5: pop] or
    [invariant on the fresh system]. The assertion's own [msg] follows the label
    after ["; "], flattened to one line. On the fresh system there is no
    command, and the failure keeps no location of its own.

    {b The scope.} When [scope] does not call back once, or raises, [execute]
    ends as follows:
    - When [scope] returns without calling back, the case fails with a
      [Failure.Check_failure] at [loc].
    - A second call runs nothing and raises [Invalid_argument]. [execute] raises
      that exception whatever else the case has to say, even when [scope]
      swallows it.
    - The exception of a failing program is raised through [scope], and
      [execute] raises it again when [scope] swallows it.
    - What [scope] raises over a failing program is dropped, unless it is a
      [Failure.Control (`Timeout _)] or a [Failure.is_fatal] exception, which
      replaces the failure of the program.
    - What [scope] raises before it calls back, or after a passing program,
      propagates as it is. *)

(** {1:declaring Declaring} *)

val stateful :
  ?__POS__:Loc.pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?count:int ->
  ?steps:int ->
  ?pp_model:(Format.formatter -> 'model -> unit) ->
  ?invariant:('model -> 'sut -> unit) ->
  string ->
  model:'model ->
  scope:(('sut -> unit) -> unit) ->
  ('model, 'sut) command list ->
  Test_tree.t
(** [stateful name ~model ~scope commands] is the property test [name] over the
    programs of [commands]. It is {!Run.prop} over
    [program ?steps ?pp_model ~model commands], with
    [execute ?loc ?invariant ~scope] as its law and {!val-summary} as its
    summary.
    - [timeout] and [count] are {!Run.prop}'s, and so is [--prop-count].
    - [steps] and [pp_model] are {!val-program}'s. [scope] and [invariant] are
      {!execute}'s.
    - [__POS__] is the declaration site, resolved once at this call. It is the
      site of the test and {!execute}'s [loc].
    - ["prop"] and ["stateful"] are always added to [tags].

    It takes no [examples], since a program cannot be written by hand, no
    [max_discard], the budget being {!Property.run}'s default, and no [retries],
    since a second attempt would replay the same programs from the root seed.

    Raises [Invalid_argument] if [timeout] is given and is not finite and
    positive, as every constructor of {!Test_tree} does. An empty [commands] and
    a negative [steps] raise at the first sample, inside the running test (see
    {!val-program}). With [~count:0] no sample is drawn and nothing raises. *)
