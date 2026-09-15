(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Stateful property testing: a property over a generated sequence of calls.

    A {{!type:command}command} is one operation of a system under test, and
    bundles four things: its argument generator, its precondition, its pure
    transition on a {e model} of the state, and a body that calls the system and
    asserts with the ordinary verbs. {!stateful} declares a property over
    {e programs} — sequences of calls drawn from a command list. It is
    {!Runner.prop} over a derived generator with a derived body: the property
    engine, the {!Failure} payload, and every renderer are unchanged, so seeds
    and replay, [--prop-count], [--max-shrink], tags, timeouts, capture, [xfail]
    and the CI reporters all apply as they do to any property.

    A program is drawn at a fixed length ([?steps]) and {e repaired} against the
    model before its shrink tree is assembled: a call is kept iff its [~pre]
    holds in the model the calls before it produced, and [~next] threads through
    the kept calls. The program shown is the program that ran, and shrinking
    only deletes calls and reduces arguments — it never substitutes or invents
    one.

    [~pre] and [~next] must be pure, and ['model] persistent: the model
    trajectory is folded when the program is drawn, when it runs, and when a
    counterexample prints, and the three must agree. A [~pre] or [~next] that
    raises is a specification bug: the case fails at the generator, unshrunk,
    with the exception, its backtrace, and the step and operation it raised at.
*)

(** {1:commands Commands} *)

type ('model, 'sut) command
(** The type for one operation of the system under test: an argument generator,
    a precondition, a model transition, and a body. Abstract, and heterogeneous
    in its argument type — a command list holds commands whose arguments have
    different types. *)

val command :
  ?pos:Loc.pos ->
  ?pre:('model -> 'arg -> bool) ->
  string ->
  'arg Gen.t ->
  next:('model -> 'arg -> 'model) ->
  ('model -> 'arg -> 'sut -> unit) ->
  ('model, 'sut) command
(** [command name gen ~next body] is the call named [name] whose argument comes
    from [gen], which moves the model as [next] says, and which does [body].
    Every function takes the model first, then the argument, then (for [body])
    the system. [body] sees the model {e before} its own transition.

    - [pre m arg] is whether the call is legal in model [m]. Defaults to
      [fun _ _ -> true].
    - [next m arg] is the model after the call. Read-only commands say so with
      [~next:Fun.const].
    - [body m arg sut] calls the system and asserts. A body that asserts nothing
      is checked only by [stateful]'s [?invariant].

    [name] identifies the command in reports; newlines in it become spaces.
    [pos] is the declaration site, captured here by default, and is what a
    failing step reports when its assertion recorded no location of its own. *)

val call :
  ?pos:Loc.pos ->
  ?pre:('model -> bool) ->
  string ->
  next:('model -> 'model) ->
  ('model -> 'sut -> unit) ->
  ('model, 'sut) command
(** [call] is {!command} for an operation with no generated argument: {!command}
    at ['arg = unit] over [Gen.unit], whose step prints as its name alone.
    Read-only commands spell [~next:Fun.id]. *)

(** {1:programs Programs}

    A program is a repaired sequence of calls together with the model they start
    from. {!stateful} wires the three functions below together; they are
    separate so that generation, printing and execution can be tested one at a
    time. *)

type ('model, 'sut) program
(** The type for one repaired program: the initial model and the calls it makes
    in order, each legal in the model its predecessors produced. *)

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
    It defaults to [20], a work budget: a failing test re-runs a whole program
    per shrink candidate, and candidates per node grow with [steps].

    Shrinking deletes calls and reduces arguments, with repair re-run at every
    node; a deep candidate may repeat its parent, but never makes a call the
    drawn program did not.

    The generator always prints: a summary line — ["5 calls, last: pop"], or
    ["(no commands)"] — then one numbered line per step, the command's name and
    its argument through the argument generator's printer — the placeholder when
    it has none, an argument being a bare value with no pre-image to render —
    preceded by the model {e before} the step when [pp_model] is given. A ["()"]
    argument is omitted; arguments are cut at 200 bytes and model cells at 60
    code points, one line each; a raising [pp_model] costs its own cell; a
    program over 40 steps prints its first and last 20 with a
    ["… (N steps omitted)"] line between.

    Sampling raises [Invalid_argument] if [commands] is empty or if [steps] is
    negative. A [~pre] or [~next] that raises escapes wrapped in an exception
    naming the operation, the step and the function —
    [step 3: close — ~pre raised Failure("nth")] — with the original backtrace:
    at sampling when the drawn program's repair raises, which fails the case,
    and at forcing when a candidate's does, which stops the shrink search
    ({!Property.run}, Shrinking). {!Failure.Timeout}, {!Failure.Exit_attempt}
    and the {!Failure.is_fatal} exceptions escape as themselves. *)

val execute :
  ?loc:Loc.t ->
  ?invariant:('model -> 'sut -> unit) ->
  scope:(('sut -> unit) -> unit) ->
  ('model, 'sut) program ->
  unit
(** [execute ~scope program] runs [program] against the system [scope] hands its
    callback, and returns [()] iff every body, every invariant check and the
    scope itself succeeded. [scope] runs once per [execute], with
    {!Test_tree.scoped}'s protocol: it must call its callback exactly once — a
    scope that returns without doing so fails the case with a {!Failure.Message}
    located at [loc], one that calls twice gets [Invalid_argument] at the second
    call — and the program's exception is re-raised {e through} it, so a release
    that raises over a failing program is dropped unless it is a
    {!Failure.Timeout} or a {!Failure.is_fatal} exception. What the scope raises
    before the callback, or after it returned from a passing program, propagates
    unconverted.

    [invariant m sut] runs on the fresh system before step 1 — which is what
    makes the empty program a real test — and after every step.

    A body's or invariant's exception is re-raised as a {!Failure.Check_failure}
    carrying the payload the property engine would have built for it, so a
    descent never crosses the engine's two acceptance classes; the control
    exceptions ({!Failure.Check_failure}, {!Failure.Skip_test},
    {!Failure.Timeout}, {!Failure.Exit_attempt}, {!Property.Discard}) and the
    {!Failure.is_fatal} set pass untouched. The failure's [msg] slot names the
    step — ["step 3 of 5: pop"], ["invariant after step 3 of 5: pop"],
    ["invariant on the fresh system"] — with a user [?msg] flattened and joined
    onto it, and a failure with no location of its own takes the command's
    declaration site. *)

(** {1:declaring Declaring} *)

val stateful :
  ?pos:Loc.pos ->
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
(** [stateful name ~model ~scope commands] declares a property test: for every
    generated program over [commands], executing it against the system [scope]
    builds must leave every body's assertions and every [?invariant] check
    satisfied.

    It is {!Runner.prop} over {!program} with {!execute} as its law, so
    [timeout], [count] and the run's [--prop-count] / [--max-shrink] knobs
    behave exactly as on a property; [steps], [pp_model] are {!program}'s and
    [scope], [invariant] are {!execute}'s. The declared tags are extended with
    ["prop"] and ["stateful"]. [pos] fixes the declaration site, which a scope
    that never ran the program reports.

    There is no [?examples] — a shrunk counterexample is copied back as a plain
    test — and no [?retries]: a program replays deterministically from the root
    seed. The test fails at its first sample with [Invalid_argument] if
    [commands] is empty or [steps] is negative. *)
