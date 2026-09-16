(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Stateful property testing: a property over a generated sequence of calls.

    A {{!type:command}command} bundles an argument generator, a precondition, a
    pure transition on a {e model} of the state, and a body that calls the
    system and asserts. {!stateful} is {!Run.prop} over a generator of
    {e programs}, sequences of calls repaired against the model so that every
    call is legal where it stands; seeds, replay, [--prop-count], the shrink
    budget, tags, timeouts and every renderer apply as to any property.

    [~pre] and [~next] must be pure and ['model] persistent: the model
    trajectory is folded when a program is drawn, run and printed, and the three
    must agree. *)

(** {1:commands Commands} *)

type ('model, 'sut) command
(** The type for one operation of the system under test. Heterogeneous in its
    argument type: a command list holds commands whose arguments differ. *)

val command :
  ?__POS__:Loc.pos ->
  ?pre:('model -> 'arg -> bool) ->
  string ->
  'arg Gen.t ->
  next:('model -> 'arg -> 'model) ->
  ('model -> 'arg -> 'sut -> unit) ->
  ('model, 'sut) command
(** [command name gen ~next body] is the call named [name] whose argument comes
    from [gen]. [pre m arg] is whether the call is legal in model [m] (default
    always); [next m arg] is the model after the call ([~next:Fun.const] for a
    read-only command); [body m arg sut] calls the system and asserts, seeing
    the model before its own transition. Newlines in [name] become spaces.
    [__POS__] is the declaration site, reported by a failing step whose
    assertion recorded no location of its own. *)

val call :
  ?__POS__:Loc.pos ->
  ?pre:('model -> bool) ->
  string ->
  next:('model -> 'model) ->
  ('model -> 'sut -> unit) ->
  ('model, 'sut) command
(** [call] is {!command} for an operation with no generated argument; its step
    prints as its name alone. Read-only commands spell [~next:Fun.id]. *)

(** {1:programs Programs}

    {!stateful} wires the two functions below together; they are separate so
    generation, printing and execution can be tested one at a time. *)

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
    [model]. [steps] (default [20]) is how many calls are drawn; repair drops
    the ones whose precondition does not hold, so a program makes at most
    [steps] calls. Shrinking deletes calls and reduces arguments, with repair
    re-run at every node; a deep candidate may repeat its parent, but never
    makes a call the drawn program did not.

    The generator always prints: a summary line (["5 calls, last: pop"] or
    ["(no commands)"]), then one numbered line per step with the command's name
    and its argument through the argument generator's printer (the placeholder
    when it has none: an argument is a bare value with no pre-image), preceded
    by the model before the step when [pp_model] is given. A ["()"] argument is
    omitted; arguments are cut at 200 bytes and model cells at 60 code points; a
    raising [pp_model] costs its own cell; a program over 40 steps prints its
    first and last 20 with a ["… (N steps omitted)"] line between.

    Sampling raises [Invalid_argument] if [commands] is empty or [steps] is
    negative. A [~pre] or [~next] that raises escapes wrapped in an exception
    naming the operation, step and function
    ([step 3: close — ~pre raised Failure("nth")]) with the original backtrace:
    at sampling it fails the case, at forcing it stops the shrink search.
    {!Failure.Timeout}, {!Failure.Exit_attempt} and the {!Failure.is_fatal}
    exceptions escape as themselves. *)

val execute :
  ?loc:Loc.t ->
  ?invariant:('model -> 'sut -> unit) ->
  scope:(('sut -> unit) -> unit) ->
  ('model, 'sut) program ->
  unit
(** [execute ~scope program] runs [program] against the system [scope] hands its
    callback and returns [()] iff every body, every invariant check and the
    scope itself succeeded. [scope] runs once, under {!Test_tree.scoped}'s
    protocol: a scope that never calls its callback fails the case with a
    {!Failure.Message} at [loc], one that calls twice gets [Invalid_argument] at
    the second call, and the program's exception is re-raised through it, so a
    release that raises over a failing program is dropped unless it is a
    {!Failure.Timeout} or {!Failure.is_fatal}. [invariant m sut] runs on the
    fresh system before step 1 and after every step.

    A body's or invariant's exception is re-raised as a {!Failure.Check_failure}
    carrying the payload the engine would have built for it; the control
    exceptions and the {!Failure.is_fatal} set pass untouched, and what the
    scope raises before the callback, or after it returned from a passing
    program, propagates unconverted. The failure's [msg] names the step
    (["step 3 of 5: pop"], ["invariant after step 3 of 5: pop"],
    ["invariant on the fresh system"]) with a user [?msg] joined onto it, and a
    failure with no location takes the command's declaration site. *)

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
(** [stateful name ~model ~scope commands] declares a property test: every
    generated program over [commands], executed against the system [scope]
    builds, must satisfy every body's assertions and every [?invariant] check.
    It is {!Run.prop} over {!program} with {!execute} as its law: [timeout],
    [count] and [--prop-count] behave as on a property; [steps] and [pp_model]
    are {!program}'s, [scope] and [invariant] {!execute}'s, and [__POS__] fixes
    the declaration site a scope that never ran the program reports. Tags gain
    ["prop"] and ["stateful"]. There is no [?examples] and no [?retries]. The
    test fails at its first sample with [Invalid_argument] if [commands] is
    empty or [steps] is negative. *)
