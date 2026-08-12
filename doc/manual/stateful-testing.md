# Stateful testing

A stateful test says: *these are the operations, this is what I think
they do to a model of the state, now try sequences of them.* One
`command` per operation, and `stateful` declares a property over
sequences of them — 100 generated programs of at most 20 calls by
default, each run against a system built for it, and a failing one
shrunk to a minimal program before it is reported.

Stateful testing ships as its own library: add `windtrap.stateful`
next to `windtrap` in the stanza — `(libraries windtrap
windtrap.stateful)` — and open `Windtrap_stateful` beside `Windtrap`.
`command`, `call` and `stateful` below are all its.

Under test here is a fixed-capacity queue over a ring buffer: `push`
raises `Full` at capacity, `pop` and `peek` raise `Empty`.

```ocaml
open Windtrap
open Windtrap_stateful
let capacity = 4

(* The model: the elements the queue should hold, oldest first. *)
type model = int list

let commands =
  [
    command "push" (Gen.int_range 0 9)
      ~pre:(fun m _ -> List.length m < capacity)
      ~next:(fun m x -> m @ [ x ])
      (fun _ x q -> Bounded_queue.push q x);
    call "pop"
      ~pre:(fun m -> m <> [])
      ~next:List.tl
      (fun m q -> equal int (List.hd m) (Bounded_queue.pop q));
    call "peek"
      ~pre:(fun m -> m <> [])
      ~next:Fun.id
      (fun m q -> equal int (List.hd m) (Bounded_queue.peek q));
    call "push when full"
      ~pre:(fun m -> List.length m = capacity)
      ~next:Fun.id
      (fun _ q -> raises Bounded_queue.Full (fun () -> Bounded_queue.push q 0));
  ]

let () =
  run "bounded_queue"
    [
      stateful "behaves like a list" ~model:[]
        ~setup:(fun () -> Bounded_queue.create capacity)
        ~pp_model:(Testable.pp (list int))
        ~invariant:(fun m q -> equal int (List.length m) (Bounded_queue.size q))
        commands;
    ]
```

`command "push" gen ~pre ~next body` is *a call named push, whose
argument comes from gen, which is legal when pre holds, which moves the
model as next says, and which does body*. `call` is the same for an
operation with no generated argument — most of them, in most APIs — and
its step prints as its name alone. Every function takes the model
first, then the argument, then (for the body) the system: expected
precedes actual, always. The body sees the model *before* its own
transition — the pre-state, which is what a postcondition needs.

The body calls the system and asserts with the ordinary verbs, so a
call's result is produced and checked in one expression and never needs
a type of its own. A body that asserts nothing — `push` here — is
checked only by `~invariant`, which runs on the fresh system before the
first call and after every call. Bodies check what a call *returns*;
the invariant checks what the state *is*.

Three words carry the rest of the chapter. The **model** is your
description of the state — here `int list`, and it is the
specification, not a second implementation. A **command** is one
operation of the system — the four parts above. A **program** is what
a case actually is: an initial model and a sequence of calls, each one
legal in the model its predecessors produced, and the counterexample a
failure prints *is* the program.

## What a failure looks like

Give `pop` a ring that wraps on the queue's length instead of its
capacity — `q.head <- (q.head + 1) mod q.size` — and a slot goes stale:

```
$ dune runtest
bounded_queue: 1 test (seed s1:667c8918d661391e)
F
──────────────────── failures (1) ────────────────────
  FAIL  behaves like a list
    test/test_bounded_queue.ml:31
      31 │       stateful "behaves like a list" ~model:[]

    counterexample (case 0, shrunk 6 steps):
      6 calls, last: pop
      []      1  push 0
      [0]     2  push 0
      [0; 0]  3  pop
      [0]     4  push 1
      [0; 1]  5  pop
      [1]     6  pop
    which failed at:
      test/test_bounded_queue.ml:14
      step 6 of 6: pop
      expected  1
      actual    0
    replay: dune exec test/test_bounded_queue.exe -- --seed s1:667c8918d661391e -f 'behaves like a list'
──────────────────────────────────────────────────────

1 failed in 0.000785s.
```

Six calls, and the bug is stated by the artifact: step 6 popped from
`[1]`, so the model says `1`; the queue handed back `0`. Three things
in that block are the design:

- **The left column is the model *before* each call** — the state the
  call was made in, from `~pp_model`. It is a fold of `~next` over the
  program computed by the printer, not a recording of the run, so it is
  present on every row including the failing one, and the initial model
  is visible. Which step failed is stated by `step 6 of 6`, not by a
  gap in the column.
- **The program shown is the program that ran.** A call whose
  precondition did not hold is not in the program at all — not skipped
  at runtime, not printed and ignored.
- **The report says what disagreed**, not that something disagreed:
  `expected 1, actual 0` is the ordinary `equal int` failure, with its
  diff, its testable, and everything else assertions give you.

The `file:line` under `which failed at:` is the failing *command's*
declaration — `call "pop"`, not the `stateful` line above it, and not
the assertion. A body is idiomatically one assertion in tail position,
whose stack frame is gone by the time it raises, so windtrap has no
site to capture there; the command records its own when you declare it,
which is the line you want anyway. Pass `~pos:__POS__` to `command` or
`call` to override it. The locator that always holds is
`step 6 of 6: pop`: a command's name is its identity in the report and
nowhere else, and `-f` filters test paths, not commands.

`shrunk 6 steps` is the search's work, and the last step is the failing
one once it converges — deleting a call after the failure never stops
the failure, and the search tries exactly that. When the block also
carries `shrink budget of 100 steps spent` or `timed out after Ns while
shrinking`, the search stopped early and trailing calls may survive.

## Preconditions: filter and selector

`~pre m arg` is whether the call is legal in model `m`. It defaults to
always-legal, which is true by construction for most commands. Repair
applies it before the program is assembled: a call is kept iff its
`~pre` holds in the model the calls before it produced, and `~next`
threads through the calls that are kept. The rule to remember is the
converse — **a stateful test never exercises a call its own model
forbids**, so `pop` is never called on an empty queue and no body needs
a guard.

Filtering is only half of it. A precondition also *selects* a rare
state: `"push when full"` is generated only at capacity, which is
exactly where it is interesting. A system that should raise on an
illegal call is therefore a command whose `~pre` selects the illegal
state and whose body asserts the raise — not something the generator
has to stumble into.

The cost of that power is that a precondition no state satisfies is
silent: the command is deleted from every program and the test passes
without ever calling it. Turn it into an outcome — but not from that
command's own body, which is exactly the code that never runs. A
`cover` there registers no requirement at all, and the run is green
with no label to show for it. Cover the *state* the precondition
selects, from `~invariant`, which runs on every case:

```ocaml
stateful "behaves like a list" ~model:[]
  ~setup:(fun () -> Bounded_queue.create capacity)
  ~pp_model:(Testable.pp (list int))
  ~invariant:(fun m q ->
    cover ~label:"reached capacity" ~at_least:5. (List.length m = capacity);
    equal int (List.length m) (Bounded_queue.size q))
  commands
```

`~at_least:1.` catches "never reached"; a higher figure calibrates the
mix — this one runs at about 30%, and the
[cookbook](../cookbook.md#6-cover-thresholds-and-the-noise-floor) has
the rule of thumb for the margin.

`~next` is required for the same reason `~pre` is optional: its absence
would be the claim *this call does not change the model*, and that
claim is false silently and vacuously — with an identity transition the
model never grows, every other precondition fails, and the test becomes
a green sequence of pushes asserting nothing. Read-only operations say
so: `~next:Fun.id` on `call`, `~next:Fun.const` on `command`.

`~pre` and `~next` must be pure, and the model must be persistent: the
model trajectory is folded three times per case — by repair when the
program is drawn, by the executor when it runs, and by the printer when
a counterexample renders — and the three must agree. A mutable
structure mutated and returned corrupts generation before the test
runs. If the state really is a hashtable, model it as a `Map`.

## Shrinking, and what a command may depend on

Shrinking removes calls and simplifies their arguments. It never
substitutes one command for another and never invents one: every call
of every candidate is a call the drawn program made, with its argument
only reduced. Deleting a call another depends on is safe — the
dependent call is dropped in the same candidate, because repair runs
again on every candidate.

That is what makes handles expressible. Generation happens before
execution, so a generated argument cannot *be* a handle that does not
exist yet; name it by position into the live set instead, and let
`~pre` keep the lookup total:

```ocaml
type model = { live : int list; next_id : int }

(* An index into the live set, not an absolute handle: it stays
   meaningful however many handles have been opened and closed. *)
let slot = Gen.int_range 0 3

let commands =
  [
    call "open"
      ~pre:(fun m -> List.length m.live < 4)
      ~next:(fun m ->
        { live = m.live @ [ m.next_id ]; next_id = m.next_id + 1 })
      (fun m pool -> Pool.open_ pool m.next_id);
    command "write"
      (Gen.pair slot (Gen.string_of (Gen.char_range 'a' 'z')))
      ~pre:(fun m (i, _) -> i < List.length m.live)
      ~next:Fun.const
      (fun m (i, data) pool -> Pool.write pool (List.nth m.live i) data);
    command "close" slot
      ~pre:(fun m i -> i < List.length m.live)
      ~next:(fun m i ->
        { m with live = List.filteri (fun j _ -> j <> i) m.live })
      (fun m i pool -> Pool.close pool (List.nth m.live i));
  ]
```

`List.nth` cannot raise here: `~pre` guarantees `i` indexes the live
set, and the model and the pool gain and lose a handle in the same
call. Worth writing down, because a `~pre` or `~next` that *does*
raise is a specification bug — see below.

## The system under test

`~setup` runs once per generated case **and once per shrink
candidate** — the search re-runs the program, so a shared system would
make it meaningless — and `~teardown` releases on every path the
executor leaves. It is the `bracket` pair, scoped to a case instead of
a test.

`temp_dir ()` is the wrong tool inside `~setup`: it is *test*-scoped,
creating a directory per call that survives until the test ends, and a
failing stateful test builds one system per shrink candidate —
hundreds of them. Mint the path in `~setup` and remove it in
`~teardown`:

```ocaml
stateful "store survives any sequence" ~model:Store_model.empty
  ~setup:(fun () ->
    let dir = Filename.temp_file "store-" ".dir" in
    Sys.remove dir;
    Sys.mkdir dir 0o700;
    (dir, Store.open_ dir))
  ~teardown:(fun (dir, store) ->
    Store.close store;
    rm_rf dir)
  commands
```

`rm_rf` is [the cookbook's](../cookbook.md#1-temporary-directories-and-files).
No `Fun.protect` is wanted around it: `~teardown` is already the
release path, and a teardown failure is reported only when the program
succeeded — on the failing path the counterexample outranks the cleanup
error, so a broken teardown does not replace the assertion you were
shown. Only a timeout or a fatal exception from the teardown itself
outranks a failure in hand: those end the run.

A `~setup` that raises propagates as it is — no teardown is owed for a
system that was never built.

## When the specification itself raises

`~pre` and `~next` are evaluated by repair, on states the program will
never execute. One that raises is a bug in the model, not a
counterexample, and windtrap reports it as such: repair stops at that
step, keeps it as the program's last call, and the failure names the
command, the step, and which of the two raised.

```ocaml
(* The bug: [List.nth] raises when the slot is past the live set. *)
command "close" slot
  ~pre:(fun m i -> List.nth m.live i >= 0)
  ~next:(fun m i -> { m with live = List.filteri (fun j _ -> j <> i) m.live })
  (fun m i pool -> Pool.close pool (List.nth m.live i));
```

```
    counterexample (case 0, shrunk 2 steps):
      1 call, last: close
      []  1  close 0
    which failed at:
      test/test_pool.ml:14
      step 1 of 1: close — ~pre raised
      uncaught exception:
        Failure("nth")
```

— followed by the raise's backtrace. The location is the `stateful`
declaration site, since `command` records none of its own. Because the
failure is an ordinary assertion-class failure, the search *minimises
the specification bug*: programs that never reach the raising state
repair cleanly and pass, so it converges on the shortest one that does.
A `~pre` poison withholds the step's body (the call is not known to be
legal); a `~next` poison runs it (only the model *after* the call is
unknown).

The corollary is that `~pre` and `~next` must neither assert nor
discard: a `check` failure or an `assume` in either escapes into the
generator, where the case reports `<generator raised before producing a
value>` and nothing shrinks. Check in a body, where the report is made
for it.

## What it costs

`~steps` is how many calls are *drawn* per case; repair removes the
ones the model forbids, so a program makes at most that many. It
defaults to 20 — a work budget, not a fact about state machines, and
the number to look at first, because a stateful test is the most
expensive kind of test windtrap runs and is worst exactly when CI is
red. Against the shipped defaults — `~count:100`, `~steps:20`,
`--max-shrink 100`, and no timeout — a passing run builds 100 systems
and makes at most `count × steps` = 2,000 calls: at most, because
repair removes the calls the model forbids, and the queue above
measures 1,133.

A failing run adds the shrink search, and that is where the cost is.
Every candidate the search considers, accepted or rejected, is a whole
program re-run with its own `~setup` and `~teardown`. One node offers
`1 + Σ_k ⌊steps/k⌋` deletion candidates — the empty program, plus one
per non-overlapping chunk at each chunk size, `k` over the powers of
two from below `steps` down to 1, so 39 at `~steps:20` — then one per
argument reduction, and each accepted step starts a fresh descent, up
to `--max-shrink` of them. The queue above converges in 50 to 100
systems and a few hundred calls; a failure that hides behind a long
prefix costs one or two orders of magnitude more, and the call count
grows with the *square* of `~steps`.

For an in-memory system that is milliseconds. For one process, socket,
or descriptor per command it is minutes. The levers:

- `~steps` — quadratic on a failing run. Lower it first.
- `~count` — linear, and only on a passing run.
- `--max-shrink N` — linear on a failing run. It is **run-wide**:
  there is no declaration-site spelling, so a suite cannot bound one
  expensive stateful test without bounding every property.
- `~timeout` — the only per-test bound on the failing path. A timeout
  that expires during shrinking ends the search and reports the best
  counterexample found so far, marked as not necessarily minimal.

`--tag stateful` selects these tests, `--exclude-tag stateful` drops
them: an expensive suite can keep them out of the inner loop and run
them on their own.

## Notes

- Stateful tests carry both `"prop"` and `"stateful"`, so everything
  the [property chapter](property-testing.md) describes applies —
  seeds and replay, `--prop-count`, `--max-shrink` and its budget
  notice, timeouts, capture, `xfail`, and the CI reporters. The replay
  line reruns exactly the failing program.
- `assume`, `collect`, `classify` and `cover` work inside command
  bodies and inside `~invariant`. Their unit is the *case*, not the
  step: a label marked at any step counts once for the program, so
  `cover ~label:"filled to capacity" ~at_least:5. (List.length m = capacity)`
  in the invariant means *5% of passing programs reached capacity at
  some point*. An `assume` that fails discards the whole program, and
  a run gives up after twice `~count` discards.
- There is no `~examples`: the program type is abstract, so a program
  cannot be spelled by hand. Copy a shrunk counterexample back as a
  plain `test` — the printed steps are the calls to make. There is no
  `~retries` either: the search re-runs the same program dozens of
  times, so a system whose behaviour varies run to run is out of
  contract, and retrying would hide that rather than settle it.
- The empty program is a real test — the invariant runs on the fresh
  system before the first call — and renders as `(no commands)`. It is
  what a `~setup` that raises, or an invariant that rejects a fresh
  system, shrinks to.
- The program printer bounds itself: arguments are cut at 200 bytes,
  model cells at 60 code points and one line each, and a program over
  40 steps prints its first and last 20 with `… (N steps omitted)`
  between. A `~pp_model` that raises costs its own cell and no more.
- `stateful` with an empty command list does not pass vacuously: the
  test fails at case 0 with
  `Invalid_argument("Windtrap_stateful.stateful: no commands to draw from")`,
  under a `<generator raised before producing a value>` counterexample.
