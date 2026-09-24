# Stateful testing

A stateful test says: *these are the operations, this is what I think
they do to a model of the state, now try sequences of them.* One
`command` per operation, and `stateful` declares a property over
sequences of them — 100 generated programs of at most 20 calls by
default, each run against a system built for it, and a failing one
shrunk to a minimal program before it is reported.

Under test here is a fixed-capacity queue over a ring buffer: `push`
raises `Full` at capacity, `pop` and `peek` raise `Empty`.

```ocaml
open Windtrap

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
  exit
  @@ run "bounded_queue"
       [
         stateful "behaves like a list" ~model:[]
           ~scope:(fun run -> run (Bounded_queue.create capacity))
           ~pp_model:(Testable.pp (list int))
           ~invariant:(fun m q ->
             equal int (List.length m) (Bounded_queue.size q))
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
which is the line you want anyway. Pass `~__POS__` to `command` or
`call` to override it. The locator that always holds is
`step 6 of 6: pop`: a command's name is its identity in the report and
nowhere else, and `-f` filters test paths, not commands.

`shrunk 6 steps` is the search's work, and the last step is the failing
one once it converges — deleting a call after the failure never stops
the failure, and the search tries exactly that. When the block also
carries `shrinking stopped after 10000 steps` or `timed out after Ns while
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
  ~scope:(fun run -> run (Bounded_queue.create capacity))
  ~pp_model:(Testable.pp (list int))
  ~invariant:(fun m q ->
    cover "reached capacity" (List.length m = capacity);
    equal int (List.length m) (Bounded_queue.size q))
  commands
```

A `cover` in the invariant catches "never reached": the run fails
unless some passing program reached capacity at some point. For the
proportion, read `classify`'s table under `-v`.

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
raise is a bug in the specification, not a counterexample, and
windtrap reports it as one: the case fails at the generator with the
exception and its backtrace, unshrunk, naming the operation, the step
and which of the two raised. With the bug `List.nth m.live i >= 0` as
`close`'s `~pre`:

```
    counterexample (case 0): <generator raised before producing a value>
    which failed with:
      uncaught exception:
        step 1: close — ~pre raised Failure("nth")
      Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
      Called from Dune__exe__Test_pool.commands.(fun) in file "test/test_pool.ml", line 14, characters 25-42
```

The same goes for an assertion or an `assume` inside either — check in
a body, where the report is made for it.

## The system under test

`~scope` builds the system a case runs against and reclaims it. It
takes a callback: everything before the call acquires, the call runs
the program, everything after it returns releases. The worked example
above is the whole of it —
`~scope:(fun run -> run (Bounded_queue.create capacity))` — because an
in-memory queue needs no release.

It runs once per generated case **and once per shrink candidate**: the
search re-runs the program, so a shared system would make it
meaningless.

Taking a callback rather than returning a system is what lets a
resource that only exists *inside* a call be the system under test —
an Eio env or switch, `In_channel.with_open_text`, any `with_`-style
API. There is no moment inside those at which the resource could be
returned, and no `~setup`/`~teardown` pair expresses them without
threads or effects; as a scope they are the plain case (fragment; not
compiled here):

```ocaml
(* fragment: requires eio_main *)
stateful "store replays" ~model:Model.empty
  ~scope:(fun run ->
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw -> run (Store.open_ ~sw ~env dir))
  commands
```

A `~setup:f ~teardown:g` pair is the same shape with the release
written out: `let sut = f () in Fun.protect ~finally:(fun () -> g sut)
(fun () -> run sut)`.

`temp_dir ()` is the wrong tool inside a scope: it is *test*-scoped,
creating a directory per call that survives until the test ends, and a
failing stateful test builds one system per shrink candidate —
hundreds of them. The same boundary applies to `setenv` and `chdir`:
the runner restores them at the *attempt* boundary, not between cases
or shrink candidates, so a scope that moves the process or binds a
variable carries that state into every later case of the run — use
absolute paths, and put process state back yourself, per case, if the
scope must touch it. Mint the path in the scope and remove it on the
way out:

```ocaml
stateful "store survives any sequence" ~model:Store_model.empty
  ~scope:(fun run ->
    let dir = Filename.temp_file "store-" ".dir" in
    Sys.remove dir;
    Sys.mkdir dir 0o700;
    let store = Store.open_ dir in
    Fun.protect
      ~finally:(fun () ->
        Store.close store;
        rm_rf dir)
      (fun () -> run store))
  commands
```

`rm_rf` is yours to write, and this is the whole of it:

```ocaml
let rec rm_rf path =
  if Sys.is_directory path then begin
    Array.iter (fun name -> rm_rf (Filename.concat path name))
      (Sys.readdir path);
    Sys.rmdir path
  end
  else Sys.remove path
```

The `Fun.protect` is yours too: the contract is `scoped`'s, and
windtrap never sees the resource, so releasing on the failing path is
the scope's own job — `let r = acquire () in run r; release r` leaks
whenever the program fails. What windtrap guarantees is that the
failure reaches you: the program's exception is re-raised *through* the
scope, a release failure never replaces it (only a skip, a timeout, an
`exit`, a discard or an interrupt outranks a failure in hand), a scope that returns without
calling back fails the case, and one that calls back twice raises
`Invalid_argument`. A scope that raises or skips *before* calling back
propagates as it is — the pattern for a suite gated on a resource the
machine does not have. Keep cleanup that can fail out of `~finally`:
`Fun.protect`'s own rule reports `Fun.Finally_raised` in place of the
work exception.

## What it costs

A stateful test is the most expensive kind windtrap runs, and it is
worst exactly when CI is red. Against the shipped defaults —
`~count:100`, `~steps:20`, no timeout — a *passing*
run builds 100 systems and makes at most `count × steps` = 2,000 calls:
at most, because repair removes the calls the model forbids, and the
queue above measures 1,133.

A *failing* run adds the shrink search, and that is where the cost is:
every candidate considered, accepted or rejected, is a whole program
re-run with its own `~scope` call, acquisition and release included. One
node offers `1 + Σ_k ⌊steps/k⌋` deletion candidates — the empty program,
plus one per non-overlapping chunk at each chunk size, `k` over the
powers of two from below `steps` down to 1, so 39 at `~steps:20` — then
one per argument reduction, and each accepted step starts a fresh
descent, up to the engine's fixed budget of 10,000. The queue above converges in 50
to 100 systems and a few hundred calls; a failure hiding behind a long
prefix costs one or two orders of magnitude more. For an in-memory
system that is milliseconds; for one process, socket or descriptor per
command it is minutes.

| lever | effect | reach for it when |
| --- | --- | --- |
| `~steps` (default 20) | calls drawn per case; **quadratic** on a failing run | first, always — it is a work budget, not a fact about the state machine |
| `~count` (default 100) | linear, and only on a passing run | the passing run is the slow one |
| `~timeout` | the only per-test bound on the failing path | a search that must not run away — the shrink budget is fixed, not a lever. Expiring during shrinking ends it and reports the best counterexample so far, marked as not necessarily minimal |

`--tag stateful` selects these tests, `--exclude-tag stateful` drops
them: an expensive suite can keep them out of the inner loop and run
them on their own.

## Notes

- Stateful tests carry both `"prop"` and `"stateful"`, so everything
  the [property chapter](property-testing.md) describes applies —
  seeds and replay, `--prop-count`, the shrink budget and its
  notice, timeouts, capture, `xfail`, and the CI reporters. The replay
  line reruns exactly the failing program.
- `assume`, `collect`, `classify` and `cover` work inside command
  bodies and inside `~invariant`. Their unit is the *case*, not the
  step: a label marked at any step counts once for the program, so
  `cover "filled to capacity" (List.length m = capacity)` in the
  invariant means *some passing program reached capacity at some
  point*. An `assume` that fails discards the whole program, and
  a run gives up after twice `~count` discards.
- There is no `~examples`: the program type is abstract, so a program
  cannot be spelled by hand. Copy a shrunk counterexample back as a
  plain `test` — the printed steps are the calls to make. There is no
  `~retries` either: the search re-runs the same program dozens of
  times, so a system whose behaviour varies run to run is out of
  contract, and retrying would hide that rather than settle it.
- The empty program is a real test — the invariant runs on the fresh
  system before the first call — and renders as `(no commands)`. It is
  what a `~scope` that raises while acquiring, or an invariant that
  rejects a fresh system, shrinks to.
- The program printer bounds itself: arguments are cut at 200 bytes,
  model cells at 60 code points and one line each, and a program over
  40 steps prints its first and last 20 with `… (N steps omitted)`
  between. A `~pp_model` that raises costs its own cell and no more.
- `stateful` with an empty command list does not pass vacuously: the
  test fails at case 0 with
  `Invalid_argument("Windtrap.stateful: no commands to draw from")`,
  under a `<generator raised before producing a value>` counterexample.
