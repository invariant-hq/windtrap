# Stateful testing

This page shows how to test a system that keeps state against a model:
describe its operations as commands, read the program of calls a failure
prints, and keep that program as a regression. The reference is
[`lib/windtrap.mli`](../../lib/windtrap.mli), under `Windtrap.stateful`.

The snippets test `Bounded_queue`, a queue of integers with a fixed
capacity.

`test/bounded_queue.ml`:

```ocaml
exception Full
exception Empty

type t = {
  data : int array;
  capacity : int;
  mutable head : int;
  mutable size : int;
}

let create capacity =
  { data = Array.make capacity 0; capacity; head = 0; size = 0 }

let size q = q.size

let push q x =
  if q.size = q.capacity then raise Full;
  q.data.((q.head + q.size) mod q.capacity) <- x;
  q.size <- q.size + 1

let pop q =
  if q.size = 0 then raise Empty;
  let x = q.data.(q.head) in
  q.head <- (q.head + 1) mod q.capacity;
  q.size <- q.size - 1;
  x

let peek q = if q.size = 0 then raise Empty else q.data.(q.head)
```

The suite is `test/test_bounded_queue.ml`, built by a `(test)` stanza.

`test/dune`:

```lisp
(test
 (name test_bounded_queue)
 (modules test_bounded_queue bounded_queue)
 (libraries windtrap))
```

Its last line runs its two groups.

`test/test_bounded_queue.ml`:

```ocaml
let () = exit (run "bounded_queue" [ queue; regressions ])
```

The files ship as `examples/04-stateful-testing/` in windtrap's
repository, and the transcripts print that directory's paths. The
failing transcripts run a version of `Bounded_queue` with the bug named
above them.

## Writing a stateful test

A stateful test draws programs, sequences of calls to the system, from a
list of commands, and checks each program against a model, a pure value
that stands for the system's state. `command name gen ~next body` is an
operation whose argument `gen` draws, and `call` is one without
argument. `~next` gives the model after the call, and the body calls the
system and asserts on what it returns, given the model before the call.
`~pre` restricts an operation to the models where it is legal, which
also selects the state it needs, as `push when full` does. A command
listed twice is drawn more often. An argument that names something
the program created, such as a handle, is drawn as an index the body
resolves in the model, as `List.nth m (i mod List.length m)` under a
`~pre` that keeps `m` non-empty.

`test/test_bounded_queue.ml`:

```ocaml
open Windtrap

let capacity = 4

let commands =
  [
    command "push" (Gen.int_range 0 9)
      ~pre:(fun m _ -> List.length m < capacity)
      ~next:(fun m x -> m @ [ x ])
      (fun _ x q -> Bounded_queue.push q x);
    call "pop"
      ~pre:(fun m -> m <> [])
      ~next:List.tl
      (fun m q -> equal ~__POS__ int (List.hd m) (Bounded_queue.pop q));
    call "peek"
      ~pre:(fun m -> m <> [])
      ~next:Fun.id
      (fun m q -> equal ~__POS__ int (List.hd m) (Bounded_queue.peek q));
    call "push when full"
      ~pre:(fun m -> List.length m = capacity)
      ~next:Fun.id
      (fun _ q ->
        raises ~__POS__ Bounded_queue.Full (fun () -> Bounded_queue.push q 0));
  ]
```

`stateful` takes the initial model and the commands. `~scope` hands each
program a fresh system, `~invariant` checks the system between calls,
and `~pp_model` prints the model beside each call of a failing program.
Keep the model persistent, such as a list or a `Map`, and `~pre` and
`~next` pure (see `Windtrap.stateful`). A `cover` in the invariant fails
the test when no program reaches the state it names, such as the full
queue that `push when full` needs. A command that no passing program
calls fails the test with `never called:` and its name, as a `~pre`
that never holds does.

`test/test_bounded_queue.ml`:

```ocaml
let queue =
  group "queue"
    [
      stateful "behaves like a list" ~model:[]
        ~scope:(fun run -> run (Bounded_queue.create capacity))
        ~pp_model:(Testable.pp (list int))
        ~invariant:(fun m q ->
          cover "reached capacity" (List.length m = capacity);
          equal ~__POS__ int (List.length m) (Bounded_queue.size q))
        commands;
    ]
```

Under `-v`, a passing run prints the share of programs that reached
capacity:

```
$ dune exec examples/04-stateful-testing/test_bounded_queue.exe -- -v --seed s1:c26eddaeb764a645 -f behaves
bounded_queue: 1 test (seed s1:c26eddaeb764a645)
  PASS  queue › behaves like a list                1.3ms
    labels (100 passing cases):
       33.0%  reached capacity
1 passed in 1.7ms.
```

## Reading a failing program

A failure prints the shrunk program as a table of calls, with the model
before each, then the call that failed and its failure. The report's
`replay:` line and `--seed` work as for a [property](property-testing.md).
`~steps` sets the most calls a program makes, 20 by default, and
`~count` the number of programs.

If `push` let one element too many in, shrinking would reduce the
program to four pushes and a push on the full queue:

```
$ dune exec examples/04-stateful-testing/test_bounded_queue.exe -- --seed s1:c26eddaeb764a645 -f behaves
bounded_queue: 1 test (seed s1:c26eddaeb764a645)
──────────────────────── failures ────────────────────────
  FAIL  queue › behaves like a list
    examples/04-stateful-testing/test_bounded_queue.ml:29
      29 │ stateful "behaves like a list" ~model:[]

    counterexample (case 0, shrunk 10 steps): 5 calls, last: push when full
       #  model before  call
       1  []            push 0
       2  [0]           push 0
       3  [0; 0]        push 0
       4  [0; 0; 0]     push 0
       5  [0; 0; 0; 0]  push when full
    which failed at:
      examples/04-stateful-testing/test_bounded_queue.ml:23
        23 │ raises ~__POS__ Bounded_queue.Full (fun () -> Bounded_queue.push q 0));
      call 5 of 5: push when full
      expected exception  Bounded_queue.Full
      but no exception was raised
──────────────────────────────────────────────────────────

replay: dune exec examples/04-stateful-testing/test_bounded_queue.exe -- --seed s1:c26eddaeb764a645 -f 'behaves'
1 failed in 1.8ms.
```

## Keeping a failing program as a regression

A stateful test takes no fixed program, so a program that failed is
kept by copying its calls into a `test`:

`test/test_bounded_queue.ml`:

```ocaml
let regressions =
  group "regressions"
    [
      test "a full queue refuses a push" (fun () ->
          let q = Bounded_queue.create capacity in
          List.iter (Bounded_queue.push q) [ 0; 0; 0; 0 ];
          raises ~__POS__ Bounded_queue.Full (fun () -> Bounded_queue.push q 0));
    ]
```

Against the same bug, the regression fails on every run, whatever the
seed:

```
$ dune exec examples/04-stateful-testing/test_bounded_queue.exe -- -f regressions
bounded_queue: 1 test
──────────────────────── failures ────────────────────────
  FAIL  regressions › a full queue refuses a push
    examples/04-stateful-testing/test_bounded_queue.ml:44
      44 │ raises ~__POS__ Bounded_queue.Full (fun () -> Bounded_queue.push q 0));

    expected exception  Bounded_queue.Full
    but no exception was raised
──────────────────────────────────────────────────────────

1 failed in 0.6ms.
```

## Giving each program a fresh system

`~scope` runs once per program and once per shrink candidate, and must
release the system whether the program passes or fails. `temp_dir`,
`setenv` and `chdir` last for the whole test, never for one program, so
a system that keeps files in a directory makes and removes its own, under
an absolute path:

```ocaml
(* fragment: Store and remove_tree are the project's own *)
let scope run =
  let dir = Filename.temp_dir "store" "" in
  let store = Store.open_dir dir in
  Fun.protect
    ~finally:(fun () ->
      Store.close store;
      remove_tree dir)
    (fun () -> run store)
```
