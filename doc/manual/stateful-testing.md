# Stateful testing

This page shows how to test code that keeps state across calls:
describe its operations as commands, read the program a failure prints,
and keep that program as a regression. Every rule is documented under
`Windtrap.stateful` in [`lib/windtrap.mli`](../../lib/windtrap.mli).

A stateful test runs the same calls on two implementations of one API
and compares their outcomes, what each call returned or raised. The
system is the code under test, and the reference is the code whose
outcomes the system must give: a model written for the test, as on this
page, or another implementation, such as `Set.Make` for a faster set. A
command is one operation of the API, a program a drawn sequence of
calls, and a value what a call made, such as a queue.

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
let () = exit (run "bounded_queue" [ queues; regressions ])
```

The files ship as `examples/04-stateful-testing/` in windtrap's
repository, and the transcripts print that directory's paths. The
failing transcripts run the example with the change named above them.

## Writing a stateful test

`command name signature reference system` is one operation of the API:
its name, its signature, then the reference's function and the
system's, the expected side first as in `equal`. A signature follows
the functions' argument order, so `Bounded_queue.push : t -> int -> unit`
takes `queue ^-> Gen.int_range 0 9 @-> returns unit`:

- `g @-> …` is an argument drawn from the generator `g`, the same value
  on both sides.
- `t ^-> …` is a value of `t` that an earlier call made, passed to the
  reference as its reference side and to the system as its system side.
- `returns w` compares the two results under the witness `w`.
- `makes t` keeps the two results as a new value of `t`.

`abstract "q"` declares the type of the queues. Only a call to `create`
makes one, and a report names them `q1`, `q2` in the order the calls
made them. `~pp` prints a queue's reference side in a failing program.
`~invariant r s`, unused here, asserts on the two sides of every queue
after every call. The model has a function per operation of
`Bounded_queue`, with the same argument order.

`test/test_bounded_queue.ml`:

```ocaml
open Windtrap

module Model = struct
  type t = { capacity : int; mutable items : int list }

  let create capacity = { capacity; items = [] }
  let size m = List.length m.items

  let peek m =
    match m.items with [] -> raise Bounded_queue.Empty | x :: _ -> x

  let pop m =
    let x = peek m in
    m.items <- List.tl m.items;
    x

  let push m x =
    if size m = m.capacity then raise Bounded_queue.Full;
    m.items <- m.items @ [ x ];
    cover "reached capacity" (size m = m.capacity)
end

let queue =
  abstract "q" ~pp:(fun ppf m -> Testable.pp (list int) ppf m.Model.items)

let commands =
  [
    command "create"
      (Gen.int_range 1 4 @-> makes queue)
      Model.create Bounded_queue.create;
    command "push"
      (queue ^-> Gen.int_range 0 9 @-> returns unit)
      Model.push Bounded_queue.push;
    command "pop" (queue ^-> returns int) Model.pop Bounded_queue.pop;
    command "peek" (queue ^-> returns int) Model.peek Bounded_queue.peek;
    command "size" (queue ^-> returns int) Model.size Bounded_queue.size;
  ]

let queues = group "queue" [ stateful "behaves like a list" commands ]
```

`stateful name commands` draws `~count` programs of at most `~steps`
calls, 100 and 20 by default. It runs each call on the system, then on
the reference, and fails at the first call whose outcomes differ. A
command listed twice is drawn twice as often, and a command that no
passing program calls fails the test with `never called:` and its name.
The `cover` in `Model.push` fails the test when no program fills a
queue.

Under `-v`, a passing run prints the share of programs that filled a
queue:

```
$ dune exec examples/04-stateful-testing/test_bounded_queue.exe -- -v --seed s1:c26eddaeb764a645 -f behaves
bounded_queue: 1 test (seed s1:c26eddaeb764a645)
  PASS  queue › behaves like a list                2.8ms
    labels (100 passing cases):
       36.0%  reached capacity
1 passed in 3.6ms.
```

## Checking the exceptions an operation raises

An exception is an outcome, compared like a result. `Model.push` raises
`Bounded_queue.Full` on a full queue and `Model.pop` raises
`Bounded_queue.Empty` on an empty one, so the system must raise them
too, and `pop` on an empty queue is tested. A verb's failure,
`Assert_failure` and `Match_failure` are never outcomes, and fail the
case.

Two exceptions are equal when their constructors have the same name
once the module path is removed, so a model can declare its own
`exception Empty`. Their payloads are not compared. To compare a
payload, both functions return their outcome as a `result`, and the
signature ends in `returns (result w e)`.

## Restricting a call to the states where it is legal

When the API forbids a call instead of raising, as for a call whose
behaviour is undefined or one that blocks, `~pre` keeps the call from
both sides. It receives the reference's arguments, a queue as its model,
and must not change them. A call whose `~pre` is `false` is skipped and
absent from the report. If `pop` were undefined on an empty queue, its
command would read:

```ocaml
(* fragment: the pop command, for a pop undefined on an empty queue *)
command "pop"
  ~pre:(fun m -> Model.size m > 0)
  (queue ^-> returns int)
  Model.pop Bounded_queue.pop
```

## Reading a failing program

A failure prints the program that its failing run executed, as a table
of the calls that ran, then the call that failed and its two outcomes,
the reference's as `expected`. `let q1 = create 1` made the value `q1`,
and `reference before` shows the reference side of each argument before
the call, as the type's `~pp` prints it. The report's `replay:` line and
`--seed` work as for a [property](property-testing.md).

If `push` let one element too many in, comparing the size with `>`
where it compares with `=`, shrinking would reduce the program to three
calls:

```
$ dune exec examples/04-stateful-testing/test_bounded_queue.exe -- --seed s1:c26eddaeb764a645 -f behaves
bounded_queue: 1 test (seed s1:c26eddaeb764a645)
──────────────────────── failures ────────────────────────
  FAIL  queue › behaves like a list
    examples/04-stateful-testing/test_bounded_queue.ml:39
      39 │ let queues = group "queue" [ stateful "behaves like a list" commands ]

    counterexample (case 1, shrunk 6 steps): 3 calls, last: push
       #  reference before  call
       1                    let q1 = create 1
       2  []                push q1 0
       3  [0]               push q1 0
    which failed at:
      examples/04-stateful-testing/test_bounded_queue.ml:31
        31 │ command "push"
      call 3 of 3: push q1 0
      expected exception  Bounded_queue.Full
      but no exception was raised
──────────────────────────────────────────────────────────

replay: dune exec examples/04-stateful-testing/test_bounded_queue.exe -- --seed s1:c26eddaeb764a645 -f 'behaves'
1 failed in 1.4ms.
```

## Reading a failure of the model

A verb's failure, `Assert_failure` or `Match_failure` in a model
function is not an outcome: the model is wrong, not the system. The
report names the model's call as `reference of call N of N`. Shrinking
reduces a program that breaks the model, as it does one that fails the
system, and keeps the two apart: a program that breaks the model never
stands for a failure of the system, nor the other way round.

If `Model.peek` asserted that the queue is never empty, writing
`[] -> assert false` where it raises `Bounded_queue.Empty`, the first
`peek` of an empty queue would break the model:

```
$ dune exec examples/04-stateful-testing/test_bounded_queue.exe -- --seed s1:c26eddaeb764a645 -f behaves
bounded_queue: 1 test (seed s1:c26eddaeb764a645)
──────────────────────── failures ────────────────────────
  FAIL  queue › behaves like a list
    examples/04-stateful-testing/test_bounded_queue.ml:39
      39 │ let queues = group "queue" [ stateful "behaves like a list" commands ]

    counterexample (case 0, shrunk 5 steps): 2 calls, last: peek
       #  reference before  call
       1                    let q1 = create 1
       2  []                peek q1
    which failed at:
      examples/04-stateful-testing/test_bounded_queue.ml:35
        35 │ command "peek" (queue ^-> returns int) Model.peek Bounded_queue.peek;
      reference of call 2 of 2: peek q1
      uncaught exception:
        File "examples/04-stateful-testing/test_bounded_queue.ml", line 10, characters 29-35: Assertion failed
      Raised at Dune__exe__Test_bounded_queue.Model.peek in file "examples/04-stateful-testing/test_bounded_queue.ml", line 10, characters 29-41
──────────────────────────────────────────────────────────

replay: dune exec examples/04-stateful-testing/test_bounded_queue.exe -- --seed s1:c26eddaeb764a645 -f 'behaves'
1 failed in 7.7ms.
```

## Keeping a failing program as a regression

A stateful test takes no fixed program, so a program that failed is
kept by copying its calls into a `test`.

`test/test_bounded_queue.ml`:

```ocaml
let regressions =
  group "regressions"
    [
      test "a full queue refuses a push" (fun () ->
          let q = Bounded_queue.create 1 in
          Bounded_queue.push q 0;
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
    examples/04-stateful-testing/test_bounded_queue.ml:47
      47 │ raises ~__POS__ Bounded_queue.Full (fun () -> Bounded_queue.push q 0));

    expected exception  Bounded_queue.Full
    but no exception was raised
──────────────────────────────────────────────────────────

1 failed in 0.6ms.
```

## Checking an outcome the API leaves open

`chooses w` ends the signature of an operation whose outcome the API
leaves open, such as the element that a bag's `take_any` removes. The
reference receives the system's outcome, `Ok v` or `Error e`, as its
last argument, returns or raises the outcome it accepts, and updates
its state to follow the choice. A choice it does not accept prints as
an `expected` and `actual` pair.

```ocaml
(* fragment: Bag is the project's own, and the model of a bag is an int list ref *)
let bag = abstract "b"

let rec remove_one x = function
  | [] -> []
  | y :: r -> if y = x then r else y :: remove_one x r

let take_any =
  command "take_any"
    (bag ^-> chooses (option int))
    (fun b seen ->
      match seen with
      | Ok (Some x) when List.mem x !b ->
          b := remove_one x !b;
          Some x
      | Ok None when !b = [] -> None
      | _ -> ( match !b with [] -> None | x :: _ -> Some x))
    Bag.take_any
```

## Releasing what a program made

A system that holds a resource, such as a directory or a socket, is
made by a command and released by `abstract ~release`. A release runs
when a program ends, whether it passed or failed, once per system side
the program made, newest first. It must accept every state the API can
reach, a store that a `close` command closed included. `temp_dir`,
`setenv` and `chdir` last for the whole test, so the command that opens
a store makes its own directory with `Filename.temp_dir`, and the
release removes it:

```ocaml
(* fragment: Store and Store_model are the project's own, and remove_tree removes a directory *)
let store =
  abstract "st" ~release:(fun st ->
      if Store.is_open st then Store.close st;
      remove_tree (Store.dir st))

let open_store =
  command "open"
    (Gen.unit @-> makes store)
    (fun () -> Store_model.empty ())
    (fun () -> Store.open_dir (Filename.temp_dir "store" ""))
```

## Running a system inside an effect handler

A system whose calls perform effects, as Eio's operations do, needs
their handler around every call. Every test runs inside the handlers
that surround `run`, so the suite installs the handler there:

```ocaml
(* fragment: requires eio, and stores is a group of stateful tests *)
let () = exit (Eio_main.run (fun _env -> run "store" [ stores ]))
```
