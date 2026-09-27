# Stateful testing

This page shows how to test code that keeps state across calls:
describe its operations as commands, read the program a failure prints,
keep that program as a regression, state rules over the whole history
of calls, and test a structure shared between domains. Every rule is
documented under `Windtrap.stateful` in
[`lib/windtrap.mli`](../../lib/windtrap.mli).

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
  PASS  queue › behaves like a list                3.3ms
    labels (100 passing cases):
       46.0%  reached capacity
1 passed in 4.1ms.
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

On several domains a command with a `~pre` runs only in a program's
prefix (see [Testing on several domains](#testing-on-several-domains)).

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

    counterexample (case 0, shrunk 7 steps): 3 calls, last: push
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
1 failed in 0.8ms.
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

    counterexample (case 0, shrunk 7 steps): 2 calls, last: peek
       #  reference before  call
       1                    let q1 = create 1
       2  []                peek q1
    which failed at:
      examples/04-stateful-testing/test_bounded_queue.ml:35
        35 │ command "peek" (queue ^-> returns int) Model.peek Bounded_queue.peek;
      reference of call 2 of 2: peek q1
      uncaught exception:
        File "examples/04-stateful-testing/test_bounded_queue.ml", line 10, characters 29-35: Assertion failed
      Raised at Test_bounded_queue.Model.peek in file "examples/04-stateful-testing/test_bounded_queue.ml", line 10, characters 29-41
──────────────────────────────────────────────────────────

replay: dune exec examples/04-stateful-testing/test_bounded_queue.exe -- --seed s1:c26eddaeb764a645 -f 'behaves'
1 failed in 1.0ms.
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

## Observing the state after every call

A call that corrupts the state fails a program only when a later call
reads the corrupted part, so the program must draw both calls, in that
order. `~invariant` reads every value after every call instead: the
case fails at the call that corrupted the state, and the program needs
no call that observes it.

The snippets test `Lru`, the project's own cache of integer bindings,
which evicts its least recently used key. The model keeps the bindings
in a list, most recently used first, and `Lru.to_list` lists them in
the same order, so the invariant compares the two lists.

`test/test_lru.ml`:

```ocaml
module Model = struct
  (* The bindings, most recently used first. *)
  type t = { capacity : int; mutable entries : (int * int) list }

  let create capacity = { capacity; entries = [] }

  let add m k v =
    let rest = List.remove_assoc k m.entries in
    m.entries <- (k, v) :: List.filteri (fun i _ -> i < m.capacity - 1) rest

  let find m k =
    let found = List.assoc_opt k m.entries in
    Option.iter (add m k) found;
    found

  let length m = List.length m.entries
  let to_list m = m.entries
end

let bindings = list (pair int int)

let cache =
  abstract "c"
    ~pp:(fun ppf m -> Testable.pp bindings ppf m.Model.entries)
    ~invariant:(fun m c -> equal bindings (Model.to_list m) (Lru.to_list c))
```

The commands pair each function of `Model` with `Lru`'s: `create`,
`add`, `find`, `length` and `to_list`. If `Lru.find` returned a hit
without making its key the most recently used, the invariant would fail
right after that `find`:

```
$ dune exec test/test_lru.exe -- --seed s1:c26eddaeb764a645
lru: 1 test (seed s1:c26eddaeb764a645)
──────────────────────── failures ────────────────────────
  FAIL  Lru › behaves like a list
    test/test_lru.ml:42
      42 │ let caches = group "Lru" [ stateful "behaves like a list" commands ]

    counterexample (case 37, shrunk 11 steps): 4 calls, last: find
       #  reference before  call
       1                    let c1 = create 2
       2  []                add c1 3 0
       3  [(3, 0)]          add c1 0 0
       4  [(0, 0); (3, 0)]  find c1 3
    which failed with:
      after call 4 of 4, on c1
      expected  [(3, 0); (0, 0)]
                  ~       ~
      actual    [(0, 0); (3, 0)]
                  ~       ~
──────────────────────────────────────────────────────────

replay: dune exec test/test_lru.exe -- --seed s1:c26eddaeb764a645
1 failed in 3.8ms.
```

`after call 4 of 4, on c1` names the call after which the invariant
failed, and the value it failed on. Without the invariant, the program
must also draw a call that shows the order after the `find`, such as
`to_list`. Over 200 seeds, this test finds the bug on 144
without the invariant and on all 200 with it, and the median case that
finds it falls from 32 to 11.

The invariant runs on both sides after every call, and no row of the
report shows it, so it must not change the state. `to_list` qualifies.
An LRU's `find` does not, since it refreshes the key it finds, and the
program would then run on states that no row of the report explains.

## Taking an argument from what a value lists

A drawn argument knows nothing of the state. A `get` whose index is
drawn from `0` to `7` is out of range on most short arrays, and tests
the bounds check more often than the element it reads.
`among w t candidates` is the type of the elements that a value of `t`
lists, such as the indices an array has or the keys a map holds.
`candidates` lists them from the value's reference side, and a call
takes one with `^->`, as it takes a value.

The snippets test `Vec`, the project's own growable array of integers,
against a model that holds an `int list ref`. The model's `get` and
`set` raise `Invalid_argument` out of range, as `Vec`'s do.

`test/test_vec.ml`:

```ocaml
let vec = abstract "v" ~pp:(fun ppf m -> Testable.pp (list int) ppf !m)
let index = among int vec (fun m -> List.init (Model.length m) Fun.id)
let value = Gen.int_range 0 9

let commands =
  [
    command "create" (Gen.unit @-> makes vec) Model.create Vec.create;
    command "push" (vec ^-> value @-> returns unit) Model.push Vec.push;
    command "get" (vec ^-> index ^-> returns int) Model.get Vec.get;
    command "set" (vec ^-> index ^-> value @-> returns unit) Model.set Vec.set;
    command "get anywhere"
      (vec ^-> Gen.int_range (-2) 9 @-> returns int)
      Model.get Vec.get;
  ]
```

When `get` runs, its index is one of the indices that its array has at
that moment, the same on both sides, and the row prints it as `int`
prints it. An array with no element skips the call, as a `~pre` that
fails does. `get anywhere` keeps an index drawn around the bounds, for
the bounds check.

If `Vec.push` copied one element too few when the array grows, the
third push would lose the element at index 1:

```
$ dune exec test/test_vec.exe -- --seed s1:3d27fa99e7f6ae87
vec: 1 test (seed s1:3d27fa99e7f6ae87)
──────────────────────── failures ────────────────────────
  FAIL  Vec › behaves like a list
    test/test_vec.ml:34
      34 │ let vecs = group "Vec" [ stateful "behaves like a list" commands ]

    counterexample (case 2, shrunk 13 steps): 5 calls, last: get
       #  reference before  call
       1                    let v1 = create ()
       2  []                push v1 0
       3  [0]               push v1 1
       4  [0; 1]            push v1 0
       5  [0; 1; 0]         get v1 1
    which failed at:
      test/test_vec.ml:27
        27 │ command "get" (vec ^-> index ^-> returns int) Model.get Vec.get;
      call 5 of 5: get v1 1
      expected  1
      actual    0
──────────────────────────────────────────────────────────

replay: dune exec test/test_vec.exe -- --seed s1:3d27fa99e7f6ae87
1 failed in 1.7ms.
```

The element is drawn with the program as a place in its list, relative
to the list's length, so deleting an earlier call keeps its place. It
shrinks toward the head of the list, here to index 1, the first that
fails. Over 200 seeds, this test finds the bug on all 200, and on 187
when `get` and `set` draw their index from `0` to `7`. The median case
that finds it falls from 22 to 15.

An element reads the nearest value of `t` before it in the signature,
else the first one after it. A function that takes a key before its map
therefore takes it with no wrapper, as `Map.find` does here against an
association list:

```ocaml
(* fragment: M is Map.Make (Int), and a map's reference side is an association list *)
let map = abstract "m"
let key = among int map (List.map fst)
let find = command "find" (key ^-> map ^-> returns int) List.assoc M.find
```

With two values of `t` before it, an element reads the nearer one. In
`transfer src dst amount`, an amount among `src`'s balance would read
`dst`, so the signature takes `dst` first, and each side reorders the
arguments with a function.

An element has limits that a value does not have:

- It goes to the reference, the `~pre` and the system alike, so neither
  side may mutate it. An element is plain data, such as an index, a
  key or a path.
- `candidates` must not change the reference, and must give the same
  list from run to run, as a `~pre` must. What it raises breaks the
  reference, and the row prints the element it could not give as `_`.
- It depends on its value only. An argument bounded by another
  argument, such as the length of `blit src i dst j len`, which must
  fit after `i`, is drawn and kept legal by a `~pre`.
- It has no name, invariant or release, and no call makes one.
- On several domains a command that takes an element runs only in the
  prefix, as a command with a `~pre` does.

A command whose value never lists an element is never called, and the
test fails with `never called:` and a hint that names the listing:
`a call runs only where its arguments resolve, its value lists an element and its ~pre holds`.

## Checking an outcome the API leaves open

`judges w` ends the signature of an operation whose outcome the API
leaves open, such as the element that a bag's `take_any` removes. The
reference receives the system's outcome, `Ok v` or `Error e`, as its
last argument, and rules on it instead of predicting it. Returning
accepts the outcome, and the reference updates its state to follow
it. A verb's failure, or the system's own exception raised again,
rejects it.

```ocaml
(* fragment: Bag is the project's own, and the model of a bag is an int list ref *)
let bag = abstract "b"

let rec remove_one x = function
  | [] -> []
  | y :: r -> if y = x then r else y :: remove_one x r

let take_any =
  command "take_any"
    (bag ^-> judges (option int))
    (fun b seen ->
      match seen with
      | Ok (Some x) ->
          mem int x !b;
          b := remove_one x !b
      | Ok None -> equal (list int) [] !b
      | Error e -> raise e)
    Bag.take_any
```

## Judging calls against a rule

A model predicts every outcome. A system with no model worth writing,
such as a scheduler or the guardrail in front of an agent's tools,
still follows rules, and a reference whose commands end in `judges`
states them. It keeps in its state what the rules need to
know of the calls so far, receives each outcome, and accepts or rejects
it. In runtime-verification terms, such a reference is a monitor of the
program's history, written in OCaml: its state is the monitor's memory,
and each call gets a verdict.

The snippets test `Guard`, the project's own policy layer in front of
an agent's tools. Its `read_file` and `http_get` return `Ran` with the
tool's output, or `Blocked` with a reason, and its policy is that no
network call runs once a secret was read. The reference is the policy,
and knows nothing of what the tools return.

`test/test_guard.ml`:

```ocaml
(* Three spellings of one secret file, and a file that holds no secret. *)
let secrets =
  [ "secrets/api.key"; "./secrets/api.key"; "notes/../secrets/api.key" ]

let path = Gen.of_list ~pp:Format.pp_print_string ("notes.md" :: secrets)
let url = Gen.of_list ~pp:Format.pp_print_string [ "https://example.com/" ]

module Policy = struct
  type t = { mutable secret : string option } (* the first secret read *)

  let create () = { secret = None }

  let read_file m p = function
    | Ok (Guard.Ran _) ->
        if List.mem p secrets && m.secret = None then m.secret <- Some p
    | Ok (Guard.Blocked why) -> failf "read_file %s was blocked: %s" p why
    | Error e -> raise e

  let http_get m _ = function
    | Ok (Guard.Ran _) ->
        Option.iter (failf "http_get ran after %s was read") m.secret
    | Ok (Guard.Blocked why) ->
        if m.secret = None then
          failf "http_get was blocked in a clean session: %s" why
    | Error e -> raise e
end

let session =
  abstract "g" ~pp:(fun ppf m ->
      match m.Policy.secret with
      | None -> Format.pp_print_string ppf "clean"
      | Some p -> Format.fprintf ppf "read %s" p)

let reply = Testable.structural ~pp:Guard.pp_reply

let commands =
  [
    command "create" (Gen.unit @-> makes session) Policy.create Guard.create;
    command "read_file"
      (session ^-> path @-> judges reply)
      Policy.read_file Guard.read_file;
    command "http_get"
      (session ^-> url @-> judges reply)
      Policy.http_get Guard.http_get;
  ]
```

The reference judges the policy in both directions: `http_get` must be
blocked after a secret was read, and must run in a clean session, so a
guard that blocks everything fails too. `path` draws the spellings that
the policy must treat alike, three names of one secret file.

How the reference ends is its verdict:

- Returning accepts the outcome, and the reference's state follows it.
- A verb's failure (here `failf`'s), `Assert_failure` or
  `Match_failure` rejects it, and the call fails with the verb's lines.
  The system's own exception raised again rejects it too, so
  `Error e -> raise e` accepts no exception.
- Any other exception, `assume` and `reject` break the reference. The
  report names it as `reference of call N of N`, as for a model's bug
  (see [Reading a failure of the model](#reading-a-failure-of-the-model)).

A judge therefore rejects, with a verb, every outcome it does not
accept, in every state. A judge that crashes on an outcome it did not
foresee, as `List.hd` does on an empty list, breaks the reference, and
the report is then about the test, whatever the system did. On several
domains the judge also runs along orders of the calls that the system
did not take, and receives outcomes in states that only those orders
reach.

If `Guard` tested a path for the `secrets/` prefix before resolving
it, a read of `./secrets/api.key` would not count, and `http_get` would
run after it:

```
$ dune exec test/test_guard.exe -- --seed s1:c26eddaeb764a645
guard: 1 test (seed s1:c26eddaeb764a645)
──────────────────────── failures ────────────────────────
  FAIL  Guard › keeps its policy
    test/test_guard.ml:49
      49 │ let guards = group "Guard" [ stateful "keeps its policy" commands ]

    counterexample (case 6, shrunk 7 steps): 3 calls, last: http_get
       #  reference before        call                              result
       1                          let g1 = create ()
       2  clean                   read_file g1 ./secrets/api.key    Ran "read secrets/api.key"
       3  read ./secrets/api.key  http_get g1 https://example.com/  Ran "200 https://example.com/"
    which failed at:
      test/test_guard.ml:44
        44 │ command "http_get"
      call 3 of 3: http_get g1 https://example.com/
      http_get ran after ./secrets/api.key was read
──────────────────────────────────────────────────────────

replay: dune exec test/test_guard.exe -- --seed s1:c26eddaeb764a645
1 failed in 1.3ms.
```

A program that holds a judging call prints a `result` column, the
system's outcome of each call, since no `expected` value stands for it.
Under the failing call, the report prints the verb's lines, here the
message that `failf` formatted. That message, or a `satisfies` claim,
is all the report says about the rule, so a judge that checks a bound
puts the bound in it, as in `between 1 and 5 bytes`.

A judge that accepts too much passes silently. Check a new judge
against a planted bug, kept in the suite as an `xfail`, or with
[mutation testing](mutation.md).

One test mixes the two forms per command: `returns` where the reference
predicts, `judges` where it rules. An operation whose outcome the
reference predicts except in one corner the API leaves open is two
commands: a `returns` command whose `~pre` excludes the corner, and a
`judges` command whose `~pre` holds only there.

## Checking an obligation at rest

Some obligations hold only once the system is at rest: every request
that a server received is answered once the server has drained its
queue. While requests wait, such an obligation does not hold, so it is
no invariant. A command that brings the system to rest, then observes
it, checks the obligation wherever a program draws it.

```ocaml
(* fragment: Server is the project's own, and the reference holds the requests sent *)
command "drain"
  (server ^-> returns (slist int Int.compare))
  (fun sent -> !sent)
  (fun s ->
    Server.drain s;
    Server.answered s)
```

`slist` compares the answers in any order, since the obligation is that
each request is answered. The program goes on after a `drain`, so one
program can check the obligation in its middle as well as at its end.
The command finds a bug only when a program draws it after the state
that triggers the bug. A server that drops the request that arrives
while three are waiting is found on 148 of 200 seeds at the default
count, and on 196 at `~count:300`.

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

## Testing on several domains

`~domains:n` tests a structure meant to be shared between domains. A
program is then a short prefix, one branch per domain and a short
suffix. The prefix and the suffix run on the test's domain, and the
branches run at once, each on a domain of its own. The test fails when
no order of the calls, each branch keeping its order and the suffix
last, replayed on the reference, gives every outcome the system gave.

One command list serves both tests. `Mpmc` is the project's own queue,
meant to be shared, and the standard library's `Queue` is its
reference.

`test/test_mpmc.ml`:

```ocaml
open Windtrap

let queue = abstract "q"

let commands =
  [
    command "create" (Gen.unit @-> makes queue) Queue.create Mpmc.create;
    command "push"
      (queue ^-> Gen.int_range 0 9 @-> returns unit)
      (fun q x -> Queue.push x q)
      Mpmc.push;
    command "pop" (queue ^-> returns (option int)) Queue.take_opt Mpmc.pop_opt;
    command "length" (queue ^-> returns int) Queue.length Mpmc.length;
  ]

let mpmc =
  group "Mpmc"
    [
      stateful "behaves like Queue" commands;
      stateful ~domains:2 "behaves like Queue from two domains" commands;
    ]

let () = exit (run "mpmc" [ mpmc ])
```

Only the prefix has one state of the reference, so a command that makes
a value or has a `~pre` runs only there: every branch and the suffix
work on the prefix's values, and no call after the prefix is refused.
An operation meant to be called from several domains at once, such as
`pop_opt`, is total, and misuse there is an outcome. Each program runs
50 times, from no value each time, and a failure prints two more
columns: `domain`, the branch of each parallel call, and `result`, what
the system returned in the failing run. Under the table, the closest
order, the one whose first difference comes latest, names the call
where it differs, above its `expected` and `actual` pair.

If `Mpmc.push` forgot to take the queue's lock, two pushes at once
could lose one:

```
$ dune exec test/test_mpmc.exe
mpmc: 2 tests (seed s1:f1eb7b7406b0cca6)
──────────────────────── failures ────────────────────────
  FAIL  Mpmc › behaves like Queue from two domains
    test/test_mpmc.ml:20
      20 │ stateful ~domains:2 "behaves like Queue from two domains" commands;

    counterexample (case 0, shrunk 9 steps): 4 calls, 2 in parallel
       #  domain  call                result
       1          let q1 = create ()
       2  1       push q1 0           ()
       3  2       push q1 0           ()
       4          length q1           1
    which failed with:
      no order of the calls gives these results
      the closest order, 2 then 3, differs at call 4: length q1
      expected  2
      actual    1
──────────────────────────────────────────────────────────

replay: dune exec test/test_mpmc.exe -- --seed s1:f1eb7b7406b0cca6
1 passed, 1 failed in 81ms.
```

The invariant of an abstract type runs after the prefix's calls only:
after the branches, several orders may explain what the system did.

The contract differs from one domain's in five ways, all stated under
`Windtrap.stateful`: a replay draws the same programs but not the same
schedules, so it may pass; a counterexample does not run once more
after the search, except under `--mutate` and `--arm`; the test takes
no retries and ignores a group's; under `--mutate` and `--arm` each
program runs once on the test's domain, so a kill does not depend on a
schedule; and a call still running one limit after the test's limit
expired fails the test as timed out and stops the run after it.
Without a limit, a deadlock hangs until interrupted, and Ctrl-C works
on it.

The domains need a processor each beside the test's. On fewer, a
failure stays a failure, but fewer schedules are tried. A structure
known to be unsafe is a negative test,
`xfail ~reason:"not thread-safe" (stateful ~domains:2 "…" commands)`,
which fails as an unexpected pass when it finds nothing. The branches
run outside the handlers that surround `run`, so a system whose calls
need an effect handler is tested on one domain. Without a model the
system is its own reference, as in
`command "add" (h ^-> key @-> nat @-> returns unit) Hashtbl.add Hashtbl.add`:
the test then checks that parallel runs agree with sequential runs of
the same code.

## Running one program twice

A system that is its own reference runs each program twice, one run
per side, call by call. Where the two sides take the same arguments,
the runs must agree. Where a call takes a drawn pair and each side
takes one half, the runs differ in that input only, and no outcome that
`returns` compares may depend on it. This is noninterference: a secret
does not reach a public output.

The snippets test `Vault`, the project's own store of a pin, which keeps
a public log of what was done.

`test/test_vault.ml`:

```ocaml
let vault = abstract "v"
let pin = Gen.int_range 0 9999

let commands =
  [
    command "create" (Gen.unit @-> makes vault) Vault.create Vault.create;
    command "store"
      (vault ^-> Gen.pair pin pin @-> returns unit)
      (fun v (a, b) ->
        cover "the pins differ" (a <> b);
        Vault.store v a)
      (fun v (_, b) -> Vault.store v b);
    command "check" (vault ^-> pin @-> returns pass) Vault.check Vault.check;
    command "log" (vault ^-> returns (list string)) Vault.log Vault.log;
  ]
```

`store` stores a different pin in each run, and `log`, the public
output, must not tell the runs apart. `check` answers whether a pin is
the stored one, which may differ between the runs, and `returns pass`
compares nothing, so it declassifies that answer. The `cover` fails the
test when no case stored two different pins.

If `store` logged the last digit of the pin, the pair would shrink to
two pins that differ by one:

```
$ dune exec test/test_vault.exe -- --seed s1:c26eddaeb764a645
vault: 1 test (seed s1:c26eddaeb764a645)
──────────────────────── failures ────────────────────────
  FAIL  Vault › logs nothing of a pin
    test/test_vault.ml:19
      19 │ let vaults = group "Vault" [ stateful "logs nothing of a pin" commands ]

    counterexample (case 1, shrunk 21 steps): 3 calls, last: log
       #  call
       1  let v1 = create ()
       2  store v1 ((0, 1))
       3  log v1
    which failed at:
      test/test_vault.ml:16
        16 │ command "log" (vault ^-> returns (list string)) Vault.log Vault.log;
      call 3 of 3: log v1
      expected  ["stored ...0"]
                            ~
      actual    ["stored ...1"]
                            ~
    labels (1 passing case):
      100.0%  the pins differ
──────────────────────────────────────────────────────────

replay: dune exec test/test_vault.exe -- --seed s1:c26eddaeb764a645
1 failed in 0.9ms.
```

Draw both halves of the pair. A fixed variation, such as a second pin
of `p + 5000`, keeps the last digit, so it cannot see a leak of that
digit, while two drawn pins can differ in any digit.

With every argument shared, the same shape checks determinism. The two
runs take the same inputs, so their outcomes differ only through what
each run sees differently. The global `Random` state is one such
thing: the system's call advances it before the reference's call runs,
so each run draws from another place in it, and a system whose
outcomes depend on it fails. The runs see the rest of the process
alike, such as the environment and the file system, so a pass shows
only that this one perturbation revealed nothing. For a function,
`Law.ignores` states noninterference (see
[Stating a textbook law](property-testing.md#stating-a-textbook-law)).

## Stating a temporal property

Temporal logic states what a history of calls must satisfy. Windtrap
has no formula language, and each kind of temporal property takes one
of the forms on this page:

| In temporal logic | In windtrap |
| --- | --- |
| always, over the state | `~invariant` on the abstract type |
| never X after Y, once Y, X since Y | a reference that remembers what it needs of the past, and judges |
| a monitor | a reference whose commands end in `judges` |
| eventually, at quiescence | a drawn command that brings the system to rest, then observes it |
| a scenario that a program must reach | a `cover` in the reference function of the call that observes it |
| noninterference, determinism | the system as its own reference, run twice |

A `cover` demands its scenario on presence, and
[Discarding and labelling cases](property-testing.md#discarding-and-labelling-cases)
gives the count that a rare scenario needs.
