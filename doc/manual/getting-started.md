# Getting started

```
opam install windtrap
```

A suite is one executable. `test/dune`:

```lisp
(test
 (name test_mylib)
 (libraries windtrap mylib))
```

`test/test_mylib.ml`:

```ocaml
open Windtrap

let () =
  exit
  @@ run "mylib"
       [
         test "addition" (fun () -> equal int 5 (Calc.add 2 3));
         group "parser"
           [
             test "empty input" (fun () ->
                 raises (Parse_error "empty") (fun () -> Calc.parse ""));
           ];
       ]
```

```
$ dune runtest
mylib: 2 passed in 0.000689s.
```

That is the whole model: `test` and `group` declare inert data, `run`
executes it and returns the exit code — `0` when everything passed, `1`
on any failure, `2` when nothing ran (the filter-typo case) — and `exit`
hands that code to the shell. `run` returns the code rather than
applying it so that one binary can host two suites or post-process a
run, and a `main` that forgets the `exit` is a type error rather than a
binary that is green on failure. A test body passes by returning and
fails by raising; the assertion verbs raise structured failures that
render as reports. A green, healthy run is
exactly one line; anything worth your attention — a failure, a test
that got slow, a test that passed on a retry — brings out the header
and a block saying what; `-v` streams one status line per test (see
[Running tests](running-tests.md)).

## A failing test

Change the expectation and the report does the diagnosis for you:

```ocaml
group "users"
  [
    test "sessions after login" (fun () ->
        let sessions = Sessions.all () in
        equal
          (list (pair string (list int)))
          [ ("alice", [ 1; 2; 3 ]); ("bob", [ 4 ]) ]
          sessions;
        equal int 3 (List.length sessions));
  ];
```

```
$ dune runtest
mylib: 1 test
──────────────────── failures (1) ────────────────────
  FAIL  users › sessions after login
    test/test_mylib.ml:19
      19 │               equal

    expected  [("alice", [1; 2; 3]); ("bob", [4])]
    actual    [("alice", [1; 2; 3]); ("bob", [4; 5]); ("carol", [])]
                                               ~~~~~~~~~~~~~~~~~~
──────────────────────────────────────────────────────

1 failed in 0.000781s.
```

No `~__POS__` annotation, no printer boilerplate: the location comes from
the assertion's call stack, and the diff is computed from the printed
values — every type gets it, not just strings. The one case the call
stack cannot serve is an assertion in tail position — the last
expression of a body such as `test "adds" (fun () -> equal int 4 (add 2 2))`
— whose frame is gone by the time it raises: the report then names the
test's declaration line and says so underneath
(`(assertion in tail position: its line is unknown; ~__POS__ names it)`);
`~__POS__` on that assertion puts its own line back. `equal` takes a
*testable* (`int`, `string`, `list (pair string (list int))`, …): a
printer plus an equality, composed like the type itself.

## The vocabulary

| | |
| --- | --- |
| `test name fn` / `group name [...]` | declare; groups nest freely |
| `equal ty expected actual` | the workhorse; expected first, always |
| `require_some o` / `require_ok r` | assert *and unwrap*: `let v = require_some (find k) in …` |
| `raises exn fn` / `raises_match pred fn` | exceptions |
| `prop name gen fn` | property test over an `'a Gen.t`; failures shrink and replay |
| `expect actual @@ __POS_OF__ {|…|}` | compare to the literal at the call; accept with `dune promote` (a `--corrected` stanza) or `-u` |
| `expect_file actual path` | compare to a committed file; accepted the same way |
| `let%expect_test` + `[%expect {|…|}]` | inline output tests (`ppx_windtrap`); accept with `dune promote` |
| `cases ~name base inputs fn` | one selectable test per input |
| `bracket ~setup ~teardown name fn` | per-test resource |
| `fixture ?teardown create` | shared resource, released by the runner |
| `focus t` | focus a test or a group while debugging (refused under CI) |
| `fail` / `failf` / `skip ~reason ()` | escape hatches |

Custom types are one line:
`let point = Testable.make ~pp:Point.pp ~equal:Point.equal`.

Every failure that needs a command to resolve it prints that command:
the acceptance line under a baseline mismatch, the replay line under a
property failure.
From here: [Assertions](assertions.md) for the full verb set,
[Property testing](property-testing.md) and [Stateful
testing](stateful-testing.md), [Baselines and expect
tests](baselines.md), or [Running tests](running-tests.md)
for the CLI. Runnable versions of each chapter's code live under
`examples/` in the distribution, one per chapter.
