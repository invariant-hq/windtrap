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
  run "mylib"
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
executes it and exits the process — `0` when everything passed, `1` on
any failure, `2` when nothing ran (the filter-typo case). A test body
passes by returning and fails by raising; the assertion verbs raise
structured failures that render as reports. A green, healthy run is
exactly one line; anything worth your attention — a failure, or a test
that got slow — brings out the header and the per-test glyph row
(`.` for a pass); `-v` prints one status line per test instead (see
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
F
──────────────────── failures (1) ────────────────────
  FAIL  users › sessions after login
    test/test_mylib.ml:19
      19 │               equal

    expected  [("alice", [1; 2; 3]); ("bob", [4])]
                                     ~~~~~~~~~~~~
    actual    [("alice", [1; 2; 3]); ("bob", [4; 5]); ("carol", [])]
                                     ~~~~~~~~~~~~~~~  ~~~~~~~~~~~~~
──────────────────────────────────────────────────────
1 failed in 0.000781s.
rerun failures only: dune exec test/test_mylib.exe -- --failed
```

No `~pos` annotation, no printer boilerplate: the location comes from
the assertion's call stack, and the diff is computed from the printed
values — every type gets it, not just strings. `equal` takes a
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
| `snapshot name actual` | compare to `__snapshots__/<file>/<name>.snap`; accept with `-u` |
| `let%expect_test` + `[%expect {|…|}]` | inline output tests (`ppx_windtrap`); accept with `dune promote` |
| `cases name inputs fn` | one selectable test per input |
| `bracket ~setup ~teardown name fn` | per-test resource |
| `fixture ?teardown create` | shared resource, released by the runner |
| `ftest` / `fgroup` | focus while debugging (refused under CI) |
| `fail` / `failf` / `skip ~reason ()` | escape hatches |

Custom types are one line:
`let point = Testable.make ~pp:Point.pp ~equal:Point.equal`.

Every failure prints the exact command to rerun, replay, or accept it.
From here: [Assertions](assertions.md) for the full verb set,
[Property testing](property-testing.md), [Snapshots and expect
tests](snapshots-and-expect.md), or [Running tests](running-tests.md)
for the CLI. Runnable versions of each chapter's code live under
`examples/` in the distribution.
