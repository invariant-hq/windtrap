# Getting started

In this tutorial we add windtrap to a dune project, write a suite of
three tests for a small module, run it, read one failure and fix it.
After it we can read a green report and a failing one, and we know which
page to open next.

## Installing windtrap

We install the library with opam:

```
opam install windtrap
```

## The module under test

The module we test is `Calc`, written for this tutorial. We create a
directory `test` at the root of the project and put three files in it.
The three files ship as `examples/01-getting-started/` in windtrap's
repository. The first file is the module.

`test/calc.ml`:

```ocaml
exception Parse_error of string

let add a b = a + b

let parse input =
  let trimmed = String.trim input in
  if trimmed = "" then raise (Parse_error "empty")
  else
    match int_of_string_opt trimmed with
    | Some n -> n
    | None -> raise (Parse_error ("not a number: " ^ trimmed))
```

`add` adds two integers. `parse` reads one integer from a string and
raises `Parse_error` when the string is empty or not a number.

## The first suite

A suite is one executable, declared by a `(test)` stanza.

`test/dune`:

```lisp
(test
 (name test_mylib)
 (modules test_mylib calc)
 (libraries windtrap))
```

The suite declares its tests and runs them.

`test/test_mylib.ml`:

```ocaml
open Windtrap

let add =
  group "add"
    [ test "adds two integers" (fun () -> equal int 5 (Calc.add 2 3)) ]

let parse =
  group "parse"
    [
      test "rejects the empty string" (fun () ->
          raises (Calc.Parse_error "empty") (fun () -> Calc.parse ""));
      prop "reads back any integer that string_of_int prints" Gen.int (fun n ->
          equal int n (Calc.parse (string_of_int n)));
    ]

let () = exit (run "mylib" [ add; parse ])
```

`group` names a list of tests, here the tests of one function.
`test` declares a test from a name that states its claim and a body that
passes by returning and fails by raising. `equal int 5 (Calc.add 2 3)`
asserts that the sum is `5` under the `int` witness.
`raises (Calc.Parse_error "empty") (fun () -> Calc.parse "")` asserts
that the call raises that exception. `prop` declares a property, a
test whose body runs once for each of 100 integers that `Gen.int`
generates (see [Property testing](property-testing.md)). `run` executes
the groups and returns the exit code, which `exit` hands to the shell on
the file's last line.

## Running the suite

The three tests pass, and a run with nothing to report prints one
line. It ends with the seed the property drew its integers from:

```
$ dune runtest
mylib: 3 passed in 1.0ms (seed s1:b02192cebcec40d2).
```

## A failing test

To see how a failure reads, we change the `5` in the `add` test to `6`
and run again. Under `FAIL` the report gives the test's path, the line
of the failing assertion, the source of that line, and the two values,
`expected` first. An assertion in tail position, the last expression of
a body as here, is reported at the test's declaration line (see
[Locating a failing assertion](assertions.md#locating-a-failing-assertion)):

```
$ dune runtest
File "test/dune", line 2, characters 7-17:
2 |  (name test_mylib)
           ^^^^^^^^^^
mylib: 3 tests (seed s1:4370715203d03b81)
──────────────────────── failures ────────────────────────
  FAIL  add › adds two integers
    test/test_mylib.ml:5
      5 │ [ test "adds two integers" (fun () -> equal int 6 (Calc.add 2 3)) ]

    expected  6
    actual    5
──────────────────────────────────────────────────────────

2 passed, 1 failed in 0.9ms.
```

## The fix

We put the `5` back and run again:

```
$ dune runtest
mylib: 3 passed in 0.6ms (seed s1:a59d57ed8371d91e).
```

## Where to go next

We have a suite that builds with the project, runs under `dune runtest`
and reports a failure with the two values it compared. The rest of the
manual has one page per kind of test and per workflow.

- [Assertions](assertions.md) covers the other verbs, the witnesses for
  other types, and the `~__POS__` argument that gives a failure the
  assertion's own line.
- [Running tests](running-tests.md) covers selecting tests, rerunning
  the last failed tests, and the report under CI.
- [Property testing](property-testing.md) covers `prop`, generators and
  shrinking.
- [Resources and structure](resources-and-structure.md) covers a suite
  over several files, the tests of a library's internals written next to
  its code with `let%test`, and the resources a test acquires.

The [index](../../README.md#documentation) lists every page with its
kind, and `lib/windtrap.mli` is the reference.
