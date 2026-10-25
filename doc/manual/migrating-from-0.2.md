# Migrating from 0.2

This page ports a suite written for windtrap 0.2 to this version. Each
entry is a breaking change: what a 0.2 suite wrote or relied on, then
what it writes now. A change this page does not list keeps a 0.2 suite
compiling and passing as before. The entries of
[`CHANGES.md`](../../CHANGES.md) after `0.2.0` list every change.

Two changes break every suite that uses them at compile time: the
signature of `cases`, and `judges` in place of `chooses`. The others
compile and can change a verdict.

## Declaring tests

- `cases ~name:f "parse" inputs fn` →
  `cases "parse" (Testable.contramap f text) inputs fn`. A table names
  its rows by how its witness prints them, so the function that named
  them becomes a witness, and a witness of the rows themselves, as
  `cases "rev" (list int) inputs fn`, needs no name at all. A text shown
  beside a row and identifying nothing is `~label:f`.

  ```ocaml
  (* 0.2 *)
  cases ~name:fst "parses" [ ("1", 1); ("10", 10) ] (fun (s, n) ->
      equal int n (int_of_string s))

  (* now *)
  cases "parses" (Testable.contramap fst text) [ ("1", 1); ("10", 10) ]
    (fun (s, n) -> equal int n (int_of_string s))
  ```
- A table is one test, not a group of one test per row:
  - `-f 'parses › 10'`, which selected one row → `-f parses`, which
    selects the table; `--failed` then reruns only the rows that
    failed.
  - one `-v` line and one summary count per row → one per table, whose
    line says `holds on all 2 rows`.
  - `~timeout` and `~retries` per row → over the whole table.
  - `skip` in a row, which skipped that row's test → skips that row
    alone, as before, and the table goes on with the next. The summary
    counts the table once: failed if a row failed, else passed if a row
    passed, and skipped only if every row skipped. Under `-v` a passing
    table with rows that skipped says
    `holds on 2 of 3 rows, 1 skipped: REASON`, with the first row's
    reason.
- `cases "t" w [] fn`, which declared no test → a vacuous test that
  fails; remove the table or give it rows.

## Assertions

- A failing verb inside a catch-all, `(try equal int 1 (f ()) with _ ->
  ())`, which passed in 0.2 → the test fails, and the block says
  `failed, then caught`. A library that ignores what its callbacks
  raise no longer hides a failing verb. Code that retries a verb until
  it holds, `let rec wait () = try check () with _ -> wait ()` →
  `eventually ~within:1. check`, which fails with the last call's
  failure.
- A verb on a domain the test spawned whose exception never reached the
  test, which passed in 0.2 → the test fails.
- A baseline holding a float printed by a witness or a generator as
  `inf`, `-inf` or `-nan` → `infinity`, `neg_infinity` and `nan`;
  accept the new text with `dune runtest` and `dune promote`.

## Property testing

- `prop ~count:0 "p" g law` with no `~examples`, which passed in 0.2 →
  vacuous, a failing verdict: it checks nothing. Give it `~examples`, or
  a count.
- A property whose every case was discarded, an unmet `cover`, and a
  stateful command never called, which failed in 0.2 as plain failures
  → `VACUOUS` verdicts, counted as `N vacuous` in the summary; the exit
  code is `1` as before.
- An `expect`, `expect_exact` or `expect_file` in the law of a property
  or of a stateful test, which recorded one mismatch per failing case,
  passed the case and could offer a correction → the case fails and
  shrinks to one counterexample, and no correction is written. Compare
  against a value the law computes, `equal t (model x) (system x)`, or
  move the `expect` to a test of its own over chosen values.
- `--prop-count 10` (`WINDTRAP_PROP_COUNT=10`) to cut every property
  short → no effect on a property that runs more: `--prop-count` raises
  a count and lowers none. Lower a property's `~count` in its
  declaration, or select fewer tests with `-f`.
- A `--seed` or a `replay:` line that a 0.2 run printed → other cases:
  generators parse choices, the float generators draw corner cases, and
  a stateful test draws its commands with another probability. Run the
  failing test again under this version and use the `replay:` line it
  prints, or keep the counterexample in the source with the
  `keep it: ~examples:[ … ]` line, or `--replay TOKEN`.

## Stateful testing

- `chooses w` → `judges w`. The reference rules on the outcome it
  receives instead of returning the one it accepts: returning accepts
  it, and a verb's failure, or the system's own exception raised again,
  rejects it.

  ```ocaml
  (* 0.2 *)
  (bag ^-> chooses (option int))
  (fun b seen ->
    match seen with
    | Ok (Some x) when List.mem x !b ->
        b := remove_one x !b;
        Some x
    | Ok None when !b = [] -> None
    | _ -> ( match !b with [] -> None | x :: _ -> Some x))

  (* now *)
  (bag ^-> judges (option int))
  (fun b seen ->
    match seen with
    | Ok (Some x) ->
        mem int x !b;
        b := remove_one x !b
    | Ok None -> equal (list int) [] !b
    | Error e -> raise e)
  ```

  A reference that returned the outcome it received returns `()`; one
  that returned another outcome to reject it fails with a verb, as
  `equal` or `mem`; `Error e -> raise e` now rejects the exception, and
  any other exception the reference raises breaks the reference and is
  reported as the test's own bug.
- On several domains, a structure that is only sequentially consistent,
  which 0.2 accepted in program order → a failure: the judge accepts
  only the orders that keep real time, so a call that returned before
  another began is ordered first, and a write that returned before a
  read on another domain began must be seen by it. Fix the structure, or
  test the weaker claim on one domain.

## Running tests

- A test with no `~timeout` in a run with no `--timeout`, which could
  run forever → a limit of ten minutes; a test meant to run longer
  declares `~timeout:3600.`, or the run passes `--timeout`.
- A test that installs its own `SIGALRM` handler or calls `Unix.alarm`,
  which 0.2 left alone when the test declared no `~timeout` → the
  runner owns `SIGALRM` during every test; run such a test alone, with
  `-f` and `--in-process`, which sets no limit.
- State that the tests leave for the code after `run`, as
  `let () = let code = run "s" tests in print_int !count; exit code` →
  the tests run in worker processes, and the process that calls `run`
  sees none of their effects; assert in the tests. A test can run in
  another worker than the one before it, under `-j`, or after a worker
  that ran a test that spawned a domain retires, so a test that reads
  state an earlier test left sets it up itself, or in a `fixture`. An
  `at_exit` handler that a test registers runs as its worker ends.
- A declaration that is not the same at each start of the program, as
  tests named from the clock or a random generator, or one per entry of
  a directory that the suite writes to → an error: each worker starts
  the test executable again and must declare its parent's tests, and
  the tests of a worker that declares others fail, saying so. Name the
  tests from fixed data, and draw the varying part inside the test.
- Expensive work at declaration, as loading a dataset into a module-level
  value, which 0.2 paid once → paid once per worker start, since each
  worker runs module initialisation again, a worker that replaces a
  retired one included. Make the value `lazy`, or a `fixture`, so that
  only a worker whose tests use it pays for it.
- The record that `--failed` reads, which 0.2 kept per suite name →
  per executable and suite: the first `--failed` after upgrading finds
  no record and exits `2`; run the suite once.
- A script that reads the last line of a run that selected nothing,
  which 0.2 ended on the `list:` hint → the summary is the last line,
  and the hint is the line above it.

## Mutation testing

- A verdict file that `--mutate` wrote under 0.2 → refused by
  `windtrap mutants`, since the file now records where each mutant ends
  (`windtrap-mutants-v5`); run the mutation tests again.
