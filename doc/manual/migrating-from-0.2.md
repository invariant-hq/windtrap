# Migrating from 0.2

This page ports a suite written for windtrap 0.2 to this version. Each
entry is a breaking change: what a 0.2 suite wrote or relied on, then
what it writes now. A change this page does not list keeps a 0.2 suite
compiling and passing as before. The entries of
[`CHANGES.md`](../../CHANGES.md) after `0.2.0` list every change.

Six changes break every suite that uses them at compile time: the
signature of `cases`, `slow` and `tag` as annotations, the labels of
`starts_with`, `ends_with` and `Exn`, the labelled file of
`expect_file`, `judges` in place of `chooses`, and the type of `cover`.
Each fails with a type error at the call. The others compile and can
change a verdict.

## Declaring tests

- `cases ~name:f "parse" inputs fn` → `cases "parse" f inputs fn`. The
  function that named a row's test is the second positional argument.
  The name it gives is the row's identity. A table over the rows' own
  values identifies them by their printing,
  `cases "rev" (Testable.to_string (list int)) inputs fn`, and a table of
  strings by the quoted string, `cases "t" (Printf.sprintf "%S") …`. A
  text shown beside a row and identifying nothing is `~label:f`.

  ```ocaml
  (* 0.2 *)
  cases ~name:fst "parses" [ ("1", 1); ("10", 10) ] (fun (s, n) ->
      equal int n (int_of_string s))

  (* now *)
  cases "parses" fst [ ("1", 1); ("10", 10) ] (fun (s, n) ->
      equal int n (int_of_string s))
  ```
- `slow "n" f` → `slow @@ test "n" f`. The optional arguments go on
  `test`: `slow ~timeout:60. "n" f` → `slow @@ test ~timeout:60. "n" f`.
  `slow` marks any test or group, as `focus` and `xfail` do. A choice
  of constructor, `(if heavy then slow else test) "n" f`, is a choice of
  mark: `(if heavy then slow else Fun.id) @@ test "n" f`.
- `~tags:[ "gpu" ]` on `test`, `group`, `slow`, `cases`, `bracket`,
  `scoped`, `prop` or `stateful` → `tag "gpu" @@ …` around the
  declaration, one `tag` per tag. `~tags:[ "slow" ]` → `slow @@ …`. A
  helper that took `?tags` only to pass it on drops it, and its caller
  tags the helper's result.
- A table is one test, not a group of one test per row:
  - `-f 'parses › 10'`, which selected one row → `-f parses`, which
    selects the table; `--failed` then reruns only the rows that
    failed.
  - one `-v` line and one summary count per row → one per table, whose
    line says `holds on all 2 rows`.
  - `~timeout` and `~retries` per row → over the whole table. A
    group's `~timeout` reaches a table the same way, as
    `group ~timeout:60. "graphs" [ cases "chain" … ]`: it bounds the
    table, where it bounded each row. Size the limit for every row
    together.
  - `--shard K/N`, which spread the rows over the shards → a table goes
    to one shard whole, so a suite split into shards rebalances, and a
    shard that holds a large table takes longer. Split a large table into
    several to spread it again.
  - `current_test ()` inside a row, which gave the row's path → the
    table's path, since the table is the test, and no call gives the
    row's. A name per row, as a golden file's, comes from the row's own
    value, which the function receives:

    ```ocaml
    cases "renders" fst docs (fun (key, doc) ->
        expect_file ~file:("golden/" ^ key ^ ".txt") (render doc))
    ```
  - `skip` in a row, which skipped that row's test → skips that row
    alone, as before, and the table goes on with the next. The summary
    counts the table once: failed if a row failed, else passed if a row
    passed, and skipped only if every row skipped. Under `-v` a passing
    table with rows that skipped says
    `holds on 2 of 3 rows, 1 skipped: REASON`, with the first row's
    reason.
- `cases "t" f [] fn`, which declared no test → a vacuous test that
  fails; remove the table or give it rows.

## Assertions

- `starts_with ~affix:p s` → `starts_with ~prefix:p s`, and
  `ends_with ~affix:x s` → `ends_with ~suffix:x s`. These are the labels
  of `String.starts_with` and `String.ends_with`.
- `Exn.invalid_arg ~substring:m`, `Exn.failure ~substring:m` and
  `Exn.sys_error ~substring:m` → `~sub:m`, the label of `contains`.
  `?substring:None` → `?sub:None`, and a punned `~substring` →
  `~sub:substring`.
- `equal (slist t cmp)` over two equal multisets in another order,
  under a `cmp` coarser than `t`'s equality, which failed in 0.2 →
  passes. `slist` compares multisets under `t`'s equality, and `cmp`
  only sorts. A `not_equal` under such an `slist` can fail now.
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

## Baselines

- `expect_file text "golden/x.txt"` →
  `expect_file ~file:"golden/x.txt" text`. The file is labelled, so the
  two strings cannot be swapped.
  `render doc |> expect_file ~file:"golden/x.txt"` reads in order.
- A stanza that runs `--corrected` with no `diff?` for a file that holds
  a baseline, which passed under `CI` when that file was stale → the
  run exits 1 under `CI`, and writes the correction still. Add the
  file's `diff?`, so that `dune promote` can accept the correction
  outside CI.
- An `expect_file` whose text lacks the final newline of its file, or
  ends with one the file lacks, which 0.2 passed by giving both sides a
  final newline → a mismatch, whose block says
  `values differ only by a trailing newline (on the expected side)`.
  0.2's `-u` and `--corrected` always wrote a final newline, so a 0.2
  baseline of a text that does not end with one now fails. End the
  produced text with a newline, or re-record the file: `dune runtest`
  then `dune promote`, or `-u` outside dune, which now write the text
  byte for byte. CR and CR LF still read as LF.

## Property testing

- `cover` passed as a value of type `string -> bool -> unit`, as
  `let check ?(cover = cover) …` called with `~cover:(fun _ _ -> ())` →
  does not type, since `cover` takes `?share:float` now and its type is
  `?share:float -> string -> bool -> unit`. Where the code needs a value
  of the old type, write `(fun label hit -> cover label hit)`.
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
  generators parse choices, the integer generators draw small values
  first and repeat values, the float generators draw corner cases, and
  a stateful test draws its commands with another probability. Run the
  failing test again under this version and use the `replay:` line it
  prints, or keep the counterexample in the source with the
  `keep it: ~examples:[ … ]` line, or `--replay TOKEN`: a token holds
  the case's choices, which no distribution reads, so it replays its
  case whatever the generators draw.
- A property that relied on a wide integer range spreading its values
  uniformly, as a `cover ~share` over `Gen.int_range 0 9999`, or on two
  values of one generator differing → an integer comes small or at an
  edge of a power of two first, and a draw repeats an earlier value of
  its generator in the case one time in eight. Nearly nine values in
  ten of `Gen.int_range 0 9999` are below 2500. State a share over a
  range of at most 256 values, which stays uniform apart from its
  corners, as `n < 50` over `Gen.int_range 0 199` for a quarter, and a
  law that needs two distinct values with `assume (a <> b)`.

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
- A value made at declaration, which 0.2 made once for the whole run →
  made again by each worker, which starts the test executable again. A
  device, a connection or a pool opened at declaration works in the
  test as it did in 0.2, but each worker has its own: a module-level
  counter starts over in each worker, under `-j` and after a worker
  retires, so file names drawn from it repeat names an earlier worker
  used, and a file that an earlier test left under such a name collides
  (`File exists`). Take a unique directory from `temp_dir ()` in the
  test, or remove what the test made before it ends. An `at_exit`
  handler registered at declaration, which 0.2 ran once, runs in each
  worker as it ends, and a declaration that clears or writes a shared
  directory does so in each worker, at the same time under `-j`.
- A declaration that is not the same at each start of the program, as
  tests named from the clock or a random generator, or one per entry of
  a directory that the suite writes to → an error: each worker starts
  the test executable again and must declare its parent's tests, and
  the tests of a worker that declares others fail, saying so. Name the
  tests from fixed data, and draw the varying part inside the test.
- A declaration that takes what only one process may hold, as a lock on
  a file or a device opened exclusively, which 0.2 took once → each
  worker waits for it forever, since the process that called `run` holds
  it; an interrupt then says that a worker was still in module
  initialisation. Take it in a `fixture`, or in the test.
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
