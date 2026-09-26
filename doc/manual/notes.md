# Design notes

This page explains why windtrap is shaped as it is. Each section states
one decision and the reasons for it. The how-to pages show how to use
what the decisions give, and `lib/windtrap.mli` is the reference.

## A suite is an executable, and `run` returns its exit code

A suite is an ordinary program. `run` executes a list of tests and
returns 0, 1 or 2, and the last line of the file hands that code to
`exit`. Returning the code lets one binary run two suites, and lets a
harness read a run's result without a child process. It also makes a
forgotten `exit` a type error: `let () = run …` does not compile, and
only an explicit `ignore` drops the code. `Cmd.eval` in cmdliner returns
its exit code the same way.

## Tests are values, listed by hand

`test` and `group` build an inert tree, and nothing runs until `run`
executes it. The runner can list, select, shard and count a suite
before any test runs, and a declaration cannot fail. The suite is the
list its file hands to `run`, not a registry filled as modules load, so
no link order decides what runs and a test reaches a suite only when the
suite names it. The price is that a test left out of the list does not
run, and `-l` is how to see what does. Inline tests are the exception:
they live beside the code they test, and the ppx registers them as
their module loads.

## A test's path is its identity

A test is named by its groups and its own name, and that path keys what
the runner keeps about it: the seed of a property, the record `--failed`
reads, and the bucket of `--shard`. A position or an index changes when
a test is added above another, while a name changes only when its author
renames it. This is why `cases` requires `~name`: a child named by its
index would take a new seed and lose its record each time a row is
inserted before it. For the same reason a suite in which two tests share
a path is refused.

## A witness is a printer and an equality

An assertion compares values under a witness, a printer and an equality
with an optional order. The printer makes a failure readable. A failure
shows the values it compared, and the diff is computed from their
printed forms, so every type with a printer gets a diff without diffing
code of its own. Witnesses compose like the types they describe, as
`list int` does, and `Testable.contramap` narrows a record to the fields
a test is about. Generation lives in `Gen` and never in a witness, so a
witness stays two functions and an order.

## Generators shrink as they generate

A generator draws a value together with the tree of its smaller
variants. Every generator therefore shrinks, and one built with `map`,
`bind` or `such_that` shrinks through its parts and keeps their
constraints, so a counterexample still satisfies what its generator
promised and no shrinker is written by hand. A generator carries its
printer too: a composite prints when its parts do, and a value computed
by a function with no printer prints the input it was computed from.
`Gen.with_pp` attaches a printer where none derives.

## Seeds replay from the path

Every generated value derives from the run's root seed, the test's path
and the case's index. Adding, removing or reordering other tests changes
no property's values, and a `replay:` line reproduces a failure from its
seed alone. The derivation is frozen under the `s1` prefix of the seed,
and what a generator draws from it is fixed within one version of
windtrap. The shrink budget is fixed too, with no option to change it,
so a replay descends to the same counterexample.

## Baselines are where the source says

A baseline is the literal at an `expect` call or the file an
`expect_file` call names. Nothing derives a baseline from a test's name
or declaration file, so renaming or moving a test cannot orphan one, and
no baseline depends on debug information. The compiler computes a
literal's position through `__POS_OF__`, and a correction rewrites that
literal in place. The `expect` family takes the produced text first and
the literal last, so a `{|…|}` block closes the call and reads as a
block; `equal` keeps the expected value first.

An expectation records its mismatch and returns. One run reports every
stale expectation of a test, and one correcting run accepts them all,
where an expectation that raised would take one round per stale literal.

## Checking reads, and accepting is a separate gesture

A run that checks writes nothing to the source tree. Under dune, a
`(test)` stanza whose suite holds baselines runs it with `--corrected`,
as the inline runner does, which writes each correction beside its
file. `dune promote` accepts what dune's `diff?` showed, one review
gesture for inline tests and suites alike.
Outside dune, `-u` rewrites in place. Neither has an environment
variable, so acceptance is never a setting a build action could inherit,
and `-u` is refused under `CI`, where nobody reviews what it writes.

## One runner

Inline tests desugar into the library. A `let%expect_test` is a `test`,
an `[%expect]` node is an `expect` call on the captured output, and
dune's inline-test runner hands each partition to the same `run` under
`--corrected`. An inline expectation therefore has the matcher, the
correction, the report and the exit codes of any other, and a change to
the runner reaches both. A second runner would drift from the first, and
every flag, report line and exit rule would be stated twice.

## Flags, and mirrors for runs with no command line

Every setting of a run is a flag, and each flag a build action can use
has a `WINDTRAP_*` mirror read through the flag's own parser. The
mirrors exist for `dune runtest` and for inline suites, which dune
starts with no command line of the user's. A mirror has its flag's
grammar, so a flag and its mirror are one vocabulary. Acceptance,
listing, `--failed` and `-x` have no mirror, since they are gestures at
a command line. One variable holding extra arguments would be a second
command line, with a splitting grammar of its own.

## Resources belong to a test

Groups have no setup or teardown hooks. `bracket` and `scoped` hold a
resource for one test, and `fixture` shares one across the run, acquired
inside the first test that uses it and released after the last. All the
user's code then runs inside some test's boundary, so a failing setup is
that test's failure, with its name, its captured output and its timeout.
A hook around a group runs outside every test, and its failure belongs
to none of them. The runner releases the fixtures on every path where it
regains control, `-x` and an interrupt included, and an interrupt skips
only the teardown of the test it cut.

## Failures are data

Every outcome is recorded as data in one run record, and every output
is a projection of it. The terminal report, the JUnit file and the
GitHub annotations read the same failures, so they cannot disagree. A
projection formats and truncates; it never changes a status, a count or
an exit code.

## The report ends on its outcome

A run with nothing to report prints one line. A failure prints as a
block when its test finishes, so a run that crashes or hangs has already
printed what it knew. The summary is the last line, and `tail -1` tells
how a run ended. The report goes to standard output, and what windtrap
says about itself, a refusal, a warning or a usage error, goes to
standard error behind `windtrap:`, so a log keeps the two apart.

A block ends with a command only when the command says something the
block does not: the seed in `replay:`, the file in `accept:`, the mutant
in `reproduce:`. Running the tests again is the ordinary next step and
needs no line.

## Three exit codes

0 means that no selected test failed, 1 that one did, and 2 that no test
ran. A mistyped filter selects nothing, and 2 keeps it from passing as a
green run. Under `--corrected` a recorded correction is no failure,
because the `diff?` that follows is the verdict.

What the environment broadcasts is not an error of a suite that cannot
honour it. A mirror reaches every stanza of a project, so a filter meant
for one suite empties the others. A selection that only the mirrors gave
therefore returns 0 when it keeps nothing, while one typed on a command
line is a typo and returns 2. For the same reason `WINDTRAP_MUTATE` runs
a suite with no mutant to test as usual, where `--mutate` refuses, and a
relative path in a mirror is read from the project root, where one on a
command line is read from the working directory.

## A test cannot end the run

A call to `exit` in code under test is intercepted and recorded as the
failure of its test. Without the interception, a function that exits 0
on some path would end the run green with tests unrun. Only `Sys.Break`
and `Out_of_memory` end a run from inside a test; a `Stack_overflow` is
an ordinary failure, since OCaml 5 recovers from it.

## Coverage is measured by runs and reported by a command

A test run prints no coverage number. Each suite writes a dump of the
code its executable links, and suites over one library link different
parts of it, so each suite's own number is a different view and none is
the project's. `windtrap coverage` merges every dump into the project's
number and holds the gate, `--min`, in one place. Instrumentation never
changes what a program or a test means. It only counts.

## Coverage counts returns

A point counts the entry of a block and, for a call, its return. A call
that raises leaves its point unvisited, so an exception's path reads
uncovered instead of covered for having been entered. OCaml code raises
often, and a report that counted every entered call would hide the
paths exceptions take. A call in tail position has no point of its own,
since observing its return would cost the tail call. Exclusions are
attributes in the source, in the spelling Bisect_ppx uses, so an
exclusion moves with its code.

## A data file names the executable that wrote it

Each dump and verdict file records the path of its executable and a
digest of its bytes, and a report leaves out a file whose executable was
rebuilt or deleted since. The digest stands in for a modification time,
which cannot tell a rebuilt executable from the one that wrote the file:
dune's cache restores a rebuilt artifact with its original time. No
option keeps a stale file, since a number computed from another build's
data describes a program that no longer exists.

## A suite is its own mutation runner

The test executable runs its own mutation run under `--mutate`. It is
the process that knows which test is running, so it can record which
tests evaluate each mutant and name them in a survivor's block, which
turns a score into a list of tests to strengthen. It forks one child per
reached mutant from its own warm state, and each child runs only the
tests that reach its mutant, up to the first failure. A tool outside the
suite would rebuild or restart the suite for each mutant, and could not
name the tests.

## Every mutant is compiled in

The backend compiles every mutant of a library into one binary, each
behind a guard, so a mutation run needs one build. A mutant changes what
the program means only when armed, in a child of a mutation run or in an
`--arm` run, and any other run of an instrumented build executes the
original. Every rewrite must type-check without type information, since
one ill-typed mutant would break the whole build: a comparison is
mutated only in a condition, where it is a `bool`. The catalogue of
mutants is a literal in the binary and cannot go stale against the code.

## A mutant killed anywhere is killed

A mutation run sees only what its own executable's tests reach, and a
mutant one suite misses may be killed by another. Each suite reported
alone would produce false survivors, sending a reader to write a test
that exists. `windtrap mutants` merges the verdict files, a kill
anywhere winning, and a build gates on its exit code; a single mutation
run exits 0 whatever it finds. A run whose selection narrows the suite
saves no verdict, since its partial answer would stand for the whole
suite in the merge.

## A known bug tests no mutant

A test marked `xfail` asserts what the code does not do yet, so its
failure is normal and tells nothing of a mutant. Named in a survivor's
block, it would send a reader to strengthen a test meant to fail. A
mutant that makes it pass reads as a fix of the bug. Rewriting `&&` to
`||` in a slug's letter test keeps every byte, which breaks the
separators and passes a test of accented letters. An `xfail` test
therefore reaches no mutant, and a line that only such tests run is
never reached.

## Equivalent mutants are dismissed in the source

Some mutants cannot be killed: `n >= 0` and `n > 0` agree wherever both
branches return the same value. They are dismissed with `[@mutate off
"reason"]` on the expression. The dismissal lives with the code, moves
when the code moves, and `git blame` says who decided it and when. There
is no suppression file to drift from the source, and windtrap never
writes the attribute itself, since an automatic dismissal would hide
defects as readily as equivalent mutants.

## Nothing depends on dune

Windtrap runs in any build. A suite is an executable, `-u` accepts
corrections, and an executable outside a build directory writes its data
under `_windtrap/` in its working directory. Under dune, the usual
spellings are a `(test)` stanza, `dune exec` for flags, `dune promote`
for corrections and `--instrument-with` for the backends. A remedy the
reporting commands print says what to do in words, and spells a dune
command only when the run, or the data it reads, came from dune.

## Two packages

`windtrap` holds the library and the `windtrap` command. `ppx_windtrap`
holds the rewriters, which depend on ppxlib, so a suite that uses no
rewriter links no ppxlib. Instrumented code links `windtrap.runtime`,
which depends on the standard library alone, so instrumenting a library
adds no test framework to what it links.
