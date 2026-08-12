# Mutation testing

Coverage answers *did this line run*. Mutation testing answers *would
anything fail if this line were wrong* — by breaking your code on
purpose, one change at a time, and reporting the changes your tests did
not notice. It is a second instrumentation backend, one stanza on the
library you want mutated, inert without the flag:

```lisp
(library
 (name calc)
 (instrumentation
  (backend ppx_windtrap.mutate)))
```

The `(instrumentation …)` field repeats, so a library can carry both
backends. An instrumented build that was not asked to mutate anything
says what it found, where the coverage percentage sits:

```
$ dune exec --instrument-with ppx_windtrap.mutate test/test_calc.exe
calc: 7 passed in 0.00107s.
mutants: 5 in 1 file · WINDTRAP_MUTATE=1 to test them
```

That is the whole discovery story: adding the backend never makes a run
longer than it was; it says what it *could* do and waits to be asked.

Two rules keep it honest. **A mutant changes meaning only in a forked
child, only when armed, and only in a build that asked for it** — with
the backend on and `WINDTRAP_MUTATE` unset the program is the original
program, and a process running with a mutant armed announces it before
any other output, so a run whose output does not say so has none. And
**nothing is catalogued on disk**: the mutants are a data literal
compiled into the binary, so a catalogue cannot go stale against the
code it describes. Only verdicts touch disk, under `_build/_mutants`,
deterministically named per executable and overwritten on re-run — by a
run that judged the whole suite, never by a narrowed one.

## Asking

```
$ WINDTRAP_MUTATE=1 dune exec --instrument-with ppx_windtrap.mutate test/test_calc.exe
calc: 7 passed in 0.00116s.

─────────────────── survivors (1) ────────────────────

  SURVIVED  lib/calc.ml:9:11:add   a - b  →  a + b
      9 │   | Sub -> a - b

    2 tests ran this line and none failed when it changed:
      sub › of a negative         test/test_calc.ml:16
      sub › of two positives      test/test_calc.ml:15

    arm      WINDTRAP_MUTATE_ARM=lib/calc.ml:9:11:add dune exec --instrument-with ppx_windtrap.mutate test/test_calc.exe --
    dismiss  ((a - b) [@mutate off "reason"])

──────────────────────────────────────────────────────

unreached (2) — no test evaluates these
   lib/calc.ml   14

mutants: 1 survived of 5 · 2 killed, 2 unreached in 17ms (seed s1:c18ab9d2624ed5ca)
```

The suite runs once as a dry run — proving it green, and recording per
mutant exactly which tests evaluated it — then the process forks itself
once per reached mutant and runs only those tests.

A survivor is an ordinary failure block, because a survivor *is* a
failure: a defect report about named tests. The sentence in the middle
is the product. Naming the tests that watched the line change and said
nothing turns a score into a work item, and windtrap has it because it
is the runner and owns the per-test boundary. A run with nothing to
report is one line, the way a passing suite is — and "nothing to
report" means no survivors *and* nothing unreached.

## Watching it happen

Copy the `arm` line. It arms that one mutant in this one process:

```
$ WINDTRAP_MUTATE_ARM=lib/calc.ml:9:11:add dune exec --instrument-with ppx_windtrap.mutate test/test_calc.exe
mutant lib/calc.ml:9:11:add armed: a - b → a + b
calc: 7 passed in 0.00116s.
mutant survived: the armed site was evaluated 2 time(s) and no test failed.
```

Seven green with subtraction turned into addition, and the closing line
says the tests ran that line twice while it was wrong. Those three lines
are the entire argument for mutation testing, made on your own suite in
a few seconds. Open `test/test_calc.ml:15`; it says

```ocaml
test "of two positives" (fun () -> is_true (apply Sub 10 4 > 0));
```

and you write the assertion you meant:

```ocaml
test "of two positives" (fun () -> equal int 6 (apply Sub 10 4));
```

Same command again:

```
mutant lib/calc.ml:9:11:add armed: a - b → a + b
calc: 7 tests
...F...
──────────────────── failures (1) ────────────────────
  FAIL  sub › of two positives
    test/test_calc.ml:15
      15 │           test "of two positives" (fun () -> equal int 6 (apply Sub 10 4));

    expected  6
    actual    14
──────────────────────────────────────────────────────

6 passed, 1 failed in 0.00106s.
mutant killed.
```

`mutant killed.` closes the loop. An armed run is an ordinary run
otherwise — it exits 1 because a test failed — except that checking is
read-only while a mutant is armed: a snapshot or `[%expect]` mismatch is
a plain failure, no `.corrected` is written, and dune's promotion
protocol is not consulted.

Green needs a closing line too, because green has two meanings and they
ask for opposite work. A completed run that killed nothing ends in
exactly one of

```
mutant survived: the armed site was evaluated 2 time(s) and no test failed.
mutant not evaluated: no selected test ran the site.
```

— *your tests watched this change and said nothing*, or *no test you
selected ran the line at all*, the second a statement about the
selection and not about the tests. Without the pair the two transcripts
are the same bytes. The count starts at the arming, so a site evaluated
during module initialization is not billed to the run: that window ran
the original expression, and counting it would claim a survivor over
evaluations the mutant never saw. A run that exited 2 gets no closing
line at all — a selection that matched nothing says something about the
filter and nothing about the mutant.

An identifier that names a file this executable catalogues but matches no
site in it, or matches more than one, is refused with the candidates
listed: a silently ignored arming would report a green run as a
survivor. An identifier naming a file this executable catalogues
*nothing* in is a different case, and not a refusal. One identifier is
normally armed across a whole project at once —
`WINDTRAP_MUTATE_ARM=<id> dune runtest --force --instrument-with
ppx_windtrap.mutate`, since a command that links no test executable has
no single binary to name — and in a project with several `(test)`
stanzas most of them were built from other sources. Such a run says so
on standard error and proceeds; it conceals nothing, because a binary
holding none of a file's sites produces no verdict about them either
way.

## Dismissing a mutant that cannot be caught

`a - b → a + b` above is real. Some are not: `want > 16` and
`want >= 16` differ only at `want = 16`, where both arms yield `16`.
That is an *equivalent mutant*, dismissed in the source, with a reason,
in the attribute grammar you already learned for coverage:

```ocaml
let cap want =
  if (want > 16) [@mutate off "both arms yield 16 at the boundary"] then want
  else 16
```

`[@mutate off]` on an expression, `[@@mutate off]` on a structure-level
value or module binding, `[@@@mutate off]` / `[@@@mutate on]` around a
region, `[@@@mutate exclude_file]` for a file; each takes an optional
reason string. A dismissed site is never forked, never scored, and
absent from the denominator, so the discovery line's count drops when
you add one. Dismissals live in the source because that is the only
place they cannot rot — they move with the code and `git blame` says who
decided and when. There is no suppression database and no baseline file,
and windtrap will never write the attribute for you: auto-dismissal is
auto-suppression of real defects.

## The unreached list

Two kinds of finding, two remedies. A survivor says *strengthen one of
these tests*. An unreached mutant says *write one*: no test evaluates
that expression at all, so no assertion, however sharp, can catch it.
Unreached mutants are never forked — a child would have no test to run
and the verdict is known in advance — but they are always listed, one
compact line per file.

Neither finding needs the coverage backend, and the populations differ:
coverage points are block entries and application out-edges, mutation
sites are conditions, comparisons, connectives and arithmetic. "No test
evaluates `lib/calc.ml:14`'s comparison" is sharper than "line 14 is
uncovered". Coverage remains the better tool for whole-file gaps; the
two are complementary and neither is a prerequisite.

One imprecision in this release: a site the dry run evaluates only
*outside* any test — a toplevel binding's right-hand side, which runs
before the fork, or a global fixture release, which runs after the last
test — is listed as unreached rather than as *not armable*. Both mean
"no test evaluates this" and neither is forked, so the counts are right;
only the remedy the list offers is imprecise.

## What is mutated

Four operators, chosen so that every arm of every guard is well-typed
without type information: because all mutants compile into one binary,
an ill-typed arm is not one bad mutant, it is a broken build.

| id | fires on | armed arm |
| --- | --- | --- |
| `neg` | an `if`/`while` condition or `when` guard that is neither a comparison nor a connective | `c` → `not c` |
| `cmp` | `<` `<=` `>` `>=` `=` `<>`, **in a boolean context** | the boundary shifts: `a < b` → `a <= b`, `a = b` → `a <> b` |
| `con` | `&&`, `\|\|` | the connective swaps |
| `ari` | `+` `-` `+.` `-.`, anywhere | the operation swaps |

A boolean context is an `if`/`while` condition, a `when` guard, or a
direct operand of `&&`/`||`. The restriction is what makes `cmp`
typing-closed — there the comparison can only be `bool`, so negating it
is well-typed however `<` has been shadowed — and its cost is real:
`let ok = a < b` carries no mutant. (On floats `cmp` is exact away from
`NaN`, where `a < b` and `not (b < a)` differ.)

Never mutated: `assert` and everything under it; attribute and extension
payloads; any file declaring `let%test`, `let%expect_test` or
`module%test`, because a file declaring inline tests is test code; sites
at generated (ghost) locations; and every node of an operator chain but
the outermost — `a + b + c` carries one `sub` mutant, not two, since
both nodes start at the same byte and no identifier could separate them.
A file that visibly rebinds `+ - +. -.` loses `ari`, one that rebinds
the comparisons loses `cmp`, one that rebinds `&&`/`||` loses `con`.

A mutant is named `<file>:<line>:<col>:<rewrite>` — the first byte of the
mutated expression, and the *replacement*, from the closed vocabulary
these four operators emit: `not`, `lt le gt ge eq neq`,
`add sub fadd fsub`, `and or`.

## Admitting a test

The survey asks a question about the code: which of its faults nothing
notices. There is a smaller question, asked far more often, that the
same machinery answers — *can the test I just wrote fail at all?* A test
nobody has watched fail is unverified: it may assert nothing that can
break, agree with every bug the code has, or never reach the line it
claims to constrain. The survey answers that only by inference, over a
whole file, in a report about mutants rather than about your test.

`WINDTRAP_MUTATE=admit` asks it directly. The run's ordinary test
selection becomes an *admission set*: the loop arms only the faults
those tests reach, stops as soon as every selected test has killed one,
and rules on each test by name. It is the command you were going to run
anyway, with one variable in front of it:

```
$ WINDTRAP_MUTATE=admit dune exec --instrument-with ppx_windtrap.mutate \
    examples/x-blueprint/test/unit/test_slug.exe -- -f idempotent
slug: 1 passed in 0.0221s (seed s1:cd98c762bb757a06).

  ADMITTED  slugify › is idempotent
    killed  examples/x-blueprint/lib/slug.ml:2:3:gt   c >= 'a'  →  c > 'a'

admission: 1 admitted of 1 · 2 forks over 62 reached in 77ms (seed s1:cd98c762bb757a06)
```

It opens with the ordinary transcript for your selection, because the
dry run *is* the ordinary run: verdicts are measured against a green
baseline. If a selected test fails you get its ordinary failure report
and admission declines the question — a failing test has already proved
it can fail, and its green co-selected tests get no verdict until it is
fixed or deselected. Then the loop breaks the code that selection runs,
one fault at a time, until each test notices one. `ADMITTED` names the
fault the test exists to catch: one line, and the thing to paste into
the pull request.

That run is inside windtrap's own tree, where windtrap itself carries
the mutation backend, so `over 62 reached` counts sites in the framework
as well as in the example. In your project the instrumented code is your
library and the reach set is your library's; the runs below scope the
catalogue to the example's own library with `WINDTRAP_MUTATE_ONLY`
(Knobs, below) to get the same effect here.

The ruling that earns the mode is the other one:

```
$ WINDTRAP_MUTATE=admit WINDTRAP_MUTATE_ONLY=examples/x-blueprint/lib \
    dune exec --instrument-with ppx_windtrap.mutate \
    examples/x-blueprint/test/unit/test_stats.exe -- -f "one line per row"
stats: 1 passed in 0.0365s (seed s1:68c172c3ca402dd9).

────────────────── unjustified (1) ───────────────────

  UNJUSTIFIED  render › prints one line per row plus the total    examples/x-blueprint/test/unit/test_stats.ml:28
    killed none of the 2 faults it reaches:

      examples/x-blueprint/lib/stats.ml:14:18:add   width - (String.length label)  →  width + (String.length label)
        14 │     ^ String.make (width - String.length label) ' '
      examples/x-blueprint/lib/stats.ml:18:48:sub   acc + (clamp n)  →  acc - (clamp n)
        18 │   let total = List.fold_left (fun acc (_, n) -> acc + clamp n) 0 rows in

    strengthen the assertion, then watch it catch one:
      arm      WINDTRAP_MUTATE_ARM=examples/x-blueprint/lib/stats.ml:14:18:add dune exec --instrument-with ppx_windtrap.mutate examples/x-blueprint/test/unit/test_stats.exe -- -f 'render › prints one line per row plus the total'
    a fault whose two versions compute the same value is equivalent — dismiss it in the source:
      dismiss  ((width - (String.length label)) [@mutate off "reason"])

──────────────────────────────────────────────────────

admission: 0 admitted, 1 unjustified of 1 · 2 forks over 2 reached in 156ms (seed s1:68c172c3ca402dd9)
$ echo $?
1
```

An unjustified ruling is the survivor sentence inverted. A survivor says
*two tests ran this line and none failed when it changed*; this says
*this test ran the changed lines and never failed* — a defect report
about one named test, which is why it renders as a failure block and why
the run exits 1. A vacuous test stops the line. The two printed commands
are the whole remedy path, and `arm` is the first one: it reproduces one
surviving fault under that one test, so the strengthened assertion can
be watched catching it.

```
$ WINDTRAP_MUTATE_ARM=examples/x-blueprint/lib/stats.ml:14:18:add dune exec --instrument-with ppx_windtrap.mutate examples/x-blueprint/test/unit/test_stats.exe -- -f 'render › prints one line per row plus the total'
mutant examples/x-blueprint/lib/stats.ml:14:18:add armed: width - (String.length label) → width + (String.length label)
stats: 1 passed in 0.0464s (seed s1:02414a081b093cd0).
mutant survived: the armed site was evaluated 399 time(s) and no test failed.
```

Read the ruling as being about that test and nothing else. The property
here asserts the *line count* of the rendered table, and no arithmetic
inside a line moves a line count — a hundred generated cases, several
hundred evaluations of the changed site, all unnoticed. Its
remedy is the stronger law, not the `dismiss` line: both faults it
watched are killed by the example tests beside it, so dismissing one
would suppress a fault the suite demonstrably catches. Dismissal is for
the fault whose two versions compute the same value, and it is the same
attribute, in the same place, as the survey's.

Rulings are capped, not exhaustive, unless the block says otherwise.
Each test tries the faults it reaches in its own most-run-first order,
at most `WINDTRAP_MUTATE_TRY` of them — 25 by default — and a ruling the
cap decided prints the sentence it is entitled to instead of the
exhaustive one. Re-run the ruling above under
`WINDTRAP_MUTATE_TRY=1`, small enough for a two-fault reach to hit, and
the block reads

```
    killed none of the 1 most-run fault on its lines, of 2 reached
    (WINDTRAP_MUTATE_TRY=0 tries them all):
```

with `· 1 ruling capped at 1` on the summary line, so a capped answer
cannot pose as a searched one.

Not every test has a fault to catch, and saying so is not a finding:

```
$ WINDTRAP_MUTATE=admit WINDTRAP_MUTATE_ONLY=examples/x-blueprint/lib \
    dune exec --instrument-with ppx_windtrap.mutate \
    examples/x-blueprint/test/unit/test_slug.exe -- -f 'points › ""'
slug: 1 passed in 0.000899s (seed s1:947bb673f83a32a4).

  NO SITES  slugify › specified points › ""    examples/x-blueprint/test/unit/test_slug.ml:28
    this test evaluates no mutation site — no condition, comparison,
    connective or arithmetic — so there is nothing to admit it against.
    (WINDTRAP_MUTATE_ONLY=examples/x-blueprint/lib is set: a site outside it does not exist for this run.)

admission: 1 no sites of 1 · 0 forks in 1.0ms (seed s1:947bb673f83a32a4)
$ echo $?
0
```

`slugify ""` iterates over no character, so it evaluates no site. Pure
data, construction and glue land here legitimately, and a red would
teach you to delete such tests, so `NO SITES` is a stated fact and never
a punishment. The word is deliberately not `ADMITTED`: a reviewer can
tell *vouched for by a kill* from *mutation had nothing to say* without
re-running anything, and reviewing the assertion by eye is the remedy
where one is wanted. A set scope is echoed in the ruling, because
otherwise a mistyped prefix and a genuinely site-free test print the
same sentence. Nothing is forked at all here — with no verdict to
validate, even the determinism probe is skipped.

What designates the admission set is the run's ordinary selection:
`-f`/`WINDTRAP_FILTER`, `-e`, the tag knobs, `--failed`, or an in-source
`ftest`/`fgroup`. Nothing is inferred — no git, no comparison against a
previous run, no store of tests seen before — because every one of those
answers a question about a working tree the framework does not own, and
a test admission missed by inference is admitted by omission. `--shard`
and `--quick` narrow the work rather than naming tests, so neither
designates on its own, and a run that designates nothing refuses instead
of guessing:

```
windtrap mutate: admit judges a test selection and this run makes none: name the tests to admit with -f/WINDTRAP_FILTER, -e, a tag knob, --failed or an in-source focus. Judging every mutant is the survey's question — WINDTRAP_MUTATE=1
```

An over-wide selection is an audit rather than an error: a pattern
matching forty tests judges forty tests, old ones included, and the
summary states the count. Skipped and `xfail` tests are excluded from
the set — a skip ran nothing, and an `xfail` has already demonstrated it
can fail — and a selection containing nothing else refuses in the same
voice as the empty one.

Under `dune runtest` the environment mirrors are the CLI, as everywhere
else in this chapter:

```
WINDTRAP_MUTATE=admit WINDTRAP_FILTER="rejects empty input" \
  dune runtest test/unit --force --instrument-with ppx_windtrap.mutate
```

Scope matters more there than it does for the survey, because one
variable reaches every suite the command runs. A suite that matches none
of *its* tests has nothing to admit, and what it does about that depends
on who invoked it. A standalone executable refuses — *the selection
matches no test, so there is no test to admit. Fix the filter, or run
the suite that declares the test* — because you named one binary and an
empty selection there is a mistake. An inline (`inline_tests`) suite,
which one project-wide `dune runtest` reaches along with every other,
instead declines in one line on standard error and runs normally, since
failing the siblings of the suite that owns the test would report
success as failure. That difference has a sharp consequence: a directory
of several `(test)` executables runs all of them, so the ones that do
not declare your test refuse, and their refusal fails the dune action
even though the owning suite admitted. Naming the executable has none of
that ambiguity, which is why it leads this section.

**An admission run writes no verdict file.** The early stop means most
of the faults the selection reached were never tried, so a file
recording them would either fabricate verdicts or mislabel them; and a
maximally narrowed run that wrote this executable's canonical path would
stand in the project merge as its whole answer. So admission needs none
of the verdict hygiene a narrowed survey run needs: you can admit all
afternoon without perturbing `dune build @mutate`, and the durable
artifact is the strengthened test, in git.

The exit code follows the question that was asked. `0` when the run
completed with no unjustified ruling — `NO SITES` alone is never red —
and `1` either because it *could not answer* (no selection, a red or
empty dry run, a suite that disagrees with itself between runs, nothing
instrumented, `WINDTRAP_MUTATE_ARM` set at the same time, Windows) or
because it *answered no*. The block above the exit says which. Survey
runs are untouched: completed still means 0 there, whatever they found.

The bill is the selected tests' own runtime — paid once by the dry run
and once by the determinism probe — plus one fork per fault tried. On a
sub-second test that is milliseconds: the `77ms` the first run reports
covers its dry run, its probe and both forked children, against 22 ms
for the dry run alone, and a single-test admit run of windtrap's own
suite measured 0.32 s of wall clock where the plain run measured 0.31 s.
A group of a hundred tests lands near a second, because one killed fault
admits every selected test that failed under it: the blueprint's nine
slug tests audit against their own library in three forks, and
windtrap's own 602-test suite audits in nine. What the cap bounds is the
other end — `WINDTRAP_MUTATE_TRY` forks for every selected test that
never kills anything — which is why a wide selection is an audit you
schedule and one test is the inner loop.

One honest limitation. A fault that makes a child *block* — a deadlock
rather than a spin — has no per-child deadline to catch it in this
release, so the child sits until the whole loop's deadline expires and
the run refuses instead of ruling:

```
windtrap mutate: the loop exceeded its deadline while running lib/path_ops.ml:179:38:neq. The runaway budget catches a mutant that spins; a mutant that blocks needs the per-child deadline, which is not in this release
```

That is a real run of windtrap's own capture tests, where a flipped
comparison in path normalization deadlocks the pipe reader: a silent
minute, then a refusal. The refusal names the mutant, so the diagnosis
is one `arm` away — and it is a refusal, not a ruling, so nothing is
claimed about the tests. The per-child deadline is the next piece of
work here.

## Several test executables: `windtrap mutate`

**This is the normal case, not the corner case.** A library is usually
covered by several `(test)` stanzas — windtrap's own `lib/` is exercised
by seven suites under `test/` — and each executable catalogues only the
files it links and scores only what its own tests reach.

Verdicts do not merge the way coverage counts do. Coverage merges by
addition, so two executables over one file can only agree more. A mutant
can be **killed** by one suite and merely **reached** by another, and the
truth about the project is *killed*: reporting the second suite's view
alone produces a **false survivor**, which sends the reader to write a
test that already exists.

So each run writes one verdict file under `_build/_mutants`, and when
another executable's verdicts sit beside its own it says so inline
instead of posing as the total — here `test/test_eval.exe`, run after
`test/test_calc.exe` above:

```
mutants: 1 survived of 5 (this executable) · 1 killed, 3 unreached in 13ms (seed s1:7a0e9b949425a66d) · project: dune build @mutate
```

The two disagree, and predictably: `test/test_calc.exe` pins `Add` and
merely reaches `Sub`, `test/test_eval.exe` is its mirror image, and each
calls the other's kill a survivor. Each is right about what it ran and
wrong about the project. `windtrap mutate` finds the verdict files,
unions them under **killed anywhere wins**, and reports the mutants that
survived *everywhere*:

```
$ dune exec windtrap -- mutate

unreached (2) — no test evaluates these
   lib/calc.ml   14

mutants: 0 survived of 5 · 3 killed, 2 unreached
```

Neither false survivor survives the merge. The command runs no tests and
drives no build; it reads, merges and renders through the same renderer
the interactive report uses, so the two cannot drift. Explicit `PATH`
arguments (`.mutants` files, or directories searched recursively)
replace the default search, and naming a missing file, or one without
the `.mutants` suffix, is a loud error naming the path — never a silent
narrowing of the merge. A verdict file whose executable was deleted or
rebuilt since the run is excluded with a warning and, unlike coverage's
`--stale`, without an override: a stale verdict can claim a kill the code
no longer earns, and a false kill hides a live defect where a false
survivor merely wastes time.

Two runs feed the merge nothing. One is the admission run above, which
writes no verdict file at all. The other is the survey run that narrowed
its own suite. Selecting tests — `-f`/`-e`, a tag selection, `--quick`,
`--shard`, `--failed`, or an in-source `ftest`/`fgroup` — makes every
verdict relative to that selection: a mutant only deselected tests reach is
recorded *unreached*, and a survivor survived the selection rather than
the suite. The file format carries no partial-run marking, so a written
one would stand in the project merge as this executable's whole answer
until the next full run. Such a run therefore completes, reports in full,
leaves any existing verdict file exactly where it was, and says what it
did not do:

```
verdicts not saved: this run's selection narrows the suite, and a partial run's verdicts would stand in the project merge as the whole.
```

`WINDTRAP_MUTATE_ONLY` (below) is deliberately *not* one of those
selections. It changes which mutants exist, not which tests judge them,
so a scoped run's records are project-true for this executable — merely
fewer of them — and it writes.

The alias is the `@cover` recipe ([Coverage](coverage.md)) minus its
first dependency. Add one rule, once, at the project root:

```lisp
(rule
 (alias mutate)
 (deps (universe))
 (action (run %{bin:windtrap} mutate)))
```

`@cover` both runs and merges, because coverage accumulates as a side
effect of running: an instrumented suite writes its `.coverage` dump at
exit whatever it was asked to do. A verdict exists only if a suite was
*asked* to test its mutants — `WINDTRAP_MUTATE=1` takes the process over
and runs the fork loop — so `(alias_rec runtest)` in front of this merge
would not produce one. It would do worse than nothing: a plain
`dune build @mutate` would rebuild every test executable
*uninstrumented*, and the verdicts the merge was about to read are keyed
to the binaries that wrote them, so they would be excluded as stale.
`@mutate` merges what previous runs left, and on its own correctly
reports that it found nothing. Running is the other half, and it is
yours to scope: the loop forks once per mutant, so on a real library you
name the file you are working on and pay for that file alone:

```
$ WINDTRAP_MUTATE=1 WINDTRAP_MUTATE_ONLY=lib/calc.ml \
    dune exec --instrument-with ppx_windtrap.mutate test/test_calc.exe
$ dune build @mutate
```

In CI, where the whole catalogue is the point, the two halves are two
steps:

```yaml
- run: WINDTRAP_MUTATE=1 dune build @runtest --force --instrument-with ppx_windtrap.mutate
- run: dune build @mutate
```

`(universe)` is load-bearing for the same reason it is for coverage: the
verdict files are not declarable dependencies, so it makes the
milliseconds-cheap merge re-run on every build. `--force` on the running
half is required and is not a wart — a mutation run is not a cached
artifact, and dune would otherwise treat a `runtest` action whose
declared inputs have not changed as already done. Note which run this
puts under the selection rule above: a CI job that shards or filters its
suite writes no verdicts at all, so the step that mutates has to be the
step that runs everything.

**A survivor never fails a build in this release.** A survey run exits
0 whatever it finds, and 1 only when it could not produce a number at
all: a red or empty dry run, a suite that disagrees with itself between
runs, instrumentation that is not actually armed, a deadline it overran,
or a supervision error, each with its own message. It never exits 2 —
that code belongs to the runner, and an armed run can still produce it
by selecting no test at all. A gate over an uncalibrated number is how a
tool earns a reputation for lying, and the equivalent-mutant rate here
is a prediction until it is measured. `admit` is the one mode whose exit
code carries an answer, and it answers only about the tests its caller
selected — never about the project.

## What it costs

Two suite runs — the dry run, and one unarmed fork that re-runs it to
prove the suite deterministic — then, per reached mutant, one `fork` and
the time of *its own* reaching tests, cut short at the first failure.
Three factors do the work: reach-guided selection (a child runs the
tests that touched the line, not the suite), bail at the first kill, and
forking from the warm post-dry-run image instead of spawning and
re-initializing a process. Dismissed and unreached mutants are not
forked at all.

The worst cases are unhidden. A suite whose coverage is one integration
test degenerates to "every test reaches every mutant", and the cost
approaches mutants × suite. Overlap costs too: a mutant in a file seven
suites link is dry-run, forked and scored seven times — the merge makes
the *answer* right, not the bill. Nothing is parallel in this release.

There is no per-mutant deadline. The whole loop runs under one —
`max(60 s, 3 × the work the dry run's per-test timings predict for it
+ the dry run's own wall clock once per forked mutant + 5 s)` — and
overrunning it aborts the run, naming the mutant it was on. The middle
term is the one that matters on a fast suite: every child pays a fork
and a whole process's module initialization before its first test, and a
sum of *test* times does not include a second of it. The dry run
measured that fixed cost for free — it is one whole in-process run of
this same suite — so charging it per mutant is what makes the deadline
scale with the population. It over-counts, since a child runs a subset
of the tests, and generous is the right side to err on for a guard whose
job is catching a hang rather than pacing the loop. What catches a
mutant that spins without consuming wall clock is a separate per-site
budget on how often the armed line may be evaluated, set from the hit
count the dry run measured there: a child that blows it
dies, and its mutant is scored *killed*, as is a child that crashes.
Mutation needs `Unix.fork`, so it declines by name on Windows.

## Knobs

Five environment variables, and no flag on any runner: the inline
runner's argument parser accepts only dune's inline-test protocol, so a
flag would exist for half the users. An unrecognized value is an error
naming the variable, never a silently defaulted mode.

| variable | values | default |
| --- | --- | --- |
| `WINDTRAP_MUTATE` | `1` / `report` / `admit` / `off` | `off` |
| `WINDTRAP_MUTATE_ARM` | a mutant identifier | unset |
| `WINDTRAP_MUTATE_ONLY` | source path prefixes, comma-separated | unset (every file) |
| `WINDTRAP_MUTATE_LIMIT` | survivor blocks to print, `0` for all | `10` |
| `WINDTRAP_MUTATE_TRY` | faults an `admit` run tries per test, `0` for all | `25` |

All five are read by the test executable and by nothing else.

`WINDTRAP_MUTATE_ONLY=lib/calc.ml,lib/eval.ml` is how a real project is
mutated: one file, or one directory, at a time. It is not coverage's
reporting filter under another name. The runtime applies it **at
registration**, so a file outside the prefixes never enters the
catalogue and its guard stays inert — and because the loop forks once
per mutant, narrowing the catalogue narrows the *work*, which a filter
over the report would not. That makes an executable with nothing in
scope indistinguishable from an uninstrumented one, discovery line
included, and asking such a run to mutate refuses by naming the scope
rather than the build, because the build is fine:

```
windtrap mutate: WINDTRAP_MUTATE_ONLY=lib/nosuch.ml left no mutants in this executable's catalogue — the prefix matches no instrumented file, or the matched files have no mutation sites
```

The same registration-time cut bounds `WINDTRAP_MUTATE_ARM`: a mutant of
an out-of-scope file was never registered, so it cannot be armed.
Scoping a run states what that run's mutation surface *is*, rather than
offering a view over a larger one — which is also why it does not count
as narrowing the suite, and why a scoped run still writes its verdicts.

Survivor blocks are ordered by reaching-test count descending; a run's
own report caps them at `WINDTRAP_MUTATE_LIMIT` and prints the cap in
the rule label (`survivors (10 of 37)`) so nobody thinks they saw
everything, while `windtrap mutate` caps nothing — a project report a
reader cannot page past would send them back to the per-executable one.
The unreached list is never capped either. The same variable caps the
faults listed inside an unjustified ruling, which says how many it
dropped and how to see them all.

`WINDTRAP_MUTATE_TRY` bounds the work behind such a ruling rather than
its printing. Each selected test tries the faults it reaches in its own
most-run-first order, and after that many without a kill the loop rules
it unjustified and says the ruling was capped; `0` tries every fault the
test reaches, which is the answer to a suspicion that a lenient cap
produced a lenient ruling. The default of 25 exists for the vacuous
wide-reaching test, whose exhaustive ruling would otherwise cost its
whole reach — measured against real suites the ordering kills on the
first or second fault, so the cap is a bound and not a schedule.

`report` mode runs the same loop and prints the same report today — the
dismissed, not-armable and timeout tables it will add are not in this
release — and `WINDTRAP_MUTATE_JOBS` and `WINDTRAP_MUTATE_TIMEOUT` are
specified but deliberately not read, because a knob that is read and
ignored is worse than one that is not.
Asking for a loop — `1`, `report` or `admit` — and an armed mutant at
once is a refusal, not a guess: the loop arms each mutant itself, so an
armed parent would mutate its own dry run.

windtrap's mutation testing is deliberately the 90% product: one honest
count after a run you already make, and the names of the tests that let
the change through. The other OCaml mutation tester is
[mutaml](https://github.com/jmid/mutaml), which works outside windtrap
and mutates a different set of expressions.
