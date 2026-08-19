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
`WINDTRAP_MUTATE=off` is the answer to that line, for a workspace whose
every build is instrumented and does not want telling every time.

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
selection and not about the tests; without the pair the two transcripts
are the same bytes. The count starts at the arming, so a site evaluated
during module initialization is not billed to the run. A run that exited
2 gets no closing line at all — a selection that matched nothing says
something about the filter and nothing about the mutant.

An identifier that names a file this executable catalogues but matches
no site in it, or matches more than one, is refused with the candidates
listed: a silently ignored arming would report a green run as a
survivor. An identifier naming a file this executable catalogues
*nothing* in is not a refusal, because one identifier is normally armed
across a whole project at once — `WINDTRAP_MUTATE_ARM=<id> dune runtest
--force --instrument-with ppx_windtrap.mutate` — where most binaries
were built from other sources. Such a run says so on standard error and
proceeds; a binary holding none of a file's sites produces no verdict
about them either way.

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

admission: 1 admitted of 1 · 2 forks over 61 reached in 77ms (seed s1:cd98c762bb757a06)
```

It opens with the ordinary transcript for your selection, because the
dry run *is* the ordinary run: verdicts are measured against a green
baseline, and a selected test that fails gets its ordinary failure
report while admission declines the question — a failing test has
already proved it can fail. Then the loop breaks the code that selection
runs, one fault at a time, until each test notices one. `ADMITTED` names
the fault the test exists to catch: one line, and the thing to paste
into the pull request. (That run is inside windtrap's own tree, where
the framework carries the backend too, so `over 61 reached` counts its
sites as well as the example's; the run below scopes the catalogue with
`WINDTRAP_MUTATE_ONLY` to get your project's effect here.)

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
are the whole remedy path, and `arm` is the first one: paste it and the
fault is reproduced under that one test, the round trip of "Watching it
happen" narrowed to the test you are fixing.

Read the ruling as being about that test and nothing else. The property
here asserts the *line count* of the rendered table, and no arithmetic
inside a line moves a line count — a hundred generated cases, several
hundred evaluations of the changed site, all unnoticed. Its remedy is
the stronger law, not the `dismiss` line: both faults it watched are
killed by the example tests beside it, so dismissing one would suppress
a fault the suite demonstrably catches. Dismissal is for the fault whose
two versions compute the same value, and it is the same attribute, in
the same place, as the survey's.

Rulings are capped, not exhaustive, unless the block says otherwise.
Each test tries the faults it reaches in its own most-run-first order,
at most `WINDTRAP_MUTATE_TRY` of them — 25 by default — and a capped
ruling says so, both in its sentence (`killed none of the N most-run
faults on its lines, of M reached`) and as `· 1 ruling capped at N` on
the summary line, so a capped answer cannot pose as a searched one.

The third ruling is `NO SITES`: a test that evaluates no mutation site —
no condition, comparison, connective or arithmetic — has nothing to
admit it against. Pure data, construction and glue land there
legitimately, so it states a fact and is never red; the word is
deliberately not `ADMITTED`, so a reviewer can tell *vouched for by a
kill* from *mutation had nothing to say* without re-running anything. A
set `WINDTRAP_MUTATE_ONLY` is echoed in the ruling, because otherwise a
mistyped prefix and a genuinely site-free test print the same sentence.

What designates the admission set is the run's ordinary selection:
`-f`/`WINDTRAP_FILTER`, `-e`, the tag knobs, `--failed`, or an in-source
`ftest`/`fgroup`. Nothing is inferred — no git, no diff against a
previous run, no store of tests seen before — because a test admission
missed by inference is admitted by omission. `--shard` narrows the work
rather than naming tests, so it does not designate on its own. An
over-wide selection judges every test it matches rather than erring, and
the summary states the count; skipped and `xfail` tests are excluded, a
skip having run nothing and an `xfail` having already demonstrated it
can fail.

A run that narrows nothing designates every test it executes. That is
the whole-suite question — the ask an alias makes, because an alias
cannot write a filter — and the run states it in one line before the
rulings, naming the other question in case that is the one you meant:

```
windtrap mutate: admitting all 9 tests this run executed; the per-mutant question is WINDTRAP_MUTATE=1
```

It is affordable because one killed fault admits every designated test
that failed under it: the blueprint's nine slug tests are ruled in three
forks, six falling to the same `or`, and windtrap's own suite — some six
hundred tests — whole, in nine forks. So the wide question is one rule
at the project root, with no filter to keep in sync:

```lisp
(rule
 (alias admit)
 (deps (universe) test/test_calc.exe)
 (action (setenv WINDTRAP_MUTATE admit (run %{exe:test/test_calc.exe}))))
```

Unlike `@mutate`, it has no merging half to pair with: an admission run
persists nothing, so the rulings it prints are the whole product. Name
the executables whose subject is the instrumented library; a suite that
observes its subject through a process it spawns has nothing to admit,
because the arming never reaches the child.

Under `dune runtest` the environment mirrors are the CLI, as everywhere
else in this chapter:

```
WINDTRAP_MUTATE=admit WINDTRAP_FILTER="rejects empty input" \
  dune runtest test/unit --force --instrument-with ppx_windtrap.mutate
```

Scope matters more there than it does for the survey, because one
variable reaches every suite the command runs. A named executable that
matches none of *its* tests refuses — you named one binary and an empty
selection there is a mistake — while an inline (`inline_tests`) suite,
reached by the same project-wide `dune runtest` as every other, declines
in one line on standard error and runs normally, since failing the
siblings of the suite that owns the test would report success as
failure. The sharp consequence: a directory of several `(test)`
executables runs all of them, so the ones that do not declare your test
refuse, and their refusal fails the dune action even though the owning
suite admitted. Naming the executable has none of that ambiguity, which
is why it leads this section.

**An admission run writes no verdict file.** The early stop leaves most
of the reached faults untried, so a file recording them would fabricate
or mislabel verdicts, and a narrowed run's file would stand in the
project merge as this executable's whole answer. So you can admit all
afternoon without perturbing `dune build @mutate`; the durable artifact
is the strengthened test, in git.

The exit code follows the question that was asked. `0` when the run
completed with no unjustified ruling — `NO SITES` alone is never red —
and `1` either because it *could not answer* (a red or empty dry run, a
suite that disagrees with itself between runs, nothing instrumented,
`WINDTRAP_MUTATE_ARM` set at the same time, Windows) or because it
*answered no*. The block above the exit says which. Survey runs are
untouched: completed still means 0 there, whatever they found.

A fault that makes a child *block* — a deadlock rather than a spin — is
ruled rather than fatal: the child's deadline expires (What it costs,
below), it is killed with its process group, and the kill is attributed
to the one test that had started and never reported, admitting it with
cause `killed (timeout)`. A hang under a fault is a detected fault,
noticed by never finishing.

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
rebuilt since the run is excluded with a warning, never merged: a stale
verdict can claim a kill the code no longer earns, and a false kill hides
a live defect where a false survivor merely wastes time.

Two runs feed the merge nothing. One is the admission run above, which
writes no verdict file at all. The other is the survey run that narrowed
its own suite. Selecting tests — `-f`/`-e`, a tag selection, `--shard`,
`--failed`, or an in-source `ftest`/`fgroup` — makes every verdict
relative to that selection: a mutant only deselected tests reach is
recorded *unreached*, and a survivor survived the selection rather than
the suite. The file format carries no partial-run marking, so a written
one would stand in the project merge as this executable's whole answer
until the next full run. Such a run therefore completes, reports in
full, leaves any existing verdict file exactly where it was, and says
what it did not do:

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

`@cover` both runs and merges; `@mutate` only merges. A coverage dump is
written by any instrumented run, but a verdict exists only if a suite was
*asked* to test its mutants, so `(alias_rec runtest)` in front of this
merge would not produce one — and would do worse than nothing, rebuilding
every test executable *uninstrumented* and thereby staling the very
verdicts the merge was about to read. `(universe)` is load-bearing for
coverage's reason: verdict files are not declarable dependencies, so it
makes the milliseconds-cheap merge re-run on every build.

Running is the other half, and it is yours to scope: the loop forks once
per mutant, so on a real library you name the file you are working on
and pay for that file alone. In CI, where the whole catalogue is the
point, the two halves are two steps:

```
$ WINDTRAP_MUTATE=1 WINDTRAP_MUTATE_ONLY=lib/calc.ml \
    dune exec --instrument-with ppx_windtrap.mutate test/test_calc.exe
$ dune build @mutate
```

```yaml
- run: WINDTRAP_MUTATE=1 dune build @runtest --force --instrument-with ppx_windtrap.mutate
- run: dune build @mutate
```

`--force` on the running half is required and is not a wart — a mutation
run is not a cached artifact, and dune would otherwise treat a `runtest`
action whose declared inputs have not changed as already done. Note
which run this puts under the selection rule above: a CI job that shards
or filters its suite writes no verdicts at all, so the step that mutates
has to be the step that runs everything.

**A survivor never fails a build in this release.** A survey run exits
0 whatever it finds, and 1 only when it could not produce a number at
all: a red or empty dry run, a suite that disagrees with itself between
runs, instrumentation that is not actually armed, a deadline it overran,
or a supervision error, each with its own message. It never exits 2 —
that code belongs to the runner, and an armed run can still produce it
by selecting no test at all. A gate over an uncalibrated number is how a
tool earns a reputation for lying, and the equivalent-mutant rate here
is a prediction until it is measured. Admission is where an exit code
carries an answer, and never about the project.

## What it costs

Two suite runs — the dry run, and one unarmed fork that re-runs it to
prove the suite deterministic — then one `fork` per reached mutant,
running only *its own* reaching tests and stopping at the first failure.
Dismissed and unreached mutants are not forked at all, and nothing is
parallel in this release, so the bill scales with the catalogue: one
file at a time is the habit, and `WINDTRAP_MUTATE_ONLY` is how you spell
it.

Every forked child runs under a deadline derived from the dry run's own
timings — never a knob — and a child that overruns is killed with its
process group and its mutant scored killed, which is the right verdict:
a fault that makes the suite hang is a fault the suite noticed. Nothing
caps a whole run, so a run of a thousand mutants takes as long as its
thousand children do. Mutation needs `Unix.fork` and declines by name on
Windows.

The derivation, the worst cases and the measurements behind those
sentences are in
[`doc/dev/testing.md`](../dev/testing.md#what-a-run-costs-and-where-the-deadline-comes-from).

## Knobs

Four environment variables — the mode, and three that shape it — and no
flag on any runner: the inline runner's argument parser accepts only
dune's inline-test protocol, so a flag would exist for half the users.
An unrecognized value is an error naming the variable, never a silently
defaulted mode. All four are read by the test executable and by nothing
else.

| variable | values | default |
| --- | --- | --- |
| `WINDTRAP_MUTATE` | `1` / `admit` / `off` | unset (discovery only) |
| `WINDTRAP_MUTATE_ARM` | a mutant identifier | unset |
| `WINDTRAP_MUTATE_ONLY` | source path prefixes, comma-separated | unset (every file) |
| `WINDTRAP_MUTATE_TRY` | faults an `admit` run tries per test, `0` for all | `25` |

`WINDTRAP_MUTATE_ONLY=lib/calc.ml,lib/eval.ml` is how a real project is
mutated: one file, or one directory, at a time. The runtime applies it
**at registration**, so a file outside the prefixes never enters the
catalogue — narrowing the *work*, which a filter over the report would
not, and which is why it is not coverage's reporting filter under
another name. Scoping a run states what that run's mutation surface
*is*, so it does not count as narrowing the suite and a scoped run still
writes its verdicts; and it bounds `WINDTRAP_MUTATE_ARM`, since a mutant
of an out-of-scope file was never registered. An executable with nothing
in scope is indistinguishable from an uninstrumented one, discovery line
included, so asking such a run to mutate names the scope rather than the
build — the build is fine:

```
windtrap mutate: WINDTRAP_MUTATE_ONLY=lib/nosuch.ml left no mutants in this executable's catalogue — the prefix matches no instrumented file, or the matched files have no mutation sites
```

`WINDTRAP_MUTATE_TRY` bounds the work behind an unjustified ruling
rather than its printing: each selected test tries the faults it reaches
in its own most-run-first order, and after that many without a kill the
loop rules it unjustified and says the ruling was capped. `0` tries
every fault the test reaches, which is the answer to a suspicion that a
lenient cap produced a lenient ruling. Why 25, and what it buys, is in
[`doc/dev/testing.md`](../dev/testing.md#what-a-run-costs-and-where-the-deadline-comes-from).

Nothing else caps a report. Survivor blocks are ordered by reaching-test
count descending and are uncapped — a survivor is a failure block, and
windtrap caps no failure block — as are the unreached list and
`windtrap mutate`'s merged report. The remedy for a file with a hundred
survivors is `WINDTRAP_MUTATE_ONLY`, not paging. The faults listed
inside an unjustified ruling are the one exception, cut to the first few
with a `… n more` line under them.

Asking for a loop — `1` or `admit` — and an armed mutant at once is a
refusal, not a guess: the loop arms each mutant itself, so an armed
parent would mutate its own dry run.

windtrap's mutation testing is deliberately the 90% product: one honest
count after a run you already make, and the names of the tests that let
the change through. The other OCaml mutation tester is
[mutaml](https://github.com/jmid/mutaml), which works outside windtrap
and mutates a different set of expressions.
