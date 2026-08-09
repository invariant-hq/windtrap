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
deterministically named per executable and overwritten on re-run.

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
```

Seven green with subtraction turned into addition. That pair of lines is
the entire argument for mutation testing, made on your own suite in a
few seconds. Open `test/test_calc.ml:15`; it says

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

The alias is the `@cover` recipe ([Coverage](coverage.md)) with one word
changed. Add one rule, once, at the project root:

```lisp
(rule
 (alias mutants)
 (deps
  (alias_rec runtest)
  (universe))
 (action
  (run %{bin:windtrap} mutate)))
```

and in CI:

```yaml
- run: WINDTRAP_MUTATE=1 dune build @mutate --force --instrument-with ppx_windtrap.mutate
```

`(universe)` is load-bearing for the same reason it is for coverage: the
verdict files are not declarable dependencies, so it makes the
milliseconds-cheap merge re-run on every build. `--force` is required and
is not a wart — a mutation run is not a cached artifact, and dune would
otherwise treat a `runtest` action whose declared inputs have not changed
as already done.

**A survivor never fails a build in this release.** A mutation run exits
0 whatever it finds, and 1 only when it could not produce a number at
all: a red or empty dry run, a suite that disagrees with itself between
runs, instrumentation that is not actually armed, a deadline it overran,
or a supervision error, each with its own message. It never exits 2. A
gate over an uncalibrated number is how a tool earns a reputation for
lying, and the equivalent-mutant rate here is a prediction until it is
measured.

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
+ 5 s)` — and overrunning it aborts the run, naming the mutant it was
on. What catches a mutant that spins without consuming wall clock is a
separate per-site budget on how often the armed line may be evaluated,
set from the hit count the dry run measured there: a child that blows it
dies, and its mutant is scored *killed*, as is a child that crashes.
Mutation needs `Unix.fork`, so it declines by name on Windows.

## Knobs

Three environment variables, and no flag on any runner: the inline
runner's argument parser accepts only dune's inline-test protocol, so a
flag would exist for half the users. An unrecognized value is an error
naming the variable, never a silently defaulted mode.

| variable | values | default |
| --- | --- | --- |
| `WINDTRAP_MUTATE` | `1` / `report` / `off` | `off` |
| `WINDTRAP_MUTATE_ARM` | a mutant identifier | unset |
| `WINDTRAP_MUTATE_LIMIT` | survivor blocks to print, `0` for all | `10` |

All three are read by the test executable and by nothing else. Survivor
blocks are ordered by reaching-test count descending; a run's own report
caps them at `WINDTRAP_MUTATE_LIMIT` and prints the cap in the rule
label (`survivors (10 of 37)`) so nobody thinks they saw everything,
while `windtrap mutate` caps nothing — a project report a reader cannot
page past would send them back to the per-executable one. The unreached
list is never capped either. `report` mode runs the same loop and
prints the same report today — the dismissed, not-armable and timeout
tables it will add are not in this release — and `WINDTRAP_MUTATE_JOBS`
and `WINDTRAP_MUTATE_TIMEOUT` are specified but deliberately not read,
because a knob that is read and ignored is worse than one that is not.
Asking for the loop and an armed mutant at once is a refusal, not a
guess: the loop arms each mutant itself, so an armed parent would mutate
its own dry run.

windtrap's mutation testing is deliberately the 90% product: one honest
count after a run you already make, and the names of the tests that let
the change through. The other OCaml mutation tester is
[mutaml](https://github.com/jmid/mutaml), which works outside windtrap
and mutates a different set of expressions.
