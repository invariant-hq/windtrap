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
backends. Then: run your tests with `--mutate`; for every mutant in the
code those tests reach, windtrap re-runs them with the mutant armed; a
mutant none of them notice is reported, naming the tests that ran it.

Two rules keep it honest. **A mutant changes meaning only in a forked
child, only when armed, and only in a build that asked for it** — with
the backend on and neither `--mutate` nor `--arm` the program is the
original program, and a process running with a mutant armed announces
it before any other output. And **nothing is catalogued on disk**: the
mutants are a data literal compiled into the binary, so a catalogue
cannot go stale against the code it describes. Only verdicts touch
disk, under the build directory's `_mutants`, one file per executable,
overwritten on re-run.

The transcripts below are from [`examples/x-blueprint`](../../examples/x-blueprint),
run inside windtrap's own tree — which is why its paths carry that
prefix and every command carries `--instrument-with`. The flag can go:
declare the backend once in `dune-workspace` and it disappears from
every command in this chapter (the example ships that file):

```lisp
(lang dune 3.0)

(context
 (default
  (instrument_with ppx_windtrap.mutate)))
```

## Running it on a file

Name the file you are working on and the suite that tests it:

```
$ dune exec --instrument-with ppx_windtrap.mutate examples/x-blueprint/test/unit/test_slug.exe -- --mutate=examples/x-blueprint/lib/slug.ml
slug: 8 passed in 0.0171s (seed s1:4fb09fe9d4bf9267).

─────────────────── survivors (4) ────────────────────

  SURVIVED  examples/x-blueprint/lib/slug.ml:2:29:gt   c >= 'A'  →  c > 'A'
      2 │   (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

    7 tests ran this line and none failed:
      slugify › emits lowercase alphanumerics and single inner dashes      examples/x-blueprint/test/unit/test_slug.ml:17
      slugify › is idempotent                                              examples/x-blueprint/test/unit/test_slug.ml:15
      slugify › specified points › "  OCaml 5.x  "                         examples/x-blueprint/test/unit/test_slug.ml:28
      …

  …

──────────────────────────────────────────────────────

mutants: 4 survived of 16 reached by this suite · 12 killed
reproduce: dune exec --instrument-with ppx_windtrap.mutate examples/x-blueprint/test/unit/test_slug.exe -- --arm <id>
```

The suite runs once as a dry run — proving it green, and recording per
mutant exactly which tests evaluated it — then the process forks itself
once per reached mutant and runs only those tests. `--mutate` alone
surveys every mutant the executable catalogues; `--mutate=PREFIX,…`
keeps to the files whose recorded path starts with a prefix, which is
how a real project is mutated — one file, or one directory, at a time.
The loop forks once per mutant, so the prefixes narrow the *work*,
which a filter over the report would not.

A survivor is an ordinary failure block, because a survivor *is* a
failure: a defect report about named tests. The header is the rewrite —
`c >= 'A'` became `c > 'A'` — and the sentence in the middle is the
product: naming the tests that watched the line change and said nothing
turns a score into a work item, and windtrap has it because it is the
runner and owns the per-test boundary. Blocks are ordered by witness
count, most-watched first, and never capped. A run with nothing to
report is one line, the way a passing suite is:

```
slug: 9 passed in 0.0457s (seed s1:c0e74ad8abd96b7d).
mutants: 16 reached by this suite · 16 killed
```

The report is about *this executable's* tests, and the run exits 0
whatever it finds: one suite's survivor may be another suite's kill,
and the project's answer is the aggregate below. Test selection is the
ordinary `-f` and tag flags — `-- -f idempotent` mutates only what the
idempotence law reaches, the summary says `of 16 reached by the 1
selected test`, and the footer carries the filter.

## Reading a survivor

All four survivors above are boundaries of a character class, and the
witnesses say why: two laws that cannot see one character's fate, and
specified points none of which sits on an edge. The remedy is a test
that does — one input touching every edge at once, added to the
`cases` beside the others:

```ocaml
cases "specified points"
  ~name:(fun (input, _) -> Printf.sprintf "%S" input)
  [ ("Hello, World!", "hello-world"); ("MiXeD", "mixed"); ("Az Za 09", "az-za-09") ]
  (fun (input, expected) -> equal string expected (Slug.slugify input));
```

Same command again: `mutants: 16 reached by this suite · 16 killed`.

Some survivors cannot be caught. `want > 16` and `want >= 16` differ
only at `want = 16`, where both arms yield `16`. That is an *equivalent
mutant*, dismissed in the source, with a reason, in the attribute
grammar you already learned for coverage:

```ocaml
let cap want =
  if (want > 16) [@mutate off "both arms yield 16 at the boundary"] then want
  else 16
```

`[@mutate off]` on an expression, `[@@mutate off]` on a structure-level
value or module binding, `[@@@mutate off]` / `[@@@mutate on]` around a
region, `[@@@mutate exclude_file]` for a file; each takes an optional
reason string. A dismissed site is never forked, never scored, and
absent from the denominator. Dismissals live in the source because that
is the only place they cannot rot — they move with the code and
`git blame` says who decided and when. There is no suppression database
and no baseline file, and windtrap never writes the attribute for you:
auto-dismissal is auto-suppression of real defects.

## Reproducing one

The `reproduce:` footer is a command with a hole. Fill it with a
survivor's identifier and that one mutant is armed in this one process,
which otherwise runs normally:

```
$ dune exec --instrument-with ppx_windtrap.mutate examples/x-blueprint/test/unit/test_slug.exe -- --arm examples/x-blueprint/lib/slug.ml:2:41:lt
mutant examples/x-blueprint/lib/slug.ml:2:41:lt armed: c <= 'Z' → c < 'Z'
slug: 8 passed in 0.0161s (seed s1:bccabc5682ff4d3e).
mutant survived: the armed site was evaluated 54544 time(s) and no test failed.
```

Eight green with `Z` no longer a letter, and the closing line says the
tests ran that comparison tens of thousands of times while it was
wrong. Those three lines are the entire argument for mutation testing,
made on your own suite in a second. With the boundary row in place:

```
mutant examples/x-blueprint/lib/slug.ml:2:41:lt armed: c <= 'Z' → c < 'Z'
slug: 9 tests (seed s1:25cc6d0339147053)
........F
──────────────────── failures (1) ────────────────────
  FAIL  slugify › specified points › "Az Za 09"
    …
──────────────────────────────────────────────────────

8 passed, 1 failed in 0.0209s.
mutant killed.
```

`mutant killed.` closes the loop. An armed run is an ordinary run
otherwise — it exits 1 because a test failed — except that checking is
read-only while a mutant is armed: an `expect` or `[%expect]` mismatch is
a plain failure, no `.corrected` is written, and dune's promotion
protocol is not consulted.

Green needs a closing line too, because green has two meanings and they
ask for opposite work: `mutant survived` above — *your tests watched
this change and said nothing* — or `mutant not evaluated: no selected
test ran the site.`, a statement about the selection and not about the
tests. The count starts at the arming, so a site evaluated during
module initialization is not billed to the run.

An identifier that matches more than one site, or names a file this
executable catalogues but matches no site in it, is refused with the
candidates listed: a silently ignored arming would report a green run
as a survivor. One naming a file this executable catalogues *nothing*
in is noted on standard error and the run proceeds — the project
report's footer arms one identifier across every suite at once, where
most binaries were built from other sources. Asking for `--mutate` and
`--arm` at once is a usage error, not a guess: the loop arms each
mutant itself, so an armed parent would mutate its own dry run.

## The whole project

A library is usually covered by several `(test)` stanzas, and each
executable scores only what its own tests reach. Verdicts do not merge
the way coverage counts do: a mutant can be **killed** by one suite and
merely **reached** by another, and the truth about the project is
*killed*. Reporting the second suite alone produces a **false
survivor**, which sends the reader to write a test that already exists.
In the example, the expect suite alone reports seven survivors of
`slug.ml`; the unit suite kills every one of them.

So the project's answer is two commands: every suite run with its
mutants, then the merge:

```
$ WINDTRAP_MUTATE=1 dune runtest --force --instrument-with ppx_windtrap.mutate
$ dune exec windtrap -- mutants
```

`WINDTRAP_MUTATE` is `--mutate`'s environment mirror, the spelling that
reaches every stanza under `dune runtest`, where no command line does:
`1` is the bare flag, `0` its absence, and anything else its prefixes.
(Those commands, verbatim, are for your project. This chapter's
capture, made inside windtrap's tree, set
`WINDTRAP_MUTATE=examples/x-blueprint` instead — windtrap's own library
carries the backend here, and an unscoped run would survey the
framework's mutants too.)

Each suite prints its own report as it runs, then `windtrap mutants`
unions the verdict files under **killed anywhere wins** and reports the
mutants that survived *everywhere*, each witness beside the executable
that ran it. Here, with the boundary row removed again:

```
─────────────────── survivors (4) ────────────────────

  SURVIVED  examples/x-blueprint/lib/slug.ml:2:29:gt   c >= 'A'  →  c > 'A'
      2 │   (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

    9 tests in 3 executables ran this line and none failed:
      issue_1.exe                         keeps UTF-8 letters
      test_slug.exe                       slugify › emits lowercase alphanumerics and single inner dashes
      test_slug.exe                       slugify › is idempotent
      test_slug.exe                       slugify › specified points › "  OCaml 5.x  "
      …
      windtrap_example_blueprint_expect   Expect_slug › slugify, at a glance

  …

──────────────────────────────────────────────────────

mutants: 4 survived of 18 reached · 14 killed · 4 executables
reproduce: WINDTRAP_MUTATE_ARM=<id> <re-run the instrumented suite>
```

That run exits 1: every survivor in it is a test to strengthen or an
equivalent mutant to dismiss, which makes it the one mutation exit code
a build may gate on. With the row restored the project has nothing to
report and exits 0:

```
mutants: 18 reached · 18 killed · 4 executables
```

The footer's placeholder is where your suite command goes — the first
of the two commands above, with `--arm`'s mirror in place of
`WINDTRAP_MUTATE=1`: the merge never ran the suite and does not know
how you spell running it.

A mutant no suite in the project reaches is a second kind of finding
with a second remedy — *write a test*, where a survivor says
*strengthen one*. It is never forked, it is never red, and it renders
in its own section of the project report — only there, because one
executable sees only the files it links and cannot know what no test
reaches. Here the verdicts on disk came from the bug-backlog suite
alone, whose one input never reaches the dash insertion:

```
  …

───────────────── never reached (1) ──────────────────

  UNREACHED  examples/x-blueprint/lib/slug.ml:12:27:ge   (Buffer.length buf) > 0  →  (Buffer.length buf) >= 0
      12 │         if !pending_sep && Buffer.length buf > 0 then Buffer.add_char buf '-';

──────────────────────────────────────────────────────

mutants: 12 survived of 15 reached · 3 killed · 1 never reached · 1 executable
```

Three facts about the run. `--force` is required and is not a wart:
a mutation run is not a cached artifact, and dune would otherwise treat
a `runtest` action whose inputs have not changed as already done. A
rebuild without the variable and the flag produces uninstrumented
executables, which stales every verdict — a verdict is invalidated by
any later build of the executable that wrote it — and the merge then
excludes each stale file with one warning line and says, once, what
heals it: re-run every suite with its mutants, then merge again;
delete the `_mutants` directory to drop leftovers of removed
executables. And `--mutate`'s prefixes scope the work without narrowing
the suite, so a scoped run still writes its verdicts; selecting tests —
`-f`, tags, `--shard`, `--failed`, an in-source `focus` — does narrow
it, and such a run reports in full, leaves any existing verdict file
where it was, and says so:

```
verdicts not saved: this run's selection narrows the suite, and a partial run's verdicts would stand in the project merge as the whole.
```

`windtrap mutants` runs no tests and drives no build — the verb says
so; it reads the build directory's `_mutants` (under `dune exec`, the
directory dune names, a private `--build-dir` included), or the
`.mutants` files and directories named as arguments, and a missing
path is a loud error, never a silent narrowing of the merge.

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
typing-closed — there the comparison can only be `bool` — and its cost
is real: `let ok = a < b` carries no mutant. (On floats `cmp` is exact
away from `NaN`.)

Never mutated: `assert` and everything under it; attribute and extension
payloads; any file declaring `let%test`, `let%expect_test` or
`module%test`, because a file declaring inline tests is test code; sites
at generated (ghost) locations; and every node of an operator chain but
the outermost — `a + b + c` carries one `sub` mutant, not two. A file
that visibly rebinds `+ - +. -.` loses `ari`, one that rebinds the
comparisons loses `cmp`, one that rebinds `&&`/`||` loses `con`.

The `<rewrite>` in a mutant's name is the *replacement*, from the closed
vocabulary these four operators emit: `not`, `lt le gt ge eq neq`,
`add sub fadd fsub`, `and or`.

## What it costs

Two suite runs — the dry run, and one unarmed fork that re-runs it to
prove the suite deterministic — then one `fork` per reached mutant,
running only *its own* reaching tests and stopping at the first failure.
Dismissed and unreached mutants are not forked at all, and nothing is
parallel in this release, so the bill scales with the population: one
file at a time is the habit, and `--mutate=lib/calc.ml` is how you
spell it.

Every forked child runs under a deadline derived from the dry run's own
timings — never a knob — and a child that overruns is killed with its
process group and its mutant scored killed, which is the right verdict:
a fault that makes the suite hang is a fault the suite noticed. Nothing
caps a whole run, so a run of a thousand mutants takes as long as its
thousand children do. Mutation needs `Unix.fork` and declines by name on
Windows. The derivation and the measurements behind those sentences are
in
[`doc/dev/testing.md`](../dev/testing.md#what-a-run-costs-and-where-the-deadline-comes-from).

## Knobs

Two flags on the test executable, each with the environment mirror
every run-changing flag has ([Running tests](running-tests.md)), for
the runs no command line reaches — `dune runtest`, and an inline
suite's generated runner. Both are read by the runner, never by the
instrumented code, which reads no flag and no environment.

| flag | mirror | effect |
| --- | --- | --- |
| `--mutate[=PREFIX,…]` | `WINDTRAP_MUTATE` | run the survey: every mutant the executable catalogues, or only those whose recorded source path starts with one of the comma-separated prefixes |
| `--arm ID` | `WINDTRAP_MUTATE_ARM` | run once with mutant `ID` armed |

`WINDTRAP_MUTATE` reads both ways: `1` (and the other truthy
spellings) is the bare flag, `0` its absence, and anything else the
prefixes, so `WINDTRAP_MUTATE=1 dune runtest --force` surveys a tree
and `WINDTRAP_MUTATE=lib/calc.ml` scopes it. The scope narrows the
mutants the loop forks over, and the verdicts it writes are for those
mutants alone, so a scoped run's file is a true, smaller answer for its
executable. Every instrumented file still registers, so `--arm` arms
whatever the executable holds, in scope or not. A prefix that leaves
nothing to test is an error naming the flag, not the build; asking for
both flags at once is a usage error.

## Without dune

Any build can instrument: the backend is a Ppxlib rewriter, so a
driver linked against it once — `let () = Ppxlib.Driver.standalone ()`
with `ppxlib` and `ppx_windtrap.mutate` — is a `-ppx` for the
compiler, and the installed `windtrap` is two archives beside the
compiler's own library (`$lib` below, where `META` is). Instrument the
library under test, not the test file; link the test against
`windtrap`; run it with the flag; merge with the installed binary.
`test/facade/nodune.t` in windtrap's tree is this session, held by a
test:

```
$ ocamlopt -ppx "./mutate_ppx.exe --as-ppx" -I "$lib/windtrap/runtime" -c calc.ml
$ ocamlopt -I +unix -I "$lib/windtrap/runtime" -I "$lib/windtrap" \
    unix.cmxa windtrap_runtime.cmxa windtrap.cmxa calc.cmx test_calc.ml -o test_calc.exe
$ ./test_calc.exe --mutate=calc.ml
calc: 4 passed in 0.0003s.

─────────────────── survivors (2) ────────────────────

  SURVIVED  calc.ml:2:16:ge   n > 0  →  n >= 0
      2 │ let sign n = if n > 0 then 1 else 0

    2 tests ran this line and none failed:
      sign of a negative      test_calc.ml:9
      sign of a positive      test_calc.ml:8

  …

──────────────────────────────────────────────────────

mutants: 2 survived of 3 reached by this suite · 1 killed
reproduce: ./test_calc.exe --arm <id>
$ ./test_calc.exe --arm calc.ml:2:16:ge
mutant calc.ml:2:16:ge armed: n > 0 → n >= 0
calc: 4 passed in 0.0002s.
mutant survived: the armed site was evaluated 2 time(s) and no test failed.
$ windtrap mutants
…
mutants: 2 survived of 3 reached · 1 killed · 1 executable
reproduce: WINDTRAP_MUTATE_ARM=<id> <re-run the instrumented suite>
```

An executable under no build directory writes its verdicts under the
working directory's own `_windtrap/mutants` — a tree built without
dune never grows a `_build` — and `windtrap mutants` finds that
directory by walking up from wherever it runs, exactly as it finds a
build directory's `_mutants`. The per-executable report's footer
spells the run as it was made, and the project report's placeholder
stands for `make test`, or whatever runs the suite, with `--arm`'s
mirror in front of it.

## One command, if you want it

The two project commands fold into one alias at the top of the test
tree:

```lisp
(rule
 (alias mutate)
 (deps (alias_rec runtest) (universe))
 (action (run %{bin:windtrap} mutants)))
```

```
$ WINDTRAP_MUTATE=1 dune build @mutate --force --instrument-with ppx_windtrap.mutate
```

Each piece is load-bearing: the variable because the suites read it,
the flag because the suites must carry the mutants, `--force` because a
mutation run is not a cached artifact, and `(universe)` because the
`.mutants` files are written at exit and are not declarable
dependencies, so without it the merge action caches against nothing
and silently goes stale. A plain `dune build @mutate` without the
variable and the flag rebuilds the executables uninstrumented, which
stales every verdict, and the merge refuses loudly. `(deps (env_var
WINDTRAP_MUTATE))` on a test stanza is the per-stanza alternative to
`--force`: dune then re-runs that suite whenever the variable changes.

windtrap's mutation testing is deliberately the 90% product: one honest
count after a run you already make, and the names of the tests that let
the change through. The other OCaml mutation tester is
[mutaml](https://github.com/jmid/mutaml), which works outside windtrap
and mutates a different set of expressions.
