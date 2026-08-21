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
backends. Then: run your tests with `WINDTRAP_MUTATE=1`; for every
mutant in the code those tests reach, windtrap re-runs them with the
mutant armed; a mutant none of them notice is reported, naming the tests
that ran it.

Two rules keep it honest. **A mutant changes meaning only in a forked
child, only when armed, and only in a build that asked for it** — with
the backend on and `WINDTRAP_MUTATE` unset the program is the original
program, and a process running with a mutant armed announces it before
any other output. And **nothing is catalogued on disk**: the mutants are
a data literal compiled into the binary, so a catalogue cannot go stale
against the code it describes. Only verdicts touch disk, under
`_build/_mutants`, one file per executable, overwritten on re-run.

The transcripts below are from [`examples/x-blueprint`](../../examples/x-blueprint),
run inside windtrap's own tree — which is why its paths carry that
prefix and every command carries `--instrument-with`. Copied out, the
example's `dune-workspace` declares the backend once and the flag
disappears from every command in this chapter.

## Running it on a file

Name the file you are working on and the suite that tests it:

```
$ WINDTRAP_MUTATE=1 WINDTRAP_MUTATE_ONLY=examples/x-blueprint/lib/slug.ml \
    dune exec --instrument-with ppx_windtrap.mutate examples/x-blueprint/test/unit/test_slug.exe
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
reproduce: WINDTRAP_MUTATE_ARM=<id> dune exec --instrument-with ppx_windtrap.mutate examples/x-blueprint/test/unit/test_slug.exe --
```

The suite runs once as a dry run — proving it green, and recording per
mutant exactly which tests evaluated it — then the process forks itself
once per reached mutant and runs only those tests.

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
$ WINDTRAP_MUTATE_ARM=examples/x-blueprint/lib/slug.ml:2:41:lt \
    dune exec --instrument-with ppx_windtrap.mutate examples/x-blueprint/test/unit/test_slug.exe
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
read-only while a mutant is armed: a snapshot or `[%expect]` mismatch is
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
most binaries were built from other sources.

## The whole project

A library is usually covered by several `(test)` stanzas, and each
executable scores only what its own tests reach. Verdicts do not merge
the way coverage counts do: a mutant can be **killed** by one suite and
merely **reached** by another, and the truth about the project is
*killed*. Reporting the second suite alone produces a **false
survivor**, which sends the reader to write a test that already exists.
In the example, the expect suite alone reports seven survivors of
`slug.ml`; the unit suite kills every one of them.

So the project's answer is one alias at the top of the test tree,
which runs every suite mutated and merges what they wrote:

```lisp
(rule
 (alias mutate)
 (deps (alias_rec runtest) (universe))
 (action (run %{bin:windtrap} mutate)))
```

```
$ WINDTRAP_MUTATE=1 dune build @mutate --force --instrument-with ppx_windtrap.mutate
```

Each suite prints its own report as it runs, then `windtrap mutate`
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
reproduce: WINDTRAP_MUTATE_ARM=<id> dune runtest --force --instrument-with ppx_windtrap.mutate
```

That run exits 1: every survivor in it is a test to strengthen or an
equivalent mutant to dismiss, which makes it the one mutation exit code
a build may gate on. With the row restored the project has nothing to
report and exits 0:

```
mutants: 18 reached · 18 killed · 4 executables
```

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

Three facts about the command. `--force` is required and is not a wart:
a mutation run is not a cached artifact, and dune would otherwise treat
a `runtest` action whose inputs have not changed as already done. A
plain `dune build @mutate`, without the variable and the flag, rebuilds
the executables uninstrumented, which stales every verdict — a verdict
is invalidated by any later build of the executable that wrote it — and
the merge refuses loudly, naming the command above. And
`WINDTRAP_MUTATE_ONLY` scopes the work without narrowing the suite, so
a scoped run still writes its verdicts; selecting tests — `-f`, tags,
`--shard`, `--failed`, an in-source `ftest` — does narrow it, and such a
run reports in full, leaves any existing verdict file where it was, and
says so:

```
verdicts not saved: this run's selection narrows the suite, and a partial run's verdicts would stand in the project merge as the whole.
```

`windtrap mutate` on its own runs no tests and drives no build; it
reads `_build/_mutants`, or the `.mutants` files and directories named
as arguments, and a missing path is a loud error, never a silent
narrowing of the merge.

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
parallel in this release, so the bill scales with the catalogue: one
file at a time is the habit, and `WINDTRAP_MUTATE_ONLY` is how you spell
it.

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

Three environment variables and no flag on any runner: the inline
runner's argument parser accepts only dune's inline-test protocol, so a
flag would exist for half the users. All three are read by the test
executable and by nothing else, and an unrecognized value is an error
naming the variable.

| variable | values | default |
| --- | --- | --- |
| `WINDTRAP_MUTATE` | `1` to run the survey, `0` for an ordinary run | unset (ordinary run) |
| `WINDTRAP_MUTATE_ONLY` | source path prefixes, comma-separated | unset (every file) |
| `WINDTRAP_MUTATE_ARM` | a mutant identifier | unset |

`WINDTRAP_MUTATE_ONLY=lib/calc.ml,lib/eval.ml` is how a real project is
mutated: one file, or one directory, at a time. The runtime applies it
**at registration**, so a file outside the prefixes never enters the
catalogue — narrowing the *work*, which a filter over the report would
not. It also bounds `WINDTRAP_MUTATE_ARM`, since a mutant of an
out-of-scope file was never registered. A prefix that leaves nothing in
the catalogue is an error naming the scope, not the build; asking for
the survey and an armed mutant at once is a refusal, not a guess — the
loop arms each mutant itself, so an armed parent would mutate its own
dry run.

windtrap's mutation testing is deliberately the 90% product: one honest
count after a run you already make, and the names of the tests that let
the change through. The other OCaml mutation tester is
[mutaml](https://github.com/jmid/mutaml), which works outside windtrap
and mutates a different set of expressions.
