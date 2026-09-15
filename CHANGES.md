# Changelog

All notable user-facing changes are documented here — features, fixes,
performance, and anything that changes the public surface. That surface is
`lib/windtrap.mli`, the CLI and `WINDTRAP_*` contract, and the runner's exit
codes. New entries go at the top of their section.

## [0.2.0] - unreleased

Windtrap 0.2.0 is a ground-up rewrite around three commitments: **declaring a
test is pure data**, **a failure shows you the values**, and **anything random
replays**.

### Highlights

**A green run is one line, and a slow test still speaks up.**

```
$ dune runtest
mylib: 48 passed in 1.2s.
```

The header and the per-test glyph row stay hidden until the run has something
to say. A failure brings them out — and so does a passing test that ran long:

```
$ dune runtest
mylib: 1 test
.
slow tests (1):
  1.31s  search › reindex
(exempt with the "slow" tag, or raise --slow-threshold SECONDS)

1 passed in 1.31s.
```

`-v` gives one line per test, and the slowest-tests list with it.

**A failure marks what actually changed, for any type.** `equal` takes a
testable — a printer and an equality — and the report is derived from the
printed rendering, so a record, a variant, or an abstract type gets the same
treatment as `int` with no diff combinator to write. The marks cover the
region where the two renderings stop agreeing, and nothing else:

```
mylib: 1 test
F
──────────────────── failures (1) ────────────────────
  FAIL  users › sessions after login
    test/test_mylib.ml:19
      19 │               equal

    expected  [("alice", [1; 2; 3]); ("bob", [4])]
    actual    [("alice", [1; 2; 3]); ("bob", [4; 5]); ("carol", [])]
                                               ~~~~~~~~~~~~~~~~~~
──────────────────────────────────────────────────────

1 failed in 0.000781s.
```

Bob's list grew and Carol's entry is new — that is what the mark spans. The
expected side lost nothing, so nothing is drawn under it. No `~pos`
annotation either: the location comes from the assertion's call stack.

**Properties are ordinary tests over generators.** One verb, an `'a Gen.t`, a
body that asserts. Shrinking is integrated and constraint-preserving, so a
counterexample arrives minimal without a shrink function.

```ocaml
prop "rev is an involution" Gen.(list int) (fun l ->
    equal (list int) l (List.rev (List.rev l)))
```

**Every failure tells you how to reproduce it.** Generated values derive from
the run's root seed, the test's path, and the case index, so a failing
property prints the exact replay command for the way the run was invoked —
`dune exec … -- --seed s1:… -f '…'` under dune, `WINDTRAP_SEED=… dune
runtest` for inline suites.

**Expect tests you can move a ppx_expect suite onto.** We ran Jane Street's
own ppx_expect corpus against `ppx_windtrap`: 33 of the 36 supported cases
run with matching semantics — 16 pass with no correction, and 17 mismatch
where upstream mismatches and record the correction upstream's runtime
records — and all 20 unsupported constructs fail at expansion with an error
naming the exact construct, rather than quietly doing something else.

**Coverage without a second toolchain.** One inert `(instrumentation (backend
ppx_windtrap.coverage))` stanza on the library under test; `dune runtest
--instrument-with ppx_windtrap.coverage` for an inline percentage, and
`windtrap coverage --min 80` to gate CI.

### Breaking changes

#### Cheat sheet

| 0.1.0 | 0.2.0 |
| --- | --- |
| `prop name (list int) law` (a `bool` law) | `prop name Gen.(list int) (fun l -> equal (list int) l (law l))` |
| `prop'` | `prop` (it is assertion-style now) |
| `prop2` / `prop3` / `prop4` | `prop` with `Gen.pair` / `Gen.triple` / `Gen.quad` |
| `~config` on a property | `~count` and `~examples` |
| `~gen` on a testable | a `Gen.t` argument; printers attach with `Gen.with_pp` |
| `Gen.oneofl` / `Gen.oneof` | `Gen.of_list` / `Gen.one_of` |
| `Gen.list_size sg g` | `Gen.list ~size:sg g` |
| `Gen.string_size sg cg` | `Gen.string_of ~size:sg cg` |
| `Gen.sized f` | `Gen.bind Gen.nat f` |
| `cover ~label:"even" ~at_least:20. c` | `cover "even" c` |
| `nat` / `small_int` testables | `Gen.nat` / `Gen.small_int` |
| `float 0.` / `float_rel ~rel:0. ~abs:0.` | `float_exact` |
| `snapshot ~pos:__POS__ s` | `snapshot "name" s` |
| `snapshotf fmt …` | `snapshot "name" (Printf.sprintf fmt …)` |
| `expect s` / `capture fn s` | `equal string s (output ())`, or `let%expect_test` |
| `group ~before_each ~after_each` | `bracket ~setup ~teardown` on each test |
| `group ~setup ~teardown` | `fixture ?teardown create` |
| `testable ~pp ()` | `Testable.structural ~pp` |
| `testable ~pp ~equal ()` | `Testable.make ~pp ~equal` |
| `of_equal eq` / `contramap f t` | `Testable.of_equal eq` / `Testable.contramap f t` |
| `seq t` / `lazy_t t` | `Testable.contramap List.of_seq (list t)` / `Testable.contramap Lazy.force t` |
| `is_ok r; Result.get_ok r` | `require_ok r` |
| `some t e v` | `equal (option t) (Some e) v` |
| `ok t e r` / `error t e r` | `equal t e (require_ok r)` / `equal t e (require_error r)` |
| `no_raise fn` | `fn ()` |
| `raises_invalid_arg "m" fn` | `raises (Invalid_argument "m") fn` |
| `raises_failure "m" fn` | `raises (Failure "m") fn` |
| an any-message or substring check | `raises_match Exn.invalid_arg fn`, `raises_match (Exn.failure ~substring:"…") fn` |
| `cases ty inputs name fn` | `cases ~name:string_of_int name inputs fn` |
| `?here:[%here]` | `?pos:__POS__`, or nothing |
| `~tags:(Tag.labels [ "net" ])` | `~tags:[ "net" ]` |
| `Tag.speed Slow` | the `slow` declaration, or `~tags:[ "slow" ]` |
| `~tags:[ "disabled" ]` | `skip ()` in the body, or `--exclude-tag` on the command line |
| `run ~quick ~filter ~seed …` | the CLI flags and `WINDTRAP_*` mirrors, or `run ~argv` |
| `--format` / `WINDTRAP_FORMAT` | default ⊂ `-v`; TAP consumers move to `--junit PATH` |
| `-q` / `--quick` | `--exclude-tag slow` |
| `(instrumentation (backend ppx_windtrap))` | `(instrumentation (backend ppx_windtrap.coverage))` |
| `--instrument-with ppx_windtrap` | `--instrument-with ppx_windtrap.coverage` |
| `windtrap coverage --summary-only` | `windtrap coverage` (per-file is the default report) |
| `windtrap coverage -C N` / `--context N` / `--skip-covered` | removed |
| `windtrap coverage --coverage-path P` / `--source-path P` | positional `PATH…` |
| `windtrap coverage -j` | `--json` (long form only) |

#### What changed, and why

- **Properties are one verb.** `prop` takes a generator and an
  assertion-style body returning `unit`; shrinking is always on and
  constraint-preserving. The rest of the 0.1.0 `Gen` surface is gone —
  `fix`, `delay`, `no_shrink`, `add_shrink_invariant`, `make_primitive`,
  `find`, `ap`, `>>=`/`>|=`, `sized`, and the `?origin`/`?ratio` knobs.
  Recursion is `let*` over `Gen.nat` — which is all `sized` ever was, minus
  the promise of an ambient size parameter windtrap does not thread. Four
  generators go too: `Gen.int32_range` / `Gen.int64_range` → `Gen.map` over
  `Gen.int_range`, `Gen.nativeint` → `Gen.map Nativeint.of_int Gen.int`, and
  `Gen.either` → `Gen.map` over `Gen.bool` and the two sides. `prop` also
  drops `?timeout`: the per-test timeout (declaration `~timeout`, or the
  runner's `--timeout`) covers the whole property, generation and shrinking
  included. A timeout that expires during shrinking ends the search and
  reports the best counterexample found so far, marked as possibly not
  minimal. `cover` is presence-only —
  `cover : string -> bool -> unit`, failing the property unless at least one
  passing case marked the label. A percentage-calibrated distribution gate has
  no replacement: `classify` prints the proportion and a human reads it, and
  the `Invalid_argument`s for an out-of-range threshold and for conflicting
  thresholds on one label go with `~at_least`. `collect` and `classify` are
  unchanged.
- **Snapshots are keyed by name, not by source position.** The baseline lives
  at `<src_dir>/__snapshots__/<src_basename>/<name>.snap`, so moving a test
  within its file no longer orphans it. Re-accept once with `-u` (or
  `WINDTRAP_UPDATE=1` under dune) and review with `git diff`, then add
  `(deps (glob_files_rec __snapshots__/**))` to the test stanza so baseline
  edits re-trigger `dune runtest`. The snapshot environment knobs
  (`WINDTRAP_SNAPSHOT_DIR`, `WINDTRAP_SNAPSHOT_DIFF_CONTEXT`,
  `WINDTRAP_SNAPSHOT_MAX_BYTES`, `WINDTRAP_SNAPSHOT_REPORT`) are gone: the
  baseline path derives from the source file, and the report is part of the
  standard output.
- **The expect-string family is removed.** `expect`, `expect_exact`,
  `capture`, and `capture_exact` have no replacement spelling. Assert on
  captured output with `equal string "…" (output ())`, snapshot it with
  `snapshot "name" (output ())`, or write a real expect test with
  `let%expect_test` and `[%expect]` (ppx_windtrap). `output ()` stays, and now
  drains standard error and subprocess output along with standard output.
- **Group hooks are removed**, so no user code runs outside a test's
  exception boundary. `group ~before_each`/`~after_each` becomes
  `bracket ~setup ~teardown` on each test — partially apply it to build a
  reusable constructor. `group ~setup`/`~teardown` for expensive shared state
  becomes `fixture ?teardown create`, acquired on first use and released by
  the runner after the last test. Accessors now work only inside a run;
  0.1.0's `fixture` was a plain lazy cache.
- **`Testable.make` requires `~equal`.** The `~gen` and `~check` fields are
  gone: generation lives in `Gen`, and diffs are computed from printed
  values, so every type gets a highlighted diff from its printer alone. The
  `nat`/`small_int` pseudo-testables were really distributions and moved to
  `Gen`; the `seq` and `lazy_t` witnesses are dropped — compare through
  `Testable.contramap List.of_seq (list t)` and
  `Testable.contramap Lazy.force t`. `of_equal` and `contramap` move behind
  `Testable.` with them: they are constructors, and the interface's rule is
  witnesses flat, constructors in `Testable`.
- **`is_ok`/`is_error` become `require_ok`/`require_error`, which assert *and*
  unwrap.** Most call sites get shorter:
  `is_ok r; Result.get_ok r` collapses to `require_ok r`. The wrapper
  testables (`some`, `ok`, `error`) go through plain composites. `is_some` and
  `is_none` keep their 0.1.0 meaning — they assert presence and absence and
  return nothing — and `require_some` is the unwrapping form beside them.
  On exceptions, `raises (Invalid_argument "m")` renders a wrong message as a
  message diff; any-message and substring forms move to `raises_match` with
  the new `Exn` predicates.
- **`cases` drops its testable and requires `~name`.** The base name comes
  first and each sub-test is named from its own value
  (`cases "ports" ~name:string_of_int [ 1; 80; 8080 ] fn`), individually
  selectable with `-f`. There is no positional `<base>.<i>` default, because a
  child's path is its identity — it keys the child's per-case property seeds
  and its entry in the `--failed` store — so a row inserted at the front would
  silently re-key every row behind it, and a numbered name carries nothing at
  the failure site anyway.
- **`?here` is gone.** Use `?pos:__POS__`, or nothing: failure locations
  default to a best-effort call-stack capture, falling back to the enclosing
  test's declaration line when the failing call's frame is gone (a call in
  tail position).
- **Bodies return `unit`.** `test`/`ftest`/`slow` take `(unit -> unit)` and
  `bracket` bodies return `unit`; 0.1.0 accepted `(unit -> 'a)` and silently
  ignored the result. End with an assertion, or `ignore`.
- **Tags are plain strings.** Every `?tags` takes a `string list`, and the
  `Tag` module is no longer public. The Quick/Slow speed pair is now just the
  `"slow"` tag, and `--quick` is `--exclude-tag slow` — the selection it built
  all along, under one name instead of two. The `"disabled"` label loses its
  meaning with them: it *deselected* a test rather than skipping it, and a
  deselected test is not counted, not listed, and not mentioned, so the
  transcript said "9 tests" where there were ten and nothing said one was
  parked. A test that must not run now says so where a reader can see it —
  `skip ()` in the body, which the transcript reports and counts, or
  `--exclude-tag` on the command line, which the header echoes.
- **`run` keeps only `?argv`.** The programmatic configuration parameters
  (`~quick`, `~filter`, `~seed`, `~format`, `~junit`, `~update`,
  `~snapshot_dir`, …) are removed. Set the same knobs through the CLI flags or
  `WINDTRAP_*` variables they mirrored, or hand `run` a synthetic `~argv`.
- **Output formats are gone; verbosity is one axis.** `--format` (and
  `WINDTRAP_FORMAT`) is removed — terminal verbosity is default ⊂ `-v`, not
  a format: both levels print the same failure blocks and the same summary, and
  the compact glyph row stays the default as in 0.1.0. TAP is gone; consumers
  should move to `--junit PATH` or the automatic GitHub Actions annotations.
  `-q` is gone entirely — in 0.1.0 it meant `--quick`, which is now
  `--exclude-tag slow`. `--seed` takes the printed `s1:` token, not an
  integer.
- **Coverage's backend has one spelling: `ppx_windtrap.coverage`.** The
  `ppx_windtrap` library carried an `(instrumentation.backend)` field of its
  own, so the bare name resolved too. Both reached the same rewriter, but the
  bare one's `ppx_runtime_libraries` linked the windtrap core into every
  instrumented library's closure — a test framework in the production
  dependency cone, which the first real downstream migration nearly shipped
  across 25 libraries by following the SKILL. The field is gone:
  `(instrumentation (backend ppx_windtrap))` is now a dune error naming the
  missing backend, and the trap cannot be re-armed. Rename one token, in the
  stanza and in `--instrument-with`.
- **Coverage percentages change meaning.** 0.2.0 grades expression coverage
  with entry points per block *and* out-edge points on calls, which count as
  covered only when the call returns. Numbers are not comparable with
  0.1.0 runs. The per-file report is now the default `windtrap coverage`
  output, with `--min PCT` to gate CI, `--json` for a machine-readable
  artifact whose per-file objects keep `uncovered_lines` and drop
  `uncovered_offsets`, `--lcov` for an LCOV tracefile — Codecov,
  Coveralls, GitLab, editor gutters, and `genhtml` for HTML — and
  `--expect PATH` to fail when a source under `PATH` has no data at all
  (not instrumented, or linked into no test executable that ran).
- **`open Windtrap` narrows.** It brings the flat values plus exactly four
  modules: `Testable`, `Gen`, `Exn`, and `Private` (unstable internals). The
  0.1.0 `Tag`, `Pp`, and `Ppx_runtime` modules are no longer public — the
  internals live under `Private`. Project modules with other names are no
  longer shadowed.

### Added

**Ordering assertions: `less`, `at_most`, `greater`, `at_least`.** Every
consumer wrote comparisons as `is_true (a < b)` — and, because that failure
can only say `expected true / actual false`, smuggled the numbers into `~msg`
with `sprintf`, or built `gt/ge/lt/le` helpers over `satisfies ~claim` for
`int` alone. The four verbs take a witness, the bound as `~than`, and the
value last, and the failure keeps both:

```ocaml
less int ~than:3 (retries ());
at_least (float 1e-6) ~than:0.4 result.accept_rate
```

```
expected  less than 3
actual    5

expected  at least 0.4
actual    0.38
```

The claim is derived from the verb and the bound, rendered by the witness,
so it cannot drift from the check the way a hand-written `satisfies ~claim`
can. The order comes from the witness: the base-type witnesses carry their
module's, the three float witnesses order exactly with `Float.compare`
(tolerance belongs to equality, so under `float 0.5` the values `1.0` and
`1.2` are equal *and* `1.0` is less than `1.2`), `Testable.structural`
carries `Stdlib.compare`, and `Testable.contramap` orders through its
projection. `Testable.with_compare` gives any other witness an order —
`Testable.make ~pp:M.pp ~equal:M.equal |> Testable.with_compare M.compare`
is the conventional trio — and `Testable.compare` reads it back. No
container witness carries one (an option or a list admits several, and a
guessed one would be accepted silently), nor do `pass`, `Testable.of_equal`
and a plain `Testable.make`; an ordering verb over such a witness raises
`Invalid_argument` naming `Testable.with_compare`, whether or not the
assertion would have held. `satisfies` stays for claims that are not orders.

**A counterexample built with `map` or `bind` prints its pre-image.** Those
combinators — and so `let+`, `and+` and `let*` — derive no printer, and until
now their counterexamples rendered as `<no printer>` unless a `Gen.with_pp`
sat on top of every composition, which consumers wrote by hand to re-attach
what `Gen.pair` had already derived. A printerless counterexample now renders
as its *pre-image*: the same shape, with each printerless `map` or `bind`
result replaced by the input its mapping function received, printed by the
generator that drew it — through `pair`, `list` and the other deriving
combinators, at any depth, down to the nearest generator that prints. A
`bind` prints its inner value alone when the inner generator prints, and
`outer -> inner` otherwise. Shrinking walks the same tree, so the pre-image
printed belongs to the shrunk value. Before, for
`let* shape = gen_shape in let+ a = gen_f32 shape and+ b = gen_f32 shape in (a, b)`:

```
counterexample (case 0, shrunk 3 steps): <no printer>
(this generator has no printer — attach one with Gen.with_pp to see the value)
```

After:

```
counterexample (case 0, shrunk 3 steps): from [1] -> ([0.], [0.])
(the value has no printer — shown is its pre-image, what map and bind computed it from)
```

`from` marks the rendering as the input of the mapping functions, not the
value the body received. `Gen.with_pp` keeps its meaning and wins over the
pre-image, and the `<no printer>` placeholder with its remedy line now
appears only when a `constant`, `pure` or `of_list` leaf without a printer
leaves nothing to render. `Failure.kind.Property` carries `rendering`
(`Value`, `Pre_image` or `Placeholder`) in place of the `printerless` flag,
and `Gen.Private.sample` yields a tree of samples — value plus rendering —
that `Gen.Private.value` and `Gen.Private.render` project.

**Inline tests that nothing drives now fail loudly.** `let%expect_test` and
`let%test` code preprocessed with `ppx_windtrap` inside a plain
`(executable)` or `(test)` stanza registers its tests at module load — and
with no `(inline_tests)` stanza nothing ever drives them: the binary exited
0 having run nothing, and its `[%expect]` payloads were never checked
against anything. The failure mode is real — a migrating project shipped
three whole directories of expect tests that way for months, every golden
unread. The first registration now installs an `at_exit` guard, and every
legitimate driving path disarms it: the runner protocol's entry in every
mode (a partition run, `-list-partitions`, the generated runner invoked by
hand), draining the registry from a hand-rolled harness, and arming a
mutant, whose process belongs to the mutation loop. A process that
terminates with registrations never claimed prints a diagnostic naming the
registered files and both fixes — add `(inline_tests)` to the library
stanza, or drive the runner protocol yourself — and exits 2, the
nothing-ran code, readable as neither a pass nor a test failure. Best
effort, against the silent 0 only: a death by signal or `Unix._exit`
bypasses `at_exit`, and those endings are already loud or deliberate.

**`setenv` and `chdir`: the environment and the working directory, scoped to
one test.** Both belong to the process, not to the test, so a test that
needed either wrote the save-and-restore by hand — and wrote it wrong,
because the restore has to survive the failure, the skip and the timeout too,
which `Fun.protect` around the body does not cover and a `bracket` teardown
covers only if nothing before it raised. `setenv name (Some v)` binds,
`setenv name None` unbinds, `chdir dir` moves, and the runner puts all of it
back at the attempt boundary — outside the timeout window, on every outcome,
once per `~retries` attempt, beside the scratch-path removal that already
worked this way.

The unbinding is a real one. OCaml's `Unix` can only bind, and binding to the
empty string is not unbinding: `Sys.getenv_opt` then answers `Some ""`, which
reads as *set* to every program that asks, so the code path a missing
variable takes stayed untestable. The unbinding half is now POSIX
`unsetenv(3)` through a C stub (on Windows, the empty assignment `_putenv`
documents as deletion), which is what makes `setenv name None` mean what it
says and what lets the runner restore a variable the test found unset.

Restoration is first-set-wins per variable: what comes back is what the
variable held before the test's *first* `setenv` of it, so binding one twice
still leaves behind what the test found. `chdir` restores the directory
captured at the test's first call. A restoration that *cannot* happen — the
test deleted the directory it came from — fails that test with a message
naming it, rather than being swallowed the way a leaked scratch directory is:
scratch under `/tmp` is inert, while a process left in the wrong place fails
everything after it under names that have nothing to do with the cause.

Both are process-global while the test runs, which the docs say plainly:
threads the test spawns and child processes it starts see the change, and a
thread still moving when the test ends races the restoration. Tests never
race each other — the runner is sequential, one domain.

**Mutation testing: the `ppx_windtrap.mutate` backend, `WINDTRAP_MUTATE`,
`windtrap mutate` and `@mutate`.** Coverage answers *did this line run*. It
cannot answer *would anything fail if this line were wrong*, and that is the
question a suite exists to answer — a test that calls `Calc.sub 10 4` and
asserts the result is positive covers the subtraction and does not test it. A
second, independently opt-in instrumentation backend,
`(instrumentation (backend ppx_windtrap.mutate))` on the library under test,
compiles every mutant of that library into the binary behind an inert guard,
and **the test executable becomes its own mutation runner**: run it with
`WINDTRAP_MUTATE=1`, and for every mutant in the code its tests reach,
windtrap re-runs those tests with the mutant armed. A mutant none of them
notice is reported, naming the tests that ran it.

```
$ WINDTRAP_MUTATE=1 dune exec --instrument-with ppx_windtrap.mutate test/test_calc.exe
calc: 7 passed in 0.001s.

──────────────── survivors (1) ────────────────

  SURVIVED  lib/calc.ml:9:11:add   a - b  →  a + b
      9 │   | Sub -> a - b

    2 tests ran this line and none failed:
      sub › of a negative       test/test_calc.ml:16
      sub › of two positives    test/test_calc.ml:15

───────────────────────────────────────────────

mutants: 1 survived of 5 reached by this suite · 4 killed
reproduce: WINDTRAP_MUTATE_ARM=<id> dune exec --instrument-with ppx_windtrap.mutate test/test_calc.exe --
```

The run executes the suite once as a dry run — proving it green and
recording, per mutant, exactly which tests evaluated it — then forks itself
once per reached mutant and runs only those tests. A survivor is a failure
block because a survivor *is* a failure — a defect report about named tests —
and naming the tests that ran the line and did not fail is what turns a score
into a work item; windtrap has it for free because it owns the runner and the
per-test boundary. Every survivor is one of two things: a weak assertion to
strengthen, or an equivalent mutant to dismiss in the source with
`[@mutate off "reason"]`, in the four spellings the coverage attribute already
uses. There is no suppression database and windtrap will never write the
attribute for you. Four operators ship (`neg`, `cmp`, `con`, `ari`). A suite
that kills everything it reaches says only `mutants: 5 reached by this suite ·
5 killed`.

The run narrows on both axes. `WINDTRAP_MUTATE_ONLY=lib/calc.ml` (a
comma-separated list of source path prefixes) mutates only the files named, so
the survey of the file you are working on costs that file's mutants and
nothing else. The ordinary test selection — `-f`, `-e`, the tag flags —
mutates only what the selected tests reach, which is how the last step of
writing a test is asking whether that test can fail:

```
WINDTRAP_MUTATE=1 WINDTRAP_MUTATE_ONLY=lib/calc.ml \
  dune exec --instrument-with ppx_windtrap.mutate test/test_calc.exe -- -f "sub"
```

The filtered run reports the same blocks, says `of 2 reached by the 2
selected tests` in its summary, and adds one line saying its verdicts were not saved, because a
partial run's verdicts would stand in the project merge as the whole.
`WINDTRAP_MUTATE_ARM=<id>` — the `reproduce:` footer spells it, with a
survivor's identifier in place of `<id>` — arms one mutant in one ordinary
run, announced on the first line, so the argument for mutation testing can be
watched happening on your own suite: the test that should have failed, not
failing. A per-executable run exits 0 whatever it finds. `WINDTRAP_MUTATE` is
a boolean like every other switch: `1` runs the survey, `0` or unset runs the
ordinary suite, anything else is an error naming the variable.

The project's answer is the merge, because a library is normally covered by
several test executables and their reports disagree by construction: a mutant
one suite kills and another merely reaches is killed, and the second suite's
view alone is a false survivor — the failure mode that makes people stop
running mutation tools. Each unfiltered run writes a verdict file under
`_build/_mutants`, and `windtrap mutate` merges them under **killed anywhere
wins**. The `@mutate` alias depends on `(alias_rec runtest)` and runs the
merge, so one command runs every suite with its mutants and merges:

```
$ WINDTRAP_MUTATE=1 dune build @mutate --force --instrument-with ppx_windtrap.mutate
…
──────────────── survivors (1) ────────────────

  SURVIVED  lib/calc.ml:14:7:ge   n > 0  →  n >= 0
     14 │   if n > 0 then

    3 tests in 2 executables ran this line and none failed:
      test_calc.exe   sub › of a negative    test/test_calc.ml:16
      test_eval.exe   eval › literal         test/test_eval.ml:9
      test_eval.exe   eval › nested          test/test_eval.ml:12

────────────── never reached (2) ──────────────

  UNREACHED  lib/calc.ml:22:5:le   n < limit  →  n <= limit
     22 │   if n < limit then

  UNREACHED  lib/calc.ml:31:14:sub   acc + x  →  acc - x
     31 │   List.fold_left (fun acc x -> acc + x) 0

───────────────────────────────────────────────

mutants: 1 survived of 12 reached · 11 killed · 2 never reached · 3 executables
reproduce: WINDTRAP_MUTATE_ARM=<id> dune runtest --force --instrument-with ppx_windtrap.mutate
```

The same blocks, with the executable beside each witness — the identity each
verdict file records: `test_calc.exe`, or the library's name for an
inline-test runner — and a second list for the mutants no executable's tests
evaluate, each with its rewrite and its line. Those have their own remedy,
*write a test* rather than strengthen one, and neither finding needs the
coverage backend enabled. The merge exits 1 when any mutant survived every
executable that reached it: the one mutation exit code a build gates on,
because every survivor in it is a test to strengthen or an equivalent mutant
to dismiss. Unreached mutants alone are never red, and a project that kills
everything reads `mutants: 14 reached · 14 killed · 3 executables`. The merge
excludes a verdict whose executable was rebuilt or deleted since it ran, so
`--force` and the backend flag are part of the command: a plain `dune build
@mutate` rebuilds the suites uninstrumented, which stales every verdict, and
the merge refuses loudly rather than reporting on air. Both stanzas — the
backend on the library, the `@mutate` alias at the top of the test tree — are
written out and commented in `examples/x-blueprint/`.

Three variables in all — `WINDTRAP_MUTATE`, `WINDTRAP_MUTATE_ONLY` and
`WINDTRAP_MUTATE_ARM` — every one environment-only, because the inline
runner's argument parser belongs to dune and a flag would exist for half the
users. No catalogue, no cache,
no configuration file, **no new dependency**, and `lib/windtrap.mli` is
unchanged. Mutation needs `Unix.fork` and declines by name on Windows, where
`WINDTRAP_MUTATE_ARM` on one mutant is the fallback. See
[the manual chapter](doc/manual/mutation.md); the laws that contain it are
Laws 11–13, 15 and 16 in [`doc/dev/architecture.md`](doc/dev/architecture.md).

**`in_order ~subs`.** Whether a string shows its parts *in order* could not be
asked without throwing the string away. A log that must show connect, then
authenticate, then disconnect was asserted with three `contains` calls, which
pass just as happily on a log that shows them backwards; the order was the
claim and nothing checked it. `in_order ~subs` searches each element from the
end of the previous element's match and, on a break, names the element that
caused it — its index and its value — the byte the search had reached, and an
excerpt of the region still to be matched. When that element is in the string
but behind the cursor the failure says so and marks it, because "out of order"
and "missing" are different bugs and only the first is invisible to
`contains`.

The excerpt every containment failure carries is bounded by what it is for.
A found occurrence keeps its full window, because there the excerpt is the
evidence, and so does an `in_order` chain break, whose cursor-anchored region
is the diagnosis itself. When the needle is *absent* there is nothing in the
haystack to mark and the excerpt is only context, so it is cut to at most 10
lines and 1 KiB — after the last complete line, never inside a UTF-8 sequence
— and the line under the block states the cut in the words it already used for
the stored bound: `(excerpt: bytes 0-1023 of a 20006-byte haystack)`. An 8 KiB
SVG that does not contain the needle no longer scrolls the verdict off the
screen.

**`starts_with` and `ends_with`.** `contains ~sub` existed and its prefix and
suffix counterparts did not, so string-shape assertions fell back to
`is_true (String.starts_with ~prefix p s)` — a boolean, with the string gone.
These carry the containment payload, so an affix that is present but in the
wrong place is reported with its offset and marked in the haystack, which is
the case `contains` cannot even fail on.

**Stateful testing: `stateful`, `command`, `call`.** A property checks a law
over one value; a cache, a queue, a pool or a cursor needs one over a
*sequence of calls*. A command bundles four facts in one place — how to draw
its argument, when it is legal, what it does to the model, and what it does to
the real thing — and its body calls the system and asserts with the ordinary
verbs. Because a result is produced and checked in one expression it never
crosses a boundary, so there is no result type to declare, no witness registry
and no `show_cmd`: the library derives the printer and there is no shrinker to
write, as everywhere else.

```
counterexample (case 0, shrunk 5 steps):
  2 calls, last: pop
  []   1  push 0
  [0]  2  pop
which failed with:
  invariant after step 2 of 2: pop
  expected  0
  actual    1
```

Programs draw at a fixed length, are repaired against the model so an illegal
call is removed rather than skipped — the program you read is the program that
ran — and shrink by deleting calls and reducing arguments, never by
substituting one command for another. `~scope` builds a fresh system per case
*and per shrink candidate* and reclaims it — it takes a callback, so a resource
that exists only *inside* one (an Eio env or switch, any `with_`-style API) is
as testable as a value some setup could have returned, and a release failure
never masks the failure you were shown. `?invariant` checks the state that no
single command owns, and `?pp_model` prints the model each call was made in. A
`~pre` or `~next` that raises is reported as a specification failure naming the
command and step, rather than escaping into the generator and silently ending
the shrink search. See
[the manual chapter](doc/manual/stateful-testing.md).

**`text`, a string witness that prints verbatim.** `string` renders with `%S`
— quoted, escaped, on one line — which buries the difference between two
multi-line values in `\n` soup. `text` prints the same string unescaped, and
because the rendering spans lines it takes the report's unified-diff path, so
rendered output, serialized documents and logs diff line by line. Equality is
unchanged, byte for byte: trailing whitespace and a missing final newline
still fail, and the diff marks them.

**`~max_discard` on a property.** The engine has always had a discard budget —
twice the effective case count — but nothing exposed it. A law with a
genuinely rare precondition had no way to buy more attempts, and the only
signal was the give-up failure quoting a budget you could not change. It is
declared where the author who knows the rate is standing, beside `~count`.

**`WINDTRAP_JUNIT`, `WINDTRAP_BAIL`, `WINDTRAP_OUTPUT`.**
Under `dune runtest` the environment mirrors *are* the CLI, and these three
flags had none — so `--junit`, which the CI guide recommends, could not be
reached from the command CI actually runs, and neither could `--bail`. They
mirror like the rest, with the same precedence (CLI > env > default) and the
same rule for a malformed value: a usage error naming the *variable*, never a
silent default.

`--failed` deliberately has no mirror: the last-failed store lives under
`-o`/`--output`, which dune's sandbox moves per run, so
`WINDTRAP_FAILED=1 dune runtest` would refuse every suite that did not fail
last time — which is every suite, on a fresh build tree. The flag is for a
directly executed binary, where the loop is real.

**`NO_COLOR` is honoured.** The de-facto standard: with the variable set to
any non-empty value, `--color auto` — the default — styles nothing, on a
terminal, under dune, and in the reporting commands alike. `--color always`
still wins, because the user asked, and `--color never` was already the
explicit spelling.

**`mem`, and a printer for `is_none` — nineteen assertion verbs in all.**
`mem t x xs` is containment one type up from `contains`: the failure shows the
element you wanted and the list you got, where `is_true (List.mem x xs)`
showed `false`. `is_none` gains `?pp`, the optional printer the `require_*`
verbs already take ("render the branch you did not want"), so an absence
assertion that fails can show the value that was there — without demanding a
witness, a printer *and* an equality both, for a type it never compares.

**Assertions.** New verbs, all of them chosen so the failure keeps the data
a boolean would have thrown away: `satisfies` (renders the rejected value),
`contains` / `not_contains` (print the needle and a bounded excerpt),
`require_some` / `require_ok` / `require_error` / `require_match` (assert and
unwrap), and the `Exn` predicates for `raises_match` — `invalid_arg`,
`failure` and `sys_error`, each taking the `?substring` constraint the stdlib
does not offer, a whole message being `raises`' job, which holds both
exceptions and diffs their messages instead of rejecting. `satisfies` also
takes `?claim`, the sentence on the expected side, which is what makes it the
comparison assertion: `satisfies ~claim:"greater than 0" int (fun n -> n > 0) n`
reports `expected greater than 0` against `actual 0`, where `is_true (n > 0)`
can only report `true` against `false`. `float_exact` is a bit-exact float
witness — every NaN equal to every NaN, `0.` and `-0.` distinct — so a test
can assert that a function returns NaN.

**Failure reports mark what changed.** Both renderings are compared and the
differing regions marked: a unified diff on multi-line values, and on short
ones a minimal edit script over code points, so a mark never splits a
multi-byte character. Plain (no-color) output carries the marks on a `~~~`
line under the side they belong to — one per side that has any, so a pure
insertion draws nothing under the expected value — where color tints the
regions in place.

A mark is only shown when it points at a small part of a mostly shared
value. Once it would cover half a side, the two values simply differ, and
scattering marks over them draws the eye to coincidental character
alignments — `Some _` against `None` marking `S`, `m`, `e` against `N`, `n`
says nothing the plain pair does not. Those pairs print unmarked, and under
color each side is tinted whole instead.

**Color says which side, everywhere.** Green is the expected side and red
the actual one, on every block that shows both — equalities, `raises`
mismatches, and the `satisfies`/`require_match` claims whose expected side
is a description rather than a value. The summary counts wear the glyph
row's colors too (green pass, red fail, yellow skip, faint expected
failure), so a color means one thing across the whole transcript.

**Generators.** Printers derive by composition — a composite prints exactly
when its components do — and attach with `Gen.with_pp`. New: `such_that`
(with a fixed resample budget of 100) and the size-controlled
`string_of`/`bytes_of`. `~examples` runs pinned regressions first on every
run, and `~count` overrides the case count per declaration.

**Test structure.** `subtest` names sub-cases that all run even after one
fails. `xfail` keeps known-bug reproductions in-tree without a red run.
`temp_dir` / `temp_file` give runner-cleaned scratch paths on every outcome,
and `current_test` exposes the running test's path.

**Diagnosis when a test raises.** An uncaught exception's report carries its
backtrace — the raise site, not just the constructor and the test's
declaration line — without the reader having to know about `OCAMLRUNPARAM=b`.

**Deterministic seeds.** Every generated value derives from the run's root
seed, the test's path, and the case index. The root seed prints as an `s1:`
token in the header of any suite declaring properties, and every property
failure prints the exact replay command for the way the run was invoked
(`dune exec <path> -- --seed … -f '…'` under dune, argv0 when run directly,
`WINDTRAP_SEED=… dune runtest` for inline suites).

**Shrinking you can see the end of.** A shrink search stops after 100
accepted steps. When it stops there rather than converging, the report says
so — `shrinking stopped after 100 steps; counterexample may not be minimal`
— so a truncated search never reads like a minimal one. `--max-shrink N`
(`WINDTRAP_MAX_SHRINK`) raises the budget.

**Snapshot workflow.** Checking is read-only and prints the acceptance
command. Update mode prints every path it writes and is refused under CI
(`WINDTRAP_UPDATE=force` overrides). After a full, clean run every stale
baseline is reported with the exact removal beneath it — the report is
advisory and windtrap deletes nothing, because a baseline is a file in git and
removing one is an edit its author makes and reviews in `git diff` like every
other edit:

```
stale baseline: test/__snapshots__/test_mytool/removed.snap
remove them: rm 'test/__snapshots__/test_mytool/removed.snap'
```

The inline (ppx) runner reports all of this identically to the library runner.

**Expect tests.** `let%expect_test`, `[%expect]`, `[%expect_exact]`,
`[%expect.output]`, `let%test`, and `module%test`, with corrections accepted
via `dune promote`. Compatibility is measured against Jane Street's pinned
ppx_expect corpus, and the bar is *matching semantics*: an adopted suite runs
unchanged, mismatching where upstream mismatches and recording the correction
upstream's runtime records. 33/36 supported cases meet it (91.7%) — 16 pass
with no correction, 17 produce the correction — and 20/20 unsupported
constructs are rejected with a loud error at the exact location. A file with
no corrections is never rewritten, and a correction patches the stale
payload's extent rather than re-rendering the file around it.

**Coverage.** An inline percentage after the test results on instrumented
runs, and a `windtrap coverage` command that merges `.coverage` files,
gates CI with `--min`, and emits `--json`. `WINDTRAP_COVERAGE` is a switch
over that inline line — the shared boolean spellings, `on` or `off`, and
nothing else — and an invalid value is refused with exit 2 rather than
ignored. Both runners behave the same, the inline (ppx) runner included.

The in-run line and the command answer different questions, and each says
which. Every in-process number is one executable's view of the code it links,
so the inline line points at the aggregate unconditionally —
`coverage: 87.2% (312/358 points) · project: dune build @cover` — and the
per-file table, the uncovered excerpts under `-u`, and the JSON are the
command's, over the merge. A dump that describes another build is excluded
from that merge, always, and named on stderr with the remedy that heals it: a
total computed from a dump known to be stale can only mislead. The `--min`
verdict states the measurement rather than a comparison —
`minimum 80%: FAILED — 75.1% (5527/7363 points)` — because the gate compares
raw percentages and coverage that *renders* equal to the threshold can still
fail, which is what the exact fraction beside it explains. A barely-tested
file's `uncovered:` cell stops after eight regions and says
`(+N more, -u shows them)`, so no row is wider than the terminal that has to
lay it out.

Every run of an instrumented executable writes its own dump into that
executable's directory under `_build/_coverage`, so a command-line tool driven
by a cram test is measured across every invocation; the first run of a
rebuilt executable removes its predecessors' dumps, so a rebuild heals itself.

Instrumentation never changes what a program means: out-edge points are given
up wherever taking one would cost a tail call — ordinary tail position, `||` and `&&` arms of every shape, and the
constructor arguments of a `[@tail_mod_cons]` function — so an instrumented
run computes what the plain run computes, at the same stack depth. A
semantics-preservation suite holds that line.

**CI ergonomics.** `--shard K/N` deterministically partitions a suite across
jobs. Failures are emitted as GitHub Actions annotations when running under
Actions (`CI` and `GITHUB_ACTIONS` both set, as Actions sets them). Failure
reports include the tail of the test's captured output
(`WINDTRAP_TAIL_ERRORS` controls how much).

### Changed

**`float` and `float_rel` refuse degenerate tolerances — `float 0.` becomes
`float_exact`.** `float 0.` was exact equality wearing a tolerance's
syntax: any `eps` at or below zero (NaN included) reduces
`|a -. b| <= eps` to the `a = b` shortcut, so the witness compared exactly
while the call site read as approximate — and, because NaN is equal to
nothing under the tolerance semantics, it was a *worse* exactness than
`float_exact`, unable to assert a NaN result. Migrations write it by the
hundred, one `float 0.` per hand-ported epsilon, and every one is an
assertion whose spelling misstates its strength. Both constructors now raise
`Invalid_argument` at construction, naming the honest spelling: `float eps`
unless `eps` is strictly positive, `float_rel ~rel ~abs` when either bound
is negative or NaN, or when both are zero. One zero bound in `float_rel`
stays legal — `~abs:0.` is a purely relative tolerance, `~rel:0.` a purely
absolute one — because a component switched off is a real configuration
where a tolerance of nothing at all is not.

**Migration: `float 0.` becomes `float_exact`; `float_rel ~rel:0. ~abs:0.`
becomes `float_exact` too.** A test that meant exactness now says so — and
gains the ability to assert NaN, which `float 0.` never had. A test that
meant a tolerance now has to state one, which is the assertion it was
silently not making.

**`WINDTRAP_UPDATE` now covers expect payloads, and the correction notice
stopped lying about promotion.** Under dune's `inline_tests` protocol every
partition of a library runs inside one action, and the per-file
`(diff? src src.corrected)` steps run only after every partition exits 0. So
a raise in `b.ml` withholds `a.ml`'s correction: the correction is computed,
its `windtrap: wrote a.ml.corrected` line prints, the sandbox is then thrown
away with the file in it, and `dune promote` has nothing to offer. No exit
code can fix this — a crashing partition that exits 0 to let the diffs run
makes the crash itself promotable, which is the one thing the promotion rule
exists to forbid.

So expect corrections stop depending on dune's channel alone. `WINDTRAP_UPDATE`
already means "accept the output I just produced as the new expectation", and
it was arbitrary that it covered snapshot baselines but not expect payloads —
the same act, on a different file. Under `WINDTRAP_UPDATE=1` a correction is
now written into the source tree directly, per file, through the machinery
baselines already use: the project root resolved above any `_build` tree, the
target proved to lie under it, an atomic write. The mismatch is reported as
accepted rather than failed, so the run goes green and names what it wrote,
and one file's correction no longer depends on another file's crash. This is
a unification, not a new concept: users who already set the variable will find
it also updates expect payloads.

Crashes remain non-promotable everywhere. A raise is never a correction, on
either channel, so no update run can bless one — a crashing partition still
exits 1 having written nothing. And acceptance is gated on the process's own
clean verdict: a partition holding a non-expect failure — an assertion beside
a stale payload, a crash in a sibling test of the same file — accepts nothing
under `WINDTRAP_UPDATE` either, so what the variable removes is exactly the
*cross-file* veto and never the per-file one the masked-assertion rule
exists for. Before overwriting, the source-tree file's
bytes are compared with the sandbox copy the correction's offsets were
computed against, and *any* difference is refused loudly with the file left
untouched: those offsets describe one file, and splicing them into another
corrupts it. Under CI an update request is refused exactly as it is for
baselines, `force` included.

The notice that was supposed to explain all this had the defect too. Its
"corrections written but NOT registered for promotion" line printed only when
the writing process itself exited nonzero — but the withholding is the whole
library's, and no partition can see a sibling's exit code. In the
cross-partition case that motivated the warning, the process that wrote the
correction exits 0 and the process that exits 1 wrote nothing, so it stayed
silent in exactly the case it was written for. Every process that writes a
correction now prints the caveat, and it names both ways out.

**A correction patches the payload it corrects, not the file it lives in.**
Adopting a ppx_expect suite promises that your corrections come out the way
ppx_expect writes them, and we first measured that against Jane Street's
`.corrected.expected` goldens — which are the output of *two* stages, not one:
ppx_expect's runtime writes payload-only patches, and the monorepo's build
then runs `bin/apply-style` over the corrected file to standardize every
expect node in it. That second stage is not part of ppx_expect, is absent from
the pinned checkout, and runs in no windtrap user's build. Emulating it bought
parity with a pipeline nobody runs and charged for it in the one place that
matters: a single stale payload re-rendered *every* node of its file, so the
first promote after an adoption reformatted whole files, and any later promote
could bury a real change in canonicalization — exactly the review hazard
`SKILL.md` warns about when it says to read a promoted diff as a code change.

The writer patches payload extents. A node's head stays on the line its author
put it on; a matching node beside a corrected one keeps its bytes; an overlong
quoted payload is written on one line rather than continuation-wrapped at
ocamlformat's 90-column margin. Two shapes have no payload literal of their
own and are still rewritten whole: a bare `[%expect]`, which materializes its
payload, and the `{%expect|…|}` shorthand, whose literal spans the node —
retagging it keeps the extension id. Payload re-indentation is unchanged, so a
suite whose payloads already carry ppx_expect's shape promotes without churn,
and a file with no corrections is never rewritten. The conformance bar is
scoped to matching semantics for the same reason: eight of the fifteen
vendored corrected goldens in `test/conformance` are byte-identical to the
upstream bytes anyway, and the seven that are not differ only where the style
pass used to reach.

**A promoted body that already had parentheses may gain a redundant pair.**
When a trailing correction appends `;` to a body that is a bare `match`, `try`
or `function`, the rewriter parenthesizes the body in the same patch, or the
`;` would bind to the last arm. It does not read the source file to notice a
body that brought its own parentheses — nothing in the AST distinguishes
`(match …)` from `match …` — so the extra pair is emitted and ocamlformat
removes it. It cannot accumulate: once the trailing node exists there is no
further trailing output to insert.

**The inline runner speaks dune's protocol and nothing else.**
`-source-tree-root` and `-diff-cmd` are gone from the `inline_tests` backend's
flags and from the runtime's parser — windtrap passed both from its own stanza
and then discarded the values, since the root is found via
`WINDTRAP_PROJECT_ROOT`/the project-root walk and the diff step is dune's. The
`inline-test=drop` build cookie goes with them: that is ppx_inline_test's
Jenga-era spelling, dune's binary contains neither it nor `drop_with_deadcode`,
and every drop mode a dune build can reach — a library with
`(inline_tests disabled)`, a profile that disables them — arrives through the
`inline_tests` cookie, which is unchanged. A hand-written harness still
passing the retired flags is unaffected: the parser's catch-all absorbs
unknown arguments. Generated code is unaffected too — the PPX emits none of
this.

**Failure blocks show control bytes instead of executing them.** A value
carrying an ESC byte used to reach the terminal intact, so a failing
assertion on styled output drew its own colours over the report, ate the
label beside it, and left nothing a `grep` for the reported bytes could
find — the failure that most needed reading was the one you could not read.
Every surface that prints compared data — both equality paths, the
containment excerpt, the predicate value, the rendered exceptions, snapshot
baselines and proposed content, the counterexample — now renders each C0 byte
and DEL as `\x1b`, `\x00`, `\x0d`, keeping newlines and tabs, which are the
block's own layout:

```
    expected  \x1b[31mred\x1b[0m
                    ~
    actual    \x1b[32mred\x1b[0m
                    ~
```

Only the rendering changes. Equality still compares bytes, snapshots still
store and accept them, and the `~~~` marks still come from a refinement made
against the raw values — moved into the escaped columns so a mark covers the
whole escape it opened. The failure block's captured-output tail is
deliberately untouched: it is a log excerpt, and it names the full log's path
for anything the terminal mangles. JUnit and GitHub bodies inherit the change
through the same projection, where the bytes previously survived only as
stripped remains.

**`windtrap.clock` is folded into the core.** The sublibrary had no dependent
of its own — only the runner ever linked it — so the monotonic-clock module
and its C stub now live in the core library and the public name is gone.
Nothing user-visible changes unless a stanza linked `windtrap.clock` directly,
in which case: delete that line.

**A printerless counterexample says `<no printer>`, and names its remedy
once.** A generator built with `map` or `bind` carries no printer — none can
be inferred for the result type — so its counterexample has nothing of its own
to render. It renders `<no printer>`, in every printerless shape including
`~examples` values, rather than reconstructing a `<from: ("a", 90)>`
provenance string from the generation path; and the advice that used to be
buried inside such a rendering lives in one place, a line under the
counterexample naming `Gen.with_pp`.

**Backtraces stop at your code.** Under every backtrace windtrap records sit
its own frames — the callback delimiter, the attempt guard, the verb that
raised. They name none of your code and they were the majority of a short one:
a `raises` failure printed two lines, half of it `Windtrap__Check.raises`, and
an uncaught exception printed five, three of them machinery. The trailing run
of windtrap frames is now dropped, in the one place a raw backtrace becomes
report text, so the terminal, JUnit and GitHub reports agree. Only a trailing
run: a callback windtrap invoked keeps both itself and the frames below it, and
a backtrace that never crossed your code is kept whole rather than emptied.

**An unknown flag suggests the near miss.** `--fliter` now answers
`unknown option '--fliter'; did you mean '--filter'?`. Transpositions count as
one edit, since they are the typo people make; short flags get no suggestion,
because any two of them are one edit apart and a confident wrong suggestion is
worse than none.

**An empty selection says why it is empty.** `no tests ran.` explained exit
code 2 and nothing else, while the overwhelmingly common cause is a mistyped
filter. It now names what narrowed the run and how many tests there were to
narrow — `no tests ran: filter "parsr" matched none of 48 tests.` — and
points at `-l`. A suite that declares nothing says that instead, and a shard that drew
an empty bucket names the shard, so neither reads as a typo. `-l`/`--list`
gives the same answer over the same paths in the same order, so
`-l -f typo` prints `no tests ran: filter "typo" matched none of 17 tests.`
where it used to print nothing at all. Exit codes are unchanged.

**JUnit reports survive `dune runtest`.** `WINDTRAP_JUNIT` named one file, but
`dune runtest` starts a process per `(test)` stanza and per inline-test library
— so suites silently overwrote each other's report, and inline partitions
dropped it entirely. A target ending in `.xml` is still that exact file, for
the single-process invocations `--junit` was written for; anything else is a
directory, and every suite writes `<dir>/<suite>.xml` into it, inline
partitions included. Point CI at `_build/junit/*.xml`. The document is written
by the shared reporting spine rather than by each runner, so the library
runner and the inline one cannot drift in when or whether a report appears —
and a mutation loop's dry run writes none, because a mutation run's output
never reports a test outcome.

**One meaning for green, on both diff paths.** The unified-diff path coloured
`- expected` red and `+ actual` green — the diff tool's convention, and the
inverse of what every other block does and of what this release promises above
("green is the expected side and red the actual one, on every block that shows
both"). A transcript shows both paths routinely, so green meant "expected" on a
one-line failure and "actual" three lines later; adopting `text` makes a reader
hit the pair constantly, which is what surfaced it. The colours are swapped;
the `--- expected` / `+++ actual` header and the `-`/`+` sigils carry the diff
convention on their own, which is why they can.

**No empty ANSI spans.** Styling an empty string emitted an open code and its
reset with nothing between them, so every styled `FAIL` header carried a stray
`ESC[2mESC[0m` from its empty attempt suffix. Invisible on a terminal, but real
bytes for anything diffing or parsing a transcript.

**The default output is compact, and a green run is one line.** The header
and the per-test glyph row (green `.` pass, red `F` fail, yellow `S` skip,
faint `x` expected failure; rows wrap at 60 with a `[k/n]` counter) print
only when the run is noteworthy — any failure, or any test not tagged
`slow` exceeding the slow threshold. A green, healthy run is exactly one
named line (`mylib: 48 passed in 1.2s.`, with the root seed appended when
the suite declares properties). A noteworthy run flushes the header and the
buffered row at the first noteworthy event, streams the rest glyph by
glyph, and replays the failures in full at the end. `-v` / `--verbose`
restores the line-per-test transcript and keeps the slowest-tests list, now
verbose-only; a passing property with collected labels prints its label
distribution there. On a terminal, a faint erasable `[k/n] current-test…`
tail runs from the start, so a hung test names itself before anything is
committed; piped output has the same shape, flushed per glyph once
noteworthy. The report is 80 columns wide either way, always: a transcript
is a report, not a canvas, and one width keeps a pipe and a wide terminal
byte-identical.

**Slow tests announce themselves.** An untagged test exceeding the slow
threshold puts the run in a faint-yellow `slow tests (n):` block between
the failure blocks and the summary — slowest first, each entry indented
with the duration in a right-aligned leading column, then one hint line
naming the opt-outs: the `slow` tag, or `--slow-threshold SECONDS`
(`WINDTRAP_SLOW_THRESHOLD` mirror; default 1, `0` disables the warnings
and the noteworthy trigger). Tests tagged `slow` are exempt everywhere.

**Focus is refused under CI.** `ftest`/`fgroup` are debugging tools: when
`CI` is set, a run containing focused tests refuses to start, so a committed
`ftest` can never quietly shrink a CI run to one test, and the refusal names
the one remedy there is — remove the `ftest`/`fgroup`. There is no override
from the environment: the workflow that sounds like one, running a deliberate
subset under CI, is spelled `-f`, `--tag` or `WINDTRAP_FILTER`, none of which
requires committing an `ftest`. Outside CI, a successful focused run prints a
warning. 0.1.0 only warned.

**The failure region is separated and the summary has room.** Failure
blocks are separated by a blank line, and so are a single test's failures
when it has several — sibling subtests, or a body and its teardown, which
report independently. The closing rule, the slow block, and the summary
each get their own space, so the verdict line is findable. Report paths
are project-relative like the location lines above them, rather than
absolute: a snapshot baseline prints
`test/__snapshots__/test_mytool/usage.snap`, and a capture log keeps
its `_build/_tests/…` prefix so the path still opens.

**No run advertises `--failed`.** The old `rerun failures only: …` line
under every failing run was an optimization hint, not a step; the summary
is the last line now. Acceptance commands still print under every
snapshot mismatch — those name a verb nobody can guess.

**Floats print the value that failed.** Counterexamples and bit-exact
witnesses render floats with the shortest decimal that round-trips to the
same double, so `0.1 +. 0.2` reports `0.30000000000000004` rather than
`0.3`. A counterexample exists to be pasted back into `~examples`; one that
does not round-trip names a value the test never saw.

**A fixture release failure is reported and counted.** A `teardown` that
raises during release appears as a `fixture release` entry in the failure
section, in the JUnit document, and in the GitHub annotations, alongside the
exit code it already set. Releases run after the last test, so the entry
sits outside the declared suite — a JUnit consumer sees one more testcase
than the suite declares.

**The per-test timeout covers teardown even after the body times out.** The
window is re-armed before teardown with whatever remains of the limit, or a
fresh one when setup and body consumed it: cleanup still has to happen, so
it gets a bounded window rather than none.

**Capture logs have one stable path per test.** A test's captured output
lives at `<log_dir>/<suite>/<groups…>/<test>.output` — a function of the
test's identity, the same on every run — so a path a failure report prints is
a path an editor can keep open across reruns, and a rerun overwrites the
previous run's logs the way a retry already did between attempts. The
name-to-filename mapping replaces anything outside `[A-Za-z0-9._-]`, which is
many-to-one, so an altered component carries a short digest of the original;
unaltered names are untouched. Snapshot baselines are keyed by the name as
written and are unaffected.

**`-o DIR` is resolved once, at startup.** A relative log directory used to
follow the process, so a test that changed directory sent the rest of the
run's logs elsewhere.

**`--verbose` gains an environment mirror** (`WINDTRAP_VERBOSE`), making the
line-per-test transcript reachable under `dune runtest`, where the mirrors
*are* the CLI.

### Fixed

**A shrink search stopped by a raising candidate no longer reports as
converged.** Forcing a shrink candidate can raise — a `Gen.map`'s function, a
repair mask — and the memoized cell caches the exception, so the siblings
behind it are unreachable. The engine read that as "no candidate accepted" and
reported a truncated search as a minimal counterexample. It now stops the way
the step budget stops it, and says so in the same words; the step count is
what tells the two apart, since a count at the budget spent it and a count
below it did not.

**The mutate backend no longer breaks the user's own type-directed record
disambiguation.** The `ari` and `cmp` guards lift the operator's two
operands out of the application, and lifted them as a chain of `let`s bound
right to left — the order the compiler *evaluates* the application in, but
the reverse of the order the type-checker *reads* it in. An expression
whose first, qualified access teaches the checker a record's type and whose
later fields lean on the lesson — `rect.Layout.x - x0 … rect.height`, an
ordinary shape in real code — stopped compiling under
`--instrument-with ppx_windtrap.mutate` with "Unbound record field": the
guard asked about `rect.height` before anything had said what `rect` is.
(An entry below fixes the mirror image — PPX-*generated* code relying on
the user's scope; this was the PPX un-typing the user's, which is strictly
worse.) The operands are now lifted through one tuple binding,
`let (l, r) = (left, right)`, whose components the checker reads left to
right — the source's order — and whose literal tuple the compiler never
builds: it destructures into the very right-operand-first `let` chain
emitted before, so evaluation order, operand count, and allocation are
unchanged, and every mutant keeps its identifier.

**A written correction is announced, and a withheld one is explained.** Every
inline-test process that writes a `.corrected` file names it on stderr
(`windtrap: wrote <file>.corrected`), and a process that wrote corrections
and still fails adds a loud notice explaining that dune withholds every
correction in the library from `dune promote` until the failure is fixed and
the suite rerun.

**A correction that cannot be written is no longer dropped silently.** When
the source is unreadable or the target unwritable, the runner prints
`Error: correction for <file> not written: <reason>` and the partition exits
1 — so a failed expect test can never be recorded as passed just because its
correction never reached disk.

**An uncaught exception in a `let%expect_test` body is an ordinary test
failure, never a correction.** The former behavior spliced an unreachable
`[%expect]` node after the raising statement, which broke the build on
promote (warning 21) or duplicated the node on every runtest+promote cycle.
Inline runs with a raising expect test now exit 1; to pin an expected
exception, catch and print it in the body.

**PPX-generated code no longer relies on type-directed record
disambiguation**, so inline-test libraries build clean under strict warning
sets (`(flags (:standard -w +a -warn-error +a))`) — adopting projects need
no `-w -42` workaround in their `dune` files.

**`Stdlib.exit` from a test can no longer kill the run.** Called from a body,
setup, teardown, or fixture release, the attempt is intercepted and recorded
as that test's (or that release's) failure; every later test still runs, and
the run exits through its own 0/1/2 contract. The runner owns the process
exit — but only in the process that started the run: a test that forks and
calls `exit` in the child terminates the child, which is what a test
spawning subprocesses expects.

**An assertion failure beside a stale `[%expect]` payload is not a
promotable correction.** An inline body that both fails an assertion and
leaves stale output exits nonzero, so dune withholds the library's
corrections; otherwise `dune promote` would bless output the assertion had
already rejected, and the real regression would surface a cycle later.

**A trailing `[%expect]` inserted after a body that ends in a `match`,
`try` or `function` lands after the body**, not inside its last arm: the
`;` the correction appends would otherwise bind to the last arm, where the
node runs on one branch only and the next run appends another beside it.
The body is parenthesized as part of the same correction, so the promoted
file means what the correction intended and converges on the next run. The
test is on the body's tail, not its head, so the common `let … in match …`
and `stmt; match …` shapes are covered too: the walk follows the tail
through `let`, `;`, `if`/`else`, `open`, `let module`, `let exception`,
`let*`, type annotations and a `fun`'s body.

**A missing snapshot baseline now fails**, with the proposed content and the
acceptance command. 0.1.0 silently created the baseline and passed.

**Reading captured output under `--stream` now fails** with "this test
requires capture". 0.1.0's expect tests silently compared against the empty
string and passed.

**A raising setup or teardown is an ordinary reported outcome.** 0.1.0 ran
group hooks outside the failure boundary, where an exception could take down
the runner; `bracket` now reports body and teardown failures independently.

**Stopping early with `--bail`/`-x` no longer skips cleanup.** Teardowns run
on every outcome, and acquired fixtures are released on every path where the
runner regains control.

**JUnit XML no longer contains ANSI escape sequences.**

## [0.1.0] - 2026-02-13

Windtrap is an all-in-one OCaml testing framework that unifies unit tests,
property-based tests, snapshot tests, and expect tests under a single API.
Instead of juggling multiple testing libraries, Windtrap gives you one
cohesive package with a PPX for inline expect tests (`ppx_windtrap`).

- Unit tests with combinators, tags, skip, brackets, and timeouts.
- Property-based testing with configurable seeds and shrinking.
- Snapshot testing with automatic file management and diffing.
- Inline expect tests via `ppx_windtrap` with automatic correction.
- CLI test runner with filtering, verbosity, and color support.
- Test coverage reporting with `bisect_ppx` integration.

### Acknowledgments

Windtrap builds on ideas and code from several OCaml projects:

- **[Alcotest](https://github.com/mirage/alcotest)** by Thomas Gazagnaire — test structure and runner design
- **Craig Ferguson's Alcotest PRs** ([#294](https://github.com/mirage/alcotest/pull/294), [#247](https://github.com/mirage/alcotest/pull/247)) — API design, subcomponent diffing, and Levenshtein distance (ISC)
- **[QCheck2](https://github.com/c-cube/qcheck)** by Simon Cruanes et al. — generator design and integrated shrinking (BSD 2-Clause)
- **[ppx_expect](https://github.com/janestreet/ppx_expect)** and **[ppx_inline_test](https://github.com/janestreet/ppx_inline_test)** by Jane Street — expect test paradigm and dune integration
- **[Bisect_ppx](https://github.com/aantron/bisect_ppx)** by Anton Bachin et al. — coverage instrumentation and runtime (MIT)
