# Changelog

All notable user-facing changes are documented here — features, fixes,
performance, and anything that changes the public surface. That surface is
`lib/windtrap.mli`, the CLI and `WINDTRAP_*` contract, and the runner's exit
codes. New entries go at the top of their section.

## [0.2.0] - unreleased

Windtrap 0.2.0 is a ground-up rewrite around three commitments: **declaring
a test is pure data**, **a failure shows you the values**, and **anything
random replays**. Everything below is relative to 0.1.0. The cheat sheet
is the migration reference — a 0.1 suite ported row by row compiles and
runs — and the bullets that follow say what each area does now and why.

### Migrating from 0.1: the cheat sheet

Declaring tests:

| 0.1.0 | 0.2.0 |
| --- | --- |
| `let () = run "mylib" tests` | `let () = exit @@ run "mylib" tests` — `run` returns the exit code |
| `run ~quick ~filter ~seed …` | the CLI flags and `WINDTRAP_*` mirrors, or `run ~argv` |
| `ftest "x" fn` / `fgroup "g" ts` | `focus (test "x" fn)` / `focus (group "g" ts)` |
| `~tags:(Tag.labels [ "net" ])` | `~tags:[ "net" ]` |
| `Tag.speed Slow` | the `slow` constructor, or `~tags:[ "slow" ]` |
| `~tags:(Tag.labels [ "disabled" ])` | `skip ()` in the body, or `--exclude-tag` on the command line |
| `group ~before_each ~after_each` | `bracket ~setup ~teardown` on each test |
| `group ~setup ~teardown` | `fixture ?teardown create` |
| sixteen `~timeout:3.` in one group | `group ~timeout:3. "integration" [ … ]` |
| `cases ty inputs name fn` | `cases ~name:string_of_int name inputs fn` |
| `?here:[%here]` / `?pos:__POS__` | `~__POS__`, or nothing |
| `let helper ?pos x = equal ?pos …` | `let helper ?__POS__ x = equal ?__POS__ …` |
| a body returning a value | a body returning `unit` — end with an assertion, or `ignore` |

Assertions:

| 0.1.0 | 0.2.0 |
| --- | --- |
| `testable ~pp ()` | `Testable.structural ~pp` |
| `testable ~pp ~equal ()` | `Testable.make ~pp ~equal` |
| `of_equal eq` / `contramap f t` | `Testable.of_equal eq` / `Testable.contramap f t` |
| `seq t` / `lazy_t t` | `Testable.contramap List.of_seq (list t)` / `Testable.contramap Lazy.force t` |
| `nat` / `small_int` testables | `int`; the distributions are `Gen.nat` / `Gen.small_int` |
| `float 0.` / `float_rel ~rel:0. ~abs:0.` | `float_exact` (a zero tolerance is refused) |
| `is_ok r; Result.get_ok r` | `require_ok r` |
| `some t e v` | `equal (option t) (Some e) v` |
| `ok t e r` / `error t e r` | `equal t e (require_ok r)` / `equal t e (require_error r)` |
| `no_raise fn` | `fn ()` |
| `raises_invalid_arg "m" fn` | `raises (Invalid_argument "m") fn` |
| `raises_failure "m" fn` | `raises (Failure "m") fn` |
| an any-message or substring check | `raises_match Exn.invalid_arg fn`, `raises_match (Exn.failure ~substring:"…") fn` |
| `is_true (a < b)` | `less int ~than:b a` (and `at_most`, `greater`, `at_least`) |
| `is_true (String.starts_with ~prefix s)` | `starts_with ~affix:prefix s` |
| `is_true (List.mem x xs)` | `mem int x xs` |

Property testing:

| 0.1.0 | 0.2.0 |
| --- | --- |
| `prop name (list int) law` (a `bool` law) | `prop name Gen.(list int) (fun l -> equal (list int) l (law l))` |
| `prop'` | `prop` (it is assertion-style now) |
| `prop2` / `prop3` / `prop4` | `prop` with `Gen.pair` / `Gen.triple` / `Gen.quad` |
| `~config` on a property | `~count`, `~examples` and `~max_discard` |
| `~gen` on a testable | a `Gen.t` argument; printers attach with `Gen.with_pp` |
| `Gen.oneofl` / `Gen.oneof` | `Gen.of_list` / `Gen.one_of` |
| `Gen.list_size sg g` | `Gen.list ~size:sg g` |
| `Gen.string_size sg cg` | `Gen.string_of ~size:sg cg` |
| `Gen.sized f` | `Gen.bind Gen.nat f` |
| `Gen.pure v` | `Gen.constant v` |
| `cover ~label:"even" ~at_least:20. c` | `cover "even" c` |

Baselines and expect tests:

| 0.1.0 | 0.2.0 |
| --- | --- |
| `snapshot ~pos:__POS__ s` / `snapshot ~name s` | `expect s @@ __POS_OF__ {|…|}` — the baseline is the literal at the call |
| `snapshotf fmt …` | `expect (Printf.sprintf fmt …) @@ __POS_OF__ {|…|}` |
| `snapshot_pp pp v` | `expect (Format.asprintf "%a" pp v) @@ __POS_OF__ {|…|}` |
| a baseline other tests read, or too big for a literal | `expect_file s "test/name.expected"` — a file at the path the call names |
| `expect s` / `capture fn s` | `expect (output ()) @@ __POS_OF__ {|…|}` (the name survives, the signature does not), or `let%expect_test` |
| `__snapshots__/<file>/<name>.snap` | gone: no baseline is derived from a test's name; a `.snap` worth keeping becomes an `.expected` file named in the call |
| `(deps (glob_files_rec __snapshots__/**))` | `(deps help.expected)` — the files the test reads, named |
| `WINDTRAP_UPDATE=1 dune runtest` | a `(test)` stanza whose action runs `%{test} --corrected` and `diff?`s each corrected file, then `dune promote`; under CI that is the only acceptance (`-u` is refused there, with no override) |
| `run ~update ~snapshot_dir` | gone: `-u` on the command line; a path is the call's |
| `WINDTRAP_SNAPSHOT_DIR`, `WINDTRAP_SNAPSHOT_DIFF_CONTEXT`, `WINDTRAP_SNAPSHOT_MAX_BYTES`, `WINDTRAP_SNAPSHOT_REPORT` | gone |

Running tests:

| 0.1.0 | 0.2.0 |
| --- | --- |
| `--format` / `WINDTRAP_FORMAT` | default ⊂ `-v`; TAP consumers move to `--junit PATH` |
| `-q` / `--quick` | `--exclude-tag slow` |
| `--bail` | `-x` / `--fail-fast` |
| `--seed 42` | `--seed s1:<16 hex>`, the token the run header prints |
| `WINDTRAP_TAIL_ERRORS` / `WINDTRAP_COLUMNS` | gone: ten lines of tail, 80 columns, always |
| `--junit PATH`, one file, no mirror | `--junit PATH`, or `WINDTRAP_JUNIT=_build/junit` — a directory, one file per suite |

Coverage and packaging:

| 0.1.0 | 0.2.0 |
| --- | --- |
| `(instrumentation (backend ppx_windtrap))` | `(instrumentation (backend ppx_windtrap.coverage))` |
| `--instrument-with ppx_windtrap` | `--instrument-with ppx_windtrap.coverage` |
| `windtrap coverage --summary-only` | `windtrap coverage` (per-file is the default report) |
| `windtrap coverage -C N` / `--context N` / `--skip-covered` | removed |
| `windtrap coverage --coverage-path P` / `--source-path P` | positional `PATH…` |
| `windtrap coverage -j` | `--json` (long form only) |
| `WINDTRAP_COVERAGE_LOG` | gone |
| `windtrap.clock` in `(libraries …)` | delete the line: the clock is in the core |
| `open Windtrap` bringing `Tag`, `Pp` and `Ppx_runtime` | `Testable`, `Gen`, `Exn` and `Private` (unstable internals) are the only modules it brings |

### Declaring tests

- **`run` returns the exit code.** `run : ?argv:string array -> string ->
  test list -> int` is the code every path used to exit with — `--help`
  and `--version` (0), a command line it cannot parse or resolve (2),
  `-l` (0), a startup refusal, a mutation loop's verdict, and the run's
  own 0/1/2 — and `main` applies it: `let () = exit @@ run "mylib" […]`.
  One binary can host two suites, a harness can post-process a run
  in-process, and a `main` that forgets the `exit` is a type error
  rather than a binary that is green on failure. `run` refuses to start
  inside an active run, and the programmatic knobs of 0.1 (`~quick`,
  `~filter`, `~seed`, `~format`, `~junit`, `~update`, `~snapshot_dir`)
  are gone: the flags and their mirrors are the configuration, or a
  synthetic `~argv`.
- **Every constructor takes `?tags`, `?timeout` and `?retries`** —
  `test`, `slow`, `group`, `cases`, `bracket`, `scoped`; `prop` and
  `stateful` take no `?retries` (they replay from the seed). On a group
  they are defaults for every test under it, the innermost declaration
  winning, so a limit for an integration group is one argument rather
  than one per test. `~tags` extend every descendant's as before.
- **`focus` and `xfail` are the two combinators.** `focus (test "x" fn)`,
  `focus (group "g" ts)`, and just as well `focus (cases …)` or
  `focus (with_db "x" fn)`, which `ftest`/`fgroup` could not spell. Under
  `CI` a run containing focused tests refuses to start, naming the
  focus site; outside CI a successful focused run prints a warning.
  `xfail ?reason t` keeps a known-bug reproduction in-tree: a failure
  reports as `XFAIL` without failing the run, and a pass fails loudly.
  Nested `xfail`s resolve innermost-wins.
- **`?__POS__` is the explicit position** on every verb, every
  constructor and `command`/`call`: the label puns with the builtin, so
  `equal ~__POS__ int 5 x` is the whole spelling, and a helper threads it
  through as `?__POS__`. The automatic location comes from the call
  stack (`-g`, dune's default); an assertion in tail position has no
  frame left, so the report names the test's declaration line, and
  `~__POS__` on the assertion gives its exact line. `?here` is gone.
- **Bodies return `unit`.** 0.1 accepted `unit -> 'a` and silently
  dropped the result.
- **Tags are plain strings** and the `Tag` module is gone. The
  Quick/Slow pair is the `"slow"` tag (`slow name fn` pre-applies it, and
  it exempts a test from the slow-test warning); `"disabled"` no longer
  deselects a test silently — `skip ()` in the body is reported and
  counted, `--exclude-tag` on the command line is echoed by the header.
  Property tests carry `"prop"`, stateful tests `"prop"` and
  `"stateful"`.
- **`cases` requires `~name` and takes no witness**: `cases "ports"
  ~name:string_of_int [ 1; 80; 8080 ] fn` declares one selectable child
  per input, named from its value. There is no numbered default because a
  child's path is its identity — it keys per-case seeds and the
  `--failed` store — and a row inserted at the front would silently re-key
  every row behind it.
- **Group hooks are gone**, so no user code runs outside a test's
  exception boundary: `bracket ~setup ~teardown name fn` scopes a
  resource to one test (teardown iff setup succeeded, on failure, skip
  and timeout alike; body and teardown failures reported as two entries),
  and `fixture ?teardown create` shares one across the run, acquired on
  first use inside that test's boundary and released by the runner after
  the last test, on every path where it regains control, `-x` included.
  A release failure is a counted `fixture release` entry. 0.1's
  `fixture` was a plain lazy cache.
- **`scoped scope name fn`** takes a `with_`-style scoping function
  whole (`Eio_main.run`, `In_channel.with_open_text path`,
  `Pool.with_connection`) and calls it once with a callback that runs the
  body; a scope that never calls back, or calls back twice, fails the
  test, and the body's failure is re-raised through the scope so its
  cleanup runs. `bracket` is `scoped` over the scope its `~setup` and
  `~teardown` write, so the two share one protocol; a fatal exception
  (`Sys.Break`, `Out_of_memory`) skips the teardown and
  ends the run.

### Assertions

- **`Testable.make` requires `~equal`**, and `~gen`/`~check` are gone:
  generation lives in `Gen`, and every diff is computed from printed
  values, so every type gets a highlighted diff from its printer alone.
  `Testable.structural ~pp` uses `( = )`; `of_equal` and `contramap` move
  behind `Testable.` — witnesses flat, constructors in `Testable`.
- **Ordering verbs: `less`, `at_most`, `greater`, `at_least`**, each
  taking a witness, the bound as `~than` and the value last; the failure
  keeps both (`expected less than 3 / actual 5`). The order is the
  witness's: base types carry their module's, the float witnesses order
  with `Float.compare` (tolerance belongs to equality),
  `Testable.structural` carries `Stdlib.compare`, `contramap` orders
  through its projection, and `Testable.with_compare` gives any other
  witness one — an ordering verb over a witness without an order raises
  `Invalid_argument` naming it.
- **`text`**, a string witness that prints verbatim: multi-line values
  diff line by line instead of drowning in `%S` escapes. Equality is
  unchanged, byte for byte.
- **`float_exact`** is a bit-exact float witness — every NaN equal to
  every NaN, `0.` and `-0.` distinct — and `float` and `float_rel` refuse
  degenerate tolerances at construction (`float eps` with `eps <= 0`,
  `float_rel` with a negative or NaN bound, or both bounds zero), naming
  the honest spelling.
- **`require_some`, `require_ok`, `require_error`, `require_match`**
  assert a shape and unwrap it, so the happy path keeps its value;
  `require_ok`/`require_error`/`require_match` take `?pp` for the
  rejected branch. `is_some`, `is_none`, `is_ok` and `is_error` keep
  asserting the shape alone; `is_none`, `is_ok` and `is_error` take the
  same `?pp`. The wrapper witnesses `some`, `ok` and `error` go through
  plain composites.
- **`satisfies ?claim ?msg t pred v`** renders the rejected value;
  `~claim` replaces the expected side's sentence. **`contains`,
  `not_contains`, `starts_with`, `ends_with`** print the needle with its
  verdict over a bounded excerpt of the haystack (an absent needle's
  excerpt is cut to ten lines and 1 KiB; a found occurrence keeps its
  window), and **`in_order ~subs`** asserts a chain of substrings, naming
  on a break the element, the byte the search had reached, and whether the
  element was present but too early. The block reads `needle  "<n>":
  <verdict>` over `haystack  <excerpt>`, the occurrence bold red in the
  haystack in color, and marked by a `~` line under its line without.
  **`mem t x xs`** is membership through a witness.
- **`raises exn fn`** asserts a structurally equal exception and reports
  a wrong `Invalid_argument`, `Failure` or `Sys_error` message as a
  message diff; **`raises_match pred fn`** takes a predicate, and `Exn`
  holds `invalid_arg`, `failure` and `sys_error`, each with `?substring`.
- **Failure reports mark what changed**: a unified diff on multi-line
  renderings, a minimal edit script over code points on short ones. In
  color the changed span is bold in its side's color inside an otherwise
  plain value and no `~` line prints (a changed span of spaces, which
  color cannot show, keeps its `~` line); without color a `~` line marks
  each side that has a changed span: both for a replacement, `actual` for
  an insertion, `expected` for a deletion. A mark that would cover half a
  side is dropped, and each value then prints whole in its side's color;
  a `~` line that a tab or a wide character would misalign is dropped
  too. Green is the expected side and red the actual one on every block
  that shows both, the unified-diff path included, and the summary counts
  wear the same colors.
- In a diff, a `-`/`+` pair that differs only in trailing spaces or tabs
  has a `~` line under the `-` line, which keeps its bytes.
- A block is bounded: a single-line value over 800 bytes prints its first
  and last 400 around `… (N bytes elided)` and draws no mark, a backtrace
  prints ten frames then `… (+N more frames)`, a diff 200 lines then `…
  (+N more diff lines)`.
- **Control bytes are shown, not executed**: every line the runner
  prints (compared values, messages, `fail` text, backtraces, test and
  suite names, captured output, paths) renders C0 bytes and DEL as
  `\x1b`, `\x0d`, keeping tabs and splitting a text at its newlines,
  with colour on or off, so a failing assertion on styled output can be
  read and grepped, a test's escape sequence never restyles the terminal,
  and a progress bar's CR no longer overwrites captured lines
  (`downloading 10%\x0ddownloading 50%`). A test name holding a newline
  prints `first\x0ahalf`, in `-l` too, and in an annotation's title.
  Floats render with the shortest decimal that round-trips, so `0.1 +.
  0.2` reports `0.30000000000000004`.
- **Backtraces stop at your code**: the trailing run of windtrap's own
  frames is dropped in the one place a raw backtrace becomes report text,
  so the terminal, JUnit and GitHub agree. An uncaught exception's report
  carries its backtrace without `OCAMLRUNPARAM=b`.

### Property testing

- **Properties are one verb.** `prop ?count ?max_discard ?examples name
  gen body` draws from an `'a Gen.t` and runs an assertion-style body;
  shrinking is always on, integrated and constraint-preserving, so a
  counterexample arrives minimal with no shrink function written. The
  0.1 `Gen` surface — `fix`, `delay`, `no_shrink`, `add_shrink_invariant`,
  `make_primitive`, `find`, `ap`, `>>=`/`>|=`, `sized`, `pure`, the
  `?origin`/`?ratio` knobs — is gone; recursion is `let*` over `Gen.nat`.
  `~examples` runs pinned regressions first on every run, unshrunk;
  `~count` overrides the case budget per declaration (the flag loses);
  `~max_discard` raises the discard budget (twice the count by default)
  where a precondition is genuinely rare. A property that discards too
  much fails rather than silently testing nothing.
- **`Gen`**: `int`, `nat`, `small_int`, `int_range`, `int32`, `int64`,
  `nativeint`, `float`, `float_range`, `unit`, `bool`, `char`,
  `char_range`, `string`, `string_of ?size`, `bytes`, `bytes_of`,
  `list ?size`, `array ?size`, `option`, `result`, `either`, `pair`,
  `triple`, `quad`, `constant`, `of_list`, `one_of`, `frequency`,
  `such_that` (a fixed resample budget of 100), `map`, `bind`, the binding
  operators, and `with_pp`. Printers derive by composition: a composite
  prints exactly when its components do.
- **A counterexample built with `map` or `bind` prints its pre-image** — the
  same shape with each printerless result replaced by the input its mapping
  function received, marked `computed from …` — down to the nearest
  generator that prints; shrinking walks the same tree, so the pre-image
  belongs to the shrunk value. A leaf with nothing to print renders the one
  placeholder `<no printer: attach one with Gen.with_pp>`; `Gen.with_pp`
  always wins.
- **Deterministic seeds.** Every generated value derives from the run's
  root seed, the test's path and the case index; the root prints as an
  `s1:` token in the header of any suite declaring properties, and every
  property failure prints the exact replay command for the way the run
  was invoked (`dune exec <path> -- --seed … -f '…'` under dune, argv0
  when run directly, `WINDTRAP_SEED=… dune runtest` for inline suites).
  Adding, removing or reordering other tests never perturbs a property's
  stream.
- **Shrinking you can see the end of.** The shrink budget is fixed at
  10,000 accepted steps — sized so no ordinary value spends it and a
  replay descends to the same node; there is no knob. A search that stops
  before converging says so under the counterexample: at the budget,
  `shrinking stopped after 10000 steps; counterexample may not be
  minimal`; when forcing a candidate raised, `shrinking stopped after 3
  steps: a candidate raised Not_found` over `counterexample may not be
  minimal`. The per-test timeout (`~timeout`, or
  `--timeout`) bounds the whole property, generation and shrinking
  included, and a timeout during shrinking reports the best counterexample
  so far over `timed out after 5s while shrinking; counterexample may not
  be minimal`. A one-line message carries the same fact in its case
  (`property failed (case 4, shrunk 10000 steps, shrink limit reached):
  …`, `…, shrinking stopped): …`, `…, shrinking timed out): …`).
- The assertion that failed on the counterexample prints under `which
  failed at:`, over its location and source line, or under `which failed
  with:` when its line is unknown. A pre-image prints `computed from <p>`
  over an aside: `(the value has no printer, so this is the input that map
  and bind computed it from; attach a printer with Gen.with_pp to see the
  value)`.
- **`assume` or `reject` outside a property fails the test** with the
  message `assume or reject was called outside a property`.
- **A discard during generation discards the case**: an `assume` or a
  `reject` in a function given to `Gen.map`, `Gen.bind` or another
  combinator counts as a discard, as an exhausted `such_that` does, and a
  shrink candidate whose generation discards is skipped. windtrap's control
  exceptions print in words wherever they are printed, as in a
  counterexample printer cut by the timeout, `<printer raised windtrap
  timeout after 1.5s>`, never as a `Windtrap__Failure` name.
- **`cover label cond` is presence-only** — the property fails unless at
  least one passing case marked the label; `~at_least` and its
  `Invalid_argument`s are gone. `collect` and `classify` print the
  distribution under `-v` and in every failure block.

### Stateful testing

- **`stateful`, `command` and `call`.** A command bundles how to draw its
  argument, when it is legal (`?pre`), what it does to the model (`~next`,
  required — its absence would be the silent claim that a call changes
  nothing) and what it does to the real thing, whose body asserts with
  the ordinary verbs; `call` is the argument-less form. `stateful name
  ~model ~scope commands` draws 100 programs of at most 20 calls
  (`?count`, `?steps`), each run against a system `~scope` builds for it
  — a callback, so a resource that exists only inside a `with_`-style
  call is a system under test — with `?invariant` checked after every
  step and `?pp_model` printing the model each call was made in.
- Programs are repaired against the model so an illegal call is removed
  rather than skipped — the program you read is the program that ran —
  and shrink by deleting calls and reducing arguments, never by
  substituting commands. The counterexample is its summary, `2 calls,
  last: get`, over a table: a row per call under the header ` #  model
  before  call`, the model being the one the call ran against and the
  column absent without `?pp_model` (`… (N calls omitted)` in the middle
  of a program over 40 calls); then the failing command's declaration
  site over `call N of M: <name>`, or the test's declaration over
  `invariant after call N of M: <name>` for an invariant. A
  `~pre` or `~next` that raises is reported once, unshrunk, as a
  specification bug naming the call, the operation and which of the two
  raised (`call 3: close, ~pre raised Failure("nth")`).

### Baselines and expect tests

- **`snapshot` and `snapshot_pp` are `expect`, `expect_exact` and
  `expect_file`.** A baseline is where the source says it is: the literal
  at an `expect actual @@ __POS_OF__ {|…|}` call — compared with
  ppx_expect's whitespace flexibility; `expect_exact` byte for byte — or
  the file an `expect_file actual path` call names, relative to the
  project root, read as line-oriented text (CR and CRLF as LF, a final
  newline forced). Nothing is derived from a test's name or declaration
  file, so no edit can orphan a baseline: the `__snapshots__` layout, the
  name grammar, the duplicate-name failure and the stale-baseline report
  are gone, and no baseline depends on `-g`. The `expect` family takes the
  produced text first and the literal last, so a `{|…|}` block reads as a
  block; `equal` stays expected-first. A missing file is a mismatch whose
  correction is the file.
- A baseline block opens on the verb that checked and how it ended: `expect:
  mismatch`, `expect_exact: mismatch`, `expect_file "<path>": mismatch` or
  `expect_file "<path>": no baseline`. A mismatch's correction follows as a
  diff with no `---`/`+++` head; a missing file's text follows `proposed (N
  lines):` as `+` lines indented two columns, 20 at most, then `… (+N more
  lines)`. An `expect_exact` that differs only by a trailing newline, which
  no line diff shows, says `values differ only by a trailing newline (on the
  <side> side)`. A path windtrap cannot prove to lie under the project root
  says so (`expect_file "<path>": the path cannot be proven to lie under the
  project root`), names the `unverified path:` and the remedy, `(set
  WINDTRAP_PROJECT_ROOT to the directory the path is relative to)`. Under
  `-v` the row of a test with a missing file ends `(no baseline)`.
- **Checking is read-only, and there are two acceptance gestures.** A
  mismatch is reported with its diff and its acceptance command in every
  mode; what the run writes is its mode. Nothing by default. Under
  `--corrected`, every correction as `<file>.corrected` beside the file it
  corrects — beside dune's build copy inside a build action — for a
  `diff?` step and `dune promote`; the `(test)` stanza is `(action (progn
  (run %{test} --corrected) (diff? test_mylib.ml test_mylib.ml.corrected)
  (diff? help.expected help.expected.corrected)))`, and because promotion
  fills a file but never creates one, a new `.expected` starts empty
  (`touch`) or is accepted once with `-u` (the report spells that
  recipe). Under `-u`, every correction in place, atomically — the
  literal rewritten in its source file, re-indented to its line, its
  delimiter kept — refused under `CI` with no override, and refused with
  `--corrected`. Neither has a mirror: acceptance is never a variable in
  a build action's environment. `WINDTRAP_UPDATE` is gone.
- **What a promotion can bless.** A correction is recorded only for a
  test whose every failure is a baseline mismatch: an assertion failure,
  a raise or a timeout beside a stale expectation withholds that test's
  corrections, a test that skipped records none, and an `xfail` test
  records none in any mode. A mismatch is a checkpoint, not an
  assertion: it is recorded and the call returns, later expectations are
  checked too, and one correcting run records every correction of a body
  in one pass. A block whose correction was withheld prints no `accept:`,
  which would promote or rewrite nothing: it ends on `no correction was
  kept: the test also failed outside its expectations; fix that failure
  and rerun` (for a test that skipped, `… the test also skipped; skip
  before the expectation or not at all, and rerun`), with only a
  property's `replay:` after it.
  A literal whose source no longer decodes to the value the binary was
  compiled with, or cannot be read, keeps no correction: its expectation
  fails, under `-u` too, and its block says `correction refused (line N):
  <reason>` instead of an `accept:`. A second text for a baseline that the
  run already corrected keeps none either: `no correction was kept:
  another check of this baseline produced a different text earlier in the
  run`. A test counts as corrected, and leaves the exit code to dune's
  `diff?`, only when each of its failures carries a kept correction; a
  refused literal or an out-of-root `expect_file` beside an accepted
  expectation fails the run under `-u`, where it used to exit 0.
  The end-of-run report names what was written, one line per file; a file
  that cannot be written — an unwritable path, a directory that cannot be
  created, a source edited during the run — is named with its reason and
  fails the run, and the files after it are still written.
- A test is not retried past an attempt whose corrections were kept: the
  next attempt would be compared with the text just recorded. Under
  `--corrected` a test declared with `~retries` whose only failure is a
  stale expectation runs once, is never listed under `flaky tests`, and
  ends as it does without `~retries`. Plain checking records nothing and
  retries as declared.
- **`ppx_windtrap` desugars into the library and nothing more.**
  `let%expect_test "n" = body` registers `test "n" (fun () ->
  Expect_test_config.run (fun () -> body))` under a group named after the
  file; `[%expect {|x|}]` is `expect (Expect_test_config.sanitize (output
  ())) (pos, {|x|})` at the node's own position, `[%expect_exact]` the
  same with `expect_exact`, a bare `[%expect]` an empty literal the first
  correction fills, `[%expect.output]` the sanitized `output ()`;
  `let%test` takes a unit-returning body of assertions, `module%test`
  groups, `[@tags …]` tags. The runtime keeps dune's `inline-test-runner
  <lib> -partition <file>` protocol and hands each partition to `run
  --corrected`, so an inline expectation is an `expect` literal like any
  other: same matcher, same correction, same `.corrected` beside dune's
  build copy, same `dune promote`, same report and exit contract, and the
  `WINDTRAP_*` mirrors are the rest of its command line. A correction
  patches the stale payload's extent and keeps the node's head and
  delimiter; a file with no corrections is never rewritten.
  `Expect_test_config` is `run` and `sanitize`, and shadowing it tunes a
  whole file. Two things ppx_expect checks are not checked, by design:
  output after a test's last node, and a node the body never reached — a
  node is a call, and only calls are checked. `[@@expect.uncaught_exn]`,
  `[%expect.unreachable]`, `[%expect.if_reached]`, `[%expectation]` and
  `[%expect]` outside a `let%expect_test` are rejected at expansion with a
  diagnostic naming the construct; a monadic config fails to compile at
  the reference. Compatibility is measured against Jane Street's pinned
  ppx_expect corpus (`test/conformance/RESULTS.md`).
- **Inline tests that nothing drives fail loudly.** `let%expect_test` in
  a stanza without `(inline_tests)` used to exit 0 having run nothing;
  the first registration now arms an `at_exit` guard that every driving
  path disarms, and a process that ends with registrations never claimed
  prints a diagnostic naming the files and both fixes, and exits 2.
- **`output ()`** consumes what the test printed since it started or the
  previous call, standard error and subprocess output included; under
  `--stream` it fails the test with "rerun without --stream" rather than
  comparing against silence (0.1 compared against the empty string). The
  0.1 `expect`/`expect_exact`/`capture`/`capture_exact` string-comparison
  family is gone; `expect (output ()) @@ __POS_OF__ {|…|}` is its spelling.

### Resources and process state

- **`temp_dir ?prefix ()` and `temp_file ?suffix ()`** give runner-owned
  scratch paths, removed after the test on every outcome, per attempt.
- **`setenv name (Some v)` / `setenv name None` and `chdir dir`** bind
  the environment and the working directory for one test; the runner puts
  both back at the attempt boundary, outside the timeout window, on every
  outcome. The unbinding is real — POSIX `unsetenv(3)` through a C stub —
  so `Sys.getenv_opt` answers `None`, not `Some ""`, and the path a
  missing variable takes is testable. Restoration is first-set-wins per
  variable; a working directory that cannot be restored fails the test
  naming it. Both are process-global while the test runs.
- **`subtest name fn`** labels sub-cases inside one body that all run
  even after one fails; **`current_test ()`** returns the running test's
  path, for keying artifacts by identity.
- **The per-test timeout covers teardown**: the window is re-armed before
  teardown with what remains, or a fresh one when the body used it all.
  A raising setup or teardown is an ordinary reported outcome (0.1 ran
  hooks outside the boundary), and stopping early with `-x` no longer
  skips cleanup.
- **`Stdlib.exit` from a test cannot kill the run**: from a body, setup,
  teardown, scope or fixture release the attempt is intercepted and
  recorded as that test's failure, every later test still runs, and the
  run returns its own code; only the process that started the run
  intercepts, so a forked child that calls `exit` terminates.
- **A skip, a timeout, an `exit` and a discard keep their meaning
  wherever they are raised.** Inside `raises`, `raises_match` (whatever the
  predicate), a `subtest`, a property's law or generator, a stateful
  `~pre`, `~next` or scope, and a fixture's acquisition, none is recorded
  as a failure of that place: an `exit` in a law is the test's intercepted
  exit, not a shrunk counterexample; an `assume` in a subtest inside a law
  discards the case; a timeout that cuts a `Fun.protect` finally is a
  `[teardown]` timeout, not an uncaught `Fun.Finally_raised`; a fixture
  whose acquisition timed out is acquired again by the next test instead
  of timing it out too. `Sys.Break` and `Out_of_memory` stop the run from
  everywhere, a counterexample printer included, and the body's exception
  is reported over a capture log that failed to flush.
- **A `Stack_overflow` is an ordinary failure**: the test that overflowed
  fails with it (a property shrinks it like any exception) and the run
  goes on. OCaml 5 recovers from it; only `Sys.Break` and `Out_of_memory`
  end the run.

### Running tests

- **The command line.** `-f`/`--filter PATTERN` (or a bare pattern),
  `-e`/`--exclude`, `--tag` and `--exclude-tag` (repeatable), `--shard
  K/N`, `--failed`, `-l`/`--list`, `-x`/`--fail-fast`, `--timeout`,
  `--slow-threshold`, `--seed s1:…`, `--prop-count`, `-u`/`--update`,
  `--corrected`, `-s`/`--stream`, `-v`/`--verbose`, `--junit PATH`,
  `--color MODE`, `-o`/`--output DIR`, `--mutate[=PREFIX,…]`, `--arm ID`,
  `-V`, `-h`. `--help` is generated from the parser. An unknown flag
  suggests the near miss (`did you mean '--filter'?`). Gone from 0.1:
  `--format`/`WINDTRAP_FORMAT` (verbosity is one axis, default ⊂ `-v`;
  TAP consumers move to `--junit` or the GitHub annotations), `-q`/`--quick`
  (`--exclude-tag slow`), `--bail`, and `WINDTRAP_TAIL_ERRORS` (the tail
  under a failure is ten lines of the 8 KiB kept, with the full log's
  path; the report is 80 columns, always).
- **Mirrors: two reading rules, no dialects.** Every flag that changes
  what a run does or reports has a `WINDTRAP_*` mirror declared beside it
  and read through the flag's own parser (`WINDTRAP_FILTER`,
  `WINDTRAP_EXCLUDE`, `WINDTRAP_TAG`, `WINDTRAP_EXCLUDE_TAG`,
  `WINDTRAP_SHARD`, `WINDTRAP_TIMEOUT`, `WINDTRAP_SLOW_THRESHOLD`,
  `WINDTRAP_SEED`, `WINDTRAP_PROP_COUNT`, `WINDTRAP_STREAM`,
  `WINDTRAP_VERBOSE`, `WINDTRAP_JUNIT`, `WINDTRAP_COLOR`,
  `WINDTRAP_OUTPUT`, `WINDTRAP_MUTATE`, `WINDTRAP_MUTATE_ARM`), because
  under `dune runtest` the mirrors *are* the CLI. A plain value is one
  token, trimmed; a repeatable flag's is comma-separated; a valueless
  flag's is a boolean in one vocabulary (`1/0`, `true/false`, `yes/no`,
  `on/off`) and any other spelling is a usage error naming the variable,
  never a silent default; an optional-value flag's reads a truthy value
  as the bare flag, a falsy one as absence, anything else as the value.
  Precedence is CLI > environment > default. `-l`, `--failed`, `-x`,
  `-u`, `--corrected`, `-h` and `-V` have no mirror: they want a command
  line (`--failed` in particular, whose store dune's sandbox would defeat).
  Three variables have no flag: `WINDTRAP_PROJECT_ROOT`,
  `WINDTRAP_COVERAGE_FILE` and `NO_COLOR`.
- **A variable changes nothing on a warm tree** unless the run passes
  `--force` or the stanza declares `(deps (env_var WINDTRAP_X))`; the
  manual shows both.
- **Exit codes are 0, 1 or 2**: passed, failed, nothing ran. Under
  `--corrected` — a build action's run — a test whose failures are all
  recorded corrections leaves the code alone and a selection the mirrors
  emptied exits 0 (still printing its `no tests ran` line), because the
  `diff?` that follows is the verdict and a `WINDTRAP_*` selection spans
  every stanza of the tree; usage errors stay 2 in every mode. `--failed`
  with nothing recorded refuses the run (exit 2).
- An empty selection says why, and a typed run says how to list what
  there is: `mylib: no tests ran: filter "parsr" matched none of 48
  tests.` then `list: ./t.exe -l`, the one line after an outcome; a build
  action, which has no launcher to restate, prints `(list the suite's
  tests with -l)` there. Under `-l` the sentence goes to standard error
  (`windtrap: no tests ran: … tests.`) and standard output stays empty.
- **The transcript is compact.** A green, healthy run is exactly one line
  (`mylib: 48 passed in 1.2s.`). The header (`mylib: N tests`) prints iff
  something follows it: the failure blocks, a `slow tests (N, over Ts):`
  section for untagged tests over `--slow-threshold`
  (`WINDTRAP_SLOW_THRESHOLD`; default one second, `0` disables), a `flaky
  tests (N):` section naming every test that passed on a later `~retries`
  attempt, a `corrections (N):` section, and the summary. On a terminal an
  erasable `[k/n] current-test…` tail names the executing test, so a hung
  test names itself. `-v` streams one status line per test and prints a
  passing property's label distribution. No run advertises `--failed`.
- A failure block prints when its test finishes, under the header and a
  58-column `── failures ──` rule, so a run that dies has already printed
  what it knew; the rule that closes the failures, the other sections and
  the summary print at the end. One blank line separates two blocks, and two
  failures of one test. Under `-v` a failed test's status line is its
  block's title: the block prints under it when the test finishes and closes
  on a blank line, no failures section repeats it, and a blank line
  separates the rows from the sections that end the run. The title of a
  failed fixture release carries no duration, since no release is timed.
- The summary is the last line of every run: `4 passed (1 flaky), 1
  skipped, 1 expected failure, 6 failed (1 subtest failure), 2 not run, 1
  correction written in 6.5s.`, zero terms omitted. `N not run` counts the
  selected tests a run stopped by `-x` never reached.
- A measured duration prints one way everywhere: `0.5ms` below 10 ms,
  `60ms` below one second, `6.5s` from there. A configured threshold
  prints as you wrote it (`over 0.01s`).
- The seed prints iff a selected test is a property: on the header when
  there is one, on the one line of a green run (`mylib: 48 passed in 60ms
  (seed s1:…).`), and nowhere when the selection holds no property.
- A failure's location is the bare `<file:line>`: the assertion's, or the
  test's for an assertion in tail position, a timeout, an uncaught
  exception or an `xfail` test that passed. Whenever the file can be read
  its source line prints under it, without its leading whitespace and
  with its control bytes escaped as a value's are, then one blank line; a
  property's inner failure has the line and no blank line. A phase comes first, as the tag `[setup]`, `[teardown]` or
  `[release]`: `[teardown] test/db.ml:36`, and the tag alone on its line
  for a failure with no location.
- A failure block ends on a command line only when the command says what
  the block does not: `accept:` for a baseline, narrowed to the block
  (`./t.exe -u -f '<test>'`, or `dune promote <file>` under a build
  action); `replay:` for a property, which carries the seed. Any other
  block ends on its facts. A test with several failures prints each
  distinct command line once, after its captured output.
- In an `--arm` run every `FAIL` title says `(mutant armed)`, and
  `replay:` carries `--arm <id>`, so the command line fails for the same
  reason. No `accept:` prints there: what differs is the mutant's output.
- An exception failure reads `expected exception  <e>` over `raised  <e>`,
  or over `but no exception was raised`; an uncaught exception is
  `uncaught exception:` over the exception, a `raises_match` rejection
  `raised exception does not satisfy the predicate:` over it, and a wrong
  message is `raised <Constructor> with the wrong message:` over the two
  messages as `expected` and `actual`, marked as any two values are.
- Two unequal values the printer cannot tell apart print once, as `both
  sides render as: <v>`; a subtest's entry names it on a `subtest   <name>`
  line under its location.
- What `-u` or `--corrected` wrote is the `corrections (N):` section above
  the summary (`wrote <path> (N expectations)`, `accepted <path>`), sorted
  by path. A file the run could not write is a row of that section,
  `could not write <path>: <reason>`, and a summary term, `N not
  written`.
- A `--corrected` run that wrote a correction and exits 1 says, after its
  summary and on standard error, that `dune promote` has nothing to
  promote yet: `windtrap: warning: dune registers a correction for
  promotion only when the run that wrote it exits 0, so the failures above
  withhold the correction written here. Fix the failures, rerun, then
  'dune promote'.` A block's `accept: dune promote <file>` prints when its
  test ends and cannot know what fails later.
- The slow section carries its threshold and no advice line, and `-v` no
  longer prints a slowest-tests list: its status lines carry every
  duration.
- A failure block's own words hold no dash and no dot glyph: the runner's
  messages read `<what>; <what to do>` (`the test called exit and was
  intercepted; a test must return or raise, never exit the process`), and
  an unmet `cover` label's row is `<label>  0  never covered`.
- A `?msg` prints each of its lines inside its block, a skip reason and
  an `xfail` reason stay in their row, and the control bytes of all three
  are escaped as every text's are.
- **What windtrap says about itself is on standard error behind
  `windtrap:`**, in the runner, the runtimes and the `windtrap` binary
  alike (the `windtrap coverage:` and `windtrap mutants:` prefixes are
  gone), standard output flushed first so a merged log keeps the order. A
  usage error is that line and the `usage:` line, exit 2: `windtrap:
  invalid value 'x' for --prop-count: expected a positive integer`; after
  `windtrap: unknown command 'x'` the binary also lists its commands. A
  message of several lines is anchored on its first, any other control
  byte but a tab prints as `\xNN` (a carriage return as `\x0d`), and one
  the run survives says so: `windtrap: warning: focus is active: 1 of 2 tests ran;
  remove the focus before committing`, `windtrap: warning: could not write
  JUnit report: <reason>`. The words are otherwise unchanged but for the
  lines an instrumented executable prints about its coverage dump, which
  name it in the sentence now that the prefix does not (`windtrap:
  warning: cannot write coverage file <path>: <reason>`).
- `--help` shows each option as a line with its spellings and its
  `WINDTRAP_*` mirror (`-f PATTERN, --filter=PATTERN (env
  WINDTRAP_FILTER)`), then its description indented under it as whole
  sentences, within 80 columns; the variables with no flag take the same
  form. `windtrap --help`, `windtrap coverage --help` and `windtrap mutants
  --help` take it too, open on a name line and list `WINDTRAP_COLOR`, which
  has no flag there; `windtrap coverage` accepts `--expect=PATH` and
  `--do-not-expect=PATH` as it did `--min=PCT`.
- `--stream` replaces capture with pass-through and changes no line of
  the transcript: a green streamed run is its tests' bytes and the one
  line, and rows are `-v`'s. The stdout channel, C stdio and descriptor 1
  are flushed at each boundary, so a failure block follows its test's
  bytes (a test's standard error is not ordered against it).
- **SIGINT, SIGTERM and SIGHUP end a run on what it knows**: `windtrap:
  interrupted in <test>` (or `interrupted while releasing <fixture>`,
  `interrupted between tests`) on standard error, the summary with `N not
  run`, the fixtures still held released, then death by the same signal,
  so the parent sees the signal. A second signal kills at
  once; a signal the process was started ignoring stays ignored; the
  handlers are installed only while a run executes. Not on Windows.
- **Captured output** — each test's stdout and stderr, C stubs and
  subprocesses included — lives at `<log root>/<suite>/<groups…>/<test>.output`,
  the same path on every run, and a failure block ends with its tail and
  that path: the last ten lines, indented two columns, under `captured
  output (N lines):`, `captured output (last 10 of N lines):` or `captured
  output (last 10 lines, B earlier bytes omitted):`, `B` counting every
  byte before the first line shown, then `full log: <path>` at the
  heading's column. `-o DIR`
  moves the log root and is resolved once, at startup. `--stream` disables
  capture.
- **The project root and the log root.** The root, which `expect_file`
  paths and `.corrected` files resolve under, is `WINDTRAP_PROJECT_ROOT`
  if set; else the directory above the build directory the process
  belongs to — from `INSIDE_DUNE`, which dune exports as the build
  context, or from the executable's own path when it lies under a
  directory whose name starts with `_build` — else the working directory.
  No marker file is consulted. Capture logs and the last-failed store
  live under `<build dir>/_tests` when a build directory was found (a
  private `--build-dir` keeps its own) and under `<tmp>/windtrap`
  otherwise, keyed by suite.
- **CI.** `--shard K/N` partitions the selected tests by a frozen hash of
  each path. `--junit PATH` (`WINDTRAP_JUNIT`) writes a JUnit document: a
  target ending in `.xml` is that file, anything else a directory into
  which every suite writes `<dir>/<suite>.xml` — inline partitions as
  `<dir>/<lib>_<partition>-<digest>.xml`, whose log directory and
  last-failed store are keyed the same way — so `WINDTRAP_JUNIT=_build/junit
  dune runtest` collects every stanza. The document prints a control
  byte as the terminal does (`\x1b`, `\x0a` in a name), never strips it,
  and notes a flaky pass in `system-out`; a `<failure>` holds the block's lines
  and its `message` is the failure as one sentence, after the subtest's
  label and your `?msg` (`contract › shape [0]: expected [1; 2], got [1;
  3]`, `deliberate: expected 1, got 2`, `uncaught exception: Not_found`,
  `expected and actual differ (5 diff lines)`, `expect: mismatch`), cut at
  80 characters. Under GitHub Actions (`CI` and `GITHUB_ACTIONS` set) the
  transcript sits in a `::group::` block, one `::error` annotation per
  failure follows its close, titled `Test failure: <path>` and carrying
  the block's lines, and the summary is still the last line. `NO_COLOR` is honoured
  under `--color auto`; `--color always` still wins, and the reporting
  commands read `WINDTRAP_COLOR` too. Under `CI`, focused tests and `-u`
  refuse to start.

### Coverage

- **One backend, spelled `ppx_windtrap.coverage`.** `(instrumentation
  (backend ppx_windtrap.coverage))` on the library under test, inert
  without `--instrument-with ppx_windtrap.coverage` (or a `dune-workspace`
  `instrument_with` line). The bare `ppx_windtrap` backend is gone: it
  linked the windtrap core into every instrumented library's closure.
- **Two commands, one reporter.** The instrumented run (`dune runtest
  --force --instrument-with ppx_windtrap.coverage`) writes one dump per
  executable under the build directory's `_coverage` — the first run of a
  rebuilt executable removes its predecessors', and a tree built without
  dune writes under the working directory's `_windtrap/coverage` — and
  `windtrap coverage` merges them into the per-file table, which is the
  default report. A test run prints no coverage number of its own: the
  gate lives only in the command. `--min PCT` exits 1 below the threshold,
  the report's last line stating the exact fraction beside the percentage;
  `--expect PATH` (`--do-not-expect` to exempt) fails when a source under
  `PATH` has no data at all; `-u` adds the uncovered source excerpts;
  `--json` prints per-file percentages and uncovered lines; `--lcov`
  prints an LCOV tracefile for Codecov, Coveralls, GitLab, editor gutters
  and `genhtml`; positional `PATH…` replaces the default search. A dump
  whose executable was deleted or rebuilt since is excluded with a warning
  line, three such lines at most and then `... and N more like that`, and
  one remedy sentence; when nothing is left the run says how many files it
  found, whether they are stale or orphaned, and that the usual cause is a
  build without the instrumentation flag. There is no override.
  `WINDTRAP_COVERAGE_FILE=path` sends one run's dump to an explicit file,
  which is also how a build rule declares it as a target.
- **Percentages change meaning.** Coverage is expression-grade with entry
  points per block and out-edge points on calls, which count only when the
  call returns, so raising paths show as uncovered instead of painted
  green for having been entered; numbers are not comparable with 0.1.
  Exclusions use Bisect_ppx's spelling (`[@coverage off]`, `[@@coverage
  off]`, `[@@@coverage off]`/`[@@@coverage on]`, `[@@@coverage
  exclude_file]`). Instrumentation never changes what a program means —
  out-edge points are given up wherever taking one would cost a tail call
  — and a semantics-preservation suite holds that line.
- The report ends on its outcome: `coverage: 71.4% (312/437 points)` is
  the last line, and under `--min` the gate is on it, `, minimum 80%:
  FAILED` or `, minimum 70%: ok`, the minimum as you gave it (`--min
  99.99999` prints `minimum 99.99999%`, never a rounded `100%`). Under `--json` and `--lcov`, whose
  standard output is the document, a gated run says the same sentence on
  standard error behind `windtrap:`.
- The table opens with a dim header row: `cover`, `points`, `file` and
  `uncovered lines (-u shows the source)`, which is `uncovered lines`
  under `-u`, where the source follows. A row lists its first eight uncovered line ranges, then `(+N more)`; the header's hint is how to see the others.
- A percentage is red below `--min`, or below 80% without it, and
  unstyled otherwise.
- Under `-u` each file's source follows the table under `<file>: 58.0%
  (69/119)`, the file bold, and the outcome follows the last file. Source
  text prints its control bytes as `\xNN`, in a survivor's block too.

### Mutation testing

- **A second backend, `ppx_windtrap.mutate`**, compiles every mutant of
  the library it instruments into the binary behind an inert guard, and
  the test executable becomes its own mutation runner: `--mutate` runs the
  suite once as a dry run — proving it green and recording per mutant
  which tests evaluated it — then forks itself once per reached mutant
  and runs only those tests. A mutant none of them notice is a
  `SURVIVED` block naming the line, the rewrite and the tests that ran it
  and did not fail. `--mutate=PREFIX,…` keeps to the files whose recorded
  path starts with a prefix (the loop forks per mutant, so the prefixes
  narrow the work); the ordinary selection (`-f`, tags) mutates only what
  the selected tests reach, reports in full, and writes no verdict file,
  which it says on standard error (`windtrap: verdicts not saved: this
  run's selection narrows the suite, and a partial run's verdicts would
  stand in the project merge as the whole.`), the report ending on its
  `mutants:` line. A
  per-executable run exits 0 whatever it finds. Four operators: `neg`,
  `cmp` (comparisons in a boolean context), `con`, `ari`. Equivalent
  mutants are dismissed in the source with `[@mutate off "reason"]` in the
  four spellings the coverage attribute uses; there is no suppression
  database. Mutation needs `Unix.fork` and declines by name on Windows,
  and in a process that has spawned a domain, which OCaml forbids to fork.
- **`--arm ID`** runs the suite once with one mutant armed, announced
  before any output (`mutant <id> armed: <before> → <after>`) and closed
  with one verdict line (`mutant killed.`, `mutant survived: …`, `mutant
  not evaluated: …`); the report's `reproduce:` line spells it.
  Armed checking is read-only: no `.corrected` is written. `--mutate` and
  `--arm` together is a usage error. Both flags have mirrors,
  `WINDTRAP_MUTATE` (a truthy value is the bare flag, a falsy one its
  absence, anything else the prefixes) and `WINDTRAP_MUTATE_ARM`; the
  runtime itself reads no flag and no environment.
- **The project answer is the merge.** Each unfiltered run writes a verdict
  file under the build directory's `_mutants` (or `_windtrap/mutants`
  without one); a `--mutate=PREFIX` run replaces the records under its
  prefixes and keeps the others when the same build wrote the file, and `windtrap mutants` merges them under **killed
  anywhere wins**, reporting survivors with the executable beside each
  reaching test plus a `never reached` section for mutants no executable's tests
  evaluate; it exits 1 when any mutant survived every executable that
  reached it — the one mutation exit code a build gates on. Two commands:
  `WINDTRAP_MUTATE=1 dune runtest --force --instrument-with
  ppx_windtrap.mutate`, then `dune exec windtrap -- mutants`; an alias
  folds them (the manual and `examples/x-blueprint` show `@mutate`). The
  merge excludes a verdict whose executable was rebuilt since it ran, on
  the coverage command's terms (three warning lines at most, the count
  and the stale/orphaned split when nothing is left), and
  `WINDTRAP_MUTATE_ARM=<id>` in front of the suite command arms one mutant
  across every suite.
- The `--mutate` loop prints what it finds as it finds it: a `SURVIVED`
  block when that mutant's child ends, in the catalogue's order, under a
  `survivors` rule that carries no count; on a terminal a dim `[3/5]
  <id>…` line names the mutant being tried. A loop that kills every
  mutant it reaches, and reaches them all, is two lines: the suite's
  summary, then `mutants: 3 reached by this suite, 3 killed`.
- A survivor's block is `SURVIVED  <id>  <before> → <after>`, the mutated
  source line as a failure block prints one, and its reaching tests
  padded to the widest name of the block.
- Mutants no test evaluated are one row per file under `never reached
  (N)`: how many, the file, and their lines as ranges, eight at most
  (`2  lib/calc.ml   lines 40-41`). Both the loop and `windtrap mutants`
  print the section and count them on the `mutants:` line, the report's
  last: `mutants: 2 survived of 5 reached by this suite, 3 killed, 2 never
  reached`.
- `reproduce:` is one command that runs as pasted, above the `mutants:`
  line: it arms the first survivor printed, by its identifier, the
  executable's path quoted where a shell would split it. The loop spells it
  from the run's launcher and restates the run's selection (`-f`, `-e`,
  `--tag`, `--exclude-tag`, `--shard`, `--failed`, or their mirrors), since
  a survivor of a narrowed run survived that selection only; `windtrap
  mutants` spells it from the executable the verdict file records:
  - under dune: `dune exec --instrument-with ppx_windtrap.mutate <exe> --
    --arm <id>`;
  - run by hand: `<exe> --arm <id>`;
  - under a build action, and for an inline runner: `WINDTRAP_MUTATE_ARM=<id>
    dune runtest --force --instrument-with ppx_windtrap.mutate`.
- SIGINT, SIGTERM and SIGHUP end a loop on what it knows: the running
  child's process group is killed, `windtrap: interrupted while testing
  <id>` goes to standard error, the `mutants:` line counts `N not tested`,
  no verdict file is written, and the process dies by the same signal. A
  reader that goes away (`… --mutate | head -1`) ends it as quietly as it
  left: nothing is said, the loop's scratch directory is removed, and the
  process dies by SIGPIPE. A loop that ran whole has written its
  verdict file before its last lines print, so neither costs it: a signal
  that arrives once the last child has ended lets the report finish,
  then the process dies by it.

### Packages and libraries

- **`windtrap`** depends on `unix`, its own C stubs and
  `windtrap.runtime` — the runtime only for the mutation loop. `open
  Windtrap` brings the flat values and exactly four modules: `Testable`,
  `Gen`, `Exn`, and `Private`, which re-exports the internals for
  windtrap's own suites and binary only and carries no stability
  guarantee; `Tag`, `Pp` and `Ppx_runtime` are no longer public, and
  project modules with other names are no longer shadowed. `windtrap.clock` is folded into the core; a stanza
  that linked it deletes the line.
- **`windtrap.runtime`** is the one stdlib-only library every instrumented
  closure links: `Windtrap_runtime.Coverage`, `.Mutate`, `.Verdicts` and
  `.Instr`. It reads no environment variable but `WINDTRAP_COVERAGE_FILE`.
- **`ppx_windtrap`** is the expect and inline-test rewriter with its
  `inline_tests.backend`; `ppx_windtrap.runtime` the module-load registry
  and runner main, a client of the public API; `ppx_windtrap.config` the
  ambient `Expect_test_config`; `ppx_windtrap.coverage` and
  `ppx_windtrap.mutate` the two instrumentation backends. The inline
  runner speaks dune's protocol and nothing else (`-source-tree-root`,
  `-diff-cmd` and the `inline-test=drop` cookie are gone), and generated
  code builds clean under `-w +a -warn-error +a`.

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
