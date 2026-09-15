# Baselines and expect tests

A baseline is a reviewed expectation that the source names: the literal
at an `expect` call, or the file an `expect_file` call names. Checking
is read-only and explicit about acceptance — a green run always means
"matched the reviewed expectation" — and both storages are accepted the
same way: under dune with `dune promote`, without dune with `-u`.

| Storage | Write | Best for |
| --- | --- | --- |
| A literal at the call | `expect actual @@ __POS_OF__ {|…|}` | short output you want visible in code review |
| A committed file | `expect_file actual "test/help.expected"` | larger output (help pages, reports, rendered JSON), and text other tests read |
| Inline, in the library | `let%expect_test` + `[%expect {|…|}]` (`ppx_windtrap`) | tests beside the code, with access to unexported bindings |

## Literals and files

```ocaml
test "tokens" (fun () ->
    print_tokens (tokenize "1 + 2");
    expect (output ()) @@ __POS_OF__ {|
      INT 1
      PLUS
      INT 2
      |});
test "help page" (fun () -> expect_file (help ()) "test/help.expected");
```

The `expect` family takes the produced text first and the literal last,
so a `{|…|}` block reads as a block; `equal` and the assertion verbs
stay expected-first. `expect` compares with ppx_expect's whitespace
flexibility — lines trimmed, blank leading and trailing lines dropped,
the block dedented — so the literal's indentation never causes a
mismatch; `expect_exact` compares byte for byte. `expect_file` names its
file relative to the project root and compares line-oriented text: CR
and CRLF read as LF and a final newline is forced on both sides. If CR
bytes or the missing final newline are the point, encode first (e.g.
`String.escaped`). Redaction is ordinary code before the call:
`expect (mask_timestamps (output ())) @@ __POS_OF__ {|…|}`.

Nothing is silently created. A first run against a missing file, or a
stale literal, fails with the proposed content or a diff and the
acceptance command for the way the run was invoked. A mismatch is
recorded and the call returns, so the body continues: one run reports
every stale expectation, and one acceptance takes them all.

```
$ dune exec test/test_mytool.exe
mytool: 1 test
F
──────────────────── failures (1) ────────────────────
  FAIL  cli › cli help
    test/test_mytool.ml:18
      18 │     [ test "cli help" (fun () -> expect_file (help ()) "test/help.expected") ]

    expect_file "test/help.expected": no baseline
    proposed (5 lines):
      ┆ Usage: mytool [OPTIONS] COMMAND
      ┆ 
      ┆ Commands:
      ┆   build    Build the project
      ┆   test     Run the tests
    accept: dune exec test/test_mytool.exe -- -u, then review with git diff
──────────────────────────────────────────────────────

1 failed in 0.00344s.
```

## Accepting under dune: `dune promote`

A `(test)` stanza declares its corrections in one action, which is
dune's own promotion idiom and what `(inline_tests)` generates behind
the scenes:

```lisp
(test
 (name test_mytool)
 (libraries windtrap mytool)
 (deps help.expected)
 (action
  (progn
   (run %{test} --corrected)
   (diff? test_mytool.ml test_mytool.ml.corrected)
   (diff? help.expected help.expected.corrected))))
```

`--corrected` makes the run write every correction beside the file it
corrects, as `<file>.corrected` — a rewritten literal in a copy of the
test file, the produced text for a file baseline — and leave the exit
code to the `diff?` that follows: a run whose only failures are
recorded corrections exits 0, and dune's diff is the verdict. `dune
runtest` then shows the correction as a diff, and `dune promote` (or
`dune promote test/help.expected` to take one file) accepts it:

```
$ dune runtest
mytool: 2 tests
.F
──────────────────── failures (1) ────────────────────
  FAIL  cli help
  …
    accept: dune promote
──────────────────────────────────────────────────────

1 passed, 1 failed in 0.000655s.
wrote test/help.expected.corrected
File "test/help.expected", line 1, characters 0-0:
diff --git a/_build/default/test/help.expected b/_build/default/test/help.expected.corrected
--- a/_build/default/test/help.expected
+++ b/_build/default/test/help.expected.corrected
@@ -2,7 +2,7 @@ Usage: mytool [OPTIONS] COMMAND
 
 Commands:
   build    Build the project
-  test     Run all tests
+  test     Run the tests
$ dune promote
Promoting _build/default/test/help.expected.corrected to test/help.expected.
```

Two rules of dune's own follow from the stanza:

- **The action sees dune's copy of the file, not the source tree.** A
  build action runs under `_build/default/`, and that is where
  `expect_file` reads its file and where the correction lands, beside
  the copy, which is where `diff?` looks; the source tree is never
  written by a build action. The `diff?` step makes its first file an
  input of the action, so dune copies it there and re-runs the test
  when it changes; `(deps help.expected)` says the same thing
  explicitly, and turns a missing file into an immediate `No rule
  found` error instead of a silently discarded correction. The `diff?`
  steps name files relative to the stanza's directory, exactly as
  `(deps …)` does.
- **A new file must exist before dune can diff it.** Promotion fills a
  file; it never creates one. Create it empty (`touch
  test/help.expected`): the next `dune runtest` shows the whole
  proposed content as the diff, and `dune promote` fills it. Or accept
  it once with `dune exec test/test_mytool.exe -- -u`, which creates
  it. The report under `--corrected` says so
  (`accept: touch 'test/help.expected' && dune runtest, then dune promote`).

Two rules of windtrap's own bound what a promotion can bless. A
correction is written only for a test whose every failure is a baseline
mismatch: an assertion failure, a raise or a timeout beside a stale
expectation withholds that test's corrections until it is fixed, a test
that skipped records none, and a test marked `xfail` records none in any
mode — its mismatch is the failure the annotation expects, not output to
promote. And
dune registers a stanza's corrections only when its action exits 0, so
any other failing test in the stanza withholds them all — the run still
names what it computed (`wrote test/help.expected.corrected`), and the
diff is one `dune promote` away once the failures are fixed. Read every
promoted diff as a code change: promotion is where bugs get blessed as
expected output.

The offer is perishable: dune rebuilds its pending-promotion set on
every invocation, so `dune promote` must directly follow the failing
`dune runtest` — run any other dune command in between and the set is
cleared, with nothing to promote until the next failing run records it
again.

## Accepting without dune: `-u`

`-u` (`--update`) applies every correction in place — the literal
rewritten in its source file, re-indented to its line; the file
written — atomically, and names what it accepted:

```
$ ./test_mytool -u
mytool: 2 passed in 0.00122s.
accepted test/help.expected
accepted test/test_mytool.ml (1 expectation)
$ git diff    # review, commit
```

It is the acceptance outside dune, and under dune it is `dune exec
test/test_mytool.exe -- -u`, an ordinary process rather than a build
action, for a baseline no rule diffs: a family of files named by a
computed path, or a stanza whose author wrote no `diff?`. The same
gating applies — a test that ends in any other failure accepts nothing.
Under CI, `-u` refuses the run before anything executes; there is no
override, because in-place acceptance is a developer's edit. `-u` and
`--corrected` cannot be combined. Neither has an environment mirror: a
build action accepts a baseline through its own `--corrected` action
and never through a variable in its environment.

A literal is rewritten only while it still holds the value the binary
was compiled with; a source edited since the build is refused, named
with its line, and left alone — rebuild and rerun.

## The rules

- **Identity is where the source says it is**: the literal's position,
  which the compiler recomputes on every build, or the file's path.
  Nothing is derived from a test's name or declaration site, so no
  refactoring can orphan a baseline, and an unreferenced `.expected`
  file is found the way any unreferenced file is.
- **One content per baseline per run.** A call shared by several tests
  — a `cases` family, a helper — must produce one text: the first
  correction is the accepted content, a later check with the same text
  passes, and one with another text fails against it rather than
  re-accepting.
- **Checking is read-only.** Without `--corrected` or `-u` a run writes
  nothing, in every mode of failure.

## Captured output: `output ()`

The runner captures each test's standard output and error (C stubs and
subprocesses included). `output ()` consumes what was captured since
the test started or the previous call — assert on it directly, or feed
it to `expect`:

```ocaml
test "greeting goes through capture" (fun () ->
    print_string "Hello, World!\n";
    expect (output ()) @@ __POS_OF__ {| Hello, World! |})
```

Under `--stream` there is no capture; `output ()` fails the test with
"rerun without --stream" rather than comparing against silence.

## Inline expect tests

Expect tests need the PPX: `opam install ppx_windtrap`, then give the
library `(inline_tests)` — dune builds and drives the runner for you:

```lisp
(library
 (name parser)
 (inline_tests)
 (preprocess
  (pps ppx_windtrap)))
```

Print what the code does; `[%expect]` holds the answer:

```ocaml
let%expect_test "tokenize" =
  print_tokens (tokenize "1 + 2");
  [%expect {|
    INT 1
    PLUS
    INT 2
    |}]
```

The PPX is a desugaring into the library and nothing more.
`let%expect_test "tokenize" = body` registers, as the module loads,
`test "tokenize" (fun () -> Expect_test_config.run (fun () -> body))`
under a group named after the file; each `[%expect {|…|}]` in the body
becomes `expect (Expect_test_config.sanitize (output ())) (pos, {|…|})`
with the node's own position as `pos`, `[%expect_exact]` becomes
`expect_exact`, and `[%expect.output]` is that sanitized `output ()`.
The runner dune generates calls `run` with `--corrected`. So an inline
expectation is an `expect` literal like any other: compared with the
same whitespace flexibility, corrected by the same mechanism, accepted
the same way. When output changes, `dune runtest` shows the diff and
`dune promote` accepts it (or from your editor; `dune runtest -w` for
the loop):

```
$ dune runtest
parser: 1 test
F
──────────────────── failures (1) ────────────────────
  FAIL  Parser › tokenize
    lib/parser.ml:29
      29 │   [%expect {|

    expect: mismatch
    @@ -1,3 +1,4 @@
      INT 1
      PLUS
      INT 2
    + EOF
    accept: dune promote
──────────────────────────────────────────────────────

1 failed in 0.001s.
wrote lib/parser.ml.corrected (1 expectation)
...
$ dune promote
```

Mechanics worth knowing:

- `[%expect]` matches with the same whitespace flexibility as `expect`;
  `[%expect_exact {|…|}]` matches byte-for-byte. A bare `[%expect]` is
  an empty literal that the first correction fills in.
- A test may hold several `[%expect]` nodes; each consumes the output
  since the previous one. A node is an ordinary call, checked when the
  code around it runs: a node in a branch not taken is not checked, and
  output printed after the last node is not checked either — end the
  test with the node that pins what it printed. A mismatch is recorded
  and the body continues, so one run reports every stale node and one
  `dune promote` accepts them all.
- `[%expect.output]` returns the captured output as a string for
  post-processing before your own assertion.
- Assertion failures and uncaught exceptions inside an expect test are
  ordinary failures, not corrections, and a test with one records no
  correction — `dune promote` can never bless an `equal` mismatch or a
  raise. They also end the body where a mismatch does not: nothing after
  a failed `equal` runs, so its later nodes are neither checked nor
  corrected. To pin an expected exception, catch and print it:
  `(try boom () with e -> print_string (Printexc.to_string e));
  [%expect {| Failure("boom") |}]`.
- `dune promote` is per-library: dune registers corrections only when
  every inline-test process of the library exits cleanly, so one
  raising test anywhere in the library withholds `dune promote` for
  *all* of the library's corrections — including other files'. The run
  still names what it computed (`wrote lib/parser.ml.corrected`). Fix
  the failures, rerun, promote.
- Shadowing `Expect_test_config` tunes a whole file: `sanitize` is
  applied to every read of captured output, and `run` wraps every body.

```ocaml
module Expect_test_config = struct
  include Expect_test_config

  let sanitize = String.map (fun c -> if c >= '0' && c <= '9' then '#' else c)
end

let report_duration ms = Printf.printf "finished in %d ms\n" ms

let%expect_test "durations are masked" =
  report_duration 37;
  [%expect {| finished in ## ms |}]
```

The same PPX also gives plain inline tests: `let%test "name" = …`
takes a unit-returning body of ordinary assertions (unlike
ppx_inline_test, where the body is a bool), `module%test Name = struct
… end` groups, and `[@tags "slow"]` tags. Outside dune, an inline suite
is the PPX via ocamlfind plus a two-line main:

```ocaml
let () = Ppx_windtrap_runtime.Ppx_runtime.init Sys.argv
let () = Ppx_windtrap_runtime.Ppx_runtime.exit ()
```

run as `./main.exe inline-test-runner mylib`; `-list-partitions` and
`-partition <file>` are the rest of dune's protocol.

## Adopting a ppx_expect suite

Most existing ppx_expect suites run unchanged after swapping the PPX
and the backend in the `dune` file — `(pps ppx_expect)` becomes
`(pps ppx_windtrap)`. The compatibility envelope is measured, not
promised: windtrap vendors the pinned upstream ppx_expect test corpus
and runs it — the suite passes where upstream's passes, and is refused
loudly where windtrap does not implement the construct (see
`test/conformance/RESULTS.md` for the current numbers). Concretely:

- Honored: `let%expect_test`, `[%expect]`, `[%expect_exact]`,
  `[%expect.output]`, `{%expect|…|}` string-extension syntax, quoted
  payloads, functor-duplicated tests, output from C stubs — with
  corrections formatted as ppx_expect formats them, and a correction
  patching the stale payload alone, so no promote reformats a file.
  Two things ppx_expect checks are not checked, by design: output
  after a test's last node, and a node the body never reached; a node
  is a call, and only calls are checked.
- Rejected loudly at expansion, with a diagnostic naming the
  construct: `[@@expect.uncaught_exn]`, `[%expect.unreachable]`,
  `[%expect.if_reached]`, `[%expectation]`. A monadic
  `Expect_test_config` (Async/Lwt) fails to compile at the config
  reference. Nothing silently changes meaning.
- Migrating a test that carried `[@@expect.uncaught_exn]`: delete the
  attribute, catch and print the exception in the body, and let one
  `dune promote` re-record the payload (windtrap prints `Printexc`
  formatting, not upstream's sexp):

  ```ocaml
  let%expect_test "boom" =
    (try boom () with e -> print_string (Printexc.to_string e));
    [%expect {| Failure("boom") |}]
  ```

## Choosing

| Output | Use |
| --- | --- |
| Short, review-worthy, produced by printing | `expect` (or `[%expect]` beside the code) |
| Large or generated (help text, JSON, renders), or read by other tests | `expect_file` |
| Needs masking or custom comparison | `output ()` + ordinary assertions, or the masking before `expect` |
