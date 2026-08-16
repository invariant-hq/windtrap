# Snapshots and expect tests

Both compare produced output against a stored expectation. They differ
in where the expectation lives: a *snapshot* baseline is a committed
file under `__snapshots__/`, best for larger output (help pages,
reports, rendered JSON); an *expect* test holds the expectation inline
in the source as `[%expect {|…|}]`, best for short output you want
visible in code review. Both are read-only when checking and explicit
about acceptance — a green run always means "matched a committed
expectation".

## File snapshots

```ocaml
test "cli help" (fun () -> snapshot "help" (help ()))
```

First run — nothing is silently created:

```
$ dune runtest
mytool: 1 test
F
──────────────────── failures (1) ────────────────────
  FAIL  cli › cli help
    test/test_mytool.ml:18
      18 │   group "cli" [ test "cli help" (fun () -> snapshot "help" (help ())) ]

    snapshot "help": no baseline at test/__snapshots__/test_mytool/help.snap
    proposed (5 lines):
      ┆ Usage: mytool [OPTIONS] COMMAND
      ┆
      ┆ Commands:
      ┆   build    Build the project
      ┆   test     Run the tests
    accept: dune exec test/test_mytool.exe -- -u, then review with git diff
──────────────────────────────────────────────────────

1 failed in 0.00109s.

$ WINDTRAP_UPDATE=1 dune runtest
mytool: 1 passed in 0.00122s.
wrote test/__snapshots__/test_mytool/help.snap (new)
$ git add test/__snapshots__ && git diff --cached    # review, commit
```

A later mismatch prints a unified diff and the same acceptance line.
Acceptance is atomic and reviewed with `git diff`; under CI an update
request refuses the run (`WINDTRAP_UPDATE=force` overrides, for
generated-baseline pipelines that know what they are doing).

The rules:

- **Identity is the name**, never a source position — refactoring
  cannot orphan a baseline. Names match `[A-Za-z0-9._-]+` and must be
  unique (case-insensitively) among the snapshots of one source file.
  A duplicate — the same name checked again from a different call
  site, or by a different test when no call site is known — fails at
  the second check with both locations shown; repeating one call site
  (a loop, a retry, a `cases` family) is a recheck against the same
  baseline, not a duplicate.
- **Storage** is `<src_dir>/__snapshots__/<src_basename>/<name>.snap`,
  scoped to the file the enclosing test was declared in (or `?pos`'s
  file) — a snapshot reached through a helper in another file does not
  relocate its baseline.
- **Snapshots are line-oriented text**: CR/CRLF normalize to LF and a
  trailing newline is forced on both sides. If CR bytes or the missing
  final newline are the point, encode first (e.g. `String.escaped`).
  Redaction is ordinary code before the call:
  `snapshot "log" (mask_timestamps out)`.
- `snapshot_pp name pp v` snapshots a pretty-printed value.
- **Stale baselines**: a baseline whose test was deleted or renamed
  sits in the tree forever unless something notices. Only a *full,
  clean* run can notice — the whole declared suite executed, with
  nothing filtered, focused, bailed, skipped or failed — because only
  such a run knows every name the suite claims. After one, baselines
  no test checked are reported:

  ```
  stale baseline: test/__snapshots__/test_mytool/removed.snap
  remove stale baselines: dune exec test/test_mytool.exe -- -u --prune
  ```

  `--prune` (`WINDTRAP_PRUNE=1`) deletes them, after a full, clean
  *update* run. `--strict-snapshots` (`WINDTRAP_STRICT_SNAPSHOTS=1`)
  fails the run on them, exit code 1, with the paths and the way out
  in the failure block — that is how a suite asserts that its stored
  baselines are exactly the set it checks, so adding or removing a
  case cannot pass unnoticed. It is off by default; a suite that
  legitimately carries unchecked baselines keeps working.

  The two compose in one order: `--prune` deletes first,
  `--strict-snapshots` judges what survived. Together they mean
  "remove them, and fail if you could not" — a granted prune leaves
  nothing to fail on, a refused one leaves everything and says why.

  After any other run neither applies, silently: a filtered run cannot
  tell a stale baseline from one it did not select this time, so
  `--strict-snapshots` under `-f parser` reports nothing and fails
  nothing.

Baselines are runtime data, invisible to dune's dependency tracking —
add the glob or editing a baseline will not re-trigger the test:

```lisp
(test
 (name test_mytool)
 (libraries windtrap)
 (deps
  (glob_files_rec __snapshots__/**)))
```

## Captured output: `output ()`

The runner captures each test's standard output and error (C stubs and
subprocesses included). `output ()` consumes what was captured since
the test started or the previous call — assert on it directly, or feed
it to `snapshot`:

```ocaml
test "greeting goes through capture" (fun () ->
    print_string "Hello, World!\n";
    equal string "Hello, World!\n" (output ()))
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

When output changes, the failure shows the runner's diff and dune
offers a correction — accept with `dune promote` (or from your editor;
`dune runtest -w` for the loop):

```
$ dune runtest
parser: 1 test
F
──────────────────── failures (1) ────────────────────
  FAIL  Parser › tokenize
    lib/parser.ml:29
      29 │   [%expect {|

    --- expected
    +++ actual
    @@ -1,3 +1,4 @@
      INT 1
      PLUS
      INT 2
    + EOF
...
$ dune promote
```

Read every promoted diff as a code change: promotion is where bugs get
blessed as expected output.

The offer is perishable: dune rebuilds its pending-promotion set on
every invocation, so `dune promote` (or `dune promote lib/parser.ml`
to take one file) must directly follow the failing `dune runtest` —
run any other dune command in between and the set is cleared, with
nothing to promote until the next failing run records it again.

Mechanics worth knowing:

- `[%expect]` matches with ppx_expect's whitespace flexibility: lines
  are trimmed, blank edges dropped, and the block dedented before
  comparison, so payload indentation never causes a mismatch.
  `[%expect_exact {|…|}]` matches byte-for-byte.
- A test may hold several `[%expect]` nodes; each consumes the output
  since the previous one. Every reached node records its result, so
  one run corrects every stale payload; a node that is never reached
  is a loud failure, not a silent pass.
- `[%expect.output]` returns the captured output as a string for
  post-processing before your own assertion.
- Assertion failures and uncaught exceptions inside an expect test are
  ordinary failures, not corrections — `dune promote` can never bless an
  `equal` mismatch or a raise. To pin an expected exception, catch and
  print it: `(try boom () with e -> print_string (Printexc.to_string e));
  [%expect {| Failure("boom") |}]`.
  `dune promote` is per-library, not per-file: dune registers
  corrections only when every inline-test process of the library exits
  cleanly, so one raising test anywhere in the library withholds
  `dune promote` for *all* of the library's corrections — including
  other files'. The run still tells you what was computed: each
  `windtrap: wrote <file>.corrected` line names a correction, and the
  caveat under it says it is not registered yet.
- `WINDTRAP_UPDATE=1` is the way past that, and it is the same variable
  and the same act as for snapshot baselines: accept the output the run
  just produced. Corrections go straight into the source tree, one file
  at a time and without dune, so a crash in `b.ml` no longer withholds
  `a.ml`'s payload. An accepted mismatch is not reported as a failure —
  the run goes green and names what it wrote, exactly as an accepted
  baseline does:

```
$ WINDTRAP_UPDATE=1 dune runtest
mylib: 4 passed in 0.0031s.
windtrap: wrote parser.ml.corrected
windtrap: accepted into the source tree: lib/parser.ml
```

  What is promotable does not widen: a raise is still never a
  correction, so no update run can bless one, and a file whose run holds
  any non-expect failure — an assertion beside a stale payload, a crash
  in a sibling test — accepts nothing until those are fixed: what the
  variable removes is the *cross-file* veto, never the per-file one.
  Review with `git diff`. Under CI an update request refuses the run, as
  it does for baselines.
- Shadowing `Expect_test_config` tunes a whole file; the useful knob
  is `sanitize`, applied to every read of captured output:

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
… end` groups, and `[@tags "slow"]` tags.

## Adopting a ppx_expect suite

Most existing ppx_expect suites run unchanged after swapping the PPX
and the backend in the `dune` file — `(pps ppx_expect)` becomes
`(pps ppx_windtrap)`. The compatibility envelope is measured, not
promised: windtrap vendors the pinned upstream ppx_expect test corpus
and holds itself to reproducing upstream's `.corrected` files
byte-identically (see `test/conformance/RESULTS.md` for the current
numbers). Concretely:

- Honored: `let%expect_test`, `[%expect]`, `[%expect_exact]`,
  `[%expect.output]`, `{%expect|…|}` string-extension syntax, quoted
  payloads, functor-duplicated tests, output from C stubs — with
  corrections formatted exactly as ppx_expect formats them, so a suite
  whose payloads already carry ppx_expect's shape sees no formatting
  churn on first promote (the caveat below covers suites that don't).
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

The first-promote caveat: a correction rewrites its file's
expectations wholesale. In a file with at least one correction, every
`[%expect]` node of the file's resolved tests is re-rendered in the
standard shape, not just the failing ones. Payloads already in
ppx_expect's shape reproduce byte-identically — that is the no-churn
case above; payloads that carry any other formatting — hand-formatted
blocks, or a suite adopted from windtrap 0.1 — are canonicalized in
the same diff. Expect the first promote after such an adoption to
reformat whole files at once, and review that diff as one-time
formatting plus the real changes. A file with no corrections is never
rewritten, so a matching suite stays byte-stable whatever shape its
payloads are in.

## Choosing

| Output | Use |
| --- | --- |
| Short, review-worthy, produced by printing | `[%expect]` |
| Large or generated (help text, JSON, renders) | `snapshot` |
| Needs masking or custom comparison | `output ()` + ordinary assertions |
