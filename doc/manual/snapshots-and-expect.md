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

    snapshot "help": no baseline at /home/dev/mytool/test/__snapshots__/test_mytool/help.snap
    proposed (5 lines):
      ┆ Usage: mytool [OPTIONS] COMMAND
      ┆
      ┆ Commands:
      ┆   build    Build the project
      ┆   test     Run the tests
    accept: dune exec test/test_mytool.exe -- -u, then review with git diff
──────────────────────────────────────────────────────
1 failed in 0.00109s.
rerun failures only: dune exec test/test_mytool.exe -- --failed

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
- **Orphans**: after a full, clean update run, `--prune`
  (`WINDTRAP_PRUNE=1`) deletes baselines no longer claimed by any
  test; without it they are reported.

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

Mechanics worth knowing:

- `[%expect]` matches with ppx_expect's whitespace flexibility: lines
  are trimmed, blank edges dropped, and the block dedented before
  comparison, so payload indentation never causes churn.
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
  corrections formatted as ppx_expect formats them, so first promote
  produces no formatting churn.
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
| Short, review-worthy, produced by printing | `[%expect]` |
| Large or generated (help text, JSON, renders) | `snapshot` |
| Needs masking or custom comparison | `output ()` + ordinary assertions |
