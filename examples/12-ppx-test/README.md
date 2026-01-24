# `12-ppx-test`

Module-based test syntax with PPX. The `ppx_windtrap` preprocessor provides
`let%test`, `module%test`, and `[%%run_tests]` for writing self-contained test
executables with hierarchical grouping.

```bash
dune exec ./examples/12-ppx-test/main.exe
```

## What You'll Learn

- Writing boolean tests with `let%test`
- Grouping tests with `module%test`
- Running test suites with `[%%run_tests]`

## Key Syntax

| Syntax | Purpose |
|--------|---------|
| `let%test "name" = expr` | Declare a test (body uses assertions) |
| `module%test Name = struct ... end` | Group tests into a hierarchy |
| `[%%run_tests]` | Execute all registered tests |
| `[%%run_tests "Suite Name"]` | Execute with a custom suite name |

## Dune Configuration

Module-based PPX tests use a regular executable:

```dune
(executable
 (name main)
 (libraries windtrap)
 (preprocess (pps ppx_windtrap)))
```

## PPX Expect Tests vs PPX Module Tests

| | `let%expect_test` ([11-ppx](../11-ppx/)) | `let%test` (this example) |
|---|---|---|
| **Paradigm** | Output comparison | Assertion-based |
| **Grouping** | Flat | Hierarchical via `module%test` |
| **Runner** | `dune runtest` (inline tests) | `dune exec` (standalone) |
| **Best for** | Testing print output | Structured test suites |

These two paradigms use different runtimes and cannot be mixed in the same file.

## Try It

1. Add a new `module%test` group for list operations.
2. Try nesting `module%test` three levels deep.
3. Run with `--format compact` to see compact output.

## Next Steps

Continue to [13-cli](../13-cli/) to learn about command-line options.
