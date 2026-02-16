# `12-ppx`

PPX test syntax. The `ppx_windtrap` preprocessor provides `let%test`,
`let%expect_test`, and `module%test` for writing self-contained test
executables with hierarchical grouping and expect tests. Tests auto-execute
when the program exits.

```bash
dune exec ./examples/12-ppx/main.exe
```

## What You'll Learn

- Writing boolean tests with `let%test`
- Grouping tests with `module%test`
- Auto-execution (no `[%%run_tests]` needed)

## Key Syntax

| Syntax | Purpose |
|--------|---------|
| `let%test "name" = expr` | Declare a test (body uses assertions) |
| `let%expect_test "name" = ...` | Output comparison test |
| `module%test Name = struct ... end` | Group tests into a hierarchy |
| `[%%run_tests "Suite Name"]` | Optional: set a custom suite name |

## Dune Configuration

Module-based PPX tests use a regular executable:

```dune
(executable
 (name main)
 (libraries windtrap)
 (preprocess (pps ppx_windtrap)))
```

## PPX Syntax Overview

| Syntax | Purpose |
|--------|---------|
| `let%test "name" = expr` | Assertion-based test |
| `let%expect_test "name" = ...` | Output comparison test |
| `module%test Name = struct ... end` | Group tests into a hierarchy |
| `[%%run_tests "Suite"]` | Optional: set a custom suite name |

Tests auto-execute when the program exits. All PPX test forms can be mixed
freely in the same file.

## Try It

1. Add a new `module%test` group for list operations.
2. Try nesting `module%test` three levels deep.
3. Run with `-v` to see verbose output with group hierarchy.

## Next Steps

Continue to [13-cli](../13-cli/) to learn about command-line options.
