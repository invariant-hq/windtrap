# `11-inline-tests`

Inline tests with PPX. The `windtrap.ppx` preprocessor provides a concise
syntax for expect tests embedded directly in source files.

```bash
dune runtest
```

## What You'll Learn

- Writing inline tests with `let%expect_test`
- Checking output with `[%expect]` (normalized) and `[%expect_exact]` (exact)
- Capturing output with `[%expect.output]` for post-processing

## Syntax Reference

### Inline Expect Tests

| Syntax | Purpose |
|--------|---------|
| `let%expect_test "name" = ...` | Declare an inline expect test |
| `[%expect {|...\|}]` | Assert normalized output matches |
| `[%expect_exact {|...\|}]` | Assert output matches exactly |
| `[%expect.output]` | Return captured output as a string |

These tests run via dune's inline test runner (`dune runtest`).

## Dune Configuration

Add PPX preprocessing to your library or executable:

```dune
(library
 (name mylib)
 (libraries windtrap)
 (inline_tests)
 (preprocess (pps windtrap.ppx)))
```

## When to Use PPX vs Runtime API

| Use Case | Approach |
|----------|----------|
| Quick output checks | PPX |
| Shared test setup | Runtime API |
| Property testing | Runtime API |
| Complex assertions | Runtime API |

## Try It

1. Add a new `let%expect_test` for a function in your code.
2. Use `[%expect.output]` to capture and transform output.

## Next Steps

Continue to [12-ppx](../12-ppx/) to learn about PPX test syntax for executables.
