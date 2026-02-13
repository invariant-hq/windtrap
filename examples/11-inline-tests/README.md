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

Add PPX preprocessing to your library:

```dune
(library
 (name mylib)
 (libraries windtrap)
 (inline_tests)
 (preprocess (pps windtrap.ppx)))
```

### Executable alternative

Libraries with `(inline_tests)` are included in releases, which may be
undesirable. You can use a standard executable instead by setting the
`WINDTRAP_PROMOTE` environment variable and adding a `(diff?)` rule:

```dune
(executable
 (name my_test)
 (libraries windtrap)
 (preprocess (pps windtrap.ppx)))

(rule
 (alias runtest)
 (action
  (progn
   (setenv WINDTRAP_PROMOTE true
    (run ./my_test.exe))
   (diff? my_test.ml my_test.ml.corrected))))
```

The test source file is identical in both cases. `WINDTRAP_PROMOTE` tells
windtrap to write `.corrected` files on mismatch, and `(diff?)` lets dune
detect the diff and register it for `dune promote`.

For multi-file tests, add a `(diff?)` entry per file that contains `[%expect]`
nodes.

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
