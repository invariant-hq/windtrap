# `06-expect`

Output testing with expect. Capture stdout and compare it against expected
values, with automatic whitespace normalization.

```bash
dune exec ./examples/06-expect/main.exe
```

## What You'll Learn

- Capturing and comparing output with `expect`
- Exact matching with `expect_exact`
- Chaining multiple expects in one test
- Capturing output while keeping return values with `capture`
- Accessing raw output for post-processing with `output`

## Key Functions

| Function | Purpose |
|----------|---------|
| `expect` | Asserts captured output matches expected (normalized) |
| `expect_exact` | Asserts captured output matches exactly |
| `capture` | Runs function, captures output, and returns result |
| `output` | Returns raw captured output for processing |

## Normalization

By default, `expect` normalizes whitespace:
- Trailing whitespace on lines is removed
- Leading/trailing blank lines are removed
- Multiple blank lines collapse to one

Use `expect_exact` when whitespace matters.

## Sequential Expects

Each `expect` clears the output buffer:

```ocaml
test "multi-step" (fun () ->
    print_string "Step 1";
    expect "Step 1";
    print_string "Step 2";
    expect "Step 2")
```

## Post-Processing Output

Use `output` when you need to transform output before comparing:

```ocaml
test "mask timestamps" (fun () ->
    Printf.printf "Time: %dms" elapsed;
    let out = output () in
    let masked = Str.global_replace (Str.regexp "[0-9]+") "N" out in
    equal Testable.string "Time: Nms" masked)
```

## Try It

1. Add trailing spaces to output and see how `expect` handles them.
2. Use `capture` to test a function that prints and returns a value.

## Next Steps

Continue to [07-snapshots](../07-snapshots/) to learn about snapshot testing.
