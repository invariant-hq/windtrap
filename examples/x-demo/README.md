# `x-demo`

A visual showcase of Windtrap test output. This example includes intentional
failures to demonstrate what error messages, diffs, and counterexamples look
like.

```bash
dune exec ./examples/x-demo/main.exe
```

## What This Shows

| Section | Demonstrates |
|---------|--------------|
| Passing | Nested groups, various assertions |
| Failing Assertions | Equality, option, result, boolean mismatches |
| Failing Exceptions | Wrong exception, missing exception, unexpected exception |
| Expect | Output capture matching and mismatches |
| Snapshots | Single-line and multiline diffs |
| Properties | Shrunk counterexamples |
| Skipped | Conditional test skipping with reasons |
| Slow | Tests marked for `-q` filtering |
| Custom Messages | Contextual failure info with `~msg` |

## Sample Output

```
Testing Windtrap Demo.

› Passing
  › Math
    › Arithmetic
      [PASS] addition                                                      <1ms
      [PASS] subtraction                                                   <1ms
      [PASS] multiplication                                                <1ms
    › Comparisons
      [PASS] less than                                                     <1ms
      [PASS] greater than                                                  <1ms
  ...

› Failing Assertions
  › Equality
  > [FAIL] int mismatch                                                    <1ms
    [FAIL] string mismatch                                                 <1ms
    [FAIL] list mismatch                                                   <1ms
  ...

› Skipped
  [SKIP] platform check                                                    <1ms
  [SKIP] feature flag                                                      <1ms
  ...
```

## Failure Output Examples

### Assertion Failure

```
┌──────────────────────────────────────────────────────────────────────────────┐
│ [FAIL] Windtrap Demo › Failing Assertions › Equality › int mismatch          │
└──────────────────────────────────────────────────────────────────────────────┘
Values are not equal
  Expected: 42
  Actual  : 99
```

### Snapshot Diff

```
┌──────────────────────────────────────────────────────────────────────────────┐
│ [FAIL] Windtrap Demo › Snapshots › Failing › multiline diff                  │
└──────────────────────────────────────────────────────────────────────────────┘
Snapshot mismatch: __snapshots__/main/document__L169_C28.snap

--- snapshot
+++ actual
@ -1,5 +1,5 @
  {
+   "name": "Bob",
-   "name": "Alice",
    ...
  }
```

### Property Counterexample

```
┌──────────────────────────────────────────────────────────────────────────────┐
│ [FAIL] Windtrap Demo › Properties › Failing › all numbers are small          │
└──────────────────────────────────────────────────────────────────────────────┘
Property failed after 1 test, shrunk in 24 steps (seed=359172598)
Counterexample: 100
Replay with: WINDTRAP_SEED=359172598
```

## Useful Commands

```bash
# Run only passing tests
dune exec ./examples/x-demo/main.exe -- "Passing"

# Stop on first failure
dune exec ./examples/x-demo/main.exe -- -x

# Skip slow tests
dune exec ./examples/x-demo/main.exe -- -q

# List all tests without running
dune exec ./examples/x-demo/main.exe -- -l

# Compact output for CI
dune exec ./examples/x-demo/main.exe -- --format compact

# Update snapshots to accept current output
dune exec ./examples/x-demo/main.exe -- -u
```
