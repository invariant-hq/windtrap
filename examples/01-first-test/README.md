# `01-first-test`

Your first Windtrap test. This example shows the minimal setup needed to write
and run tests with Windtrap.

```bash
dune exec ./examples/01-first-test/main.exe
```

## What You'll Learn

- Opening the `Windtrap` module to access the testing API
- Creating tests with `test`
- Running tests with `run`
- Comparing values with `equal` and `Testable.int`

## Key Functions

| Function | Purpose |
|----------|---------|
| `run` | Entry point that runs all tests and reports results |
| `test` | Creates a single test case with a name and function |
| `equal` | Asserts that two values are equal |
| `Testable.int` | Tells Windtrap how to compare and print integers |

## Output Walkthrough

When you run this example, you'll see:

```
Testing First Test.

...

All tests passed in <1ms. 3 tests run.
```

By default, output uses compact format (one dot per passing test). Use `-v` for
verbose output showing each test name and status.

## Try It

1. Change `equal Testable.int 5 (add 2 3)` to `equal Testable.int 6 (add 2 3)`
   and run again to see what a failing test looks like.

2. Add a new test case for multiplication.

## Next Steps

Continue to [02-groups](../02-groups/) to learn how to organize tests.
