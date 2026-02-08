# `02-groups`

Organize tests into hierarchical groups. Groups make test output easier to read
and let you apply shared configuration to multiple tests.

```bash
dune exec ./examples/02-groups/main.exe
```

## What You'll Learn

- Organizing tests with `group`
- Nesting groups for hierarchy
- Mixing groups and standalone tests

## Key Functions

| Function | Purpose |
|----------|---------|
| `group` | Creates a named container for tests and subgroups |

## Output Walkthrough

When you run this example, you'll see:

```
Testing Groups.

..........

All tests passed in <1ms. 10 tests run.
```

Use `-v` for verbose output to see the group hierarchy:

```
Testing Groups.

› Math
  › Addition
    PASS positive numbers 43μs
    PASS negative numbers 37μs
    PASS with zero 35μs
  › Multiplication
    PASS positive numbers 36μs
    PASS by zero 35μs
    PASS by one 35μs
› Comparisons
  PASS less than 37μs
  PASS greater than 35μs
  PASS equality 35μs
PASS standalone test 36μs

All tests passed in <1ms. 10 tests run.
```

The indentation shows how groups nest. `Math` contains `Addition` and
`Multiplication`, each with their own tests.

## Try It

1. Add a new group called `Division` under `Math`.
2. Move the `standalone test` into a new group called `Misc`.

## Next Steps

Continue to [03-assertions](../03-assertions/) to learn about the assertion
toolkit.
