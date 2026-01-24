# `03-assertions`

The assertion toolkit. Windtrap provides assertions for common patterns, with
clear failure messages that include source locations.

```bash
dune exec ./examples/03-assertions/main.exe
```

## What You'll Learn

- Equality assertions: `equal`, `not_equal`
- Boolean assertions: `is_true`, `is_false`
- Option assertions: `is_some`, `is_none`, `some`
- Result assertions: `is_ok`, `is_error`, `ok`, `error`
- Adding custom failure messages with `~msg`

## Key Functions

| Function | Purpose |
|----------|---------|
| `equal` | Asserts two values are equal |
| `not_equal` | Asserts two values are different |
| `is_true` | Asserts a boolean is true |
| `is_false` | Asserts a boolean is false |
| `is_some` | Asserts an option is `Some _` |
| `is_none` | Asserts an option is `None` |
| `some` | Asserts an option is `Some` with a specific value |
| `is_ok` | Asserts a result is `Ok _` |
| `is_error` | Asserts a result is `Error _` |
| `ok` | Asserts a result is `Ok` with a specific value |
| `error` | Asserts a result is `Error` with a specific value |

## Failure Messages

All assertions accept an optional `~msg` parameter. When a test fails, the
message appears in the output to help identify the issue:

```ocaml
equal ~msg:"user count should match" Testable.int 5 actual_count
```

## Try It

1. Change one assertion to fail and observe the error message.
2. Use `~msg` to add context to an assertion.

## Next Steps

Continue to [04-exceptions](../04-exceptions/) to learn about testing
exceptions.
