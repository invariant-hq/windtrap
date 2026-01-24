# `04-exceptions`

Testing exceptions. Verify that code raises specific exceptions, matches
exception patterns, or runs without raising.

```bash
dune exec ./examples/04-exceptions/main.exe
```

## What You'll Learn

- Checking for exact exceptions with `raises`
- Pattern matching on exceptions with `raises_match`
- Verifying no exception is raised with `no_raise`

## Key Functions

| Function | Purpose |
|----------|---------|
| `raises` | Asserts that a function raises a specific exception |
| `raises_match` | Asserts that a function raises an exception matching a predicate |
| `no_raise` | Asserts that a function completes without raising, returning its result |

## Usage Patterns

### Exact Exception Matching

```ocaml
test "fails with message" (fun () ->
    raises (Failure "invalid input") (fun () -> validate input))
```

### Pattern Matching

```ocaml
test "fails with some message" (fun () ->
    raises_match
      (function Failure msg -> String.length msg > 0 | _ -> false)
      (fun () -> validate input))
```

### Capturing Return Value

```ocaml
test "succeeds with value" (fun () ->
    let result = no_raise (fun () -> compute data) in
    equal Testable.int 42 result)
```

## Try It

1. Define a new custom exception and test for it.
2. Use `raises_match` to check exception message content.

## Next Steps

Continue to [05-testables](../05-testables/) to learn about type-safe
comparisons.
