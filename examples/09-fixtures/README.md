# `09-fixtures`

Setup and teardown with fixtures. Use `bracket` for tests that need resources
acquired before and released after execution.

```bash
dune exec ./examples/09-fixtures/main.exe
```

## What You'll Learn

- Resource management with `bracket`
- Creating reusable test constructors
- Marking slow tests with `slow`
- Setting per-test timeouts

## Key Functions

| Function | Purpose |
|----------|---------|
| `bracket` | Creates test with setup and teardown |
| `slow` | Marks test as slow (skipped with `--quick`) |
| `~timeout` | Sets maximum test duration in seconds |

## The Bracket Pattern

`bracket` ensures resources are properly released even if tests fail:

```ocaml
bracket
  ~setup:(fun () -> open_connection ())
  ~teardown:(fun conn -> close_connection conn)
  "test name"
  (fun conn -> (* use conn *))
```

## Reusable Fixtures

Partially apply `bracket` to create reusable test constructors:

```ocaml
let with_db = bracket
  ~setup:Database.connect
  ~teardown:Database.disconnect

(* Use it for multiple tests *)
group "Database" [
  with_db "can query" (fun db -> ...);
  with_db "can insert" (fun db -> ...);
]
```

## Slow Tests

Mark expensive tests so they can be skipped:

```ocaml
slow "performance benchmark" (fun () ->
    let result = run_expensive_operation () in
    is_true result)
```

Run with `--quick` to skip slow tests.

## Try It

1. Create a fixture for temporary files.
2. Add a test with `~timeout:0.001` to see timeout failures.

## Next Steps

Continue to [10-tags](../10-tags/) to learn about tagging and filtering.
