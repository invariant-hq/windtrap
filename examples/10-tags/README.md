# `10-tags`

Tagging and filtering tests. Tags let you categorize tests and run specific
subsets from the command line.

```bash
dune exec ./examples/10-tags/main.exe
```

## What You'll Learn

- Adding tags to tests and groups with `~tags`
- Creating tag sets with `Tag.labels`
- Filtering tests by name or tag
- Combining tags with the `slow` modifier

## Key Functions

| Function | Purpose |
|----------|---------|
| `~tags:(Tag.labels [...])` | Adds tags to a test or group |
| `~filter` (in `run`) | Filters tests by pattern |
| `--filter` (CLI) | Filters tests at runtime |

## Adding Tags

Apply tags to individual tests or entire groups:

```ocaml
(* Tag a single test *)
test ~tags:(Tag.labels ["integration"]) "api call" (fun () -> ...)

(* Tag all tests in a group *)
group "Database" ~tags:(Tag.labels ["integration"; "slow"]) [
  test "connects" (fun () -> ...);
  test "queries" (fun () -> ...);
]
```

## Filtering Tests

Run specific tests by name or tag:

```bash
# Run tests with "database" in the name
dune exec ./examples/10-tags/main.exe -- --filter "database"

# Run only integration tests
dune exec ./examples/10-tags/main.exe -- --filter "integration"

# Skip slow tests
dune exec ./examples/10-tags/main.exe -- --quick
```

## Common Tag Patterns

| Tag | Purpose |
|-----|---------|
| `integration` | Tests that use external services |
| `slow` | Tests that take significant time |
| `flaky` | Tests that sometimes fail |
| `wip` | Work-in-progress tests |

## Try It

1. Run with `--filter "Integration"` to run only integration tests.
2. Add a new tag and filter for it.

## Next Steps

Continue to [11-inline-tests](../11-inline-tests/) to learn about inline expect tests.
