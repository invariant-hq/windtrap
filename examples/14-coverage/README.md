# `14-coverage`

Code coverage. Windtrap has built-in coverage instrumentation — no external
tools needed.

```bash
dune exec ./examples/14-coverage/main.exe --instrument-with windtrap
```

## What You'll Learn

- Enabling coverage instrumentation in dune
- Running tests with `--instrument-with`
- Viewing inline coverage results
- Generating per-file coverage reports with `windtrap coverage`
- Viewing uncovered source snippets with `windtrap coverage -u`
- Exporting machine-readable uncovered lines with `windtrap coverage -j`

## Dune Configuration

Add an `instrumentation` stanza to the library or executable you want to
measure:

```dune
(executable
 (name main)
 (libraries windtrap)
 (instrumentation
  (backend windtrap)))
```

Coverage is only activated when you pass `--instrument-with windtrap` to
dune. Without that flag, the stanza is ignored and there is zero overhead.

## How It Works

1. **Instrumentation**: `--instrument-with windtrap` inserts visit counters at
   every expression in your code at compile time.
2. **Execution**: When tests run, counters are incremented for each visited
   expression.
3. **Reporting**: An inline coverage percentage is printed after the test
   summary. Coverage data is also written to `_build/_coverage/` as `.coverage`
   files.

## Viewing Reports

After running with instrumentation:

```bash
# Inline summary (printed automatically after test results)
# Coverage: 20.47% of expressions.

# Per-file breakdown with uncovered line ranges (default)
dune exec windtrap -- coverage

# Uncovered source snippets (most useful for iterating)
dune exec windtrap -- coverage -u

# Machine-readable report (for scripts/agents)
dune exec windtrap -- coverage -j

# One-line summary only
dune exec windtrap -- coverage --summary-only
```

## Coverage Attributes

Exclude code from coverage with attributes:

```ocaml
(* Exclude a single expression *)
let debug () = (print_endline "debug" [@coverage off])

(* Exclude a region *)
[@@coverage.off]
let internal_helper () = ...
[@@coverage.on]

(* Exclude an entire file *)
[@@coverage.exclude_file]
```

## Environment Variables

| Variable | Purpose |
|----------|---------|
| `WINDTRAP_COVERAGE_FILE` | Override the `.coverage` output file path |
| `WINDTRAP_COVERAGE_LOG` | Error logging: `SILENT`, `STDERR`, or a file path |

## Try It

1. Run without `--instrument-with` and note there is no coverage line.
2. Run with `--instrument-with windtrap` and observe the coverage
   percentage.
3. Add a test for the untested `Triangle` branch of `perimeter` and see the
   coverage increase.

## Next Steps

You've completed the Windtrap examples. For more information:

- API documentation: `dune build @doc`
- Source code: [github.com/invariant-hq/windtrap](https://github.com/invariant-hq/windtrap)
