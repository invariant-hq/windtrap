# `13-cli`

Command-line options. Windtrap tests accept options for filtering, output
format, and more.

```bash
dune exec ./examples/13-cli/main.exe -- --help
```

## What You'll Learn

- Available command-line options
- Environment variable configuration
- Common usage patterns

## Command-Line Options

### Common Options

| Option | Purpose |
|--------|---------|
| `-s, --stream` | Stream test output to console (don't capture) |
| `-v, --verbose` | Verbose output (alias for `--format verbose`) |
| `-q, --quick` | Skip slow tests |
| `-x, --fail-fast` | Stop on first failure |
| `-l, --list` | List tests without running |
| `-u, --update` | Update snapshots |
| `-f, --filter PATTERN` | Filter tests by name (or use positional arg) |

### Output

| Option | Purpose |
|--------|---------|
| `--format FMT` | Output format: verbose, compact, tap, junit |
| `--junit PATH` | Write JUnit XML to file |

### Other

| Option | Purpose |
|--------|---------|
| `--seed N` | Random seed for property tests |
| `--timeout N` | Default timeout in seconds |
| `-o, --output DIR` | Directory for test logs |

## Usage Examples

```bash
# List all tests
dune exec ./examples/13-cli/main.exe -- -l

# Run only unit tests
dune exec ./examples/13-cli/main.exe -- "Unit"

# Skip slow tests
dune exec ./examples/13-cli/main.exe -- -q

# Stream output to console
dune exec ./examples/13-cli/main.exe -- -s

# Compact output for CI
dune exec ./examples/13-cli/main.exe -- --format compact

# Stop on first failure
dune exec ./examples/13-cli/main.exe -- -x
```

## Environment Variables

| Variable | Purpose |
|----------|---------|
| `WINDTRAP_STREAM` | Stream output (1/true/yes) |
| `WINDTRAP_FORMAT` | Output format (verbose/compact/tap/junit) |
| `WINDTRAP_FILTER` | Filter pattern |
| `WINDTRAP_UPDATE` | Update snapshots (1/true/yes) |
| `WINDTRAP_SEED` | Random seed for property tests |
| `WINDTRAP_COLOR` | Color output (always/never/auto) |

## Programmatic Options

Pass options to `run` directly:

```ocaml
run ~stream:true ~quick:true "My Tests" [...]
```

## CI Configuration

For continuous integration, use compact mode with fail-fast:

```bash
dune exec ./mytest.exe -- --format compact -x
```

## Try It

1. Run with `-l` to see all test names.
2. Use a positional filter to run a subset of tests.
3. Compare default and `--format compact` output.

## Congratulations!

You've completed the Windtrap examples. For more information:

- API documentation: `dune build @doc`
- Source code: [github.com/tmattio/windtrap](https://github.com/tmattio/windtrap)
