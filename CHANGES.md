# Changelog

All notable changes to this project will be documented in this file.

- Only document user-facing changes (features, bug fixes, performance improvements, API changes, etc.)
- Add new entries at the top of the appropriate section (most recent first)

## [Unreleased]

Windtrap is an all-in-one OCaml testing framework that unifies unit tests, property-based tests, snapshot tests, and expect tests under a single API. Instead of juggling multiple testing libraries, Windtrap gives you one cohesive package with a PPX for inline expect tests (`ppx_windtrap`).

- `windtrap coverage` now defaults to per-file output with uncovered line ranges (use `--summary-only` for the old one-line summary).
- `windtrap coverage` auto-detects source paths (defaults to `.`), removing the need to pass `--source-path .` explicitly.
- `windtrap coverage` accepts positional arguments as coverage paths (e.g., `windtrap coverage _build/_coverage/`).
- `windtrap coverage` now supports short flags: `-u` (`--show-uncovered`), `-j` (`--json`), `-C` (`--context`).
- `windtrap coverage --show-uncovered` displays uncovered source code snippets with context lines, colored for terminal output.
- `windtrap coverage --json` now emits machine-readable per-file reports with uncovered offsets and uncovered line numbers.
- `windtrap coverage --coverage-path` now accepts both directories and explicit `.coverage` files, with deterministic file ordering.

- Unit tests with combinators, tags, skip, brackets, and timeouts.
- Property-based testing with configurable seeds and shrinking.
- Snapshot testing with automatic file management and diffing.
- Inline expect tests via `ppx_windtrap` with automatic correction.
- CLI test runner with filtering, verbosity, and color support.
