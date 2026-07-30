# Windtrap manual

One library for all your OCaml tests: unit, property, snapshot, and
expect tests from one flat API, plus coverage. This manual is the
long-form companion to the API reference in `lib/windtrap.mli` — the
reference is the contract; these chapters show the workflows.

Read [Getting started](getting-started.md) first. After that, chapters
are independent — go where your suite needs you:

| Chapter | What it covers |
| --- | --- |
| [Getting started](getting-started.md) | Install, first suite, first failure — five minutes |
| [Assertions](assertions.md) | The sixteen verbs, testables, `Exn` predicates, failure output |
| [Property testing](property-testing.md) | `prop`, `Gen`, shrinking, seeds and replay, distribution checks |
| [Snapshots and expect tests](snapshots-and-expect.md) | File baselines, `[%expect]` + `dune promote`, adopting ppx_expect |
| [Resources and structure](resources-and-structure.md) | `bracket`, `fixture`, temp paths, `cases`, tags, focus, `xfail` |
| [Running tests](running-tests.md) | The CLI and its `WINDTRAP_*` mirrors, selection, sharding, CI output |
| [Coverage](coverage.md) | The one-stanza setup, report modes, `windtrap coverage` |
| [Cookbook](../cookbook.md) | Recipes windtrap deliberately does not absorb |

Every OCaml snippet in these chapters is compiled by the mirrors in
[`snippets/`](snippets/) — a snippet that rots breaks the build — and
every transcript is captured from the real runner (paths and suite
names adapted to the chapter's story; timings vary by machine).

Migrating from windtrap 0.1? The 0.2.0 entry in
[`CHANGES.md`](../../CHANGES.md) doubles as the migration reference.
Contributing? Start with [`doc/dev/architecture.md`](../dev/architecture.md).
