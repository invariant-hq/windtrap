# Windtrap manual

One library for all your OCaml tests: unit, property, stateful, snapshot
and expect tests from one flat API, plus coverage and mutation testing.
This manual is the long-form companion to the API reference in
`lib/windtrap.mli` — the reference is the contract; these chapters show
the workflows.

Read [Getting started](getting-started.md) first. After that, chapters
are independent — go where your suite needs you:

| Chapter | What it covers |
| --- | --- |
| [Getting started](getting-started.md) | Install, first suite, first failure — five minutes |
| [Assertions](assertions.md) | The assertion verbs, testables, `Exn` predicates, failure output |
| [Property testing](property-testing.md) | `prop`, `Gen`, shrinking, seeds and replay, distribution checks |
| [Stateful testing](stateful-testing.md) | `stateful`, `command`, models and preconditions, per-case systems, cost |
| [Snapshots and expect tests](snapshots-and-expect.md) | File baselines, `[%expect]` + `dune promote`, adopting ppx_expect |
| [Resources and structure](resources-and-structure.md) | `bracket`, `scoped`, `fixture`, temp paths, `setenv`, `chdir`, `cases`, tags, focus, `xfail` |
| [Running tests](running-tests.md) | The CLI and its `WINDTRAP_*` mirrors, selection, sharding, CI output |
| [Coverage](coverage.md) | The one-stanza setup, the inline number, `windtrap coverage` and its gate |
| [Mutation testing](mutation.md) | The second backend, survivors and their witnesses, arming one mutant, admitting a test, `windtrap mutate` |
| [Cookbook](../cookbook.md) | Recipes windtrap deliberately does not absorb |

Every OCaml snippet in these chapters is compiled by a mirror in
[`snippets/`](snippets/), so a snippet that rots breaks the build.

Transcripts are different, and deliberately so: each is captured from a
real run and then adapted by hand to the chapter's story — paths, line
numbers and suite names rewritten, timings left as measured. Nothing
checks them, so a renderer change is a reason to re-capture the ones it
touches; `doc/dev/release.md` says so on the release bar.

Migrating from windtrap 0.1? The 0.2.0 entry in
[`CHANGES.md`](../../CHANGES.md) doubles as the migration reference.
Contributing? Start with [`doc/dev/architecture.md`](../dev/architecture.md).
