# Resources and structure

Windtrap has no group-level hooks — no user code ever runs outside a
test's exception boundary. Resources are scoped by three constructors
instead — one for a resource setup can return, one for a resource that
is only ever handed to a callback, one shared by the whole run — and
everything else here shapes the suite: table-driven tests, tags,
focus, and known-bug bookkeeping.

## Per-test resources: `bracket`

`bracket ~setup ~teardown name fn` runs `setup ()`, passes the
resource to the body, and runs `teardown` on it iff setup succeeded —
on every outcome, including failure, skip, and timeout. A body failure
and a teardown failure are reported independently; neither masks the
other. Partial application builds reusable constructors:

```ocaml
let with_db = bracket ~setup:Db.connect ~teardown:Db.close

let tests =
  [
    with_db "insert then get" (fun db ->
        Db.insert db "alice";
        equal int 1 (Db.count db));
  ]
```

## Callback-scoped resources: `scoped`

`bracket` needs a resource that setup can *return*. Most OCaml
resources are never handed over that way — they are handed to a
callback and reclaimed when it returns:

```
Eio_main.run @@ fun env -> ...
Eio.Switch.run @@ fun sw -> ...
In_channel.with_open_text path @@ fun ic -> ...
Mutex.protect m @@ fun () -> ...
```

There is no moment inside such a function at which the resource could
be returned, so no `~setup`/`~teardown` pair expresses it without
threads or effects. `scoped scope name fn` takes the scoping function
itself; windtrap calls it once, with a callback that runs the body:

```ocaml
let with_conn = scoped Pool.with_connection

let tests =
  [ with_conn "counts rows" (fun conn -> equal int 0 (Pool.count conn)) ]
```

The scope is positional and comes *before* the optional arguments, so
a partially applied constructor keeps them:
`with_conn ~timeout:30. "slow query" fn` is well typed.

**Cleanup is the scope's, not windtrap's.** This is the one place the
library promises less than `bracket` does. With `bracket`, windtrap
calls `teardown` and guarantees it on every outcome. With `scoped`,
windtrap never sees the resource: it records the body's failure — an
assertion, a `skip`, a timeout — and re-raises it *through* the scope,
so a scope that cancels or cleans up on the exception path does so.
Whether it does is the scope's contract. `Eio_main.run` and anything
built on `Fun.protect` reclaim on both paths;
`let r = acquire () in fn r; release r` leaks whenever the body fails,
and windtrap cannot fix that from the outside.

**The callback must be called exactly once.** A scope that returns
without calling it fails the test — a body that never ran is not a
pass, and a silent green here would be the worst outcome available. A
scope that calls it twice runs the body on the first call only and
fails the test: one execution per test is what snapshot registration,
`subtest` labels and scratch paths are keyed by (use `cases` or
`~retries` to repeat a body). A scope that *skips* instead of calling
back is a skip, not a missing body — the pattern for a suite gated on
a resource the machine does not have.

**Failures are attributed by how far the callback got.** What the body
raises is the body's failure. What the scope raises before the
callback is a `[setup]` failure and what it raises after the callback
returned is a `[teardown]` failure, so a scope that cannot acquire
reads differently from one that cannot release; a release failure that
replaces the body's exception is reported alongside it, two entries,
as under `bracket`. `~timeout` covers the whole scope call, and the
window is re-armed as the body leaves the callback, so a release that
blocks after a body timeout is cut short rather than left to hang the
run.

## Run-scoped resources: `fixture`

`fixture ?teardown create` returns an accessor for a resource shared
across the run. Nothing runs at creation; the first call inside a test
acquires (inside *that* test's failure boundary), later calls return
the cached value, and the runner releases acquired fixtures after the
last test in reverse acquisition order — on every path where it
regains control, `--bail` included:

```ocaml
let server = fixture ~teardown:Server.stop Server.start

let tests =
  [ test "responds" (fun () -> is_true (Server.ping (server ()))) ]
```

A fixture no selected test touches is never acquired. A `skip` raised
during acquisition is cached: every test using the fixture skips with
the same reason — the pattern for suites gated on an unavailable
device (see the [cookbook](../cookbook.md)).

Release happens after the last test, which puts it outside every
per-test timeout: there is no window left to inherit and no limit to
fall back on, so a `teardown` that blocks hangs the run after the last
result — the same code in a `bracket` teardown would be cut short by
the test's limit. Give a `teardown` that waits on the outside world
its own deadline. Each release is announced before it runs
(`releasing fixture (test/test_mytool.ml:12)`), so a hang names the
fixture.

## Scratch paths: `temp_dir` and `temp_file`

```ocaml
test "writes a config" (fun () ->
    let dir = temp_dir () in
    let file = Filename.concat dir "config.json" in
    Config.write file;
    is_true (Sys.file_exists file))
```

Fresh paths owned by the runner, removed after the test on every
outcome — there is no lifecycle to write. Paths are per test attempt:
anything that must outlive the test (a fixture's resource) must not
live in them.

## Process state: `setenv` and `chdir`

```ocaml
test "reads the token from the environment" (fun () ->
    setenv "API_TOKEN" (Some "t-123");
    equal (option string) (Some "t-123") (Config.token ());
    setenv "API_TOKEN" None;
    equal (option string) None (Config.token ()))
```

The environment and the working directory belong to the process, not to
the test, so nothing scopes them but putting them back — which is what
the runner does when the test ends, on every outcome and once per
`~retries` attempt, the same bargain the scratch paths make. The
unbinding is a real one: after `setenv name None`, `Sys.getenv_opt`
answers `None` and not `Some ""`, which is what makes it usable to test
the path a *missing* variable takes.

What comes back is what the variable held before the test's **first**
`setenv` of it, so binding one twice still leaves behind what the test
found.

```ocaml
test "builds in place" (fun () ->
    chdir (temp_dir ());
    Out_channel.with_open_text "built.txt" (fun oc ->
        Out_channel.output_string oc "ok");
    is_true (Sys.file_exists "built.txt"))
```

`chdir` restores the directory the process was in at the test's first
`chdir`. If that directory is gone — the test deleted it — the test
fails saying so, rather than leaving every later test to run from
somewhere unexpected: a leaked scratch directory is inert, a process in
the wrong place is not.

Both are process-global while the test runs: threads the test spawns and
child processes it starts see them, and a thread still moving when the
test ends races the restoration. Tests never race *each other* here —
the runner is sequential, one domain — but ordering threads within one
test is that test's own job.

## One test per input: `cases`

`cases name inputs fn` declares a group with one child per input, so
one bad input does not mask the rest and each is selectable with `-f`:

```ocaml
cases "ports parse" ~name:Fun.id [ "1"; "80"; "8080"; "65535" ]
  (fun input -> ignore (require_ok (parse_port input)))
```

`?name` derives the child's name from the input (here the string
itself); without it children are numbered `ports parse.0`, `.1`, ….

The row list — and each `?name` application — is evaluated at
*declaration* time, before any test runs: rows are data, not test
code. A row that needs test-scoped work (`temp_dir`, `setenv`, an
assertion, IO against the system under test) cannot be a row; keep the
list pure and do per-input work inside the body. A table whose rows
must be computed inside a test does not convert to `cases` — use
`subtest` in one body instead.

For sub-cases *inside* one body — labels, not selectable tests — use
`subtest`:

```ocaml
test "backend contract" (fun () ->
    List.iter
      (fun (name, count) -> subtest name (fun () -> equal int 12 count))
      backends)
```

A failing subtest is recorded as `backend contract › <name>` and its
siblings still run; the test fails at the end with every entry.

## Tags, slow tests, timeouts, retries

`~tags` on `test`/`group` label tests (group tags extend every
descendant); select with `--tag`/`--exclude-tag`. `slow name fn` is
`test` with the `"slow"` tag pre-applied, and `--quick` drops
slow-tagged tests. Property tests carry `"prop"` automatically.

`~timeout:60.` caps one test in seconds (setup and body share the
window, and teardown is re-armed with what is left of it — or with a
fresh window if they used it all, since cleanup still has to happen; a
`scoped` test spends the window on the whole scope call, re-armed on
the same terms; and for properties, generation and shrinking too, see
[Property testing](property-testing.md#notes); the runner's
`--timeout` sets the default); `~retries:2` gives a failing test extra
attempts — for the flaky-by-nature, not as a way of life.

## Focus: `ftest` and `fgroup`

While debugging, promote `test` to `ftest` (or `group` to `fgroup`):
when any focused node exists, only focused tests run. Focus is a local
tool — under CI a run containing focused tests refuses to start
(`WINDTRAP_ALLOW_FOCUS=1` overrides), and a successful focused run
prints a warning so it cannot slip into a commit silently.

## Known bugs: `xfail`

`xfail t` marks a test (or a whole group) as *expected to fail*: it
still runs, a failure reports as `XFAIL` without failing the run, and
a pass fails loudly ("expected to fail, but the test passed") — the
bug-fixed signal. Use it to keep a reproduction in-tree without a red
run; use `skip` when the body must not run at all:

```ocaml
xfail ~reason:"issue #42"
  (test "http resolves to its TCP port" (fun () ->
       equal int 8080 (require_match tcp_port (resolve "http"))))
```

## Test identity

A test is named by its path — group names, then its own, joined with
`" › "`; that string is what `-f` matches, and duplicate paths are a
startup error. `current_test ()` returns the executing test's path as
a list — use it to key artifacts by test identity instead of
duplicating names by hand. `srandom ()` gives a `Random.State.t`
seeded from the run's root seed and that path
([Property testing](property-testing.md#notes)).
