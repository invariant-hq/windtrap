# Resources and structure

Windtrap has no group-level hooks — no user code ever runs outside a
test's exception boundary. Resources are scoped by two constructors
instead, and everything else here shapes the suite: table-driven
tests, tags, focus, and known-bug bookkeeping.

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

## One test per input: `cases`

`cases name inputs fn` declares a group with one child per input, so
one bad input does not mask the rest and each is selectable with `-f`:

```ocaml
cases "ports parse" ~name:Fun.id [ "1"; "80"; "8080"; "65535" ]
  (fun input -> ignore (require_ok (parse_port input)))
```

`?name` derives the child's name from the input (here the string
itself); without it children are numbered `ports parse.0`, `.1`, ….
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

`~timeout:60.` caps one test in seconds (setup, body, and teardown —
and for properties, generation and shrinking too, see
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
