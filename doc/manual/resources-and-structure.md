# Resources and structure

This page shows how to lay out a suite over files and groups, test a
library's internals next to its code, and give a test a resource that
the runner releases. It also shows how to bound a test in time, retry
it, skip it, or keep a known bug running. The reference is the
declaring-tests and running-test sections of
[`lib/windtrap.mli`](../../lib/windtrap.mli).

The snippets test `Db` and `Server`, in-memory stand-ins for a database
connection and a shared server process, and `Keys`, a library that
checks row names, all in a directory `test/`. The files ship as
`examples/06-resources-and-structure/` in windtrap's repository, and the
transcripts print that directory's paths.

## Laying out a suite

A group is a top-level value, named after the thing its tests are about,
and each test's name states its claim. A body of one to three lines
stays inline; a longer one is a top-level function the group lists by
name, as in `db_tests.ml` below. In a suite over several files, each
module exports its groups, and the last line of the suite's file runs
them.

`test/test_storage.ml`:

```ocaml
let () =
  exit
    (Windtrap.run "storage"
       [
         Db_tests.database;
         Server_tests.server;
         Gpu_tests.gpu;
         Process_tests.process;
       ])
```

The stanza builds every module of the directory but the library's into
one executable.

`test/dune`:

```lisp
(test
 (name test_storage)
 (modules :standard \ keys)
 (libraries windtrap))
```

A test's path is its groups' names and its own, joined with `" › "`, and
it is what `-f` matches and what identifies the test (see the
declaring-tests section of `lib/windtrap.mli`). `current_test ()` is the
running test's path, to name a file after the test.

A test left out of the list does not run. `-l` prints the paths of the
tests that do, and runs nothing:

```
$ dune exec examples/06-resources-and-structure/test_storage.exe -- -l
database › an insert adds one row
database › a name is stored › alice
database › a name is stored › bob
database › a name is stored › carol
database › every inserted row is found
database › a duplicate row counts once
server › it answers a ping
server › a first session gets id 1
server › reindexing keeps it running
server › a backup completes
gpu › the device has a name
gpu › the device is not the CPU
process state › a config is written to a fresh directory
process state › the token is read from the environment
process state › a build writes in the working directory
```

## Testing a library's internals

A library's internals are tested next to the code, with `let%test` in a
library that has `(inline_tests)` and `(preprocess (pps ppx_windtrap))`.
An executable suite is for tests from outside the library and for tests
that need `bracket`, `scoped` or `fixture`. A library can have both, and
each runner runs its own tests alone.

`test/dune`:

```lisp
(library
 (name keys)
 (modules keys)
 (inline_tests)
 (preprocess
  (pps ppx_windtrap)))
```

`Keys` exports `valid` alone, and its tests reach `normalize` beside it.

`test/keys.mli`:

```ocaml
val valid : string -> bool
(** [valid name] is [true] iff [name], trimmed and lowercased, is a word of
    letters, digits and underscores. *)
```

A `let%test` body returns `unit` and asserts with the verbs, and
`module%test` makes a group of the tests inside its module. The forms
are stated in [`ppx/ppx_windtrap.mli`](../../ppx/ppx_windtrap.mli), and
[`let%expect_test`](baselines.md#writing-expect-tests-inside-a-library)
compares printed output.

`test/keys.ml`:

```ocaml
let normalize name = String.lowercase_ascii (String.trim name)

let valid name =
  let name = normalize name in
  name <> ""
  && String.for_all
       (function 'a' .. 'z' | '0' .. '9' | '_' -> true | _ -> false)
       name

let%test "a name is trimmed and lowercased" =
  Windtrap.(equal string "alice" (normalize "  Alice "))

module%test Valid = struct
  let%test "a name of letters is valid" = Windtrap.is_true (valid "Alice")
  let%test "a blank name is not" = Windtrap.is_false (valid "  ")
end
```

`dune runtest` runs the suite, then the library's tests, one line per
file:

```
$ dune runtest
storage: 12 passed, 2 skipped, 1 expected failure in 3.5ms.
keys/keys.ml: 3 passed in 0.8ms.
```

## Giving each test its own resource

A group has no setup or teardown of its own; a test gets a resource from
`bracket`, `scoped` or `fixture`. `bracket ~setup ~teardown` makes a
test whose body receives what `setup ()` returns, and the runner calls
`teardown` on it after the body, whatever the outcome (see
`Windtrap.bracket`). Applied to its two functions alone, it is a
constructor for every test that needs the resource.

`test/db_tests.ml`:

```ocaml
open Windtrap

let with_db = bracket ~setup:Db.connect ~teardown:Db.close

let every_row_is_found db =
  let names = [ "alice"; "bob"; "carol" ] in
  List.iter (Db.insert db) names;
  List.iter
    (fun name -> subtest name (fun () -> is_true (Db.mem db name)))
    names

let a_duplicate_counts_once db =
  Db.insert db "alice";
  Db.insert db "alice";
  equal int 1 (Db.count db)

let database =
  group "database"
    [
      with_db "an insert adds one row" (fun db ->
          Db.insert db "alice";
          equal int 1 (Db.count db));
      cases ~name:Fun.id "a name is stored" [ "alice"; "bob"; "carol" ]
        (fun name ->
          let db = Db.connect () in
          Db.insert db name;
          is_true (Db.mem db name));
      with_db "every inserted row is found" every_row_is_found;
      xfail ~reason:"issue #42"
        (with_db "a duplicate row counts once" a_duplicate_counts_once);
    ]
```

A failure in `setup` or `teardown` prints `[setup]` or `[teardown]`
before its location, and a teardown failure is reported beside the
body's.

## Running one test per input

`cases ~name base inputs fn` is a group of one test per input, named by
`name input`, as `a name is stored` above. Each input is a test of its
own, so one that fails does not stop the others, and `-f` selects one:

```
$ dune exec examples/06-resources-and-structure/test_storage.exe -- -v -f 'a name is stored › bob'
storage: 1 test
  PASS  database › a name is stored › bob          0.1ms
1 passed in 0.5ms.
```

The inputs and their names are computed when the suite is declared,
outside any test (see `Windtrap.cases`). An input can carry its test's
expected text as a `__POS_OF__` literal, which `expect` checks and a
correction rewrites for that input alone (see
[Giving each row of a table its own literal](baselines.md#giving-each-row-of-a-table-its-own-literal)).

## Naming the parts of one test

`subtest name fn` runs `fn` as a named part of the running test, as
`every_row_is_found` does for each row it inserted. A part that fails is
recorded under its name, the next parts still run, and the test fails at
the end. The parts share one body, one resource and one setup; `-f`
cannot select one (see `Windtrap.subtest`).

## Sharing one resource across the run

`fixture create` is an accessor: the first test that calls it acquires
the resource with `create ()`, and later calls return the same value.
The runner releases the acquired fixtures after the last test. `scoped`
makes a test from a function that hands a resource to a callback, such
as a session that exists only inside `Server.with_session`.

`test/server_tests.ml`:

```ocaml
open Windtrap

let shared_server = fixture ~teardown:Server.stop Server.start
let with_session = scoped (fun f -> Server.with_session (shared_server ()) f)

let server =
  group ~timeout:5. "server"
    [
      test "it answers a ping" (fun () ->
          is_true (Server.ping (shared_server ())));
      with_session "a first session gets id 1" (fun session ->
          equal int 1 (Server.session_id session));
      slow ~timeout:60. "reindexing keeps it running" (fun () ->
          is_true (Server.reindex (shared_server ())));
      test ~retries:2 "a backup completes" (fun () ->
          is_true (Server.backup (shared_server ())));
    ]
```

A scope calls its callback once and releases the resource when the
callback raises, as `Fun.protect` does. No timeout covers a release, and
a `teardown` that waits on the outside world needs a deadline of its own
(see `Windtrap.scoped` and `Windtrap.fixture`).

Under `-v` the runner names each fixture it releases, with the line
where `fixture` was applied:

```
$ dune exec examples/06-resources-and-structure/test_storage.exe -- -v -f server
storage: 4 tests
  PASS  server › it answers a ping                 0.1ms
  PASS  server › a first session gets id 1         0.0ms
  PASS  server › reindexing keeps it running       0.0ms
  PASS  server › a backup completes                0.0ms
releasing fixture (examples/06-resources-and-structure/server_tests.ml:3)
4 passed in 0.5ms.
```

## Bounding a test in time and retrying it

`~timeout` bounds a test in seconds, and on a group it bounds every test
under it that sets none, as the server's five seconds do. `~retries`
gives a failing test more attempts, and a test that passes on a later
attempt is listed under `flaky tests`. `--timeout` sets the limit of the
tests that have none (see the declaring-tests section of
`lib/windtrap.mli`).

## Marking a slow test

`slow` declares a test tagged `slow`, as `reindexing keeps it running`
is, and `--exclude-tag slow` leaves the tagged tests out of a run (see
[Running tests](running-tests.md)). `~tags` puts any tag on a test or on
every test of a group. An untagged test that runs longer than
`--slow-threshold` seconds, one by default, is listed under `slow tests`
in the report.

## Skipping when the machine lacks a resource

`skip ~reason ()` ends the test as skipped. In a fixture's `create`, the
skip is kept for the run, so every test that calls the accessor skips
with the same reason.

`test/gpu_tests.ml`:

```ocaml
open Windtrap

let device =
  fixture (fun () ->
      match Sys.getenv_opt "GPU_DEVICE" with
      | Some name -> name
      | None -> skip ~reason:"GPU_DEVICE is not set" ())

let gpu =
  group "gpu"
    [
      test "the device has a name" (fun () -> is_true (device () <> ""));
      test "the device is not the CPU" (fun () ->
          is_false (String.equal (device ()) "cpu"));
    ]
```

The summary counts the skips, and `-v` prints each with its reason:

```
$ dune exec examples/06-resources-and-structure/test_storage.exe -- -v -f gpu
storage: 2 tests
  SKIP  gpu › the device has a name (GPU_DEVICE is not set)
  SKIP  gpu › the device is not the CPU (GPU_DEVICE is not set)
2 skipped in 0.5ms.
```

## Using files, variables and a working directory

`temp_dir ()` is a fresh directory that the runner removes when the test
ends, so a fixture's resource must not live in it. `setenv` binds or
unbinds a variable, and `chdir` changes the working directory, both for
the rest of the test. The runner restores both when the test ends, on
every outcome.

`test/process_tests.ml`:

```ocaml
open Windtrap

let write_file path text =
  Out_channel.with_open_text path (fun oc -> Out_channel.output_string oc text)

let token () = Sys.getenv_opt "API_TOKEN"

let a_config_is_written () =
  let path = Filename.concat (temp_dir ()) "config.json" in
  write_file path "{}";
  is_true (Sys.file_exists path)

let the_token_is_read () =
  setenv "API_TOKEN" (Some "t-123");
  equal (option string) (Some "t-123") (token ());
  setenv "API_TOKEN" None;
  equal (option string) None (token ())

let a_build_writes_in_place () =
  chdir (temp_dir ());
  write_file "built.txt" "ok";
  is_true (Sys.file_exists "built.txt")

let process =
  group "process state"
    [
      test "a config is written to a fresh directory" a_config_is_written;
      test "the token is read from the environment" the_token_is_read;
      test "a build writes in the working directory" a_build_writes_in_place;
    ]
```

The environment and the working directory belong to the process.
Threads and child processes inherit the binding, and a thread still
running when the test ends races its restoration (see
`Windtrap.setenv`).

## Testing code that runs an event loop

A body may run its own event loop, as `Lwt_main.run` does, and a
function that runs the loop around a callback is a scope. `Eio_main.run`
is one, and `scoped Eio_main.run` makes tests whose body receives the
environment:

```ocaml
(* fragment: requires eio_main *)
let with_eio = scoped Eio_main.run

let clock =
  group "clock"
    [
      with_eio "a sleep advances the clock" (fun env ->
          let clock = Eio.Stdenv.clock env in
          let before = Eio.Time.now clock in
          Eio.Time.sleep clock 0.01;
          greater float_exact ~than:before (Eio.Time.now clock));
    ]
```

## Keeping a known bug in the suite

`xfail ~reason t` runs the test or group `t` and counts its failure as
expected, so the run stays green. It fails when `t` passes, and the
report says it was expected to fail. In
[mutation testing](mutation.md#what-a-mutation-run-runs) it reaches no
mutant. Under `-v` the failure prints dim
under its `XFAIL` line:

```
$ dune exec examples/06-resources-and-structure/test_storage.exe -- -v -f duplicate
storage: 1 test
  XFAIL  database › a duplicate row counts once    0.1ms (expected failure: issue #42)
    examples/06-resources-and-structure/db_tests.ml:30
      30 │ (with_db "a duplicate row counts once" a_duplicate_counts_once);

    expected  1
    actual    2

1 expected failure in 0.6ms.
```
