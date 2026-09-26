# Baselines and expect tests

This page shows how to compare produced text with a baseline, reviewed
text kept as a literal in the test or as a committed file. It shows how
a stale baseline reads under `dune runtest`, how to accept the change,
and how to write expect tests inside a library. The reference is the
baselines section of [`lib/windtrap.mli`](../../lib/windtrap.mli).

The module under test builds a help text and a report, and prints a
greeting.

`test/mytool.ml`:

```ocaml
let help () =
  String.concat "\n"
    [
      "Usage: mytool [OPTIONS] COMMAND";
      "";
      "Commands:";
      "  build    Build the project";
      "  test     Run the tests";
      "";
      "Options:";
      "  --help   Show this help";
    ]

let report ~rows = Printf.sprintf "processed %d rows\nstatus: ok" rows
let greet name = Printf.printf "Hello, %s!\n" name
```

The suite is `test/test_mytool.ml`. The files ship as
`examples/05-baselines/` in windtrap's repository, and the transcripts
print that directory's paths.

## Comparing text with a literal

`expect` compares a text with a literal up to whitespace (see
`Windtrap.expect`), and `expect_exact` compares byte for byte.
`__POS_OF__` pairs the literal with its position, where a correction
rewrites it. The correction of an `expect_exact` text that holds a CR is a
quoted literal, with the CR written `\r`.

`test/test_mytool.ml`:

```ocaml
open Windtrap

let report_counts_the_rows () =
  expect (Mytool.report ~rows:42)
  @@ __POS_OF__ {|
    processed 42 rows
    status: ok
    |}
```

A mismatch fails the test and the body goes on, so one run reports every
stale baseline (see `Windtrap.expect`).

## Keeping a baseline in a file

`expect_file` compares a text with a file, named relative to the project
root whatever the working directory. Under dune the root is the
directory that holds the build directory, here windtrap's repository,
and `WINDTRAP_PROJECT_ROOT` sets another. The file holds the text as it
is.

`test/help.expected`:

```
Usage: mytool [OPTIONS] COMMAND

Commands:
  build    Build the project
  test     Run the tests

Options:
  --help   Show this help
```

The group lists the three tests of the suite, the second one on the
file.

`test/test_mytool.ml`:

```ocaml
let messages =
  group "messages"
    [
      test "the report counts the rows" report_counts_the_rows;
      test "the help lists the commands" (fun () ->
          expect_file (Mytool.help ()) "examples/05-baselines/help.expected");
      test "the greeting names the user" (fun () ->
          Mytool.greet "Ada";
          expect (output ()) @@ __POS_OF__ {| Hello, Ada! |});
    ]
```

To start a file baseline under dune, create the file empty, name it in
`(deps …)`, run the tests and promote the correction. Dune stops before
the suite runs when a file named in `(deps …)` does not exist. Outside
dune, `-u` writes the missing file.

## Checking printed output

The third test compares what it printed. `output ()` is what the running
test wrote to standard output and standard error since the previous call
(see `Windtrap.output`), and `expect` takes it as any other text.

## Running expectations under dune

The suite's last line runs the group.

`test/test_mytool.ml`:

```ocaml
let () = exit (run "mytool" [ messages ])
```

The stanza runs the suite with `--corrected`, which writes the
correction of a stale baseline beside its file as `<file>.corrected`.
Each `diff?` then compares one file that holds baselines with its
correction. `(deps …)` names every file an `expect_file` reads.

`test/dune`:

```lisp
(test
 (name test_mytool)
 (modules test_mytool mytool)
 (libraries windtrap)
 (deps help.expected)
 (action
  (progn
   (run %{test} --corrected)
   (diff? test_mytool.ml test_mytool.ml.corrected)
   (diff? help.expected help.expected.corrected))))
```

With every baseline current, `dune runtest` prints the suite's line and
one line per file of the library's expect tests (see below):

```
$ dune runtest
mytool: 3 passed in 0.6ms.
tokenizer/timing.ml: 1 passed in 0.5ms.
tokenizer/tokens.ml: 2 passed in 0.5ms.
```

## Reading a stale expectation

To see a stale expectation, change `processed` to `read` in
`Mytool.report` and run the tests again. The block shows the baseline's
lines with `-`, the produced lines with `+`, and the command that
accepts the change. Dune then prints its own diff of the source file:

```
$ dune runtest
mytool: 3 tests
──────────────────────── failures ────────────────────────
  FAIL  messages › the report counts the rows
    examples/05-baselines/test_mytool.ml:5
      5 │ @@ __POS_OF__ {|

    expect: mismatch
    @@ -1,2 +1,2 @@
    - processed 42 rows
    + read 42 rows
      status: ok
    accept: dune promote examples/05-baselines/test_mytool.ml
──────────────────────────────────────────────────────────

corrections (1):
  wrote examples/05-baselines/test_mytool.ml.corrected (1 expectation)

2 passed, 1 failed, 1 correction written in 0.8ms.
File "examples/05-baselines/test_mytool.ml", line 1, characters 0-0:
diff --git a/_build/default/examples/05-baselines/test_mytool.ml b/_build/default/examples/05-baselines/test_mytool.ml.corrected
index 2ef2112..d9ba548 100644
--- a/_build/default/examples/05-baselines/test_mytool.ml
+++ b/_build/default/examples/05-baselines/test_mytool.ml.corrected
@@ -3,7 +3,7 @@ open Windtrap
 let report_counts_the_rows () =
   expect (Mytool.report ~rows:42)
   @@ __POS_OF__ {|
-    processed 42 rows
+    read 42 rows
     status: ok
     |}
 
tokenizer/timing.ml: 1 passed in 0.5ms.
tokenizer/tokens.ml: 2 passed in 0.5ms.
```

## Accepting a change

Run the command of the `accept:` line,
`dune promote examples/05-baselines/test_mytool.ml`, to copy the
correction over the source file. `dune promote` alone accepts every
correction dune holds. Review the change with `git diff`, and the next
`dune runtest` passes.

Dune forgets a correction at its next command, so run `dune promote`
right after the failing `dune runtest`. It keeps one only from a run
that exits 0, so another failing test of the suite withholds it, and the
run says so on standard error.

The stanza's action stops at its first `diff?` that fails, so dune holds
at most one correction per stanza and run. When two files are stale, the
second file's `accept:` line finds nothing to promote until the first is
accepted and the tests run again.

## Accepting without dune promote

A suite run by hand writes no correction. Its report closes on one
`accept:` line, above the summary, which reruns the run's tests with
`-u`:

```
$ dune exec examples/05-baselines/test_mytool.exe
mytool: 3 tests
──────────────────────── failures ────────────────────────
  FAIL  messages › the report counts the rows
    examples/05-baselines/test_mytool.ml:5
      5 │ @@ __POS_OF__ {|

    expect: mismatch
    @@ -1,2 +1,2 @@
    - processed 42 rows
    + read 42 rows
      status: ok
──────────────────────────────────────────────────────────

accept: dune exec examples/05-baselines/test_mytool.exe -- -u
2 passed, 1 failed in 1.6ms.
```

To accept, run the line. `-u` rewrites every stale literal in place, and
its row under `corrections` says to build the executable again before the
next run:

```
$ dune exec examples/05-baselines/test_mytool.exe -- -u
mytool: 3 tests
corrections (1):
  accepted examples/05-baselines/test_mytool.ml (1 expectation; rebuild before the tests see it)

3 passed, 1 correction accepted in 2.2ms.
```

Review the change with `git diff`. `-u` is refused under CI (see
[Running tests](running-tests.md)).

## Reading a block with no `accept:` line

A test that also failed outside its expectations, or skipped, keeps no
correction until that is fixed. Its block says `no correction was kept:`
and why, in place of the `accept:` line. A correction the source cannot
take, as when the file changed since the build, says
`correction refused (line N):` and the reason.

## Writing expect tests inside a library

An expect test lives next to the code it tests, in a library with
`(inline_tests)` preprocessed by `ppx_windtrap`.

`test/dune`:

```lisp
(library
 (name tokenizer)
 (modules tokens timing)
 (inline_tests)
 (preprocess
  (pps ppx_windtrap)))
```

`let%expect_test` declares a test, and each `[%expect {|…|}]` compares
what the test printed since the previous node.

`test/tokens.ml`:

```ocaml
type token = Int of int | Plus | Eof

let tokenize input =
  let tokens =
    String.split_on_char ' ' input
    |> List.filter (fun s -> s <> "")
    |> List.map (function
      | "+" -> Plus
      | s -> (
          match int_of_string_opt s with
          | Some n -> Int n
          | None -> invalid_arg ("tokenize: " ^ s)))
  in
  tokens @ [ Eof ]

let print_tokens tokens =
  List.iter
    (function
      | Int n -> Printf.printf "INT %d\n" n
      | Plus -> print_endline "PLUS"
      | Eof -> print_endline "EOF")
    tokens

let%expect_test "a sum is two integers around a plus" =
  print_tokens (tokenize "1 + 2");
  [%expect {|
    INT 1
    PLUS
    INT 2
    EOF
    |}]

let%expect_test "repeated spaces are skipped" =
  print_tokens (tokenize "1   +  2");
  [%expect {|
    INT 1
    PLUS
    INT 2
    EOF
    |}]
```

`dune runtest` runs them. A stale `[%expect]` fails as a stale `expect`
does, and `dune promote` accepts it. A node is checked when the body
reaches it, and nothing checks what the test prints after its last node.
The rewriter's forms are stated in
[`ppx/ppx_windtrap.mli`](../../ppx/ppx_windtrap.mli).

## Masking what changes between runs

A module named `Expect_test_config` configures the expect tests below it
in its file. Its `sanitize` rewrites the printed text before the
comparison and before a correction is written.

`test/timing.ml`:

```ocaml
module Expect_test_config = struct
  include Expect_test_config

  let sanitize = String.map (fun c -> if c >= '0' && c <= '9' then '#' else c)
end

let report_duration ms = Printf.printf "finished in %d ms\n" ms

let%expect_test "the duration is reported in milliseconds" =
  report_duration 37;
  [%expect {| finished in ## ms |}]
```

The default configuration and the contract of an override are in
[`ppx/config/expect_test_config.mli`](../../ppx/config/expect_test_config.mli).
For `expect` and `expect_file`, mask the text in code before the call.

## Moving a ppx_expect suite

Replace `ppx_expect` with `ppx_windtrap` in the library's `(pps …)`. The
expect tests then run and are accepted as above.
`[@@expect.uncaught_exn]`, `[%expect.unreachable]` and the other forms
windtrap lacks fail to compile with a message that names them; the list
is in [`ppx/ppx_windtrap.mli`](../../ppx/ppx_windtrap.mli). Rewrite a
test that relied on `[@@expect.uncaught_exn]` to catch the exception and
print it before its node. An `Expect_test_config` of Async or Lwt does
not compile. The upstream corpus windtrap passes and refuses is
described in
[`test/conformance/RESULTS.md`](../../test/conformance/RESULTS.md).
