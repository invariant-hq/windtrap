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
  String.concat ""
    (List.map
       (fun line -> line ^ "\n")
       [
         "Usage: mytool [OPTIONS] COMMAND";
         "";
         "Commands:";
         "  build    Build the project";
         "  test     Run the tests";
         "";
         "Options:";
         "  --help   Show this help";
       ])

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
is. CR LF reads as LF, and the final newline counts: a text that lacks
the file's final newline fails, and its block says
`values differ only by a trailing newline (on the expected side)`.
Each line of the help, the last included, ends with one.

`test/help.expected`:

```
Usage: mytool [OPTIONS] COMMAND

Commands:
  build    Build the project
  test     Run the tests

Options:
  --help   Show this help
```

The group lists the tests of the suite. The second one is on the file,
and the last is a table of one test per user.

`test/test_mytool.ml`:

```ocaml
let messages =
  group "messages"
    [
      test "the report counts the rows" report_counts_the_rows;
      test "the help lists the commands" (fun () ->
          expect_file ~file:"examples/05-baselines/help.expected"
            (Mytool.help ()));
      cases "the greeting names the user" ~id:fst
        [
          ("Ada", __POS_OF__ {| Hello, Ada! |});
          ("Grace", __POS_OF__ {| Hello, Grace! |});
        ]
        (fun (name, greeting) ->
          Mytool.greet name;
          expect (output ()) greeting);
    ]
```

To start a file baseline under dune, create the file empty, name it in
`(deps …)` and in a `diff?`, run the tests and promote the correction.
Dune stops before the suite runs when a file named in `(deps …)` does
not exist. A file the stanza does not name is missing to the run, which
fails. Outside dune, `-u` writes the missing file.

## Checking printed output

The greeting's tests compare what they printed. `output ()` is what the
running test wrote to standard output and standard error since the
previous call (see `Windtrap.output`), and `expect` takes it as any
other text.

## Giving each row of a table its own literal

A row of `cases` can carry its own literal, as the greeting's rows do.
`expect` checks each row against its literal, and `dune promote` or
`-u` rewrites the literals of the stale rows alone (see
`Windtrap.expect`). Each row keeps or withholds its corrections as a
test of its own would: a row that also fails another way keeps none,
and the rows after it are compared with the baselines it left as they
were. A literal written in the body of `cases` is shared, and every row
must produce its one text.

## Expectations in a property

A property's law claims what it checks for every value it draws, and no
one literal can hold for all of them. An `expect` in a `prop` or
`stateful` law therefore fails its case as `equal` would: the case
shrinks, the report shows the one counterexample with its mismatch, and
no correction is written for the literal, under `--corrected` and `-u`
alike. The mismatch says so in one line, `no correction: a property's
law claims this for every value, and one text cannot hold for them all`.
A table is the place for a literal per input.

## Running expectations under dune

The suite's last line runs the group.

`test/test_mytool.ml`:

```ocaml
let () = exit (run "mytool" [ messages ])
```

The stanza runs the suite with `--corrected`, which writes the
correction of a stale baseline beside its file as `<file>.corrected`.
Each `diff?` then compares one file that holds baselines with its
correction, and `concurrent` runs every `diff?` whatever the others
find, so that dune holds the correction of each stale file and one
`dune promote` takes them all (it needs `(lang dune 3.8)`). In a
`progn` the first `diff?` that fails would end the action, and each
further stale file would take another run and another promotion.
`(deps …)` names every file an `expect_file` reads. The stanza spells a
path from its own directory, as `help.expected`, where the call spells
it from the project root.

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
   (concurrent
    (diff? test_mytool.ml test_mytool.ml.corrected)
    (diff? help.expected help.expected.corrected)))))
```

With every baseline current, `dune runtest` prints the suite's line and
one line per file of the library's expect tests (see below):

```
$ dune runtest
tokenizer/timing.ml: 1 passed in 0.8ms.
tokenizer/tokens.ml: 2 passed in 1.3ms.
mytool: 3 passed in 1.9ms.
```

## Reading a stale expectation

To see a stale expectation, change `processed` to `read` in
`Mytool.report` and run the tests again. The block shows the baseline's
lines with `-` and the produced lines with `+`, and the line above the
summary is the command that accepts every correction of the run. Dune
then prints its own diff of the source file:

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
──────────────────────────────────────────────────────────

corrections (1):
  wrote examples/05-baselines/test_mytool.ml.corrected (1 expectation)

accept: dune promote examples/05-baselines/test_mytool.ml
2 passed, 1 failed, 1 correction written in 2.2ms.
File "examples/05-baselines/test_mytool.ml", line 1, characters 0-0:
diff --git a/_build/default/examples/05-baselines/test_mytool.ml b/_build/default/examples/05-baselines/test_mytool.ml.corrected
index 015a41c..0339f35 100644
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
 
```

## Accepting a change

Run the command of the `accept:` line,
`dune promote examples/05-baselines/test_mytool.ml`, to copy the
correction over the source file. `dune promote` alone accepts every
correction dune holds. Review the change with `git diff`, and the next
`dune runtest` passes.

Dune forgets a correction at its next command, so run `dune promote`
right after the failing `dune runtest`. It keeps one only from a run
that exits 0, so another failing test of the suite withholds it: the
report then prints no `accept:` line, and the run says on standard
error what does accept it, fixing the failures first or `-u` now:

```
windtrap: warning: dune registers a correction for promotion only when the run that wrote it exits 0, so the failures above withhold the correction written here. Fix the failures, rerun, then 'dune promote'. To accept it now: dune exec examples/05-baselines/test_mytool.exe -- -u
```

A file baseline that does not exist yet is the one exception: once the
missing files are all that fail, the line creates them first,
`accept: touch 'examples/05-baselines/help.expected' && dune runtest; dune promote …`.

The `accept:` line names every file whose correction dune holds, which
under the stanza above is every stale file of the run. A stanza whose
`diff?` sit in the `progn` itself holds only the first stale file's:
wrap them in `concurrent`.

Under CI a stale baseline fails the run itself, whatever its mode. A
`--corrected` run exits 0 on a mismatch only so that a `diff?` can hand
the correction to `dune promote`, and no one promotes under CI: a stanza
that lacks the file's `diff?` would pass. The run still writes the
correction, and says on standard error why it exits 1:

```
windtrap: warning: CI is set, so a baseline mismatch fails the run: no one promotes the correction written here under CI. To accept it, rerun outside CI, then 'dune promote'.
```

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
2 passed, 1 failed in 1.5ms.
```

To accept, run the line. `-u` rewrites every stale literal in place, and
its row under `corrections` says to build the executable again before the
next run:

```
$ dune exec examples/05-baselines/test_mytool.exe -- -u
mytool: 3 tests
corrections (1):
  accepted examples/05-baselines/test_mytool.ml (1 expectation; rebuild before the tests see it)

3 passed, 1 correction accepted in 1.4ms.
```

Review the change with `git diff`. `-u` is refused under CI (see
[Running tests](running-tests.md)).

## Reading a correction that was not kept

A test that also failed outside its expectations, or skipped, keeps no
correction until that is fixed. Its block says `no correction was kept:`
and why, and no `accept:` line accepts it. A correction the source
cannot take, as when the file changed since the build, says
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
reaches it, and a node the body never reaches fails the test. What the
test prints after its last node fails the test when the body returns,
and its correction appends a node that holds it, so a new test can start
with no node at all.
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
