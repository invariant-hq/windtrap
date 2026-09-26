The inline runner's protocol as dune drives it: `inline-test-runner LIB`
runs a library's tests, `-partition FILE` the tests of one of its files,
and `-list-partitions` names the files. runner.exe is the main that the
inline_tests backend generates, over the fixtures of inline/, which
belong to no library. The report's suites pin what a report prints; this
session pins what only a process shows: the exit status, the stream,
the files a run leaves, the tests a runner runs and the line that the
call stack gives a failure.

A run's environment is stated, and its logs stay in this directory:

  $ run() {
  >   env -i PATH="$PATH" WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 \
  >       WINDTRAP_OUTPUT="$PWD/_logs" "$@" > out 2> err
  > }
  $ partition() {
  >   file=$1; shift
  >   run "$@" ./inline/runner.exe inline-test-runner inline -partition "$file"
  > }

The protocol

-list-partitions prints the basename of each file that registered a
test, sorted, and exits 0:

  $ run ./inline/runner.exe inline-test-runner inline -list-partitions
  $ cat out err
  crash.ml
  masked.ml
  release.ml
  sanitized.ml
  stale.ml
  tail.ml
  trailing.ml
  unreached.ml

A partition runs its file's tests alone, as the suite LIB/FILE, reports
on standard output and exits with its run's code. The one test of
stale.ml fails on a stale payload alone, so its correction is written
and the run exits 0:

  $ partition stale.ml
  $ head -1 out
  inline/stale.ml: 1 test
  $ cat err

Started without the protocol's arguments, the runner runs nothing and
exits 0 in silence:

  $ run ./inline/runner.exe
  $ cat out err

An executable that links test code of no library and never runs the
protocol exits 2, and says why on standard error. It links stale.ml,
whose test the runner ran above:

  $ run ./inline/undriven.exe
  [2]
  $ cat out
  $ cat err
  windtrap: registered inline tests were never driven: this executable links ppx_windtrap-preprocessed test code of no library (stale.ml) and nothing ran it.
  windtrap: move the tests into a library stanza with (inline_tests), whose inline runner dune builds and drives, or drive the runner protocol yourself (Ppx_windtrap_runtime.Ppx_runtime.init/exit). Exiting 2: nothing ran.

A library's inline tests belong to its runner. A suite that links
linked_shapes runs its own tests alone, and the runner of linked_scene,
which links linked_shapes too, lists and runs linked_scene's alone:

  $ run ./inline/test_shapes.exe
  $ head -1 out | sed -E 's/ in [0-9.]+m?s\./ in DURATION./'
  shapes: 1 passed in DURATION.
  $ run ./inline/scene_runner.exe inline-test-runner linked_scene -list-partitions
  $ cat out
  scene.ml
  $ run ./inline/scene_runner.exe inline-test-runner linked_scene
  $ head -1 out | sed -E 's/ in [0-9.]+m?s\./ in DURATION./'
  linked_scene: 1 passed in DURATION.

Corrections

The runner runs with --corrected: a correction goes beside the build's
copy of the source, where dune's diff? reads it, and leaves the exit
code alone. Dune registers a library's corrections only when all its
partitions exit 0, so a failure that is not a stale payload fails its
partition.

The stale payload's partition above wrote its correction:

  $ diff inline/stale.ml inline/stale.ml.corrected
  8c8
  <   [%expect {| stale payload |}]
  ---
  >   [%expect {| fresh output |}]
  [1]
  $ rm inline/stale.ml.corrected

A source that the runner cannot read takes no correction, and the same
partition fails. The system's reason is masked:

  $ partition stale.ml WINDTRAP_PROJECT_ROOT="$PWD/elsewhere"
  [1]
  $ sed -n -E 's/(cannot be read): .*/\1: REASON/p' out
      correction refused (line 8): the source file cannot be read: REASON
  $ test -e inline/stale.ml.corrected || echo 'no correction'
  no correction

An uncaught exception fails its partition, which writes nothing:

  $ partition crash.ml
  [1]
  $ grep FAIL out
    FAIL  Crash › an uncaught exception
  $ test -e inline/crash.ml.corrected || echo 'no correction'
  no correction

A test that also failed outside its expectations keeps no correction:

  $ partition masked.ml
  [1]
  $ grep 'no correction' out
      no correction was kept: the test also failed outside its expectations; fix that failure and rerun
  $ test -e inline/masked.ml.corrected || echo 'no correction'
  no correction

A fixture whose release raises fails a run whose one test passed:

  $ partition release.ml
  [1]
  $ grep FAIL out
    FAIL  fixture release

What a correction writes

The correction holds the sanitized text, and a node that matched keeps
its spelling:

  $ partition sanitized.ml
  $ diff inline/sanitized.ml inline/sanitized.ml.corrected
  18c18
  <   [%expect {| stale |}];
  ---
  >   [%expect {| pid NNNN |}];
  [1]

Output after a test's last node is checked as the payload of a node
that is not there, and the correction appends that node, in a nested
test too. Blank output passes, and a body that raised checks nothing
after it. The partition failed, so standard error says that dune
withholds the correction:

  $ partition trailing.ml
  [1]
  $ grep -E '^inline/|FAIL' out
  inline/trailing.ml: 5 tests
    FAIL  Trailing › output after the last node
    FAIL  Trailing › a body with no node
    FAIL  Trailing › Nested › a nested test's node is indented under its head
    FAIL  Trailing › a body that raises checks nothing after it
  $ cat err
  windtrap: warning: dune registers a correction for promotion only when the run that wrote it exits 0, so the failures above withhold the correction written here. Fix the failures, rerun, then 'dune promote'.
  $ diff inline/trailing.ml inline/trailing.ml.corrected
  14c14,15
  <   print_string "goodbye\n"
  ---
  >   print_string "goodbye\n";
  >   [%expect {| goodbye |}]
  18c19,23
  <   print_endline "two"
  ---
  >   print_endline "two";
  >   [%expect {|
  >     one
  >     two
  >     |}]
  27c32,33
  <     print_string "inner"
  ---
  >     print_string "inner";
  >     [%expect {| inner |}]
  [1]

A node that a run of its test never reaches fails the test when the
body returns. The failure is located at the first such node and names
the others, and it takes no correction. A node reached twice passes,
and each functor instance is a test of its own:

  $ partition unreached.ml
  [1]
  $ grep -E '^inline/|FAIL|unreached\.ml:[0-9]|reaching' out
  inline/unreached.ml: 6 tests
    FAIL  Unreached › a node behind a branch not taken
      test/cram/run/inline/unreached.ml:14
      the body returned without reaching this node
    FAIL  Unreached › every node not reached is named
      test/cram/run/inline/unreached.ml:18
      the body returned without reaching this node, nor the nodes of lines 19, 20
    FAIL  Unreached › a node is judged in each instance (2)
      test/cram/run/inline/unreached.ml:34
      the body returned without reaching this node
    FAIL  Unreached › an override that never calls the body fails
      test/cram/run/inline/unreached.ml:55
      the body returned without reaching this node
  $ test -e inline/unreached.ml.corrected || echo 'no correction'
  no correction

Locations

The assertion that ends a let%test or a let%expect_test body is
located at its own line, not at the test's declaration:

  $ partition tail.ml
  [1]
  $ grep -E -A1 'tail\.ml:[0-9]' out
      test/cram/run/inline/tail.ml:10
        10 │ Windtrap.(equal int 1 two)
  --
      test/cram/run/inline/tail.ml:15
        15 │ Windtrap.is_true false
