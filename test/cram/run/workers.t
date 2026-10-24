Baselines that tests share, across worker processes. The parent alone
judges a check under a correcting mode, in the order of the selection,
so a run gives the same report, the same exit code and the same files
whatever its number of workers, the order its tests end in and the
workers it replaces. Each block runs one suite of workers_main.exe in
a fresh copy of the project, once at -j 1, whose outcome it shows, then
in every other mode, each of which must leave byte for byte what -j 1
left: its exit code, both streams and every file of the project.

  $ fresh() {
  >   rm -rf proj _logs && mkdir -p proj/test/cram/run && cp workers_main.ml proj/test/cram/run/
  >   printf 'h old\n' > proj/test/cram/run/h.expected
  >   printf 'k old\n' > proj/test/cram/run/k.expected
  >   case "$vars" in *table*) printf 't old\n' > proj/test/cram/run/t.expected ;; esac
  > }
  $ scrub() {
  >   sed -E 's/ in [0-9.]+(m|µ)?s\./ in DURATION./'
  > }
  $ outcome() {
  >   vars=; while [ "$#" -gt 0 ] && [ "$1" != -- ]; do vars="$vars $1"; shift; done; shift
  >   fresh
  >   (cd proj && env -i PATH="$PATH" TMPDIR="${TMPDIR:-/tmp}" WINDTRAP_COLOR=never \
  >      WINDTRAP_SLOW_THRESHOLD=0 WINDTRAP_PROJECT_ROOT="$PWD" $vars \
  >      ../workers_main.exe -o ../_logs "$@" > ../out 2> ../err; echo "exit $?" > ../code)
  >   { cat code; scrub < out; cat err
  >     (cd proj && find . -type f | sort | while read f; do echo "### $f"; cat "$f"; done); } > "$result"
  > }
  $ show() {
  >   result=reference; outcome "$@"
  >   cat code; grep '^  FAIL  ' out; tail -1 out | scrub; cat err
  >   (cd proj/test/cram/run && for f in *.expected *.expected.corrected; do
  >      [ -e "$f" ] && echo "$f: $(cat "$f")"; done
  >    for f in workers_main.ml workers_main.ml.corrected; do
  >      [ -e "$f" ] && diff -U0 ../../../../workers_main.ml "$f" | grep '^[-+] '; done; true)
  > }
  $ same() {
  >   result=current; outcome "$@"
  >   if cmp -s reference current; then echo same; else diff reference current; fi
  > }

One literal site that two tests reach with different output: the second
check is a conflict, which keeps no correction, under --corrected:

  $ show WORKERS_SUITE=conflict -- --corrected
  exit 1
    FAIL  a
    FAIL  b
  1 passed, 2 failed, 1 correction written in DURATION.
  windtrap: warning: dune registers a correction for promotion only when the run that wrote it exits 0, so the failures above withhold the correction written here. Fix the failures, rerun, then 'dune promote'.
  h.expected: h old
  k.expected: k old
  -  let check s = expect s @@ __POS_OF__ {|old|} in
  +  let check s = expect s @@ __POS_OF__ {| one |} in
  $ same WORKERS_SUITE=conflict REPLACE=domain -- --corrected
  same
  $ same WORKERS_SUITE=conflict -- --corrected -j 3
  same
  $ same WORKERS_SUITE=conflict REPLACE=domain -- --corrected -j 3
  same

and under -u, where the first correction is written in place:

  $ show WORKERS_SUITE=conflict -- -u
  exit 1
    FAIL  b
  2 passed, 1 failed, 1 correction accepted in DURATION.
  h.expected: h old
  k.expected: k old
  -  let check s = expect s @@ __POS_OF__ {|old|} in
  +  let check s = expect s @@ __POS_OF__ {| one |} in
  $ same WORKERS_SUITE=conflict REPLACE=domain -- -u
  same
  $ same WORKERS_SUITE=conflict -- -u -j 3
  same
  $ same WORKERS_SUITE=conflict REPLACE=domain -- -u -j 3
  same

A worker killed between the two checks fails its test, and the second
check is still a conflict with the first, at -j 1 as at -j 3:

  $ show WORKERS_SUITE=conflict REPLACE=crash -- -u
  exit 1
    FAIL  between
    FAIL  b
  1 passed, 2 failed, 1 correction accepted in DURATION.
  h.expected: h old
  k.expected: k old
  -  let check s = expect s @@ __POS_OF__ {|old|} in
  +  let check s = expect s @@ __POS_OF__ {| one |} in
  $ same WORKERS_SUITE=conflict REPLACE=crash -- -u -j 3
  same

Corrections spread across many tests, under every mode:

  $ show WORKERS_SUITE=spread --
  exit 1
    FAIL  a1 slow, corrects s1
    FAIL  a2 conflicts on s1
    FAIL  b2 slow, corrects s2
    FAIL  c1 corrects s3
    FAIL  c2 agrees on s3
    FAIL  row
    FAIL  d fails outside
    FAIL  f subtests
    FAIL  g labelled
    FAIL  h1 corrects a file
    FAIL  h2 conflicts on the file
    FAIL  m missing file
    FAIL  k1 slow, one spelling
    FAIL  k2 other spelling
    FAIL  r retried conflict (3 attempts)
    FAIL  o order
    FAIL  z corrects s4
  2 passed, 1 expected failure, 17 failed (7 subtest failures) in DURATION.
  h.expected: h old
  k.expected: k old
  $ same WORKERS_SUITE=spread REPLACE=domain --
  same
  $ same WORKERS_SUITE=spread -- -j 3
  same
  $ same WORKERS_SUITE=spread REPLACE=domain -- -j 8
  same

  $ show WORKERS_SUITE=spread -- --corrected
  exit 1
    FAIL  a1 slow, corrects s1
    FAIL  a2 conflicts on s1
    FAIL  b2 slow, corrects s2
    FAIL  b3 matches the old s2
    FAIL  c1 corrects s3
    FAIL  row
    FAIL  d fails outside
    FAIL  f subtests
    FAIL  g labelled
    FAIL  h1 corrects a file
    FAIL  h2 conflicts on the file
    FAIL  m missing file
    FAIL  k1 slow, one spelling
    FAIL  k2 other spelling
    FAIL  r retried conflict (3 attempts)
    FAIL  o order
    FAIL  z corrects s4
  2 passed, 1 expected failure, 17 failed (7 subtest failures), 3 corrections written in DURATION.
  windtrap: warning: dune registers a correction for promotion only when the run that wrote it exits 0, so the failures above withhold the corrections written here. Fix the failures, rerun, then 'dune promote'.
  h.expected: h old
  k.expected: k old
  h.expected.corrected: h new
  k.expected.corrected: k two
  -  let s1 x = expect x @@ __POS_OF__ {| s1 old |} in
  -  let s2 x = expect x @@ __POS_OF__ {| s2 old |} in
  -  let s3 x = expect_exact x @@ __POS_OF__ {|s3 old|} in
  -  let s4 x = expect x @@ __POS_OF__ {| s4 old |} in
  +  let s1 x = expect x @@ __POS_OF__ {| s1 new |} in
  +  let s2 x = expect x @@ __POS_OF__ {| s2 new |} in
  +  let s3 x = expect_exact x @@ __POS_OF__ "s3\r\nnew" in
  +  let s4 x = expect x @@ __POS_OF__ {| s4 x |} in
  -      [ ("x", __POS_OF__ {| row x old |}); ("y", __POS_OF__ {| row y old |}) ]
  +      [ ("x", __POS_OF__ {| row x new |}); ("y", __POS_OF__ {| row y new |}) ]
  $ same WORKERS_SUITE=spread REPLACE=domain -- --corrected
  same
  $ same WORKERS_SUITE=spread -- --corrected -j 3
  same
  $ same WORKERS_SUITE=spread REPLACE=domain -- --corrected -j 8
  same

  $ show WORKERS_SUITE=spread -- -u
  exit 1
    FAIL  a2 conflicts on s1
    FAIL  b3 matches the old s2
    FAIL  d fails outside
    FAIL  f subtests
    FAIL  g labelled
    FAIL  h2 conflicts on the file
    FAIL  r retried conflict (3 attempts)
    FAIL  o order
    FAIL  z corrects s4
  10 passed, 1 expected failure, 9 failed (4 subtest failures), 4 corrections accepted in DURATION.
  h.expected: h new
  k.expected: k two
  missing.expected: m
  -  let s1 x = expect x @@ __POS_OF__ {| s1 old |} in
  -  let s2 x = expect x @@ __POS_OF__ {| s2 old |} in
  -  let s3 x = expect_exact x @@ __POS_OF__ {|s3 old|} in
  -  let s4 x = expect x @@ __POS_OF__ {| s4 old |} in
  +  let s1 x = expect x @@ __POS_OF__ {| s1 new |} in
  +  let s2 x = expect x @@ __POS_OF__ {| s2 new |} in
  +  let s3 x = expect_exact x @@ __POS_OF__ "s3\r\nnew" in
  +  let s4 x = expect x @@ __POS_OF__ {| s4 x |} in
  -      [ ("x", __POS_OF__ {| row x old |}); ("y", __POS_OF__ {| row y old |}) ]
  +      [ ("x", __POS_OF__ {| row x new |}); ("y", __POS_OF__ {| row y new |}) ]
  $ same WORKERS_SUITE=spread REPLACE=domain -- -u
  same
  $ same WORKERS_SUITE=spread -- -u -j 3
  same
  $ same WORKERS_SUITE=spread REPLACE=domain -- -u -j 8
  same

Each row of a table settles its corrections alone, in the order of the
rows, as a test of its own would: rows a and c keep theirs, row b, which
also fails outside its expectations, keeps none, so row c is compared
with the file as it was. A table that runs alone, in its worker or in
the parent's process, leaves the same, at -j 1 as at -j 8:

  $ show WORKERS_SUITE=table -- --corrected
  exit 1
    FAIL  rows
  1 failed (5 subtest failures), 2 corrections written in DURATION.
  windtrap: warning: dune registers a correction for promotion only when the run that wrote it exits 0, so the failures above withhold the corrections written here. Fix the failures, rerun, then 'dune promote'.
  h.expected: h old
  k.expected: k old
  t.expected: t old
  t.expected.corrected: t c
  -      ("a", __POS_OF__ {| a old |}, false);
  +      ("a", __POS_OF__ {| a new |}, false);
  -      ("c", __POS_OF__ {| c old |}, false);
  +      ("c", __POS_OF__ {| c new |}, false);
  $ same WORKERS_SUITE=table -- --corrected --in-process
  same
  $ same WORKERS_SUITE=table -- --corrected -j 8
  same
  $ show WORKERS_SUITE=table -- -u
  exit 1
    FAIL  rows
  1 failed, 2 corrections accepted in DURATION.
  h.expected: h old
  k.expected: k old
  t.expected: t c
  -      ("a", __POS_OF__ {| a old |}, false);
  +      ("a", __POS_OF__ {| a new |}, false);
  -      ("c", __POS_OF__ {| c old |}, false);
  +      ("c", __POS_OF__ {| c new |}, false);
  $ same WORKERS_SUITE=table -- -u --in-process
  same
  $ same WORKERS_SUITE=table -- -u -j 8
  same

and beside the tests that other workers run meanwhile, one of which
checks the file the table corrected, and a property whose law checks a
literal: its mismatch refutes a case and is never a correction, so -u
leaves that literal as it was:

  $ show WORKERS_SUITE=tables -- --corrected --seed s1:0000000000000001
  exit 1
    FAIL  slow, corrects s5
    FAIL  rows
    FAIL  a law checks s6
  1 passed, 3 failed (5 subtest failures), 2 corrections written in DURATION.
  windtrap: warning: dune registers a correction for promotion only when the run that wrote it exits 0, so the failures above withhold the corrections written here. Fix the failures, rerun, then 'dune promote'.
  h.expected: h old
  k.expected: k old
  t.expected: t old
  t.expected.corrected: t c
  -      ("a", __POS_OF__ {| a old |}, false);
  +      ("a", __POS_OF__ {| a new |}, false);
  -      ("c", __POS_OF__ {| c old |}, false);
  +      ("c", __POS_OF__ {| c new |}, false);
  -        expect "s5 new" @@ __POS_OF__ {| s5 old |});
  +        expect "s5 new" @@ __POS_OF__ {| s5 new |});
  $ same WORKERS_SUITE=tables -- --corrected --seed s1:0000000000000001 -j 8
  same
  $ same WORKERS_SUITE=tables REPLACE=domain -- --corrected --seed s1:0000000000000001 -j 8
  same
  $ show WORKERS_SUITE=tables -- -u --seed s1:0000000000000001
  exit 1
    FAIL  rows
    FAIL  a law checks s6
  2 passed, 2 failed, 2 corrections accepted in DURATION.
  h.expected: h old
  k.expected: k old
  t.expected: t c
  -      ("a", __POS_OF__ {| a old |}, false);
  +      ("a", __POS_OF__ {| a new |}, false);
  -      ("c", __POS_OF__ {| c old |}, false);
  +      ("c", __POS_OF__ {| c new |}, false);
  -        expect "s5 new" @@ __POS_OF__ {| s5 old |});
  +        expect "s5 new" @@ __POS_OF__ {| s5 new |});
  $ same WORKERS_SUITE=tables -- -u --seed s1:0000000000000001 -j 8
  same
  $ same WORKERS_SUITE=tables REPLACE=domain -- -u --seed s1:0000000000000001 -j 8
  same

Under -x the run stops at the first failure, and keeps nothing from the
tests that other workers ran past it:

  $ show WORKERS_SUITE=spread -- -x -u
  exit 1
    FAIL  a2 conflicts on s1
  1 passed, 1 failed, 18 not run, 1 correction accepted in DURATION.
  h.expected: h old
  k.expected: k old
  -  let s1 x = expect x @@ __POS_OF__ {| s1 old |} in
  +  let s1 x = expect x @@ __POS_OF__ {| s1 new |} in
  $ same WORKERS_SUITE=spread -- -x -u -j 8
  same
  $ same WORKERS_SUITE=spread REPLACE=domain -- -x -u -j 8
  same
