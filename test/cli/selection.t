What narrows a run, and what the runner says when the narrowing leaves
nothing: the listing, Law 11's nothing-ran exit, and the sentence that
names the selection back to the reader in the words they typed.

  $ run() {
  >   env -i PATH="$PATH" WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 \
  >       WINDTRAP_PROJECT_ROOT="$PWD" \
  >       WINDTRAP_OUTPUT="$PWD/_logs" "$@"
  > }

-l lists the selection in declaration order, and runs nothing:

  $ run ./suite_main.exe -l -f math
  math › adds
  math › subtracts

Patterns add up, where tags narrow: a second -f, or a second bare
pattern, keeps the tests that contain either one, and an empty
selection names every pattern back.

  $ run ./suite_main.exe -l -f adds boom
  math › adds
  boom
  $ run ./suite_main.exe -l zzznope yyy
  windtrap: no tests ran: filter "zzznope" or "yyy" matched none of 5 tests.

A listing whose filter matches nothing says why on stderr, leaves
stdout empty for whatever reads the paths, and still exits 0: it did
what it was asked, and answering a mistyped filter with silence would be
the dead end the empty run's own "list:" hint leads to.

  $ run ./suite_main.exe -l -f zzznope > out 2> err
  $ cat out
  $ cat err
  windtrap: no tests ran: filter "zzznope" matched none of 5 tests.

Without -l the same selection is Law 11's nothing-ran: exit 2, the
suite named, and the way out spelled:

  $ run ./suite_main.exe -f zzznope
  fixture: no tests ran: filter "zzznope" matched none of 5 tests.
  list: ./suite_main.exe -l
  [2]

Under --corrected (what a build action passes) the same emptied
selection is not an error: a WINDTRAP_* selection spans every stanza
and inline partition of the tree, so a stanza it leaves empty exits 0
with the same line, and dune's diff? is the verdict. A build action has
no launcher to restate, so the way out names the flag. A command line
that does not parse still exits 2 under the flag.

  $ run ./suite_main.exe -f zzznope --corrected
  fixture: no tests ran: filter "zzznope" matched none of 5 tests.
  (list the suite's tests with -l)
  $ run ./suite_main.exe --corrected --nosuchflag > /dev/null 2>&1
  [2]

A selection that fails exits 1. Durations move and the declaration line
moves with the fixture, so both are filtered; everything else is the
transcript byte for byte.

  $ run ./suite_main.exe -f boom > out 2>&1
  [1]
  $ sed -E 's/ in [0-9.]+m?s\./ in DURATION./; s/suite_main\.ml:[0-9]+/suite_main.ml:LINE/' out
  fixture: 1 test
  ──────────────────────── failures ────────────────────────
    FAIL  boom
      test/cli/suite_main.ml:LINE
      deliberate
      expected  1
      actual    2
  ──────────────────────────────────────────────────────────
  
  1 failed in DURATION.

-x stops at the first counted failure, and the summary says how many
selected tests the run never reached: "1 failed" alone would read as
"the rest passed".

  $ run ./suite_main.exe -x > out 2>&1
  [1]
  $ sed -E 's/ in [0-9.]+m?s\./ in DURATION./' out | tail -n 1
  2 passed, 1 failed, 2 not run in DURATION.

A selection that passes exits 0 (the four tests left when the one
failing test is excluded, baseline and slow-tagged test included). The
baseline is planted where WINDTRAP_PROJECT_ROOT sends the child's
lookup:

  $ mkdir -p test/cli
  $ echo 'hello from the fixture' > test/cli/greeting.expected
  $ run ./suite_main.exe -e boom > out 2> err
  $ sed -E 's/ in [0-9.]+m?s\./ in DURATION./' out
  fixture: 4 passed in DURATION.
  $ cat err

Every narrowing flag at once, so the sentence has to name each part and
join the last with "and":

  $ run ./suite_main.exe -l -f zzznope -e yyy --tag a --tag b --exclude-tag c --shard 1/3
  windtrap: no tests ran: filter "zzznope", exclusion "yyy", tag "a", "b", excluded tag "c" and shard 1/3 matched none of 5 tests.

The sentence is meant to be read and retyped, so it escapes for a
reader rather than for OCaml: the quote, the backslash, the three named
control characters, and a hex fallback for everything else below space.

  $ filter=$(printf 'a"b\\c\nd\te\rf\001g\177h')
  $ run ./suite_main.exe -l -f "$filter"
  windtrap: no tests ran: filter "a\"b\\c\nd\te\rf\x01g\x7fh" matched none of 5 tests.

--failed is the one part of the sentence with a prerequisite: the store
the last run left under -o. That coupling is the whole scenario: the
recording run first, the flag joining the sentence second.

  $ run ./suite_main.exe -f boom -o store > /dev/null 2>&1
  [1]
  $ run ./suite_main.exe -l -o store --failed --tag zzznope
  windtrap: no tests ran: tag "zzznope" and --failed matched none of 5 tests.

A listing still makes the startup checks a real run makes, and reports
them the way a real run does: the message on stderr, and the check's
own exit code rather than the listing's 0.

  $ mkdir empty-store
  $ run ./suite_main.exe -l -o empty-store --failed > out 2> err
  [2]
  $ cat out
  $ cat err
  windtrap: no recorded failures match the current suite
