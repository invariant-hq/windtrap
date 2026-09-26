`windtrap mutants` merges the verdict files of a project's executables and
reports its survivors. Its discovery and its exclusion of files from other
builds are data.t's; this session pins the merge and its report. A path
the command builds is absolute and spelled by the platform, so a verdict
file is shown by its name. The projects lie in a scratch directory outside
the build directory, whose own verdict files the command would otherwise
find.

  $ bin=$PWD
  $ mkdata() { "$bin/mkdata.exe" "$@"; }
  $ run() {
  >   env -i PATH="$PATH" WINDTRAP_COLOR=never "$@" > "$bin/out" 2> "$bin/err"
  >   code=$?; cat "$bin/out"
  >   if [ -s "$bin/err" ]; then
  >     echo '--- stderr'
  >     sed -E 's#[^ ()]*[/\\]([^/\\ ()]+\.mutants)#.../\1#g' "$bin/err"
  >   fi
  >   return $code
  > }
  $ scratch=$(cd "$(mktemp -d)" && pwd -P)

Every project holds the same two sources and draws its mutants from five,
each written as a verdict file records it:

  $ plant() {
  >   mkdir -p "$1/lib"
  >   printf 'let add a b = a + b\nlet sub a b = a - b\nlet cmp a b = a < b\n' > "$1/lib/calc.ml"
  >   printf 'let ok p q = p || q\nlet neither p q = not (p || q)\nlet both p q = p && q\n' > "$1/lib/util.ml"
  > }
  $ add='lib/calc.ml:1:14:add:a + b:a - b'
  $ sub='lib/calc.ml:2:14:sub:a - b:a + b'
  $ lt='lib/calc.ml:3:14:lt:a < b:a <= b'
  $ or='lib/util.ml:1:13:or:p || q:p && q'
  $ and='lib/util.ml:3:22:and:p && q:p || q'

The merge

Three executables over one library. A kills add, which B only reaches;
sub survives in B and in C, with different reaching tests; lt survives in
A, and a crash in B kills it; no test reaches or and and:

  $ plant "$scratch/three" && cd "$scratch/three"
  $ mkdata mutants _build/_mutants/windtrap-a.mutants "$add=killed" "$sub=unreached" \
  >   "$lt=survived:calc/compares" "$or=unreached" "$and=unreached"
  $ mkdata mutants _build/_mutants/windtrap-b.mutants "$add=survived:cli/runs" \
  >   "$sub=survived:cli/subtracts" "$lt=killed" "$or=unreached" "$and=unreached"
  $ mkdata mutants _build/_mutants/windtrap-c.mutants "$add=unreached" \
  >   "$sub=survived:prop/sub law" "$lt=unreached" "$or=unreached" "$and=unreached"

A mutant killed anywhere is killed. The one survivor is drawn from its
record, its reaching tests are those of every executable that reached it,
each named by its file, and the mutants no test reached are counted by
file:

  $ run windtrap mutants
  ───────────────────── survivors (1) ──────────────────────
    SURVIVED  lib/calc.ml:2:14:sub  a - b → a + b
        2 │ let sub a b = a - b
  
      2 tests in 2 executables ran this line and none failed:
        windtrap-b.mutants  cli › subtracts
        windtrap-c.mutants  prop › sub law
  ──────────────────────────────────────────────────────────
  
  ─────────────────── never reached (2) ────────────────────
    2  lib/util.ml   lines 1, 3
  ──────────────────────────────────────────────────────────
  
  reproduce: WINDTRAP_MUTATE_ARM=lib/calc.ml:2:14:sub dune runtest --force --instrument-with ppx_windtrap.mutate
  mutants: 1 survived of 3 reached, 2 killed, 2 never reached, 3 executables
  [1]
  $ cp "$bin/out" merged

The order of the files on the command line is not part of the answer:

  $ run windtrap mutants _build/_mutants/windtrap-c.mutants \
  >   _build/_mutants/windtrap-a.mutants _build/_mutants/windtrap-b.mutants > /dev/null
  [1]
  $ diff merged "$bin/out"

Sources are found under the root of the project from a directory below it
too:

  $ cd lib && run windtrap mutants > /dev/null; grep '│' "$bin/out"; cd ..
        2 │ let sub a b = a - b

WINDTRAP_COLOR=always colours SURVIVED red, a never-reached count yellow and
the counts as a suite's summary does; the command to reproduce stays plain:

  $ esc() { sed "s/$(printf '\033')/\\\\e/g"; }
  $ run WINDTRAP_COLOR=always windtrap mutants > coloured
  [1]
  $ esc < coloured
  \e[2m───────────────────── survivors (1) ──────────────────────\e[0m
    \e[31mSURVIVED\e[0m  \e[1mlib/calc.ml:2:14:sub\e[0m  a - b → a + b
        \e[2m2 │\e[0m let sub a b = a - b
  
      2 tests in 2 executables ran this line and none failed:
        windtrap-b.mutants  cli › subtracts
        windtrap-c.mutants  prop › sub law
  \e[2m──────────────────────────────────────────────────────────\e[0m
  
  \e[2m─────────────────── never reached (2) ────────────────────\e[0m
    \e[33m2\e[0m  lib/util.ml   lines 1, 3
  \e[2m──────────────────────────────────────────────────────────\e[0m
  
  reproduce: WINDTRAP_MUTATE_ARM=lib/calc.ml:2:14:sub dune runtest --force --instrument-with ppx_windtrap.mutate
  mutants: \e[31m1 survived\e[0m of 3 reached, \e[32m2 killed\e[0m, \e[33m2 never reached\e[0m, 3 executables

B's file alone reports the survivor A kills. Survivors that as many tests
reached are in identifier order:

  $ run windtrap mutants _build/_mutants/windtrap-b.mutants
  ───────────────────── survivors (2) ──────────────────────
    SURVIVED  lib/calc.ml:1:14:add  a + b → a - b
        1 │ let add a b = a + b
  
      1 test ran this line and did not fail:
        windtrap-b.mutants  cli › runs
  
    SURVIVED  lib/calc.ml:2:14:sub  a - b → a + b
        2 │ let sub a b = a - b
  
      1 test ran this line and did not fail:
        windtrap-b.mutants  cli › subtracts
  ──────────────────────────────────────────────────────────
  
  ─────────────────── never reached (2) ────────────────────
    2  lib/util.ml   lines 1, 3
  ──────────────────────────────────────────────────────────
  
  reproduce: WINDTRAP_MUTATE_ARM=lib/calc.ml:1:14:add dune runtest --force --instrument-with ppx_windtrap.mutate
  mutants: 2 survived of 3 reached, 1 killed, 2 never reached, 1 executable
  [1]

A project with nothing to report is one line, and exits 0:

  $ plant "$scratch/clean" && cd "$scratch/clean"
  $ mkdata mutants _build/_mutants/all.mutants "$add=killed" "$sub=killed" "$lt=killed"
  $ run windtrap mutants
  mutants: 3 reached, 3 killed, 1 executable

A mutant that no test reached is listed and not scored, so it leaves the
exit code 0, and there is no command to reproduce:

  $ plant "$scratch/unreached" && cd "$scratch/unreached"
  $ mkdata mutants _build/_mutants/all.mutants "$add=killed" "$sub=killed" "$or=unreached"
  $ run windtrap mutants
  ─────────────────── never reached (1) ────────────────────
    1  lib/util.ml   line 1
  ──────────────────────────────────────────────────────────
  
  mutants: 2 reached, 2 killed, 1 never reached, 1 executable

A site that ran outside every test in one executable is not never reached,
and a test's verdict in another executable outranks it:

  $ plant "$scratch/outside" && cd "$scratch/outside"
  $ mkdata mutants _build/_mutants/a.mutants "$sub=outside_tests" "$or=outside_tests" "$and=unreached"
  $ mkdata mutants _build/_mutants/b.mutants "$sub=killed" "$or=unreached" "$and=unreached"
  $ run windtrap mutants
  ─────────────────── never reached (1) ────────────────────
    1  lib/util.ml   line 3
  ──────────────────────────────────────────────────────────
  
  ────────────── evaluated outside tests (1) ───────────────
    These sites ran outside every test, at module initialization or in a fixture release.
    1  lib/util.ml   line 1
  ──────────────────────────────────────────────────────────
  
  mutants: 1 reached, 1 killed, 1 never reached, 1 evaluated outside tests, 2 executables

A mutant whose site one executable's child did not evaluate is no
survivor: it is listed with the command that arms it, and exits 0:

  $ plant "$scratch/not-evaluated" && cd "$scratch/not-evaluated"
  $ mkdata mutants _build/_mutants/a.mutants "$add=killed" "$sub=survived:cli/runs"
  $ mkdata mutants _build/_mutants/b.mutants "$add=killed" "$sub=not_evaluated"
  $ run windtrap mutants
  ─────────────────── not evaluated (1) ────────────────────
    Each site ran in the dry run and not in its mutant's child.
    lib/calc.ml:2:14:sub  a - b → a + b
      arm: WINDTRAP_MUTATE_ARM=lib/calc.ml:2:14:sub dune runtest --force --instrument-with ppx_windtrap.mutate
  ──────────────────────────────────────────────────────────
  
  mutants: 2 reached, 1 killed, 1 not evaluated, 2 executables

Survivors are ordered by how many tests reached them, then by identifier:

  $ plant "$scratch/order" && cd "$scratch/order"
  $ mkdata mutants _build/_mutants/one.mutants "$add=survived:t/a" "$sub=survived:t/b;t/c"
  $ mkdata mutants _build/_mutants/two.mutants "$sub=survived:u/d"
  $ run windtrap mutants
  ───────────────────── survivors (2) ──────────────────────
    SURVIVED  lib/calc.ml:2:14:sub  a - b → a + b
        2 │ let sub a b = a - b
  
      3 tests in 2 executables ran this line and none failed:
        one.mutants  t › b
        one.mutants  t › c
        two.mutants  u › d
  
    SURVIVED  lib/calc.ml:1:14:add  a + b → a - b
        1 │ let add a b = a + b
  
      1 test ran this line and did not fail:
        one.mutants  t › a
  ──────────────────────────────────────────────────────────
  
  reproduce: WINDTRAP_MUTATE_ARM=lib/calc.ml:2:14:sub dune runtest --force --instrument-with ppx_windtrap.mutate
  mutants: 2 survived of 2 reached, 2 executables
  [1]

Reaching tests sort by their spelled path, where a space sorts before the
separator:

  $ plant "$scratch/spelled" && cd "$scratch/spelled"
  $ mkdata mutants _build/_mutants/s.mutants "$add=survived:a/b;a b"
  $ run windtrap mutants
  ───────────────────── survivors (1) ──────────────────────
    SURVIVED  lib/calc.ml:1:14:add  a + b → a - b
        1 │ let add a b = a + b
  
      2 tests ran this line and none failed:
        s.mutants  a b
        s.mutants  a › b
  ──────────────────────────────────────────────────────────
  
  reproduce: WINDTRAP_MUTATE_ARM=lib/calc.ml:1:14:add dune runtest --force --instrument-with ppx_windtrap.mutate
  mutants: 1 survived of 1 reached, 1 executable
  [1]

A survivor whose source is not found keeps its identifier and its
rewrite, with no source line:

  $ mkdir "$scratch/no-sources" && cd "$scratch/no-sources"
  $ mkdata mutants _build/_mutants/b.mutants "$sub=survived:t/b"
  $ run windtrap mutants
  ───────────────────── survivors (1) ──────────────────────
    SURVIVED  lib/calc.ml:2:14:sub  a - b → a + b
  
      1 test ran this line and did not fail:
        b.mutants  t › b
  ──────────────────────────────────────────────────────────
  
  reproduce: WINDTRAP_MUTATE_ARM=lib/calc.ml:2:14:sub dune runtest --force --instrument-with ppx_windtrap.mutate
  mutants: 1 survived of 1 reached, 1 executable
  [1]

Executables

A file that records the executable that wrote it names its reaching tests
by the executable's name, and the command to reproduce a survivor runs that
executable through dune:

  $ plant "$scratch/fresh" && cd "$scratch/fresh"
  $ mkdir -p _build/default/test && echo 'a suite' > _build/default/test/a.exe
  $ mkdata mutants _build/_mutants/a.mutants --exe _build/default/test/a.exe \
  >   "$add=killed" "$sub=unreached" "$lt=survived:calc/compares" "$or=unreached" "$and=unreached"
  $ run windtrap mutants
  ───────────────────── survivors (1) ──────────────────────
    SURVIVED  lib/calc.ml:3:14:lt  a < b → a <= b
        3 │ let cmp a b = a < b
  
      1 test ran this line and did not fail:
        a.exe  calc › compares
  ──────────────────────────────────────────────────────────
  
  ─────────────────── never reached (3) ────────────────────
    1  lib/calc.ml   line 2
    2  lib/util.ml   lines 1, 3
  ──────────────────────────────────────────────────────────
  
  reproduce: dune exec --instrument-with ppx_windtrap.mutate test/a.exe -- --arm lib/calc.ml:3:14:lt
  mutants: 1 survived of 2 reached, 1 killed, 3 never reached, 1 executable
  [1]

A file of an executable that no longer exists, or that was rebuilt since,
is excluded, so its kill never reaches the report, and the remedy is said
once:

  $ echo 'a sibling' > _build/default/test/b.exe
  $ echo 'another sibling' > _build/default/test/c.exe
  $ mkdata mutants _build/_mutants/b.mutants --exe _build/default/test/b.exe "$lt=killed"
  $ mkdata mutants _build/_mutants/c.mutants --exe _build/default/test/c.exe "$lt=killed"
  $ rm _build/default/test/b.exe
  $ echo 'rebuilt since' > _build/default/test/c.exe
  $ run windtrap mutants
  ───────────────────── survivors (1) ──────────────────────
    SURVIVED  lib/calc.ml:3:14:lt  a < b → a <= b
        3 │ let cmp a b = a < b
  
      1 test ran this line and did not fail:
        a.exe  calc › compares
  ──────────────────────────────────────────────────────────
  
  ─────────────────── never reached (3) ────────────────────
    1  lib/calc.ml   line 2
    2  lib/util.ml   lines 1, 3
  ──────────────────────────────────────────────────────────
  
  reproduce: dune exec --instrument-with ppx_windtrap.mutate test/a.exe -- --arm lib/calc.ml:3:14:lt
  mutants: 1 survived of 2 reached, 1 killed, 3 never reached, 1 executable
  --- stderr
  windtrap: .../b.mutants: its executable (_build/default/test/b.exe) no longer exists; excluding it
  windtrap: .../c.mutants: not written by the executable now at _build/default/test/c.exe (rebuilt since); excluding it
  windtrap: re-run every suite with its mutants, then merge again; delete the files whose executable no longer exists
    WINDTRAP_MUTATE=1 dune runtest --force --instrument-with ppx_windtrap.mutate
  [1]

When every file is excluded, the command says what a verdict file is and
what invalidates one:

  $ echo 'rebuilt since' > _build/default/test/a.exe
  $ rm _build/_mutants/b.mutants _build/_mutants/c.mutants
  $ run windtrap mutants
  --- stderr
  windtrap: .../a.mutants: not written by the executable now at _build/default/test/a.exe (rebuilt since); excluding it
  windtrap: found 1 .mutants file and every one is stale
    A verdict is written only by a run asked to test its mutants, and it is
    invalidated by any later build of the executable that wrote it.
  windtrap: re-run every suite with its mutants, then merge again; delete the files whose executable no longer exists
    WINDTRAP_MUTATE=1 dune runtest --force --instrument-with ppx_windtrap.mutate
  [1]

An executable outside every build directory writes under _windtrap, and
the remedy names no command:

  $ rm _build/_mutants/a.mutants
  $ echo 'a hand-built suite' > no-such-exe
  $ mkdata mutants _windtrap/mutants/abs.mutants --exe no-such-exe "$lt=killed"
  $ rm no-such-exe
  $ run windtrap mutants > abs
  [1]
  $ sed -E 's#\(.*[/\\]no-such-exe\)#(.../no-such-exe)#' abs
  --- stderr
  windtrap: .../abs.mutants: its executable (.../no-such-exe) no longer exists; excluding it
  windtrap: found 1 .mutants file and every one is orphaned
    A verdict is written only by a run asked to test its mutants, and it is
    invalidated by any later build of the executable that wrote it.
  windtrap: re-run every suite with its mutants (--mutate, instrumented with ppx_windtrap.mutate, forcing the runs your build tool cached), then merge again; delete the files whose executable no longer exists

The executable column names each file by its executable's basename, an
inline-test runner by its library, and a file with no identity by its own
name. Rows sort by executable, then test, in one column as wide as the
widest name. The survivor is reproduced in the first executable of its
reaching tests that runs alone, here test_calc.exe, though an inline test
comes first:

  $ plant "$scratch/labels" && cd "$scratch/labels"
  $ mkdir -p _build/default/test _build/default/lib/.my_lib_expect.inline-tests
  $ echo 'the unit suite' > _build/default/test/test_calc.exe
  $ echo 'the inline runner' > _build/default/lib/.my_lib_expect.inline-tests/inline-test-runner.exe
  $ mkdata mutants _build/_mutants/unit.mutants --exe _build/default/test/test_calc.exe \
  >   "$add=survived:calc/adds;calc/adds zero"
  $ mkdata mutants _build/_mutants/inline.mutants \
  >   --exe _build/default/lib/.my_lib_expect.inline-tests/inline-test-runner.exe \
  >   "$add=survived:my_lib_expect/add"
  $ mkdata mutants _build/_mutants/plain.mutants "$add=survived:hand/written"
  $ run windtrap mutants
  ───────────────────── survivors (1) ──────────────────────
    SURVIVED  lib/calc.ml:1:14:add  a + b → a - b
        1 │ let add a b = a + b
  
      4 tests in 3 executables ran this line and none failed:
        my_lib_expect  my_lib_expect › add
        plain.mutants  hand › written
        test_calc.exe  calc › adds
        test_calc.exe  calc › adds zero
  ──────────────────────────────────────────────────────────
  
  reproduce: dune exec --instrument-with ppx_windtrap.mutate test/test_calc.exe -- --arm lib/calc.ml:1:14:add
  mutants: 1 survived of 1 reached, 3 executables
  [1]

Without it, the survivor is reproduced through the build, since dune alone
runs an inline-test runner:

  $ run windtrap mutants _build/_mutants/inline.mutants _build/_mutants/plain.mutants | grep reproduce
  reproduce: WINDTRAP_MUTATE_ARM=lib/calc.ml:1:14:add dune runtest --force --instrument-with ppx_windtrap.mutate

One column serves every block:

  $ plant "$scratch/column" && cd "$scratch/column"
  $ mkdir -p _build/default/test
  $ echo short > _build/default/test/t.exe
  $ echo long > _build/default/test/a_long_name.exe
  $ mkdata mutants _build/_mutants/short.mutants --exe _build/default/test/t.exe "$add=survived:calc/adds"
  $ mkdata mutants _build/_mutants/long.mutants --exe _build/default/test/a_long_name.exe \
  >   "$sub=survived:calc/subtracts"
  $ run windtrap mutants | grep '›'
        t.exe            calc › adds
        a_long_name.exe  calc › subtracts

The command runs an executable in which the mutant survived. Two files
name t.exe here, and the first in path order did not reach the mutant:

  $ plant "$scratch/launcher" && cd "$scratch/launcher"
  $ mkdir -p _build/default/a _build/default/b
  $ echo 'suite a' > _build/default/a/t.exe
  $ echo 'suite b' > _build/default/b/t.exe
  $ mkdata mutants _build/_mutants/1.mutants --exe _build/default/a/t.exe "$add=unreached"
  $ mkdata mutants _build/_mutants/2.mutants --exe _build/default/b/t.exe "$add=survived:calc/adds"
  $ run windtrap mutants | grep reproduce
  reproduce: dune exec --instrument-with ppx_windtrap.mutate b/t.exe -- --arm lib/calc.ml:1:14:add

A target without a directory is spelled from the current one, whether the
executable is at the root of the build context or its identity has one
component:

  $ plant "$scratch/root-exe" && cd "$scratch/root-exe"
  $ mkdir -p _build/default && echo 'a suite' > _build/default/t.exe
  $ mkdata mutants _build/_mutants/t.mutants --exe _build/default/t.exe "$add=survived:calc/adds"
  $ run windtrap mutants | grep reproduce
  reproduce: dune exec --instrument-with ppx_windtrap.mutate ./t.exe -- --arm lib/calc.ml:1:14:add
  $ plant "$scratch/one-component" && cd "$scratch/one-component"
  $ mkdir _build && echo 'a suite' > _build/t.exe
  $ mkdata mutants _build/_mutants/t.mutants --exe _build/t.exe "$add=survived:calc/adds"
  $ run windtrap mutants | grep reproduce
  reproduce: dune exec --instrument-with ppx_windtrap.mutate ./t.exe -- --arm lib/calc.ml:1:14:add

An executable under no build directory is run as it is, and a path a
shell would split is one quoted word, as is such a dune target:

  $ plant "$scratch/built by hand" && cd "$scratch/built by hand"
  $ echo 'built by hand' > test_calc.exe
  $ mkdata mutants _windtrap/mutants/calc.mutants --exe test_calc.exe "$add=survived:calc/adds"
  $ run windtrap mutants | grep reproduce | sed "s|'.*/built by hand/|'.../built by hand/|"
  reproduce: '.../built by hand/test_calc.exe' --arm lib/calc.ml:1:14:add
  $ plant "$scratch/spaced" && cd "$scratch/spaced"
  $ mkdir -p "_build/default/my tests" && echo 'a suite' > "_build/default/my tests/a.exe"
  $ mkdata mutants _build/_mutants/a.mutants --exe "_build/default/my tests/a.exe" "$add=survived:calc/adds"
  $ run windtrap mutants | grep reproduce
  reproduce: dune exec --instrument-with ppx_windtrap.mutate 'my tests/a.exe' -- --arm lib/calc.ml:1:14:add

Files that cannot be merged

Nothing to merge is exit 1, with a hint that names the backend and the
flag a verdict needs:

  $ mkdir "$scratch/empty" && cd "$scratch/empty"
  $ run windtrap mutants
  --- stderr
  windtrap: no .mutants files found
  Instrument the library under test with ppx_windtrap.mutate and run every suite with its mutants (--mutate) first; every mutation run writes its verdicts under the build directory's _mutants or under _windtrap/mutants.
  [1]

A truncated file is corrupt, and nothing is reported:

  $ plant "$scratch/truncated" && cd "$scratch/truncated"
  $ mkdata mutants whole.mutants "$add=killed" "$sub=unreached" "$lt=survived:calc/compares"
  $ mkdir -p _build/_mutants
  $ head -c $(( $(wc -c < whole.mutants) - 6 )) whole.mutants > _build/_mutants/cut.mutants
  $ run windtrap mutants
  --- stderr
  windtrap: .../cut.mutants: corrupt verdict file: truncated test name
  [1]

A file of another format is refused and never converted, and the remedy
is to delete it, which a coverage dump in the directory shares:

  $ mkdir -p "$scratch/foreign/_build/_mutants" && cd "$scratch/foreign"
  $ printf 'windtrap-mutants-v0\n1\nsome older payload\n' > _build/_mutants/old.mutants
  $ run windtrap mutants
  --- stderr
  windtrap: .../old.mutants: not a windtrap verdict file (expected header "windtrap-mutants-v3", found "windtrap-mutants-v0"); files written by other windtrap versions are not readable - delete the stale verdict files, then re-run the mutation tests
  [1]
  $ rm _build/_mutants/old.mutants
  $ printf 'windtrap-coverage-v3\n0\n' > _build/_mutants/cov.mutants
  $ run windtrap mutants
  --- stderr
  windtrap: .../cov.mutants: not a windtrap verdict file (expected header "windtrap-mutants-v3", found "windtrap-coverage-v3"); files written by other windtrap versions are not readable - delete the stale verdict files, then re-run the mutation tests
  [1]

A PATH that cannot be used fails the command, and the files named beside
it are not reported:

  $ cd "$scratch/three"
  $ run windtrap mutants _build/_mutants/windtrap-a.mutants absent.mutants
  --- stderr
  windtrap: absent.mutants: no such file or directory
  [1]

The command line


--help says what the command does and does not do, and its exit code, in
80 columns:

  $ run windtrap mutants --help
  windtrap mutants - merge .mutants verdict files and report the survivors
  
  usage: windtrap mutants [OPTIONS] [PATH...]
  
  Merges the .mutants verdict files written by mutation runs and reports the
  mutants that survived every test executable. Without PATH arguments the files
  are found under the build directory's _mutants (or _windtrap/mutants in a tree
  built without one), walking up from the current directory to the enclosing
  project root; PATH arguments (.mutants files, or directories searched
  recursively) replace that default.
  
  Runs no tests and drives no build.
  Exits 1 when any mutant survived every executable that reached it.
  
  OPTIONS:
    --color=MODE (env WINDTRAP_COLOR)
        Color output: always, never or auto.
  
    -h, --help
        Print this help and exit.
  
  ENVIRONMENT (no flag):
    NO_COLOR
        Any value: never style output (--color auto).
  $ cp "$bin/out" help
  $ awk '{ sub(/\r$/, "") } length > 80' help
  $ run windtrap mutants -h > /dev/null && diff help "$bin/out"
  $ run windtrap mutants -help > /dev/null && diff help "$bin/out"

An unknown option is a usage error, as is a lone dash:

  $ run windtrap mutants --frobnicate
  --- stderr
  windtrap: unknown option '--frobnicate'
  usage: windtrap mutants [OPTIONS] [PATH...]
  [2]
  $ run windtrap mutants -
  --- stderr
  windtrap: unknown option '-'
  usage: windtrap mutants [OPTIONS] [PATH...]
  [2]

--color is the runner's flag, in either spelling, and it hides
WINDTRAP_COLOR:

  $ run WINDTRAP_COLOR=always windtrap mutants --color never > /dev/null
  [1]
  $ diff merged "$bin/out"
  $ run windtrap mutants --color=always > /dev/null
  [1]
  $ diff coloured "$bin/out"
  $ run WINDTRAP_COLOR=sometimes windtrap mutants --color=always > /dev/null
  [1]
  $ diff coloured "$bin/out"
  $ run windtrap mutants --color=sometimes
  --- stderr
  windtrap: invalid value 'sometimes' for --color: expected always, never or auto
  usage: windtrap mutants [OPTIONS] [PATH...]
  [2]
  $ run windtrap mutants --color
  --- stderr
  windtrap: option '--color' requires an argument
  usage: windtrap mutants [OPTIONS] [PATH...]
  [2]

The first argument that starts with a dash ends the parse: a PATH before
-h is not looked at, and an option before --help is refused:

  $ run windtrap mutants absent.mutants -h > /dev/null && diff help "$bin/out"
  $ run windtrap mutants -x --help
  --- stderr
  windtrap: unknown option '-x'
  usage: windtrap mutants [OPTIONS] [PATH...]
  [2]

Without --color, a value of WINDTRAP_COLOR the runner refuses is a usage
error, said before any PATH is looked at:

  $ run WINDTRAP_COLOR=sometimes windtrap mutants
  --- stderr
  windtrap: invalid value 'sometimes' for WINDTRAP_COLOR: expected always, never or auto
  [2]
  $ run WINDTRAP_COLOR=sometimes windtrap mutants absent.mutants
  --- stderr
  windtrap: invalid value 'sometimes' for WINDTRAP_COLOR: expected always, never or auto
  [2]
  $ cd / && rm -rf "$scratch"
