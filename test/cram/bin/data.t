Where the two commands find their files and which of them they merge
(bin/data_files.mli), through `windtrap coverage`. A path the command
builds is absolute and spelled by the platform, so a data file is shown
by its name. The projects lie in a scratch directory outside the build
directory, whose own dumps the command would otherwise find.

  $ bin=$PWD
  $ mkdata() { "$bin/mkdata.exe" "$@"; }
  $ run() {
  >   env -i PATH="$PATH" WINDTRAP_COLOR=never "$@" > "$bin/out" 2> "$bin/err"
  >   code=$?; cat "$bin/out"
  >   if [ -s "$bin/err" ]; then
  >     echo '--- stderr'
  >     sed -E 's#[^ ()]*[/\\]([^/\\ ()]+\.(coverage|cov))#.../\1#g' "$bin/err"
  >   fi
  >   return $code
  > }
  $ scratch=$(cd "$(mktemp -d)" && pwd -P)

`project DIR` plants a project and enters it. Its two executables visited
lib/foo.ml's first and second lines, and the second also lib/bar.ml's
first. The executables are given relative paths, since on Windows they do
not read the shell's absolute ones:

  $ project() {
  >   mkdir -p "$1/lib" && cd "$1"
  >   printf 'let a = 1\nlet b = 2\nlet c = 3\n' > lib/foo.ml
  >   printf 'let d = 4\nlet e = 5\n' > lib/bar.ml
  >   mkdata coverage _build/_coverage/windtrap-a.coverage lib/foo.ml=1,0,0
  >   mkdata coverage _build/_coverage/windtrap-b.coverage lib/foo.ml=0,1,0 lib/bar.ml=1,0
  > }
  $ project "$scratch/proj"

Discovery

From the project root and from any directory under it, the files are those
under _build/_coverage, and sources resolve against the root:

  $ run windtrap coverage
     cover    points   file         uncovered lines (-u shows the source)
     50.0%    1/2      lib/bar.ml   2
     66.7%    2/3      lib/foo.ml   3
  coverage: 60.0% (3/5 points)
  $ cd lib && run windtrap coverage && cd ..
     cover    points   file         uncovered lines (-u shows the source)
     50.0%    1/2      lib/bar.ml   2
     66.7%    2/3      lib/foo.ml   3
  coverage: 60.0% (3/5 points)

From a directory inside the build directory the root is the build
directory's parent, and a sandbox's leftovers under it are never read:

  $ mkdir -p _build/default/examples && cd _build/default/examples
  $ run windtrap coverage
     cover    points   file         uncovered lines (-u shows the source)
     50.0%    1/2      lib/bar.ml   2
     66.7%    2/3      lib/foo.ml   3
  coverage: 60.0% (3/5 points)
  $ cd "$scratch/proj"
  $ mkdir -p _build/.sandbox/_build/_coverage _build/.sandbox/0abc/default
  $ echo 'not a coverage file' > _build/.sandbox/_build/_coverage/junk.coverage
  $ cd _build/.sandbox/0abc/default && run windtrap coverage && cd "$scratch/proj"
     cover    points   file         uncovered lines (-u shows the source)
     50.0%    1/2      lib/bar.ml   2
     66.7%    2/3      lib/foo.ml   3
  coverage: 60.0% (3/5 points)

Outside a build directory, the walk up takes a directory named _build
only, and passes over an ancestor under a .sandbox directory:

  $ mkdir -p "$scratch/walk-up/src" && cd "$scratch/walk-up"
  $ mkdata coverage _build_ci/_coverage/ci.coverage lib/foo.ml=1,1,1
  $ cd src && run windtrap coverage
  --- stderr
  windtrap: no .coverage files found
  Instrument the library under test with ppx_windtrap.coverage and run its tests first; every instrumented test executable writes its dump at exit, under the build directory's _coverage or under _windtrap/coverage.
  [1]
  $ cd "$scratch/proj"
  $ mkdir -p .sandbox/0abc/_build/_coverage
  $ echo 'not a coverage file' > .sandbox/0abc/_build/_coverage/junk.coverage
  $ cd .sandbox/0abc && run windtrap coverage && cd "$scratch/proj"
     cover    points   file         uncovered lines (-u shows the source)
     50.0%    1/2      lib/bar.ml   2
     66.7%    2/3      lib/foo.ml   3
  coverage: 60.0% (3/5 points)

INSIDE_DUNE names the build directory whose files are merged, as dune
sets it for a rule's action and under `dune exec`, and a value that names
no build directory is ignored:

  $ mkdir -p "$scratch/build-dirs/lib" && cd "$scratch/build-dirs"
  $ printf 'let a = 1\nlet b = 2\nlet c = 3\n' > lib/foo.ml
  $ mkdata coverage _build/_coverage/shared.coverage lib/foo.ml=1,1,1
  $ mkdata coverage _build_ci/_coverage/private.coverage lib/foo.ml=1,0,0
  $ run INSIDE_DUNE=_build_ci/default windtrap coverage
     cover    points   file         uncovered lines (-u shows the source)
     33.3%    1/3      lib/foo.ml   2-3
  coverage: 33.3% (1/3 points)
  $ run windtrap coverage
     cover    points   file         uncovered lines (-u shows the source)
    100.0%    3/3      lib/foo.ml
  coverage: 100.0% (3/3 points)
  $ run INSIDE_DUNE=1 windtrap coverage
     cover    points   file         uncovered lines (-u shows the source)
    100.0%    3/3      lib/foo.ml
  coverage: 100.0% (3/3 points)

PATH arguments

A PATH replaces discovery, and sources then resolve against the current
directory. A directory contributes its files at any depth, a file itself:

  $ cd "$scratch"
  $ run windtrap coverage proj/_build/_coverage
     cover    points   file         uncovered lines (-u shows the source)
     50.0%    1/2      lib/bar.ml   (source not found)
     66.7%    2/3      lib/foo.ml   (source not found)
  coverage: 60.0% (3/5 points)
  $ run windtrap coverage proj/_build/_coverage/windtrap-a.coverage
     cover    points   file         uncovered lines (-u shows the source)
     33.3%    1/3      lib/foo.ml   (source not found)
  coverage: 33.3% (1/3 points)
  $ mkdir -p nested/one/two
  $ cp proj/_build/_coverage/windtrap-a.coverage nested/one/two/a.coverage
  $ cp proj/_build/_coverage/windtrap-b.coverage nested/one/b.coverage
  $ run windtrap coverage nested | tail -n 1
  coverage: 60.0% (3/5 points)

A PATH that does not exist, or a file without the extension, fails the
whole command, whatever else is named:

  $ run windtrap coverage no-such-dir/absent.coverage
  --- stderr
  windtrap: .../absent.coverage: no such file or directory
  [1]
  $ cp proj/_build/_coverage/windtrap-a.coverage renamed.cov
  $ run windtrap coverage renamed.cov
  --- stderr
  windtrap: renamed.cov: not a .coverage file
  [1]
  $ run windtrap coverage proj/_build/_coverage/windtrap-a.coverage no-such-dir/absent.coverage
  --- stderr
  windtrap: .../absent.coverage: no such file or directory
  [1]

A directory without files is no data, as is an empty data directory:

  $ mkdir explicit-empty
  $ run windtrap coverage explicit-empty
  --- stderr
  windtrap: no .coverage files found
  Instrument the library under test with ppx_windtrap.coverage and run its tests first; every instrumented test executable writes its dump at exit, under the build directory's _coverage or under _windtrap/coverage.
  [1]
  $ mkdir -p bare/_build/_coverage && cd bare
  $ run windtrap coverage
  --- stderr
  windtrap: no .coverage files found
  Instrument the library under test with ppx_windtrap.coverage and run its tests first; every instrumented test executable writes its dump at exit, under the build directory's _coverage or under _windtrap/coverage.
  [1]

Files of other builds

A file records the executable that wrote it. Here the executable on disk
wrote it, and it is merged:

  $ mkdir -p "$scratch/builds/lib" "$scratch/builds/_build/default/test" && cd "$scratch/builds"
  $ printf 'let a = 1\nlet b = 2\nlet c = 3\n' > lib/foo.ml
  $ echo 'the instrumented build' > _build/default/test/a.exe
  $ mkdata coverage _build/_coverage/a.coverage --exe _build/default/test/a.exe lib/foo.ml=1,1,1
  $ run windtrap coverage
     cover    points   file         uncovered lines (-u shows the source)
    100.0%    3/3      lib/foo.ml
  coverage: 100.0% (3/3 points)

A file whose executable no longer exists, and one whose executable was
rebuilt since, as dune's cache replays a test after the sources revert,
are named and left out, and the remedy is said once:

  $ echo 'an intermediate build' > _build/default/test/b.exe
  $ echo 'a deleted suite' > _build/default/test/gone.exe
  $ mkdata coverage _build/_coverage/b.coverage --exe _build/default/test/b.exe lib/ghost.ml=1
  $ mkdata coverage _build/_coverage/gone.coverage --exe _build/default/test/gone.exe lib/ghost.ml=0
  $ echo 'the reverted build' > _build/default/test/b.exe
  $ rm _build/default/test/gone.exe
  $ run windtrap coverage
     cover    points   file         uncovered lines (-u shows the source)
    100.0%    3/3      lib/foo.ml
  coverage: 100.0% (3/3 points)
  --- stderr
  windtrap: .../b.coverage: not written by the executable now at default/test/b.exe (rebuilt since); excluding it
  windtrap: .../gone.coverage: its executable (default/test/gone.exe) no longer exists; excluding it
  windtrap: re-run the suite instrumented (forcing the runs your build tool cached), then merge again; delete the files whose executable no longer exists

When every file is left out, the command says how many there were and what
they are. Three are named at most, and the rest are counted:

  $ echo 'an uninstrumented rebuild' > _build/default/test/a.exe
  $ run windtrap coverage
  --- stderr
  windtrap: .../a.coverage: not written by the executable now at default/test/a.exe (rebuilt since); excluding it
  windtrap: .../b.coverage: not written by the executable now at default/test/b.exe (rebuilt since); excluding it
  windtrap: .../gone.coverage: its executable (default/test/gone.exe) no longer exists; excluding it
  windtrap: found 3 .coverage files and every one is stale or orphaned (1 orphaned)
    They were written by executables that no longer exist or have been rebuilt since.
    The usual cause is a build without the instrumentation flag.
  windtrap: re-run the suite instrumented (forcing the runs your build tool cached), then merge again; delete the files whose executable no longer exists
  [1]
  $ for name in d e; do
  >   echo 'the instrumented build' > _build/default/test/$name.exe
  >   mkdata coverage _build/_coverage/$name.coverage --exe _build/default/test/$name.exe lib/ghost.ml=1
  >   echo 'an uninstrumented rebuild' > _build/default/test/$name.exe
  > done
  $ run windtrap coverage
  --- stderr
  windtrap: .../a.coverage: not written by the executable now at default/test/a.exe (rebuilt since); excluding it
  windtrap: .../b.coverage: not written by the executable now at default/test/b.exe (rebuilt since); excluding it
  windtrap: .../d.coverage: not written by the executable now at default/test/d.exe (rebuilt since); excluding it
  windtrap: ... and 2 more like that
  windtrap: found 5 .coverage files and every one is stale or orphaned (1 orphaned)
    They were written by executables that no longer exist or have been rebuilt since.
    The usual cause is a build without the instrumentation flag.
  windtrap: re-run the suite instrumented (forcing the runs your build tool cached), then merge again; delete the files whose executable no longer exists
  [1]
  $ cd _build/_coverage && rm b.coverage d.coverage e.coverage gone.coverage && cd ../..
  $ run windtrap coverage
  --- stderr
  windtrap: .../a.coverage: not written by the executable now at default/test/a.exe (rebuilt since); excluding it
  windtrap: found 1 .coverage file and every one is stale
    They were written by executables that no longer exist or have been rebuilt since.
    The usual cause is a build without the instrumentation flag.
  windtrap: re-run the suite instrumented (forcing the runs your build tool cached), then merge again; delete the files whose executable no longer exists
  [1]

An executable outside every build directory is recorded by its absolute
path:

  $ rm _build/_coverage/a.coverage
  $ echo 'a hand-built suite' > no-such-exe
  $ mkdata coverage _build/_coverage/abs.coverage --exe no-such-exe lib/ghost.ml=1
  $ rm no-such-exe
  $ run windtrap coverage > abs
  [1]
  $ sed -E 's#\(.*[/\\]no-such-exe\)#(.../no-such-exe)#' abs
  --- stderr
  windtrap: .../abs.coverage: its executable (.../no-such-exe) no longer exists; excluding it
  windtrap: found 1 .coverage file and every one is orphaned
    They were written by executables that no longer exist or have been rebuilt since.
    The usual cause is a build without the instrumentation flag.
  windtrap: re-run the suite instrumented (forcing the runs your build tool cached), then merge again; delete the files whose executable no longer exists

A file is judged from its header before its records are read. Beside a
dump of the executable on disk, a leftover whose records are corrupt is
left out when another build wrote it, and ends the command when that
executable did:

  $ project "$scratch/judged"
  $ mkdir -p _build/default/test && echo 'the instrumented build' > _build/default/test/a.exe
  $ echo 'an earlier build' > _build/default/test/old.exe
  $ mkdata coverage _build/_coverage/a.coverage --exe _build/default/test/a.exe lib/foo.ml=1,0,0
  $ mkdata coverage earlier.coverage --exe _build/default/test/old.exe lib/foo.ml=1
  $ { head -n 2 earlier.coverage; echo 2; } > _build/_coverage/leftover.coverage
  $ rm _build/default/test/old.exe
  $ run windtrap coverage
     cover    points   file         uncovered lines (-u shows the source)
     50.0%    1/2      lib/bar.ml   2
     66.7%    2/3      lib/foo.ml   3
  coverage: 60.0% (3/5 points)
  --- stderr
  windtrap: .../leftover.coverage: its executable (default/test/old.exe) no longer exists; excluding it
  windtrap: re-run the suite instrumented (forcing the runs your build tool cached), then merge again; delete the files whose executable no longer exists
  $ echo 'a later build' > _build/default/test/old.exe
  $ run windtrap coverage
     cover    points   file         uncovered lines (-u shows the source)
     50.0%    1/2      lib/bar.ml   2
     66.7%    2/3      lib/foo.ml   3
  coverage: 60.0% (3/5 points)
  --- stderr
  windtrap: .../leftover.coverage: not written by the executable now at default/test/old.exe (rebuilt since); excluding it
  windtrap: re-run the suite instrumented (forcing the runs your build tool cached), then merge again; delete the files whose executable no longer exists
  $ { head -n 2 _build/_coverage/a.coverage; echo 2; } > _build/_coverage/leftover.coverage
  $ run windtrap coverage
  --- stderr
  windtrap: .../leftover.coverage: corrupt coverage file: expected file name length at offset 82
  [1]

Nothing judges a file that records no executable, one whose executable is
recorded below a build directory while the file lies in none, or one whose
executable cannot be read (unreadable.t). They are merged:

  $ cd "$scratch/builds"
  $ echo 'the suite' > _build/default/test/loose.exe
  $ mkdata coverage ../loose.coverage --exe _build/default/test/loose.exe lib/foo.ml=1,1,1
  $ rm _build/default/test/loose.exe
  $ run windtrap coverage ../loose.coverage
     cover    points   file         uncovered lines (-u shows the source)
    100.0%    3/3      lib/foo.ml
  coverage: 100.0% (3/3 points)

A merge that fails after files were left out says the exclusions and the
remedy first, and a file that cannot be loaded is said alone:

  $ project "$scratch/failed"
  $ mkdir -p _build/default/test && echo 'a deleted suite' > _build/default/test/gone.exe
  $ mkdata coverage _build/_coverage/c.coverage --exe _build/default/test/gone.exe lib/ghost.ml=1
  $ rm _build/default/test/gone.exe
  $ mkdata coverage _build/_coverage/d.coverage lib/foo.ml=1,0
  $ run windtrap coverage
  --- stderr
  windtrap: .../c.coverage: its executable (default/test/gone.exe) no longer exists; excluding it
  windtrap: re-run the suite instrumented (forcing the runs your build tool cached), then merge again; delete the files whose executable no longer exists
  windtrap: lib/foo.ml: coverage point tables disagree across coverage files (executables built from different sources?); re-run every instrumented test executable from one build, then merge again; delete the coverage files only if leftovers remain
  [1]
  $ echo 'not a coverage file' > _build/_coverage/d.coverage
  $ run windtrap coverage
  --- stderr
  windtrap: .../d.coverage: not a windtrap coverage file (expected header "windtrap-coverage-v3", found "not a coverage file"); files written by other windtrap versions are not readable - delete the stale coverage files, then re-run the instrumented tests
  [1]

Files that cannot be merged

A file that is not a dump, one of another version and a truncated one are
refused by name, and nothing is merged:

  $ mkdir -p "$scratch/refused/_build/_coverage" && cd "$scratch/refused"
  $ echo 'not a coverage file' > _build/_coverage/bad.coverage
  $ run windtrap coverage
  --- stderr
  windtrap: .../bad.coverage: not a windtrap coverage file (expected header "windtrap-coverage-v3", found "not a coverage file"); files written by other windtrap versions are not readable - delete the stale coverage files, then re-run the instrumented tests
  [1]
  $ rm _build/_coverage/bad.coverage
  $ printf 'WINDTRAP-COVERAGE-1\nsome v1 payload\n' > _build/_coverage/old.coverage
  $ run windtrap coverage
  --- stderr
  windtrap: .../old.coverage: not a windtrap coverage file (expected header "windtrap-coverage-v3", found "WINDTRAP-COVERAGE-1"); files written by other windtrap versions are not readable - delete the stale coverage files, then re-run the instrumented tests
  [1]
  $ rm _build/_coverage/old.coverage
  $ mkdata coverage whole.coverage lib/foo.ml=1,0,0
  $ head -c $(( $(wc -c < whole.coverage) - 4 )) whole.coverage > _build/_coverage/cut.coverage
  $ run windtrap coverage
  --- stderr
  windtrap: .../cut.coverage: corrupt coverage file: inverted extent 20-2 in lib/foo.ml
  [1]

Two executables built from different sources disagree about a file's
points:

  $ rm _build/_coverage/cut.coverage
  $ mkdata coverage _build/_coverage/one.coverage lib/foo.ml=1,0,0
  $ mkdata coverage _build/_coverage/two.coverage lib/foo.ml=1,0
  $ run windtrap coverage
  --- stderr
  windtrap: lib/foo.ml: coverage point tables disagree across coverage files (executables built from different sources?); re-run every instrumented test executable from one build, then merge again; delete the coverage files only if leftovers remain
  [1]

Dumps an executable writes

A test executable over an instrumented library, run from a build
directory, writes its dump under the build directory's _coverage, which
the command merges without a warning. Under --instrument-with the dump
also holds the instrumented core, so the session shows calc.ml's row:

  $ mkdir -p "$scratch/written/_build/default/test" "$scratch/written/test/cram/bin"
  $ cd "$scratch/written"
  $ cp "$bin/calc.ml" test/cram/bin/
  $ cp "$bin/pins_add.exe" _build/default/test/
  $ run WINDTRAP_SLOW_THRESHOLD=0 _build/default/test/pins_add.exe > /dev/null
  $ run windtrap coverage > /dev/null; cat "$bin/err"; grep calc.ml "$bin/out" | tr -s ' '
   75.0% 3/4 test/cram/bin/calc.ml 15

Outside every build directory the dump goes under the working directory's
_windtrap/coverage, and the command finds it there and from below:

  $ mkdir -p "$scratch/standalone/bin" "$scratch/standalone/test/cram/bin" "$scratch/standalone/lib/deep"
  $ cd "$scratch/standalone"
  $ cp "$bin/calc.ml" test/cram/bin/
  $ cp "$bin/pins_add.exe" bin/
  $ run WINDTRAP_SLOW_THRESHOLD=0 bin/pins_add.exe > /dev/null
  $ ls -d _*
  _windtrap
  $ run windtrap coverage > /dev/null; cat "$bin/err"; grep calc.ml "$bin/out" | tr -s ' '
   75.0% 3/4 test/cram/bin/calc.ml 15
  $ cd lib/deep
  $ run windtrap coverage > /dev/null; grep calc.ml "$bin/out" | tr -s ' '
   75.0% 3/4 test/cram/bin/calc.ml 15
  $ cd / && rm -rf "$scratch"
