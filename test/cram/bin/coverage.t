`windtrap coverage` merges a project's dumps and reports its coverage: the
table, the source view, the two documents and the two gates. Which files
it merges is data.t's. The projects lie in a scratch directory outside
the build directory, whose own dumps the command would otherwise find.

  $ bin=$PWD
  $ mkdata() { "$bin/mkdata.exe" "$@"; }
  $ run() {
  >   env -i PATH="$PATH" WINDTRAP_COLOR=never "$@" > "$bin/out" 2> "$bin/err"
  >   code=$?; cat "$bin/out"
  >   if [ -s "$bin/err" ]; then echo '--- stderr'; cat "$bin/err"; fi
  >   return $code
  > }
  $ errs() { run "$@" > /dev/null; code=$?; cat "$bin/err"; return $code; }
  $ scratch=$(cd "$(mktemp -d)" && pwd -P)

`project DIR` plants a project and enters it. Its two executables visited
lib/foo.ml's first and second lines, and the second also lib/bar.ml's
first; a point is a line here. The executables are given relative paths,
since on Windows they do not read the shell's absolute ones:

  $ project() {
  >   mkdir -p "$1/lib" && cd "$1"
  >   printf 'let a = 1\nlet b = 2\nlet c = 3\n' > lib/foo.ml
  >   printf 'let d = 4\nlet e = 5\n' > lib/bar.ml
  >   mkdata coverage _build/_coverage/windtrap-a.coverage lib/foo.ml=1,0,0
  >   mkdata coverage _build/_coverage/windtrap-b.coverage lib/foo.ml=0,1,0 lib/bar.ml=1,0
  > }
  $ project "$scratch/proj"

The report

The counts of the two executables add up. Each file has its row under the
header, with its uncovered lines, and the outcome is the last line:

  $ run windtrap coverage
     cover    points   file         uncovered lines (-u shows the source)
     50.0%    1/2      lib/bar.ml   2
     66.7%    2/3      lib/foo.ml   3
  coverage: 60.0% (3/5 points)

-u adds each file with uncovered lines under a heading that ends on its
numbers, the uncovered source marked, and the outcome follows the last
file:

  $ run windtrap coverage -u
     cover    points   file         uncovered lines
     50.0%    1/2      lib/bar.ml   2
     66.7%    2/3      lib/foo.ml   3
  
  lib/bar.ml: 50.0% (1/2)
  
        1 │ let d = 4
    ▌   2 │ let e = 5
  
  lib/foo.ml: 66.7% (2/3)
  
        2 │ let b = 2
    ▌   3 │ let c = 3
  
  coverage: 60.0% (3/5 points)
  $ cp "$bin/out" shown
  $ run windtrap coverage --show-uncovered > /dev/null && diff shown "$bin/out"

A file with no point is at 100%, a file whose source is missing says so,
and a file whose source is shorter than its points is stale. None of them
gets a heading under -u. Names sort by String.compare, so lib/Z.ml comes
before lib/a.ml:

  $ edges() {
  >   mkdir -p "$1/lib" && cd "$1"
  >   echo 'let x' > lib/s.ml
  >   : > lib/Z.ml
  >   mkdata coverage _build/_coverage/edges.coverage lib/s.ml=1,0 lib/a.ml=0 lib/Z.ml=
  > }
  $ edges "$scratch/edges"
  $ run windtrap coverage
     cover    points   file       uncovered lines (-u shows the source)
    100.0%    0/0      lib/Z.ml
      0.0%    0/1      lib/a.ml   (source not found)
     50.0%    1/2      lib/s.ml   stale: the source changed; re-run the instrumented tests
  coverage: 33.3% (1/3 points)
  $ run windtrap coverage -u
     cover    points   file       uncovered lines
    100.0%    0/0      lib/Z.ml
      0.0%    0/1      lib/a.ml   (source not found)
     50.0%    1/2      lib/s.ml   stale: the source changed; re-run the instrumented tests
  coverage: 33.3% (1/3 points)
  $ cd "$scratch/proj"

--min

--min adds its verdict to the outcome line, and exits 1 below the minimum.
The gate compares the percentages before they are rounded:

  $ run windtrap coverage --min 50
     cover    points   file         uncovered lines (-u shows the source)
     50.0%    1/2      lib/bar.ml   2
     66.7%    2/3      lib/foo.ml   3
  coverage: 60.0% (3/5 points), minimum 50%: ok
  $ run windtrap coverage --min 80
     cover    points   file         uncovered lines (-u shows the source)
     50.0%    1/2      lib/bar.ml   2
     66.7%    2/3      lib/foo.ml   3
  coverage: 60.0% (3/5 points), minimum 80%: FAILED
  [1]
  $ run windtrap coverage -u --min 80 > view
  [1]
  $ tail -n 2 view
  
  coverage: 60.0% (3/5 points), minimum 80%: FAILED
  $ for min in 0 60 100; do
  >   run windtrap coverage --min=$min > gated; echo "[$?] $(tail -n 1 gated)"
  > done
  [0] coverage: 60.0% (3/5 points), minimum 0%: ok
  [0] coverage: 60.0% (3/5 points), minimum 60%: ok
  [1] coverage: 60.0% (3/5 points), minimum 100%: FAILED
  $ mkdir "$scratch/full" && cd "$scratch/full"
  $ mkdata coverage _build/_coverage/full.coverage lib/foo.ml=1,1,1 lib/bar.ml=2,1
  $ run windtrap coverage --min 100 | tail -n 1
  coverage: 100.0% (5/5 points), minimum 100%: ok
  $ mkdir "$scratch/thirds" && cd "$scratch/thirds"
  $ mkdata coverage _build/_coverage/t.coverage lib/foo.ml=1,1,0
  $ run windtrap coverage --min 66.7
     cover    points   file         uncovered lines (-u shows the source)
     66.7%    2/3      lib/foo.ml   (source not found)
  coverage: 66.7% (2/3 points), minimum 66.7%: FAILED
  [1]
  $ cd "$scratch/proj"

A value that is not a percentage from 0 to 100 is a usage error:

  $ for value in eleventy '' nan inf -inf -1 100.5 120; do
  >   run windtrap coverage "--min=$value"; echo "[$?]"
  > done
  --- stderr
  windtrap: invalid value 'eleventy' for --min: expected a percentage (0-100)
  usage: windtrap coverage [OPTIONS] [PATH...]
  [2]
  --- stderr
  windtrap: invalid value '' for --min: expected a percentage (0-100)
  usage: windtrap coverage [OPTIONS] [PATH...]
  [2]
  --- stderr
  windtrap: invalid value 'nan' for --min: expected a percentage (0-100)
  usage: windtrap coverage [OPTIONS] [PATH...]
  [2]
  --- stderr
  windtrap: invalid value 'inf' for --min: expected a percentage (0-100)
  usage: windtrap coverage [OPTIONS] [PATH...]
  [2]
  --- stderr
  windtrap: invalid value '-inf' for --min: expected a percentage (0-100)
  usage: windtrap coverage [OPTIONS] [PATH...]
  [2]
  --- stderr
  windtrap: invalid value '-1' for --min: expected a percentage (0-100)
  usage: windtrap coverage [OPTIONS] [PATH...]
  [2]
  --- stderr
  windtrap: invalid value '100.5' for --min: expected a percentage (0-100)
  usage: windtrap coverage [OPTIONS] [PATH...]
  [2]
  --- stderr
  windtrap: invalid value '120' for --min: expected a percentage (0-100)
  usage: windtrap coverage [OPTIONS] [PATH...]
  [2]

The documents

--json writes the report as one document on standard output:

  $ run windtrap coverage --json
  { "summary": { "visited": 3, "total": 5, "percentage": 60.00 },
    "files": [
      { "path": "lib/bar.ml", "visited": 1, "total": 2,
        "percentage": 50.00,
        "uncovered_lines": [2] },
      { "path": "lib/foo.ml", "visited": 2, "total": 3,
        "percentage": 66.67,
        "uncovered_lines": [3] } ] }

A file with no point is at 100.00, and a merge of no point too. A file
without its source, or with a stale one, lists no line:

  $ cd "$scratch/edges" && run windtrap coverage --json && cd "$scratch/proj"
  { "summary": { "visited": 1, "total": 3, "percentage": 33.33 },
    "files": [
      { "path": "lib/Z.ml", "visited": 0, "total": 0,
        "percentage": 100.00,
        "uncovered_lines": [] },
      { "path": "lib/a.ml", "visited": 0, "total": 1,
        "percentage": 0.00,
        "uncovered_lines": [] },
      { "path": "lib/s.ml", "visited": 1, "total": 2,
        "percentage": 50.00,
        "uncovered_lines": [] } ] }
  $ mkdir "$scratch/no-point" && cd "$scratch/no-point"
  $ mkdata coverage _build/_coverage/none.coverage lib/Z.ml=
  $ run windtrap coverage --json | head -n 1
  { "summary": { "visited": 0, "total": 0, "percentage": 100.00 },
  $ cd "$scratch/proj"

A recorded name is escaped as JSON needs, and every other byte is written
as it is:

  $ mkdir "$scratch/escape" && cd "$scratch/escape"
  $ mkdata coverage _build/_coverage/names.coverage 'lib/q\"b\\s\tt\r\nx\001\127\195\169.ml=0'
  $ run windtrap coverage --json
  { "summary": { "visited": 0, "total": 1, "percentage": 0.00 },
    "files": [
      { "path": "lib/q\"b\\s\tt\r\nx\u0001é.ml", "visited": 0, "total": 1,
        "percentage": 0.00,
        "uncovered_lines": [] } ] }
  $ cd "$scratch/proj"

--lcov writes an LCOV tracefile: each file's lines with their visits, in
the merged counts:

  $ run windtrap coverage --lcov
  TN:
  SF:lib/bar.ml
  DA:1,1
  DA:2,0
  LF:2
  LH:1
  end_of_record
  TN:
  SF:lib/foo.ml
  DA:1,1
  DA:2,1
  DA:3,0
  LF:3
  LH:2
  end_of_record

A file whose source is missing or stale has no record, and is named on
standard error, in its place among the records:

  $ cd "$scratch/edges" && run windtrap coverage --lcov && cd "$scratch/proj"
  TN:
  SF:lib/Z.ml
  LF:0
  LH:0
  end_of_record
  --- stderr
  windtrap: lib/a.ml: source not found; omitted from the lcov output
  windtrap: lib/s.ml: the source changed since the run; omitted from the lcov output
  $ mkdir -p "$scratch/lcov-order/lib" && cd "$scratch/lcov-order"
  $ echo 'let a = 1' > lib/a.ml && echo 'let c = 1' > lib/c.ml
  $ mkdata coverage _build/_coverage/abc.coverage lib/a.ml=1 lib/b.ml=1 lib/c.ml=0
  $ env -i PATH="$PATH" windtrap coverage --lcov 2>&1
  TN:
  SF:lib/a.ml
  DA:1,1
  LF:1
  LH:1
  end_of_record
  windtrap: lib/b.ml: source not found; omitted from the lcov output
  TN:
  SF:lib/c.ml
  DA:1,0
  LF:1
  LH:0
  end_of_record
  $ cd "$scratch/proj"

Under a document the verdict of --min goes to standard error, and standard
output stays the document:

  $ run windtrap coverage --json > json
  $ run windtrap coverage --lcov > lcov
  $ errs windtrap coverage --json --min 80
  windtrap: coverage: 60.0% (3/5 points), minimum 80%: FAILED
  [1]
  $ diff json "$bin/out"
  $ errs windtrap coverage --json --min 50
  windtrap: coverage: 60.0% (3/5 points), minimum 50%: ok
  $ diff json "$bin/out"
  $ errs windtrap coverage --lcov --min 80
  windtrap: coverage: 60.0% (3/5 points), minimum 80%: FAILED
  [1]
  $ diff lcov "$bin/out"

-u, --color=always and a WINDTRAP_COLOR the report refuses change neither
a document nor the exit code:

  $ for doc in --json --lcov; do
  >   run windtrap coverage $doc > plain
  >   run windtrap coverage $doc -u > shown; echo "[$?]"; diff plain shown
  >   run windtrap coverage $doc --color=always > shown; echo "[$?]"; diff plain shown
  >   run WINDTRAP_COLOR=sometimes windtrap coverage $doc > shown; echo "[$?]"; diff plain shown
  > done
  [0]
  [0]
  [0]
  [0]
  [0]
  [0]

One document owns standard output, and --json twice is --json:

  $ run windtrap coverage --lcov --json
  --- stderr
  windtrap: --json and --lcov each own standard output; pick one
  usage: windtrap coverage [OPTIONS] [PATH...]
  [2]
  $ run windtrap coverage --json --json > twice && diff json twice

--expect

--expect names the sources the merge must hold. A source under the
directory that no executable recorded is named with the reasons it can be
missing, and exits 1. A preprocessed twin of a recorded file and a lexer
source are held by their module, and dot directories, _build and _opam
are not searched:

  $ mkdir -p lib/.hidden lib/_build lib/_opam
  $ cp lib/foo.ml lib/foo.pp.ml
  $ echo 'rule token = parse eof { () }' > lib/bar.mll
  $ echo 'let e = 5' > lib/.hidden/ghost.ml
  $ echo 'let x = 1' > lib/_build/built.ml
  $ echo 'let y = 2' > lib/_opam/switch.ml
  $ errs windtrap coverage --expect lib
  $ echo 'let d = 4' > lib/baz.ml
  $ run windtrap coverage --expect lib
     cover    points   file         uncovered lines (-u shows the source)
     50.0%    1/2      lib/bar.ml   2
     66.7%    2/3      lib/foo.ml   3
  coverage: 60.0% (3/5 points)
  --- stderr
  windtrap: lib/baz.ml: expected source has no coverage data (not instrumented, or linked into no test executable that ran)
  [1]

--do-not-expect exempts a path, and each flag takes its value in either
spelling. A covered file passes:

  $ errs windtrap coverage --expect lib --do-not-expect lib/baz.ml
  $ errs windtrap coverage --expect=lib --do-not-expect=lib/baz.ml
  $ errs windtrap coverage --expect lib/foo.ml

Sources are named once and in path order, whatever the order of the
flags. A recorded name matches a source by its stem, whatever its
preprocessing suffix, and a backslash separates as a slash does:

  $ mkdir -p "$scratch/stems/lib" && cd "$scratch/stems"
  $ for name in zeta alpha baz qux extra; do echo 'let x = 1' > lib/$name.ml; done
  $ mkdata coverage _build/_coverage/stems.coverage './lib//baz.pp.ml=1' 'lib\\qux.ml=1'
  $ errs windtrap coverage --expect lib/zeta.ml --expect lib --expect lib/extra.ml
  windtrap: lib/alpha.ml: expected source has no coverage data (not instrumented, or linked into no test executable that ran)
  windtrap: lib/extra.ml: expected source has no coverage data (not instrumented, or linked into no test executable that ran)
  windtrap: lib/zeta.ml: expected source has no coverage data (not instrumented, or linked into no test executable that ran)
  [1]

A path --expect names must exist, and the first one missing is named.
--do-not-expect's paths are looked at only under --expect:

  $ errs windtrap coverage --expect lib/nope
  windtrap: lib/nope: no such file or directory
  [1]
  $ errs windtrap coverage --do-not-expect nope
  $ errs windtrap coverage --expect lib --do-not-expect nope
  windtrap: nope: no such file or directory
  [1]
  $ errs windtrap coverage --do-not-expect gone3 --expect gone1 --expect gone2
  windtrap: gone1: no such file or directory
  [1]
  $ cd "$scratch/proj"

Under --json both gates run, and standard output stays the document:

  $ errs windtrap coverage --json --expect lib --min 80
  windtrap: lib/baz.ml: expected source has no coverage data (not instrumented, or linked into no test executable that ran)
  windtrap: coverage: 60.0% (3/5 points), minimum 80%: FAILED
  [1]
  $ diff json "$bin/out"

No data

Without dumps the command exits 1, with a hint that names the backend:

  $ mkdir "$scratch/empty" && cd "$scratch/empty"
  $ run windtrap coverage
  --- stderr
  windtrap: no .coverage files found
  Instrument the library under test with ppx_windtrap.coverage and run its tests first; every instrumented test executable writes its dump at exit, under the build directory's _coverage or under _windtrap/coverage.
  [1]
  $ cd "$scratch/proj"

The command line

--help documents each flag under its name, in 80 columns, and the one
variable that has no flag:

  $ run windtrap coverage --help
  windtrap coverage - merge .coverage files and report
  
  usage: windtrap coverage [OPTIONS] [PATH...]
  
  Merges the .coverage files written by instrumented test executables and
  reports expression coverage per source file. Without PATH arguments the files
  are found under the build directory's _coverage (or _windtrap/coverage in a
  tree built without one), walking up from the current directory to the
  enclosing project root; PATH arguments (.coverage files, or directories
  searched recursively) replace that default.
  
  OPTIONS:
    --min=PCT
        Exit 1 when total coverage is below PCT.
  
    --json
        Machine-readable report on standard output.
  
    --lcov
        LCOV tracefile on standard output (genhtml, Codecov, Coveralls, editor
        gutters).
  
    --expect=PATH
        Exit 1 unless every .ml/.mll/.mly under PATH (or PATH itself) has
        coverage data; repeatable.
  
    --do-not-expect=PATH
        Exempt PATH, a file or a directory, from --expect.
  
    -u, --show-uncovered
        Also render uncovered source excerpts.
  
    --color=MODE (env WINDTRAP_COLOR)
        Color output: always, never or auto.
  
    -h, --help
        Print this help and exit.
  
  ENVIRONMENT (no flag):
    NO_COLOR
        Any value: never style output (--color auto).
  $ cp "$bin/out" help
  $ awk '{ sub(/\r$/, "") } length > 80' help
  $ run windtrap coverage -h > /dev/null && diff help "$bin/out"
  $ run windtrap coverage -help > /dev/null && diff help "$bin/out"

An unknown option is a usage error, as is a flag without its value. A
flag that takes no value takes none after =, and --flag=value is split
before any flag is read:

  $ run windtrap coverage --frobnicate
  --- stderr
  windtrap: unknown option '--frobnicate'
  usage: windtrap coverage [OPTIONS] [PATH...]
  [2]
  $ for flag in --min --expect --do-not-expect --color; do run windtrap coverage -u $flag; echo "[$?]"; done
  --- stderr
  windtrap: option '--min' requires an argument
  usage: windtrap coverage [OPTIONS] [PATH...]
  [2]
  --- stderr
  windtrap: option '--expect' requires an argument
  usage: windtrap coverage [OPTIONS] [PATH...]
  [2]
  --- stderr
  windtrap: option '--do-not-expect' requires an argument
  usage: windtrap coverage [OPTIONS] [PATH...]
  [2]
  --- stderr
  windtrap: option '--color' requires an argument
  usage: windtrap coverage [OPTIONS] [PATH...]
  [2]
  $ run windtrap coverage --min --expect=lib
  --- stderr
  windtrap: invalid value '--expect' for --min: expected a percentage (0-100)
  usage: windtrap coverage [OPTIONS] [PATH...]
  [2]
  $ for arg in --json=1 --help=1 -; do run windtrap coverage $arg; echo "[$?]"; done
  --- stderr
  windtrap: unknown option '--json=1'
  usage: windtrap coverage [OPTIONS] [PATH...]
  [2]
  --- stderr
  windtrap: unknown option '--help=1'
  usage: windtrap coverage [OPTIONS] [PATH...]
  [2]
  --- stderr
  windtrap: unknown option '-'
  usage: windtrap coverage [OPTIONS] [PATH...]
  [2]

The first argument that ends the parse decides:

  $ run windtrap coverage --frobnicate --help
  --- stderr
  windtrap: unknown option '--frobnicate'
  usage: windtrap coverage [OPTIONS] [PATH...]
  [2]
  $ run windtrap coverage --help --frobnicate > /dev/null && diff help "$bin/out"
  $ run windtrap coverage --json --lcov --frobnicate
  --- stderr
  windtrap: --json and --lcov each own standard output; pick one
  usage: windtrap coverage [OPTIONS] [PATH...]
  [2]

--color is the runner's flag, in either spelling, and it hides
WINDTRAP_COLOR:

  $ run windtrap coverage > plain
  $ run WINDTRAP_COLOR=always windtrap coverage > coloured
  $ cmp -s plain coloured || echo styled
  styled
  $ run WINDTRAP_COLOR=always windtrap coverage --color never > /dev/null
  $ diff plain "$bin/out"
  $ run WINDTRAP_COLOR=sometimes windtrap coverage --color=always > /dev/null
  $ diff coloured "$bin/out"
  $ run windtrap coverage --color=sometimes
  --- stderr
  windtrap: invalid value 'sometimes' for --color: expected always, never or auto
  usage: windtrap coverage [OPTIONS] [PATH...]
  [2]

Without --color, a value of WINDTRAP_COLOR the runner refuses is a usage
error, said before any PATH is looked at:

  $ run WINDTRAP_COLOR=sometimes windtrap coverage
  --- stderr
  windtrap: invalid value 'sometimes' for WINDTRAP_COLOR: expected always, never or auto
  [2]
  $ run WINDTRAP_COLOR=sometimes windtrap coverage absent.coverage
  --- stderr
  windtrap: invalid value 'sometimes' for WINDTRAP_COLOR: expected always, never or auto
  [2]
  $ cd / && rm -rf "$scratch"
