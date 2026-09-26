Two executables over one library, each running its own mutation loop,
disagree about the library, and `windtrap mutants` merges their verdict
files into the project's answer. pins_add.exe pins calc.ml's add and only
reaches sub; pins_sub.exe does the reverse; both reach shared and pin
nothing about it; neither calls never.

The two executables run from a scratch build directory, where they write
their verdict files, and the source they recorded is planted where it was
recorded. --mutate scopes each loop to calc.ml's mutants, since under
--instrument-with the executables also link an instrumented core.

  $ bin=$PWD
  $ run() {
  >   env -i PATH="$PATH" WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 "$@" \
  >     > "$bin/out" 2> "$bin/err"
  >   code=$?; cat "$bin/out"
  >   if [ -s "$bin/err" ]; then echo '--- stderr'; cat "$bin/err"; fi
  >   return $code
  > }
  $ root=$(cd "$(mktemp -d)" && pwd -P) && cd "$root"
  $ mkdir -p _build/default/test test/cram/bin
  $ cp "$bin/calc.ml" test/cram/bin/
  $ cp "$bin/pins_add.exe" "$bin/pins_sub.exe" _build/default/test/

Each executable alone calls the mutant its sibling kills a survivor:

  $ run _build/default/test/pins_add.exe --mutate=test/cram/bin/calc.ml > add
  $ grep -e SURVIVED -e '^--- stderr' add
    SURVIVED  test/cram/bin/calc.ml:13:14:add  a - b → a + b
    SURVIVED  test/cram/bin/calc.ml:14:17:sub  a + b → a - b
  $ run _build/default/test/pins_sub.exe --mutate=test/cram/bin/calc.ml > sub
  $ grep -e SURVIVED -e '^--- stderr' sub
    SURVIVED  test/cram/bin/calc.ml:12:14:sub  a + b → a - b
    SURVIVED  test/cram/bin/calc.ml:14:17:sub  a + b → a - b

The merge keeps the one mutant that survived everywhere, with the reaching
tests of both executables, each named by its executable, and the mutant
neither reached:

  $ run windtrap mutants
  ───────────────────── survivors (1) ──────────────────────
    SURVIVED  test/cram/bin/calc.ml:14:17:sub  a + b → a - b
        14 │ let shared a b = a + b
  
      2 tests in 2 executables ran this line and none failed:
        pins_add.exe  shared › shared is nonzero
        pins_sub.exe  shared › shared is not 99
  ──────────────────────────────────────────────────────────
  
  ─────────────────── never reached (1) ────────────────────
    1  test/cram/bin/calc.ml   lines 15
  ──────────────────────────────────────────────────────────
  
  reproduce: dune exec --instrument-with ppx_windtrap.mutate test/pins_add.exe -- --arm test/cram/bin/calc.ml:14:17:sub
  mutants: 1 survived of 3 reached, 2 killed, 1 never reached, 2 executables
  [1]

The command's identifier, pasted into that executable, arms the survivor:

  $ id=$(sed -n 's/^reproduce: .* --arm //p' "$bin/out")
  $ run _build/default/test/pins_add.exe --arm "$id" > armed
  $ head -n 1 armed
  mutant test/cram/bin/calc.ml:14:17:sub armed: a + b → a - b
  $ cd / && rm -rf "$root"
