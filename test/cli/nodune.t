Coverage and mutation without dune: the toolchain's compiler, the
installed windtrap, and nothing else — no dune and no ocamlfind in this
session. The two driver executables are what a findlib user builds
once (a Ppxlib standalone linked against the backend); everything
after that is ocamlopt.

The installed windtrap is found the way findlib finds it, on OCAMLPATH
or beside the compiler's own library. The scratch project lives in a
directory that is under no build directory, because that is the case
the session exists to show, and every run gets a clean environment:
a developer's INSIDE_DUNE or WINDTRAP_* setting must not reshape it.

  $ lib=$(for d in $(echo "$OCAMLPATH" | tr ':' ' ') $(dirname "$(ocamlopt -where)"); do
  >   if [ -f "$d/windtrap/META" ]; then echo "$d"; break; fi; done)
  $ test -f "$lib/windtrap/windtrap.cmxa" && test -f "$lib/windtrap/runtime/windtrap_runtime.cmxa"
  $ here=$PWD
  $ proj=$(mktemp -d "${TMPDIR:-/tmp}/nodune.XXXXXX")
  $ cd "$proj"
  $ run() {
  >   env -i PATH="$PATH" HOME="$HOME" TMPDIR="${TMPDIR:-/tmp}" \
  >       WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 "$@" > out 2>&1
  >   code=$?
  >   sed -E "s|$proj|PROJ|g; s/ in [0-9.]+m?s/ in TIME/; s/\(seed s1:[0-9a-f]+\)/(seed SEED)/" out
  >   return $code
  > }

A library with an untested arm and an untested boundary, and the
suite that leaves them:

  $ cat > calc.ml <<'ML'
  > let add a b = a + b
  > let sign n = if n > 0 then 1 else 0
  > let describe n = if n > 0 then "positive" else "non-positive"
  > ML
  $ cat > test_calc.ml <<'ML'
  > open Windtrap
  > 
  > let () =
  >   exit
  >   @@ run "calc"
  >        [
  >          test "add" (fun () -> equal int 5 (Calc.add 2 3));
  >          test "sign of a positive" (fun () -> equal int 1 (Calc.sign 5));
  >          test "sign of a negative" (fun () -> equal int 0 (Calc.sign (-3)));
  >          test "describe" (fun () -> equal string "positive" (Calc.describe 7));
  >        ]
  > ML

Coverage. The library is instrumented through -ppx; the test file is
not, and links windtrap as any test does:

  $ ocamlopt -ppx "$here/coverage_ppx.exe --as-ppx" -I "$lib/windtrap/runtime" -c calc.ml
  $ ocamlopt -I +unix -I "$lib/windtrap/runtime" -I "$lib/windtrap" \
  >   unix.cmxa windtrap_runtime.cmxa windtrap.cmxa calc.cmx test_calc.ml -o test_calc.exe

The run is one green line — a run prints no coverage number of its own
— and the dump lands under the working directory's _windtrap, never a
_build, in one directory per executable:

  $ run ./test_calc.exe
  calc: 4 passed in TIME.
  $ find _windtrap -type f | sed -E 's|windtrap-[0-9a-f]+|windtrap-HASH|; s|/[0-9a-f]+-[0-9a-f]+\.coverage|/DIGEST-TOKEN.coverage|'
  _windtrap/coverage/windtrap-HASH/DIGEST-TOKEN.coverage
  $ test ! -e _build

The installed binary finds it from the working directory and merges;
the untested arm is the uncovered line. Only the library's row is
pinned: when windtrap's own tree is built under the coverage backend,
the installed core is instrumented too and adds its rows to this
report, and the --min gate (test/instr/coverage_cmd pins it) would
then measure the core rather than calc.ml; the columns are squeezed
because the table aligns to its widest row:

  $ run windtrap coverage | grep calc.ml | tr -s ' '
   85.7% 6/7 calc.ml uncovered: 3

Mutation. The same library through the other backend, then the survey
scoped to the file, with --mutate:

  $ ocamlopt -ppx "$here/mutate_ppx.exe --as-ppx" -I "$lib/windtrap/runtime" -c calc.ml
  $ ocamlopt -I +unix -I "$lib/windtrap/runtime" -I "$lib/windtrap" \
  >   unix.cmxa windtrap_runtime.cmxa windtrap.cmxa calc.cmx test_calc.ml -o test_calc.exe
  $ run ./test_calc.exe --mutate=calc.ml
  calc: 4 passed in TIME.
  
  ─────────────────── survivors (2) ────────────────────
  
    SURVIVED  calc.ml:2:16:ge   n > 0  →  n >= 0
        2 │ let sign n = if n > 0 then 1 else 0
  
      2 tests ran this line and none failed:
        sign of a negative      test_calc.ml:9
        sign of a positive      test_calc.ml:8
  
    SURVIVED  calc.ml:3:20:ge   n > 0  →  n >= 0
        3 │ let describe n = if n > 0 then "positive" else "non-positive"
  
      1 test ran this line and did not fail:
        describe                test_calc.ml:10
  
  ──────────────────────────────────────────────────────
  
  mutants: 2 survived of 3 reached by this suite · 1 killed
  reproduce: ./test_calc.exe --arm <id>

The footer's flag, with a survivor's identifier from its head row,
arms that one mutant in an otherwise ordinary run:

  $ id=$(grep SURVIVED out | head -1 | awk '{print $2}')
  $ run ./test_calc.exe --arm "$id"
  mutant calc.ml:2:16:ge armed: n > 0 → n >= 0
  calc: 4 passed in TIME.
  mutant survived: the armed site was evaluated 2 time(s) and no test failed.

The verdict file is the run's, beside the coverage dumps, and the
installed binary merges it — exiting 1, because a survivor of every
suite that reached it is the one mutation exit code a build gates on:

  $ find _windtrap/mutants -type f | sed -E 's|windtrap-[0-9a-f]+|windtrap-HASH|'
  _windtrap/mutants/windtrap-HASH.mutants
  $ run windtrap mutants
  
  ─────────────────── survivors (2) ────────────────────
  
    SURVIVED  calc.ml:2:16:ge   n > 0  →  n >= 0
        2 │ let sign n = if n > 0 then 1 else 0
  
      2 tests ran this line and none failed:
        test_calc.exe   sign of a negative
        test_calc.exe   sign of a positive
  
    SURVIVED  calc.ml:3:20:ge   n > 0  →  n >= 0
        3 │ let describe n = if n > 0 then "positive" else "non-positive"
  
      1 test ran this line and did not fail:
        test_calc.exe   describe
  
  ──────────────────────────────────────────────────────
  
  mutants: 2 survived of 3 reached · 1 killed · 1 executable
  reproduce: WINDTRAP_MUTATE_ARM=<id> <re-run the instrumented suite>
  [1]

The scratch project is the session's own and leaves with it:

  $ cd "$here" && rm -rf "$proj"
