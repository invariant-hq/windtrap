The mutation flags on an executable with nothing to mutate — the
commonest sibling of all, since the aggregate's remedy hands one
identifier to every suite in a tree at once, and `--mutate` asked of a
build that was never instrumented is the commonest misconfiguration.
The loop's own scenarios are test/instr/loop's; what these sessions
pin is the facade: the flags parse, the run still happens, and the
answer is a sentence on stderr rather than a green run with no report.

  $ run() {
  >   env -i PATH="$PATH" WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 \
  >       WINDTRAP_PROJECT_ROOT="$PWD" "$@"
  > }

A mutation run needs a green dry run before it looks at the catalogue
at all, so the runs below that are meant to get that far select the two
tests that pass with nothing planted beside them: -f math.

--mutate on an executable that catalogues nothing under the prefix is
refused by name after the suite ran: exit 1, the ordinary transcript on
stdout, one `windtrap:` line on stderr. The sentence is the same
whether the core this fixture links is instrumented (a catalogue the
prefix leaves empty) or plain (no catalogue at all): the prefix is what
the reader typed, and the missing-backend diagnosis is reserved for the
bare flag, so a build that is instrumented and fine is never blamed.

  $ run ./suite_main.exe --mutate=::no-such-source:: -f math > out 2> err
  [1]
  $ sed -E 's/ in [0-9.]+m?s\./ in DURATION./' out
  fixture: 2 passed in DURATION.
  $ cat err
  windtrap: --mutate=::no-such-source:: leaves no mutant in this executable's catalogue: no instrumented file matches the prefix (is the library under test instrumented with ppx_windtrap.mutate?), or the matched files have no mutation sites

--arm with an identifier naming a file this executable catalogues
nothing of is not a refusal: the run proceeds exactly as it would
unarmed, exits 0, and says once on stderr whose mutant it is not —
exiting 1 here would fail a whole tree's build for the one executable
that armed the mutant correctly.

  $ run ./suite_main.exe --arm lib/absent.ml:1:0:add -f math > out 2> err
  $ sed -E 's/ in [0-9.]+m?s\./ in DURATION./' out
  fixture: 2 passed in DURATION.
  $ cat err
  windtrap: lib/absent.ml:1:0:add: not this executable's mutant; it catalogues no site in lib/absent.ml (if you expected one, is the library under test instrumented with ppx_windtrap.mutate?)

Nothing is armed in such a run, so a failure in it is an ordinary one:
its block says nothing about a mutant.

  $ run ./suite_main.exe --arm lib/absent.ml:1:0:add -f boom > out 2> err
  [1]
  $ grep -E 'FAIL|mutant|arm' out
    FAIL  boom

A malformed identifier is refused before anything runs: the runtime's
grammar owns the spelling, and a value it cannot parse is never a
silently unarmed run.

  $ run ./suite_main.exe --arm not-an-identifier > out 2> err
  [1]
  $ cat out
  $ cat err
  windtrap: "not-an-identifier" is not a mutant identifier: no ':' separator; expected <file>:<line>:<col>:<rewrite>

Both flags at once is a usage error, before any run: the loop arms each
mutant itself, so an armed parent would mutate its own dry run.

  $ run ./suite_main.exe --mutate --arm lib/absent.ml:1:0:add > out 2> err
  [2]
  $ cat out
  $ cat err
  windtrap: options '--mutate' and '--arm' cannot be combined
  usage: suite_main.exe [OPTIONS] [PATTERN]

The mirrors reach the same code: WINDTRAP_MUTATE=1 with --arm is the
same refusal, and a falsy WINDTRAP_MUTATE is no mutation run at all.

  $ run WINDTRAP_MUTATE=1 ./suite_main.exe --arm lib/absent.ml:1:0:add > out 2> err
  [2]
  $ head -1 err
  windtrap: options '--mutate' and '--arm' cannot be combined
  $ run WINDTRAP_MUTATE=off ./suite_main.exe -f math > out 2> err
  $ sed -E 's/ in [0-9.]+m?s\./ in DURATION./' out
  fixture: 2 passed in DURATION.
  $ cat err

--mutant is the verb a reader types; the near miss is suggested.

  $ run ./suite_main.exe --mutant > out 2> err
  [2]
  $ head -1 err
  windtrap: unknown option '--mutant'; did you mean '--mutate'?
