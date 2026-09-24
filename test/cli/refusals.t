The two things a run refuses before executing a test, and the one it
only warns about. Both refusals are properties of the suite rather than
of a flag, so each has its own declaration in the fixture.

  $ run() {
  >   env -i PATH="$PATH" WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 \
  >       WINDTRAP_PROJECT_ROOT="$PWD" "$@"
  > }

Two tests at one path is a startup refusal: exit 1, the path named, the
rule stated, and nothing on stdout because nothing ran.

  $ run FACADE_FIXTURE=duplicate ./suite_main.exe > out 2> err
  [1]
  $ cat out
  $ cat err
  windtrap: duplicate test paths:
    dup › twice
  Every full test path must be unique.

Focus outside CI narrows, it never fails: the run exits 0 and the
warning goes to stderr, where it cannot be mistaken for part of the
transcript. The warning names no site: it is what the committer is
told.

  $ run FACADE_FIXTURE=focus ./suite_main.exe > out 2> err
  $ sed -E 's/ in [0-9.]+m?s\./ in DURATION./' out
  fixture: 1 passed in DURATION.
  $ cat err
  windtrap: warning: focus is active: 1 of 2 tests ran; remove the focus before committing

Under CI the same suite refuses to start, and says which site to
remove:

  $ run CI=1 FACADE_FIXTURE=focus ./suite_main.exe > out 2> err
  [1]
  $ cat out
  $ sed -E 's/suite_main\.ml:[0-9]+/suite_main.ml:LINE/' err
  windtrap: focused tests committed (focus at test/cli/suite_main.ml:LINE); remove focus to run under CI
