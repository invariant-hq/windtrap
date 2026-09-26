ppx_windtrap's expansion as the compiler types it. The expansion applies
Expect_test_config.run at type (unit -> unit) -> unit, so a run of
another type is a type error at the test that names it. pp.exe writes
the expansion as an AST, and the compiler types it against the installed
libraries:

  $ lib=$INSIDE_DUNE/../install/default/lib
  $ ../pp.exe -apply ppx_windtrap --impl ./wrong_run.ml --dump-ast -o wrong_run.ast
  $ ocamlc -color never -stop-after typing -w -a \
  >   -I "$lib/windtrap" -I "$lib/ppx_windtrap/runtime" -impl wrong_run.ast
  File "./wrong_run.ml", lines 10-12, characters 0-19:
  10 | let%expect_test "a run of another type" =
  11 |   print_string "x";
  12 |   [%expect {| x |}]
  Error: The value Expect_test_config.run has type (unit -> int) -> unit
         but an expression was expected of type (unit -> unit) -> unit
         Type int is not compatible with type unit
  [2]
