(* One healthy expect test, registered at module load. Which executable
   links this module decides its fate: undriven_runner.exe drives the
   registration through the inline-test-runner protocol and the test
   runs; undriven_main.exe links it and drives nothing, the defect
   class the guard exists for.

   [link] is the anchor each main references: this module is a library
   unit, and a unit nothing references is dropped by the linker,
   initializers and all. The fixture is about linked test code, so the
   linking must be a fact the mains pin, not an accident. *)

let link = ()

let%expect_test "a registered test" =
  print_string "the test ran";
  [%expect {| the test ran |}]
