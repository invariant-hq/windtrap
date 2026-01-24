(** Your first Windtrap test.

    This example shows the minimal setup needed to write and run tests. *)

open Windtrap

(** A simple function we want to test. *)
let add a b = a + b

let () =
  run "First Test"
    [
      test "addition works" (fun () ->
          (* [equal] checks that two values are equal. [Testable.int] tells
             Windtrap how to compare and print integers. *)
          equal Testable.int 5 (add 2 3));
      test "adding zero" (fun () -> equal Testable.int 42 (add 42 0));
      test "negative numbers" (fun () -> equal Testable.int (-1) (add 2 (-3)));
    ]
