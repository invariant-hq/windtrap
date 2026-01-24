(** Organizing tests with groups.

    Groups let you structure tests hierarchically. The output reflects this
    structure, making it easy to see which tests belong together. *)

open Windtrap

let () =
  run "Groups"
    [
      group "Math"
        [
          group "Addition"
            [
              test "positive numbers" (fun () -> equal Testable.int 5 (2 + 3));
              test "negative numbers" (fun () ->
                  equal Testable.int (-1) (-3 + 2));
              test "with zero" (fun () -> equal Testable.int 42 (42 + 0));
            ];
          group "Multiplication"
            [
              test "positive numbers" (fun () -> equal Testable.int 12 (3 * 4));
              test "by zero" (fun () -> equal Testable.int 0 (999 * 0));
              test "by one" (fun () -> equal Testable.int 7 (7 * 1));
            ];
        ];
      group "Comparisons"
        [
          test "less than" (fun () -> is_true (1 < 2));
          test "greater than" (fun () -> is_true (5 > 3));
          test "equality" (fun () -> is_true (42 = 42));
        ];
      (* Standalone tests can appear alongside groups. *)
      test "standalone test" (fun () -> is_true true);
    ]
