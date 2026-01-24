(** The assertion toolkit.

    Windtrap provides assertions for common patterns. All assertions include the
    source location in failure messages for easy debugging. *)

open Windtrap

let () =
  run "Assertions"
    [
      group "Equality"
        [
          test "equal" (fun () -> equal Testable.int 42 42);
          test "not_equal" (fun () -> not_equal Testable.int 1 2);
          test "string equality" (fun () ->
              equal Testable.string "hello" "hello");
        ];
      group "Booleans"
        [
          test "is_true" (fun () -> is_true (1 < 2));
          test "is_false" (fun () -> is_false (1 > 2));
        ];
      group "Options"
        [
          test "is_some" (fun () -> is_some (Some 42));
          test "is_none" (fun () -> is_none None);
          test "some with value check" (fun () ->
              some Testable.int 42 (Some 42));
        ];
      group "Results"
        [
          test "is_ok" (fun () -> is_ok (Ok 42));
          test "is_error" (fun () -> is_error (Error "oops"));
          test "ok with value check" (fun () ->
              ok Testable.string "success" (Ok "success"));
          test "error with value check" (fun () ->
              error Testable.string "failed" (Error "failed"));
        ];
      group "Custom messages"
        [
          test "with message" (fun () ->
              equal ~msg:"user count should match" Testable.int 5 5);
        ];
    ]
