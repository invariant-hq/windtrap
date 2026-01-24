(** Tagging and filtering tests.

    Tags let you categorize tests and run subsets. Use --filter to select tests
    by name or tag. *)

open Windtrap

let () =
  run "Tags"
    [
      group "Unit tests"
        [
          test "fast check" (fun () -> equal Testable.int 42 42);
          test "another fast check" (fun () -> is_true true);
        ];
      group "Integration tests"
        ~tags:(Tag.labels [ "integration" ])
        [
          test "database connection" (fun () ->
              (* In real code, this would connect to a database. *)
              is_true true);
          test "api endpoint" (fun () ->
              (* In real code, this would call an API. *)
              is_true true);
        ];
      group "Slow tests" ~tags:(Tag.labels [ "slow" ])
        [
          slow "performance benchmark" (fun () ->
              let _ = List.init 10000 succ in
              is_true true);
        ];
      group "Platform-specific"
        [
          test ~tags:(Tag.labels [ "linux" ]) "linux only" (fun () ->
              if Sys.os_type <> "Unix" then skip ~reason:"Not on Linux" ()
              else is_true true);
          test ~tags:(Tag.labels [ "macos" ]) "macos only" (fun () ->
              if Sys.os_type <> "Unix" then skip ~reason:"Not on macOS" ()
              else is_true true);
        ];
      group "Feature flags"
        ~tags:(Tag.labels [ "feature-x" ])
        [
          test "feature x works" (fun () -> is_true true);
          test "feature x edge case" (fun () -> is_true true);
        ];
    ]
