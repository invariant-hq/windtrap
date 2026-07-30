open Windtrap
module A = Windtrap_example_aggregation.Half_a

let () =
  run "half_a"
    [
      test "greets by name" (fun () ->
          equal string "hello, ada" (A.greet "ada"));
      test "greets strangers" (fun () ->
          equal string "hello, stranger" (A.greet ""));
      test "shouts" (fun () -> equal string "ADA!" (A.shout "ada"));
      test "shouts silence" (fun () -> equal string "!" (A.shout ""));
      test "parses booleans" (fun () ->
          equal (result bool string) (Ok true) (A.parse_bool "true");
          equal (result bool string) (Ok false) (A.parse_bool "false"));
      test "rejects non-booleans" (fun () ->
          equal (result bool string) (Error "not a bool: maybe")
            (A.parse_bool "maybe"));
    ]
