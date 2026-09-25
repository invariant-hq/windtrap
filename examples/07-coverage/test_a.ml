open Windtrap
module A = Windtrap_example_coverage.Half_a

let greet =
  group "greet"
    [
      test "names the person" (fun () ->
          equal string "hello, ada" (A.greet "ada"));
      test "greets a stranger" (fun () ->
          equal string "hello, stranger" (A.greet ""));
    ]

let shout =
  group "shout"
    [
      test "raises the case" (fun () -> equal string "ADA!" (A.shout "ada"));
      test "shouts silence" (fun () -> equal string "!" (A.shout ""));
    ]

let parse_bool =
  group "parse_bool"
    [
      test "reads both booleans" (fun () ->
          equal (result bool string) (Ok true) (A.parse_bool "true");
          equal (result bool string) (Ok false) (A.parse_bool "false"));
      test "rejects another word" (fun () ->
          equal (result bool string) (Error "not a bool: maybe")
            (A.parse_bool "maybe"));
    ]

let () = exit (run "half_a" [ greet; shout; parse_bool ])
