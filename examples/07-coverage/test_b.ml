open Windtrap
module A = Windtrap_example_coverage.Half_a
module B = Windtrap_example_coverage.Half_b

let greet =
  group "greet"
    [
      test "names the person" (fun () -> equal string "hello, b" (A.greet "b"));
    ]

let clamp =
  group "clamp"
    [
      test "raises a low value" (fun () -> equal int 0 (B.clamp 0 9 (-5)));
      test "lowers a high value" (fun () -> equal int 9 (B.clamp 0 9 50));
      test "keeps a value in range" (fun () -> equal int 4 (B.clamp 0 9 4));
    ]

let arithmetic =
  group "arithmetic"
    [
      test "sums a list" (fun () -> equal int 6 (B.sum [ 1; 2; 3 ]));
      test "signs a positive" (fun () -> equal int 1 (B.sign 3));
      test "signs a negative" (fun () -> equal int (-1) (B.sign (-3)));
      test "signs zero" (fun () -> equal int 0 (B.sign 0));
    ]

let () = exit (run "half_b" [ greet; clamp; arithmetic ])
