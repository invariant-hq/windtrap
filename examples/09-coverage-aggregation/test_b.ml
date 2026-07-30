open Windtrap
module A = Windtrap_example_aggregation.Half_a
module B = Windtrap_example_aggregation.Half_b

let () =
  run "half_b"
    [
      (* The one cross-half call: it links all of Half_a into this
         executable, so this stanza's inline percentage drops — its
         denominator now includes Half_a's points while its tests visit
         only [greet]'s. The project number (dune build @cover) is
         unaffected: test_a covers the rest. *)
      test "greets from half_b's suite too" (fun () ->
          equal string "hello, b" (A.greet "b"));
      test "clamps low" (fun () -> equal int 0 (B.clamp 0 9 (-5)));
      test "clamps high" (fun () -> equal int 9 (B.clamp 0 9 50));
      test "passes through" (fun () -> equal int 4 (B.clamp 0 9 4));
      test "sums" (fun () -> equal int 6 (B.sum [ 1; 2; 3 ]));
      test "sign of positive" (fun () -> equal int 1 (B.sign 3));
      test "sign of negative" (fun () -> equal int (-1) (B.sign (-3)));
      test "sign of zero" (fun () -> equal int 0 (B.sign 0));
    ]
