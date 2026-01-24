(** Property-based and parameterized testing.

    Property tests check that invariants hold for randomly generated inputs.
    When a property fails, Windtrap automatically shrinks to find a minimal
    counterexample.

    Parameterized tests ([cases]) run the same test logic across a list of
    specific inputs, generating one test per case with automatic naming. *)

open Windtrap

let () =
  run "Properties"
    [
      group "List properties"
        [
          prop "reverse is involutive"
            Testable.(list int)
            (fun l -> List.rev (List.rev l) = l);
          prop "append length"
            Testable.(pair (list int) (list int))
            (fun (l1, l2) ->
              List.length (l1 @ l2) = List.length l1 + List.length l2);
          prop "sort is idempotent"
            Testable.(list int)
            (fun l ->
              let sorted = List.sort Int.compare l in
              List.sort Int.compare sorted = sorted);
        ];
      group "String properties"
        [
          prop "length of concat"
            Testable.(pair string string)
            (fun (s1, s2) ->
              String.length (s1 ^ s2) = String.length s1 + String.length s2);
        ];
      group "Numeric properties"
        [
          prop "addition is commutative"
            Testable.(pair int int)
            (fun (a, b) -> a + b = b + a);
          prop "multiplication distributes over addition"
            Testable.(triple int int int)
            (fun (a, b, c) -> a * (b + c) = (a * b) + (a * c));
        ];
      group "With assumptions"
        [
          prop "division is inverse of multiplication"
            Testable.(pair int int)
            (fun (a, b) ->
              (* Skip when divisor is zero. *)
              assume (b <> 0);
              a * b / b = a);
        ];
      group "Using prop'"
        [
          prop' "list is sorted after sort"
            Testable.(list int)
            (fun l ->
              let sorted = List.sort Int.compare l in
              let rec is_sorted = function
                | [] | [ _ ] -> true
                | x :: y :: rest -> x <= y && is_sorted (y :: rest)
              in
              is_true (is_sorted sorted));
        ];
      (* Parameterized tests: run the same assertion across multiple inputs.
         Each case becomes its own test with an auto-generated name. *)
      cases
        Testable.(triple int int int)
        [ (2, 3, 5); (0, 0, 0); (-1, 1, 0); (100, -100, 0) ]
        "addition"
        (fun (a, b, expected) -> equal Testable.int expected (a + b));
    ]
