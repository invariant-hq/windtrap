(** Type-safe comparisons with Testables.

    Testables combine equality checking with pretty-printing. When a test fails,
    Windtrap shows both values in a readable format. *)

open Windtrap

type point = { x : int; y : int }
(** A custom type we want to test. *)

(** Create a testable for our custom type. *)
let point_testable = Testable.of_equal (fun p1 p2 -> p1.x = p2.x && p1.y = p2.y)

(** A testable with a custom printer. *)
let point_pp fmt p = Format.fprintf fmt "Point(%d, %d)" p.x p.y

let point_testable_with_pp =
  Testable.make ~pp:point_pp ~equal:(fun p1 p2 -> p1.x = p2.x && p1.y = p2.y) ()

let () =
  run "Testables"
    [
      group "Built-in types"
        [
          test "int" (fun () -> equal Testable.int 42 42);
          test "string" (fun () -> equal Testable.string "hello" "hello");
          test "bool" (fun () -> equal Testable.bool true true);
          test "float (epsilon)" (fun () ->
              equal (Testable.float 0.001) 3.14159 3.14160);
          test "char" (fun () -> equal Testable.char 'a' 'a');
        ];
      group "Combinators"
        [
          test "list" (fun () ->
              equal (Testable.list Testable.int) [ 1; 2; 3 ] [ 1; 2; 3 ]);
          test "option" (fun () ->
              equal (Testable.option Testable.int) (Some 42) (Some 42));
          test "result" (fun () ->
              equal
                (Testable.result Testable.int Testable.string)
                (Ok 42) (Ok 42));
          test "pair" (fun () ->
              equal
                (Testable.pair Testable.int Testable.string)
                (1, "one") (1, "one"));
          test "triple" (fun () ->
              equal
                (Testable.triple Testable.int Testable.string Testable.bool)
                (1, "one", true) (1, "one", true));
        ];
      group "Unordered comparison"
        [
          test "slist ignores order" (fun () ->
              equal
                (Testable.slist Testable.int Int.compare)
                [ 3; 1; 2 ] [ 1; 2; 3 ]);
        ];
      group "Custom types"
        [
          test "of_equal" (fun () ->
              equal point_testable { x = 1; y = 2 } { x = 1; y = 2 });
          test "with custom printer" (fun () ->
              equal point_testable_with_pp { x = 3; y = 4 } { x = 3; y = 4 });
        ];
      group "Transforming testables"
        [
          test "contramap" (fun () ->
              (* Compare strings by their length *)
              let by_length = Testable.contramap String.length Testable.int in
              equal by_length "hello" "world");
        ];
      group "Always pass"
        [ test "pass ignores values" (fun () -> equal Testable.pass () ()) ];
    ]
