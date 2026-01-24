(** Testing exceptions.

    Windtrap provides assertions for verifying that code raises specific
    exceptions or no exceptions at all. *)

open Windtrap

exception Custom_error of string

let might_fail condition =
  if condition then failwith "something went wrong" else 42

let validate_positive n =
  if n < 0 then raise (Custom_error "negative number") else n

let () =
  run "Exceptions"
    [
      group "Expected exceptions"
        [
          test "raises exact exception" (fun () ->
              raises (Failure "boom") (fun () -> failwith "boom"));
          test "raises custom exception" (fun () ->
              raises (Custom_error "negative number") (fun () ->
                  validate_positive (-1) |> ignore));
        ];
      group "Pattern matching"
        [
          test "raises_match with predicate" (fun () ->
              raises_match
                (function Failure msg -> String.length msg > 0 | _ -> false)
                (fun () -> failwith "error occurred"));
          test "match on custom exception" (fun () ->
              raises_match
                (function
                  | Custom_error msg -> String.sub msg 0 8 = "negative"
                  | _ -> false)
                (fun () -> validate_positive (-5) |> ignore));
        ];
      group "No exception"
        [
          test "no_raise returns value" (fun () ->
              let result = no_raise (fun () -> might_fail false) in
              equal Testable.int 42 result);
          test "safe computation" (fun () ->
              let result = no_raise (fun () -> 2 + 2) in
              equal Testable.int 4 result);
        ];
    ]
