(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(* The reducer, alone over hand-written tests of choice sequences. *)

open Windtrap
module Choice = Windtrap.Private.Choice
module Shrink = Windtrap.Private.Shrink

let choices = Testable.(array int64)
let case ?(spans = []) ?(dependents = []) choices : Shrink.case =
  { choices; spans; dependents }

(* A test that fails when [fails] holds of the choices, reading them all. *)
let failing fails choices =
  if fails choices then
    Shrink.Fails { parsed = case choices; keep = ignore }
  else Shrink.Passes

let shrunk ?(budget = 10_000) ?(cost = 1) fails start =
  let best, _, ending = Shrink.shrink ~budget ~cost (failing fails) start in
  (best.choices, ending)

let sum = Array.fold_left Int64.add 0L

let reducer =
  group "The reducer"
    [
      test "shortlex orders by length, then unsigned at the first difference"
        (fun () ->
          equal (list int) [ -1; 1; -1; 0 ]
            (List.map
               (fun (a, b) -> Int.compare (Shrink.shortlex a b) 0)
               [
                 ([| 9L |], [| 0L; 0L |]);
                 ([| 1L; 0L |], [| 0L; 9L |]);
                 ([| 1L |], [| -1L |]);
                 ([| 3L |], [| 3L |]);
               ]));
      test "a threshold on a sum shrinks to one choice at the threshold"
        (fun () ->
          equal
            (pair choices string)
            ([| 100L |], "converged")
            (let best, ending =
               shrunk
                 (fun c -> Int64.compare (sum c) 100L >= 0)
                 (case [| 7L; 300L; 12L; 5_000L |])
             in
             (best, match ending with Converged -> "converged" | _ -> "spent")));
      test "a span is deleted whole" (fun () ->
          (* Two choices that only fail together, inside one span. *)
          let fails c = Array.length c >= 2 && c.(0) = 1L && c.(1) = 1L in
          let spans = [ { Choice.start = 2; stop = 4 } ] in
          equal choices [| 1L; 1L |]
            (fst (shrunk fails (case ~spans [| 1L; 1L; 5L; 5L |]))));
      test
        "a deletion lowers a dependent by the positions it counts past inside"
        (fun () ->
          (* The last choice counts past the two before it: deleting one of
             them lowers it by one, both by two, and the dependent itself
             nothing. *)
          let start =
            case
              ~dependents:[ { Choice.at = 2; over = [ 1; 0 ] } ]
              [| 7L; 8L; 2L |]
          in
          let deletions = List.of_seq (Seq.take 5 (Shrink.candidates start)) in
          equal (list choices)
            [ [| 7L |]; [| 0L |]; [| 7L; 8L |]; [| 7L; 1L |]; [| 8L; 1L |] ]
            deletions);
      test "every test spends the cost, a discard one, a repeat nothing"
        (fun () ->
          let runs = ref 0 in
          let test c =
            incr runs;
            if Array.length c = 1 then Shrink.Discards
            else failing (fun c -> sum c > 0L) c
          in
          let _, _, ending =
            Shrink.shrink ~budget:7 ~cost:3 test (case [| 1L; 1L |])
          in
          equal
            (pair int string)
            (3, "spent")
            ( !runs,
              match ending with Converged -> "converged" | _ -> "spent" ));
      test "a search that spends the budget ends at the next candidate"
        (fun () ->
          let runs = ref 0 in
          let test c =
            incr runs;
            failing (fun c -> sum c > 0L) c
          in
          let best, steps, ending =
            Shrink.shrink ~budget:1 ~cost:1 test (case [| 5L; 5L |])
          in
          equal
            (triple int int string)
            (1, steps, "spent")
            ( !runs,
              (if best.choices = [| 5L; 5L |] then 0 else 1),
              match ending with Converged -> "converged" | _ -> "spent" ));
    ]

let () = exit (run "shrink" [ reducer ])
