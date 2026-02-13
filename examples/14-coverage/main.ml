(* Code coverage.

   Windtrap has built-in coverage instrumentation powered by windtrap.ppx.

   To enable coverage, add an (instrumentation ...) stanza to the dune file of
   the library you want to measure, then run tests with [--instrument-with]:

   {[dune exec ./examples/14-coverage/main.exe \
       --instrument-with windtrap.ppx]}

   After the run, coverage data is written to [_build/_coverage/]. Use the
   [windtrap coverage] command to view a report:

   {[dune exec windtrap -- coverage --per-file]} *)

open Windtrap

(* ── A small library to measure coverage on ─────────────────────────── *)

type shape =
  | Circle of float
  | Rect of float * float
  | Triangle of float * float

let area = function
  | Circle r -> Float.pi *. r *. r
  | Rect (w, h) -> w *. h
  | Triangle (b, h) -> 0.5 *. b *. h

let perimeter = function
  | Circle r -> 2.0 *. Float.pi *. r
  | Rect (w, h) -> 2.0 *. (w +. h)
  | Triangle _ -> failwith "perimeter: triangle requires side lengths"

let describe shape =
  let a = area shape in
  Printf.sprintf "area=%.2f" a

(* ── Tests ───────────────────────────────────────────────────────────── *)

let () =
  run "Coverage"
    [
      group "area"
        [
          test "circle" (fun () ->
              let a = area (Circle 1.0) in
              equal Testable.(float 0.01) Float.pi a);
          test "rectangle" (fun () ->
              equal Testable.(float 0.01) 12.0 (area (Rect (3.0, 4.0))));
          test "triangle" (fun () ->
              equal Testable.(float 0.01) 6.0 (area (Triangle (4.0, 3.0))));
        ];
      group "perimeter"
        [
          test "circle" (fun () ->
              let p = perimeter (Circle 1.0) in
              equal Testable.(float 0.01) (2.0 *. Float.pi) p);
          test "rectangle" (fun () ->
              equal Testable.(float 0.01) 14.0 (perimeter (Rect (3.0, 4.0))));
          (* Note: the Triangle branch of [perimeter] is intentionally left
             untested to demonstrate partial coverage. *)
        ];
      group "describe"
        [
          test "circle description" (fun () ->
              let s = describe (Circle 1.0) in
              equal Testable.string "area=3.14" s);
        ];
    ]
