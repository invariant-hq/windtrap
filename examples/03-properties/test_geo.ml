(* The guide's property example: one [pp] feeds both worlds — Testable.make
   for assertions and Gen.with_pp for counterexamples. Known regressions
   worth keeping forever go in code via [~examples]. *)

open Windtrap
open Geo

let pp_shape ppf = function
  | Circle r -> Format.fprintf ppf "Circle %g" r
  | Rect (w, h) -> Format.fprintf ppf "Rect (%g, %g)" w h

let shape = Testable.make ~pp:pp_shape ~equal:( = )

let gen_shape =
  Gen.(
    one_of
      [
        map (fun r -> Circle r) (float_range 0. 100.);
        map
          (fun (w, h) -> Rect (w, h))
          (pair (float_range 0. 100.) (float_range 0. 100.));
      ])
  |> Gen.with_pp pp_shape

let gen_rect =
  Gen.(
    map
      (fun (w, h) -> Rect (w, h))
      (pair (float_range 0. 10.) (float_range 0. 10.)))
  |> Gen.with_pp pp_shape

let () =
  run "geo"
    [
      prop "area non-negative" gen_shape (fun s ->
          is_true (Float.compare (Geo.area s) 0. >= 0));
      prop "rect area matches the formula"
        ~examples:[ Rect (2., 0.) ]
        gen_rect
        (fun s ->
          match s with
          | Rect (w, h) -> equal (float 1e-9) (w *. h) (Geo.area s)
          | Circle _ -> ());
      test "one pp feeds both worlds" (fun () ->
          equal shape (Circle 1.) (Circle 1.));
    ]
