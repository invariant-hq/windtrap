(* The property chapter's examples (doc/manual/property-testing.md): one
   [pp] feeds both worlds — Testable.make for assertions and Gen.with_pp for
   counterexamples; known regressions worth keeping forever go in code via
   [~examples]; [assume] discards a rare precondition; [cover] and
   [classify] say whether the generator reaches the interesting region. *)

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
    let+ w = float_range 0. 10. and+ h = float_range 0. 10. in
    Rect (w, h))
  |> Gen.with_pp pp_shape

(* A codec whose round trip is a law. *)
let encode l = String.concat "," (List.map string_of_int l)

let decode = function
  | "" -> []
  | s -> List.map int_of_string (String.split_on_char ',' s)

let () =
  exit
  @@ run "geo"
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
         prop "decode inverts encode"
           Gen.(list small_int)
           (fun l -> equal (list int) l (decode (encode l)));
         prop "division round-trips"
           Gen.(pair small_int small_int)
           (fun (a, b) ->
             assume (b <> 0);
             equal int a ((a / b * b) + (a mod b)));
         prop "parity is exercised" ~count:200 Gen.small_int (fun n ->
             cover "even" (n mod 2 = 0);
             cover "odd" (n mod 2 <> 0);
             classify "zero" (n = 0);
             equal int n n);
       ]
