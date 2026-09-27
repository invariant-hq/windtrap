open Windtrap

let size = Gen.(map float_of_int (int_range 0 100))

let gen_shape =
  Gen.(
    one_of
      [
        map (fun r -> Geo.Circle r) size;
        (let+ w = size and+ h = size in
         Geo.Rect (w, h));
      ])
  |> Gen.with_pp Geo.pp

let area =
  group "area"
    [
      prop "is never negative" gen_shape (fun s ->
          at_least ~__POS__ float_exact ~than:0. (Geo.area s));
    ]

let shape = Testable.make ~pp:Geo.pp ~equal:( = )

let to_string =
  group "to_string"
    [
      prop "is read back by of_string" gen_shape (fun s ->
          equal ~__POS__ (option shape) (Some s)
            (Geo.of_string (Geo.to_string s)));
    ]

let close = float_rel ~rel:1e-9 ~abs:1e-9

let scale =
  group "scale"
    [
      prop "multiplies the area by k squared"
        ~examples:[ (2., Geo.Rect (1., 3.)) ]
        Gen.(pair (float_range 0. 10.) gen_shape)
        (fun (k, s) ->
          equal ~__POS__ close (k *. k *. Geo.area s) (Geo.area (Geo.scale k s)));
    ]

let inverse =
  group "inverse"
    [
      prop "undoes scale by k"
        Gen.(pair (float_range 0. 10.) gen_shape)
        (fun (k, s) ->
          assume (k > 0.);
          let back = Geo.scale (1. /. k) (Geo.scale k s) in
          classify "circle" (match s with Circle _ -> true | Rect _ -> false);
          cover "rect" (match s with Rect _ -> true | Circle _ -> false);
          equal ~__POS__ close (Geo.area s) (Geo.area back));
    ]

let rec gen_drawing depth =
  if depth = 0 then Gen.map (fun s -> Geo.Shape s) gen_shape
  else
    Gen.(
      one_of
        [
          map (fun s -> Geo.Shape s) gen_shape;
          map
            (fun ds -> Geo.Group ds)
            (list ~size:(int_range 0 4) (gen_drawing (depth - 1)));
        ])

let total_area =
  group "total_area"
    [
      prop "is never negative"
        Gen.(
          with_pp Geo.pp_drawing
            (let* depth = int_range 0 3 in
             gen_drawing depth))
        (fun d -> at_least ~__POS__ float_exact ~than:0. (Geo.total_area d));
    ]

let read s = require_some (Geo.of_string s)

let codec =
  group "codec"
    [
      prop "reads back what it prints" gen_shape
        (Law.round_trip shape string Geo.to_string read);
      cases ~name:Fun.id "prints what it reads"
        [ "circle 1"; "rect 2 0.5" ]
        (Law.round_trip string shape read Geo.to_string);
      test "reads back a unit circle" (fun () ->
          Law.round_trip shape string Geo.to_string read (Geo.Circle 1.));
    ]

let drawing =
  Testable.(
    with_compare Geo.compare_drawing
      (make ~pp:Geo.pp_drawing ~equal:Geo.equal_drawing))

let any_drawing =
  Gen.(
    with_pp Geo.pp_drawing
      (let* depth = int_range 0 3 in
       gen_drawing depth))

let regroup d = Geo.Group [ d ]

let witness =
  group "witness"
    [
      prop "equal_drawing is an equivalence"
        Gen.(pair any_drawing any_drawing)
        (Law.equivalence ~respell:regroup drawing);
      prop "compare_drawing is a total order"
        Gen.(triple any_drawing any_drawing any_drawing)
        (Law.order ~respell:regroup drawing);
      prop "hash_drawing ignores regrouping" any_drawing
        (Law.ignores drawing int Geo.hash_drawing regroup);
    ]

let combine =
  group "combine"
    [
      prop "is associative"
        Gen.(triple any_drawing any_drawing any_drawing)
        (Law.associative drawing Geo.combine);
      prop "has empty as its neutral element" any_drawing
        (Law.neutral drawing Geo.combine Geo.empty);
      prop "adds the areas"
        Gen.(pair any_drawing any_drawing)
        (Law.homomorphic drawing close Geo.total_area Geo.combine ( +. ));
    ]

let turns = Gen.int_range 0 3

let turn =
  group "turn"
    [
      prop "is associative"
        Gen.(triple turns turns turns)
        (Law.associative int Geo.turn);
      prop "has no_turn as its neutral element" turns
        (Law.neutral int Geo.turn Geo.no_turn);
      prop "has undo as its inverse" turns
        (Law.invertible int Geo.turn Geo.no_turn Geo.undo);
    ]

let factor = Gen.float_range 0. 10.

let scaling =
  group "scaling"
    [
      prop "grows the area with the factor"
        Gen.(pair (pair factor factor) gen_shape)
        (fun (ks, s) ->
          Law.monotone float_exact float_exact
            (fun k -> Geo.area (Geo.scale k s))
            ks);
      prop "keeps a shape valid"
        Gen.(pair factor gen_shape)
        (fun (k, s) -> Law.preserves shape (Geo.scale k) Geo.is_valid s);
    ]

let part_of =
  group "part_of"
    [
      prop "is a partial order"
        Gen.(triple any_drawing any_drawing any_drawing)
        (fun (a, x, y) ->
          let b = Geo.combine a x in
          Law.partial_order drawing Geo.part_of (a, b, Geo.combine b y));
    ]

let () =
  exit
    (run "geo"
       [
         area;
         to_string;
         scale;
         inverse;
         total_area;
         codec;
         witness;
         combine;
         turn;
         scaling;
         part_of;
       ])
