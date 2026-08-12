(* Stats mixes the ladder's rungs in one place — its own suite: a shape
   law, two hand-derived points, and a snapshot for the render nobody
   wants to hand-maintain. Labels are generated [a-z] only — the
   line-count law is about rows, so newline-bearing labels are excluded
   by construction rather than by [assume]. *)

open Windtrap
module Stats = Windtrap_example_blueprint.Stats

let gen_rows =
  Gen.(
    list ~size:(int_range 0 8) (pair (string_of (char_range 'a' 'z')) small_int))

let line_count s = List.length (String.split_on_char '\n' s)

let () =
  run "stats"
    [
      group "render"
        [
          prop "prints one line per row plus the total" gen_rows (fun rows ->
              equal int (List.length rows + 1) (line_count (Stats.render rows)));
          test "pads and draws a single row" (fun () ->
              equal text "x  ##\ntotal 2" (Stats.render [ ("x", 2) ]));
          test "clamps negative counts to zero" (fun () ->
              equal text "x  \ntotal 0" (Stats.render [ ("x", -2) ]));
          test "renders a small table" (fun () ->
              snapshot "histogram"
                (Stats.render [ ("reds", 3); ("greens", 5); ("blues", 0) ]));
        ];
    ]
