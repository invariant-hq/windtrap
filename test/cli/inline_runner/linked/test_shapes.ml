open Windtrap
open Linked_shapes

let area =
  group "area"
    [
      test "a 2 by 3 rectangle has area 6" (fun () ->
          equal float_exact 6. (Shapes.area (Rectangle (2., 3.))));
    ]

let () = exit (run "shapes" [ area ])
