(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let area =
  Windtrap.group "area"
    [
      Windtrap.test "a 2 by 3 rectangle has area 6" (fun () ->
          Windtrap.equal Windtrap.float_exact 6.
            (Linked_shapes.Shapes.area (Rectangle (2., 3.))));
    ]

let () = exit (Windtrap.run "shapes" [ area ])
