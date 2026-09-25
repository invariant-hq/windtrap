let total shapes =
  List.fold_left (fun sum s -> sum +. Linked_shapes.Shapes.area s) 0. shapes

let%test "an empty scene covers nothing" = assert (total [] = 0.)
