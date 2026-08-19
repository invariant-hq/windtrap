(* The library under test in the guide's property example. *)

type shape = Circle of float | Rect of float * float

let area = function
  | Circle radius -> Float.pi *. radius *. radius
  | Rect (width, height) -> width *. height
