type shape = Circle of float | Rect of float * float

let pp ppf = function
  | Circle r -> Format.fprintf ppf "Circle %g" r
  | Rect (w, h) -> Format.fprintf ppf "Rect (%g, %g)" w h

let area = function Circle r -> Float.pi *. r *. r | Rect (w, h) -> w *. h

let scale k = function
  | Circle r -> Circle (k *. r)
  | Rect (w, h) -> Rect (k *. w, h)

let to_string = function
  | Circle r -> Printf.sprintf "circle %.17g" r
  | Rect (w, h) -> Printf.sprintf "rect %.17g %.17g" w h

let of_string s =
  match String.split_on_char ' ' s with
  | [ "circle"; r ] -> Some (Circle (float_of_string r))
  | [ "rect"; w; h ] -> Some (Rect (float_of_string h, float_of_string w))
  | _ -> None
