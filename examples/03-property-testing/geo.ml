type shape = Circle of float | Rect of float * float

let pp ppf = function
  | Circle r -> Format.fprintf ppf "Circle %g" r
  | Rect (w, h) -> Format.fprintf ppf "Rect (%g, %g)" w h

let area = function Circle r -> Float.pi *. r *. r | Rect (w, h) -> w *. h

let scale k = function
  | Circle r -> Circle (k *. r)
  | Rect (w, h) -> Rect (k *. w, k *. h)

let to_string = function
  | Circle r -> Printf.sprintf "circle %.17g" r
  | Rect (w, h) -> Printf.sprintf "rect %.17g %.17g" w h

let of_string s =
  match String.split_on_char ' ' s with
  | [ "circle"; r ] -> Some (Circle (float_of_string r))
  | [ "rect"; w; h ] -> Some (Rect (float_of_string w, float_of_string h))
  | _ -> None

type drawing = Shape of shape | Group of drawing list

let rec pp_drawing ppf = function
  | Shape s -> pp ppf s
  | Group ds ->
      Format.fprintf ppf "Group [%a]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
           pp_drawing)
        ds

let rec total_area = function
  | Shape s -> area s
  | Group ds -> List.fold_left (fun sum d -> sum +. total_area d) 0. ds

let rec shapes = function
  | Shape s -> [ s ]
  | Group ds -> List.concat_map shapes ds

let equal_drawing a b = shapes a = shapes b
let compare_drawing a b = compare (shapes a) (shapes b)
let hash_drawing d = Hashtbl.hash (shapes d)
let empty = Group []
let combine a b = Group [ a; b ]
let no_turn = 0
let turn a b = (a + b) mod 4
let undo t = (4 - t) mod 4

let rec subsequence xs ys =
  match (xs, ys) with
  | [], _ -> true
  | _ :: _, [] -> false
  | x :: xs', y :: ys' ->
      if x = y then subsequence xs' ys' else subsequence xs ys'

let part_of a b = subsequence (shapes a) (shapes b)

let is_valid = function
  | Circle r -> r >= 0.
  | Rect (w, h) -> w >= 0. && h >= 0.
