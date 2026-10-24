(* Planted: [push] copies one element too few when the array grows. *)

(* A growable array of integers. *)

type t = { mutable data : int array; mutable length : int }

let create () = { data = Array.make 2 0; length = 0 }
let length v = v.length

let push v x =
  if v.length = Array.length v.data then begin
    let data = Array.make (2 * Array.length v.data) 0 in
    Array.blit v.data 0 data 0 (v.length - 1);
    v.data <- data
  end;
  v.data.(v.length) <- x;
  v.length <- v.length + 1

let get v i =
  if i < 0 || i >= v.length then invalid_arg "Vec.get" else v.data.(i)

let set v i x =
  if i < 0 || i >= v.length then invalid_arg "Vec.set" else v.data.(i) <- x
