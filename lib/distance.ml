(*---------------------------------------------------------------------------
   Copyright (c) 2020-2021 Craig Ferguson
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   Levenshtein edit distance adapted from Craig Ferguson's work on Alcotest
   (https://github.com/mirage/alcotest/pull/247).
  ---------------------------------------------------------------------------*)

type index = int

type command =
  | Insert of { expected : index; actual : index }
  | Delete of { expected : index }
  | Substitute of { expected : index; actual : index }

type edit_script = command list

let map_expected f = function
  | Insert i -> Insert { i with expected = f i.expected }
  | Delete d -> Delete { expected = f d.expected }
  | Substitute s -> Substitute { s with expected = f s.expected }

let map_actual f = function
  | Insert i -> Insert { i with actual = f i.actual }
  | Substitute s -> Substitute { s with actual = f s.actual }
  | Delete _ as d -> d

module Subarray : sig
  type 'a t

  val of_array : 'a array -> 'a t
  val of_list : 'a list -> 'a t
  val of_string : string -> char t
  val get : 'a t -> int -> 'a
  val length : 'a t -> int
  val origin : 'a t -> int
  val truncate : origin:int -> length:int -> 'a t -> 'a t
end = struct
  type 'a t = { get : int -> 'a; origin : int; length : int }

  let of_array a = { get = Array.get a; origin = 0; length = Array.length a }
  let of_list l = of_array (Array.of_list l)
  let of_string s = { get = String.get s; origin = 0; length = String.length s }

  let get { get; origin; length } i =
    if i >= length then invalid_arg "index out of bounds";
    get (i + origin)

  let length { length; _ } = length
  let origin { origin; _ } = origin

  let truncate ~origin ~length
      { get; origin = prev_origin; length = prev_length } =
    if origin < prev_origin || length > prev_length then
      invalid_arg "Cannot expand array during truncation";
    { get; origin; length }
end

type ('a, _) container =
  | Array : ('a, 'a array) container
  | List : ('a, 'a list) container
  | String : (char, string) container

(* ───── Levenshtein Edit Distance ───── *)

(* Standard Wagner-Fischer algorithm: build cost matrix, then backtrack
   to recover the minimal edit script. Operates on Subarrays to allow
   strip_common_outer to narrow the working range before the O(n*m) DP. *)

let construct_grid ~equal a b =
  let grid_x_length = Subarray.length a + 1
  and grid_y_length = Subarray.length b + 1 in
  let grid = Array.make_matrix grid_x_length grid_y_length 0 in

  for i = 0 to grid_x_length - 1 do
    for j = 0 to grid_y_length - 1 do
      let cost =
        if min i j = 0 then max i j
        else if equal (Subarray.get a (i - 1)) (Subarray.get b (j - 1)) then
          grid.(i - 1).(j - 1)
        else
          1 + min grid.(i - 1).(j) (min grid.(i).(j - 1) grid.(i - 1).(j - 1))
      in
      grid.(i).(j) <- cost
    done
  done;
  grid

let reconstruct_edit_script grid =
  let rec aux acc = function
    | 0, 0 -> acc
    | i, 0 -> List.init i (fun k -> Delete { expected = k })
    | 0, j -> List.init j (fun k -> Insert { expected = k; actual = k })
    | i, j ->
        let delete_cost = grid.(i - 1).(j)
        and insert_cost = grid.(i).(j - 1)
        and subst_cost = grid.(i - 1).(j - 1) in
        if grid.(i).(j) = subst_cost && delete_cost >= subst_cost then
          (* Characters match -- diagonal move with no cost increase *)
          aux acc (i - 1, j - 1)
        else if delete_cost <= insert_cost && delete_cost <= subst_cost then
          aux (Delete { expected = i - 1 } :: acc) (i - 1, j)
        else if insert_cost <= subst_cost then
          aux (Insert { expected = i; actual = j - 1 } :: acc) (i, j - 1)
        else
          aux
            (Substitute { expected = i - 1; actual = j - 1 } :: acc)
            (i - 1, j - 1)
  in
  aux [] (Array.length grid - 1, Array.length grid.(0) - 1)

let levenshtein_dp ~equal a b =
  let grid = construct_grid ~equal a b in
  reconstruct_edit_script grid
  |> List.map (fun cmd ->
      cmd
      |> map_expected (( + ) (Subarray.origin a))
      |> map_actual (( + ) (Subarray.origin b)))

(* Strip matching prefix and suffix before running DP, reducing the
   problem size. Returns `Equal if the sequences are identical. *)
let strip_common_outer ~equal (a, b) =
  let len_a = Subarray.length a and len_b = Subarray.length b in

  let rec modify_until_nonequal op (i, j) =
    let a_oob = i < 0 || i >= len_a and b_oob = j < 0 || j >= len_b in
    if a_oob && b_oob then `Equal
    else if a_oob || b_oob || not (equal (Subarray.get a i) (Subarray.get b j))
    then `Non_equal (i, j)
    else modify_until_nonequal op (op i, op j)
  in

  match modify_until_nonequal (( + ) 1) (0, 0) with
  | `Equal -> `Equal
  | `Non_equal (a_origin, b_origin) -> (
      match modify_until_nonequal (( + ) (-1)) (len_a - 1, len_b - 1) with
      | `Equal -> assert false
      | `Non_equal (a_last, b_last) ->
          `Non_equal
            ( Subarray.truncate ~origin:a_origin
                ~length:(a_last - a_origin + 1)
                a,
              Subarray.truncate ~origin:b_origin
                ~length:(b_last - b_origin + 1)
                b ))

let levenshtein (type a c) (typ : (a, c) container) ~(equal : a -> a -> bool)
    (a : c) (b : c) : edit_script =
  let a, b =
    match typ with
    | Array -> (Subarray.of_array a, Subarray.of_array b)
    | List -> (Subarray.of_list a, Subarray.of_list b)
    | String -> (Subarray.of_string a, Subarray.of_string b)
  in
  match strip_common_outer ~equal (a, b) with
  | `Equal -> []
  | `Non_equal (a, b) -> levenshtein_dp ~equal a b

let edit_indices (edits : edit_script) : int list * int list =
  let expected_indices, actual_indices =
    List.fold_left
      (fun (exp, act) cmd ->
        match cmd with
        | Insert { actual; _ } -> (exp, actual :: act)
        | Delete { expected } -> (expected :: exp, act)
        | Substitute { expected; actual } -> (expected :: exp, actual :: act))
      ([], []) edits
  in
  (List.rev expected_indices, List.rev actual_indices)
