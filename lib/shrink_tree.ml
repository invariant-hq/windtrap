(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC

  Lazy rose trees for integrated shrinking (the QCheck2/Hedgehog design),
  with memoized children and a power-of-two chunked list shrink. Adapted
  from windtrap-next's shrink_tree.ml.
  --------------------------------------------------------------------------*)

type 'a t = { root : 'a; children : 'a t Seq.t }

let rec memoize sequence =
  let node =
    lazy
      (match sequence () with
      | Seq.Nil -> Seq.Nil
      | Seq.Cons (value, tail) -> Seq.Cons (value, memoize tail))
  in
  fun () -> Lazy.force node

let make ~root ~children = { root; children = memoize children }
let leaf root = make ~root ~children:Seq.empty
let root tree = tree.root
let children tree = tree.children

let rec map f tree =
  make ~root:(f tree.root)
    ~children:(Seq.map (fun child -> map f child) tree.children)

let rec bind tree f =
  let bound = f tree.root in
  make ~root:bound.root
    ~children:
      (Seq.append
         (Seq.map (fun candidate -> bind candidate f) tree.children)
         bound.children)

let rec pair left right =
  let left_children = Seq.map (fun child -> pair child right) left.children in
  let right_children = Seq.map (fun child -> pair left child) right.children in
  make ~root:(left.root, right.root)
    ~children:(Seq.append left_children right_children)

let roots trees =
  let values = ref [] in
  for index = Array.length trees - 1 downto 0 do
    values := trees.(index).root :: !values
  done;
  !values

let without_range trees ~first ~length =
  let source_length = Array.length trees in
  let candidate = Array.make (source_length - length) trees.(0) in
  for index = 0 to first - 1 do
    candidate.(index) <- trees.(index)
  done;
  for index = first + length to source_length - 1 do
    candidate.(index - length) <- trees.(index)
  done;
  candidate

let copy_with trees index value =
  let candidate = Array.copy trees in
  candidate.(index) <- value;
  candidate

let largest_power_of_two_below length =
  let rec loop power =
    if power > (length - 1) / 2 then power else loop (power * 2)
  in
  if length <= 1 then 0 else loop 1

let rec list_array trees =
  let length = Array.length trees in
  let rec chunk_candidates chunk first () =
    if chunk = 0 then Seq.Nil
    else if first + chunk <= length then
      let candidate = without_range trees ~first ~length:chunk in
      Seq.Cons (list_array candidate, chunk_candidates chunk (first + chunk))
    else chunk_candidates (chunk / 2) 0 ()
  in
  let rec element_candidates index candidates () =
    match candidates () with
    | Seq.Cons (candidate, tail) ->
        let trees = copy_with trees index candidate in
        Seq.Cons (list_array trees, element_candidates index tail)
    | Seq.Nil ->
        let next = index + 1 in
        if next = length then Seq.Nil
        else element_candidates next trees.(next).children ()
  in
  let structural =
    if length = 0 then Seq.empty
    else
      Seq.cons (leaf [])
        (chunk_candidates (largest_power_of_two_below length) 0)
  in
  let element =
    if length = 0 then Seq.empty else element_candidates 0 trees.(0).children
  in
  make ~root:(roots trees) ~children:(Seq.append structural element)

let list trees = list_array (Array.of_list trees)
