(*---------------------------------------------------------------------------
   Copyright (c) 2013-2017 Guillaume Bury, Simon Cruanes, Vincent Hugot,
                           Jan Midtgaard
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: BSD-2-Clause

   Tree structure and list shrinking algorithm derived from QCheck2
   (https://github.com/c-cube/qcheck).
  ---------------------------------------------------------------------------*)

type 'a t = Tree of 'a * 'a t Seq.t

let root (Tree (x, _)) = x
let children (Tree (_, xs)) = xs
let pure x = Tree (x, Seq.empty)

let rec map f (Tree (x, xs)) =
  let y = f x in
  let ys () = Seq.map (map f) xs () in
  Tree (y, ys)

let ( >|= ) t f = map f t

let rec ap (Tree (f, fs)) (Tree (x, xs)) =
  let y = f x in
  let ys () =
    Seq.append
      (Seq.map (fun f' -> ap f' (Tree (x, xs))) fs)
      (Seq.map (fun x' -> ap (Tree (f, fs)) x') xs)
      ()
  in
  Tree (y, ys)

let ( <*> ) = ap
let liftA2 f a b = map f a <*> b

let rec bind (Tree (x, xs)) f =
  let (Tree (y, ys_of_x)) = f x in
  let ys_of_xs () = Seq.map (fun x' -> bind x' f) xs () in
  let ys () = Seq.append ys_of_xs ys_of_x () in
  Tree (y, ys)

let ( >>= ) = bind

let rec make_primitive shrink x =
  let shrink_trees () = Seq.map (make_primitive shrink) (shrink x) () in
  Tree (x, shrink_trees)

let rec add_shrink_invariant p (Tree (x, xs)) =
  let xs' () =
    Seq.filter_map
      (fun (Tree (x', _) as t) ->
        if p x' then Some (add_shrink_invariant p t) else None)
      xs ()
  in
  Tree (x, xs')

let rec opt (Tree (x, xs)) =
  let shrinks () = Seq.cons (pure None) (Seq.map opt xs) () in
  Tree (Some x, shrinks)

let rec sequence_list = function
  | [] -> pure []
  | hd :: tl -> liftA2 List.cons hd (sequence_list tl)

let rec drop_at i = function
  | [] -> []
  | _ :: xs when i = 0 -> xs
  | x :: xs -> x :: drop_at (i - 1) xs

let list_split l len =
  let rec go acc n = function
    | [] -> (List.rev acc, [])
    | xs when n = 0 -> (List.rev acc, xs)
    | x :: xs -> go (x :: acc) (n - 1) xs
  in
  go [] len l

(* List shrinking strategy:
   - Small lists (< 4): try dropping each element one at a time.
   - Larger lists: bisection -- try each half, then drop head, then trim tail.
   In both cases, fall through to element-wise shrinking via sequence_list. *)
let rec build_list_shrink_tree l =
  match l with
  | [] -> Seq.empty
  | _ :: _ ->
      fun () ->
        let len = List.length l in
        if len < 4 then
          let rec drop_each i () =
            if i >= len then (children (sequence_list l)) ()
            else
              let dropped = drop_at i l in
              Seq.Cons
                ( Tree (List.map root dropped, build_list_shrink_tree dropped),
                  drop_each (i + 1) )
          in
          drop_each 0 ()
        else
          let half = (len + 1) / 2 in
          let xs, ys = list_split l half in
          let xs_roots = List.map root xs in
          let ys_roots = List.map root ys in
          (* Try first half only, then second half only *)
          Seq.Cons
            ( Tree (xs_roots, build_list_shrink_tree xs),
              fun () ->
                Seq.Cons
                  ( Tree (ys_roots, build_list_shrink_tree ys),
                    fun () ->
                      (* Drop the head element *)
                      let rest = List.tl l in
                      let rest_roots = List.map root rest in
                      Seq.Cons
                        ( Tree (rest_roots, build_list_shrink_tree rest),
                          fun () ->
                            (* Drop from the second half to reduce length by one *)
                            if List.length ys > 1 then
                              let shorter = xs @ List.tl ys in
                              Seq.Cons
                                ( Tree
                                    ( List.map root shorter,
                                      build_list_shrink_tree shorter ),
                                  fun () -> (children (sequence_list l)) () )
                            else (children (sequence_list l)) () ) ) )
