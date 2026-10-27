(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [pair_off ~equal xs ys] pairs each element of [xs] with the first element
   of [ys] left that equals it, and is the elements of either left unpaired,
   in order. Quadratic, so it runs only on what the walk below sets aside or
   on one tie of the order. *)
let pair_off ~equal xs ys =
  let rec take x seen = function
    | [] -> None
    | y :: ys when equal x y -> Some (List.rev_append seen ys)
    | y :: ys -> take x (y :: seen) ys
  in
  let unpaired, ys =
    List.fold_left
      (fun (unpaired, ys) x ->
        match take x [] ys with
        | Some ys -> (unpaired, ys)
        | None -> (x :: unpaired, ys))
      ([], ys) xs
  in
  (List.rev unpaired, ys)

let rec span p = function
  | x :: xs when p x ->
      let tie, rest = span p xs in
      (x :: tie, rest)
  | xs -> ([], xs)

let difference ~equal ~compare expected actual =
  (* [missing] and [extra] are reversed. *)
  let rec walk missing extra e a =
    match (e, a) with
    | [], a -> (missing, List.rev_append a extra)
    | e, [] -> (List.rev_append e missing, extra)
    | x :: e', y :: a' when equal x y -> walk missing extra e' a'
    | x :: e', y :: a' ->
        let c = compare x y in
        if c < 0 then walk (x :: missing) extra e' a
        else if c > 0 then walk missing (y :: extra) e a'
        else
          let tie_e, e = span (fun z -> compare z x = 0) e in
          let tie_a, a = span (fun z -> compare z x = 0) a in
          let tie_missing, tie_extra = pair_off ~equal tie_e tie_a in
          walk
            (List.rev_append tie_missing missing)
            (List.rev_append tie_extra extra)
            e a
  in
  let missing, extra =
    walk [] []
      (List.stable_sort compare expected)
      (List.stable_sort compare actual)
  in
  pair_off ~equal (List.rev missing) (List.rev extra)

type explanation = {
  missing : string;
  missing_count : int;
  extra : string;
  extra_count : int;
}

(* The sides of the innermost [explained] on each domain, and what was
   offered for them. *)
type slot = {
  expected : Obj.t;
  actual : Obj.t;
  mutable offered : explanation Lazy.t option;
}

let current : slot option Domain.DLS.key = Domain.DLS.new_key (fun () -> None)

let offer expected actual e =
  match Domain.DLS.get current with
  | Some s when s.expected == Obj.repr expected && s.actual == Obj.repr actual
    ->
      s.offered <- Some e
  | Some _ | None -> ()

let explained expected actual fn =
  let enclosing = Domain.DLS.get current in
  let slot =
    { expected = Obj.repr expected; actual = Obj.repr actual; offered = None }
  in
  Domain.DLS.set current (Some slot);
  match fn () with
  | v ->
      Domain.DLS.set current enclosing;
      (v, Option.map Lazy.force slot.offered)
  | exception exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      Domain.DLS.set current enclosing;
      Printexc.raise_with_backtrace exn backtrace
