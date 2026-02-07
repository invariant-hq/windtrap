(*---------------------------------------------------------------------------
   Copyright (c) 2013-2017 Guillaume Bury, Simon Cruanes, Vincent Hugot,
                           Jan Midtgaard
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: BSD-2-Clause

   Shrinking algorithms derived from QCheck2
   (https://github.com/c-cube/qcheck).
  ---------------------------------------------------------------------------*)

(* Binary search shrinking: halve the distance between current candidate and
   destination at each step, producing a sequence that converges on the target. *)

let int_towards dest x () =
  Seq.unfold
    (fun current ->
      if current = x then None
      else
        (* Halve each operand before subtracting to avoid overflow.
           Naive (x - current) / 2 would overflow for int_towards min_int max_int. *)
        let half_diff = (x / 2) - (current / 2) in
        if half_diff = 0 then
          (* current is adjacent to x; emit it, then use x as sentinel to stop *)
          Some (current, x)
        else Some (current, current + half_diff))
    dest ()

let int32_towards dest x () =
  Seq.unfold
    (fun current ->
      if Int32.equal current x then None
      else
        let half_diff = Int32.sub (Int32.div x 2l) (Int32.div current 2l) in
        if Int32.equal half_diff 0l then Some (current, x)
        else Some (current, Int32.add current half_diff))
    dest ()

let int64_towards dest x () =
  Seq.unfold
    (fun current ->
      if Int64.equal current x then None
      else
        let half_diff = Int64.sub (Int64.div x 2L) (Int64.div current 2L) in
        if Int64.equal half_diff 0L then Some (current, x)
        else Some (current, Int64.add current half_diff))
    dest ()

(* Float halving can produce infinitely many distinct values without reaching
   the target, so we cap at 15 iterations to keep shrinking bounded. *)
let nativeint_towards dest x () =
  Seq.unfold
    (fun current ->
      if Nativeint.equal current x then None
      else
        let half_diff =
          Nativeint.sub (Nativeint.div x 2n) (Nativeint.div current 2n)
        in
        if Nativeint.equal half_diff 0n then Some (current, x)
        else Some (current, Nativeint.add current half_diff))
    dest ()

let float_towards dest x () =
  let rec go current limit () =
    if limit <= 0 then Seq.Nil
    else if Float.equal current x then Seq.Nil
    else
      let half_diff = (x -. current) /. 2.0 in
      if Float.abs half_diff < Float.epsilon then
        Seq.Cons (current, fun () -> Seq.Nil)
      else Seq.Cons (current, go (current +. half_diff) (limit - 1))
  in
  go dest 15 ()
