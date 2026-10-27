(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type error = { error : float; tolerance : float }

(* The comparisons of a tolerance that [observed] keeps, on each domain: the
   one whose error is the largest share of its tolerance. *)
let observer : error option ref option Domain.DLS.key =
  Domain.DLS.new_key (fun () -> None)

let observing () = Option.is_some (Domain.DLS.get observer)
let ratio e = if e.error = 0. then 0. else e.error /. e.tolerance

let observe error tolerance =
  match Domain.DLS.get observer with
  | None -> ()
  | Some worst -> (
      let e = { error; tolerance } in
      match !worst with
      | Some w when ratio w > ratio e || (ratio w = ratio e && w.error >= error)
        ->
          ()
      | Some _ | None -> worst := Some e)

let observed fn =
  let enclosing = Domain.DLS.get observer in
  let worst = ref None in
  Domain.DLS.set observer (Some worst);
  match fn () with
  | v ->
      Domain.DLS.set observer enclosing;
      (v, !worst)
  | exception exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      Domain.DLS.set observer enclosing;
      Printexc.raise_with_backtrace exn backtrace
