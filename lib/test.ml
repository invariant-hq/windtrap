(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* ───── Types ───── *)

type pos = string * int * int * int

type t =
  | Test of {
      name : string;
      fn : unit -> unit;
      pos : pos option;
      tags : Tag.t;
      timeout : float option;
      retries : int;
    }
  | Group of {
      name : string;
      children : t list;
      pos : pos option;
      tags : Tag.t;
      setup : (unit -> unit) option;
      teardown : (unit -> unit) option;
    }

(* ───── Test Creation ───── *)

(* Wraps [fn] to discard its return value, so users can write tests returning
   any type without needing to add [ignore] themselves. *)
let test ?pos ?(tags = Tag.empty) ?timeout ?(retries = 0) name fn =
  Test { name; fn = (fun () -> ignore (fn ())); pos; tags; timeout; retries }

let group ?pos ?(tags = Tag.empty) ?setup ?teardown name children =
  Group { name; children; pos; tags; setup; teardown }

let slow ?pos ?(tags = Tag.empty) ?timeout ?retries name fn =
  test ?pos ~tags:(Tag.merge tags (Tag.speed Slow)) ?timeout ?retries name fn

let bracket ?pos ?(tags = Tag.empty) ?timeout ?(retries = 0) ~setup ~teardown
    name fn =
  let wrapped () =
    let resource = setup () in
    Fun.protect ~finally:(fun () -> teardown resource) (fun () -> fn resource)
  in
  Test
    { name; fn = (fun () -> ignore (wrapped ())); pos; tags; timeout; retries }

(* ───── Test Traversal ───── *)

(* Visitor events for depth-first traversal. This decouples the runner from the
   tree representation: consumers see a flat stream of enter/leave/case events
   without needing to pattern-match on [t] directly. *)
type visit =
  | Case of {
      name : string;
      fn : unit -> unit;
      pos : pos option;
      tags : Tag.t;
      timeout : float option;
      retries : int;
    }
  | Enter_group of {
      name : string;
      pos : pos option;
      tags : Tag.t;
      setup : (unit -> unit) option;
    }
  | Leave_group of { name : string; teardown : (unit -> unit) option }

let rec fold f acc t =
  match t with
  | Test { name; fn; pos; tags; timeout; retries } ->
      f acc (Case { name; fn; pos; tags; timeout; retries })
  | Group { name; children; pos; tags; setup; teardown } ->
      let acc = f acc (Enter_group { name; pos; tags; setup }) in
      let acc = List.fold_left (fold f) acc children in
      f acc (Leave_group { name; teardown })

let iter f t = fold (fun () v -> f v) () t
