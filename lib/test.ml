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
      focused : bool;
    }
  | Group of {
      name : string;
      children : t list;
      pos : pos option;
      tags : Tag.t;
      setup : (unit -> unit) option;
      teardown : (unit -> unit) option;
      before_each : (unit -> unit) option;
      after_each : (unit -> unit) option;
      focused : bool;
    }

(* ───── Test Creation ───── *)

(* Wraps [fn] to discard its return value, so users can write tests returning
   any type without needing to add [ignore] themselves. *)
let test ?pos ?(tags = Tag.empty) ?timeout ?(retries = 0) name fn =
  Test
    {
      name;
      fn = (fun () -> ignore (fn ()));
      pos;
      tags;
      timeout;
      retries;
      focused = false;
    }

let ftest ?pos ?tags ?timeout ?retries name fn =
  match test ?pos ?tags ?timeout ?retries name fn with
  | Test t -> Test { t with focused = true }
  | Group _ -> assert false

let group ?pos ?(tags = Tag.empty) ?setup ?teardown ?before_each ?after_each
    name children =
  Group
    {
      name;
      children;
      pos;
      tags;
      setup;
      teardown;
      before_each;
      after_each;
      focused = false;
    }

let fgroup ?pos ?tags ?setup ?teardown ?before_each ?after_each name children =
  match
    group ?pos ?tags ?setup ?teardown ?before_each ?after_each name children
  with
  | Group g -> Group { g with focused = true }
  | Test _ -> assert false

let slow ?pos ?(tags = Tag.empty) ?timeout ?retries name fn =
  test ?pos ~tags:(Tag.merge tags (Tag.speed Slow)) ?timeout ?retries name fn

let bracket ?pos ?(tags = Tag.empty) ?timeout ?(retries = 0) ~setup ~teardown
    name fn =
  let wrapped () =
    let resource = setup () in
    Fun.protect ~finally:(fun () -> teardown resource) (fun () -> fn resource)
  in
  Test
    {
      name;
      fn = (fun () -> ignore (wrapped ()));
      pos;
      tags;
      timeout;
      retries;
      focused = false;
    }

let rec has_focused_one = function
  | Test { focused; _ } -> focused
  | Group { focused; children; _ } ->
      focused || List.exists has_focused_one children

let has_focused tests = List.exists has_focused_one tests

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
      focused : bool;
    }
  | Enter_group of {
      name : string;
      pos : pos option;
      tags : Tag.t;
      setup : (unit -> unit) option;
      before_each : (unit -> unit) option;
      after_each : (unit -> unit) option;
      focused : bool;
    }
  | Leave_group of { name : string; teardown : (unit -> unit) option }

let rec fold f acc t =
  match t with
  | Test { name; fn; pos; tags; timeout; retries; focused } ->
      f acc (Case { name; fn; pos; tags; timeout; retries; focused })
  | Group
      {
        name;
        children;
        pos;
        tags;
        setup;
        teardown;
        before_each;
        after_each;
        focused;
      } ->
      let acc =
        f acc
          (Enter_group
             { name; pos; tags; setup; before_each; after_each; focused })
      in
      let acc = List.fold_left (fold f) acc children in
      f acc (Leave_group { name; teardown })

let iter f t = fold (fun () v -> f v) () t
