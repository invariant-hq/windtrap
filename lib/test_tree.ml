(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   The tree shape and traversal derive from windtrap v1's lib/test.ml,
   rebuilt without group hooks, with bracket kept as data, and with
   declaration-file capture for snapshot scoping.
  ---------------------------------------------------------------------------*)

type body =
  | Body of (unit -> unit)
  | Bracket : {
      setup : unit -> 'r;
      body : 'r -> unit;
      teardown : 'r -> unit;
    }
      -> body

type xfail = { reason : string option }

type t =
  | Test of {
      name : string;
      body : body;
      loc : Loc.t option;
      file : string option;
      tags : Tag.t;
      timeout : float option;
      retries : int;
      focused : bool;
      xfail : xfail option;
    }
  | Group of {
      name : string;
      children : t list;
      loc : Loc.t option;
      tags : Tag.t;
      focused : bool;
      xfail : xfail option;
    }

(* ───── Declaration ───── *)

let check_timeout = function
  | None -> ()
  | Some seconds ->
      if (not (Float.is_finite seconds)) || seconds <= 0. then
        invalid_arg "windtrap: timeout must be finite and positive"

let check_retries retries =
  if retries < 0 then invalid_arg "windtrap: retries must be non-negative"

(* Declaration location and file. [?pos] wins; the backtrace fallback is
   best-effort and can attribute to the wrong frame when the constructor
   call was reached through tail calls (documented in the interface). The
   [let] keeps the capture out of tail position. *)
let declared_at pos =
  let loc = Loc.resolve ?pos () in
  let file = Option.map (fun (l : Loc.t) -> l.Loc.file) loc in
  (loc, file)

let make_test ?pos ?(tags = []) ?timeout ?(retries = 0) ~focused name body =
  check_timeout timeout;
  check_retries retries;
  let loc, file = declared_at pos in
  Test
    {
      name;
      body;
      loc;
      file;
      tags = Tag.of_list tags;
      timeout;
      retries;
      focused;
      xfail = None;
    }

let test ?pos ?tags ?timeout ?retries name fn =
  make_test ?pos ?tags ?timeout ?retries ~focused:false name (Body fn)

let ftest ?pos ?tags ?timeout ?retries name fn =
  make_test ?pos ?tags ?timeout ?retries ~focused:true name (Body fn)

let slow ?pos ?(tags = []) ?timeout ?retries name fn =
  make_test ?pos ~tags:(Tag.slow :: tags) ?timeout ?retries ~focused:false name
    (Body fn)

let bracket ?pos ?tags ?timeout ?retries ~setup ~teardown name fn =
  make_test ?pos ?tags ?timeout ?retries ~focused:false name
    (Bracket { setup; body = fn; teardown })

let make_group ?pos ?(tags = []) ~focused name children =
  (* Groups record a location (for focus sites) but no declaration file:
     snapshot scoping reads the leaf test's own file. *)
  let loc = Loc.resolve ?pos () in
  Group { name; children; loc; tags = Tag.of_list tags; focused; xfail = None }

let group ?pos ?tags name children =
  make_group ?pos ?tags ~focused:false name children

let fgroup ?pos ?tags name children =
  make_group ?pos ?tags ~focused:true name children

let cases ?pos ?(tags = []) ?name:name_of base inputs fn =
  let loc, file = declared_at pos in
  let child index input =
    let child_name =
      match name_of with
      | Some render -> render input
      | None -> base ^ "." ^ string_of_int index
    in
    Test
      {
        name = child_name;
        body = Body (fun () -> fn input);
        loc;
        file;
        tags = Tag.empty;
        timeout = None;
        retries = 0;
        focused = false;
        xfail = None;
      }
  in
  Group
    {
      name = base;
      children = List.mapi child inputs;
      loc;
      tags = Tag.of_list tags;
      focused = false;
      xfail = None;
    }

(* ───── Expected failures (amendment B12) ───── *)

let xfail ?reason = function
  | Test t -> Test { t with xfail = Some { reason } }
  | Group g -> Group { g with xfail = Some { reason } }

(* ───── Focus ───── *)

let rec node_has_focus = function
  | Test { focused; _ } -> focused
  | Group { focused; children; _ } ->
      focused || List.exists node_has_focus children

let has_focus tests = List.exists node_has_focus tests

let focus_sites tests =
  let rec node acc = function
    | Test { focused; loc; _ } -> if focused then (`Ftest, loc) :: acc else acc
    | Group { focused; loc; children; _ } ->
        let acc = if focused then (`Fgroup, loc) :: acc else acc in
        List.fold_left node acc children
  in
  List.rev (List.fold_left node [] tests)

(* ───── Flattening ───── *)

type case = {
  path : string list;
  body : body;
  loc : Loc.t option;
  file : string option;
  tags : Tag.t;
  focused : bool;
  timeout : float option;
  retries : int;
  xfail : xfail option;
}

(* Innermost annotation wins: a node's own [xfail] overrides the inherited
   one, so re-marking a test inside an [xfail] group refines the reason. *)
let own_or_inherited own inherited =
  match own with Some _ -> own | None -> inherited

let flatten tests =
  let rec node ~rev_groups ~tags:inherited ~focused:ancestor ~xfail acc =
    function
    | Test t ->
        {
          path = List.rev (t.name :: rev_groups);
          body = t.body;
          loc = t.loc;
          file = t.file;
          tags = Tag.union inherited t.tags;
          focused = ancestor || t.focused;
          timeout = t.timeout;
          retries = t.retries;
          xfail = own_or_inherited t.xfail xfail;
        }
        :: acc
    | Group g ->
        let rev_groups = g.name :: rev_groups in
        let tags = Tag.union inherited g.tags in
        let focused = ancestor || g.focused in
        let xfail = own_or_inherited g.xfail xfail in
        List.fold_left (node ~rev_groups ~tags ~focused ~xfail) acc g.children
  in
  List.rev
    (List.fold_left
       (node ~rev_groups:[] ~tags:Tag.empty ~focused:false ~xfail:None)
       [] tests)

let path_to_string path = String.concat " › " path
