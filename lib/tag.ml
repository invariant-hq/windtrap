(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type speed = Quick | Slow

module String_set = Set.Make (String)

type t = { speed : speed option; labels : String_set.t }

let empty = { speed = None; labels = String_set.empty }
let speed s = { empty with speed = Some s }
let add_label label t = { t with labels = String_set.add label t.labels }
let labels ls = { empty with labels = String_set.of_list ls }

(* Innermost (child) speed wins, supporting inheritance from parent groups *)
let merge a b =
  {
    speed = (match b.speed with Some _ -> b.speed | None -> a.speed);
    labels = String_set.union a.labels b.labels;
  }

let get_speed t = Option.value ~default:Quick t.speed
let get_labels t = String_set.elements t.labels
let has_label label t = String_set.mem label t.labels

(* ───── Tag Predicates ───── *)

type predicate = { required : String_set.t; dropped : String_set.t }

(* By default, tests tagged "disabled" are skipped without requiring
   an explicit --drop-tag flag from the user. *)
let initial_predicate =
  { required = String_set.empty; dropped = String_set.singleton "disabled" }

let empty_predicate =
  { required = String_set.empty; dropped = String_set.empty }

let drop_tag tag pred =
  {
    dropped = String_set.add tag pred.dropped;
    required = String_set.remove tag pred.required;
  }

let require_tag tag pred =
  {
    dropped = String_set.remove tag pred.dropped;
    required = String_set.add tag pred.required;
  }

(* A test is disabled if it lacks any required tag OR has any dropped tag *)
let is_disabled_by_predicate pred t =
  let tags = t.labels in
  (not (String_set.is_empty pred.required))
  && not (String_set.subset pred.required tags)
  || not (String_set.is_empty (String_set.inter pred.dropped tags))

let filter_predicate pred t =
  if is_disabled_by_predicate pred t then `Skip else `Run
