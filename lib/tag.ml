(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Adapted from windtrap 0.1's lib/tag.ml. v3 drops the separate speed
   type: "slow" is an ordinary tag, pre-applied by the [slow] constructor
   and dropped like any other. *)

module String_set = Set.Make (String)

type t = String_set.t

let empty = String_set.empty
let of_list = String_set.of_list
let union = String_set.union
let mem = String_set.mem

(* Well-known tags *)

let slow = "slow"

(* Selection predicates *)

type predicate = { required : String_set.t; dropped : String_set.t }

let any = { required = String_set.empty; dropped = String_set.empty }

let require name p =
  {
    required = String_set.add name p.required;
    dropped = String_set.remove name p.dropped;
  }

let drop name p =
  {
    required = String_set.remove name p.required;
    dropped = String_set.add name p.dropped;
  }

let accepts p tags =
  String_set.subset p.required tags
  && String_set.is_empty (String_set.inter p.dropped tags)
