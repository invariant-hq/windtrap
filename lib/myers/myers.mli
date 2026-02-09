(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Minimal polymorphic Myers diff implementation. *)

module type Equal = sig
  type t

  val equal : t -> t -> bool
end

module Line : sig
  type 'a t = Delete of 'a | Insert of 'a | Keep of 'a
end

val compute :
  (module Equal with type t = 'a) -> 'a list -> 'a list -> 'a Line.t list
(** [compute (module E) before after] returns a shortest edit script from
    [before] to [after]. *)

val diff :
  ?context:int ->
  ?expected_label:string ->
  ?actual_label:string ->
  string ->
  string ->
  string
(** [diff expected actual] renders a unified diff for text inputs. *)

val print_diff :
  ?context:int ->
  ?expected_label:string ->
  ?actual_label:string ->
  string ->
  string ->
  unit
(** [print_diff expected actual] writes {!diff} to stdout. *)
