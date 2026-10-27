(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tables: properties over a finite list of rows.

    A table is {!Run.property} over [Gen.of_list rows], each row a {!Run.row}.
    The facade's [cases] states its contract. *)

val shown_bytes : int
(** [shown_bytes] is the number of bytes of a row's id that shows in its tag,
    [80]: a longer id is cut in the middle ({!Text.elide_middle}). *)

val check :
  ?loc:Loc.t ->
  ?label:('a -> string) ->
  ('a -> string) ->
  'a list ->
  ('a -> unit) ->
  unit
(** [check id rows fn] is the body of the test that {!cases} declares, [loc]
    being its declaration site. A table with no row is vacuous, with a
    {!Failure.Vacuous} failure. Under [--failed] the table runs only the rows
    that {!Run.recorded} names. A recorded counterexample is stale too, and a
    table with a stale record runs no row.

    Then the rows run as {!Run.property} runs a domain checked whole, with
    [count] the number of rows, so [--prop-count] changes nothing that runs. Row
    [i] runs [fn] inside {!Run.row}, with the row's [id] as its identity and the
    tag [row I of N, LABEL: ID], the id cut to one line of at most
    {!shown_bytes} and the label to at most [60]. A failure of the property is
    that of {!Run.property} with each refuted row marked [table], with its
    [label]. *)

val cases :
  ?__POS__:Loc.pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  ?label:('a -> string) ->
  string ->
  ('a -> string) ->
  'a list ->
  ('a -> unit) ->
  Test_tree.t
(** [cases name id rows fn] is the {!Test_tree.test} [name] whose body is
    [check id rows fn] (see {!Windtrap.cases}). *)
