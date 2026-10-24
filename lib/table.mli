(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tables: properties over a finite list of rows.

    A table is {!Run.property} over [Gen.of_list rows], whose domain is finite
    and so checked whole: every row runs, in order, and every row refuted is
    reported, none shrunk. A row is identified by its printed value, which
    [--failed] records; a label only shows beside it. Each row is a {!Run.row},
    so it settles its baseline corrections alone. *)

val shown_bytes : int
(** [shown_bytes] is the number of bytes of a row's printing that shows in its
    tag, [80]: a longer printing is cut in the middle ({!Text.elide_middle}). *)

val check :
  ?loc:Loc.t ->
  ?label:('a -> string) ->
  'a Testable.t ->
  'a list ->
  ('a -> unit) ->
  unit
(** [check w rows fn] is the body of the test that {!cases} declares, [loc]
    being its declaration site. It prints every row with [w], once, and applies
    [label] to each, an empty label showing as none.
    - A table with no row fails with a message that says it checks nothing.
    - Two rows that print alike, byte for byte, are duplicates: the table fails
      with a message that names their positions and runs no row.
    - Under [--failed] the table runs only the rows that {!Run.recorded} names.
      A recorded row that no row prints as, and a recorded counterexample, are
      stale: the table fails with a message that names them and runs no row.

    Then the rows run as {!Run.property} runs a domain checked whole, with
    [count] the number of rows, so [--prop-count] changes nothing that runs. Row
    [i] runs [fn] inside {!Run.row}, with the identity [printed.(i)] and the tag
    [row I of N, LABEL: VALUE], the printing cut to one line of at most
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
  'a Testable.t ->
  'a list ->
  ('a -> unit) ->
  Test_tree.t
(** [cases name w rows fn] is the {!Test_tree.test} [name] whose body is
    [check w rows fn] (see {!Windtrap.cases}). *)
