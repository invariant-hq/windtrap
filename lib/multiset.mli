(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Multisets of list elements under a witness's equality.

    {!Testable.slist} compares two lists with {!difference} and, when they
    differ, offers what is missing and what is extra to the {!explained} that
    {!Check.equal} runs, so a failure shows the difference and not two whole
    lists.

    @canonical Windtrap.Private.Multiset *)

val difference :
  equal:('a -> 'a -> bool) ->
  compare:('a -> 'a -> int) ->
  'a list ->
  'a list ->
  'a list * 'a list
(** [difference ~equal ~compare expected actual] is [(missing, extra)]:
    [missing] the elements of [expected] paired with no element of [actual], and
    [extra] the elements of [actual] paired with none of [expected], each in the
    order of [compare]. Every pair is of two elements equal under [equal],
    applied to the [expected] element first. The two lists are equal as
    multisets iff both are empty.

    The pairs are sought in the order of [compare], which both lists are sorted
    by: the next elements of the two sides are paired when [equal] holds, an
    element that [compare] puts before the other side's is set aside, and the
    elements on which [compare] ties are paired among themselves. The elements
    set aside are then paired among themselves, as an equality looser than the
    order, a tolerance's, can equate two elements that the order puts apart. So
    with an equality that is an equivalence, two lists differ iff they are
    unequal multisets, and [compare] needs only to put equal elements next to
    each other: it may be coarser than [equal], as an order on one field of a
    record is. *)

type explanation = {
  missing : string;  (** {!difference}'s [missing], printed as a list. *)
  missing_count : int;  (** The number of elements of [missing]. *)
  extra : string;  (** {!difference}'s [extra], printed as a list. *)
  extra_count : int;  (** The number of elements of [extra]. *)
}
(** The type for the difference of two lists compared as multisets. *)

val offer : 'a -> 'a -> explanation Lazy.t -> unit
(** [offer expected actual e] gives [e] to the innermost {!explained} on the
    calling domain when its two sides are [expected] and [actual] themselves,
    physically, so that the comparison of a part of the values, as an [slist]
    inside a [list], explains nothing. It does nothing otherwise. *)

val explained : 'a -> 'a -> (unit -> 'b) -> 'b * explanation option
(** [explained expected actual fn] is [fn ()] with the explanation offered for
    [expected] and [actual] while it ran, if any. What [fn] raises is raised
    again. *)
