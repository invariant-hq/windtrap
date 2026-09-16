(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Source locations for failures and test declarations.

    A location comes from an explicit {!type:pos} (the [~__POS__] the caller
    passed on) or, failing that, from a best-effort walk of the call stack
    ({!capture}); {!resolve} is that rule. When neither yields one there is
    none: a report without a location beats one with a wrong location. Capture
    reads debug information, so a program built without [-g] gets no automatic
    location. *)

(** {1:types Types} *)

type pos = string * int * int * int
(** The type of [__POS__]: file, line, start column, end column. *)

type t = { file : string; line : int; column : int }
(** The type for source locations. [file] is as recorded at compile time
    (usually relative to the project root); [line] is 1-based; [column] is
    0-based. *)

(** {1:constructors Constructors} *)

val of_pos : pos -> t
(** [of_pos p] is the location of [p], keeping its start column. *)

val capture : unit -> t option
(** [capture ()] is the location of the first call-stack slot, inlined slots
    included, whose compilation unit is neither windtrap's nor the standard
    library's; [None] when the walk reaches a {!delimit} frame first, when no
    eligible slot has a location, or without debug information. Bounded and
    cheap enough to call at every failure construction. *)

val delimit : (unit -> 'a) -> 'a
(** [delimit fn] is [fn ()] under a capture delimiter: a {!capture} during [fn]
    never walks past this call's frame. The runner wraps every user callback in
    it, so an assertion in tail position reports no location rather than the
    runner's caller. Never inlined; the call to [fn] is not a tail call. Raises
    whatever [fn] raises, backtrace preserved. *)

val resolve : ?__POS__:pos -> unit -> t option
(** [resolve ?__POS__ ()] is [Some (of_pos p)] when [__POS__] is [Some p], and
    [capture ()] otherwise. *)

val own_unit : string -> bool
(** [own_unit defname] is [true] iff the compilation unit of [defname], a
    {!Printexc.Slot} debug name such as ["Windtrap__Check.raises"], is one of
    windtrap's own: the [Windtrap] alias unit, a [Windtrap__]-wrapped module, or
    the coverage runtime. Whole unit names are matched. *)

(** {1:observers Observers} *)

val to_string : t -> string
(** [to_string loc] is [loc] spelled ["file:line"], the form reports print. *)

val equal : t -> t -> bool
(** [equal a b] is structural equality, column included: two checks on one line
    are two distinct sites. *)
