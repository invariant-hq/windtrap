(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Source locations of failures and test declarations.

    A location comes from an explicit {!type-pos}, the [~__POS__] that a caller
    passed on, or else from a best-effort walk of the call stack
    ({!val-capture}). {!resolve} is that rule. When neither gives one there is
    none. The walk reads debug information, so a program built without [-g] gets
    a location from an explicit position only. *)

(** {1:locations Locations} *)

type pos = string * int * int * int
(** The type for [__POS__] values: file, line, start column, end column. *)

type t = {
  file : string;
      (** The file as recorded at compile time, which under dune is relative to
          the project root, as ["test/test_users.ml"]. Such a path does not open
          from the directory in which [dune runtest] runs a suite, so a client
          that reads the file must try it under the project root first. *)
  line : int;  (** The line, counted from one. *)
  column : int;  (** The start column, counted from zero. *)
}
(** The type for source locations. *)

val of_pos : pos -> t
(** [of_pos p] is the location of [p]: its file, its line and its start column.
*)

val to_string : t -> string
(** [to_string loc] is [loc] spelled [file:line], with [file] as recorded and
    without the column. *)

(** {1:capturing Capturing} *)

val capture : unit -> t option
(** [capture ()] is the location of the innermost call-stack slot, inlined slots
    included, whose compilation unit is neither windtrap's ({!own_unit}) nor the
    standard library's ([Stdlib], a name that starts with [Stdlib__] or with
    [Camlinternal]).

    It is [None] when the walk reaches a {!delimit} frame first, when no
    eligible slot has a location, and without debug information. The walk reads
    the 24 innermost entries of the call stack, so a user frame beyond them
    gives [None].

    [capture] reads the call stack of its caller and never the backtrace of the
    last exception, so an exception handled earlier does not disturb it. A
    client must call it within the call that it locates. *)

val delimit : (unit -> 'a) -> 'a
(** [delimit fn] is [fn ()] under a capture delimiter, so a {!val-capture}
    during [fn] never walks past the frame of this call. [delimit] is never
    inlined and its call to [fn] is not a tail call, so the frame stays on the
    stack for as long as [fn] runs. What [fn] raises is raised again with its
    backtrace, whatever the exception is.

    An assertion in tail position has replaced the frame of the function that
    called it, so the walk meets no frame of the user before the delimiter. It
    then has no location, where it would otherwise get that of the caller of the
    runner. A client that calls user code must call it under [delimit]. *)

val resolve : ?__POS__:pos -> unit -> t option
(** [resolve ?__POS__ ()] is [Some (of_pos p)] when [__POS__] is [Some p], and
    [capture ()] otherwise. An explicit position reads no call stack, so it
    needs no debug information. It is the location rule of every function that
    takes a [?__POS__]. *)

val own_unit : string -> bool
(** [own_unit defname] is [true] iff the compilation unit of [defname] is one of
    windtrap's. [defname] is a debug name as [Printexc.Slot.name] gives it, such
    as ["Windtrap__Check.raises"]. Its unit is the text before its first ['.'],
    or the whole name when it has none.

    Windtrap's units are ["Windtrap"], the names that start with ["Windtrap__"],
    and those of the instrumentation runtime: ["Windtrap_runtime"] and the names
    that start with ["Windtrap_runtime__"]. Whole unit names are compared, so a
    user library named [Windtrap_helpers] is not windtrap's. *)

(**/**)

(* Exported for the unit suite, and read by nothing else. [equal a b] is [true]
   iff [a] and [b] have the same file, the same line and the same column. *)

val equal : t -> t -> bool

(**/**)
