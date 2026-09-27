(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Printers over [Format], and the styles of the report.

    A printer is a {!type-t}. {!str}, {!pf} and {!to_string} format with one.

    The module holds what the library prints with and is no general printing
    toolbox, so an addition must come with its caller. No value writes to a
    standard channel, because the sink is always an argument, and no value
    writes an escape sequence. *)

(** {1:types Types} *)

type 'a t = Format.formatter -> 'a -> unit
(** The type for printers of ['a] values. *)

type style =
  [ `Bold
  | `Faint  (** Faint, also called dim. *)
  | `Red
  | `Green
  | `Yellow
  | `Bold_red
  | `Bold_green ]
(** The type for the styles of a report's text ({!Report_sections.span}). Bold
    with a colour is a style of its own because styles do not nest: the reset
    that closes one style closes every style. *)

(** {1:output Formatting} *)

val str : ('a, Format.formatter, unit, string) format4 -> 'a
(** [str fmt ...] is [Format.asprintf fmt ...]. *)

val pf : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
(** [pf ppf fmt ...] is [Format.fprintf ppf fmt ...]. *)

val flush : Format.formatter -> unit -> unit
(** [flush ppf ()] is [Format.pp_print_flush ppf ()]. *)

val to_string : 'a t -> 'a -> string
(** [to_string pp v] is [v] formatted with [pp], as a string. A printer with
    break hints, as {!list} is, breaks a long value at the 78 columns of
    [Format.asprintf]. *)

(** {1:printers Printers}

    The printers of the base types, and the placeholder for a value that has
    none. *)

val abstract : string
(** [abstract] is ["<abstract>"], the rendering of a value that has no printer:
    every value under {!Testable.of_equal}, and the value that a verb of
    {!Check} rejects when it was given no printer. A producer must use
    [abstract] and never spell the placeholder itself. *)

val string : string t
(** [string] formats a string verbatim, without quotes and without escapes. *)

val int : int t
(** [int] formats an [int] in decimal. *)

val int32 : int32 t
(** [int32] formats an [int32] in decimal, without the [l] of a literal. *)

val int64 : int64 t
(** [int64] formats an [int64] in decimal, without the [L] of a literal. *)

val decimal : float t
(** [decimal] formats a finite float as the shortest decimal without an exponent
    that reads back to it, so a number prints as a user configured it: [80.] as
    [80], [0.5] as [0.5] and [99.99999] as [99.99999]. It keeps at most 17
    places after the point, so a value below [0.1] can print rounded. *)

val float_exact : float t
(** [float_exact] formats a float as the shortest decimal that reads back to the
    same bits. A whole value keeps its point, and a rendering with an exponent
    gets none, so [1.] prints as [1.], [-0.] as [-0.] and [1e300] as [1e+300].
    The values that no literal denotes print as the [Stdlib] values that do:
    [infinity], [neg_infinity], and [nan] for every NaN, whatever its sign and
    payload. *)

val bool : bool t
(** [bool] formats [true] and [false]. *)

(** {1:combinators Combinators} *)

val list : ?sep:unit t -> 'a t -> 'a list t
(** [list ?sep pp] formats the elements of a list with [pp], separated by [sep],
    which defaults to {!semi}. It prints no brackets. The elements are in one
    box, so a long list wraps at the margin. *)

val array : ?sep:unit t -> 'a t -> 'a array t
(** [array ?sep pp] is {!list} for arrays. *)

val option : 'a t -> 'a option t
(** [option pp] formats [None] as [None], and [Some v] as [Some], a space and
    [v] under [pp]. [v] gets no parentheses, so [Some (Some 1)] prints as
    [Some Some 1]. *)

val result : ok:'a t -> error:'e t -> ('a, 'e) result t
(** [result ~ok ~error] is {!option} for results, with [Ok v] under [ok] and
    [Error e] under [error]. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
(** [pair a b] formats a pair as [(x, y)], with [x] under [a] and [y] under [b].
*)

val brackets : 'a t -> 'a t
(** [brackets pp] formats a value as [pp] does, between [\[] and [\]]. *)

val semi : unit t
(** [semi] formats [;] and a break hint. *)
