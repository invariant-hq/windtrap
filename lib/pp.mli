(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Formatting helpers and explicit ANSI styling.

    A small [Fmt]-shaped layer over {!Stdlib.Format}: short aliases, composable
    printers, and styling that is explicit at every call site. Failure payloads
    store plain strings rendered with {!to_string}; only renderers call
    {!styled_string}, passing their own [~ansi] decision; no global state
    controls styling. No printer here writes to a standard channel: a sink is
    always a parameter. *)

(** {1:types Types} *)

type 'a t = Format.formatter -> 'a -> unit
(** The type for printers of values of type ['a]. *)

type style = [ `Bold | `Faint | `Red | `Green | `Yellow | `Cyan | `White ]
(** The type for ANSI styles understood by {!styled_string}. *)

(** {1:output Output} *)

val abstract : string
(** [abstract] is ["<abstract>"], what a value with no rendering prints as:
    {!Testable.of_equal}'s witness and the fallback of every verb taking an
    optional printer. *)

val str : ('a, Format.formatter, unit, string) format4 -> 'a
(** [str fmt ...] formats to a string. Equivalent to {!Format.asprintf}. *)

val pf : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
(** [pf ppf fmt ...] formats to [ppf]. Equivalent to {!Format.fprintf}. *)

val flush : Format.formatter -> unit -> unit
(** [flush ppf ()] flushes [ppf]. *)

val to_string : 'a t -> 'a -> string
(** [to_string pp v] is [v] formatted with [pp] as a string; it contains no
    escape codes unless [pp] itself emits them. *)

(** {1:printers Printers} *)

val string : string t
(** [string] formats a string verbatim. *)

val int : int t
val int32 : int32 t
val int64 : int64 t

val float_exact : float t
(** [float_exact] is the shortest decimal rendering that round-trips to the
    exact bits (15 significant digits, else 16, else 17), so a printed float
    pastes back as the same double. Non-finite values render as [nan], [inf],
    [-inf]; the sign of zero survives. The only float printer here. *)

val bool : bool t

(** {1:combinators Combinators} *)

val list : ?sep:unit t -> 'a t -> 'a list t
(** [list ?sep pp] formats list elements with [pp], separated by [sep] (default
    {!semi}), inside a compacting box. *)

val array : ?sep:unit t -> 'a t -> 'a array t
(** [array ?sep pp] is like {!list} for arrays. *)

val option : 'a t -> 'a option t
(** [option pp] formats [None] as ["None"] and [Some v] as ["Some <v>"]. *)

val result : ok:'a t -> error:'e t -> ('a, 'e) result t
(** [result ~ok ~error] formats [Ok v] as ["Ok <v>"] and [Error e] as
    ["Error <e>"]. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
(** [pair pp_a pp_b] formats a pair as ["(<a>, <b>)"]. *)

val brackets : 'a t -> 'a t
(** [brackets pp] wraps the output of [pp] in square brackets. *)

val semi : unit t
(** [semi] formats ["; "] with a break hint. *)

(** {1:styling Styling}

    Styles do not nest: the reset that closes one style also ends any enclosing
    style. Styled output is plain bytes, not width-transparent; a caller laying
    out columns measures with {!Text.strip_ansi} and {!Text.length_utf8}. *)

val styled_string : ansi:bool -> style -> string -> string
(** [styled_string ~ansi s str] is [str] wrapped in the escape codes for [s]
    when [ansi] is [true], and [str] unchanged otherwise. An empty [str] is
    returned bare under [ansi] too. *)
