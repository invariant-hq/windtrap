(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Formatting helpers and explicit ANSI styling.

    A minimal substitute for [Fmt] built on {!Stdlib.Format}: short aliases for
    common formatting operations, composable printers, and ANSI styling that is
    explicit at every call site.

    Styling discipline: failure payloads store plain strings rendered with
    {!to_string}; only renderers call {!styled} or {!styled_string}, passing
    their own [~ansi] decision (derived from the environment's color detection).
    No global state controls styling, so a printer used to build failure data
    can never leak escape codes into it. *)

(** {1:types Types} *)

type 'a t = Format.formatter -> 'a -> unit
(** The type for printers of values of type ['a]. *)

type style = [ `Bold | `Faint | `Red | `Green | `Yellow | `Cyan | `White ]
(** The type for ANSI styles understood by {!styled} and {!styled_string}. *)

(** {1:output Output} *)

val str : ('a, Format.formatter, unit, string) format4 -> 'a
(** [str fmt ...] formats to a string. Equivalent to {!Format.asprintf}. *)

val pf : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
(** [pf ppf fmt ...] formats to [ppf]. Equivalent to {!Format.fprintf}. *)

val pr : ('a, Format.formatter, unit) format -> 'a
(** [pr fmt ...] formats to standard output. Equivalent to {!Format.printf}. *)

val epr : ('a, Format.formatter, unit) format -> 'a
(** [epr fmt ...] formats to standard error. Equivalent to {!Format.eprintf}. *)

val flush : Format.formatter -> unit -> unit
(** [flush ppf ()] flushes [ppf]. *)

val to_string : 'a t -> 'a -> string
(** [to_string pp v] is [v] formatted with [pp] as a string. This is the
    rendering used for failure payloads: it never contains escape codes unless
    [pp] itself emits them. *)

(** {1:printers Printers} *)

val string : string t
(** [string] formats a string verbatim. *)

val int : int t
val int32 : int32 t
val int64 : int64 t

val float : float t
(** [float] formats with {!Format.pp_print_float} (e.g. [1.] prints as ["1."]).
*)

val bool : bool t
val char : char t

(** {1:combinators Combinators} *)

val list : ?sep:unit t -> 'a t -> 'a list t
(** [list ?sep pp] formats list elements with [pp], separated by [sep], inside a
    compacting box (long lists wrap at the margin). [sep] defaults to {!semi}.
*)

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

val comma : unit t
(** [comma] formats [", "] with a break hint. *)

(** {1:styling Styling}

    Renderer-side only. Both functions take the ANSI decision explicitly; with
    [~ansi:false] they are the identity, so the same rendering code serves color
    and monochrome transports.

    Styles do not nest: the reset that closes an inner [styled] also ends any
    enclosing style. Style sibling fragments, not containers. *)

val styled : ansi:bool -> style -> 'a t -> 'a t
(** [styled ~ansi s pp] is [pp] wrapped in the escape codes for [s] when [ansi]
    is [true], and [pp] unchanged otherwise. Escape codes are emitted as
    zero-width pretty-printing tokens and do not perturb line breaking. *)

val styled_string : ansi:bool -> style -> string -> string
(** [styled_string ~ansi s str] is [str] wrapped in the escape codes for [s]
    when [ansi] is [true], and [str] unchanged otherwise. For building styled
    strings outside a formatter. *)
