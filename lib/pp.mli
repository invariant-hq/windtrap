(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Lightweight pretty-printing with ANSI styling.

    A minimal replacement for {!Fmt}, using only {!Stdlib.Format}. Provides
    short aliases for common format operations and composable formatters with
    optional ANSI color support. *)

(** {1 Types} *)

type 'a t = Format.formatter -> 'a -> unit
(** A formatter for values of type ['a]. *)

type style = [ `Bold | `Faint | `Green | `Red | `Yellow | `Cyan | `White ]
(** Supported ANSI styles. *)

(** {1 Style Configuration} *)

val use_ansi_stdout : bool ref
(** Whether to emit ANSI escape codes when printing to [stdout]. Default:
    [false]. Set by {!Env.setup_color}. *)

val use_ansi_stderr : bool ref
(** Whether to emit ANSI escape codes when printing to [stderr]. Default:
    [false]. Set by {!Env.setup_color}. *)

(** {1 Basic Output} *)

val str : ('a, Format.formatter, unit, string) format4 -> 'a
(** Format to string. Equivalent to {!Format.asprintf}. *)

val pf : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
(** Print to a formatter. Equivalent to {!Format.fprintf}. *)

val pr : ('a, Format.formatter, unit) format -> 'a
(** Print to stdout. Equivalent to {!Format.printf}. *)

val epr : ('a, Format.formatter, unit) format -> 'a
(** Print to stderr. Equivalent to {!Format.eprintf}. *)

val flush : Format.formatter -> unit -> unit
(** [flush ppf ()] flushes the formatter [ppf]. *)

(** {1 Styled Output} *)

val styled : style -> 'a t -> 'a t
(** [styled style pp] wraps [pp] with ANSI escape codes for [style] when
    printing to [stdout] or [stderr] (controlled by {!use_ansi_stdout} and
    {!use_ansi_stderr}). Prints without styling on other formatters or when ANSI
    is disabled. *)

(** {1 Basic Formatters} *)

val string : string t
val int : int t
val int32 : int32 t
val int64 : int64 t
val bool : bool t
val char : char t

(** {1 Combinators} *)

val list : ?sep:unit t -> 'a t -> 'a list t
(** [list ?sep pp] formats a list with [pp] for each element and [sep] between
    them. Default separator: {!semi}. *)

val array : ?sep:unit t -> 'a t -> 'a array t
(** [array ?sep pp] formats an array with [pp] for each element and [sep]
    between them. Default separator: {!semi}. *)

val option : 'a t -> 'a option t
(** [option pp] formats [None] as ["None"] and [Some v] as ["Some <v>"]. *)

val result : ok:'a t -> error:'b t -> ('a, 'b) result t
(** [result ~ok ~error] formats [Ok v] as ["Ok <v>"] and [Error e] as
    ["Error <e>"]. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
(** [pair pp_a pp_b] formats a pair as ["(<a>, <b>)"]. *)

val brackets : 'a t -> 'a t
(** [brackets pp] wraps the output of [pp] with square brackets. *)

val semi : unit t
(** Format ["; "] followed by a break hint. *)

val comma : unit t
(** Format [", "] followed by a break hint. *)

(** {1 Utilities} *)

val to_string : 'a t -> 'a -> string
(** [to_string pp v] formats [v] using [pp] and returns the result as a string.
*)

val styled_string : style -> string -> string
(** [styled_string style s] wraps [s] with ANSI escape codes for [style] when
    {!use_ansi_stdout} is enabled. Unlike {!styled}, this works outside of
    [Format] formatters (e.g. for building styled strings directly). *)
