(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Formatting helpers and explicit ANSI styling.

    A small [Fmt]-shaped layer over {!Stdlib.Format} holding what this library
    actually prints with: short aliases, composable printers, and ANSI styling
    that is explicit at every call site. It is not a general-purpose printing
    toolbox — an entry here earns its place by having a caller.

    Styling discipline: failure payloads store plain strings rendered with
    {!to_string}; only renderers call {!styled_string}, passing their own
    [~ansi] decision (derived from the environment's color detection). No global
    state controls styling, so a printer used to build failure data can never
    leak escape codes into it.

    Output discipline likewise: there is no printer here that writes to a
    standard channel. A sink is always a parameter — {!pf}'s formatter,
    [Render.create]'s [~out] — so a run's transcript has one destination that
    its caller chose. *)

(** {1:types Types} *)

type 'a t = Format.formatter -> 'a -> unit
(** The type for printers of values of type ['a]. *)

type style = [ `Bold | `Faint | `Red | `Green | `Yellow | `Cyan | `White ]
(** The type for ANSI styles understood by {!styled_string}. *)

(** {1:output Output} *)

val abstract : string
(** [abstract] is what a value with no rendering prints as (["<abstract>"]):
    {!Testable.of_equal}'s witness, and the fallback of every verb taking an
    optional printer for a rejected value. One spelling, because it is one thing
    a reader learns to recognise. *)

val str : ('a, Format.formatter, unit, string) format4 -> 'a
(** [str fmt ...] formats to a string. Equivalent to {!Format.asprintf}. *)

val pf : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
(** [pf ppf fmt ...] formats to [ppf]. Equivalent to {!Format.fprintf}. *)

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

val float_exact : float t
(** [float_exact] is the shortest decimal rendering that round-trips to the
    exact bits — 15 significant digits, else 16, else 17. It is the only float
    printer here, deliberately: everything this library prints a float into is
    something a reader may copy back and expect the same double — a property
    counterexample pasted into [~examples], a bit-exact witness — and a
    fixed-precision rendering is not the value that was there. A caller wanting
    a compact, lossy spelling asks for it at the call site, as {!Testable}'s
    [%g] instances do. Non-finite values render as [nan], [inf], [-inf], and the
    sign of zero survives. *)

val bool : bool t

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
(** [semi] formats ["; "] with a break hint. It is {!list}'s default; a caller
    wanting another separator passes its own [?sep]. *)

(** {1:styling Styling}

    Renderer-side only, and a single function: the ANSI decision is taken
    explicitly, and with [~ansi:false] it is the identity, so the same rendering
    code serves color and monochrome transports.

    Styles do not nest: the reset that closes one style also ends any enclosing
    style. Style sibling fragments, not containers.

    Styled output is plain bytes, not zero-width tokens, so it is not
    width-transparent: a caller laying out columns measures with
    {!Text.strip_ansi} and {!Text.length_utf8} rather than letting {!Format}
    count. That is what the renderer does — it emits whole lines through a bare
    ["%s"] and does its own truncation — and it is why a styled ['a t]
    combinator would buy nothing here. *)

val styled_string : ansi:bool -> style -> string -> string
(** [styled_string ~ansi s str] is [str] wrapped in the escape codes for [s]
    when [ansi] is [true], and [str] unchanged otherwise. For building styled
    strings outside a formatter.

    An empty [str] is returned bare under [ansi] too: styling nothing is
    nothing, and lines assembled from optional fragments would otherwise carry
    an open code and its reset with nothing between them. *)
