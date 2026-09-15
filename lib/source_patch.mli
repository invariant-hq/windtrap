(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Rewriting string literals in OCaml source at recorded positions.

    A correction of an [expect] literal is a {!patch}: the position the literal
    was compiled at, the value it was compiled with, and the value to write.
    {!apply} rewrites a file's bytes with every patch of that file at once,
    keeping each literal's delimiter and the author's layout elsewhere.

    The flexible comparison form of a literal ({!normalize}) and the formatting
    of a flexible payload ({!format_flexible}) are ppx_expect's, so a correction
    is byte-compatible with one ppx_expect would write. *)

(** {1:flexible Flexible text} *)

val normalize : string -> string
(** [normalize s] is the comparison form of a flexible literal: every line
    right-stripped, blank leading and trailing lines dropped, and the block
    dedented by the smallest indentation of its nonempty lines. Two texts match
    flexibly iff their normalizations are equal. *)

(** {1:literals Literal rendering} *)

(** The type for string literal delimiters. *)
type delimiter =
  | Quote  (** ["…"]: a correction escapes its lines onto one source line. *)
  | Tag of string
      (** [{tag|…|tag}]: a correction grows the tag until the contents hold
          neither delimiter. The string-extension spelling of an expect node,
          [{%expect tag|…|tag}], is this delimiter with its head kept. *)

val fix_tag : contents:string -> string -> string
(** [fix_tag ~contents tag] is [tag] extended with ["xxx"] until neither [{tag|]
    nor [|tag}] occurs in [contents]. *)

val format_flexible : delimiter:delimiter -> column:int -> string -> string
(** [format_flexible ~delimiter ~column raw] is the contents of a flexible
    literal for the output [raw]: a single line padded with one space on each
    side under {!Tag} and bare under {!Quote}; several lines each indented at
    [column + 2] under {!Tag}, opening after a newline and closing on a line of
    that indentation, and indented by one space under {!Quote}. *)

val literal : delimiter:delimiter -> string -> string
(** [literal ~delimiter contents] is the literal text holding [contents]:
    [{tag|contents|tag}] with the tag grown by {!fix_tag}, or ["…"] with every
    line escaped and joined by [\n]. *)

(** {1:patches Patches} *)

(** The type for how a literal is compared and therefore rewritten. *)
type style =
  | Flexible  (** [expect]: contents are reformatted by {!format_flexible}. *)
  | Exact  (** [expect_exact]: contents are written verbatim. *)

type patch
(** The type for one literal rewrite. *)

val patch : site:Loc.pos -> literal:string -> style:style -> string -> patch
(** [patch ~site ~literal ~style content] rewrites the literal at [site], whose
    compiled value is [literal], to hold [content]. [site] is the position of
    the [__POS_OF__ literal] expression (parenthesized or not), of the literal
    itself, as [__POS_OF__] records it, or of an [[%expect]] node whose payload
    the literal is. A node with no payload, [[%expect]], compiles to the empty
    literal and is patched by inserting one. *)

(** The type for refused patches. *)
type error =
  | No_literal of Loc.pos
      (** No string literal follows the position in the file. *)
  | Drifted of Loc.pos
      (** The literal at the position decodes to a value other than the one the
          patch was compiled with: the file changed since the build. *)

val error_message : error -> string
(** [error_message e] is a one-line description of [e] naming the site. *)

val apply : string -> patch list -> (string, error) result
(** [apply source patches] is [Ok text] where [text] is [source] with every
    patch applied: for each, the file is lexed forward from the patch's position
    past an optional opening parenthesis, the [__POS_OF__] token, an expect
    node's head ([[%expect]] or [[%expect_exact]]) and whitespace to one string
    literal, which is replaced by {!literal} of the new contents (formatted by
    {!format_flexible} at the indentation of the position's line for a
    {!Flexible} patch) in the literal's own delimiter; a node with no payload
    gets the literal inserted before its closing bracket. A literal decodes as
    the lexer compiles it, escapes and continuation lines resolved and a CRLF
    newline read as LF. Patches apply in position order, offsets adjusted, and
    the rest of the file is byte-identical.

    [Error e] names the first patch refused, and nothing is applied. *)
