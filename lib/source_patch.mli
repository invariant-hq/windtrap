(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Rewriting string literals in OCaml source at recorded positions.

    The correction of a literal is a {!type-patch}: the position that the
    literal was compiled at, the value that it was compiled with, how it is
    compared, and the text to write. {!apply} rewrites the bytes of a file with
    every patch of that file at once, and {!normalize} is the form under which a
    flexible literal is compared.

    The comparison form and the layout of a corrected literal are those of
    ppx_expect, so a correction is byte for byte the one that ppx_expect writes.
    Every function is pure, and none raises. *)

(** {1:flexible Flexible text} *)

val normalize : string -> string
(** [normalize s] is the form under which a flexible literal is compared. Two
    texts match flexibly iff their normalizations are equal. It is [s] after
    three steps:
    - Every line is stripped of its trailing whitespace.
    - The blank lines at both ends are dropped.
    - The block is dedented by the smallest indentation of its nonempty lines.

    Whitespace is space, TAB, LF, VT, FF and CR, so a CR LF line end reads as
    LF. Indentation counts the leading spaces of a line and nothing else, and a
    line loses all its leading whitespace. A block that is indented with tabs
    thus loses its relative indentation. *)

(** {1:patches Patches} *)

(** The type for how the client compares a literal, which decides how {!apply}
    lays out its new contents. *)
type style =
  | Flexible
      (** Compared under {!normalize}. The new contents are laid out by
          {!apply}. *)
  | Exact
      (** Compared byte for byte. The new contents are the [content] of the
          patch as given, inside the delimiter of the literal. *)

type patch
(** The type for the rewrite of one literal, or for the insertion of an expect
    node after the body of an expect test ({!trailing}). *)

val patch : site:Loc.pos -> literal:string -> style:style -> string -> patch
(** [patch ~site ~literal ~style content] is the rewrite of the literal at
    [site], whose compiled value is [literal], to hold [content]. It checks
    nothing.
    - [site] is the position of one of three things: a [__POS_OF__ literal]
      expression, in parentheses or not, the literal itself, or an expect node,
      spelled [[%expect …]] or [{%expect|…|}], whose payload is the literal.
    - [literal] must be the value that the running executable was compiled with,
      because {!apply} refuses a patch whose [literal] is not what the source
      decodes to ({!Drifted}). A node without payload, [[%expect]], compiles to
      [""]. *)

val trailing : site:Loc.pos -> string -> patch
(** [trailing ~site content] is the insertion of an [[%expect]] node that holds
    [content], laid out as a {!Flexible} patch's, after the body of the expect
    test at [site]. It checks nothing. The file, line and start column of [site]
    are those of the test's [let%expect_test] or [[%%expect_test]], and its end
    column is the end of the body, counted from the start of that line.

    {!apply} writes [;], a newline, and the node indented two columns right of
    the test's head, at the end of the body. It refuses the patch as {!Drifted}
    when the test's head is not at [site] or the body does not end on a token
    there. *)

(** The type for refused patches. The payload is the [site] of the patch. *)
type error =
  | No_literal of Loc.pos
      (** No string literal follows the position. The line is not in the file,
          or the literal is never closed, or something other than whitespace,
          [(], [__POS_OF__] and the head of a node stands before it, a comment
          included. *)
  | Drifted of Loc.pos
      (** The literal at the position decodes to another value than the
          [literal] of the patch, or the node has no payload and that [literal]
          is not [""], or the test of a {!trailing} patch is not at its site.
          The file changed since the build. *)

val error_message : error -> string
(** [error_message e] is one sentence on [e], on one line, naming neither file
    nor line, which its caller names:
    [no string literal at the recorded position] for {!No_literal}, and for
    {!Drifted} that the literal differs from the value the binary was compiled
    with, then [rebuild and rerun]. *)

val apply : string -> patch list -> (string, error) result
(** [apply source patches] is [Ok text], where [text] is [source] with every
    patch applied. [source] is the bytes of the file that the positions were
    recorded in. The caller must pass the patches of that file alone, because
    the file of a position is compared with nothing.

    {b Decoding.} The literal that follows the position decodes as the compiler
    reads it under OCaml 5.2 and later, with its escapes and continuation lines
    resolved and a CR LF newline read as LF. OCaml 5.0 and 5.1 keep the CR of
    such a newline in the compiled value. No version drops more than one CR
    before an LF, and the decoder drops them all. In both cases the patch is
    refused as {!Drifted}.

    {b Rewriting.} The literal is replaced by one that holds the new contents in
    its own delimiter. A quoted literal stays quoted. Each line is escaped by
    [String.escaped] and the lines are joined by [\n], so the literal is one
    source line and its non-ASCII bytes become decimal escapes. A [{tag|…|tag}]
    literal keeps its tag and its contents are written raw. The tag grows by
    [xxx] until the contents hold neither [{tag|] nor [|tag}].

    The head of a [{%expect tag|…|tag}] node is kept. A node without payload
    gets a space and a [{|…|}] literal before its closing bracket.

    The new contents of an {!Exact} patch that hold a CR are written in a quoted
    literal, whatever the delimiter of the old one, and the CR is written [\r].
    A [{%ext|…|}] node becomes [[%ext "…"]], and a node without payload gets a
    space and the quoted literal.

    The new contents of an {!Exact} patch are its [content] as given. Those of a
    {!Flexible} patch are the lines of [normalize content], laid out as follows,
    where [c] is the number of leading spaces of the line that holds the
    position:
    - With no line, the contents are one space in a tagged literal and empty in
      a quoted one.
    - One line stands between two spaces in a tagged literal, and bare in a
      quoted one.
    - Several lines in a tagged literal are a newline, then each line indented
      by [c + 2] spaces before its own indentation, then a last line of [c + 2]
      spaces, on which the delimiter closes.
    - Several lines in a quoted literal are a space and a newline, then each
      line indented by one space before its own indentation, then a newline and
      a space.

    The literal that a {!Flexible} patch writes normalizes to
    [normalize content], so the corrected literal matches on the next run.

    {b The result.} The patches apply in the order of their positions, whatever
    the order of the list. Every byte outside the replaced literals is copied,
    and in a CR LF file the lines of a new literal end in LF alone. [patches]
    must hold one patch at most for a literal.

    [Error e] is the first patch of the list that is refused, and no patch is
    then applied. *)
