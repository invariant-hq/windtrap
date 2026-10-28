(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Line sets: the lines of some source files, which [--lines] and [--diff] give
    to scope mutation and filter the reports of coverage and mutation.

    A file is named as the instrumenter records it, relative to the root of the
    project, and as [git diff] spells it from the root of its repository; a
    leading [./] is dropped, and nothing else is normalised.

    @canonical Windtrap.Private.Lines *)

type t
(** The type for line sets: for each file, a set of 1-based lines. *)

val empty : t
(** [empty] holds no line. *)

val is_empty : t -> bool
(** [is_empty t] is [true] iff [t] holds no line. *)

val union : t -> t -> t
(** [union a b] holds the lines of [a] and of [b]. *)

val mem : t -> file:string -> line:int -> bool
(** [mem t ~file ~line] is [true] iff [t] holds [line] of [file]. *)

val files : t -> string list
(** [files t] is the files of which [t] holds a line, sorted. *)

val to_string : t -> string
(** [to_string t] is [t] in the syntax of {!of_spec}, each file once with its
    ranges merged, the files sorted. *)

val of_spec : string -> (t, string) result
(** [of_spec spec] reads [FILE:A-B,…]: comma-separated items, each a range [A-B]
    or a line [A] with [1 <= A <= B]. An item [FILE:RANGE] names the file of its
    range and of the bare ranges that follow, split at its last colon, so a file
    may hold one. [Error expected] describes the syntax when an item is
    malformed, a range is empty, or a range names no file. *)

val of_diff : string -> t
(** [of_diff text] is the lines that the unified diff [text] adds or changes in
    its new files, as [git diff -U0] prints them: for each file whose header
    [+++ b/FILE] follows a [---] line, the lines [c] to [c + d - 1] of each hunk
    [@@ -a,b +c,d @@], with [d = 1] when [,d] is absent. A hunk that only
    deletes adds no line, and a deleted file ([+++ /dev/null]) none. A header
    without the [b/] prefix names its file as written. Anything else is ignored.
    A diff with context lines counts them as changed, so [-U0] gives the lines
    that changed and no others. *)
