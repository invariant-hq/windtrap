(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** JSON text.

    Windtrap's machine documents are JSON
    ({{:https://www.rfc-editor.org/rfc/rfc8259}RFC 8259}), and a document
    exchanged between systems must be UTF-8 (§8.1). A {!value} is written so
    that its text is always valid JSON and always UTF-8, whatever bytes its
    strings hold.

    @canonical Windtrap.Private.Json *)

(** The values a document holds. *)
type value =
  | Int of int
  | Float of float
  | Bool of bool
  | String of string
  | List of value list
  | Object of (string * value) list
      (** The fields of an object, written in their order. *)

val to_string : value -> string
(** [to_string v] is the JSON text of [v], on one line: [", "] separates the
    elements of a list and the fields of an object, and [": "] follows a key.

    - A string, a key included, escapes its quotes and its backslashes, writes a
      newline, a carriage return and a tab as [\n], [\r] and [\t], and its other
      control bytes and DEL as [\u00XX]. Each malformed UTF-8 sequence in it
      becomes U+FFFD, so the text is UTF-8 even when the string is not.
    - An integer is its decimal. A float is the shortest of its decimals to 15,
      16 and 17 significant digits that reads back to it, and [null] when it is
      not finite: JSON has no infinity and no NaN. *)
