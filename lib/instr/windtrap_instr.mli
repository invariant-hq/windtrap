(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Shared plumbing of the instrumentation runtimes.

    [Windtrap_coverage] and [Windtrap_mutate] are deliberately separate
    sub-libraries — neither links the other — yet both name their output file
    by the same build-path rule, record the same writer identity, write
    through the same atomic rename, and parse their files back with the same
    hand-rolled scanner. This module is that shared ground, extracted so the
    semantics cannot drift. The two runtimes are its only intended callers, and
    everything that varies between them is a {!type:format} constant, never a
    hook.

    Stdlib only: either runtime puts this module into the closure of every
    instrumented library, so it must never pull the windtrap core (or anything
    else) along. *)

(** {1:formats Formats} *)

type format = {
  magic : string;
      (** The version-tagged header line, e.g. ["windtrap-coverage-v3"].
          {!start} requires it and {!add_header} writes it. *)
  kind : string;
      (** The file's name in error messages: ["coverage"] or ["verdict"]. *)
  dir : string;
      (** The directory under [_build] the files live in, e.g. ["_coverage"].
      *)
  ext : string;  (** The file extension, without the dot, e.g. ["coverage"]. *)
  remedy : string;
      (** The fix an {!Unknown_format} message names, as a full clause. *)
  who : string;
      (** The runtime's module name — the prefix of its [Invalid_argument]
          messages, e.g. ["Windtrap_coverage"]. *)
}
(** The type for a runtime's on-disk format: the constants that differ between
    the two runtimes, declared once beside each magic string. Every function
    below that names, reads or reports a data file takes one. *)

(** {1:identities Writer identities} *)

type identity = { exe : string; digest : string }
(** The type for data-file writer identities: [exe] is the writing
    executable's {!exe_identity} and [digest] the lowercase hex MD5 of its
    contents at write time. Both runtimes re-export this type; the reporting
    command compares the recorded digest against the executable now on disk to
    detect deleted or rebuilt writers — content, not mtimes, because dune's
    shared cache restores artifacts with their original timestamps. *)

val file_digest : string -> string option
(** [file_digest path] is the lowercase hex MD5 of the file at [path], and
    [None] when it cannot be read. Digesting reads the file once (a few
    milliseconds for a typical test binary); callers keep it off the test
    path. *)

(** {1:paths Build paths}

    One root rule, shared by output naming, identity recording and the
    reporting command's discovery: a path's project root is the parent of its
    {e topmost} [_build] component, and paths below [_build] are compared with
    any [.sandbox/<digest>] prefix stripped, so sandboxed and direct runs
    agree. *)

val absolute : string -> string
(** [absolute path] is [path] resolved against the current directory when
    relative, and [path] itself otherwise. *)

val build_root : path:string -> string option
(** [build_root ~path] is the parent directory of the topmost [_build]
    component of [path] (resolved against the current directory when
    relative), and [None] when [path] has no [_build] component. *)

val exe_identity : exe:string -> string
(** [exe_identity ~exe] is the identity recorded for the executable at path
    [exe]: its path below the topmost [_build] directory (with any
    [.sandbox/<digest>] prefix removed), or its absolute path when [exe] is
    not under a [_build] directory. *)

val output_file : format -> exe:string -> string
(** [output_file f ~exe] is the deterministic data-file path for the
    executable at path [exe] (resolved against the current directory when
    relative): [<root>/_build/<f.dir>/windtrap-<hash>.<f.ext>], where [<root>]
    is the parent of the topmost [_build] component of [exe] and [<hash>] the
    hex MD5 of [exe]'s path below [_build] (sandbox prefix removed). When
    [exe] is not under a [_build] directory, [<root>] is the current directory
    and the full path of [exe] is hashed. *)

(** {1:errors Errors} *)

(** The type for data-file errors — the three ways a file fails that both
    formats share. [Windtrap_mutate] re-exports this type as its [error];
    [Windtrap_coverage] adds a fourth, merge-time case of its own and injects
    these three. *)
type error =
  | Unknown_format of { path : string; header : string }
      (** [path] does not start with the format's magic string; [header] is
          its escaped first line, truncated to 64 bytes. *)
  | Unreadable of { path : string; reason : string }
      (** [path] cannot be read; [reason] is the system message. *)
  | Corrupt of { path : string; reason : string }
      (** [path] has the right magic but malformed data. *)

val pp_error : format -> Format.formatter -> error -> unit
(** [pp_error f ppf e] formats a human-readable message for [e] in [f]'s
    vocabulary, including the likely fix. *)

(** {1:files Reading and writing} *)

val read_file : string -> (string, error) result
(** [read_file path] is the contents of the file at [path], read whole and
    binary. [Error (Unreadable _)] when it cannot be opened or read, and
    [Error (Corrupt _)] when it shrinks while being read. *)

val write_file : string -> string -> unit
(** [write_file path data] writes [data] to [path], creating [path]'s
    directory if needed. The write is atomic: the data goes to a uniquely
    named temporary file next to [path] (exclusive creation, retried under a
    fresh random suffix on collision), which is then renamed over [path] — a
    reader never observes a partial file, concurrent writers never interleave
    into a shared temp file, and a leftover [.tmp] from a crashed run is
    skipped, not reused.

    Raises [Sys_error] if the file cannot be written. *)

(** {1:header The header}

    A data file is its magic line, an optional identity line
    ([exe <digest> <len> <path>]), then format-specific records. The identity
    line is unambiguous: everything after it starts with a digit. *)

val add_header : format -> Buffer.t -> identity option -> unit
(** [add_header f buffer identity] appends [f]'s magic line to [buffer], then
    the identity line when [identity] is given — the writing runtime passes
    one, while merged or synthetic collections, which have no single writer,
    serialize without.

    Raises [Invalid_argument] (prefixed with [f.who]) if [identity.exe] is
    [""] or [identity.digest] is not 32 lowercase hex characters. *)

(** {1:parsing Parser scaffolding}

    Both file formats are parsed by the same strict scanner: a mutable
    {!type:cursor} over the whole input, and readers that raise {!Parse_error}
    — caught by each runtime's [of_string], which turns the carried reason
    into a [Corrupt] error. Nothing is repaired and nothing is guessed. *)

type cursor
(** The type for parse cursors: a position in an input string. Created by
    {!start}, advanced by the [read_*] functions below. *)

exception Parse_error of string
(** Raised by the readers below on malformed input, carrying a human-readable
    reason. Never escapes a runtime's [of_string]. *)

val parse_fail : ('a, unit, string, 'b) format4 -> 'a
(** [parse_fail fmt ...] raises {!Parse_error} with the formatted reason. The
    runtimes use it for their format-specific checks, so their reasons and the
    scaffolding's read as one vocabulary. *)

val start : format -> path:string -> string -> (cursor, error) result
(** [start f ~path s] is a cursor over [s] positioned after [f]'s magic
    string, or [Error (Unknown_format _)] naming [path] when [s] does not
    begin with [f.magic] followed by whitespace or the end of input. *)

val read_nat : cursor -> string -> int
(** [read_nat c what] reads a non-negative decimal integer, skipping leading
    whitespace. Raises {!Parse_error} naming [what] when none is there, when
    it is negative, or when it does not fit in an [int]. *)

val read_count : cursor -> string -> int
(** [read_count c what] is {!read_nat} plus a cheap sanity bound: a count of
    things each at least one byte wide cannot exceed the input's length, and
    ["<what> exceeds data"] rejects it before an allocation or a loop trusts
    it. *)

val read_name : cursor -> string -> string
(** [read_name c what] reads a length-prefixed string: a natural (named
    ["<what> length"]), one space, then exactly that many bytes — which may
    themselves be spaces or newlines. Raises {!Parse_error} when the prefix,
    the space or the bytes are missing. *)

val read_word : cursor -> string -> string
(** [read_word c what] reads a maximal non-empty run of non-whitespace bytes,
    skipping leading whitespace. Raises {!Parse_error} naming [what] at end of
    input. *)

val read_identity : cursor -> identity option
(** [read_identity c] reads the optional identity line: [None], with [c]
    moved past leading whitespace only, when the input at [c] does not start
    with [exe]. Raises {!Parse_error} on a digest that is not 32 lowercase hex
    characters or an empty executable path. *)

val finish : cursor -> unit
(** [finish c] accepts end of input: it skips trailing whitespace and raises
    {!Parse_error} (["trailing data at offset %d"]) if anything else remains.
*)
