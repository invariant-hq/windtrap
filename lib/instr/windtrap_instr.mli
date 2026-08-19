(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Shared plumbing of the instrumentation runtimes.

    [Windtrap_coverage] and [Windtrap_mutate] are separate sub-libraries —
    neither links the other — yet both name their output file by the same
    build-path rule, record the same writer identity, write through the same
    atomic rename, and parse their files back with the same scanner. This is
    that shared ground. The two runtimes are its only intended callers, and
    everything that varies between them is a {!type:format} constant, never a
    hook.

    Stdlib only: either runtime puts this module into the closure of every
    instrumented library, so it must never pull the windtrap core (or anything
    else) along. *)

(** {1:formats Formats} *)

type format = {
  magic : string;
      (** The version-tagged header line, e.g. ["windtrap-coverage-v3"]. *)
  kind : string;
      (** The file's name in error messages: ["coverage"] or ["verdict"]. *)
  dir : string;  (** The directory under [_build], e.g. ["_coverage"]. *)
  ext : string;  (** The file extension, without the dot. *)
  remedy : string;
      (** The fix an {!Unknown_format} message names, as a full clause. *)
  who : string;
      (** The runtime's module name — the prefix of its [Invalid_argument]
          messages. *)
}
(** The type for a runtime's on-disk format: what differs between the two
    runtimes, declared once beside each magic string. Every function below
    that names, reads or reports a data file takes one. *)

(** {1:identities Writer identities} *)

type identity = { exe : string; digest : string }
(** The type for data-file writer identities: [exe] is the writing
    executable's {!exe_identity} and [digest] the lowercase hex MD5 of its
    contents at write time. Content, not mtimes: dune's cache restores
    rebuilt artifacts with their original timestamps. *)

val file_digest : string -> string option
(** [file_digest path] is the lowercase hex MD5 of the file at [path], [None]
    when it cannot be read. It reads the whole file. *)

(** {1:paths Build paths}

    One root rule, shared by output naming, identity recording and the
    reporting commands' discovery: a path's project root is the parent of its
    {e topmost} [_build] component, and paths below [_build] are compared with
    any [.sandbox/<digest>] prefix stripped, so sandboxed and direct runs
    agree. *)

val absolute : string -> string
(** [absolute path] resolves [path] against the current directory when it is
    relative. *)

val build_root : path:string -> string option
(** [build_root ~path] is the parent of the topmost [_build] component of
    [path] ({!absolute}'d first), and [None] when it has none. *)

val exe_identity : exe:string -> string
(** [exe_identity ~exe] is the identity recorded for the executable at [exe]:
    its path below the topmost [_build] (sandbox prefix removed), or its
    absolute path when it is not under one. *)

val output_file : format -> exe:string -> string
(** [output_file f ~exe] is
    [<root>/_build/<f.dir>/windtrap-<hash>.<f.ext>], where [<root>] is
    {!build_root} and [<hash>] the hex MD5 of {!exe_identity}. When [exe] is
    not under a [_build], [<root>] is the current directory. *)

(** {1:errors Errors} *)

(** The type for data-file errors — the three ways a file fails that both
    formats share. [Windtrap_mutate] re-exports it as its [error];
    [Windtrap_coverage] wraps it beside a merge-time case of its own. *)
type error =
  | Unknown_format of { path : string; header : string }
      (** [path] does not start with the format's magic string; [header] is
          its escaped first line, truncated to 64 bytes. *)
  | Unreadable of { path : string; reason : string }
      (** [path] cannot be read; [reason] is the system message. *)
  | Corrupt of { path : string; reason : string }
      (** [path] has the right magic but malformed data. *)

val pp_error : format -> Format.formatter -> error -> unit
(** [pp_error f ppf e] formats [e] in [f]'s vocabulary, ending in [f.remedy].
*)

(** {1:files Reading and writing} *)

val read_file : string -> (string, error) result
(** [read_file path] is the whole file at [path], read binary.
    [Error (Unreadable _)] when it cannot be opened or read,
    [Error (Corrupt _)] when it shrinks while being read. *)

val write_file : string -> string -> unit
(** [write_file path data] writes [data] atomically — a temporary file next to
    [path], renamed over it — creating [path]'s directory if needed, so a
    reader never observes a partial file.

    Raises [Sys_error] if the file cannot be written. *)

(** {1:header The header}

    A data file is its magic line, an optional identity line
    ([exe <digest> <len> <path>]), then format-specific records. The identity
    line is unambiguous: everything after it starts with a digit. *)

val add_header : format -> Buffer.t -> identity option -> unit
(** [add_header f buffer identity] appends [f]'s magic line, then the identity
    line when given — merged or synthetic data, which has no single writer,
    passes [None].

    Raises [Invalid_argument] (prefixed with [f.who]) if [identity.exe] is
    [""] or [identity.digest] is not 32 lowercase hex characters. *)

(** {1:parsing Parser scaffolding}

    One strict scanner for both formats: a mutable {!type:cursor} over the
    whole input, and readers that raise {!Parse_error} — caught by each
    runtime's [of_string], which turns the reason into a [Corrupt] error.
    Nothing is repaired and nothing is guessed. *)

type cursor
(** The type for parse cursors: a position in an input string. *)

exception Parse_error of string
(** Raised by the readers below, carrying a human-readable reason. Never
    escapes a runtime's [of_string]. *)

val parse_fail : ('a, unit, string, 'b) format4 -> 'a
(** [parse_fail fmt ...] raises {!Parse_error} with the formatted reason, so a
    runtime's own checks read in the scaffolding's vocabulary. *)

val start : format -> path:string -> string -> (cursor, error) result
(** [start f ~path s] is a cursor over [s] past [f]'s magic string, or
    [Error (Unknown_format _)] naming [path] when [s] does not begin with
    [f.magic] followed by whitespace or the end of input. *)

val read_nat : cursor -> string -> int
(** [read_nat c what] reads a non-negative decimal integer after any
    whitespace. Raises {!Parse_error} naming [what] when there is none, it is
    negative, or it does not fit in an [int]. *)

val read_count : cursor -> string -> int
(** [read_count c what] is {!read_nat} bounded by the remaining input: a count
    of things at least one byte wide cannot exceed it. *)

val read_name : cursor -> string -> string
(** [read_name c what] reads a length-prefixed string: a natural, one space,
    then exactly that many bytes, whitespace included. Raises {!Parse_error}
    when any of the three is missing. *)

val read_word : cursor -> string -> string
(** [read_word c what] reads a maximal run of non-whitespace bytes after any
    whitespace. Raises {!Parse_error} naming [what] at end of input. *)

val read_identity : cursor -> identity option
(** [read_identity c] reads the optional identity line, or is [None] with [c]
    past leading whitespace only when the input does not start with [exe].
    Raises {!Parse_error} on a malformed digest or an empty path. *)

val finish : cursor -> unit
(** [finish c] accepts end of input, raising {!Parse_error} on anything but
    trailing whitespace. *)
