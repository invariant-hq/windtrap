(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Shared plumbing of the instrumentation data files.

    {!Coverage} and {!Verdicts} name their output file by the same build-path
    rule, record the same writer identity, write through the same atomic rename
    and parse with the same scanner; what varies between the two formats is a
    {!type:format} constant. Stdlib only: this module is in the closure of every
    instrumented library. *)

(** {1:formats Formats} *)

type format = {
  magic : string;
      (** The version-tagged header line, e.g. ["windtrap-coverage-v3"]. *)
  kind : string;
      (** The file's name in error messages: ["coverage"] or ["verdict"]. *)
  dir : string;
      (** The data directory's name, e.g. ["coverage"] ({!data_dir},
          {!standalone_data_dir}). *)
  ext : string;  (** The file extension, without the dot. *)
  remedy : string;
      (** The fix an {!Unknown_format} message names, as a full clause. *)
  who : string;
      (** The owning module's name, the prefix of its [Invalid_argument]
          messages. *)
}
(** The type for an instrumentation on-disk format. *)

(** {1:identities Writer identities} *)

type identity = { exe : string; digest : string }
(** The type for data-file writer identities: [exe] is the writing executable's
    {!exe_identity} and [digest] the lowercase hex MD5 of its contents at write
    time. *)

val file_digest : string -> string option
(** [file_digest path] is the lowercase hex MD5 of the file at [path], or [None]
    when it cannot be read. *)

(** {1:paths Build paths}

    A path's build directory is the path cut after its first component whose
    name starts with [_build]; paths below it are compared with any
    [.sandbox/<digest>] prefix stripped and lexically normalized ([.] and empty
    components dropped, [..] resolved), so every spelling of one executable is
    one identity and one data file. An executable under a build directory writes
    in {!data_dir}; one outside any writes in {!standalone_data_dir} under the
    directory it was started in. *)

val absolute : string -> string
(** [absolute path] resolves [path] against the current directory when it is
    relative. *)

val build_dir : path:string -> string option
(** [build_dir ~path] is the build directory [path] ({!absolute}'d first) lies
    in, e.g. ["/w/_build"] for ["/w/_build/default/test/t.exe"], or [None] when
    no component starts with [_build]. *)

val build_root : path:string -> string option
(** [build_root ~path] is the parent of {!build_dir}[ ~path], or [None]. *)

val exe_identity : exe:string -> string
(** [exe_identity ~exe] is the identity recorded for the executable at [exe]:
    its normalized path below its {!build_dir} (sandbox prefix removed), or its
    absolute path when under none. *)

val data_dir : format -> build_dir:string -> string
(** [data_dir f ~build_dir] is [<build_dir>/_<f.dir>]. *)

val standalone_data_dir : format -> root:string -> string
(** [standalone_data_dir f ~root] is [<root>/_windtrap/<f.dir>]. *)

val output_file : format -> exe:string -> string
(** [output_file f ~exe] is [<dir>/windtrap-<hash>.<f.ext>], [<dir>] the
    {!data_dir} of [exe]'s {!build_dir}, or the {!standalone_data_dir} of the
    current directory when [exe] is under none, and [<hash>] the hex MD5 of
    {!exe_identity}. One file per executable, replaced on every run. *)

val output_dir : format -> exe:string -> string
(** [output_dir f ~exe] is [<dir>/windtrap-<hash>], {!output_file}'s stem
    without the extension: one directory per executable, for a format whose
    every run keeps its own file ({!write_new_file}). *)

(** {1:errors Errors} *)

(** The type for data-file errors shared by both formats. *)
type error =
  | Unknown_format of { path : string; header : string }
      (** [path] does not start with the format's magic string; [header] is its
          escaped first line, truncated to 64 bytes. *)
  | Unreadable of { path : string; reason : string }
      (** [path] cannot be read; [reason] is the system message. *)
  | Corrupt of { path : string; reason : string }
      (** [path] has the right magic but malformed data. *)

val pp_error : format -> Format.formatter -> error -> unit
(** [pp_error f ppf e] formats [e] in [f]'s vocabulary, ending in [f.remedy]. *)

(** {1:files Reading and writing} *)

val read_file : string -> (string, error) result
(** [read_file path] is the whole file at [path], read binary:
    [Error (Unreadable _)] when it cannot be opened or read, [Error (Corrupt _)]
    when it shrinks while being read. *)

val write_file : string -> string -> unit
(** [write_file path data] writes [data] atomically, through a temporary next to
    [path] renamed over it, creating [path]'s directory if needed.

    Raises [Sys_error] if the file cannot be written. *)

val write_new_file : string -> prefix:string -> ext:string -> string -> string
(** [write_new_file dir ~prefix ~ext data] writes [data] atomically, through a
    [.tmp] sibling and a rename as {!write_file} does, to a fresh file
    [<prefix><token>.<ext>] in [dir], creating [dir] if needed, and is that
    file's path. [token] is six hex digits reserved by exclusive creation, so
    concurrent writers never share a name.

    Raises [Sys_error] if no name can be reserved or the file cannot be written.
*)

(** {1:header The header}

    A data file is its magic line, an optional identity line
    ([exe <digest> <len> <path>]), then format-specific records; everything
    after the identity line starts with a digit. *)

val add_header : format -> Buffer.t -> identity option -> unit
(** [add_header f buffer identity] appends [f]'s magic line, then the identity
    line when given; merged or synthetic data, which has no single writer,
    passes [None].

    Raises [Invalid_argument] (prefixed with [f.who]) if [identity.exe] is [""]
    or [identity.digest] is not 32 lowercase hex characters. *)

(** {1:parsing Parser scaffolding}

    One strict scanner for both formats: a mutable {!type:cursor} over the whole
    input, and readers that raise {!Parse_error}, which each format's
    [of_string] turns into a [Corrupt] error. *)

type cursor
(** The type for parse cursors: a position in an input string. *)

exception Parse_error of string
(** Raised by the readers below with a human-readable reason. Never escapes a
    format's [of_string]. *)

val parse_fail : ('a, unit, string, 'b) format4 -> 'a
(** [parse_fail fmt ...] raises {!Parse_error} with the formatted reason. *)

val start : format -> path:string -> string -> (cursor, error) result
(** [start f ~path s] is a cursor over [s] past [f]'s magic string, or
    [Error (Unknown_format _)] naming [path] when [s] does not begin with
    [f.magic] followed by whitespace or the end of input. *)

val read_nat : cursor -> string -> int
(** [read_nat c what] reads a non-negative decimal integer after any whitespace.
    Raises {!Parse_error} naming [what] when there is none, it is negative, or
    it does not fit in an [int]. *)

val read_count : cursor -> string -> int
(** [read_count c what] is {!read_nat} bounded by the remaining input. *)

val read_name : cursor -> string -> string
(** [read_name c what] reads a length-prefixed string: a natural, one space,
    then exactly that many bytes. Raises {!Parse_error} when any of the three is
    missing. *)

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
