(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What the two instrumentation file formats share.

    The dumps of {!Coverage} and the verdict files of {!Verdicts} are named by
    one build-path rule, record the same writer identity, are written through
    the same atomic rename and are read by the same scanner. What differs
    between the two formats is a {!type-format} constant. The module depends on
    the standard library only and has no effect when it loads. *)

(** {1:formats Formats} *)

type format = {
  magic : string;
      (** The first line of a file, which carries the version of the format, as
          ["windtrap-coverage-v3"]. *)
  kind : string;
      (** The name of a file in error messages, ["coverage"] or ["verdict"]. *)
  dir : string;
      (** The name of the data directory, as ["coverage"] (see {!data_dir} and
          {!standalone_data_dir}). *)
  ext : string;  (** The file extension, without the dot. *)
  remedy : string;
      (** What the message of an {!Unknown_format} tells its reader to do, as a
          full clause. *)
  who : string;
      (** The name of the module that owns the format. It prefixes the
          [Invalid_argument] messages of {!add_header}. *)
}
(** The type for file formats: the constants that differ between the two. *)

(** {1:identities Writer identities} *)

type identity = { exe : string; digest : string }
(** The type for the identity of the executable that wrote a file. [exe] is its
    {!exe_identity}, and [digest] is the lowercase hexadecimal MD5 of its
    contents when it wrote. An executable at [exe] whose {!file_digest} differs
    is not the one that wrote the file. *)

val file_digest : string -> string option
(** [file_digest path] is the lowercase hexadecimal MD5 of the contents of the
    file at [path], or [None] when the file cannot be read. *)

(** {1:paths Build paths}

    The build directory of a path is the path cut after its first component
    whose name starts with [_build], so dune's default directory and a private
    one such as [_build_ci] are both recognized. Naming a file, recording an
    identity and the discovery of files by the [windtrap] command all apply this
    one rule, so a change to it moves the three together.

    A path is first made absolute and then normalized lexically. [.] and empty
    components are dropped, and [..] is resolved against the component before
    it, without following a symbolic link. The spellings [test/a.exe],
    [./test/a.exe] and [test/sub/../a.exe] of one executable have one identity
    and one file. Below the build directory a leading [.sandbox/<digest>] is
    removed, so a run inside dune's sandbox and a direct run agree.

    Normalization also rewrites every ['\\'] of the path to ['/'], on every
    platform. On Unix, a path with a backslash in one of its names gets the
    identity and the file name of a path that does not exist.

    An executable below a build directory writes under {!data_dir}. An
    executable below none writes under the {!standalone_data_dir} of the current
    directory, because a tree built without dune must never grow a [_build]. The
    current directory is read when {!output_file} or {!output_dir} is called,
    not when the process starts. A value below that needs the current directory,
    for a relative path or for an executable below no build directory, raises
    [Sys_error] if it cannot be read. *)

val absolute : string -> string
(** [absolute path] is [path] when it is absolute, and [path] below the current
    directory otherwise. It does not normalize. *)

val build_dir : path:string -> string option
(** [build_dir ~path] is the build directory that [path] lies in, normalized and
    separated by ['/'], or [None] when no component of [path] starts with
    [_build]. It is ["/w/_build"] for ["/w/_build/default/test/t.exe"], and
    ["/w/_build_ci"] for ["/w/_build_ci/.sandbox/3f/default"]. The first
    matching component decides whatever it names, so every path below
    [/home/u/_build_farm] has that directory as its build directory. *)

val build_root : path:string -> string option
(** [build_root ~path] is the parent directory of [build_dir ~path], or [None]
    when there is none. *)

val exe_identity : exe:string -> string
(** [exe_identity ~exe] is the normalized path of [exe] below its build
    directory, without the sandbox prefix, or its normalized absolute path when
    [exe] lies below no build directory. It is the [exe] of an {!type-identity},
    and it is [""] for a path that ends at its build directory. A relative
    result means an executable below a build directory and an absolute one an
    executable below none, which is how a reader finds the executable again. The
    build context is part of the result, as in [default/test/t.exe], and the
    build directory is not. *)

val data_dir : format -> build_dir:string -> string
(** [data_dir f ~build_dir] is [<build_dir>/_<f.dir>]. Every executable below
    [build_dir] writes the files of [f] there, and the [windtrap] command finds
    them there. *)

val standalone_data_dir : format -> root:string -> string
(** [standalone_data_dir f ~root] is [<root>/_windtrap/<f.dir>], where an
    executable below no build directory writes the files of [f]. *)

val output_file : format -> exe:string -> string
(** [output_file f ~exe] is [<dir>/windtrap-<hash>.<f.ext>]. [<dir>] is the
    {!data_dir} of the build directory of [exe], or the {!standalone_data_dir}
    of the current directory when [exe] lies below none. [<hash>] is the
    hexadecimal MD5 of [exe_identity ~exe]. It is one file for each executable,
    for a format whose writer replaces its file on every run. *)

val output_dir : format -> exe:string -> string
(** [output_dir f ~exe] is [output_file f ~exe] without its extension. It is one
    directory for each executable, for a format whose every run keeps a file of
    its own (see {!write_new_file}). *)

(** {1:errors Errors} *)

(** The type for the errors of reading a file, the same for the two formats. *)
type error =
  | Unknown_format of { path : string; header : string }
      (** [path] does not start with the magic line of the format. [header] is
          its first line, cut to 64 bytes and then escaped. *)
  | Unreadable of { path : string; reason : string }
      (** [path] cannot be read. [reason] is the message of the system. *)
  | Corrupt of { path : string; reason : string }
      (** [path] starts with the magic line and its data is malformed, or the
          file shrank while {!read_file} read it. *)

val pp_error : format -> Format.formatter -> error -> unit
(** [pp_error f ppf e] formats one line on [e] for a person, in the words of
    [f]. [f.kind] names the file. The message of an {!Unknown_format} ends with
    [f.remedy], and those of {!Unreadable} and {!Corrupt} give the reason and no
    remedy. Nothing is printed here, and where the line shows is the contract of
    the caller. The message is not stable enough for a program to match. *)

(** {1:files Reading and writing} *)

val read_file : string -> (string, error) result
(** [read_file path] is the contents of the file at [path], read in binary mode.
    It is [Error (Unreadable _)] when the file cannot be opened or read, and
    [Error (Corrupt _)] when the file shrinks while it is read. *)

val write_file : string -> string -> unit
(** [write_file path data] writes [data] to [path], and creates the directory of
    [path] if needed. It writes a temporary file beside [path] and renames it
    over [path], so a reader never sees a partial file. Of two writers at the
    same time, the last rename wins and the file is whole. Raises [Sys_error] if
    the file cannot be written, and leaves no temporary file then. *)

val write_new_file : string -> prefix:string -> ext:string -> string -> string
(** [write_new_file dir ~prefix ~ext data] writes [data] to a new file
    [<prefix><token>.<ext>] in [dir], through a temporary file and a rename as
    {!write_file} does. It creates [dir] if needed and is the path of the new
    file. [<token>] is six hexadecimal digits that the creation of the temporary
    file reserves, so writers at the same time never share a name, as when
    several processes of one executable exit together. Raises [Sys_error] if no
    name can be reserved in ten attempts, or if the file cannot be written. *)

(** {1:header The header}

    A file is its magic line, an optional identity line, and then the records of
    its format. The magic line is the [magic] of the format and a line feed. The
    identity line is [exe <digest> <length> <path>] and a line feed. [<digest>]
    is 32 lowercase hexadecimal digits, and [<length>] is the number of bytes of
    [<path>], so [<path>] may hold any byte. In every format, what follows the
    identity line must start with a digit, because {!read_identity} tells the
    line from the records by its first three bytes. *)

val add_header : format -> Buffer.t -> identity option -> unit
(** [add_header f buffer identity] appends the magic line of [f] to [buffer],
    and then the identity line when [identity] is given. Raises
    [Invalid_argument], in a message that [f.who] prefixes, if [identity.exe] is
    empty or if [identity.digest] is not 32 lowercase hexadecimal digits. The
    magic line is already in [buffer] then. *)

(** {1:parsing Parsing}

    One strict scanner reads both formats. A {!type-cursor} runs over the whole
    input, and the readers below raise {!Parse_error}, which the [of_string] of
    each format turns into a {!Corrupt} error. Nothing is repaired and nothing
    is guessed. Whitespace is a space, a tab, a carriage return or a line feed.
    The readers skip it before a number or a word, and never inside a name. *)

type cursor
(** The type for cursors: a position in an input string, which the readers
    advance. *)

exception Parse_error of string
(** Raised by the readers below, with a reason for a person. It never escapes
    the [of_string] of a format. *)

val parse_fail : ('a, unit, string, 'b) format4 -> 'a
(** [parse_fail fmt ...] raises {!Parse_error} with the formatted reason. A
    format reports the checks of its own with it. *)

val start : format -> path:string -> string -> (cursor, error) result
(** [start f ~path s] is a cursor over [s] that stands after [f.magic]. It is
    [Error (Unknown_format _)] naming [path] when [s] does not begin with
    [f.magic] followed by whitespace or by the end of the input. An input that
    begins with [windtrap-coverage-v30] is of another format, and the magic
    string alone is a valid start. *)

val read_nat : cursor -> string -> int
(** [read_nat c what] reads a decimal natural number after any whitespace.
    Raises {!Parse_error} naming [what] if there is no number, if it is
    negative, or if it does not fit an [int]. *)

val read_count : cursor -> string -> int
(** [read_count c what] is {!read_nat}, bounded by the length of the whole
    input, the part already read included. It is for a count of items that each
    take at least one byte. Raises {!Parse_error} naming [what] if the number is
    larger. *)

val read_name : cursor -> string -> string
(** [read_name c what] reads a length-prefixed string: a natural number, one
    space, and then that many bytes, whitespace included. Raises {!Parse_error}
    naming [what] if one of the three is missing. *)

val read_word : cursor -> string -> string
(** [read_word c what] reads a maximal run of bytes that are not whitespace,
    after any whitespace. Raises {!Parse_error} naming [what] at the end of the
    input. *)

val read_identity : cursor -> identity option
(** [read_identity c] reads the identity line when the input, after any
    whitespace, starts with the three bytes [exe]. It is [None] otherwise, with
    [c] past that whitespace only. Raises {!Parse_error} if the digest is not 32
    lowercase hexadecimal digits, or if the path is malformed, truncated or
    empty. *)

val finish : cursor -> unit
(** [finish c] accepts the end of the input, after any whitespace. Raises
    {!Parse_error} if anything else remains. *)
