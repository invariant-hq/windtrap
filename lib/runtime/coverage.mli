(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The coverage runtime: the registry of points, the dump written at exit, and
    the report data.

    Code instrumented by [ppx_windtrap.coverage] calls {!register} once for each
    source file, when the module of the file loads, and {!visit} at every point.
    The [windtrap coverage] command loads the dumps of several executables,
    merges them and renders {!file_reports}.

    Coverage never changes what a program or a test means, with the one
    exception that {{!section-ondisk}Dumps} states. A dump that cannot be
    written is a warning on standard error that leaves the exit code alone, and
    nothing in this module fails a run on its coverage. *)

(** {1:points Points and instrumentation}

    Generated code is the only caller of {!register} and {!visit}. *)

type point = { start_ofs : int; end_ofs : int }
(** The type for coverage points. A point is the half-open byte extent from
    [start_ofs] to [end_ofs] in its source file, in the offsets of
    [Lexing.position.pos_cnum], with [0 <= start_ofs <= end_ofs].

    A point is the entry of a block or the out-edge of an application. An entry
    has the extent of its block, which for an arm of a [match] or of [&&] is the
    whole arm. An out-edge fires when the application returns and has the extent
    of the application, so a call that raises leaves the call uncovered.
    Out-edges nest inside their block. *)

val register : file:string -> points:point array -> counts:int array -> unit
(** [register ~file ~points ~counts] records the point table of [file] and the
    array [counts] that {!visit} increments, where [counts.(i)] counts the
    visits of [points.(i)]. [counts] is kept and not copied. [file] is the path
    of the source file as the instrumenter read it, which under dune is relative
    to the workspace root, as [lib/calc.ml].

    The first call of a process installs the [at_exit] dump and resolves where
    the dump goes (see {{!section-ondisk}Dumps}). It reads
    [WINDTRAP_COVERAGE_FILE], the current directory and {!Instr.executable}
    then, and not at exit. If it needs the current directory and cannot read it,
    a warning goes to standard error. The process then writes no dump.

    When [file] is registered again with an equal table, as when one source is
    compiled into two modules, the dump adds up the counts of the two
    registrations and counts the points of the file once. A table that differs
    from an earlier one for [file] is dropped, with a warning on standard error.

    Raises [Invalid_argument] if [points] and [counts] differ in length, if a
    point breaks the invariant of {!type-point}, or if a count is negative. Only
    a broken instrumenter produces such a table, and the exception is raised
    when the module of [file] loads. *)

val visit : int array -> int -> unit
(** [visit counts i] adds one to [counts.(i)], which saturates at [max_int].
    Increments that race from several domains can lose counts and never reset
    one, so a visited point stays visited. Raises [Invalid_argument] if [i] is
    outside [counts], which only a broken instrumenter causes. *)

(** {1:collections Collections} *)

(** The type for the errors of coverage data. *)
type error =
  | Data of Instr.error
      (** A dump cannot be read, does not start with the magic line of this
          version, or is malformed. *)
  | Point_mismatch of { file : string }
      (** Two collections carry different point tables for [file], so the
          executables were built from different sources. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] formats one line on [e] for a person. A [Data] error is
    formatted by {!Instr.pp_error}. The message is not stable enough for a
    program to match. *)

type t
(** The type for coverage collections. They are immutable, and a file name
    occurs once in a collection. *)

val empty : t
(** [empty] is the collection with no file. *)

val merge : t -> t -> (t, error) result
(** [merge a b] is the union of [a] and [b], where the counts of a file that
    both hold are added, saturating at [max_int]. It is
    [Error (Point_mismatch _)] naming the first file of [b], in the order of
    names, whose point table differs from that of [a]. *)

val files : t -> string list
(** [files t] is the file names of [t], in the order of [String.compare]. *)

(** {1:ondisk Dumps}

    When an instrumented process that registered a file exits, it writes the
    counts of the process as they stand then. The dump holds every instrumented
    library that the executable links, whether or not it is the code under test.

    By default the dump is a new file [<digest>-<token>.coverage] in the
    directory [Instr.output_dir format ~exe:Instr.executable] (see
    {!Instr.output_dir}), named after the digest of its writer. Every run keeps
    a dump of its own there, so the runs of one executable add up in the merge
    of [windtrap coverage]. The directory belongs to the runtime. A dump that
    has an identity first removes every [.coverage] file of the directory whose
    name does not start with its own digest, so the first dump of a rebuilt
    executable removes those of its predecessors. The name of the directory
    depends on the path of the executable, so an executable that is renamed or
    moved leaves its previous directory behind.

    When [WINDTRAP_COVERAGE_FILE] is set and not empty, the process writes to
    that path instead and replaces the file atomically on every run. A relative
    path is resolved against the directory that was current at the first
    {!register}.

    The dump is an [at_exit] function that runs once in a process. A forked
    child that leaves through [exit] dumps too. The counts from before the fork
    are in both dumps, so they add up twice in the merge. Under
    [WINDTRAP_COVERAGE_FILE] it is the same path, where the last process to exit
    wins and nothing is merged. The same holds for an instrumented program that
    a test spawns with the variable inherited.

    The first line of a dump is the magic line [windtrap-coverage-v3], which
    carries the version of the format. {!load} refuses another first line, and
    nothing is promised from one version to the next. The writer's
    {!type-identity} may follow the magic line. The [at_exit] dump records it
    when the executable can be read back at exit. *)

type identity = Instr.identity = { exe : string; digest : string }
(** The type for the identity of the writer of a dump, which is
    {!Instr.identity}. *)

val format : Instr.format
(** [format] is the constants of the [.coverage] format: the magic line above,
    the data directory [coverage], the extension [coverage] and the kind
    [coverage]. *)

val load : string -> (t * identity option, error) result
(** [load path] is [Ok (t, identity)] when the file at [path] is a dump, where
    [identity] is the recorded writer, if any. The file is read with
    {!Instr.read_file}, and every error names [path]. Otherwise it is:
    - [Error (Data e)] with the error [e] of the read.
    - [Error (Data (Unknown_format _))] for another first line.
    - [Error (Data (Corrupt _))] for data that is truncated or invalid: a
      negative count or a count larger than the input, an inverted extent, a
      malformed identity line, or bytes after the last record.
    - [Error (Point_mismatch _)] when two entries of one file carry different
      point tables. Two entries with an equal table are accepted, and their
      counts are added. *)

(** {1:reports Report data}

    The data below carries no presentation. *)

type summary = { visited : int; total : int }
(** The type for counts of points: [visited] points were visited at least once,
    out of [total]. *)

val summary : t -> summary
(** [summary t] is the counts over all the files of [t]. *)

type file_report = {
  file : string;
      (** The name of the source file, as recorded at instrumentation. *)
  summary : summary;  (** The counts of this file. *)
  uncovered_extents : point list;
      (** The extents of the points that were never visited, in the order of the
          point table, whether or not the source was found. *)
  uncovered_lines : int list;
      (** The 1-based lines that an unvisited extent touches, sorted and without
          duplicates. They are the lines of [line_hits] with [0] visits, and
          [[]] when [source] is [None].

          One unvisited extent is enough to mark a line, so a line that visited
          and unvisited points share is marked. An unvisited inner point marks
          the lines of its own extent only, and an unvisited outer point covers
          the lines of the points inside it. An empty extent marks the line that
          holds its [start_ofs]. *)
  line_hits : (int * int) list;
      (** [(line, visits)] for every 1-based line that a point touches, sorted
          by line. [visits] is the smallest count among the points that touch
          the line. A line that no point touches is absent. It is [[]] when
          [source] is [None]. *)
  source : string option;
      (** The text of the source, when it was found under the roots of the
          report and is consistent with the point table. *)
  stale : bool;
      (** [true] when the source was found and is shorter than the extents of
          the point table require, so it changed since the data was recorded.
          [source] is then [None], and [uncovered_lines] and [line_hits] are
          [[]]. A renderer must report the staleness and must paint no line. An
          edit that leaves the file long enough is not detected. *)
}
(** The type for the report data of one file. *)

val file_reports : ?source_roots:string list -> t -> file_report list
(** [file_reports ?source_roots t] is one report for each file of [t], in the
    order of file names. The source of a file is looked up under its recorded
    name and then under each root of [source_roots], in order. The first
    candidate that is a readable file wins, even if it is stale. [source_roots]
    defaults to [["."]]. The function reads each source from disk, once in a
    call. *)
