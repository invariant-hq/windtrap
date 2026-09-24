(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The coverage runtime: the registry of points, the dump written at exit, and
    the report data.

    Code instrumented by [ppx_windtrap.coverage] calls {!register} once for each
    source file, when the module of the file loads, and {!visit} at every point.
    The first registration installs an [at_exit] function, which writes the
    counts of the process to a [.coverage] dump. The [windtrap coverage] command
    loads the dumps of several executables, merges them and renders
    {!file_reports}. {!snapshot} gives a test or a tool the counts of its own
    process. This module computes the data of a report, which is counts of
    points, uncovered lines and percentages, and it renders nothing.

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
    Out-edges nest inside their block, and {!lines_of_extents} says what lines
    follow from extents that overlap. *)

val register : file:string -> points:point array -> counts:int array -> unit
(** [register ~file ~points ~counts] records the point table of [file] and the
    array [counts] that {!visit} increments, where [counts.(i)] counts the
    visits of [points.(i)]. [counts] is kept and not copied. [file] is the path
    of the source file as the instrumenter read it, which under dune is relative
    to the workspace root, as [lib/calc.ml]. It is the key of every collection,
    and the name under which {!file_reports} looks for the source.

    The first call of a process installs the [at_exit] dump and resolves where
    the dump goes (see {{!section-ondisk}Dumps}). It reads
    [WINDTRAP_COVERAGE_FILE], the current directory and [Sys.executable_name]
    then, and not at exit. If it needs the current directory and cannot read it,
    a warning goes to standard error. The process then writes no dump.

    When [file] is registered again with an equal table, as when one source is
    compiled into two modules, {!snapshot} adds up the counts of the two
    registrations and counts the points of the file once. A table that differs
    from an earlier one for [file] is dropped, with a warning on standard error.
    The executable links two incompatible instrumentations of one source, and
    rebuilding from scratch is the remedy.

    Each warning is one line behind [windtrap: warning:], written when the
    module loads and whatever the flags of a run.

    Raises [Invalid_argument] if [points] and [counts] differ in length, if a
    point breaks the invariant of {!type-point}, or if a count is negative. Only
    a broken instrumenter produces such a table, and the exception is raised
    when the module of [file] loads. *)

val visit : int array -> int -> unit
(** [visit counts i] adds one to [counts.(i)], which saturates at [max_int].
    Increments that race from several domains can lose counts and never reset
    one, so a visited point stays visited. Raises [Invalid_argument] if [i] is
    outside [counts], which only a broken instrumenter causes. *)

(** {1:collections Collections}

    A collection is plain data: for each source file, a point table and the
    counts accumulated for it. Collections come from {!snapshot} for this
    process, from {!load} for a dump, and from {!add}. {!merge} combines them.
*)

(** The type for the errors of coverage data. *)
type error =
  | Data of Instr.error
      (** A dump cannot be read, does not start with the magic line of this
          version, or is malformed. A dump of another version is refused, never
          converted. *)
  | Point_mismatch of { file : string }
      (** Two collections carry different point tables for [file], so the
          executables were built from different sources. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] formats one line on [e] for a person. A [Data] error is
    formatted by {!Instr.pp_error}, and the message of a {!Point_mismatch} ends
    with a remedy. It prints nothing, and where its text shows is the contract
    of its caller. The message is not stable enough for a program to match. *)

type t
(** The type for coverage collections. They are immutable, and a file name
    occurs once in a collection. *)

val empty : t
(** [empty] is the collection with no file. *)

val is_empty : t -> bool
(** [is_empty t] is [true] iff [t] has no file. *)

val add :
  t ->
  file:string ->
  points:point array ->
  counts:int array ->
  (t, error) result
(** [add t ~file ~points ~counts] is [t] with the data of [file] added. When [t]
    has no [file], copies of the two arrays go in. When [t] has [file] with an
    equal point table, the counts are added, saturating at [max_int]. Otherwise
    it is [Error (Point_mismatch _)]. Raises [Invalid_argument] as {!register}
    does for a malformed table. *)

val merge : t -> t -> (t, error) result
(** [merge a b] is the union of [a] and [b], where the counts of a file that
    both hold are added, saturating at [max_int]. It is
    [Error (Point_mismatch _)] naming the first file of [b], in the order of
    names, whose point table differs from that of [a]. *)

val files : t -> string list
(** [files t] is the file names of [t], in the order of [String.compare]. *)

val filter : (string -> bool) -> t -> t
(** [filter keep t] is [t] with only the files whose name satisfies [keep].
    {!snapshot} holds every instrumented library that the executable links,
    whether or not it is the code under test, so a caller that speaks of
    particular files narrows with [filter]. *)

val snapshot : unit -> t
(** [snapshot ()] is a collection that copies the counts of this process as they
    stand, or {!empty} when nothing has registered. A later {!visit} does not
    change it. It covers the whole process, and it is what the [at_exit] dump
    serializes. *)

(** {1:ondisk Dumps}

    When an instrumented process exits, it writes the serialization of
    {!snapshot} to a new file under [output_dir ~exe:Sys.executable_name]. Every
    run keeps a dump of its own there, so the runs of one executable add up in
    the merge of [windtrap coverage]. When [WINDTRAP_COVERAGE_FILE] is set and
    not empty, the process writes to that path instead and replaces the file
    atomically on every run. A relative path is resolved against the directory
    that was current at the first {!register}.

    The dump is an [at_exit] function that runs once in a process. A forked
    child that leaves through [exit] dumps too. Under {!output_dir} that is one
    more file. The counts from before the fork are in both dumps, so they add up
    twice in the merge. Under [WINDTRAP_COVERAGE_FILE] it is the same path,
    where the last process to exit wins and nothing is merged. The same holds
    for an instrumented program that a test spawns with the variable inherited.

    A dump that cannot be written is one line on standard error, behind
    [windtrap: warning:], written at exit, after the report of a run and
    whatever its flags. It changes nothing else, and never the exit code.

    Only the write is protected that way. The dump builds its string first, and
    an executable that lies below no build directory and whose own file name
    starts with [_build] has the identity [""] (see {!Instr.exe_identity}).
    {!to_string} then raises [Invalid_argument] at exit, and the process ends on
    that exception, whatever exit code it was ending with. An executable that
    runs from dune's build directory is never in that case.

    The first line of a dump is the magic line [windtrap-coverage-v3], which
    carries the version of the format. {!of_string} and {!load} refuse another
    first line, and nothing is promised from one version to the next. The
    writer's {!type-identity} may follow the magic line. The [at_exit] dump
    records it when the executable can be read back at exit. *)

type identity = Instr.identity = { exe : string; digest : string }
(** The type for the identity of the writer of a dump, which is
    {!Instr.identity}. *)

val format : Instr.format
(** [format] is the constants of the [.coverage] format: the magic line above,
    the data directory [coverage], the extension [coverage] and the kind
    [coverage]. [windtrap coverage] discovers the dumps with it (see
    {!Instr.data_dir}). *)

val output_dir : exe:string -> string
(** [output_dir ~exe] is [Instr.output_dir format ~exe], the directory that the
    executable at [exe] dumps into.

    Every run writes a new [<digest>-<token>.coverage] there, named after the
    digest of its writer, so a cram test that runs a command-line tool several
    times leaves as many dumps. The directory belongs to the runtime. A dump
    that has an identity first removes every [.coverage] file of the directory
    whose name does not start with its own digest, so the first dump of a
    rebuilt executable removes those of its predecessors.

    The name of the directory depends on the path of [exe], so an executable
    that is renamed or moved leaves its previous directory behind. *)

val to_string : ?identity:identity -> t -> string
(** [to_string ?identity t] is [t] in the format of a dump. Files are ordered by
    name, so equal collections give equal strings. [identity] is recorded after
    the magic line when it is given. A merged or synthetic collection has no
    single writer and is written without one. Raises [Invalid_argument] if
    [identity.exe] is empty or if [identity.digest] is not 32 lowercase
    hexadecimal digits. *)

val of_string : ?path:string -> string -> (t * identity option, error) result
(** [of_string ?path s] is [Ok (t, identity)] when [s] parses, where [identity]
    is the recorded writer, if any. [path] names the input in errors and
    defaults to ["<string>"]. Otherwise it is:
    - [Error (Data (Unknown_format _))] for another first line.
    - [Error (Data (Corrupt _))] for data that is truncated or invalid: a
      negative count or a count larger than the input, an inverted extent, a
      malformed identity line, or bytes after the last record.
    - [Error (Point_mismatch _)] when two entries of one file carry different
      point tables. Two entries with an equal table are accepted, and their
      counts are added.

    [of_string (to_string ?identity t)] is [Ok (t, identity)]. *)

val load : string -> (t * identity option, error) result
(** [load path] reads the file at [path] with {!Instr.read_file} and parses it
    as [of_string ~path] does, with the errors of the read under {!Data}. *)

(** {1:reports Report data}

    The data below carries no presentation. Ranges, excerpts and styles are the
    choices of a renderer. *)

type summary = { visited : int; total : int }
(** The type for counts of points: [visited] points were visited at least once,
    out of [total]. *)

val summary : t -> summary
(** [summary t] is the counts over all the files of [t]. *)

val percentage : summary -> float
(** [percentage s] is [100. *. visited /. total], and [100.] when [s.total] is
    [0]. *)

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
          [[]] when [source] is [None]. *)
  line_hits : (int * int) list;
      (** [(line, visits)] for every 1-based line that a point touches, sorted
          by line. [visits] is the smallest count among the points that touch
          the line, so a line that holds an untested arm, or a call that never
          returned, has [0]. A line that no point touches is absent. It is [[]]
          when [source] is [None]. *)
  source : string option;
      (** The text of the source, when it was found under the roots of the
          report and is consistent with the point table. *)
  stale : bool;
      (** [true] when the source was found and is shorter than the extents of
          the point table require, so it changed since the data was recorded.
          [source] is then [None], and [uncovered_lines] and [line_hits] are
          [[]]. A renderer must report the staleness, whose remedy is to run the
          instrumented tests again, and must paint no line. An edit that leaves
          the file long enough is not detected. *)
}
(** The type for the report data of one file. *)

val file_reports : ?source_roots:string list -> t -> file_report list
(** [file_reports ?source_roots t] is one report for each file of [t], in the
    order of file names. The source of a file is looked up under its recorded
    name and then under each root of [source_roots], in order. The first
    candidate that is a readable file wins, even if it is stale. [source_roots]
    defaults to [["."]]. A file whose source is missing still reports its
    summary and its extents. The function reads each source from disk, once in a
    call. *)

val lines_of_extents : source:string -> point list -> int list
(** [lines_of_extents ~source extents] is the 1-based lines of [source] that an
    extent of [extents] intersects, sorted and without duplicates. It is the
    rule that turns uncovered points into uncovered lines.

    One unvisited extent is enough to mark a line, so a line that visited and
    unvisited points share is marked, as the line of
    [let f = function A -> 1 | B -> 2] when only [A] was exercised. An unvisited
    inner point marks the lines of its own extent only, and an unvisited outer
    point covers the lines of the points inside it. The summary counts points
    and not lines, so this rule does not change it.

    An empty extent marks the line that holds [start_ofs]. An offset past the
    end of [source] counts as its last line, and an empty [source] gives [[]].
*)
