(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Expression-coverage runtime: accumulation, [.coverage] files, report data.

    Instrumented code calls {!register} once per source file at module load and
    {!visit} at every point; the first registration installs an [at_exit]
    handler that writes the process's data to a [.coverage] file (see
    {{!ondisk}Coverage files}). The [windtrap coverage] command loads and merges
    the files of several executables and renders {!file_reports}. This module
    computes report data only; styling and layout belong to renderers.

    Enabling coverage never changes what programs or tests mean (guarantee 10):
    visits sequence before their block or after an application returned,
    out-edge visits fire only after the application has returned and are never
    inserted in tail position, and every failure on the dump path is a warning
    on [stderr], never an altered exit code. *)

(** {1:points Points and instrumentation}

    Called by generated code only. *)

type point = { start_ofs : int; end_ofs : int }
(** The type for coverage points, a half-open byte extent
    \[[start_ofs];[end_ofs][)] in the source file following
    [Lexing.position.pos_cnum]. A point is a control-flow block's entry (a
    function's leaf body, an arm, a branch, a loop body; the extent is the
    block, an arm's spanning the whole arm) or an application's out-edge, which
    fires when the application returns (the extent is the application). Out-edge
    extents nest inside their block's; {!lines_of_extents} resolves the overlap.
    Invariant: [0 <= start_ofs <= end_ofs]. *)

val register : file:string -> points:point array -> counts:int array -> unit
(** [register ~file ~points ~counts] records [file]'s point table and the live
    [counts] array that {!visit} increments, position [i] counting visits of
    [points.(i)]. The array is kept, not copied. The first call installs the
    [at_exit] dump and resolves the output path.

    Registering [file] again with an equal point table (one source compiled into
    two modules) adds the two registrations' counts in {!snapshot} and counts
    the file's points once. A registration whose table differs from an earlier
    one for the same [file] is dropped with a warning on [stderr]: the
    executable links two incompatible instrumentations of one source, and a
    rebuild from clean is the fix.

    Raises [Invalid_argument] if [points] and [counts] differ in length, if a
    point violates the extent invariant, or if a count is negative. *)

val visit : int array -> int -> unit
(** [visit counts i] increments [counts.(i)], saturating at [max_int]. Racing
    increments from parallel domains can lose counts but never reset one.

    Raises [Invalid_argument] if [i] is outside [counts]. *)

(** {1:collections Collections}

    A collection is plain data: per source file, a point table and accumulated
    counts. *)

(** The type for coverage-data errors, all recoverable: the reporting command
    prints them via {!pp_error} and exits nonzero. *)
type error =
  | Data of Instr.error
      (** A [.coverage] file that cannot be read, does not carry this version's
          magic string, or is malformed. Other versions' files are rejected, not
          converted. *)
  | Point_mismatch of { file : string }
      (** Two collections carry different point tables for [file]: the
          executables were built from different sources. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] formats a message for [e], including the likely fix. *)

type t
(** The type for coverage collections. Immutable; file names are unique within a
    collection. *)

val empty : t
(** [empty] is the collection with no files. *)

val is_empty : t -> bool
(** [is_empty t] is [true] iff [t] has no files. A registered file with zero
    visits is data, not emptiness. *)

val add :
  t ->
  file:string ->
  points:point array ->
  counts:int array ->
  (t, error) result
(** [add t ~file ~points ~counts] is [t] with [file]'s data added: inserted
    (arrays copied) if absent, counts added with saturation if present with an
    equal point table, and [Error (Point_mismatch _)] otherwise.

    Raises [Invalid_argument] under {!register}'s malformed-table conditions. *)

val merge : t -> t -> (t, error) result
(** [merge a b] is the union of the two collections, counts added with
    saturation for shared files, and [Error (Point_mismatch _)] if any shared
    file's point tables differ. *)

val files : t -> string list
(** [files t] is the file names of [t], ordered by name. *)

val filter : (string -> bool) -> t -> t
(** [filter keep t] is [t] with only the files whose name satisfies [keep]. The
    registry {!snapshot} reads holds every instrumented library the executable
    links; a caller speaking about particular files narrows with this. *)

val snapshot : unit -> t
(** [snapshot ()] is a collection copying the current in-process counts, or
    {!empty} when nothing registered. Later {!visit}s do not affect it. *)

(** {1:ondisk Coverage files}

    At process exit an instrumented executable writes {!snapshot}'s
    serialization to a fresh file under
    {!output_dir}[ ~exe:Sys.executable_name], or to the path in
    [WINDTRAP_COVERAGE_FILE] when set (relative to the directory current at
    first {!register}), replaced atomically on every run. Nothing is written
    when nothing was registered; a dump failure prints a [windtrap coverage:]
    warning on [stderr] and changes nothing else.

    The format is versioned by the magic string [windtrap-coverage-v3] on the
    first line; other headers are rejected and cross-version compatibility is
    not promised. The magic line may be followed by the writer's {!identity},
    recorded by the [at_exit] dump on a best-effort basis; the reporting command
    uses it to exclude dumps whose executable was deleted or rebuilt since. *)

type identity = Instr.identity = { exe : string; digest : string }
(** The type for dump writer identities, {!Instr}'s re-exported: [exe] is the
    writer's {!Instr.exe_identity} and [digest] the lowercase hex MD5 of its
    contents at dump time; digesting reads the executable once at exit, off the
    test path. *)

val format : Instr.format
(** [format] is the [.coverage] format's constants: magic string, data directory
    name ([coverage]) and extension. *)

val output_dir : exe:string -> string
(** [output_dir ~exe] is the directory the executable at [exe] dumps into:
    [<build_dir>/_coverage/windtrap-<hash>] when [exe] is under a build
    directory and [<cwd>/_windtrap/coverage/windtrap-<hash>] otherwise
    ({!Instr.output_dir}). Every run writes a fresh [<digest>-<token>.coverage]
    there, named after the writer's content digest, so several runs of one
    executable all count in the merge; the first dump of a rebuilt executable
    removes the files not named after its own digest. The name depends on
    [exe]'s path: a renamed executable orphans its previous directory, which the
    reporting command detects through the recorded {!identity}. *)

val to_string : ?identity:identity -> t -> string
(** [to_string t] is [t] serialized in the [.coverage] format, files ordered by
    name so equal collections serialize identically. [identity] is recorded
    after the magic line when given: the [at_exit] dump passes it, while merged
    or synthetic collections, which have no single executable, serialize without
    one.

    Raises [Invalid_argument] if [identity.exe] is [""] or [identity.digest] is
    not 32 lowercase hex characters. *)

val of_string : ?path:string -> string -> (t * identity option, error) result
(** [of_string s] is [Ok (t, id)] when [s] parses, [id] the recorded writer
    identity if any. [path], used in errors, defaults to ["<string>"]. Errors:
    [Data (Unknown_format _)] for a foreign header (the pre-release
    [windtrap-coverage-v2] and v1's [WINDTRAP-COVERAGE-1] included),
    [Data (Corrupt _)] for truncated or invalid data (negative counts, inverted
    extents, a malformed identity line, trailing garbage), [Point_mismatch] for
    conflicting duplicate entries. [of_string (to_string ?identity t)] is
    [Ok (t, identity)]. *)

val load : string -> (t * identity option, error) result
(** [load path] reads and parses the [.coverage] file at [path].
    [Error (Data (Unreadable _))] when it cannot be read; otherwise as
    {!of_string}. *)

(** {1:reports Report data}

    Presentation-free data for the renderers. *)

type summary = { visited : int; total : int }
(** The type for point-count summaries: [visited] points visited at least once
    out of [total]. *)

val summary : t -> summary
(** [summary t] is the aggregate count over all files of [t]. *)

val percentage : summary -> float
(** [percentage s] is [100. *. visited /. total], and [100.] when [s.total] is
    [0]. *)

type file_report = {
  file : string;  (** The source file name as recorded at instrumentation. *)
  summary : summary;  (** This file's point counts. *)
  uncovered_extents : point list;
      (** The unvisited points' extents, in point-table order, whether or not
          the source was found. *)
  uncovered_lines : int list;
      (** The 1-based source lines touched by [uncovered_extents], sorted and
          without duplicates: the lines of [line_hits] with [0] hits. [[]] when
          [source] is [None]. *)
  line_hits : (int * int) list;
      (** [(line, hits)] for every 1-based line some point touches, sorted by
          line; [hits] is the fewest visits of any point touching the line, the
          rule of {!lines_of_extents} per line. [[]] when [source] is [None]. *)
  source : string option;
      (** The source text, when found under the report's roots and consistent
          with the point table. *)
  stale : bool;
      (** [true] when the source was found but is shorter than the point table's
          extents require; [source] is then [None] and [uncovered_lines] is
          [[]], and renderers report the staleness (re-running the instrumented
          tests is the fix) rather than paint lines the data does not describe.
          Edits that leave the file long enough are not detectable. *)
}
(** The type for per-file report data. *)

val file_reports : ?source_roots:string list -> t -> file_report list
(** [file_reports t] is one report per file of [t], ordered by file name. A
    file's source is looked up as recorded and under each root of [source_roots]
    in order (default [["."]]); the first readable candidate wins. A missing
    source still reports summary and extents; a source that no longer matches
    the data is reported [stale]. *)

val lines_of_extents : source:string -> point list -> int list
(** [lines_of_extents ~source extents] is the sorted, duplicate-free list of
    1-based lines of [source] that at least one extent of [extents] intersects.
    Intersecting one unvisited extent marks a line, so a line shared by visited
    and unvisited points is marked, and an unvisited inner point contributes
    only its own extent. An empty extent marks the line containing [start_ofs];
    offsets past the end of [source] clamp to its last line; an empty [source]
    gives [[]]. *)
