(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Expression-coverage runtime: accumulation, [.coverage] files, report data.

    Instrumented code (produced by [ppx_windtrap.coverage]) calls {!register}
    once per source file at module load time and {!visit} at every point. The
    first registration installs an [at_exit] handler that writes the process's
    data to a [.coverage] file under [_build/_coverage] (see
    {{!ondisk}Coverage files}). The test runner reads the same in-process data
    through {!snapshot} for its inline summary; the [windtrap coverage] command
    {!load}s and {!merge}s the files of several executables and renders
    {!file_reports}.

    This module computes report {e data} only — point counts, uncovered lines,
    percentages. Styling, layout, and printing belong to the renderers, ranges
    and excerpt regions included. Enabling coverage never changes what programs
    or tests mean: entry visits sequence before their block, out-edge visits
    fire only after the application has returned and are never inserted in tail
    position, and every failure on the dump path is a warning on [stderr], never
    an altered exit code. *)

(** {1:points Points and instrumentation}

    The functions of this section are called by PPX-generated code; user code
    and windtrap itself never call them. *)

type point = { start_ofs : int; end_ofs : int }
(** The type for coverage points, identified by a byte extent in the source
    file. A point is either a control-flow block's entry (a function leaf body,
    an arm, a branch, a loop body, a condition arm — the extent is the block, an
    arm's spanning the whole arm) or an application's out-edge, which fires only
    when the application returns (the extent is the application, so a raising
    call paints the call). Out-edge extents nest inside their enclosing block's
    extent; the report layer resolves overlap (see {!lines_of_extents}). The
    extent is half-open — \[[start_ofs];[end_ofs][)] — following
    [Lexing.position.pos_cnum]: [start_ofs] is the offset of the point's first
    byte and [end_ofs] the offset one past its last byte. Invariant:
    [0 <= start_ofs <= end_ofs]. *)

val register : file:string -> points:point array -> counts:int array -> unit
(** [register ~file ~points ~counts] records [file]'s instrumentation: [points]
    is its point table and [counts] the live array that {!visit} increments —
    position [i] of [counts] counts visits of point [points.(i)]. The array is
    kept, not copied; {!snapshot} reads it. The first call installs the
    [at_exit] dump handler and resolves the output path (see
    {{!ondisk}Coverage files}).

    Registering [file] again with a point table equal to the previous one is
    allowed (the same source file compiled into two modules); the registrations'
    counts add in {!snapshot} and the file's points are counted once. A
    registration whose table {e differs} from an earlier one for the same [file]
    means the executable links two incompatible instrumentations of one source
    file — its data would be meaningless, so the registration is dropped with a
    warning on [stderr] (a rebuild from clean is the fix). It is not an
    exception because [register] runs at module load inside the user's program,
    and coverage never changes what programs mean.

    Raises [Invalid_argument] if [points] and [counts] differ in length, if a
    point violates the extent invariant, or if a count is negative — a malformed
    table can only come from a broken instrumenter, and fails fast. *)

val visit : int array -> int -> unit
(** [visit counts i] increments [counts.(i)], saturating at [max_int]. Inserted
    by the instrumenter at block entry (a sequence prefix) or on an
    application's return (a post-visit wrapper). Racing increments from parallel
    domains can lose counts but can never reset one: a visited point stays
    visited.

    Raises [Invalid_argument] if [i] is outside [counts] (an instrumenter bug).
*)

(** {1:collections Collections}

    A collection is plain data — per source file, a point table and accumulated
    counts. Collections come from {!snapshot} (this process), {!load} (a
    [.coverage] file), or {!add} (tests, tools); {!merge} combines them. *)

(** The type for coverage-data errors. All are recoverable: the reporting
    command prints them via {!pp_error} and exits nonzero. *)
type error =
  | Data of Instr.error
      (** A [.coverage] file that cannot be read, does not carry this version's
          magic string, or is malformed. Files written by other windtrap
          versions are rejected, not converted. *)
  | Point_mismatch of { file : string }
      (** Two collections carry different point tables for source file [file] —
          the executables were built from different sources. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] formats a human-readable message for [e], including the
    likely fix. *)

type t
(** The type for coverage collections: for each instrumented source file, its
    point table and visit counts. Immutable; file names are unique within a
    collection. *)

val empty : t
(** [empty] is the collection with no files. *)

val is_empty : t -> bool
(** [is_empty t] is [true] iff [t] has no files. Note that a registered file
    with zero visits is data, not emptiness: [is_empty (snapshot ())] is [false]
    whenever instrumented code was linked. *)

val add :
  t ->
  file:string ->
  points:point array ->
  counts:int array ->
  (t, error) result
(** [add t ~file ~points ~counts] is [t] with [file]'s data added. If [file] is
    absent it is inserted (arrays are copied); if present with an equal point
    table, counts add with saturation; otherwise the result is
    [Error (Point_mismatch _)].

    Raises [Invalid_argument] under {!register}'s malformed-table conditions. *)

val merge : t -> t -> (t, error) result
(** [merge a b] combines two collections: the union of their files, counts added
    with saturation for files present in both. [Error (Point_mismatch _)] if any
    shared file's point tables differ — merging is loud, never silently wrong.
*)

val files : t -> string list
(** [files t] is the file names of [t], ordered by name. *)

val filter : (string -> bool) -> t -> t
(** [filter keep t] is [t] with only the files whose name satisfies [keep].

    The registry {!snapshot} reads is process-global: every instrumented library
    linked into an executable is in it, whether or not it is the code under
    test. A caller that means to speak about {e particular} files — a test
    asserting on its own fixture's counts, say — narrows with this rather than
    assuming the process contains nothing else. *)

val snapshot : unit -> t
(** [snapshot ()] is a collection copying the current in-process counts;
    {!empty} when nothing registered. Later {!visit}s do not affect the returned
    value. The runner calls this at run end for the inline summary.

    This is the whole process; {!filter} narrows it. *)

(** {1:ondisk Coverage files}

    At process exit, an instrumented executable writes {!snapshot}'s
    serialization to a fresh file under {!output_dir}[ ~exe:Sys.executable_name]
    — or to the path in the [WINDTRAP_COVERAGE_FILE] environment variable when
    set (relative paths resolve against the directory current at first
    {!register}), which is replaced atomically on every run. Under {!output_dir}
    every run keeps its own file, so the runs of one executable add up in the
    reporting command's merge, and the first dump of a rebuilt executable
    removes the files its predecessors wrote. Nothing is written when nothing
    was registered. A dump failure prints a [windtrap coverage:] warning on
    [stderr] and changes nothing else — never the exit code.

    The format is versioned by the magic string [windtrap-coverage-v3] on the
    first line; {!of_string} and {!load} reject any other header loudly, and
    cross-version compatibility is not promised. The magic line may be followed
    by the writing executable's {!identity} — the at_exit dump records it (best
    effort); the reporting command uses it to exclude dumps whose executable was
    deleted or rebuilt since the run. *)

type identity = Instr.identity = { exe : string; digest : string }
(** The type for dump writer identities — [Instr]'s, re-exported, so the
    reporting command handles both formats' identities with one pass: [exe] is
    the writing executable's {!Instr.exe_identity} and [digest] the lowercase
    hex MD5 of its contents at dump time. An executable at [exe] whose digest
    differs is {e not} the one that wrote the dump. Digesting reads the
    executable once at exit (a few milliseconds for a typical test binary), off
    the test path. *)

val output_dir : exe:string -> string
(** [output_dir ~exe] is the directory the executable at path [exe] dumps into:
    [<root>/_build/_coverage/windtrap-<hash>], with [<root>] and [<hash>] by
    {!Instr.output_dir}'s rule. Every run writes a fresh
    [<digest>-<token>.coverage] there, named after the writer's content digest,
    so several runs of one executable — a command-line tool driven by a cram
    test, say — all count in the merge. The directory belongs to the runtime:
    the first dump of a rebuilt executable removes the files not named after its
    own digest, its predecessors', so a rebuild never leaves a stale dump
    behind. The name depends on the executable's path, so renaming or moving a
    test executable orphans its previous directory; the reporting command
    detects the orphans through the recorded {!identity} and excludes them with
    a warning. *)

val to_string : ?identity:identity -> t -> string
(** [to_string t] is [t] serialized in the [.coverage] format. Deterministic:
    files are ordered by name, so equal collections serialize identically
    regardless of construction order. [identity] is recorded after the magic
    line when given; the at_exit dump passes it, while merged or synthetic
    collections, which have no single executable, serialize without one.

    Raises [Invalid_argument] if [identity.exe] is [""] or [identity.digest] is
    not 32 lowercase hex characters. *)

val of_string : ?path:string -> string -> (t * identity option, error) result
(** [of_string s] is [Ok (t, id)] when [s] parses: [t] the collection and [id]
    the recorded writer identity, [None] when [s] carries none. [path], used in
    errors, defaults to ["<string>"]. Errors: [Data (Unknown_format _)] for a
    foreign header (including the pre-release [windtrap-coverage-v2] and v1's
    [WINDTRAP-COVERAGE-1]), [Data (Corrupt _)] for truncated or invalid data
    (negative counts, inverted extents, a malformed identity line, trailing
    garbage), [Point_mismatch] for conflicting duplicate entries.

    Round trip: [of_string (to_string ?identity t)] is [Ok (t, identity)]. *)

val load : string -> (t * identity option, error) result
(** [load path] reads and parses the [.coverage] file at [path].
    [Error (Data (Unreadable _))] when the file cannot be read; otherwise as
    {!of_string}. *)

(** {1:reports Report data}

    Everything below is presentation-free data for the renderers: the counts
    behind the terminal summary line and the per-file table. Ranges and excerpt
    regions are layout, and live with the renderer that draws them. *)

type summary = { visited : int; total : int }
(** The type for point-count summaries: [visited] points visited at least once
    out of [total] instrumented points. *)

val summary : t -> summary
(** [summary t] is the aggregate count over all files of [t]. *)

val percentage : summary -> float
(** [percentage s] is [100. *. visited /. total], and [100.] when [s.total] is
    [0]. *)

type file_report = {
  file : string;  (** The source file name as recorded at instrumentation. *)
  summary : summary;  (** This file's point counts. *)
  uncovered_extents : point list;
      (** The unvisited points' extents, in point-table order. Available whether
          or not the source was found. *)
  uncovered_lines : int list;
      (** The 1-based source lines touched by [uncovered_extents], sorted,
          without duplicates — the lines of [line_hits] with [0] hits. [[]] when
          [source] is [None]. *)
  line_hits : (int * int) list;
      (** [(line, hits)] for every 1-based source line some point touches,
          sorted by line: [hits] is the fewest visits of any point touching the
          line, so a line holding an untested arm or a call that never returned
          has [0] hits and is in [uncovered_lines] — the same rule as
          {!lines_of_extents}, per line. Lines no point touches are absent. [[]]
          when [source] is [None]. *)
  source : string option;
      (** The source text, when found under the report's roots and consistent
          with the point table; excerpt rendering needs it. *)
  stale : bool;
      (** [true] when the source was found but is shorter than the point table's
          extents require — it changed since the data was recorded. [source] is
          then [None] and [uncovered_lines] is [[]]: renderers must report the
          staleness (re-running the instrumented tests is the fix) rather than
          paint lines of code the data does not describe. Edits that leave the
          file long enough are not detectable. *)
}
(** The type for per-file report data. *)

val file_reports : ?source_roots:string list -> t -> file_report list
(** [file_reports t] is one report per file of [t], ordered by file name. Each
    file's source is looked up as recorded and under each root of [source_roots]
    in order (default [["."]], the project root when run via [dune exec]); the
    first readable candidate wins. A file whose source is missing still reports
    its summary and extents; a source that no longer matches the data is
    rejected as [stale] rather than mis-reported. *)

val lines_of_extents : source:string -> point list -> int list
(** [lines_of_extents ~source extents] is the sorted, duplicate-free list of
    1-based lines of [source] that at least one extent of [extents] intersects.
    This is the rule that turns uncovered points into uncovered lines:

    - Nesting needs no special case. An unvisited inner point inside a visited
      outer one — an out-edge whose enclosing block was entered, an arm of a
      visited [match] — contributes only its own extent (the visited outer one
      is not in the uncovered set): the inner extent overrides the outer. An
      unvisited outer point subsumes its inner points, whose extents its own
      contains.
    - A line shared by visited and unvisited points — a one-line
      [match]/[function], say [let f = function A -> 1 | B -> 2] with only [A]
      exercised, or a covered call whose enclosing line holds an uncovered
      out-edge — {e is} marked: intersecting one unvisited extent suffices. The
      alternative (marking only lines wholly inside unvisited extents) would
      hide the unvisited arm entirely. The summary is unaffected either way; it
      counts points, not lines.
    - An empty extent marks the line containing [start_ofs]; offsets past the
      end of [source] clamp to its last line; if [source] is empty the result is
      [[]]. *)
