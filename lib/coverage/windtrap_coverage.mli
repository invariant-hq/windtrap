(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   Portions adapted from Bisect_ppx (MIT license).
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Coverage data collection and reporting.

    Instrumented code calls {!register_file} at module load time; visit
    functions increment counters during execution; an [at_exit] handler writes
    [.coverage] files to disk. The test runner reads {!data} directly for inline
    reporting. *)

(** {1 Types} *)

type instrumented_file = {
  filename : string;
  points : int array;
  counts : int array;
}

type coverage = (string, instrumented_file) Hashtbl.t

(** {1 Output Configuration} *)

val set_output_prefix : string -> unit
(** [set_output_prefix path] sets the directory and filename prefix for
    [.coverage] files written at exit. For example,
    [set_output_prefix "/project/_build/_coverage/windtrap"] writes files like
    [/project/_build/_coverage/windtrap123456789.coverage]. The directory is
    created automatically when coverage data is written. Can be overridden by
    the [WINDTRAP_COVERAGE_FILE] environment variable. *)

(** {1 Registration} *)

val register_file :
  filename:string -> points:int array -> [ `Visit of int -> unit ]
(** Register an instrumented source file. Called by PPX-generated code at module
    load time. Returns a visit function that increments the counter for a given
    point index. Registers an [at_exit] handler on first call. *)

(** {1 Data Access} *)

val data : unit -> coverage
(** Return the accumulated in-memory coverage data. Empty if no instrumented
    files have been registered. *)

val reset_counters : unit -> unit
(** Reset all counters to zero without removing registered files. *)

(** {1 Serialization} *)

val file_identifier : string
(** Magic header for [.coverage] files. *)

val serialize : coverage -> string
(** Serialize coverage data to the binary format used by [.coverage] files. *)

(** {1 Deserialization} *)

val read_file : string -> instrumented_file list
(** Read a single [.coverage] file. Returns an empty list if the file has an
    invalid header, is unreadable, or is truncated. *)

(** {1 Merging} *)

val merge : coverage -> instrumented_file list -> unit
(** [merge tbl files] folds [files] into [tbl], using saturating addition when
    the same source file appears more than once. *)

(** {1 Reporting} *)

type summary = { visited : int; total : int }

type file_report = {
  filename : string;
  summary : summary;
  uncovered_offsets : int list;
  uncovered_lines : int list;
  source_available : bool;
}

val summarize : coverage -> summary
(** Compute aggregate (visited, total) counts across all files. *)

val summarize_per_file : coverage -> (string * summary) list
(** Per-file summaries sorted by filename. *)

val percentage : summary -> float
(** [percentage s] is [100.0] when [s.total = 0]. *)

val coverage_style : summary -> [ `Green | `Yellow | `Red ]
(** Return a color style based on coverage percentage: green >= 80%, yellow >=
    60%, red < 60%. Useful with {!Pp.styled} for inline output. *)

val collapse_ranges : int list -> (int * int) list
(** [collapse_ranges lines] collapses a sorted list of line numbers into
    contiguous ranges. E.g., [[1; 2; 3; 7; 8]] becomes [[(1, 3); (7, 8)]]. *)

val format_ranges : (int * int) list -> string
(** [format_ranges ranges] formats ranges as ["1-3, 7-8"]. Single lines appear
    without a dash. *)

val reports : ?source_paths:string list -> coverage -> file_report list
(** Build deterministic per-file coverage reports with uncovered points and
    uncovered line numbers.

    [source_paths] is used to resolve source files for line mapping. When a
    source file can't be found or read, [uncovered_lines] is empty and
    [source_available] is [false]. *)

val print_summary :
  per_file:bool ->
  skip_covered:bool ->
  ?source_paths:string list ->
  coverage ->
  unit
(** Print a human-readable coverage report to stdout with color-coded
    percentages. When [per_file] is true and [source_paths] is provided,
    uncovered line ranges are shown below each file. When [skip_covered] is
    true, files with 100% coverage are omitted from the per-file report. *)

val print_uncovered :
  ?context:int -> ?source_paths:string list -> coverage -> unit
(** Print uncovered source code snippets with surrounding context lines. Each
    uncovered line is prefixed with [>] and highlighted. Context lines are
    dimmed. Only files with uncovered lines and available source are shown.
    [context] defaults to [1]. *)
