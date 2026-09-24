(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Mutation verdicts: what a run made of each mutant, and the [.mutants] file
    that carries the verdicts of one executable to the merge.

    The mutation loop writes one verdict file for each test executable.
    [windtrap mutants] loads the files of a project and combines them with
    {!merge}, under which a mutant that is killed anywhere is killed. The file
    exists to be merged, because a library is often tested by several
    executables. Instrumented code never holds a verdict, because a verdict is
    earned by a run that asked for one. Nothing here runs when the module loads
    or when the process exits. The module prints nothing, and where the text of
    {!pp_error} shows is the contract of its caller. *)

(** {1:verdicts Verdicts}

    A verdict is one of three cases, and never a boolean or an exit code. A
    failure to supervise the child that arms a mutant is none of the three, and
    a caller must record no verdict for it. *)

type witness = string list
(** The type for the path of a reaching test: the names of its groups, the
    outermost first, and then its own, as [["arithmetic"; "adds"]]. *)

(** The type for verdicts. A survivor always names a reaching test, because a
    mutant that no test reached is {!Unreached}. *)
type verdict =
  | Killed
      (** A reaching test failed, or the child that armed the mutant crashed or
          hung. Each is a change of behaviour that the suite detected, and a
          report counts them as one number. *)
  | Survived of { witness : witness; others : witness list }
      (** Every reaching test passed. [witness] and [others] are the reaching
          tests, sorted and without duplicates, and [witness] is the first. The
          remedy is to strengthen one of them. {!survived} builds the value, and
          one that is built by hand is sorted when it passes through
          {!merge_verdict}, {!add} or {!of_string}. *)
  | Unreached
      (** No test evaluated the site, so the loop forks no child for the mutant.
          It never counts as a survivor, and the remedy is to write a test. *)

val survived : witness list -> verdict
(** [survived ws] is the {!Survived} verdict whose reaching tests are [ws],
    sorted and without duplicates. Raises [Invalid_argument] if [ws] is empty.
*)

val merge_verdict : verdict -> verdict -> verdict
(** [merge_verdict a b] is the verdict of a mutant that one executable saw as
    [a] and another as [b]. It is {!Killed} if either is. Otherwise it is
    {!Survived} with the reaching tests of both if either is, and {!Unreached}
    if both are. A mutant that one suite kills and another only reaches is
    killed, because a report of the second view alone would send its reader to
    write a test that exists.

    [merge_verdict] is commutative, associative and idempotent, and {!Unreached}
    is its unit, so merging any number of files in any order gives one answer.
    The laws hold up to the sorting of a {!Survived} that was built by hand. *)

(** {1:collections Collections} *)

type record = {
  id : Mutate.id;  (** The identifier of the mutant. *)
  before : string;  (** The source text of the original expression. *)
  after : string;  (** The source text of the armed expression. *)
  verdict : verdict;  (** What the run made of the mutant. *)
}
(** The type for the record of one mutant. A record is self-describing: it
    carries the two renderings that a report draws the mutant with, so a report
    needs no catalogue and outlives the executable that produced it. *)

val record_of_mutant : Mutate.mutant -> verdict -> record
(** [record_of_mutant m v] is the record of [m] with the verdict [v].
    [m.dismissed] is dropped. *)

type t
(** The type for verdict collections: finite maps from {!Mutate.type-id} to
    {!type-record}, ordered by {!Mutate.compare_id}. They are immutable. *)

val empty : t
(** [empty] is the collection with no record. *)

val add : t -> record -> t
(** [add t r] is [t] with [r] recorded. When [t] already holds a record under
    [r.id], the two verdicts are combined with {!merge_verdict}, and the smaller
    [(before, after)] pair in lexicographic order is kept. A {!Survived} verdict
    is sorted on its way in. These rules make [add] and {!merge} commutative,
    associative and idempotent, so a report never depends on the order in which
    files are read.

    [add] checks nothing of [r.id]. It accepts an empty [file], a [line] below
    [1], a negative [col] and a [rewrite] outside {!Mutate.rewrites}, and
    {!of_string} then refuses what {!to_string} writes for the collection. *)

val records : t -> record list
(** [records t] is the records of [t], ordered by {!Mutate.compare_id}. *)

val merge : t -> t -> t
(** [merge a b] is the union of [a] and [b], where two records that share an
    identifier are combined as {!add} does. {!empty} is its unit. *)

(** {1:files Verdict files}

    An executable has one verdict file, at {!output_file}. A mutation run that
    tests the whole suite of the executable replaces the file, where a coverage
    dump adds a file on every run. The file holds one record for each mutant of
    the catalogue that is in the scope of the run and is not dismissed by
    [[@mutate off]]. A reached mutant has its verdict, and every other one is
    {!Unreached}.

    A run whose selection narrows the suite leaves the file alone. A run under
    [--mutate=PREFIX] does replace the file, with the records of the mutants
    under the prefix only. The records that an earlier run wrote for the other
    source files are then gone until the next run without a prefix.

    The first line of a file is the magic line [windtrap-mutants-v3], which
    carries the version of the format. A file with another first line is
    refused, never converted, and nothing is promised from one version to the
    next. The writer's {!type-identity} may follow the magic line. *)

(** The type for the errors of reading a verdict file, which is {!Instr.error},
    where each case is described. There is no error of disagreement, because
    {!merge_verdict} is total: two files may differ about a mutant without
    either being corrupt. *)
type error = Instr.error =
  | Unknown_format of { path : string; header : string }
  | Unreadable of { path : string; reason : string }
  | Corrupt of { path : string; reason : string }

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] is [Instr.pp_error format ppf e]. It calls the file a
    verdict file, and its remedy is to delete the stale verdict files and to run
    the mutation tests again. *)

type identity = Instr.identity = { exe : string; digest : string }
(** The type for the identity of the writer of a verdict file, which is
    {!Instr.identity}. *)

val format : Instr.format
(** [format] is the constants of the [.mutants] format: the magic line above,
    the data directory [mutants], the extension [mutants] and the kind
    [verdict]. [windtrap mutants] discovers the files with it (see
    {!Instr.data_dir}). *)

val exe_identity : exe:string -> string
(** [exe_identity ~exe] is [Instr.exe_identity ~exe], the [exe] that a verdict
    file records for the executable at [exe]. *)

val writer_identity : exe:string -> identity option
(** [writer_identity ~exe] is the identity to record for the executable at
    [exe], or [None] when [exe] cannot be read. *)

val build_root : path:string -> string option
(** [build_root ~path] is [Instr.build_root ~path]. *)

val output_file : exe:string -> string
(** [output_file ~exe] is [Instr.output_file format ~exe], the one verdict file
    of the executable at [exe]. An executable that is renamed or moved leaves
    its previous file behind. *)

val to_string : ?identity:identity -> t -> string
(** [to_string ?identity t] is [t] in the format of a verdict file. Records are
    ordered by {!Mutate.compare_id} and reaching tests are sorted, so equal
    collections give equal strings. [identity] is recorded after the magic line
    when it is given. A merged collection has no single writer and is written
    without one. Raises [Invalid_argument] if [identity.exe] is empty or if
    [identity.digest] is not 32 lowercase hexadecimal digits. *)

val of_string : ?path:string -> string -> (t * identity option, error) result
(** [of_string ?path s] is [Ok (t, identity)] when [s] parses, where [identity]
    is the recorded writer, if any. [path] names the input in errors and
    defaults to ["<string>"]. It is [Error (Unknown_format _)] for another first
    line, and [Error (Corrupt _)] for data that is truncated or invalid:
    - a negative count, or a count larger than the input,
    - an empty file name, a line below [1] or a negative column,
    - a rewrite outside {!Mutate.rewrites}, or an unknown verdict,
    - a survivor with no reaching test, or a second record for one identifier,
    - a malformed identity line, or bytes after the last record.

    Nothing is repaired, and a file with one malformed record is not read at
    all.

    [of_string (to_string ?identity t)] is [Ok (t, identity)] for every [t]
    whose identifiers [of_string] accepts, and [Error (Corrupt _)] for any other
    (see {!add}). A collection built from the mutants of {!Mutate.val-catalogue}
    round-trips, unless a source file was registered under the name [""]. *)

val load : string -> (t * identity option, error) result
(** [load path] reads the file at [path] with {!Instr.read_file} and parses it
    as [of_string ~path] does. *)

val save : ?identity:identity -> string -> t -> unit
(** [save ?identity path t] writes [to_string ?identity t] to [path] with
    {!Instr.write_file}, so a reader never sees a partial file and a run that
    crashes never leaves a truncated one. A new run replaces the file, so
    verdicts never add up on disk.

    Raises [Invalid_argument] as {!to_string} does, before anything is written,
    and [Sys_error] if the file cannot be written. *)
