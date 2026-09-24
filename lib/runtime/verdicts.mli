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

type reaching_test = string list
(** The type for a test that evaluated a mutant, by its path: the names of its
    groups, the outermost first, and then its own, as [["arithmetic"; "adds"]].
*)

(** The type for verdicts. A survivor always names a reaching test, because a
    mutant that no test reached is {!Unreached}. *)
type verdict =
  | Killed
      (** A reaching test failed, or the child that armed the mutant crashed or
          hung. Each is a change of behaviour that the suite detected, and a
          report counts them as one number. *)
  | Survived of { first : reaching_test; others : reaching_test list }
      (** Every reaching test passed. [first] and [others] are the reaching
          tests, sorted and without duplicates, and [first] is the first. The
          remedy is to strengthen one of them. {!survived} builds the value, and
          one that is built by hand is sorted when it passes through {!add}. *)
  | Unreached
      (** No test evaluated the site, so the loop forks no child for the mutant.
          It never counts as a survivor, and the remedy is to write a test. *)

val survived : reaching_test list -> verdict
(** [survived ts] is the {!Survived} verdict whose reaching tests are [ts],
    sorted and without duplicates. Raises [Invalid_argument] if [ts] is empty.
*)

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
(** [add t r] is [t] with [r] recorded. A {!Survived} verdict is sorted on its
    way in. When [t] already holds a record under [r.id], the smaller
    [(before, after)] pair in lexicographic order is kept, and the two verdicts
    combine into the verdict of a mutant that one executable saw one way and
    another the other way. It is {!Killed} if either is. Otherwise it is
    {!Survived} with the reaching tests of both if either is, and {!Unreached}
    if both are. A mutant that one suite kills and another only reaches is
    killed, because a report of the second view alone would send its reader to
    write a test that exists.

    [add] checks nothing of [r.id]. It accepts an empty [file], a [line] below
    [1], a negative [col] and a [rewrite] outside {!Mutate.rewrites}, and
    {!load} then refuses the file that {!save} writes for the collection. *)

val records : t -> record list
(** [records t] is the records of [t], ordered by {!Mutate.compare_id}. *)

val merge : t -> t -> t
(** [merge a b] is the union of [a] and [b], where two records that share an
    identifier are combined as {!add} does. [merge] is commutative, associative
    and idempotent, {!empty} is its unit, and an {!Unreached} verdict leaves the
    verdict it is combined with unchanged, so merging any number of files in any
    order gives one answer. *)

(** {1:files Verdict files}

    An executable has one verdict file, at {!output_file}. A mutation run that
    tests the whole suite of the executable replaces the file, where a coverage
    dump adds a file on every run. The file holds one record for each mutant of
    the catalogue that is in the scope of the run and is not dismissed by
    [[@mutate off]]. A reached mutant has its verdict, and every other one is
    {!Unreached}.

    A run whose selection narrows the suite leaves the file alone. A run under
    [--mutate=PREFIX] replaces the records of the mutants under the prefix. It
    keeps the records of the other source files when the file was written by the
    same build, with an equal {!type-identity}, and it replaces a file that
    another build wrote, or that it cannot read, whole.

    The first line of a file is the magic line [windtrap-mutants-v3], which
    carries the version of the format. A file with another first line is
    refused, never converted, and nothing is promised from one version to the
    next. The writer's {!type-identity} may follow the magic line. *)

(** The type for the errors of reading a verdict file, which is {!Instr.error},
    where each case is described. There is no error of disagreement, because
    {!merge} is total: two files may differ about a mutant without either being
    corrupt. *)
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

val writer_identity : exe:string -> identity option
(** [writer_identity ~exe] is the identity to record for the executable at
    [exe], whose [exe] is [Instr.exe_identity ~exe], or [None] when [exe] cannot
    be read. *)

val output_file : exe:string -> string
(** [output_file ~exe] is [Instr.output_file format ~exe], the one verdict file
    of the executable at [exe]. An executable that is renamed or moved leaves
    its previous file behind. *)

val load : string -> (t * identity option, error) result
(** [load path] is [Ok (t, identity)] when the file at [path] is a verdict file,
    where [identity] is the recorded writer, if any. The file is read with
    {!Instr.read_file}, and every error names [path]. Otherwise it is the error
    of the read, [Error (Unknown_format _)] for another first line, or
    [Error (Corrupt _)] for data that is truncated or invalid:
    - a negative count, or a count larger than the input,
    - an empty file name, a line below [1] or a negative column,
    - a rewrite outside {!Mutate.rewrites}, or an unknown verdict,
    - a survivor with no reaching test, or a second record for one identifier,
    - a malformed identity line, or bytes after the last record.

    Nothing is repaired, and a file with one malformed record is not read at
    all. *)

val save : ?identity:identity -> string -> t -> unit
(** [save ?identity path t] writes [t] to [path] in the format of a verdict
    file, with {!Instr.write_file}, so a reader never sees a partial file and a
    run that crashes never leaves a truncated one. A new run replaces the file,
    so verdicts never add up on disk. Records are ordered by
    {!Mutate.compare_id} and reaching tests are sorted, so equal collections
    give equal files. [identity] is recorded after the magic line when it is
    given. A merged collection has no single writer and is written without one.

    [load path] is then [Ok (t, identity)] for every [t] whose identifiers
    {!load} accepts, and [Error (Corrupt _)] for any other (see {!add}). A
    collection built from the mutants of {!Mutate.val-catalogue} round-trips,
    unless a source file was registered under the name [""].

    Raises [Invalid_argument] before anything is written if [identity.exe] is
    empty or if [identity.digest] is not 32 lowercase hexadecimal digits, and
    [Sys_error] if the file cannot be written. *)
