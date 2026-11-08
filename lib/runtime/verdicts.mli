(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Mutation verdicts: what a run made of each mutant, and the [.mutants] file
    that carries the verdicts of one suite to the merge.

    The mutation loop writes what it made of a suite's mutants to the suite's
    verdict file. [windtrap mutants] loads the files of a project and combines
    them with {!merge}, under which a mutant that is killed anywhere is killed.
    Nothing here runs when the module loads or when the process exits. *)

(** {1:verdicts Verdicts}

    A verdict is one of five cases, and never a boolean or an exit code. What a
    run could not decide is {!Unknown}, which outranks a survivor, so a mutant
    that no run judged never reads as survived.

    A run judges a mutant only where it evaluated it. The tests of an armed
    mutant run in a worker: a worker process of the loop or of an [--arm] run,
    or the run's own process where it runs its tests itself. Until a worker
    evaluates the armed site, it computes the unmutated program, so a test that
    fails there fails for another reason, and a test that passes there tested
    nothing. Shi, Bell and Marinov, "Mitigating the Effects of Flaky Tests on
    Mutation Testing" (ISSTA 2019, §3.2), make the same rule with the test as
    the unit; here the unit is the worker, since a test can fail on what an
    earlier test of its worker built with the mutant, a fixture or a cached
    value. *)

type reaching_test = string list
(** The type for a test that evaluated a mutant, by its path: the names of its
    groups, the outermost first, and then its own, as [["arithmetic"; "adds"]].
*)

(** The type for verdicts. A survivor always names a reaching test, because a
    mutant that no test reached is {!Outside_tests} or {!Unreached}. *)
type verdict =
  | Killed
      (** A reaching test failed, or a fixture release failed, in a worker that
          had evaluated the armed site by then, its release included for a
          release; or the armed site passed its runaway budget
          ({!Mutate.Runaway}), whatever the code under test made of it. *)
  | Unknown of string
      (** No verdict, and the string says why: a worker ended before its tests
          did, by a crash, an exit or its deadline, and no failure killed; its
          only failures came in workers that had not evaluated the armed site;
          its tests passed and never evaluated it, as when the dry run cached
          the result of the site in a table or a forced lazy value; or the suite
          could not be mutation-tested, as when its dry run is red, it is not
          deterministic or its selection narrows it. *)
  | Survived of { first : reaching_test; others : reaching_test list }
      (** Every reaching test passed, and the armed site was evaluated. [first]
          and [others] are the reaching tests, sorted and without duplicates,
          and [first] is the first. *)
  | Outside_tests
      (** No test reached the site, and the dry run evaluated it outside every
          test, at module initialization or in a fixture release. The loop
          starts no worker for the mutant. *)
  | Unreached
      (** No test reached the site, and the dry run did not evaluate it outside
          every test either. The loop starts no worker for the mutant. *)

val survived : reaching_test list -> verdict
(** [survived ts] is the {!Survived} verdict whose reaching tests are [ts],
    sorted and without duplicates. Raises [Invalid_argument] if [ts] is empty.
*)

(** {1:collections Collections} *)

type record = {
  id : Mutate.id;  (** The identifier of the mutant. *)
  end_line : int;  (** The [end_line] of its site ({!Mutate.type-site}). *)
  end_col : int;  (** The [end_col] of its site. *)
  before : string;  (** The source text of the original expression. *)
  after : string;  (** The source text of the armed expression. *)
  verdict : verdict;  (** What the run made of the mutant. *)
}
(** The type for the record of one mutant. A record is self-describing: it
    carries the two renderings and the extent that a report draws the mutant
    with, so a report needs no catalogue and outlives the executable that
    produced it. *)

val record_of_mutant : Mutate.mutant -> verdict -> record
(** [record_of_mutant m v] is the record of [m] with the verdict [v]. *)

type t
(** The type for verdict collections: finite maps from {!Mutate.type-id} to
    {!type-record}, ordered by {!Mutate.compare_id}. They are immutable. *)

val empty : t
(** [empty] is the collection with no record. *)

val add : t -> record -> t
(** [add t r] is [t] with [r] recorded. A {!Survived} verdict is sorted on its
    way in. When [t] already holds a record under [r.id], the smaller
    [(before, after, end_line, end_col)] in lexicographic order is kept, and the
    two verdicts combine into the verdict of a mutant that one suite saw one way
    and another the other way. It is {!Killed} if either is. Otherwise it is
    {!Unknown} if either is, with the smaller reason in [String.compare] order
    when both are, then {!Survived} with the reaching tests of both if either
    is, then {!Outside_tests} if either is, and {!Unreached} if both are.

    [add] checks nothing of [r.id] or of the end. It accepts an empty [file], a
    [line] below [1], a negative [col], an end before the start and a [rewrite]
    outside {!Mutate.rewrites}, and {!load} then refuses the file that {!save}
    writes for the collection. *)

val records : t -> record list
(** [records t] is the records of [t], ordered by {!Mutate.compare_id}. *)

val merge : t -> t -> t
(** [merge a b] is the union of [a] and [b], where two records that share an
    identifier are combined as {!add} does. [merge] is commutative, associative
    and idempotent, {!empty} is its unit, and an {!Unreached} verdict leaves the
    verdict it is combined with unchanged, so merging any number of files in any
    order gives one answer. *)

(** {1:files Verdict files}

    The first line of a file is the magic line [windtrap-mutants-v6], which
    carries the version of the format. A file with another first line is
    refused, never converted, and nothing is promised from one version to the
    next. *)

(** The type for the errors of reading a verdict file, which is {!Instr.error},
    where each case is described. *)
type error = Instr.error =
  | Unknown_format of { path : string; header : string }
  | Unreadable of { path : string; reason : string }
  | Corrupt of { path : string; reason : string }

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] is [Instr.pp_error format ppf e]. *)

type identity = Instr.identity = { exe : string; digest : string }
(** The type for the identity of the writer of a verdict file, which is
    {!Instr.identity}. *)

val format : Instr.format
(** [format] is the constants of the [.mutants] format: the magic line above,
    the data directory [mutants], the extension [mutants] and the kind
    [verdict]. *)

val writer_identity : exe:string -> identity option
(** [writer_identity ~exe] is the identity to record for the executable at
    [exe], whose [exe] is [Instr.exe_identity ~exe], or [None] when [exe] cannot
    be read. *)

val output_file : exe:string -> suite:string -> string
(** [output_file ~exe ~suite] is [<dir>/<hash>.mutants], the verdict file of the
    suite named [suite] of the executable at [exe], so the suites of one
    executable, such as the partitions of an inline-test library, never replace
    what another found. [<dir>] is [Instr.output_dir format ~exe] and [<hash>]
    the hexadecimal MD5 of [suite]. An executable that is renamed or moved, and
    a suite that is renamed, leave their previous file behind. *)

type file = {
  verdicts : t;  (** The records of the file. *)
  identity : identity option;  (** The recorded writer, if any. *)
  suite : string;  (** The name of the suite whose verdicts the file holds. *)
  refused : string option;
      (** [Some reason] when the writer could not mutation-test its suite, and
          [reason] says why. Its records then hold what it could not judge as
          {!Unknown}. *)
}
(** The type for what a verdict file holds. *)

val load : string -> (file, error) result
(** [load path] is [Ok file] when the file at [path] is a verdict file. The file
    is read with {!Instr.read_file}, and every error names [path]. Otherwise it
    is the error of the read, [Error (Unknown_format _)] for another first line,
    or [Error (Corrupt _)] for data that is truncated or invalid:
    - a negative count, or a count larger than the input,
    - an empty file name, a line below [1] or a negative column,
    - a rewrite outside {!Mutate.rewrites}, or an unknown verdict,
    - a survivor with no reaching test, or a second record for one identifier,
    - a malformed identity line, no suite line, a status that is neither
      [tested] nor [refused] with its reason, or bytes after the last record.

    Nothing is repaired, and a file with one malformed record is not read at
    all. *)

val save :
  ?identity:identity -> ?refused:string -> suite:string -> string -> t -> unit
(** [save ?identity ?refused ~suite path t] writes [t] to [path] in the format
    of a verdict file, with {!Instr.write_file}. Records are ordered by
    {!Mutate.compare_id} and reaching tests are sorted, so equal collections
    give equal files. [identity] is recorded after the magic line when it is
    given. A merged collection has no single writer and is written without one.
    The suite's name follows, then the status line: [tested], or [refused] and
    [refused]'s reason.

    [load path] is then [Ok { verdicts = t; identity; suite; refused }] for
    every [t] whose identifiers {!load} accepts, and [Error (Corrupt _)] for any
    other (see {!add}). A collection built from the mutants of
    {!Mutate.val-catalogue} round-trips, unless a source file was registered
    under the name [""].

    Raises [Invalid_argument] before anything is written if [identity.exe] is
    empty or if [identity.digest] is not 32 lowercase hexadecimal digits, and
    [Sys_error] if the file cannot be written. *)
