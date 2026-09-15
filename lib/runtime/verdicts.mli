(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Mutation verdicts: what a run made of each mutant, and the file that carries
    it between executables.

    {!Mutate} is what generated code is compiled against — site registration at
    module load, the armed-slot read in every guard — and instrumented code
    never holds a verdict: a verdict is earned by a run that asked. The mutation
    loop in the windtrap core writes these collections, one file per test
    executable, and the [windtrap mutants] command loads and merges them. The
    format lives here, beside {!Coverage}'s, so the runtime states both
    data-file formats and one lifecycle rule for each (see
    {{!files}Verdict files}); nothing here runs at module load or at exit.

    The file exists to be {b merged}: a library is normally covered by several
    test executables, and {!merge_verdict} — killed anywhere wins — is the
    reason the format exists at all. *)

(** {1:verdicts Verdicts}

    Three verdicts, never a boolean and never an exit code. There is
    deliberately no fourth {e errored} verdict: under the child's [bail = true]
    a killed child always runs fewer tests than were selected, so any verdict
    keyed on "ran fewer tests than expected" would fire on every kill. A failure
    of the parent's own supervision — a [fork] or [waitpid] that fails — aborts
    the run and names the errno instead, because a score over an unknown number
    of unsupervised children is not a score. *)

type witness = string list
(** The type for test paths: the names from the run root inwards, e.g.
    [["calc"; "arithmetic"; "adds"]]. *)

(** The type for mutant verdicts. *)
type verdict =
  | Killed
      (** A test failed, or the child crashed or hung: divergence is a detected
          behaviour change however it arrived, and the report carries one killed
          count. *)
  | Survived of { witness : witness; others : witness list }
      (** Every test that reached the mutant passed; [witness] and [others] are
          those tests, sorted and without duplicates.
          {e Strengthen one of them.}

          The witnesses are split so that
          {b a survivor always names at least one test}: a mutant no test
          reached is {!Unreached} and is never forked, so a survivor with an
          empty witness list would be a report contradicting itself — "no test
          ran this line and none failed when it changed". Build one with
          {!survived} rather than by hand. *)
  | Unreached
      (** No test evaluated the site. Never scored as survived — the remedy is
          to write a test, not to strengthen one. *)

val survived : witness list -> verdict
(** [survived ws] is the {!Survived} verdict whose witnesses are [ws] sorted and
    without duplicates — the constructor for a caller holding the tests that
    reached the mutant and passed.

    Raises [Invalid_argument] if [ws] is empty. *)

val merge_verdict : verdict -> verdict -> verdict
(** [merge_verdict a b] is the verdict of a mutant observed as [a] by one test
    executable and [b] by another. {b Killed anywhere wins}: the result is
    [Killed] if either is, [Survived] only if every executable that reached it
    survived, and [Unreached] only if neither reached it. A merged [Survived]'s
    witnesses are the union.

    This is the load-bearing rule of the whole file format. A library covered by
    several [(test)] stanzas is the normal case, and a mutant killed by suite A
    while merely reached by suite B is {e killed}; reporting B's view alone
    produces a false survivor, which sends the reader to write a test that
    already exists.

    The operation is commutative, associative and idempotent, with [Unreached]
    as its unit — so merging any number of files in any order gives one answer.
*)

(** {1:files Verdict files}

    Each instrumented test executable's mutation run writes one verdict file
    under the build directory's [_mutants] (or [_windtrap/mutants] outside any);
    [windtrap mutants] loads them all, {!merge}s them, and renders the survivors
    that survive {e everywhere}. The catalogue never touches disk — only
    verdicts do. The lifecycle rule: one file per executable, at {!output_file},
    replaced whole by every run that tests the executable's full suite and left
    untouched by a run that narrows it — unlike a coverage dump, which every run
    adds to.

    The format is versioned by the magic string [windtrap-mutants-v3] on the
    first line; {!of_string} and {!load} reject any other header loudly, and
    cross-version compatibility is not promised. The magic line may be followed
    by the writing executable's {!type:identity}, which the merge uses to
    exclude verdicts whose executable was deleted or rebuilt since the run.

    A file is {b self-describing}: each {!type:record} carries not only the
    mutant's identifier and verdict but the [before]/[after] renderings the
    report draws it with. The catalogue does not travel — it lives inside the
    instrumented binary, which the merging command never links — so a record
    naming only an identifier would produce a project-level report strictly
    worse than the per-executable one it replaces. It is also what lets a report
    outlive the executable that produced it. *)

(** The type for verdict-file errors. All are recoverable: the reporting command
    prints them via {!pp_error} and exits nonzero. There is no mismatch error
    here, unlike coverage's point tables: {!merge_verdict} is total, so two
    files can disagree about a mutant without either being corrupt. *)
type error = Instr.error =
  | Unknown_format of { path : string; header : string }
      (** [path] does not start with this version's magic string; [header] is
          its escaped first line. Files written by other windtrap versions are
          rejected, not converted. *)
  | Unreadable of { path : string; reason : string }
      (** [path] cannot be read; [reason] is the system message. *)
  | Corrupt of { path : string; reason : string }
      (** [path] has the right magic but malformed data. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] formats a human-readable message for [e], including the
    likely fix. *)

type record = {
  id : Mutate.id;  (** The mutant's identifier. *)
  before : string;  (** The original expression's source text. *)
  after : string;  (** The armed expression's source text. *)
  verdict : verdict;  (** What the run made of the mutant. *)
}
(** The type for one verdict-file record: a mutant, as much of it as a report
    needs to draw, and its verdict. A mutant dismissed by [[@mutate off]] has no
    record: it is never forked and never receives a verdict. *)

val record_of_mutant : Mutate.mutant -> verdict -> record
(** [record_of_mutant m v] is [m]'s record with verdict [v] — the identifier and
    renderings of [m], which is what the loop holds when a child reports.
    [m.dismissed] is dropped, having no meaning for a mutant that was tested. *)

type t
(** The type for verdict collections: a finite map from {!Mutate.id} to its
    {!type:record}. Immutable. *)

val empty : t
(** [empty] is the collection with no records. *)

val add : t -> record -> t
(** [add t r] is [t] with [r] recorded, combined with any record already under
    [r.id]: the verdicts through {!merge_verdict}, and the renderings by keeping
    the lexicographically smaller [(before, after)] of the two.

    Two records for one identifier are expected to agree on the rendering, and
    can disagree only if they came from different builds of one source — where
    nothing in the data says which build the reader is looking at. The rule is
    therefore picked for determinism rather than for cleverness: it keeps {!add}
    and {!merge} commutative, associative and idempotent, so a report never
    depends on the order the files happened to be read in. Survivor witnesses
    are sorted and deduplicated for the same reason. *)

val records : t -> record list
(** [records t] is [t]'s records ordered by {!Mutate.compare_id}. *)

val merge : t -> t -> t
(** [merge a b] is the union of [a] and [b], combining shared identifiers as
    {!add} does. Commutative, associative and idempotent, with {!empty} as its
    unit — so merging any number of verdict files in any order gives one answer.
*)

type identity = Instr.identity = { exe : string; digest : string }
(** The type for verdict-file writer identities — [Instr]'s, re-exported, so the
    reporting command handles both instrumentation formats' identities with one
    pass: [exe] is the writing executable's {!exe_identity} and [digest] the
    lowercase hex MD5 of its contents at write time. An executable at [exe]
    whose digest differs is {e not} the one that wrote the file — the content
    comparison survives rebuilds that dune's cache restores with their original
    timestamps, which mtimes do not. *)

val format : Instr.format
(** [format] is the [.mutants] format's constants: its magic string, the name of
    the directory its files live in ([mutants]) and their extension. For the
    reporting command's discovery ({!Instr.data_dir}). *)

val exe_identity : exe:string -> string
(** [exe_identity ~exe] is the [exe] field a verdict file records for the
    executable at path [exe]: its path below its build directory
    ({!Instr.build_dir}, with any [.sandbox/<digest>] prefix removed, so
    sandboxed and direct runs record the same identity), or its absolute path
    when [exe] is under none. The reporting command resolves a relative identity
    against the file's own build directory to detect deleted or rebuilt
    executables. *)

val writer_identity : exe:string -> identity option
(** [writer_identity ~exe] is the {!type:identity} to record when writing a
    verdict file on behalf of the executable at [exe]: its {!exe_identity} and
    the hex MD5 of its bytes, and [None] when [exe] cannot be read. Digesting
    reads the executable once (a few milliseconds for a typical test binary),
    off the test path. *)

val build_root : path:string -> string option
(** [build_root ~path] is {!Instr.build_root}[ ~path]: the parent of the build
    directory [path] (resolved against the current directory when relative) lies
    in, and [None] when no component of [path] starts with [_build]. One rule
    for {!output_file}, {!exe_identity} and the reporting command's file
    discovery, so a file written from inside dune's sandbox and a report run
    from anywhere in the checkout resolve the same root. *)

val output_file : exe:string -> string
(** [output_file ~exe] is the deterministic verdict-file path for the executable
    at path [exe] (resolved against the current directory when relative):
    [<build_dir>/_mutants/windtrap-<hash>.mutants] when [exe] is under a build
    directory, [<hash>] being the hex digest of [exe]'s path below it (with any
    [.sandbox/<digest>] prefix removed, so sandboxed and direct runs write the
    same file), and [<cwd>/_windtrap/mutants/windtrap-<hash>.mutants] otherwise,
    with the full path of [exe] hashed.

    The name depends on the executable's path: renaming or moving a test
    executable orphans its previous verdict file. The reporting command detects
    orphans through the recorded {!exe_identity} and excludes them with a
    warning. *)

val to_string : ?identity:identity -> t -> string
(** [to_string t] is [t] serialized in the verdict-file format. Deterministic:
    records are ordered by {!Mutate.compare_id} and witnesses are sorted, so
    equal collections serialize identically regardless of construction order.
    [identity] is recorded after the magic line when given; a merged collection,
    which has no single writer, serializes without one.

    Raises [Invalid_argument] if [identity.exe] is [""] or [identity.digest] is
    not 32 lowercase hex characters. *)

val of_string : ?path:string -> string -> (t * identity option, error) result
(** [of_string s] is [Ok (t, id)] when [s] parses: [t] the collection and [id]
    the recorded writer identity, [None] when [s] carries none. [path], used in
    errors, defaults to ["<string>"]. Errors: [Unknown_format] for a foreign
    header, [Corrupt] for truncated or invalid data — a negative or oversized
    count, a line that is not 1-based, a rewrite outside {!Mutate.rewrites}, an
    unknown verdict tag, a survivor naming no test, a duplicate identifier, a
    malformed identity line, or trailing garbage. Nothing is repaired and
    nothing is guessed: a file this module cannot read exactly is not read at
    all.

    Round trip: [of_string (to_string ?identity t)] is [Ok (t, identity)]. *)

val load : string -> (t * identity option, error) result
(** [load path] reads and parses the verdict file at [path].
    [Error (Unreadable _)] when the file cannot be read; otherwise as
    {!of_string}. *)

val save : ?identity:identity -> string -> t -> unit
(** [save path t] writes [to_string ?identity t] to [path], creating [path]'s
    directory if needed. The write is atomic: the data goes to a uniquely named
    temporary file next to [path], which is then renamed over it, so a reader
    never observes a partial file and a crashed run never leaves a truncated
    one. Re-running replaces the file; verdicts never accumulate on disk.

    Raises [Sys_error] if the file cannot be written, and [Invalid_argument]
    under {!to_string}'s conditions. *)
