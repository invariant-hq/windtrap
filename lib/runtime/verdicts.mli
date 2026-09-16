(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Mutation verdicts: what a run made of each mutant, and the file that carries
    it between executables.

    The mutation loop writes one verdict file per test executable and
    [windtrap mutants] loads and merges them under {!merge_verdict}, killed
    anywhere wins. Instrumented code never holds a verdict, and nothing here
    runs at module load or at exit. *)

(** {1:verdicts Verdicts}

    Three verdicts, never a boolean and never an exit code. A failure of the
    parent's own supervision aborts the run instead of producing one. *)

type witness = string list
(** The type for test paths: the names from the run root inwards, e.g.
    [["calc"; "arithmetic"; "adds"]]. *)

(** The type for mutant verdicts. *)
type verdict =
  | Killed
      (** A test failed, or the child crashed or hung: a detected behaviour
          change however it arrived. *)
  | Survived of { witness : witness; others : witness list }
      (** Every test that reached the mutant passed; [witness] and [others] are
          those tests, sorted and without duplicates. The split is what makes a
          survivor always name at least one test: a mutant no test reached is
          {!Unreached}. Build one with {!survived}. *)
  | Unreached  (** No test evaluated the site. Never scored as survived. *)

val survived : witness list -> verdict
(** [survived ws] is the {!Survived} verdict whose witnesses are [ws] sorted and
    without duplicates.

    Raises [Invalid_argument] if [ws] is empty. *)

val merge_verdict : verdict -> verdict -> verdict
(** [merge_verdict a b] is the verdict of a mutant observed as [a] by one
    executable and [b] by another: [Killed] if either is, [Survived] with the
    union of witnesses if every executable that reached it survived, and
    [Unreached] if neither reached it. Commutative, associative and idempotent,
    with [Unreached] as its unit. *)

(** {1:files Verdict files}

    One file per executable, at {!output_file}, replaced whole by every run that
    tests the executable's full suite and left untouched by a run that narrows
    it. The format is versioned by the magic string [windtrap-mutants-v3] on the
    first line; other headers are rejected and cross-version compatibility is
    not promised. The magic line may be followed by the writer's
    {!type:identity}, which the merge uses to exclude verdicts whose executable
    was deleted or rebuilt since. Each {!type:record} carries the
    [before]/[after] renderings the report draws it with, so a report needs no
    catalogue and outlives the executable that produced it. *)

(** The type for verdict-file errors, all recoverable: the reporting command
    prints them via {!pp_error} and exits nonzero. There is no mismatch error:
    {!merge_verdict} is total. *)
type error = Instr.error =
  | Unknown_format of { path : string; header : string }
      (** [path] does not start with this version's magic string; [header] is
          its escaped first line. Other versions' files are rejected, not
          converted. *)
  | Unreadable of { path : string; reason : string }
      (** [path] cannot be read; [reason] is the system message. *)
  | Corrupt of { path : string; reason : string }
      (** [path] has the right magic but malformed data. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] formats a message for [e], including the likely fix. *)

type record = {
  id : Mutate.id;  (** The mutant's identifier. *)
  before : string;  (** The original expression's source text. *)
  after : string;  (** The armed expression's source text. *)
  verdict : verdict;  (** What the run made of the mutant. *)
}
(** The type for one verdict-file record. A mutant dismissed by [[@mutate off]]
    has no record. *)

val record_of_mutant : Mutate.mutant -> verdict -> record
(** [record_of_mutant m v] is [m]'s record with verdict [v]; [m.dismissed] is
    dropped. *)

type t
(** The type for verdict collections: a finite map from {!Mutate.id} to
    {!type:record}. Immutable. *)

val empty : t
(** [empty] is the collection with no records. *)

val add : t -> record -> t
(** [add t r] is [t] with [r] recorded, combined with any record already under
    [r.id]: verdicts through {!merge_verdict}, renderings by keeping the
    lexicographically smaller [(before, after)], so {!add} and {!merge} are
    commutative, associative and idempotent. *)

val records : t -> record list
(** [records t] is [t]'s records ordered by {!Mutate.compare_id}. *)

val merge : t -> t -> t
(** [merge a b] is the union of [a] and [b], shared identifiers combined as
    {!add} does. Commutative, associative and idempotent, with {!empty} as its
    unit. *)

type identity = Instr.identity = { exe : string; digest : string }
(** The type for verdict-file writer identities, {!Instr}'s re-exported: [exe]
    is the writer's {!exe_identity} and [digest] the lowercase hex MD5 of its
    contents at write time. *)

val format : Instr.format
(** [format] is the [.mutants] format's constants: magic string, data directory
    name ([mutants]) and extension. *)

val exe_identity : exe:string -> string
(** [exe_identity ~exe] is the [exe] field recorded for the executable at [exe]:
    its path below its build directory ({!Instr.build_dir}, any
    [.sandbox/<digest>] prefix removed), or its absolute path when under none.
    The reporting command resolves a relative identity against the file's own
    build directory. *)

val writer_identity : exe:string -> identity option
(** [writer_identity ~exe] is the {!type:identity} to record for the executable
    at [exe]: its {!exe_identity} and the hex MD5 of its bytes, or [None] when
    [exe] cannot be read. Digesting reads the executable once, off the test
    path. *)

val build_root : path:string -> string option
(** [build_root ~path] is {!Instr.build_root}[ ~path]: the parent of the build
    directory [path] (resolved against the current directory when relative) lies
    in, or [None] when no component of [path] starts with [_build]. *)

val output_file : exe:string -> string
(** [output_file ~exe] is the verdict-file path for the executable at [exe]
    (resolved against the current directory when relative):
    [<build_dir>/_mutants/windtrap-<hash>.mutants] when [exe] is under a build
    directory, [<hash>] the hex digest of its {!exe_identity}, and
    [<cwd>/_windtrap/mutants/windtrap-<hash>.mutants] otherwise. A renamed
    executable orphans its previous file, which the reporting command detects
    through the recorded identity. *)

val to_string : ?identity:identity -> t -> string
(** [to_string t] is [t] serialized in the verdict-file format, records ordered
    by {!Mutate.compare_id} and witnesses sorted, so equal collections serialize
    identically. [identity] is recorded after the magic line when given; a
    merged collection, which has no single writer, serializes without one.

    Raises [Invalid_argument] if [identity.exe] is [""] or [identity.digest] is
    not 32 lowercase hex characters. *)

val of_string : ?path:string -> string -> (t * identity option, error) result
(** [of_string s] is [Ok (t, id)] when [s] parses, [id] the recorded writer
    identity if any. [path], used in errors, defaults to ["<string>"]. Errors:
    [Unknown_format] for a foreign header, [Corrupt] for truncated or invalid
    data (a negative or oversized count, a line below 1, a rewrite outside
    {!Mutate.rewrites}, an unknown verdict tag, a survivor naming no test, a
    duplicate identifier, a malformed identity line, trailing garbage). Nothing
    is repaired. [of_string (to_string ?identity t)] is [Ok (t, identity)]. *)

val load : string -> (t * identity option, error) result
(** [load path] reads and parses the verdict file at [path].
    [Error (Unreadable _)] when it cannot be read; otherwise as {!of_string}. *)

val save : ?identity:identity -> string -> t -> unit
(** [save path t] writes [to_string ?identity t] to [path], creating [path]'s
    directory if needed. Atomic: a uniquely named temporary next to [path] is
    renamed over it, so a reader never observes a partial file. Re-running
    replaces the file.

    Raises [Sys_error] if the file cannot be written, and [Invalid_argument]
    under {!to_string}'s conditions. *)
