(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Baselines: the per-run registry of expectations, read-only checking, and the
    corrections a run writes.

    A baseline is where the source says it is: the literal at an [expect] or
    [expect_exact] call's position, or the file an [expect_file] call names,
    relative to the project root. {!check} compares a produced text with its
    baseline and raises {!Failure.Check_failure} with a {!Failure.Baseline}
    payload when they differ; what the run does with the produced text is its
    {!type:mode}. A registry is one value per run, created by the runner and
    carried in the run record — never a global.

    Corrections are recorded while tests run, kept or dropped per test by
    {!settle}, and written once by {!write} after the last test: in {!Corrected}
    mode as [<path>.corrected] beside the file — beside dune's build copy of the
    source when the run is a build action, beside the file itself otherwise —
    for a [diff?] action and [dune promote] to take up; in {!Update} mode into
    the file itself, under the project root. Every write is atomic
    ({!Atomic_file}).

    File baselines are line-oriented text: both sides of every comparison, and
    every written file, are canonicalized by replacing CR and CRLF line endings
    with LF and forcing a trailing newline. *)

(** {1:modes Modes} *)

(** The type for what a run writes. *)
type mode =
  | Check  (** Compare and fail; write nothing (the default). *)
  | Corrected
      (** Compare and fail; write every recorded correction as a [.corrected]
          file beside the file it corrects. *)
  | Update
      (** Accept: a differing or missing baseline is not a failure, and every
          recorded correction is written in place. Refused under [CI] by the
          runner. *)

(** {1:subjects Subjects} *)

(** The type for what a check compares against. The registry is keyed by the
    literal's position or the file's path: a key is accepted with at most one
    content per run. *)
type subject =
  | Literal of { pos : Loc.pos; value : string; exact : bool }
      (** The string literal at [pos], compiled to [value]. It is compared byte
          for byte when [exact], and through {!Source_patch.normalize}
          otherwise. [pos] is what [__POS_OF__] recorded for the literal. *)
  | File of string
      (** The file at this path, relative to the project root. A missing file is
          a {!Failure.Missing} baseline whose correction is the file. *)

(** {1:registry Registries} *)

type t
(** The type for per-run registries: the keys checked so far with the content
    each compares against, the corrections recorded, and what {!write} wrote.
    Mutable; created per run and never shared across runs. *)

val create : ?root:string -> ?cwd:string -> mode:mode -> unit -> t
(** [create ~mode ()] is a fresh, empty registry for a run in [mode].

    [root] is the project root paths resolve under; a relative [root] is made
    absolute against the current directory. Defaults to
    {!Path_ops.project_root}[ ()]. [cwd] is the directory the run started in,
    which decides whether the run is a build action: when it lies inside a build
    context under [root] ({!Path_ops.build_root}), baselines are read from and
    corrections written beside dune's copy of each file in that context.
    Defaults to the current directory. *)

val mode : t -> mode
(** [mode t] is the mode [t] was created with. *)

(** {1:checking Checking} *)

val check : t -> ?loc:Loc.t -> ?correct:bool -> subject -> string -> unit
(** [check t subject actual] compares [actual] with [subject]'s baseline and
    returns [()] iff they are equal — byte for byte for an exact literal,
    through {!Source_patch.normalize} for a flexible one, canonicalized as
    line-oriented text for a file. Every failure raises {!Failure.Check_failure}
    carrying {!Failure.Baseline} with [loc] and the state named below. [correct]
    (default [true]) is whether this check may record a correction; with
    [correct] false the check behaves as in {!Check} mode whatever [t]'s mode
    is.

    The first check of a key this run reads its baseline — the compiled literal,
    or the file at its resolved path, read once — and every later check compares
    against the same content. A key whose path cannot be proven to lie under
    [t]'s root ({!Path_ops.reconstruct}) fails with {!Failure.Unresolvable}
    naming the unproven candidate, in every mode.

    A differing baseline fails with {!Failure.Mismatch} carrying both texts in
    their comparison form; a missing file with {!Failure.Missing} carrying the
    canonical content the check would accept. In {!Check} mode that is all. In
    {!Corrected} and {!Update} mode the check also records a correction — the
    literal rewritten to [actual], or the file holding it — and the key is
    thereby accepted with [actual]'s content for the rest of the run: a later
    check of the same key passes with that content and fails with
    {!Failure.Mismatch} against it with any other, never re-accepting. In
    {!Update} mode a check that records a correction does not fail.

    File-system errors while reading a file raise [Sys_error]. *)

val settle : t -> keep:bool -> int
(** [settle t ~keep] closes the test attempt that recorded corrections since the
    previous call: with [keep] they are kept for {!write}, otherwise they are
    dropped and their keys are unaccepted again. It is the number of corrections
    kept. The runner calls it after every attempt; which attempts keep their
    corrections is its rule ({!Runner}, {e Corrections}). *)

(** {1:writing Writing} *)

val write : t -> unit
(** [write t] writes every kept correction, once, after the last test: in
    {!Corrected} mode [<path>.corrected] beside each corrected file, in
    {!Update} mode each file in place; nothing in {!Check} mode. A source file's
    literal patches are applied together ({!Source_patch.apply}) to the file's
    current bytes, and a file refused by the patcher — a literal that no longer
    decodes to the value the binary was compiled with — or one that cannot be
    read or written is recorded in {!refusals} and left alone. Parent
    directories of a new file are created. *)

type written = {
  path : string;  (** The file written. *)
  literals : int;  (** The literals patched in it; [0] for a file baseline. *)
}
(** The type for what {!write} wrote. *)

val writes : t -> written list
(** [writes t] is the files {!write} wrote, in path order. *)

val refusals : t -> (string * string) list
(** [refusals t] is the files {!write} could not write, each with the reason, in
    path order. A refusal fails the run: the correction reached nothing. *)
