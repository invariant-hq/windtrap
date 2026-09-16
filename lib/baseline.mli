(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Baselines: the per-run registry of expectations, read-only checking, and the
    corrections a run writes.

    A baseline is where the source says it is: the literal at an [expect] or
    [expect_exact] call's position, or the file an [expect_file] call names,
    relative to the project root. {!check} compares a produced text with its
    baseline; what the run does with the produced text is its {!type:mode}.
    Corrections are recorded while tests run, kept or dropped per test by
    {!settle}, and written once by {!write} after the last test: in {!Corrected}
    mode as [<path>.corrected] beside the file (beside dune's build copy when
    the run is a build action), in {!Update} mode into the file itself. Every
    write is atomic ({!Os.atomic_write}). File baselines are line-oriented text:
    both sides of every comparison and every written file are canonicalized by
    replacing CR and CRLF line endings with LF and forcing a trailing newline.
*)

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
      (** The string literal at [pos], compiled to [value], compared byte for
          byte when [exact] and through {!Source_patch.normalize} otherwise.
          [pos] is what [__POS_OF__] recorded for the literal. *)
  | File of string
      (** The file at this path, relative to the project root. A missing file is
          a {!Failure.Missing} baseline whose correction is the file. *)

(** {1:registry Registries} *)

type t
(** The type for per-run registries: the keys checked so far with the content
    each compares against, the corrections recorded, and what {!write} wrote.
    Mutable; created per run and never shared across runs. *)

val create : ?root:string -> ?cwd:string -> mode:mode -> unit -> t
(** [create ~mode ()] is a fresh, empty registry for a run in [mode]. [root] is
    the project root paths resolve under, made absolute against the current
    directory if relative; defaults to {!Os.project_root}[ ()]. [cwd] is the
    directory the run started in, default the current one: when it lies inside a
    build context under [root] ({!Os.build_root}), the run is a build action and
    baselines are read from, and corrections written beside, dune's copy of each
    file in that context. *)

val mode : t -> mode
(** [mode t] is the mode [t] was created with. *)

(** {1:checking Checking} *)

val check : t -> ?loc:Loc.t -> ?correct:bool -> subject -> string -> unit
(** [check t subject actual] returns [()] iff [actual] equals [subject]'s
    baseline: byte for byte for an exact literal, through
    {!Source_patch.normalize} for a flexible one, canonicalized for a file.
    [correct] (default [true]) is whether this check may record a correction;
    [false] behaves as {!Check} mode whatever [t]'s mode.

    The first check of a key this run reads its baseline once; later checks
    compare against the same content. A key whose path cannot be proven to lie
    under [t]'s root ({!Os.reconstruct}) fails with {!Failure.Unresolvable}
    naming the unproven candidate, in every mode. Every failure raises
    {!Failure.Check_failure} carrying {!Failure.Baseline} with [loc]:
    {!Failure.Mismatch} with both texts in their comparison form,
    {!Failure.Missing} with the canonical content the check would accept, or
    {!Failure.Unresolvable}. In {!Corrected} and {!Update} mode the check also
    records a correction and the key is accepted with [actual]'s content for the
    rest of the run: a later check of the key passes with that content and fails
    with {!Failure.Mismatch} against it with any other. In {!Update} mode a
    check that records a correction does not fail. File-system errors while
    reading a file raise [Sys_error]. *)

val settle : t -> keep:bool -> int
(** [settle t ~keep] closes the test attempt that recorded corrections since the
    previous call: with [keep] they are kept for {!write}, otherwise dropped and
    their keys unaccepted again. It is the number of corrections kept. The
    runner calls it after every attempt ({!Run}, {e Corrections}). *)

(** {1:writing Writing} *)

val write : t -> unit
(** [write t] writes every kept correction, once: in {!Corrected} mode
    [<path>.corrected] beside each corrected file, in {!Update} mode each file
    in place, nothing in {!Check} mode. A source file's literal patches are
    applied together ({!Source_patch.apply}) to the file's current bytes; a file
    the patcher refuses, or that cannot be read or written, is recorded in
    {!refusals} and left alone. Parent directories of a new file are created. *)

type written = {
  path : string;  (** The file written. *)
  literals : int;  (** The literals patched in it; [0] for a file baseline. *)
}
(** The type for what {!write} wrote. *)

val writes : t -> written list
(** [writes t] is the files {!write} wrote, in path order. *)

val refusals : t -> (string * string) list
(** [refusals t] is the files {!write} could not write, each with the reason, in
    path order. A refusal fails the run. *)
