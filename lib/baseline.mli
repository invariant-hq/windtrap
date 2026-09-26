(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Baselines: the registry of a run, its read-only check and the corrections
    that the run writes.

    A baseline is the text that a produced text is compared with: the string
    literal at a recorded position, or a file under the project root
    ({!type-subject}). {!check} compares the two, and what the run does when
    they differ is its {!type-mode}. Under {!Corrected} and {!Update} the check
    records a correction, {!settle} keeps or drops the corrections of each
    attempt, and {!val-write} writes the kept ones once.

    A file baseline is lines of text. Both sides of its comparison and every
    file written are made canonical: CR and CRLF line endings become LF and the
    text ends with a newline, so an empty file equals an empty produced text. *)

(** {1:modes Modes} *)

(** The type for what a run does with a produced text that differs from its
    baseline. *)
type mode =
  | Check  (** Compare and fail. Nothing is recorded and nothing is written. *)
  | Corrected
      (** Compare and fail, and write every kept correction as a [.corrected]
          file. *)
  | Update
      (** Accept. For a baseline that differs or is missing the check records a
          correction and does not fail, and every kept correction is written in
          place. {!check} gives the two failures that remain. *)

(** {1:subjects Subjects} *)

(** The type for what a check compares against.

    A subject has a key in the registry: the whole [pos] of a literal, or the
    path of a file as the subject spells it. A key is accepted with at most one
    content among the kept attempts of a run (see {!check}). Two spellings of
    one file, as ["a/b"] and ["a/./b"], are two keys. Each reads the file and
    each can record a correction, and {!val-write} then writes the later of the
    two contents without any mismatch between them. *)
type subject =
  | Literal of { pos : Loc.pos; value : string; exact : bool }
      (** The string literal at [pos], compiled to [value]. It is compared byte
          for byte when [exact], and otherwise both sides go through
          {!Source_patch.normalize}. [pos] must be a site that
          {!Source_patch.val-patch} accepts: the position that [__POS_OF__]
          recorded for the literal, or that of an [[%expect]] node, with the
          payload of the node as [value]. *)
  | File of string
      (** The file at this path, which {!check} proves under the project root
          ({!Os.reconstruct}). A file that {!Os.file_exists} does not find is a
          {!Failure.Missing} baseline, whose correction is the file. *)

(** {1:registry Registries} *)

type t
(** The type for registries: the keys checked so far with the content that each
    compares against, the corrections recorded, and what {!val-write} did with
    each file. A registry is mutable and not thread-safe. A client must create
    one per run and must not share it between runs. *)

val create : ?root:string -> ?cwd:string -> mode:mode -> unit -> t
(** [create ~mode ()] is an empty registry for a run in [mode].
    - [root] is the project root under which the paths of the subjects are
      proven. A relative one is made absolute against the current directory.
      Defaults to [Os.project_root ()].
    - [cwd] is the directory in which the run started, made absolute in the same
      way. Defaults to the current directory.

    When [cwd] lies inside a build context under [root] ({!Os.build_root}), the
    run is a build action, and a check reads dune's copy of a file in that
    context. The test compares strings, so [root] must be normalized as
    {!Os.project_root} normalizes it: no [.] or [..] segment, and no repeated or
    trailing separator.

    Raises [Sys_error] as {!Os.project_root} does. *)

val mode : t -> mode
(** [mode t] is the mode that [t] was created with. *)

(** {1:checking Checking} *)

val check : t -> ?loc:Loc.t -> ?correct:bool -> subject -> string -> unit
(** [check t subject actual] compares [actual] with the baseline of [subject]:
    byte for byte for an exact literal, through {!Source_patch.normalize} on
    both sides for a flexible one, and as canonical lines of text for a file. It
    returns [()] when they are equal. When they differ it raises
    {!Failure.Check_failure} with the failure that {!Failure.val-baseline}
    builds at [loc], which bounds its texts, except under {!Update}, where it
    records a correction and returns (see the steps).
    - [loc] is the location that a failure carries. [check] computes none.
    - [correct] is whether this check may record a correction. With [false] the
      check behaves as under {!constructor-Check} whatever the mode of [t].
      Defaults to [true].

    A check takes these steps in order.
    + The path of [subject], which for a literal is the file of its [pos], is
      proven under the root of [t] by {!Os.reconstruct}. When it cannot be, the
      check fails in every mode with {!Failure.Unresolvable}, which names the
      unproven candidate, and nothing is registered.
    + The first check of a key reads its baseline, once for the run: the
      compiled [value] of a literal, for which no file is read, or the file at
      its resolved path.
    + A key that has an accepted content is compared with it alone. Any other
      content is a {!Failure.Mismatch} against it, in every mode, marked
      {!Failure.Conflict}, and records no correction.
    + Otherwise a difference is a {!Failure.Mismatch} of both texts in their
      comparison form, and a missing file a {!Failure.Missing} of the canonical
      content that the check would accept. Under {!Corrected} and {!Update} the
      check then records a correction, which holds [actual] whole where the
      failure bounds it: the literal as {!Source_patch.val-patch} rewrites it to
      [actual], or the file holding the canonical [actual]. The key is accepted
      with that content from then on, unless a {!settle} drops the attempt.
    + Before it records the correction of a literal, the check reads the source
      file that {!val-write} will patch, once per run, and tries the patch on it
      alone. When the file cannot be read or the patch is refused, no correction
      is recorded and the check fails, under {!Update} too, with the mismatch
      marked {!Failure.Refused} with the literal's line and a reason that names
      no path. A file baseline is not tried.

    Raises [Sys_error] if a file exists and cannot be read. *)

val settle : t -> keep:bool -> int
(** [settle t ~keep] closes the attempt that recorded corrections since the
    previous call. With [keep] they are kept for {!val-write}. Otherwise they
    are dropped, and their keys are no longer accepted, so the next attempt is
    compared with the baselines as they were first read.

    The result is the number of corrections kept. With [~keep:false] it is [0]
    whether the attempt had recorded none or several, and nothing else counts
    the corrections dropped. *)

(** {1:writing Writing} *)

val write : t -> unit
(** [write t] writes the kept corrections once, and a second call writes
    nothing. Under {!Corrected} a correction goes to [<path>.corrected] beside
    the file that the run read, which is dune's copy of the file under a build
    action (see {!create}) and the file itself otherwise. Under {!Update} it
    goes into the file itself, under the project root, whichever copy the run
    read. It then replaces a file baseline whatever happened to it since its
    first check. Every write is atomic ({!Os.atomic_write}), and the parent
    directories of a new file are created. The literals of one source file are
    patched together ({!Source_patch.apply}) into the bytes that a file holds
    then: the copy that the run read under {!Corrected}, the file itself under
    {!Update}.

    [write] raises no [Sys_error] and no [Unix.Unix_error]. A file that cannot
    be written is a {!Refused} entry of {!writes}, none of its literals is
    written, and the files after it are still written. It is refused when its
    source cannot be read or changed since the check tried its patches, when a
    parent directory cannot be created, or when its write fails. *)

(** The type for what {!val-write} did with one file. [path] is absolute: the
    [.corrected] file under {!Corrected}, the file itself under {!Update}. *)
type write =
  | Written of { path : string; literals : int }
      (** [path] was written with [literals] patched literals, [0] for a file
          baseline. *)
  | Refused of { path : string; reason : string }
      (** [path] was not written, for [reason], one sentence that does not
          repeat [path]. *)

val writes : t -> write list
(** [writes t] is every file that {!val-write} attempted, in the order of their
    paths. *)
