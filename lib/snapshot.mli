(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Snapshot testing.

    Compare textual output against reference files stored on disk. In {!Check}
    mode, mismatches raise {!Failure.Check_failure} with a unified diff. In
    {!Update} mode, snapshot files are written or overwritten silently.

    {2 Snapshot Location}

    By default, snapshots are colocated with source files:
    {[
      source_dir / __snapshots__ / source_basename / key.snap
    ]}

    For example, a snapshot in [example/demo.ml] at line 42, column 10:
    {[
      example / __snapshots__ / demo / L42_C10.snap
    ]}

    Override with the [WINDTRAP_SNAPSHOT_DIR] environment variable or the
    [~root_dir] config option for centralized storage. *)

type pos = Failure.pos
(** Source position from [__POS__]. *)

type here = Failure.here
(** Source position from [ppx_here]. *)

(** {1 Mode} *)

type mode =
  | Check  (** Compare against existing snapshots (default). *)
  | Update  (** Write or overwrite snapshots instead of failing. *)

(** {1 Configuration} *)

module Config : sig
  type t
  (** Immutable snapshot configuration. *)

  val default : unit -> t
  (** [default ()] creates a config from environment variables.

      - [WINDTRAP_UPDATE=1]: enables {!Update} mode (default: {!Check})
      - [WINDTRAP_SNAPSHOT_DIR]: centralized snapshot directory (default:
        colocated)
      - [WINDTRAP_SNAPSHOT_DIFF_CONTEXT]: unified diff context lines (default:
        3)
      - [WINDTRAP_SNAPSHOT_MAX_BYTES]: diff truncation limit in bytes (default:
        8192)
      - [WINDTRAP_SNAPSHOT_REPORT=1]: report snapshot creates/updates to stderr
        (default: off) *)

  val create :
    ?root_dir:string ->
    ?mode:mode ->
    ?diff_context:int ->
    ?max_bytes:int ->
    ?report_updates:bool ->
    ?filters:(string -> string) list ->
    unit ->
    t
  (** [create ?root_dir ?mode ?diff_context ?max_bytes ?report_updates ?filters
       ()] creates a config with custom settings. Unspecified values fall back
      to environment variable defaults from {!default}. *)

  val with_filter : (string -> string) -> t -> t
  (** [with_filter f t] returns a new config with [f] appended to the filter
      chain. Filters are applied in order: the first filter added runs first. *)

  val with_mode : mode -> t -> t
  (** [with_mode m t] returns a new config with mode set to [m]. *)

  val mode : t -> mode
  (** Return the snapshot mode. *)

  val diff_context : t -> int
  (** Return the number of unified diff context lines. *)

  val max_bytes : t -> int
  (** Return the diff truncation limit in bytes. *)

  val report_updates : t -> bool
  (** Return whether snapshot creates/updates are reported to stderr. *)

  val filters : t -> (string -> string) list
  (** Return the normalization filters in application order. *)
end

val set_config : Config.t -> unit
(** [set_config c] sets the global snapshot configuration. Called by the runner
    before tests execute. *)

(** {1 Assertions} *)

val snapshot : ?here:here -> ?pos:pos -> ?name:string -> string -> unit
(** [snapshot ~pos:__POS__ actual] compares [actual] against the stored snapshot
    file.

    Location is derived from [~pos] or [~here]; at least one or [~name] must be
    provided. The snapshot content is canonicalized through configured filters
    and newline normalization before comparison.

    Behavior depends on the current mode:
    - {!Check}: compares against the existing file. If the file does not exist,
      it is auto-created (except in CI, where it raises instead).
    - {!Update}: writes the snapshot file unconditionally.

    @raise Failure.Check_failure
      if the snapshot mismatches in {!Check} mode, or if no snapshot file exists
      in CI. The error includes a unified diff.

    @raise Failure.Check_failure
      if neither [~pos]/[~here] nor [~name] is provided. *)

val snapshot_pp :
  ?here:here -> ?pos:pos -> ?name:string -> 'a Pp.t -> 'a -> unit
(** [snapshot_pp ~pos:__POS__ pp value] pretty-prints [value] using [pp] and
    snapshots the result. See {!snapshot} for details on mode and location. *)

val snapshotf :
  ?here:here ->
  ?pos:pos ->
  ?name:string ->
  ('a, Format.formatter, unit, unit) format4 ->
  'a
(** [snapshotf ~pos:__POS__ fmt ...] formats a string and snapshots the result.
    See {!snapshot} for details on mode and location. *)
