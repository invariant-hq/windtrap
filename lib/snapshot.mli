(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Name-keyed snapshot baselines: the per-run registry, read-only checking,
    atomic acceptance, orphan reporting and pruning.

    A snapshot check compares a produced string against a committed baseline
    file. A baseline's identity is its {e name} — matching [[A-Za-z0-9._-]+],
    unique case-insensitively among the snapshots of one scoping source file
    (baselines are files; macOS and Windows filesystems are case-insensitive) —
    never a source position (RFC Law 2). Baselines are stored at
    [<src_dir>/__snapshots__/<src_basename>/<name>.snap], where [src_basename]
    is the scoping file's basename without its extension. The layout is frozen:
    renaming a source file re-keys its baselines, and the recipe is [git mv] of
    the [__snapshots__/<basename>] directory before the next run.

    {!type:t} is the registry: one value per run, created by the runner and
    carried in the run record — never a global (RFC Law 9). {!check} is the
    engine behind the public [snapshot] and [snapshot_pp] wrappers; both feed
    one registry, so they share one namespace per file by construction. Checking
    is read-only (RFC Law 1): a missing baseline or a mismatch raises
    {!Failure.Check_failure} with a typed {!Failure.snapshot_state} payload,
    from which renderers derive diffs and acceptance commands (RFC Laws 3 and
    4). Only an update-mode run writes — atomically, through {!Atomic_file} —
    and {!resolve_mode} guards update mode against CI.

    Snapshots are line-oriented {e text}: both sides of every comparison, and
    every accepted baseline, are canonicalized by replacing CR and CRLF line
    endings with LF and forcing a trailing newline. Content in which CR bytes or
    a missing final newline are significant must be encoded before the call
    (e.g. [String.escaped]). Redaction is likewise ordinary code applied before
    the call ([snapshot "log" (mask_timestamps out)]).

    {b Note.} Baselines are runtime data, invisible to dune's dependency
    tracking: a change under [__snapshots__/] alone does not re-trigger
    [dune runtest]. Test stanzas close the hole with
    [(deps (glob_files_rec __snapshots__/** ))]. *)

(** {1:modes Modes and the CI guard} *)

(** The type for run-level snapshot modes. *)
type mode =
  | Check  (** Compare against baselines; never write (the default). *)
  | Update  (** Accept missing and mismatched baselines by writing them. *)

(** The type for CI-guarded mode decisions. *)
type mode_resolution =
  | Mode of mode  (** Run with this mode. *)
  | Refused_in_ci
      (** An update was requested, [CI] is set, and the request was not forced:
          the run must refuse to start rather than rewrite baselines on a CI
          machine. *)

val resolve_mode : ci:bool -> Env.update -> mode_resolution
(** [resolve_mode ~ci request] applies the CI guard to an update [request],
    already merged from programmatic, CLI and environment sources by the caller:
    {!Env.No_update} is [Mode Check]; {!Env.Update} is [Mode Update], except
    {!Refused_in_ci} when [ci] is [true]; {!Env.Force_update} is [Mode Update]
    even under CI. Pure. *)

(** {1:registry Registries} *)

type t
(** The type for per-run snapshot registries: the names checked so far, keyed
    case-insensitively per scoping file, each with its first check's site and
    checking test, and the baseline content this run compares against; the
    baseline directories the run consulted; and the paths written by acceptance.
    Mutable; created per run and never shared across runs. *)

val create : ?root:string -> mode:mode -> unit -> t
(** [create ~mode ()] is a fresh, empty registry for a run in [mode].

    [root] is the project root that scoping files are resolved under; a relative
    [root] is made absolute against the current directory. Defaults to
    {!Path_ops.project_root}[ ()]. *)

val mode : t -> mode
(** [mode t] is the mode [t] was created with. *)

(** {1:checking Checking} *)

val check :
  t ->
  ?update_eligible:bool ->
  ?loc:Loc.t ->
  test:string ->
  scope:string option ->
  name:string ->
  string ->
  unit
(** [check t ~scope ~name actual] compares [actual], canonicalized, against the
    baseline for [name] scoped by [scope], and returns [()] iff they are equal.
    Every failure raises {!Failure.Check_failure} carrying {!Failure.Snapshot}
    with [name], the resolved baseline path, [loc], and the
    {!Failure.snapshot_state} named below.

    [scope] is the compile-time path of the scoping source file: the file of the
    caller's [~pos], or the enclosing test's declaration file — never a
    backtrace frame at snapshot call time, so a snapshot reached through a
    helper in another file does not relocate its baseline (callers uphold this;
    [loc] is display and site-identity data only, it never chooses the path).
    [Some file] resolves under [t]'s root via {!Path_ops.reconstruct}. [None] —
    neither [~pos] nor a declaration file was available — or a reconstruction
    that cannot be proven to lie under the root fails with
    {!Failure.Unresolvable}; the failure's [path] is the unproven candidate
    path, or [""] when there is no scope at all. Neither mode creates any
    directory or file for an unresolved scope: update mode never writes through
    an unverified reconstruction.

    [test] is the run-unique rendered path of the checking test (the runner
    rejects duplicate paths at startup), stable across retry attempts.

    The first check of a name registers it: [test], the check's site [loc], its
    baseline path, and — once known — the baseline content that every later
    check of the name compares against. A later check of the same name is an
    ordinary recheck (loops, retries, [cases] families) when both sites are
    known and equal, or when a site is unknown and [test] is the registered
    checker; it fails with {!Failure.Duplicate} — carrying the first check's
    site and test, while the raised failure's own [loc] is the second check's —
    when both sites are known and unequal, or when a site is unknown and [test]
    differs. Detection therefore never requires a call-stack frame to have
    survived: [test] alone catches cross-test duplicates, and callers pass their
    best site as [loc] (the facade falls back to the checking test's declaration
    site). Identity is case-insensitive: ["Help"] collides with ["help"].

    In {!Check} mode:
    - No baseline file existed at the name's first check this run: fails with
      {!Failure.Missing} carrying the canonicalized proposed content; nothing is
      written, and a baseline appearing on disk mid-run is not re-read.
    - The baseline — read from disk and canonicalized at the name's first check
      this run — differs from [actual]: fails with {!Failure.Mismatch} carrying
      both canonical texts; the file is not touched.

    In {!Update} mode, when [update_eligible] is [true]: a missing or differing
    baseline is accepted — parent directories are created and the canonical
    content written atomically via {!Atomic_file.write} — and recorded in
    {!writes} as {!Created} or {!Updated}; an equal baseline is left untouched
    and unrecorded. A name is accepted with at most one content per run: after
    acceptance, a check of the same name with different content fails with
    {!Failure.Mismatch} against the accepted content — never last-write-wins.
    Acceptance happens only while the run's baseline content for the name is
    unknown: when an earlier check already read the baseline from disk (for
    instance under [update_eligible = false]), a later eligible check with
    different content fails with {!Failure.Mismatch} rather than re-accepting —
    the baseline a run compares against never changes once established. With
    [update_eligible = false] (the runner passes it for tests outside the update
    scope, keeping updates filter-scoped) the call checks exactly as in {!Check}
    mode. [update_eligible] defaults to [true] and is irrelevant in {!Check}
    mode.

    Raises [Invalid_argument] if [name] is empty, contains a character outside
    [[A-Za-z0-9._-]], or starts with {!Atomic_file.temp_prefix} (reserved for
    temporary files). File-system errors while reading or writing a baseline
    raise [Sys_error]. *)

(** {1:acceptance Acceptance report} *)

(** The type for acceptance outcomes of one written baseline path. *)
type write_status =
  | Created  (** No baseline existed; one was written. *)
  | Updated  (** A differing baseline existed; it was replaced. *)

val writes : t -> (string * write_status) list
(** [writes t] is the baseline paths written by update-mode acceptance so far,
    with their statuses, in the order they were written. Always [[]] in {!Check}
    mode. Renderers print one line per entry (RFC: every written path is
    reported with created/updated status). *)

(** {1:orphans Orphans and pruning} *)

val orphans : t -> string list
(** [orphans t] is the sorted paths of stale baselines: directory entries of the
    baseline directories [t] consulted that end in [".snap"], are not
    {!Atomic_file.is_temp_name} temporaries, and whose name — the basename
    without [".snap"], compared case-insensitively — was never checked this run.
    Reads the directories at call time; a consulted directory that does not
    exist contributes nothing.

    Reporting never deletes; deletion is {!prune}. A run that executed only part
    of the suite — a filter, focus, skips — under-registers names, so callers
    report orphans only for full runs (the same conditions {!prune} refuses). *)

type prune_refusal = {
  not_update_run : bool;  (** The run was not an update run. *)
  filtered : bool;  (** A filter narrowed the run. *)
  skipped : int;  (** Number of selected tests that skipped. *)
  failed : int;  (** Number of selected tests that failed. *)
  focused : int;  (** Number of focused tests (focus narrows the run). *)
}
(** The type for pruning refusals: every blocking condition of the run, as
    reported by the runner. A refusal has at least one [true] or positive field.
    [not_update_run] and [filtered] block because only a full unfiltered update
    run registers every name; [skipped], [failed] and [focused] block because
    such a test's snapshot calls never registered their names. Renderers explain
    each blocker. *)

val prune :
  t ->
  filtered:bool ->
  skipped:int ->
  failed:int ->
  focused:int ->
  (string list, prune_refusal) result
(** [prune t ~filtered ~skipped ~failed ~focused] deletes stale baselines after
    a full, clean update run: when [mode t] is {!Update}, [filtered] is [false]
    and every count is [0], it removes {!orphans}[ t] — only [.snap] files in
    directories this run consulted — and is [Ok deleted] (sorted; paths whose
    removal failed are omitted). Otherwise it deletes nothing and is
    [Error refusal] with every blocker recorded.

    Deletion is never implicit: callers invoke [prune] only under an explicit
    [--prune]/[WINDTRAP_PRUNE=1] request. Baseline directories shared by several
    test executables must be pruned from the executable that references the full
    set, or by hand — the orphan report makes the stale set visible either way.

    Raises [Invalid_argument] if any count is negative. *)
