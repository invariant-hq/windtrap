(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Shared driver wiring: the producers both runners compose a run's reporting
    from.

    Two thin drivers exist — the facade's [run] and the inline (ppx) runner
    ([Ppx_runtime]) — and one behavior serves both (the "one behavior, both
    runners" doctrine): every transcript line class has exactly one producer
    here, so the two runners cannot drift apart byte-wise. The five producers
    are renderer construction ({!val:renderer}), the event observer
    ({!observe}), the GitHub envelope ({!github_start}, {!github_end},
    {!github_annotations}), the snapshot/prune report ({!report_snapshots}), and
    the coverage seam ({!snapshot_coverage}, {!coverage_summary},
    {!coverage_report}).

    What the runners legitimately do {e not} share stays visible at their call
    sites, as a parameter here or a line in the thin drivers: the invocation
    context ([`Exe] vs [`Mirrors]), the header seed policy ({!observe}'s [seed]
    — the inline runner always passes [None]), the output-level and
    coverage-mode resolution sources (parsed CLI vs the [WINDTRAP_*] mirrors of
    [Cli.empty]), GitHub gating, list-only handling, JUnit, and the exit
    discipline. This module never decides them.

    This module sits below both drivers: it depends only on the runner, the
    renderers, and the environment — never on [Cli] resolution or either driver.
*)

(** {1:renderer Renderer construction} *)

val renderer :
  config:Run.config ->
  mode:[ `Quiet | `Compact | `Verbose ] ->
  invocation:Render.invocation ->
  unit ->
  Render.t
(** [renderer ~config ~mode ~invocation ()] is the run's terminal renderer on
    [Format.std_formatter], wired from the environment and [config] exactly as
    both runners require: color from {!Env.resolve_color} over [config.color],
    the terminal status, and [INSIDE_DUNE]/[TERM]; width and tail bounds from
    [config.columns]/[config.tail_errors]; the slow threshold from
    [config.slow_threshold]. The live tail is on only for a TTY outside GitHub
    Actions — under the GitHub sink the transcript sits inside the [::group::]
    envelope and cursor controls must never land in the CI log.

    Effects: reads the environment (terminal status, [INSIDE_DUNE], [TERM],
    [GITHUB_ACTIONS], and the color mirrors via {!Env.resolve_color}). *)

(** {1:observer The event observer} *)

val observe : Render.t -> seed:Seed.seed option -> Runner.event -> unit
(** [observe renderer ~seed event] streams [event] through [renderer]: the
    header on [Run_started], the live tail on [Test_started], the per-test
    progress mark on [Test_finished], and the release notice
    ([releasing <name>]) on [Fixture_release].

    [seed] is the header's seed, the one policy difference between the runners'
    observers: the facade passes the root seed iff the suite declares property
    tests; the inline runner always passes [None]. *)

(** {1:github The GitHub envelope}

    The [::group::] fold around the transcript and the [::error::] annotation
    block after it, written to standard output at column zero. Each producer is
    a no-op unless [github] — the caller's gating decision
    ({!Env.in_github_actions}, minus list-only runs in the facade). *)

val github_start : github:bool -> string -> unit
(** [github_start ~github suite] opens the folded log section named [suite]. *)

val github_end : github:bool -> unit
(** [github_end ~github] closes the folded log section. *)

val github_annotations :
  github:bool -> invocation:Render.invocation -> Run.result list -> unit
(** [github_annotations ~github ~invocation results] prints the
    {!Render_github.annotations} block for [results] — after {!github_end}, so
    annotations are never folded away. *)

(** {1:snapshots The snapshot/prune report} *)

val report_snapshots :
  out:Format.formatter ->
  output:[ `Quiet | `Compact | `Verbose ] ->
  invocation:Render.invocation ->
  Runner.outcome ->
  unit
(** [report_snapshots ~out ~output ~invocation outcome] prints the run's
    baseline maintenance lines on [out]: one [wrote <path> (new|updated)] line
    per accepted baseline ({!Snapshot.writes}, paths spelled by
    {!Path_ops.display} — the one producer for both runners), then either the
    [pruned <path>] lines of a granted [--prune], or the
    [stale baseline: <path>] lines with the prune refusal's explanation, or the
    stale-baseline lines with the removal hint spelled from [invocation]
    ([<exe> -u --prune] under [`Exe],
    [WINDTRAP_UPDATE=1 WINDTRAP_PRUNE=1 dune runtest] under [`Mirrors]).

    Prints nothing under [`Quiet] — quiet keeps only the failure blocks and the
    summary. *)

(** {1:coverage The coverage seam} *)

val snapshot_coverage : Run.t -> Windtrap_coverage.t
(** [snapshot_coverage run] snapshots in-process coverage at run end: when
    instrumented code registered any data, the summary is recorded into [run]
    ({!Run.set_coverage}) for renderers to project like any other run data.
    Returns the collection for {!coverage_report}. The core library's entire
    coverage coupling lives here and in the renderers.

    The recorded summary carries the sibling fact ({!Run.summary.siblings}):
    whether other executables' [.coverage] dumps sit beside this process's dump
    destination, read here — at snapshot time, the run's one filesystem look —
    so renderers stay projections. Best-effort by design: on a cold parallel
    first run a sibling's dump may not exist yet (dumps are written atomically
    at exit, after this snapshot), so the fact can be absent once; it is
    deterministic from the second run on, and a spurious sibling (an orphaned
    dump) only makes the resulting hint advisory, never wrong. *)

val coverage_summary :
  coverage_mode:[ `Summary | `Report | `Full | `Off ] ->
  Run.t ->
  Run.summary option
(** [coverage_summary ~coverage_mode run] is the [?coverage] argument for
    {!Render.finish}: the recorded snapshot under [`Summary], [None] otherwise —
    the report modes print their own line ({!coverage_report}), and [`Off]
    prints nothing. *)

(** {1:releases Fixture release failures} *)

val results_with_releases : Runner.outcome -> Run.result list
(** [results_with_releases outcome] is the run's results followed by one
    synthetic result per fixture-release failure.

    Releases run after the last test, so their failures never enter
    {!Run.results} — {!Runner.outcome.release_failures} carries them beside it,
    and every sink (terminal, JUnit, GitHub) projects results. This is the
    projection: a [Fail] result at path ["fixture release"], counted, carrying
    the failure with its [Release] phase and the fixture's declaration site. Law
    8 requires body and release failures both to be reported; without this the
    run exits [1] with a report that says everything passed.

    The synthetic results are not recorded into the run, and must not be:
    {!Runner} decides whether the whole suite executed by comparing the result
    count against the selected count, so an extra row there would disable orphan
    reporting and [--prune]. *)

val coverage_report :
  Render.t ->
  coverage_mode:[ `Summary | `Report | `Full | `Off ] ->
  Run.t ->
  Windtrap_coverage.t ->
  unit
(** [coverage_report renderer ~coverage_mode run collection] prints the per-file
    coverage report ({!Render.coverage_report}) after {!Render.finish} when
    [coverage_mode] is [`Report] or [`Full] and the run recorded coverage; a
    no-op otherwise. Sources are recorded workspace-relative, so they resolve
    against {!Path_ops.project_root} — under [dune runtest] the cwd is inside
    [_build], where the recorded paths never open. *)
