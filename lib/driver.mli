(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Shared driver wiring: a run's whole reporting, and the producers it is
    composed from.

    Two thin drivers exist — the facade's [run] and the inline (ppx) runner
    ([Ppx_runtime]) — and one behavior serves both (the "one behavior, both
    runners" doctrine): every transcript line class has exactly one producer
    here, and they are composed in exactly one order, so the two runners cannot
    drift apart byte-wise. The five producers are renderer construction
    ({!val:renderer}), the event observer ({!observe}), the GitHub envelope
    ({!github_start}, {!github_end}, {!github_annotations}), the snapshot/prune
    report ({!report_snapshots}), and the coverage seam ({!snapshot_coverage},
    {!coverage_summary}, {!coverage_report}); {!execute_and_report} is the order
    they run in, around {!Runner.execute}. A runner that composed them itself
    would be free to get that order wrong, which is the same drift by another
    route.

    What the runners legitimately do {e not} share stays visible at their call
    sites, as an argument to {!execute_and_report} or a line in the thin
    drivers: the invocation context ([`Exe] vs [`Mirrors]), the two header
    policies ([seed] and [selection] — the inline runner passes [None] for
    both), the output-level and coverage-mode resolution sources (parsed CLI vs
    the [WINDTRAP_*] mirrors of [Cli.empty]), the GitHub gating decision, the
    [--list] listing, JUnit, the correction protocol, and the exit discipline.
    This module never decides them.

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

val selection_description : Run.config -> string option
(** [selection_description config] describes what narrows the run — the filter,
    exclusion, tags, [--quick], [--failed], the shard — in the spelling the
    reader typed, or [None] when nothing narrows it. It exists so an empty
    selection can say why it is empty; the phrasing of that sentence is
    {!Render}'s, the configuration behind it is the driver's. *)

val junit_path : suite:string -> string -> string
(** [junit_path ~suite target] is the file [suite]'s JUnit report is written
    to. A [target] naming an [.xml] file is that file; anything else is a
    directory, and the report lands at [<target>/<suite>.xml] with [suite] made
    filename-safe ({!Path_ops.sanitize_component}).

    The two forms exist because [--junit] and [WINDTRAP_JUNIT] are asked in
    different situations. A flag on one executable is one suite and one file.
    The mirror is read under [dune runtest], which starts a process per [(test)]
    stanza and per inline-test library, and a single fixed path would have each
    silently overwrite the last. *)

val write_junit :
  invocation:Render.invocation ->
  suite:string ->
  duration:float ->
  results:Run.result list ->
  string ->
  unit
(** [write_junit ~invocation ~suite ~duration ~results target] writes [suite]'s
    JUnit report to {!junit_path}, creating the directory when [target] is one.
    A report that cannot be written is a warning on standard error, never a
    failed run: the report is a CI convenience, not the run's verdict.

    Shared with the inline runner, so an inline partition writes its report on
    the same terms as a standalone suite. *)

val observe :
  Render.t ->
  seed:Seed.seed option ->
  selection:string option ->
  Runner.event ->
  unit
(** [observe renderer ~seed ~selection event] streams [event] through
    [renderer]: the header on [Run_started], the live tail on [Test_started],
    the per-test progress mark on [Test_finished], and the release notice
    ([releasing <name>]) on [Fixture_release]. [selection] rides along to the
    header ({!selection_description}), unprinted unless the run selects nothing.

    [seed] and [selection] are the two policy differences between the runners'
    observers, and the inline runner passes [None] for both.

    - [seed] is the header's seed: the facade passes the root seed iff the suite
      declares property tests.
    - [selection] is what an empty run explains itself with: the facade passes
      {!selection_description}. The inline runner deliberately does not. Under
      [dune runtest] a [WINDTRAP_FILTER] narrows {e every} partition, and the
      ones it empties are not typos — {!Ppx_runtime.inline_exit_code} exits [0]
      on them for exactly that reason — so the sentence would be a paragraph of
      noise per partition on a working command; and its second line offers [-l],
      which the inline protocol does not have. *)

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

(** {1:spine The execute-and-report spine} *)

val execute_and_report :
  ?on_event:(Runner.event -> unit) ->
  invocation:Render.invocation ->
  seed:Seed.seed option ->
  selection:string option ->
  github:bool ->
  output:[ `Quiet | `Compact | `Verbose ] ->
  coverage_mode:[ `Summary | `Report | `Full | `Off ] ->
  config:Run.config ->
  suite:string ->
  Test_tree.t list ->
  (Runner.outcome * Run.result list, Runner.startup_error) result
(** [execute_and_report ~invocation ~seed ~selection ~github ~output
     ~coverage_mode ~config ~suite tests] runs [tests] as suite [suite] and
    writes the run's whole report on standard output, composing the producers
    above in the one order both runners use: {!val:renderer} and {!observe},
    {!github_start}, {!Runner.execute}, then — for a run that happened —
    {!results_with_releases}, {!snapshot_coverage}, {!Render.finish} (its
    [?coverage] from {!coverage_summary}), {!coverage_report},
    {!report_snapshots}, {!github_end}, {!github_annotations}, and a flush of
    both standard formatters.

    [Ok (outcome, results)] carries the outcome and the results
    {e as the sinks saw them} — {!Run.results} plus the synthetic release rows —
    for the caller's own transports and exit code.

    [seed] and [selection] are {!observe}'s two header policies, passed through
    rather than derived: the runners genuinely disagree about both, and the
    reasons are documented there. In particular this function does {e not} call
    {!selection_description} itself — an inline partition emptied by a mirror is
    not a mistyped filter.

    [on_event] is a {e second} subscriber to {!Runner.execute}'s single
    [?on_event] slot, composed here after {!observe} rather than replacing it —
    replacing it would silently delete the run's whole transcript. The order is
    fixed here and not the caller's: the transcript sees every event first.
    Defaults to ignoring. It is subject to {!Runner.execute}'s observer
    contract: it cannot alter status, counts or scheduling, and if it raises the
    run aborts with that exception, so a subscriber must be total. The mutation
    loop subscribes with it to build its reach map while the dry run prints its
    ordinary output.

    {!github_annotations} runs {e after} {!github_end}, deliberately: an
    [::error::] block written inside the [::group::] envelope folds away with
    the transcript, and annotations are the part a reviewer must see without
    unfolding anything.

    A [config.list_only] run reports nothing and is [Ok (outcome, [])]:
    {!Runner.execute} applied the startup checks and the selection without
    running a test, so there is no run to project — the caller prints the
    listing.

    [Error error] is a refused startup: {!github_end} has closed the envelope
    and {!Runner.startup_message} is already on [stderr], so all the caller
    decides is what to do with {!Runner.startup_exit_code} — the library runner
    exits on it, the inline runner folds it into dune's promotion protocol.

    Effects: the union of the producers' — reads the environment, writes the
    transcript on [Format.std_formatter] and the GitHub envelope on standard
    output, and everything {!Runner.execute} itself does (capture logs, the
    last-failed store, accepted baselines, the exit guard). *)
