(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Shared driver wiring: a run's whole reporting, and the producers it is
    composed from.

    Two thin drivers exist — the facade's [run] and the inline (ppx) runner
    ([Ppx_runtime]) — and one behavior serves both (the "one behavior, both
    runners" doctrine): every transcript line class has exactly one producer,
    and they are composed in exactly one order, so the two runners cannot drift
    apart byte-wise. The six producers are renderer construction
    ({!val:renderer}), the event observer ({!observe}), the GitHub envelope
    ({!github_start}, {!github_end}, {!github_annotations}), the snapshot
    report ({!Render.report_snapshots} — every transcript byte leaves through a
    renderer), the coverage seam ({!snapshot_coverage}, {!coverage_data}), and
    the JUnit sink ({!write_junit}, the one producer that writes a file rather
    than a transcript line); {!execute_and_report} is the order they run in,
    around
    {!Runner.execute}. A runner that composed them itself would be free to get
    that order wrong, which is the same drift by another route.

    What the runners legitimately do {e not} share stays visible at their call
    sites, as a field of the spine record ({!type:t}) or a line in the thin
    drivers: the invocation context ([`Exe] vs [`Mirrors]), the two header
    policies ([seed] and [selection] — the inline runner passes [None] for
    both), the output-level and coverage-mode resolution sources (parsed CLI vs
    the [WINDTRAP_*] mirrors of [Cli.empty]), the GitHub gating decision, the
    [--list] listing ({!Runner.list_selection}, which never reaches this
    module), the correction protocol, and the exit discipline. This module never
    decides them.

    This module sits below both drivers: it depends only on the runner, the
    renderers, and the environment — never on [Cli] resolution or either driver.
*)

(** {1:record The spine record} *)

type t = {
  invocation : Render.invocation;
      (** The hint context every command hint derives from, computed once at
          startup ({!Render.type-invocation}). *)
  seed : Seed.seed option;
      (** The header's seed: the facade passes the root seed iff the suite
          declares property tests; the inline runner passes [None] ({!observe}).
      *)
  selection : string option;
      (** What an empty run explains itself with: the facade passes
          {!selection_description}; the inline runner passes [None]
          ({!observe}). *)
  github : bool;
      (** The GitHub gating decision ({!Env.in_github_actions}). *)
  output : [ `Compact | `Verbose ];  (** The resolved output level. *)
  coverage : bool;
      (** Whether the inline coverage line prints ({!Cli.settings}). *)
  junit : string option;
      (** [--junit]/[WINDTRAP_JUNIT]: where to also write a JUnit report,
          [None] for no report ({!write_junit}). *)
  render : Render.settings;
      (** The presentation knobs the run's renderer is built from
          ({!val:renderer}). *)
  config : Run.config;  (** What the runner reads. *)
  suite : string;  (** The suite name. *)
}
(** The type for run spines: everything {!execute_and_report} consumes beyond
    the tree, as one value — so every place that runs a suite passes the same
    record, and a knob cannot silently drop out of one call site. The thin
    drivers build it once at run entry; the mutation loop threads it whole,
    replacing [config] per child. *)

(** {1:renderer Renderer construction} *)

val renderer :
  render:Render.settings ->
  mode:[ `Compact | `Verbose ] ->
  invocation:Render.invocation ->
  unit ->
  Render.t
(** [renderer ~render ~mode ~invocation ()] is the run's terminal renderer on
    [Format.std_formatter], wired from the environment and the resolved
    {!Render.settings} exactly as both runners require: color from
    {!Env.resolve_color} over [render.color], the terminal status, and
    [INSIDE_DUNE]/[TERM]; width and tail bounds from
    [render.columns]/[render.tail_errors]; the slow threshold from
    [render.slow_threshold]. The live tail is on only for a TTY outside GitHub
    Actions — under the GitHub sink the transcript sits inside the [::group::]
    envelope and cursor controls must never land in the CI log.

    Effects: reads the environment (terminal status, [INSIDE_DUNE], [TERM],
    [GITHUB_ACTIONS], and the color mirrors via {!Env.resolve_color}). *)

(** {1:observer The event observer} *)

val selection_description : Run.config -> string option
(** [selection_description config] describes what narrows the run — the filter,
    exclusion, tags, [--failed], the shard — in the spelling the
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
      ones it empties are not typos — [Ppx_runtime.inline_exit_code] exits [0]
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

(** {1:coverage The coverage seam} *)

val snapshot_coverage : unit -> Render.coverage_summary option
(** [snapshot_coverage ()] is what instrumented code registered in this process,
    scoped by [WINDTRAP_COVERAGE_ONLY], or [None] when nothing registered. Core
    windtrap's entire coverage coupling lives here and in the renderers: three
    runtime calls, and no rendering decision. Whether the resulting line prints
    is {!t.coverage}'s. *)

val coverage_data :
  ?source_roots:string list -> Windtrap_coverage.t -> Render.coverage
(** [coverage_data collection] is [collection] as the renderer's section data
    ({!Render.coverage}): the aggregate counts and one {!Render.coverage_file}
    per file, sources resolved under [source_roots]
    ({!Windtrap_coverage.file_reports}, whose current-directory default it
    keeps). The one builder of that data — this seam links the runtime, so
    Render does not have to — used by the [windtrap coverage] command over
    merged files, which is the only place a per-file coverage table is drawn.
*)

(** {1:silent Execution without reporting} *)

val execute :
  ?on_event:(Runner.event -> unit) ->
  ?allowlist:string list ->
  t ->
  Test_tree.t list ->
  (Runner.outcome, Runner.startup_error) result
(** [execute t tests] is {!Runner.execute} over [t]'s [config] and [suite],
    reporting {e nothing} — no renderer, no envelope, no snapshot report. For
    the one caller population that needs that: mutation children, which run a
    session with the standard descriptors on [/dev/null] and the verdict on a
    pipe. Drivers use {!execute_and_report}, always. [on_event] and [allowlist]
    are {!Runner.execute}'s; only [t.config] and [t.suite] are consulted, so a
    child runs with the spine it was handed, [config] swapped for its own. *)

(** {1:spine The execute-and-report spine} *)

val execute_and_report :
  ?on_event:(Runner.event -> unit) ->
  t ->
  Test_tree.t list ->
  (Runner.outcome, Runner.startup_error) result
(** [execute_and_report t tests] runs [tests] as suite [t.suite] — [t.config] is
    what the runner reads, [t.render] the presentation knobs the run's renderer
    is built from ({!val:renderer}) — and writes the run's whole report on
    standard output, composing the producers above in the one order both runners
    use: {!val:renderer} and {!observe}, {!github_start}, {!Runner.execute},
    then — for a run that happened — {!snapshot_coverage}, {!Render.finish} (its
    [?coverage] from {!coverage_summary}) over {!Run.results},
    {!coverage_report}, {!Render.report_snapshots}, {!github_end},
    {!github_annotations}, {!write_junit} over [t.junit], and a flush of both
    standard formatters.

    [Ok outcome] is {!Runner.execute}'s outcome, reported. {!Run.results} is the
    list every sink projected — the runner's verdict rows included
    ({!Run.type-subject}) — so a caller's own transport (JUnit) reads the same
    rows the terminal showed.

    [t.seed] and [t.selection] are {!observe}'s two header policies, passed
    through rather than derived: the runners genuinely disagree about both, and
    the reasons are documented there. In particular this function does {e not}
    call {!selection_description} itself — an inline partition emptied by a
    mirror is not a mistyped filter.

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

    [Error error] is a refused startup: {!github_end} has closed the envelope
    and {!Runner.startup_message} is already on [stderr], so all the caller
    decides is what to do with {!Runner.startup_exit_code} — the library runner
    exits on it, the inline runner folds it into dune's promotion protocol.

    Reporting state must be flushed or fork-inert at fork points: everything
    here reports through [Format.std_formatter] and the standard descriptors,
    and a caller that forks mid-run (the mutation loop) must flush both
    formatters and both descriptors before every fork, or buffered transcript
    bytes duplicate into the child. Nothing here holds hidden buffers beyond the
    formatters.

    Effects: the union of the producers' — reads the environment, writes the
    transcript on [Format.std_formatter] and the GitHub envelope on standard
    output, and everything {!Runner.execute} itself does (capture logs, the
    last-failed store, accepted baselines, the exit guard). *)
