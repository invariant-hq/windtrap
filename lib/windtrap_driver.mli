(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The drive-side client surface: what a thing that {e runs suites} may use.

    One client drives runs from outside the core wiring — the inline (ppx)
    runner ([Ppx_runtime]) — and until the facade existed what it consumed was
    "whatever [Private] exports". This module names that surface: every
    submodule below is a constrained re-export of the core module of the same
    name, cut to what the client demonstrably uses, so the client diet is a
    signature the compiler enforces rather than a grep rule. It re-exports —
    module aliases, re-exported types — and wraps nothing: no behavior, no
    state. (The mutation loop, in-core, is not a client: the drivers call
    [Mutate_loop.execute_and_report] by name, and the loop names core modules
    directly.)

    This interface {e is} the specification of the drive-side client surface
    (Law 12: an instrumentation subsystem's out-of-core code is a client of
    this facade and/or an observer on [Driver.execute_and_report]'s
    [?on_event]). Widening it is a design act: a client need that falls
    outside it changes this file first, with the reason recorded here.

    {b The census} (what the inline runner uses, and nothing more): settings
    resolution from the environment mirrors ([Cli.settings] over [Cli.empty],
    [Cli.error_message]); construction of the {!Driver.type-t} spine and the
    run through [Driver.execute_and_report]; JUnit ([Driver.write_junit]); its
    promotion exit code off [Run.results]' {!Run.type-result} rows and
    [Runner.startup_exit_code]; GitHub gating ([Env.in_github_actions]); and
    the Law 16d hook registration ({!module-Registry}).

    Not here, deliberately: [Cli.parse]/[Cli.help] (the facade's [run] is the
    only argv parser, and it is core), [Driver]'s producer seams ([observe],
    the GitHub envelope, the coverage seam — composed inside
    [Driver.execute_and_report], never by clients), the staged halves and the
    mutation knobs (the loop is core and reads them directly), and everything
    body-side ({!Windtrap_testkit}).

    Private-stable: this surface moves with co-versioned clients only. Whether
    ecosystem drivers someday get a public spelling is deliberately undecided;
    until then it ships under [Windtrap.Private] like the modules it cuts. *)

(** {1:settings Settings resolution}

    One invocation's knobs, resolved once at run entry ({!Cli.settings}). *)

module Cli : sig
  type parsed = Cli.parsed
  (** The type for raw parse results ({!Cli.type-parsed}). Clients never parse;
      they resolve from {!empty} (under [dune runtest] the environment mirrors
      {e are} the CLI). *)

  val empty : parsed
  (** [empty] is the record with every flag absent ({!Cli.empty}). *)

  type error = Cli.error
  (** The type for parse and resolution errors ({!Cli.type-error}). *)

  val error_message : error -> string
  (** [error_message error] is {!Cli.error_message}: one line for users, naming
      the offending flag or variable. *)

  type settings = Cli.settings = {
    config : Run.config;  (** The run configuration. *)
    render : Render.settings;  (** The renderer's presentation knobs. *)
    coverage_mode : [ `Summary | `Report | `Full | `Off ];
        (** The coverage rendering mode. *)
    output_level : [ `Quiet | `Compact | `Verbose ];
        (** The terminal verbosity level. *)
  }
  (** The type for everything one invocation resolves to ({!Cli.type-settings}).
  *)

  val settings : ?overrides:parsed -> parsed -> (settings, error) result
  (** [settings cli] is {!Cli.settings}: the one resolution call a driver makes,
      with one error to render instead of four. *)
end

(** {1:spine The spine} *)

module Driver : sig
  type t = Driver.t = {
    invocation : Render.invocation;
        (** The hint context every command hint derives from. *)
    seed : Windtrap_gen.Seed.seed option;  (** The header's seed policy. *)
    selection : string option;  (** What an empty run explains itself with. *)
    github : bool;  (** The GitHub gating decision. *)
    output : [ `Quiet | `Compact | `Verbose ];
        (** The resolved output level. *)
    coverage_mode : [ `Summary | `Report | `Full | `Off ];
        (** The resolved coverage mode. *)
    render : Render.settings;
        (** The presentation knobs the run's renderer is built from. *)
    config : Run.config;  (** What the runner reads. *)
    suite : string;  (** The suite name. *)
  }
  (** The type for run spines ({!Driver.type-t}): everything a run consumes
      beyond the tree, as one record. The thin drivers build it once at run
      entry; the mutation loop threads it whole, replacing [config] per child.
  *)

  val execute_and_report :
    ?on_event:(Runner.event -> unit) ->
    t ->
    Test_tree.t list ->
    (Runner.outcome, Runner.startup_error) result
  (** [execute_and_report t tests] is {!Driver.execute_and_report}: the run and
      its whole report, producers composed in the one order both runners use.
      [on_event] is a second subscriber, composed after the transcript's. *)

  val write_junit :
    invocation:Render.invocation ->
    suite:string ->
    duration:float ->
    results:Run.result list ->
    string ->
    unit
  (** [write_junit ~invocation ~suite ~duration ~results target] is
      {!Driver.write_junit}: the JUnit report, on the same terms for an inline
      partition as for a standalone suite. *)
end

(** {1:runner Runner readers}

    The completed run and the events that stream while it executes — read-only
    from here: execution is reached through {!module-Driver} only. *)

module Runner : sig
  (** The type for progress events ({!Runner.type-event}) — named here because
      [Driver.execute_and_report]'s [?on_event] subscriber receives them. *)
  type event = Runner.event =
    | Run_started of { suite : string; total : int; selected : int }
    | Test_started of { path : string list }
    | Test_finished of Run.result
    | Fixture_release of { name : string }

  type startup_error = Runner.startup_error
  (** The type for refusals decided before any test executes
      ({!Runner.type-startup_error}). *)

  val startup_exit_code : startup_error -> int
  (** [startup_exit_code error] is {!Runner.startup_exit_code}. *)

  type outcome = Runner.outcome = {
    run : Run.t;  (** The run record every sink projects. *)
    selected : Test_tree.case list;  (** The selection, execution order. *)
    total : int;  (** Leaf tests before selection. *)
    focus_active : bool;  (** A focused node narrowed the selection. *)
    bailed : bool;  (** [--bail] stopped the run early. *)
    failed_paths : string list;  (** What counted as failed. *)
    orphans : string list;  (** Baselines still stale after a full clean run. *)
    pruned : (string list, Snapshot.prune_refusal) result option;
        (** The [--prune] decision, when requested. *)
    duration : float;  (** Wall-clock seconds. *)
    exit_code : int;  (** The [0]/[1]/[2] contract. *)
  }
  (** The type for completed runs ({!Runner.type-outcome}). *)
end

(** {1:rows Run rows and configuration} *)

module Run : sig
  type t = Run.t
  (** The type for per-run records ({!Run.type-t}), read off
      {!Runner.type-outcome}. *)

  type config = Run.config = {
    seed : Windtrap_gen.Seed.seed;
    filter : string option;
    exclude : string option;
    tags : string list;
    exclude_tags : string list;
    shard : (int * int) option;
    quick : bool;
    failed_only : bool;
    list_only : bool;
    bail : int option;
    stream : bool;
    update : Env.update;
    prune : bool;
    strict_snapshots : bool;
    timeout : float option;
    prop_count : int option;
    max_shrink : int option;
    max_discard : int option;
    max_prop_count : int option;
    junit : string option;
    log_dir : string;
    allow_focus : bool;
  }
  (** The type for resolved run configuration — {!Run.type-config}, fields
      documented there. Re-exported whole so the inline runner builds its spine
      off the resolved record, and the compiler walks this site when a field is
      added. *)

  (** The type for what a row reports on ({!Run.type-subject}). Consumers that
      reason about tests dispatch on this, never on the reporting path. *)
  type subject = Run.subject = Test | Fixture_release | Stale_baselines

  type result = Run.result = {
    path : string list;  (** The row's reporting path. *)
    subject : subject;  (** What the row reports on. *)
    outcome : Failure.outcome;  (** The classified outcome. *)
    counted : bool;  (** Whether the row counted as failed. *)
    xfail : Test_tree.xfail option;  (** The expected-failure annotation. *)
    slow_tagged : bool;  (** Exempt from the slow threshold. *)
    duration : float;  (** Seconds, attempts summed. *)
    attempts : int;  (** [1] plus retries used. *)
    prop_stats : Property.stats option;  (** Property bookkeeping. *)
    srandom_root : Windtrap_gen.Seed.seed option;
        (** The replay root, when drawn. *)
  }
  (** The type for result rows ({!Run.type-result}): what the inline runner's
      promotion exit code and the mutation loop's verdicts classify. *)

  val results : t -> result list
  (** [results t] is {!Run.results}: the recorded rows in execution order,
      verdict rows last. *)
end

(** {1:env Environment facts} *)

module Env : sig
  val in_github_actions : unit -> bool
  (** [in_github_actions ()] is {!Env.in_github_actions} — the inline runner's
      gating input for the GitHub envelope. *)

  (** The type for snapshot update requests ({!Env.type-update}) — a
      {!Run.type-config} field. *)
  type update = Env.update = No_update | Update | Force_update
end

(** {1:registry The registry}

    The Law 16d armed hooks, whole: registration is exactly this surface's
    client's act. See {!Registry}. *)

module Registry = Registry
