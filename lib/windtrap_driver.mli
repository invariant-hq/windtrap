(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The drive-side client surface: what a thing that {e runs suites} may use.

    Two clients drive runs from outside the core wiring — the inline (ppx)
    runner ([Ppx_runtime]) and the mutation loop ([Mutate_loop]) — and until now
    what they consumed was "whatever [Private] exports". This module names that
    surface: every submodule below is a constrained re-export of the core module
    of the same name, cut to what those clients demonstrably use, so the client
    diet is a signature the compiler enforces rather than a grep rule. It
    re-exports — module aliases, re-exported types — and wraps nothing: no
    behavior, no state.

    This interface {e is} the specification of the drive-side client surface
    (Law 12: an instrumentation subsystem is a client of this facade and/or an
    observer on [Driver.execute_and_report]'s [?on_event]). Widening it is a
    design act: a client need that falls outside it changes this file first,
    with the reason recorded here.

    {b The census} (what each client uses, and nothing more):

    - {b Both}: [Cli.error_message]; the {!Driver.type-t} spine record;
      [Runner.outcome]'s readers; [Run.results] and the {!Run.type-result} rows.
    - {b The inline runner}: settings resolution from the environment mirrors
      ([Cli.settings] over [Cli.empty]); spine construction; JUnit
      ([Driver.write_junit]); its promotion exit code off [Run]'s rows and
      [Runner.startup_exit_code]; GitHub gating ([Env.in_github_actions]); the
      registry consult and the Law 16d hook registration ({!module-Registry}).
    - {b The mutation loop}: the mutation knobs ([Cli.mutation]);
      [Driver.execute_and_report] for the dry run and renderer construction
      ([Driver.renderer]) for its own report; the staged halves
      ([Driver.plan]/[Driver.execute]) for its forked children; per-child config
      surgery on {!Run.type-config} ([Env]'s update vocabulary included);
      [Runner]'s event stream and startup errors; verdict classification over
      the rows ([Run.fixture_release_path] included); scope and display facts
      ([Env.mutate_only], [Path_ops]); the armed hooks it fires
      ({!module-Registry}).

    Not here, deliberately: [Cli.parse]/[Cli.help] (the facade's [run] is the
    only argv parser, and it is core), [Driver]'s producer seams ([observe], the
    GitHub envelope, the coverage seam — composed inside
    [Driver.execute_and_report], never by clients), and everything body-side
    ({!Windtrap_testkit}).

    Private-stable: this surface moves with co-versioned clients only. Whether
    ecosystem drivers someday get a public spelling is deliberately undecided;
    until then it ships under [Windtrap.Private] like the modules it cuts. *)

(** {1:settings Settings resolution}

    One invocation's knobs, resolved once at run entry ({!Cli.settings}); the
    mutation knobs, resolved by the one module that may read them
    ({!Cli.mutation}). *)

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

  type mutation = Cli.mutation = {
    mode : [ `Off | `Loop | `Report | `Admit ];  (** [WINDTRAP_MUTATE]. *)
    arm : string option;  (** [WINDTRAP_MUTATE_ARM], unparsed. *)
    limit : int;  (** [WINDTRAP_MUTATE_LIMIT]; [0] for all. *)
    tries : int;  (** [WINDTRAP_MUTATE_TRY]; [0] for all reached. *)
  }
  (** The type for the mutation knobs ({!Cli.type-mutation}). *)

  val mutation : unit -> (mutation, error) result
  (** [mutation ()] is {!Cli.mutation}: the four mutation variables, loudly —
      never a silently defaulted mode. *)
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

  val plan : t -> Test_tree.t list -> (Runner.plan, Runner.startup_error) result
  (** [plan t tests] is {!Driver.plan} — the deciding half, for mutation's
      children. *)

  val execute :
    ?on_event:(Runner.event -> unit) -> Runner.plan -> Runner.outcome
  (** [execute plan] is {!Driver.execute} — the running half, reporting nothing.
  *)

  val renderer :
    render:Render.settings ->
    mode:[ `Quiet | `Compact | `Verbose ] ->
    invocation:Render.invocation ->
    unit ->
    Render.t
  (** [renderer ~render ~mode ~invocation ()] is {!Driver.val-renderer}: the
      terminal renderer wired exactly as both runners wire it — for output that
      is legitimately a driver's own (the mutation report). *)

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
  (** The type for progress events ({!Runner.type-event}), for the mutation
      loop's composed observer. *)
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

  val startup_message : startup_error -> string
  (** [startup_message error] is {!Runner.startup_message}. *)

  type plan = Runner.plan
  (** The type for planned runs ({!Runner.type-plan}); made and executed through
      {!module-Driver}'s staged halves. *)

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
      documented there. Re-exported whole because the mutation loop performs
      per-child config surgery (clearing the path selections its pruned tree
      already expresses, forcing [update]/[prune] read-only, its own [log_dir])
      and the compiler must walk those sites when a field is added. *)

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

  val fixture_release_path : string list
  (** [fixture_release_path] is {!Run.fixture_release_path} — the mutation
      loop's verdict vocabulary spells release kills with it. *)

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
      {!Run.type-config} field; the mutation loop forces [No_update] so an armed
      run can never write a baseline. *)
  type update = Env.update = No_update | Update | Force_update

  val mutate_only : unit -> string list
  (** [mutate_only ()] is {!Env.mutate_only} — the catalogue scope, named in the
      mutation report. *)
end

(** {1:paths Path display} *)

module Path_ops : sig
  val project_root : unit -> string
  (** [project_root ()] is {!Path_ops.project_root}. *)

  val reconstruct : root:string -> string -> (string, string) result
  (** [reconstruct ~root path] is {!Path_ops.reconstruct} — the mutation report
      resolves recorded source paths with it. *)
end

(** {1:registry The registry}

    The run-interception slot and the Law 16d hooks, whole: it exists for
    exactly this surface's clients. See {!Registry}. *)

module Registry = Registry
