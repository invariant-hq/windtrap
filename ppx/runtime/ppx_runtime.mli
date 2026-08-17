(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The ordinary-OCaml half of [ppx_windtrap]: registration, expect-test
    semantics, corrections, and the inline-test-runner protocol.

    The PPX is a location recorder and nothing more: it rewrites [let%test] /
    [module%test] / [let%expect_test] into calls to the registration functions
    below, each [[%expect]] / [[%expect_exact]] node into {!expect}, and
    [[%expect.output]] into {!expect_output} — passing source locations, payload
    literals, and the ambient [Expect_test_config]'s [run] and [sanitize]. Every
    semantic decision — matching, normalization, per-node reachability,
    correction formatting, exit codes — lives here, in ordinary OCaml, testable
    without the PPX.

    {b Life cycle.} Module initializers run the registration calls as the test
    library loads, building a per-source-file registry. The generated runner
    main then calls {!init} with the argument vector and {!exit}: under dune's
    [inline_tests] backend the vector carries the
    [inline-test-runner <lib> -partition <file>] protocol, and {!exit} collects
    the selected partition, executes it through [Runner.execute], writes pending
    [.corrected] files into the sandbox, and terminates with the
    promotion-protocol exit code (see {!inline_exit_code}).

    {b Expect tests.} An expect test declares its [[%expect]] nodes up front
    ({!type:node}: per-node ids, exact payload locations, delimiters); the body
    calls {!expect} with the node's id. Each call consumes captured output
    ([Capture.output] through the run record), sanitizes it, and compares —
    normalized for [[%expect]] ({!normalize}), raw for [[%expect_exact]].
    Mismatches do not abort the body: every reached node records its result, so
    one run corrects every stale payload. Under [WINDTRAP_UPDATE] a mismatch
    that records a correction is not reported as a failure at all — the run
    accepts it into the source tree instead, as it accepts a snapshot baseline
    (see {!flush_corrections_report}). At the end of the body the runtime checks
    trailing output and {e per-node} reachability: a node reached twice and a
    node reached never can never cancel out. Corrections re-indent payloads
    relative to the node exactly as ppx_expect does, so adopting a ppx_expect
    suite produces no formatting churn on first promote; a corrected file
    additionally standardizes the shape of every node its resolved tests declare
    — the corrected-file style the conformance corpus goldens pin (see
    {!corrected_source}).

    {b Duplicated tests.} A functor whose body declares tests, instantiated more
    than once, registers the same names and locations several times. ppx_expect
    runs every instance; windtrap does too — later duplicates are renamed with a
    [" (2)"], [" (3)"], … suffix in their registration scope to satisfy the
    runner's path-uniqueness law — and expect nodes accumulate reaches
    {e across} instances, keyed by source span: instances whose outputs format
    identically resolve to one correction, and genuinely different outputs
    resolve to ppx_expect's "test ran multiple times" CR block.

    The cwd is captured at module-load time — tests may [chdir] — and
    [.corrected] files are written there, next to the copied source in dune's
    sandbox, where the backend's [diff?] action and [dune promote] expect them.
    That channel is per-library: dune registers the corrections of a library
    only if every one of its partitions exits cleanly. Under [WINDTRAP_UPDATE]
    corrections are additionally accepted into the source tree, per file and
    without dune — the channel snapshot baselines already use (see
    {!flush_corrections_report}).

    Registration state is module-global by nature (module initializers run
    before any run record exists); everything {e per-run} — capture, results,
    per-test expect state — lives in the run record and the per-test frames. *)

(* The shared core vocabulary, substituted rather than aliased: the
   runtime lives outside the core, and these names must mean the core's
   modules without this signature re-exporting them. *)
module Test_tree := Windtrap.Private.Test_tree
module Runner := Windtrap.Private.Runner

(** {1:locations Locations}

    Locations are byte-offset ranges into the source file, as recorded by the
    PPX from [Lexing.position]: [start_bol] is the offset of the start-of-line,
    so [start_pos - start_bol] is the column the correction formatter indents
    relative to. [line] is 1-based, for failure reports. *)

type loc = { line : int; start_bol : int; start_pos : int; end_pos : int }
(** The type for source ranges. [start_pos] and [end_pos] are byte offsets into
    the file's contents; [start_bol] is the byte offset where [start_pos]'s line
    begins. *)

(** {1:nodes Expect nodes}

    One value per [[%expect]] / [[%expect_exact]] node lexically inside a
    [let%expect_test] body, built by the PPX and passed to {!add_expect_test}.
    Ids are the node's index in the body, in source order, starting at [0]; the
    rewritten node calls {!expect} with the same id. *)

(** The type for payload delimiters, preserved so corrections keep the author's
    spelling. *)
type delimiter =
  | Quote
      (** ["…"] — corrections escape every line and newline onto one source
          line, wrapped with line-continuation escapes past the 90-column margin
          (the corpus's corrected-quote shape). *)
  | Tag of string  (** [{tag|…|tag}] — corrections re-tag on conflict. *)

type payload = { contents : string; delimiter : delimiter; literal_loc : loc }
(** The type for expect payloads: the literal's [contents] as written, its
    [delimiter], and [literal_loc], the range of the whole literal {e including}
    delimiters — the exact range a correction overwrites. The field is
    deliberately not named [loc]: field names are unique across this module's
    record types, so every field of a generated record literal resolves by its
    qualified path alone, never by type-directed disambiguation — which is
    warning 42, fatal in user code compiled with [-w +a -warn-error +a]. *)

(** The type for node kinds: which extension the node was written as. *)
type node_kind =
  | Expect  (** [[%expect]]: matched via {!normalize}. *)
  | Expect_exact  (** [[%expect_exact]]: matched byte-for-byte. *)

type node = {
  id : int;  (** The node's index within its test body, source order. *)
  kind : node_kind;
  loc : loc;  (** The whole extension point [[%expect …]]. *)
  payload : payload option;
      (** [None] for a bare [[%expect]] — it expects empty output, and a
          correction rewrites the whole node. *)
}
(** The type for declared expect nodes. *)

(** {1:registration Registration}

    Called from generated module initializers, before any run starts. [file] is
    the compile-time source path ([loc.loc_start.pos_fname]); its basename is
    the test's partition and its capitalized module name becomes the grouping
    group in {!collect}. [loc] is the extension point's location; [tags] come
    from [[@tags]] attributes.

    A name already registered in the same scope — the enclosing [module%test]
    group, or the file's top level — is renamed by appending [" (2)"], [" (3)"],
    …: functor-instantiated tests register the same name several times, and
    every instance must run (see the module preamble). *)

val add_test :
  file:string -> loc:loc -> tags:string list -> string -> (unit -> unit) -> unit
(** [add_test ~file ~loc ~tags name fn] registers the [let%test] test [name]
    with body [fn], under the group stack opened by {!enter_group} when one is
    open and at the file's top level otherwise. *)

val enter_group : file:string -> tags:string list -> string -> unit
(** [enter_group ~file ~tags name] opens a [module%test] group: subsequent
    registrations nest under [name] until the matching {!leave_group}. Groups
    nest freely. *)

val leave_group : unit -> unit
(** [leave_group ()] closes the innermost open group, registering it with its
    accumulated children.

    Raises [Invalid_argument] if no group is open. *)

val add_expect_test :
  file:string ->
  loc:loc ->
  tags:string list ->
  run:((unit -> unit) -> unit) ->
  sanitize:(string -> string) ->
  nodes:node list ->
  body_loc:loc ->
  body_wrap:int option ->
  trailing_loc:loc ->
  string ->
  (unit -> unit) ->
  unit
(** [body_wrap] is [Some offset] when the body is a bare [match], [try] or
    [function] — the offset of its first character. A trailing correction
    sequences [;] onto the body, and after such a body that [;] binds to the
    last arm, so the inserted node would land inside the arm: the offset lets
    the patch parenthesize the body in the same edit. [None] for every other
    shape.

    [add_expect_test ~file ~loc ~tags ~run ~sanitize ~nodes ~body_loc
     ~trailing_loc name body] registers the [let%expect_test] test [name]. The
    generated call passes [run] and [sanitize] as [Expect_test_config.run] /
    [Expect_test_config.sanitize] — the {e ambient} names, so a user module
    shadowing [Expect_test_config] is honored, and a monadic config fails to
    compile at those references (the ppx_expect-shaped contract: [run] must fit
    [(unit -> unit) -> unit]).

    - [nodes] declares every expect node lexically inside [body].
    - [body_loc] spans from the [let%expect_test] keyword to the end of the
      body: its column indents inserted trailing nodes, and its end is where the
      separating [";"] goes.
    - [trailing_loc] is the zero-width point where a trailing-output correction
      inserts a new node (the end of the extension point).

    The registered test wraps [body] with the expect machinery described in the
    module preamble; [sanitize] is applied to every read of captured output.
    Body outcomes: a body that returns is checked for trailing output and
    per-node reachability; everything a body raises — [Failure.Check_failure],
    [Failure.Skip_test], [Failure.Timeout], fatal exceptions, and any other
    uncaught exception alike — propagates to the runner, and none of it is a
    correction: nodes reached before the exception still resolve, so their
    corrections are recorded, but nothing is spliced at the trailing point — a
    node inserted after a raising statement can never be reached on a future
    run, so such a correction could never converge under [dune promote]. The
    exception, its backtrace, and the test's captured output belong to the
    failure report. To pin an expected exception, catch and print it —
    [(try boom () with e -> print_string (Printexc.to_string e))] followed by an
    ordinary [[%expect]] node.

    A skip raised in the body ([skip ()], [Failure.Skip_test]) makes the test an
    ordinary skip: nothing is checked and nothing is recorded — no correction
    for any node, the ones reached before the skip included, no trailing-output
    insertion, and no unreached-node failure — so no [.corrected] content ever
    exists for the test's nodes, and the test plays no part in the promotion
    exit rule (see {!inline_exit_code}). Any other reading would blank the
    goldens of environment-gated expect tests on promote. *)

(** {1:execution Expect node execution} *)

val expect : id:int -> unit
(** [expect ~id] runs the declared node [id] of the executing expect test:
    consumes captured output, sanitizes it, records the node's result — a
    mismatch is recorded and corrected but does {e not} raise here; failures are
    reported when the test body ends.

    Raises [Invalid_argument] when no expect test is executing or [id] was not
    declared — unreachable through the PPX, which rejects [[%expect]] outside
    [let%expect_test]. *)

val expect_output : unit -> string
(** [expect_output ()] is [[%expect.output]]: consumes and returns the captured
    output since the previous consumption, sanitized.

    Raises [Invalid_argument] when no expect test is executing. *)

val normalize : string -> string
(** [normalize s] is the [[%expect]] matching form of [s]: [s] is split on [\n]
    (a ["\r\n"] pair counts as one newline; a lone [\r] is an ordinary byte),
    every line is stripped of surrounding whitespace with indentation counted in
    leading {e spaces} only, leading and trailing blank lines are dropped, and
    the block is dedented by the minimum indentation of its nonempty lines
    (relative indentation is preserved). Two payloads match iff their
    normalizations are equal — ppx_expect's default formatting flexibility
    exactly: its comparison runs both sides through its payload formatter, which
    is [normalize] plus a uniform node-relative re-indent, so the equalities
    coincide (the whitespace set, line splitting, and legacy
    strip-but-count-spaces rule are the pinned reference's). *)

(** {1:collection Collection} *)

val collect : unit -> Test_tree.t list
(** [collect ()] drains the registry into a test tree: top-level registrations
    grouped per source file under the file's module name ([my_file.ml] →
    [My_file]), files in first-registration order, and — when {!init} parsed a
    [-partition] argument — only the tests of that partition. A second call
    returns [[]] until new registrations arrive. Draining claims the registry
    for the undriven-registration guard (see {!section:undriven}).

    Raises [Invalid_argument] if a group opened by {!enter_group} was never
    closed. *)

val partitions : unit -> string list
(** [partitions ()] is the sorted list of partition names seen by registration —
    one per source file, its basename — the [-list-partitions] answer. *)

(** {1:corrections Corrections}

    A correction rewrites one range of one source file with freshly formatted
    content: a stale payload, a whole bare node, or an inserted trailing node.
    Recording happens while tests run; writing happens once, after the run.

    Writing reproduces ppx_expect's corrected files byte-for-byte (the
    conformance corpus goldens): in a file with at least one correction,
    {e every} node of the file's resolved tests is re-rendered in standard shape
    — a single-line payload collapses onto the node's line
    ([[%expect {| hello |}]]), a multi-line payload puts the extension head on
    its own line with contents at node column + 2, quoted payloads are
    re-escaped (see {!type:delimiter}), string-extension nodes keep their
    [{%expect …|}] spelling, and a reached bare [[%expect]] materializes as
    [[%expect {| |}]]. A file with no corrections is never rewritten: matching
    alone causes no churn, whatever the payload's formatting. Nodes of tests
    that skipped, never ran, or were never reached keep their source bytes. *)

val corrected_source : file:string -> source:string -> string option
(** [corrected_source ~file ~source] is the corrected content of [file] —
    [source] with every recorded correction applied and the file's resolved
    nodes re-rendered in standard shape — or [None] when no corrections were
    recorded for [file]. Pure with respect to the filesystem;
    {!flush_corrections_report} is this plus the read and the write. *)

type flush_report = {
  written : string list;  (** [.corrected] names written beside the source. *)
  accepted : string list;
      (** Source files rewritten in place, project-root relative. *)
  refused : string list;
      (** Sources whose correction did not fully land — nothing written, or
          written but not accepted — each already reported on [stderr]. *)
}
(** The type for what one flush did. *)

val flush_corrections_report : accept:bool -> flush_report
(** [flush_corrections_report ~accept] restores the module-load cwd, then writes
    [<basename>.corrected] there for every file with recorded corrections —
    dune's diff action expects the corrected file next to the copied source in
    the sandbox — and clears the table.

    With [accept], each written correction is {e additionally} accepted into the
    source tree, the channel snapshot baselines already use from inside the same
    sandboxed action: the recorded path is reconstructed against
    [Path_ops.project_root ()], proven to lie under it, and published with
    [Atomic_file.write]. That is what makes one file's correction independent of
    another file's crash — dune registers corrections only when {e every}
    partition of the library exits cleanly (see {!correction_notice}), and this
    channel does not go through dune at all. What is promotable is unchanged: an
    uncaught exception is not a correction under [WINDTRAP_UPDATE] any more than
    without it, so no acceptance can bless one (see {!add_expect_test}).

    [accept] is the caller's whole decision, and {!exit} passes the conjunction
    of two facts: the run's resolved update mode ([Snapshot.Update] —
    [WINDTRAP_UPDATE] after the CI refusal and the [force] override) {e and} the
    process's own clean verdict ({!inline_exit_code} [= 0]). Output produced
    beside a non-expect failure — an assertion failing beside a stale payload, a
    crash later in the same partition — is therefore never accepted, under
    [WINDTRAP_UPDATE] too: since dune gives each file its own partition, the
    gate removes exactly the {e cross-file} veto and keeps the per-file one the
    masked-assertion rule exists for. A declined acceptance cannot restore the
    failure blocks the update mode already suppressed at record time; the
    declined notice and the nonzero exit carry them, and the [.corrected] files
    still await [dune promote] once the failures are fixed.

    Acceptance is guarded by a drift check. The corrected content is a patch by
    byte offsets into the sandbox {e copy} of the source, so it describes the
    source-tree file only while the two are byte-identical. The bytes are
    compared first, and any difference — a source edited while the tests ran, a
    stale sandbox, a [WINDTRAP_PROJECT_ROOT] aimed elsewhere — is refused
    loudly, leaving the file untouched.

    A file whose source cannot be read, whose target cannot be written, or whose
    acceptance is refused is never skipped silently: a line naming the source
    path and the reason is printed on [stderr], the file is returned in
    [refused], and {!exit} then terminates nonzero — a correction that did not
    fully land must not read as passed (see {!inline_exit_code}). *)

(** {1:protocol The runner protocol}

    The generated runner main is [init Sys.argv; exit ()]. The backend invokes
    it as
    [inline-test-runner <lib> -partition <file> -source-tree-root <root>
     -diff-cmd -], and once with [-list-partitions] to enumerate partitions. *)

val init : string array -> unit
(** [init argv] parses the inline-test-runner protocol arguments out of [argv]:
    [inline-test-runner <lib>] (runner mode and the library name),
    [-partition <file>], [-list-partitions], [-source-tree-root <root>], and
    [-diff-cmd <cmd>] (accepted for protocol compatibility). Unrecognized
    arguments are ignored. Only the first call parses; later calls are no-ops.
    Every call claims the registry for the undriven-registration guard (see
    {!section:undriven}). *)

val exit : unit -> 'a
(** [exit ()] runs the inline suite and terminates the process. Not in runner
    mode ({!init} saw no [inline-test-runner]) it exits [0] — the runner
    executable does nothing when invoked by hand. Otherwise it answers
    [-list-partitions] (print and exit [0]); collects the partition's tests
    (none registered: exit [0]); resolves configuration from the environment
    mirrors alone ([WINDTRAP_*] — under [dune runtest] they are the CLI;
    resolution errors print and exit [2]); executes through [Runner.execute]
    with the terminal renderer, wired exactly as the library runner wires it —
    [WINDTRAP_QUIET]/[WINDTRAP_VERBOSE] pick the verbosity level,
    [WINDTRAP_SLOW_THRESHOLD] tunes the slow warnings with ["slow"]-tagged tests
    exempt, and accepted baselines report their written paths project-root
    relative, one behavior across both runners (and GitHub annotations under
    GitHub Actions); writes [.corrected] files — accepting them into the source
    tree as well when the run resolved to [Snapshot.Update], the same
    [WINDTRAP_UPDATE] decision the run's snapshot baselines were made under —
    and names what it wrote on [stderr] (see {!correction_notice}); and exits
    with {!inline_exit_code} — forced to [1] when any correction reached neither
    channel (see {!flush_corrections_report}): dune's promotion diff can only
    surface corrections that exist, so an unwritable one must fail the partition
    instead of exiting [0] with nothing for the diff to catch. *)

val inline_exit_code : Runner.outcome -> int
(** [inline_exit_code outcome] is the inline runner's exit code for [outcome] —
    dune's promotion protocol, not the standalone runner's [0]/[1]/[2] contract:

    - [0] when the run passed, and also when nothing ran (an empty selection is
      an empty partition, not a filter typo);
    - [0] when {e every} failed test's failures are expect mismatches with
      recorded corrections and no fixture release failed: dune then reaches the
      [diff?] step, which shows the diff and registers the promotion;
    - [1] otherwise: any assertion failure, uncaught exception, timeout,
      unreached expect node, or release failure. Corrections already recorded
      are still written by {!flush_corrections_report}; under dune they are
      withheld from promotion until a rerun in which every partition exits
      cleanly (see {!correction_notice}), and under [WINDTRAP_UPDATE] the ones
      recorded before the failure are accepted into the source tree — the exit
      code is untouched by that, so a crashing partition still exits [1] with
      [WINDTRAP_UPDATE] set, and the crash itself is not a correction to accept.

    The [0]-on-corrections case presumes the corrections reach disk: {!exit}
    overrides this code to [1] when {!flush_corrections_report} could not write
    one — including a refused acceptance — because a failed expect test with no
    [.corrected] for dune to diff and no rewritten source would otherwise be
    recorded as passed.

    Skipped tests are invisible to this rule: a skip — in a plain or an expect
    test — neither forces [1] nor helps reach [0]. A run of skips and covered
    corrections exits [0]; a run of skips and one assertion failure exits [1];
    an all-skipped run exits [0]. *)

val correction_notice :
  accepted:string list ->
  refused:string list ->
  declined:bool ->
  string list ->
  string option
(** [correction_notice ~accepted ~refused ~declined written] is the [stderr]
    notice for a runner process that wrote the [.corrected] files [written];
    [accepted] names the source files it also rewrote in place, [refused] the
    ones whose acceptance was attempted and refused, and [declined] says an
    acceptance was requested but withheld because this process's own verdict was
    not clean. [None] when [written] is empty. The first line —
    [windtrap: wrote <files>] — prints whenever anything was written: dune runs
    every partition of a library inside one action, and any partition's nonzero
    exit fails the whole action, skips every diff step, and discards the sandbox
    with all computed [.corrected] files in it, so this line is the only trace
    of a computed correction that survives a sibling partition's failure.

    The explanation under it matches what actually happened, one case only:
    accepted paths are named ([windtrap: accepted into the source tree: …] —
    those went nowhere near dune's channel, so no caveat applies to them);
    refusals point back at the reasons already printed and say to resolve them —
    never advising the acceptance that just failed; a declined acceptance says
    fixing the failures comes first, because acceptance never blesses output
    produced beside a non-expect failure; and only a run that asked for none of
    it gets the caveat with both ways out: dune registers a correction for
    promotion only when every inline-test process of the library exits cleanly —
    fix the failures, rerun, then [dune promote], or rerun with
    [WINDTRAP_UPDATE=1]. The caveat is {e unconditional}, not gated on this
    process's own exit code: the withholding is the whole library's, and no
    partition can see whether a sibling just vetoed its correction (gating it
    left it silent in exactly the cross-file case it exists for). {!exit} prints
    the notice after {!flush_corrections_report}. *)

(** {1:undriven The undriven-registration guard}

    The silent success the guard closes: [let%expect_test] code preprocessed
    with [ppx_windtrap] inside a plain [(executable)] or [(test)] stanza
    registers its tests at module load, and with no [(inline_tests)] stanza
    nothing ever drives the registry — the binary exits [0] having run nothing,
    and its expectations are never checked against anything.

    The first registration installs a [Stdlib.at_exit] handler. A process that
    terminates normally with registrations never claimed by any driving path
    prints a diagnostic on [stderr] — naming the registered files, the missing
    [(inline_tests)] stanza and the runner protocol — and exits [2]: Law 11's
    nothing-ran code, which can be read as neither a pass nor a test failure.
    The handler cannot see the code the process was about to exit with, so it
    fires on every unclaimed normal termination, a crashing one included — the
    diagnostic is true there too, and the exit stays nonzero.

    {b The claim rule.} The registry is claimed — once, for the process's life —
    by any of:

    - {!init}: the runner protocol's entry, in every mode — a partition run,
      [-list-partitions], and the generated runner invoked by hand (which then
      does nothing, by {!exit}'s documented contract: a deliberate invocation is
      not a silent one);
    - {!collect}: whoever drains the registry owns the execution of what they
      took — the rule that covers hand-rolled harnesses driving [Runner]
      directly;
    - {{!section:armed} [enter_armed]}: a process with a mutant armed belongs to
      the mutation loop, whose transcript and exit code are Law 16's — the guard
      must write into neither;
    - {!reset}: a test seam; its caller owns the registry by construction.

    Running a suite claims nothing by itself: a standalone [Windtrap.run]
    executable that also links preprocessed test code it never drains dies with
    the diagnostic, because those registrations can run under no invocation of
    that executable — which is the defect, not a false positive.

    The guard is best-effort, against the silent [0] only: death by signal and
    [Unix._exit] bypass [at_exit] — those endings are already loud or
    deliberate. It also cannot fight the core runner's exit guard: that guard is
    installed mid-run, later in the [at_exit] chain, so an in-run exit it
    cancels ([Failure.Exit_attempt]) never reaches this handler, which fires
    only at the exit that finally proceeds. *)

(** {1:armed Armed processes}

    The mutation subsystem's one reach into this module's behaviour (Law 16d).
    It reaches in to stop a write, never to start one. *)

val enter_armed : unit -> unit
(** [enter_armed ()] puts this module into the state a process with a mutant
    armed requires, and does not come back out — a process that arms stays armed
    for its life. It also claims the registry for the undriven-registration
    guard: an armed process's transcript and exit code are Law 16's, and the
    guard must write into neither (see {!section:undriven}). Beyond that, two
    things, always needed together:

    - {b Checking becomes read-only.} An [[%expect]] or [[%expect_exact]]
      mismatch is a plain failure: no correction is recorded, so
      {!flush_corrections_report} writes no [.corrected] file, accepts nothing
      into the source tree whatever [WINDTRAP_UPDATE] says, and finds nothing to
      name on [stderr], and no failed test is recorded as covered, so
      {!inline_exit_code} never downgrades a failing run to [0] on dune's
      promotion protocol. Matching, normalization, per-node reachability and the
      failures they report are unchanged. An armed mutant changes program output
      on purpose, and a run that rewrote the source tree from mutated output
      would violate Law 1 outright.
    - {b The cross-run tables are cleared}: recorded corrections, the
      styled-writer registry, the merged per-node reach histories and the
      covered paths. A forked mutation child inherits the parent dry run's reach
      histories, and its first mismatch would otherwise resolve against the
      {e parent's} outputs as ppx_expect's "test ran multiple times" CR block
      instead of as the mismatch that killed the mutant. Registration, the
      protocol arguments and the duplicate-name counters are kept: the child
      runs the tests the parent registered.

    Snapshots need no counterpart. [Snapshot.resolve_mode] maps [Env.No_update]
    to [Snapshot.Check] and writing is reachable only under [Snapshot.Update],
    so an armed run's [update = No_update] already makes snapshot checking
    read-only by construction.

    The mutation loop fires it in every process that has a mutant armed — each
    forked child, and an interactive [WINDTRAP_MUTATE_ARM] run — and reaches it
    through [Registry.on_armed], where this module registers it at load time,
    rather than as a dependency: this module sits {e above} the loop, and the
    registry is the seam that keeps the two from naming each other (see
    [Mutate_loop.execute_and_report]). *)

(** {1:seams Test seams} *)

val reset : unit -> unit
(** [reset ()] restores {e every} piece of state this module keeps between calls
    to its module-load value — registrations and open groups, the partitions
    seen, the duplicate-name counters, recorded corrections and the nodes
    {!corrected_source} re-renders with them, the merged per-node reach
    histories, the covered paths {!inline_exit_code} reads, and the protocol
    arguments including {!init}'s once-guard. The clearing is total by
    construction, not by enumeration: the runtime holds that state in a single
    record and [reset] assigns a fresh one. The module-load cwd is not run state
    and survives (see {!flush_corrections_report}). Calling it claims the
    registry for the undriven-registration guard — the seam's caller owns the
    registry by construction (see {!section:undriven}).

    For this module's own test suite, which registers synthetic suites
    repeatedly in one process. Never called by generated code. *)
