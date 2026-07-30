(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The ordinary-OCaml half of [ppx_windtrap]: registration, expect-test
    semantics, corrections, and the inline-test-runner protocol.

    The PPX is a location recorder and nothing more (RFC Law 10): it rewrites
    [let%test] / [module%test] / [let%expect_test] into calls to the
    registration functions below, each [[%expect]] / [[%expect_exact]] node into
    {!expect}, and [[%expect.output]] into {!expect_output} — passing source
    locations, payload literals, and the ambient [Expect_test_config]'s [run]
    and [sanitize]. Every semantic decision — matching, normalization, per-node
    reachability, correction formatting, exit codes — lives here, in ordinary
    OCaml, testable without the PPX.

    {b Life cycle.} Module initializers run the registration calls as the test
    library loads, building a per-source-file registry. The generated runner
    main then calls {!init} with the argument vector and {!exit}: under dune's
    [inline_tests] backend the vector carries the
    [inline-test-runner <lib> -partition <file>] protocol, and {!exit} collects
    the selected partition, executes it through {!Runner.execute}, writes
    pending [.corrected] files into the sandbox, and terminates with the
    promotion-protocol exit code (see {!inline_exit_code}; RFC Law 11's scoped
    deviation).

    {b Expect tests.} An expect test declares its [[%expect]] nodes up front
    ({!type:node}: per-node ids, exact payload locations, delimiters); the body
    calls {!expect} with the node's id. Each call consumes captured output
    ({!Capture.output} through the run record), sanitizes it, and compares —
    normalized for [[%expect]] ({!normalize}), raw for [[%expect_exact]].
    Mismatches do not abort the body: every reached node records its result, so
    one run corrects every stale payload. At the end of the body the runtime
    checks trailing output and {e per-node} reachability (RFC compat mechanism
    (d)): a node reached twice and a node reached never can never cancel out.
    Corrections re-indent payloads relative to the node exactly as ppx_expect
    does (mechanism (c)), so adopting a ppx_expect suite produces no formatting
    churn on first promote; a corrected file additionally standardizes the shape
    of every node its resolved tests declare — the corrected-file style the
    conformance corpus goldens pin (see {!corrected_source}).

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

    Registration state is module-global by nature (module initializers run
    before any run record exists); everything {e per-run} — capture, results,
    per-test expect state — lives in the run record and the per-test frames (RFC
    Law 9). *)

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

type payload = { contents : string; delimiter : delimiter; loc : loc }
(** The type for expect payloads: the literal's [contents] as written, its
    [delimiter], and the [loc] of the whole literal {e including} delimiters —
    the exact range a correction overwrites. *)

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
  trailing_loc:loc ->
  string ->
  (unit -> unit) ->
  unit
(** [add_expect_test ~file ~loc ~tags ~run ~sanitize ~nodes ~body_loc
     ~trailing_loc name body] registers the [let%expect_test] test [name]. The
    generated call passes [run] and [sanitize] as [Expect_test_config.run] /
    [Expect_test_config.sanitize] — the {e ambient} names, so a user module
    shadowing [Expect_test_config] is honored, and a monadic config fails to
    compile at those references (RFC compat mechanism (b): [run] must fit
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
    per-node reachability; everything a body raises — {!Failure.Check_failure},
    {!Failure.Skip_test}, {!Failure.Timeout}, fatal exceptions, and any other
    uncaught exception alike — propagates to the runner, and none of it is a
    correction (RFC Law 11): nodes reached before the exception still resolve,
    so their corrections are recorded, but nothing is spliced at the trailing
    point — a node inserted after a raising statement can never be reached on a
    future run, so such a correction could never converge under [dune promote].
    The exception, its backtrace, and the test's captured output belong to the
    failure report. To pin an expected exception, catch and print it —
    [(try boom () with e -> print_string (Printexc.to_string e))] followed by an
    ordinary [[%expect]] node.

    A skip raised in the body ([skip ()], {!Failure.Skip_test}) makes the test
    an ordinary skip (RFC amendment C2): nothing is checked and nothing is
    recorded — no correction for any node, the ones reached before the skip
    included, no trailing-output insertion, and no unreached-node failure — so
    no [.corrected] content ever exists for the test's nodes, and the test plays
    no part in the promotion exit rule (see {!inline_exit_code}). Any other
    reading would blank the goldens of environment-gated expect tests on
    promote. *)

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
    returns [[]] until new registrations arrive.

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
    {!flush_corrections} is this plus the read and the write. *)

val flush_corrections : unit -> string list
(** [flush_corrections ()] restores the module-load cwd, then writes
    [<basename>.corrected] there for every file with recorded corrections —
    dune's diff action expects the corrected file next to the copied source in
    the sandbox — and clears the table. Returns the written file names. Files
    whose source cannot be read are skipped (their corrections are dropped);
    {!exit}'s exit code does not depend on the write succeeding. *)

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
*)

val exit : unit -> 'a
(** [exit ()] runs the inline suite and terminates the process. Not in runner
    mode ({!init} saw no [inline-test-runner]) it exits [0] — the runner
    executable does nothing when invoked by hand. Otherwise it answers
    [-list-partitions] (print and exit [0]); collects the partition's tests
    (none registered: exit [0]); resolves configuration from the environment
    mirrors alone ([WINDTRAP_*] — under [dune runtest] they are the CLI;
    resolution errors print and exit [2]); executes through {!Runner.execute}
    with the terminal renderer, wired exactly as the library runner wires it —
    [WINDTRAP_QUIET]/[WINDTRAP_VERBOSE] pick the verbosity level,
    [WINDTRAP_SLOW_THRESHOLD] tunes the slow warnings with ["slow"]-tagged tests
    exempt, and accepted baselines report their written paths project-root
    relative, one behavior across both runners (and GitHub annotations under
    GitHub Actions); writes [.corrected] files; and exits with
    {!inline_exit_code}. *)

val inline_exit_code : Runner.outcome -> int
(** [inline_exit_code outcome] is the inline runner's exit code for [outcome] —
    dune's promotion protocol, the scoped Law 11 deviation:

    - [0] when the run passed, and also when nothing ran (an empty selection is
      an empty partition, not a filter typo);
    - [0] when {e every} failed test's failures are expect mismatches with
      recorded corrections and no fixture release failed: dune's [progn] then
      reaches the [diff?] action, which shows the diff and registers the
      promotion;
    - [1] otherwise: any assertion failure, uncaught exception, timeout,
      unreached expect node, or release failure. Corrections already recorded
      are still written by {!flush_corrections} and surface on the next run.

    Skipped tests are invisible to this rule (RFC amendment C2): a skip — in a
    plain or an expect test — neither forces [1] nor helps reach [0]. A run of
    skips and covered corrections exits [0]; a run of skips and one assertion
    failure exits [1]; an all-skipped run exits [0]. *)

(** {1:seams Test seams} *)

val reset : unit -> unit
(** [reset ()] clears all registration, partition, correction, and protocol
    state, including {!init}'s once-guard — for this module's own test suite,
    which registers synthetic suites repeatedly in one process. Never called by
    generated code. *)
