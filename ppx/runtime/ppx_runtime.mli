(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The ordinary-OCaml half of [ppx_windtrap]: registration, expect-test
    semantics, corrections, and the inline-test-runner protocol.

    The PPX is a location recorder and nothing more. It rewrites [let%test] /
    [module%test] / [let%expect_test] into the registration calls below, each
    [[%expect]] / [[%expect_exact]] node into {!expect}, and [[%expect.output]]
    into {!expect_output}, passing source locations, payload literals, and the
    ambient [Expect_test_config]'s [run] and [sanitize]. Every semantic decision
    — matching, normalization, per-node reachability, correction formatting,
    exit codes — lives here, in ordinary OCaml, testable without the PPX. Why
    each is what it is, is argued in [ppx_runtime.ml] beside the code it
    justifies; this signature states what a caller may rely on.

    {b Life cycle.} Module initializers run the registration calls as the test
    library loads, building a per-source-file registry. The generated runner
    main is [init Sys.argv; exit ()]: under dune's [inline_tests] backend the
    argument vector carries the [inline-test-runner <lib> -partition <file>]
    protocol, and {!exit} collects the partition, executes it, writes pending
    [.corrected] files beside the copied source in dune's sandbox — accepting
    them into the source tree as well under [WINDTRAP_UPDATE] — and terminates
    with the promotion-protocol exit code.

    Registration state is module-global by nature, since module initializers run
    before any run record exists; everything per-run — capture, results,
    per-test expect state — lives in the run record and the per-test frames. *)

(* The shared core vocabulary, substituted rather than aliased: the
   runtime lives outside the core, and these names must mean the core's
   modules without this signature re-exporting them. *)
module Test_tree := Windtrap.Private.Test_tree
module Runner := Windtrap.Private.Runner

(** {1:locations Locations} *)

type loc = { line : int; start_bol : int; start_pos : int; end_pos : int }
(** The type for source ranges. [start_pos] and [end_pos] are byte offsets into
    the file; [line] is 1-based and [start_bol] is the offset of [line]'s first
    byte, so a node's column is [start_pos - start_bol]. *)

(** {1:nodes Expect nodes} *)

(** The type for payload delimiters, preserved so corrections keep the author's
    spelling. *)
type delimiter =
  | Quote
      (** ["…"] — corrections escape every line and newline onto one source
          line. *)
  | Tag of string  (** [{tag|…|tag}] — corrections re-tag on conflict. *)

type payload = { contents : string; delimiter : delimiter; literal_loc : loc }
(** The type for expect payloads: the literal's [contents] as written, its
    delimiter, and the extent of the literal itself — the range a correction
    patches. *)

(** The type for node kinds: which extension the node was written as. *)
type node_kind =
  | Expect  (** [[%expect]]: matched via {!Private.normalize}. *)
  | Expect_exact  (** [[%expect_exact]]: matched byte for byte. *)

type node = {
  id : int;
  kind : node_kind;
  loc : loc;
  payload : payload option;  (** [None] for a bare [[%expect]]. *)
}
(** The type for declared expect nodes. Ids are the node's index in the body, in
    source order, from [0]; the rewritten node calls {!expect} with the same
    id. *)

(** {1:registration Registration}

    Called from generated module initializers, before any run starts. [file] is
    the compile-time source path ([loc.loc_start.pos_fname]); its basename is
    the test's partition and its capitalized module name becomes the grouping
    group. [loc] is the extension point's location; [tags] come from [[@tags]]
    attributes.

    A name already registered in the same scope — the enclosing [module%test]
    group, or the file's top level — is renamed by appending [" (2)"], [" (3)"],
    …, so that a functor instantiated several times runs every instance under a
    path the runner's uniqueness law accepts. *)

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
(** [add_expect_test ~file ~loc ~tags ~run ~sanitize ~nodes ~body_loc ~body_wrap
     ~trailing_loc name body] registers the [let%expect_test] test [name]. The
    generated call passes [run] and [sanitize] as [Expect_test_config.run] /
    [Expect_test_config.sanitize] — the {e ambient} names, so a user module
    shadowing [Expect_test_config] is honored, and a monadic config fails to
    compile at those references ([run] must fit [(unit -> unit) -> unit]).
    [sanitize] is applied to every read of captured output.

    - [nodes] declares every expect node lexically inside [body].
    - [body_loc] spans from the [let%expect_test] keyword to the end of the
      body: its column indents an inserted trailing node, and its end is where
      the separating [";"] goes.
    - [body_wrap] is [Some offset] when the body is a bare [match], [try] or
      [function] — the offset of its first character — so the same patch can
      parenthesize it. Otherwise that [";"] would bind to the body's last arm
      and strand the inserted node inside it. [None] for every other shape.
    - [trailing_loc] is the zero-width point where a trailing-output correction
      inserts a new node (the end of the extension point).

    A body that returns is checked for trailing output and per-node
    reachability: a node reached twice and a node reached never cannot cancel
    out.

    Everything the body raises propagates to the runner —
    [Failure.Check_failure], [Failure.Skip_test], [Failure.Timeout] and any
    other exception alike — and none of it is a correction. Nodes reached
    before an exception still resolve, but nothing is spliced at the trailing
    point, since a node inserted after a raising statement could never be
    reached on a future run. To pin an expected exception, catch and print it,
    then match it with an ordinary node.

    A skip makes the test an ordinary skip: nothing is checked and nothing is
    recorded — no correction for any node, the ones reached before the skip
    included, no trailing insertion, no unreached-node failure — and the test
    plays no part in the promotion exit rule. *)

(** {1:execution Expect node execution} *)

val expect : id:int -> unit
(** [expect ~id] runs the declared node [id] of the executing expect test:
    consumes captured output, sanitizes it, records the node's result. A
    mismatch is recorded and corrected but does {e not} raise here; failures are
    reported when the body ends.

    Raises [Invalid_argument] when no expect test is executing or [id] was not
    declared — unreachable through the PPX, which rejects [[%expect]] outside
    [let%expect_test]. *)

val expect_output : unit -> string
(** [expect_output ()] is [[%expect.output]]: consumes and returns the captured
    output since the previous consumption, sanitized.

    Raises [Invalid_argument] when no expect test is executing. *)

(** {1:protocol The runner protocol}

    The backend invokes the generated runner as
    [inline-test-runner <lib> -partition <file>], and once with
    [-list-partitions] to enumerate partitions. *)

val init : string array -> unit
(** [init argv] parses the inline-test-runner protocol out of [argv]:
    [inline-test-runner <lib>] (runner mode and the library name),
    [-partition <file>] and [-list-partitions]. Unrecognized arguments are
    ignored. Only the first call parses; later calls are no-ops. Every call
    claims the registry (see {!section:undriven}). *)

val exit : unit -> 'a
(** [exit ()] runs the inline suite and terminates the process.

    Not in runner mode — {!init} saw no [inline-test-runner] — it exits [0]: the
    generated runner does nothing when invoked by hand. Otherwise it answers
    [-list-partitions] (print, exit [0]); collects the partition (nothing
    registered: exit [0]); resolves configuration from the [WINDTRAP_*] mirrors
    alone, which are the CLI under [dune runtest] (a resolution error prints and
    exits [2]); executes through [Runner.execute] with the terminal renderer,
    wired exactly as the library runner wires it, so verbosity, slow warnings,
    accepted-baseline paths and GitHub annotations behave identically under both
    runners; flushes the corrections, accepting them into the source tree when
    the run resolved to [Snapshot.Update] {e and} this partition's own verdict
    was clean; names on [stderr] what it wrote; and exits with
    {!Private.inline_exit_code}, forced to [1] when a correction reached neither
    channel — dune's promotion diff can only surface corrections that exist. *)

(** {1:undriven The undriven-registration guard}

    The silent success this closes: preprocessed test code inside a plain
    [(executable)] or [(test)] stanza registers its tests at module load, and
    with no [(inline_tests)] stanza nothing ever drives the registry — the
    binary exits [0] having run nothing. So a process that terminates normally
    with registrations no driving path ever claimed prints a diagnostic on
    [stderr] — naming the registered files, the missing stanza and the runner
    protocol — and exits [2], Law 11's nothing-ran code.

    {b The claim rule.} The registry is claimed, once for the process's life,
    by {!init} in every mode; by {!Private.collect}, since whoever drains the
    registry owns the execution of what they took, which covers a hand-rolled
    harness driving [Runner] directly; by arming a mutant, through the
    [Registry.on_armed] hook this module registers, that process's transcript
    belonging to the mutation loop (Law 16); and by {!Private.reset}, whose
    caller owns the registry by construction.

    Running a suite claims nothing by itself. A standalone [Windtrap.run]
    executable that also links preprocessed test code it never drains dies with
    the diagnostic, because those registrations can run under no invocation of
    that executable — which is the defect, not a false positive.

    The guard is best-effort, against the silent [0] only: death by signal and
    [Unix._exit] bypass [at_exit], and those endings are already loud or
    deliberate. *)

(** {1:private The test-only surface}

    Everything below is the runtime's own test suite reaching into its
    implementation: the seams that let [test/unit/test_ppx_runtime.ml] check
    normalization, collection, correction formatting, the flush and the exit
    protocol as ordinary functions rather than as process transcripts.
    Generated code calls none of it, and neither should anything else. *)

module Private : sig
  val normalize : string -> string
  (** [normalize s] is the [[%expect]] matching form of [s]: lines stripped of
      surrounding whitespace, blank edges dropped, the block dedented by the
      minimum indentation of its nonempty lines. Two payloads match iff their
      normalizations are equal, which is ppx_expect's default formatting
      flexibility exactly. *)

  val collect : unit -> Test_tree.t list
  (** [collect ()] drains the registry into a test tree: registrations grouped
      per source file under the file's module name ([my_file.ml] → [My_file]),
      files in first-registration order, restricted to the [-partition] file
      when {!init} parsed one. A second call is [[]] until new registrations
      arrive. Draining claims the registry (see {!section:undriven}).

      Raises [Invalid_argument] if a group opened by {!enter_group} was never
      closed. *)

  val partitions : unit -> string list
  (** [partitions ()] is the sorted basenames of the source files seen by
      registration — the [-list-partitions] answer. *)

  val corrected_source : file:string -> source:string -> string option
  (** [corrected_source ~file ~source] is [source] with every correction
      recorded for [file] applied, or [None] when none were; the filesystem is
      not touched. A correction patches the payload literal's extent, so every
      other byte of the file keeps its place — except for the two shapes whose
      payload {e is} the node, a bare [[%expect]] and the [{%expect …|}]
      shorthand, which are rewritten whole. *)

  type flush_report = {
    written : string list;  (** [.corrected] names written beside the source. *)
    accepted : string list;
        (** Source files rewritten in place, project-root relative. *)
    refused : string list;
        (** Sources whose correction did not fully land, each already reported
            on [stderr]. *)
  }
  (** The type for what one flush did. *)

  val flush_corrections_report : accept:bool -> flush_report
  (** [flush_corrections_report ~accept] writes [<basename>.corrected] beside
      each corrected source, where dune's diff action and [dune promote] expect
      it, and clears the table; with [accept] every correction is
      {e additionally} accepted into the source tree, guarded by a drift check
      against the bytes it was computed from. Nothing fails silently: a file
      that could not be read, written or accepted is named with its reason on
      [stderr] and returned in [refused]. *)

  val inline_exit_code : Runner.outcome -> int
  (** [inline_exit_code outcome] is the inline runner's exit code — dune's
      promotion protocol, not the standalone runner's [0]/[1]/[2] contract. It
      is [0] when the run passed, when nothing ran (an empty selection is an
      empty partition, not a filter typo), and when {e every} failed test's
      failures are expect mismatches with recorded corrections, which is what
      lets dune reach the [diff?] step that registers the promotion; [1]
      otherwise. A skip neither forces [1] nor helps reach [0]. *)

  val correction_notice :
    accepted:string list ->
    refused:string list ->
    declined:bool ->
    string list ->
    string option
  (** [correction_notice ~accepted ~refused ~declined written] is the [stderr]
      notice for a process that wrote the [.corrected] files [written], or
      [None] when [written] is empty. Its first line names them
      unconditionally; the explanation under it names exactly one case — the
      paths [accepted] into the source tree, the [refused] ones, an acceptance
      [declined] until the failures are fixed, or dune's rule that a correction
      is registered for promotion only when every partition of the library
      exits cleanly. *)

  val reset : unit -> unit
  (** [reset ()] restores {e every} piece of state this module keeps between
      calls to its module-load value; the module-load cwd is not run state and
      survives. For this module's own test suite, which registers synthetic
      suites repeatedly in one process, and never called by generated code.
      Calling it claims the registry (see {!section:undriven}). *)
end
