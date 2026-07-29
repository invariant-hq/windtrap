(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The inert test declaration tree.

    A suite is a list of {!type:t} values: leaf tests and nested groups,
    declared with the constructors below — the declaration surface the facade
    re-exports (RFC "The declaration surface"). Declaring is pure data
    construction: no body, setup, or teardown runs until the runner executes the
    tree. There are no group hooks of any kind — no user callback can run
    outside a test's exception boundary (RFC Law 8) — and {!bracket} stores its
    three closures unrun, so the runner captures body and teardown outcomes
    independently.

    {b Paths.} A test is named by its {e path}: the names of its enclosing
    groups, root first, then its own name. {!flatten} derives every path;
    {!path_to_string} renders one as the canonical string that selection filters
    match and per-case seed derivation hashes (RFC Law 7 — the joined form is
    frozen; renaming or regrouping a test intentionally re-keys its property
    streams). Duplicate full paths are a startup error detected by the runner;
    the tree only makes paths derivable.

    {b Declaration files.} Each test records the source file it was declared in,
    consumed by snapshot scoping (RFC "Snapshots", location resolution rule
    (2)): a [snapshot] call without [~pos] scopes its baseline by the enclosing
    test's declaration file. The file comes from the constructor's [?pos] when
    given, and otherwise from a best-effort walk of the call stack at
    declaration time ({!Loc.capture}) — never from backtrace frames at snapshot
    {e call} time. The fallback can mis-attribute when the constructor call is
    reached through tail calls (the declaring frame is gone), so helpers that
    wrap constructors should thread [?pos] through. *)

(** {1:trees Trees} *)

type t
(** The type for test trees: a leaf test or a named group of subtrees. Inert
    data; bodies are run only by the runner. *)

type xfail = { reason : string option  (** The known defect, for reports. *) }
(** The type for expected-failure annotations (see {!val:xfail}). *)

(** The type for leaf-test bodies, as stored on the node. *)
type body =
  | Body of (unit -> unit)  (** An ordinary body. *)
  | Bracket : {
      setup : unit -> 'r;
      body : 'r -> unit;
      teardown : 'r -> unit;
    }
      -> body
      (** A {!bracket} test, kept as three separate closures — never
          pre-composed — so the runner can run [teardown] iff [setup] succeeded,
          on every outcome, and report body and teardown failures independently
          (RFC "Resources"). *)

(** {1:declaring Declaring tests}

    Constructor arguments common to several constructors:

    - [pos] is the declaration position ([__POS__]). It records the node's
      location and declaration file; when omitted both are captured from the
      call stack, best effort (see the module preamble).
    - [tags] are extra tag names for the node, unioned with ancestors' tags at
      {!flatten} time. Defaults to [[]].
    - [timeout] is the per-test limit in seconds, covering setup, body, and
      teardown. Defaults to the runner's default timeout.
    - [retries] is the number of extra attempts the runner gives a failing test.
      Defaults to [0].

    Constructors raise [Invalid_argument] if [retries < 0] or if [timeout] is
    given and is not finite and positive. *)

val test :
  ?pos:Loc.pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  (unit -> unit) ->
  t
(** [test name fn] declares the test [name] with body [fn]. The body runs inside
    the runner's per-test boundary; it fails by raising (assertion verbs, any
    exception) and passes by returning. *)

val ftest :
  ?pos:Loc.pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  (unit -> unit) ->
  t
(** [ftest] is {!test} with the focus flag set: when any focused node exists,
    the runner runs only focused tests (see {!section-focus}). *)

val group : ?pos:Loc.pos -> ?tags:string list -> string -> t list -> t
(** [group name children] declares a group. Groups nest freely; [name] becomes a
    path component and [tags] extend every descendant's effective tags. *)

val fgroup : ?pos:Loc.pos -> ?tags:string list -> string -> t list -> t
(** [fgroup] is {!group} with the focus flag set: every test under it is
    focused. *)

val slow :
  ?pos:Loc.pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  (unit -> unit) ->
  t
(** [slow] is {!test} with the {!Tag.slow} tag pre-applied ([-q] drops it). *)

val cases :
  ?pos:Loc.pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  ?name:('a -> string) ->
  string ->
  'a list ->
  ('a -> unit) ->
  t
(** [cases ?name:render name inputs fn] declares one test per input: a group
    named [name] whose [i]th child (zero-based, declaration order) runs
    [fn input]. The child is named [render input] when [render] is given —
    applied at declaration time — and ["<name>.<i>"] otherwise, making each
    sub-test individually selectable by path filter. All children share the
    [cases] call's declaration position and file, and each child runs under
    [timeout] and [retries] — per child, not per table: every input gets the
    full budget. *)

val bracket :
  ?pos:Loc.pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  setup:(unit -> 'r) ->
  teardown:('r -> unit) ->
  string ->
  ('r -> unit) ->
  t
(** [bracket ~setup ~teardown name fn] declares a test scoping a resource: the
    runner calls [setup ()], passes the resource to [fn], and calls [teardown]
    on it iff [setup] succeeded — on every outcome, including skip and timeout.
    The three closures are stored unrun (see {!type:body}); partial application
    ([let with_db = bracket ~setup ~teardown]) builds reusable constructors. *)

val xfail : ?reason:string -> t -> t
(** [xfail t] marks [t] — and, through a group, every test under it — as
    {e expected to fail} (amendment B12). Marked tests still run; the runner
    inverts what counts as failed: a failing outcome reports as an expected
    failure and does not fail the run, while a passing outcome fails loudly
    (["expected to fail, but the test passed"]). Skips are unaffected. Expected
    failures never enter the last-failed store; an unexpected pass does (see
    {!Runner}).

    [reason] names the known defect for reports (e.g. ["issue #42"]). Nested
    annotations compose innermost-wins: the annotation closest to a test is the
    one recorded on its flattened {!type:case}.

    Use [xfail] to keep a known-bug reproduction in-tree without a red run; use
    {!Tag.disabled} or [skip] when the body must not run at all. *)

(** {1:focus Focus} *)

val has_focus : t list -> bool
(** [has_focus tests] is [true] iff any node in [tests] carries the focus flag
    ({!ftest}, {!fgroup}). *)

val focus_sites : t list -> ([ `Ftest | `Fgroup ] * Loc.t option) list
(** [focus_sites tests] is every focus-flagged node in declaration order, with
    its kind and declaration location — the data behind the CI focus guard's
    error message
    (["focused tests committed (ftest at test/test_users.ml:31, …)"], RFC "The
    declaration surface") and the runner's out-of-CI warning. *)

(** {1:flattening Flattening} *)

type case = {
  path : string list;
      (** The test's full path: enclosing group names root-first, then the
          test's own name. Never empty. *)
  body : body;  (** The stored body (see {!type:body}). *)
  loc : Loc.t option;  (** The declaration location, when known. *)
  file : string option;
      (** The declaration file (compile-time path), for snapshot scoping. *)
  tags : Tag.t;  (** Effective tags: the node's own unioned with ancestors'. *)
  focused : bool;  (** [true] iff the test or any ancestor is focused. *)
  timeout : float option;  (** The declared per-test limit, seconds. *)
  retries : int;  (** Declared extra attempts. *)
  xfail : xfail option;
      (** The innermost {!val:xfail} annotation on the test or an ancestor;
          [None] for a test expected to pass. *)
}
(** The type for flattened leaf tests: everything the runner needs to select and
    execute one test, with ancestry already applied. *)

val flatten : t list -> case list
(** [flatten tests] is the leaf tests of [tests] in depth-first declaration
    order — the runner's execution order. Pure: bodies are not run. *)

val path_to_string : string list -> string
(** [path_to_string path] joins [path] with [" › "] — the canonical rendering
    matched by [-f]/[-e] filters and hashed by per-case seed derivation
    ({!Seed.derive}). The separator is frozen (RFC Law 7). *)
