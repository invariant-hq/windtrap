(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The inert test declaration tree.

    A suite is a list of {!type:t} values: leaf tests and nested groups. The
    constructors below are the declaration surface the facade re-exports, and
    windtrap.mli documents what each one promises a user. This interface states
    the three rules the rest of the library depends on.

    {b Declaring is data construction.} Nothing a user wrote runs until the
    runner executes the tree — there are no group hooks of any kind, so no user
    callback can run outside a test's exception boundary, and {!bracket} and
    {!scoped} store their closures unrun so the runner can attribute each one's
    outcome separately.

    {b A test's path is its identity.} The path is its enclosing group names,
    root first, then its own name; {!path_to_string} renders it, and that
    rendering is what selection filters match, what per-case seed derivation
    hashes, and what the last-failed store records. The joined form is frozen,
    and renaming or regrouping a test intentionally re-keys its property
    streams. Duplicate paths are the runner's startup error; the tree only makes
    paths derivable.

    {b Declaration sites come from [?pos], else the call stack.} A node records
    where it was declared, from its [?pos] when given and otherwise from
    {!Loc.capture} at declaration time. Snapshot scoping reads that site's file
    ({!Snapshot.check}), never a frame at snapshot {e call} time. The fallback
    can mis-attribute when the constructor call is reached through tail calls,
    so a helper that wraps a constructor threads [?pos] through. *)

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
          on every outcome, and report body and teardown failures independently.
      *)
  | Scoped : { scope : ('r -> unit) -> unit; body : 'r -> unit } -> body
      (** A {!scoped} test: [scope] is a caller-supplied scoping function and
          [body] the callback it is expected to invoke exactly once. The two are
          kept apart for the same reason {!Bracket}'s three are, but the runner
          has less to promise here: acquisition and release are one call it does
          not control, so it can only run [body] inside [scope] and attribute
          what comes out. *)

(** {1:declaring Declaring tests}

    Shared arguments: [pos] is the declaration position ([__POS__], see the
    preamble); [tags] are extra tag names, unioned with ancestors' at {!flatten}
    time; [timeout] is the per-test limit in seconds covering setup, body and
    teardown (for {!scoped}, the whole [scope] call); [retries] is the number of
    extra attempts a failing test gets. Constructors raise [Invalid_argument] if
    [retries < 0] or if [timeout] is given and is not finite and positive. *)

val test :
  ?pos:Loc.pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  (unit -> unit) ->
  t
(** [test name fn] declares the test [name] with body [fn]: it fails by raising
    and passes by returning. *)

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
(** [slow] is {!test} with the {!Tag.slow} tag pre-applied ([--exclude-tag slow]
    drops it). *)

val cases :
  ?pos:Loc.pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  name:('a -> string) ->
  string ->
  'a list ->
  ('a -> unit) ->
  t
(** [cases ~name:render base inputs fn] declares one test per input: a group
    named [base] whose children, in declaration order, run [fn input] named
    [render input], applied at declaration time. All children share the [cases]
    call's declaration position, and [timeout] and [retries] apply per child.

    [name] is required because a child's path is its identity (see the
    preamble): a positional default would re-key every later child's seeds and
    store entry whenever a row is inserted. *)

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
    The three closures are stored unrun (see {!type:body}). *)

val scoped :
  (('r -> unit) -> unit) ->
  ?pos:Loc.pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  ('r -> unit) ->
  t
(** [scoped scope name fn] declares a test whose resource is scoped by [scope] —
    a function that acquires, calls back, and releases on return
    ([Eio_main.run], [In_channel.with_open_text path]). The runner calls [scope]
    once with a callback that runs [fn] on the resource, and releases nothing
    itself.

    [scope] is positional and precedes the optional arguments so that
    [scoped Eio_main.run] is itself a constructor with [?pos], [?tags],
    [?timeout] and [?retries] intact — applying a positional argument only
    erases the optionals declared {e before} it.

    {!Runner} owns the four-way attribution of what comes back out. *)

val xfail : ?reason:string -> t -> t
(** [xfail t] marks [t] — and, through a group, every test under it — as
    {e expected to fail}: {!Runner} inverts what counts as failed for it, and
    [reason] names the known defect for reports. Nested annotations compose
    innermost-wins, so the annotation closest to a test is the one recorded on
    its flattened {!type:case}. *)

(** {1:focus Focus} *)

val focus_sites : t list -> ([ `Ftest | `Fgroup ] * Loc.t option) list
(** [focus_sites tests] is every focus-flagged node ({!ftest}, {!fgroup}) in
    declaration order, with its kind and declaration location. Non-empty is what
    "focus is active" means; the sites themselves are the CI refusal's message
    (["focused tests committed (ftest at test/test_users.ml:31, …)"]) and the
    out-of-CI warning's. *)

(** {1:flattening Flattening} *)

type case = {
  path : string list;
      (** The test's full path: enclosing group names root-first, then the
          test's own name. Never empty. *)
  body : body;  (** The stored body (see {!type:body}). *)
  loc : Loc.t option;
      (** The declaration site, when known; its file is what snapshot scoping
          keys on. *)
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
    ({!Seed.derive}). The separator is frozen. *)
