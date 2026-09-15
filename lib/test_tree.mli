(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The inert test declaration tree.

    A suite is a list of {!type:t} values: leaf tests and nested groups. The
    constructors and the two annotations below are the declaration surface the
    facade re-exports, and windtrap.mli documents what each one promises a user.
    This interface states the three rules the rest of the library depends on.

    {b Declaring is data construction.} Nothing a user wrote runs until the
    runner executes the tree — there are no group hooks of any kind, so no user
    callback can run outside a test's exception boundary, and {!scoped} stores
    its scope and its body unrun so the runner can attribute each one's outcome
    separately. A group's tags, limit and retries are resolved onto its tests at
    {!flatten} time, innermost wins.

    {b A test's path is its identity.} The path is its enclosing group names,
    root first, then its own name; {!path_to_string} renders it, and that
    rendering is what selection filters match, what per-case seed derivation
    hashes, and what the last-failed store records. The joined form is frozen,
    and renaming or regrouping a test intentionally re-keys its property
    streams. Duplicate paths are the runner's startup error; the tree only makes
    paths derivable.

    {b Declaration sites come from [?__POS__], else the call stack.} A node
    records where it was declared, from its [?__POS__] when given and otherwise
    from {!Loc.capture} at declaration time; an annotation never changes it. The
    fallback can mis-attribute when the constructor call is reached through tail
    calls, so a helper that wraps a constructor threads [?__POS__] through. *)

(** {1:trees Trees} *)

type t
(** The type for test trees: a leaf test or a named group of subtrees. Inert
    data; bodies are run only by the runner. *)

type xfail = { reason : string option  (** The known defect, for reports. *) }
(** The type for expected-failure annotations (see {!val:xfail}). *)

(** The type for leaf-test bodies, as stored on the node. *)
type body =
  | Body of (unit -> unit)  (** An ordinary body. *)
  | Scoped : { scope : ('r -> unit) -> unit; body : 'r -> unit } -> body
      (** A {!scoped} test: [scope] is a scoping function and [body] the
          callback it is expected to invoke exactly once. The two are kept apart
          so the runner can run [body] inside [scope] and attribute what comes
          out by how far the callback got. *)

(** {1:declaring Declaring tests}

    Shared arguments: [__POS__] is the declaration position ([__POS__] at the
    call site, see the preamble); [tags] are extra tag names, unioned with
    ancestors' at {!flatten} time; [timeout] is the per-test limit in seconds
    covering setup, body and teardown (for {!scoped}, the whole [scope] call);
    [retries] is the number of extra attempts a failing test gets. On a group,
    [timeout] and [retries] are defaults for every test under it, and the
    innermost declaration wins. Constructors raise [Invalid_argument] if
    [retries < 0] or if [timeout] is given and is not finite and positive. *)

val test :
  ?__POS__:Loc.pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  (unit -> unit) ->
  t
(** [test name fn] declares the test [name] with body [fn]: it fails by raising
    and passes by returning. *)

val slow :
  ?__POS__:Loc.pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  (unit -> unit) ->
  t
(** [slow] is {!test} with the {!Tag.slow} tag pre-applied ([--exclude-tag slow]
    drops it). *)

val group :
  ?__POS__:Loc.pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  t list ->
  t
(** [group name children] declares a group. Groups nest freely; [name] becomes a
    path component, [tags] extend every descendant's effective tags, and
    [timeout] and [retries] are defaults for every test under it. *)

val cases :
  ?__POS__:Loc.pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  name:('a -> string) ->
  string ->
  'a list ->
  ('a -> unit) ->
  t
(** [cases ~name:render base inputs fn] is
    [group base (List.map (fun i -> test (render i) (fun () -> fn i)) inputs)],
    with every child recording the [cases] call's declaration position and the
    optional arguments on the group, so [timeout] and [retries] apply per child.
    [name] is required because a child's path is its identity (see the
    preamble): a positional default would re-key every later child's seeds and
    store entry whenever a row is inserted. *)

val scoped :
  (('r -> unit) -> unit) ->
  ?__POS__:Loc.pos ->
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
    itself; {!Runner} owns the attribution of what comes back out. [scope]
    precedes the optional arguments so that [scoped Eio_main.run] keeps them:
    applying a positional argument erases only the optionals declared before it.
*)

val bracket :
  ?__POS__:Loc.pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  setup:(unit -> 'r) ->
  teardown:('r -> unit) ->
  string ->
  ('r -> unit) ->
  t
(** [bracket ~setup ~teardown name fn] is {!scoped} over the scope
    [fun k -> let r = setup () in match k r with () -> teardown r | exception e
     -> teardown r; raise e], except that a {!Failure.is_fatal} exception skips
    the teardown. So [teardown] runs iff [setup] succeeded, on every outcome of
    [fn] including a skip and a timeout — the runner re-arms the window as the
    body leaves the callback — and a teardown failure is attributed by the
    scoped arm's phase rule: a failure after the callback returned is a
    [Teardown] failure, reported beside the body's. *)

(** {1:annotating Annotations}

    Each annotation is a [t -> t] that rewrites the node it is applied to and
    leaves its declaration site alone; on a group it reaches every test under
    it. *)

val focus : t -> t
(** [focus t] flags [t] — and, through a group, every test under it — as
    focused: when any focused node exists, the runner runs only focused tests
    (see {!section-focus}). *)

val xfail : ?reason:string -> t -> t
(** [xfail t] marks [t] — and, through a group, every test under it — as
    {e expected to fail}: {!Runner} inverts what counts as failed for it, and
    [reason] names the known defect for reports. Nested annotations resolve
    innermost-wins: the one nearest the test is the one recorded on its
    flattened {!type:case}. *)

(** {1:focus Focus} *)

val focus_sites : t list -> Loc.t option list
(** [focus_sites tests] is the declaration site of every {!focus}-flagged node
    in declaration order. Non-empty is what "focus is active" means; the sites
    themselves are the CI refusal's message
    (["focused tests committed (focus at test/test_users.ml:31, …)"]) and the
    out-of-CI warning's. *)

(** {1:flattening Flattening} *)

type case = {
  path : string list;
      (** The test's full path: enclosing group names root-first, then the
          test's own name. Never empty. *)
  body : body;  (** The stored body (see {!type:body}). *)
  loc : Loc.t option;
      (** The declaration site, when known: the location a failure recorded
          without one is attributed to. *)
  tags : Tag.t;  (** Effective tags: the node's own unioned with ancestors'. *)
  focused : bool;  (** [true] iff the test or any ancestor is focused. *)
  timeout : float option;
      (** The innermost [timeout] declared on the test or an ancestor; [None]
          for the runner's default. *)
  retries : int;
      (** The innermost [retries] declared on the test or an ancestor; [0] when
          none. *)
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
