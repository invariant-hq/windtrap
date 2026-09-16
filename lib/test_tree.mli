(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The inert test declaration tree.

    A suite is a list of {!type:t} values: leaf tests and nested groups.
    Declaring is data construction: nothing a user wrote runs until the runner
    executes the tree, there are no group hooks, and a group's tags, limit and
    retries are resolved onto its tests at {!flatten} time, innermost wins. A
    test's path (its enclosing group names, then its own name, joined by
    {!path_to_string}) is its identity: what filters match, what seed derivation
    hashes and what the last-failed store records. A node's declaration site
    comes from its [?__POS__] when given and otherwise from {!Loc.capture},
    which can mis-attribute through tail calls; an annotation never changes it.
*)

(** {1:tags Tags}

    Tags are plain strings attached to tests and groups; a test's effective tag
    set is the union of its own and its ancestors'. *)

module Tag : sig
  type t
  (** The type for immutable sets of tag names. *)

  val empty : t
  (** [empty] is the set with no tags. *)

  val of_list : string list -> t
  (** [of_list names] is the set of the tags in [names]. *)

  val union : t -> t -> t
  (** [union parent child] is the union of both sets. *)

  val mem : string -> t -> bool
  (** [mem name tags] is [true] iff [name] is in [tags]. *)

  val slow : string
  (** [slow] is ["slow"], pre-applied by the {!slow} constructor. An ordinary
      tag: [--exclude-tag slow] drops it. *)

  val prop : string
  (** [prop] is ["prop"], pre-applied by the property constructors. The run
      header prints the root seed iff a test carries it. *)

  type predicate
  (** The type for tag selection predicates: a set of required tags and a set of
      dropped tags. A tag cannot be both; adding it to one set removes it from
      the other, so the last flag wins. *)

  val any : predicate
  (** [any] requires nothing and drops nothing. *)

  val require : string -> predicate -> predicate
  (** [require name p] is [p] requiring [name]. *)

  val drop : string -> predicate -> predicate
  (** [drop name p] is [p] dropping [name]. *)

  val accepts : predicate -> t -> bool
  (** [accepts p tags] is [true] iff [tags] contains every required tag of [p]
      and none of its dropped tags. *)
end

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
          callback it is expected to invoke exactly once, kept apart so the
          runner can attribute what comes out by how far the callback got. *)

(** {1:declaring Declaring tests}

    Shared arguments: [__POS__] is the declaration position; [tags] are extra
    tag names, unioned with ancestors'; [timeout] is the per-test limit in
    seconds covering setup, body and teardown (for {!scoped}, the whole [scope]
    call); [retries] is the number of extra attempts a failing test gets. On a
    group, [timeout] and [retries] are defaults for every test under it,
    innermost wins. Constructors raise [Invalid_argument] if [retries < 0] or if
    [timeout] is given and is not finite and positive. *)

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
(** [slow] is {!test} with the {!Tag.slow} tag pre-applied. *)

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
    every child recording the [cases] call's declaration position and the
    optional arguments sitting on the group, so [timeout] and [retries] apply
    per child. *)

val scoped :
  (('r -> unit) -> unit) ->
  ?__POS__:Loc.pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  ('r -> unit) ->
  t
(** [scoped scope name fn] declares a test whose resource is scoped by [scope],
    a function that acquires, calls back and releases on return ([Eio_main.run],
    [In_channel.with_open_text path]). The runner calls [scope] once with a
    callback that runs [fn] on the resource, and releases nothing itself.
    [scope] precedes the optional arguments so that [scoped Eio_main.run] keeps
    them. *)

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
    the teardown. [teardown] runs iff [setup] succeeded, on every outcome of
    [fn] including a skip and a timeout, and a teardown failure is a [Teardown]
    failure reported beside the body's. *)

(** {1:annotating Annotations}

    Each annotation is a [t -> t] that rewrites the node it is applied to and
    leaves its declaration site alone; on a group it reaches every test under
    it. *)

val focus : t -> t
(** [focus t] flags [t], and through a group every test under it, as focused:
    when any focused node exists, the runner runs only focused tests. *)

val xfail : ?reason:string -> t -> t
(** [xfail t] marks [t], and through a group every test under it, as expected to
    fail; [reason] names the known defect for reports. Nested annotations
    resolve innermost-wins. *)

(** {1:focus Focus} *)

val focus_sites : t list -> Loc.t option list
(** [focus_sites tests] is the declaration site of every {!focus}-flagged node
    in declaration order; non-empty means focus is active. *)

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
(** The type for flattened leaf tests, with ancestry already applied. *)

val flatten : t list -> case list
(** [flatten tests] is the leaf tests of [tests] in depth-first declaration
    order, the runner's execution order. Pure: bodies are not run. *)

val path_to_string : string list -> string
(** [path_to_string path] joins [path] with [" › "], the canonical rendering
    matched by [-f]/[-e] filters and hashed by {!Seed.derive}. The separator is
    frozen. *)
