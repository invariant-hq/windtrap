(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The inert test declaration tree.

    A suite is a list of {!t} values, leaf tests and nested groups. Declaring is
    data construction. A constructor stores a body, a scope, a setup and a
    teardown without calling them, and the [name] of {!cases} is the one
    function of the user that a constructor calls. A group has no hooks, so
    every callback that a tree stores belongs to one test, and the runner calls
    it inside the exception boundary of that test. {!flatten} resolves what the
    groups declare onto their tests.

    The path of a test is the names of its enclosing groups, outermost first,
    then its own. {!path_to_string} joins it into the path string, which is the
    identity of the test. The filters of a selection match it, {!Seed.derive}
    hashes it and the last-failed store records it, so renaming a test or moving
    it to another group changes all three. {!flatten} returns two tests that
    have one path as they are, and {!Run} refuses such a suite
    ({!Run.Duplicate_paths}).

    The declaration site of a node is [Loc.resolve ?__POS__ ()], fixed when its
    constructor is applied. A capture cannot see through a helper that wraps a
    constructor, so a function of the library that wraps one must take
    [?__POS__] and pass it on. *)

(** {1:tags Tags}

    A tag is a plain string on a test or a group. The effective tags of a test
    are its own and those of its ancestors. *)

module Tag : sig
  (** Tag sets and the predicates that select over them. *)

  type t
  (** The type for immutable sets of tag names. *)

  val mem : string -> t -> bool
  (** [mem name tags] is [true] iff [name] is in [tags]. *)

  val slow : string
  (** [slow] is ["slow"], the tag that {!Test_tree.slow} adds. A test that
      carries it, on itself or on an ancestor, is left out of the slow tests of
      a run (see {!Run.result.slow_tagged}). It is otherwise an ordinary tag,
      which [--exclude-tag slow] drops. *)

  val prop : string
  (** [prop] is ["prop"], the tag of a property. A selection holds a property
      iff a selected test carries it ({!Run.Run_started}). *)

  type predicate
  (** The type for tag selection predicates: a set of required tags and a set of
      dropped tags. No tag is in both, so for a tag given to {!require} and to
      {!drop} the later call decides.

      {!Run} applies every [--tag] and then every [--exclude-tag], so a tag
      given to both flags is excluded, whatever their order on the command line.
  *)

  val any : predicate
  (** [any] requires nothing and drops nothing, so it accepts every set. *)

  val require : string -> predicate -> predicate
  (** [require name p] is [p] with [name] required and no longer dropped. *)

  val drop : string -> predicate -> predicate
  (** [drop name p] is [p] with [name] dropped and no longer required. *)

  val accepts : predicate -> t -> bool
  (** [accepts p tags] is [true] iff [tags] holds every required tag of [p] and
      none of its dropped tags. *)

  (**/**)

  (* Exported for the unit suites. Every other client reads a set with [mem]
     and [accepts]. [empty] is the set without a tag, [of_list names] is the set
     of the tags of [names], and [union a b] is the set of the tags of either.
  *)

  val empty : t
  val of_list : string list -> t
  val union : t -> t -> t

  (**/**)
end

(** {1:trees Trees} *)

type t
(** The type for test trees: a leaf test, or a named group of trees. A value is
    inert data. A name is any string, and no constructor validates one. *)

type xfail = { reason : string option  (** The known defect, when stated. *) }
(** The type for expected-failure annotations (see {!val-xfail}). In a
    {!type-case}, [None] is a test expected to pass, and
    [Some { reason = None }] one expected to fail for no stated reason. *)

(** The type for the bodies of leaf tests, as a node stores them (see
    {{!Run.section-scoped}scoped tests} for how the runner calls a [Scoped]). *)
type body =
  | Body of (unit -> unit)  (** An ordinary body. *)
  | Scoped : { scope : ('r -> unit) -> unit; body : 'r -> unit } -> body
      (** The body of a {!scoped} test. [scope] is the scoping function and
          [body] the callback that it must call once. *)

(** {1:declaring Declaring tests}

    The six constructors take the same four optional arguments, and each stores
    what it is given on the node that it builds.
    - [__POS__] is the declaration site (see the preamble of the module).
    - [tags] are the tags of the node itself. Defaults to [[]].
    - [timeout] is the limit of a test in seconds, over its setup, its body and
      its teardown, and for {!scoped} over the whole call of [scope].
    - [retries] is the number of extra attempts that a failing test gets.

    A node without [timeout] or without [retries] declares none, and {!flatten}
    takes that of the nearest enclosing group that declares one, so on a group
    the two are the defaults of every test under it.

    Every constructor raises [Invalid_argument], when it is applied, if
    [timeout] is given and is not finite and positive, or if [retries] is
    negative. *)

val test :
  ?__POS__:Loc.pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  (unit -> unit) ->
  t
(** [test name fn] is the test [name] with the body [fn], stored as a {!Body}.
    The body passes by returning and fails by raising. *)

val slow :
  ?__POS__:Loc.pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  (unit -> unit) ->
  t
(** [slow name fn] is {!val-test} with {!Tag.slow} added to [tags]. *)

val group :
  ?__POS__:Loc.pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  t list ->
  t
(** [group name children] is the group [name] over [children], which can hold
    groups in turn. [name] is a component of the path of every test under the
    group, and [tags] join their effective tags. *)

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
(** [cases ~name base inputs fn] is
    [group base (List.map (fun i -> test (name i) (fun () -> fn i)) inputs)],
    with two differences. Every child has the site of the [cases] call as its
    declaration site. The optional arguments sit on the group, so [tags] reach
    every child, and [timeout] and [retries] apply to each of them.

    [name] is applied to every input, in the order of [inputs], when [cases] is
    applied, so what it raises escapes at declaration, outside any test. *)

val scoped :
  (('r -> unit) -> unit) ->
  ?__POS__:Loc.pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  ('r -> unit) ->
  t
(** [scoped scope name fn] is the test [name] whose body [fn] receives the
    resource that [scope] provides, stored as a {!Scoped} with nothing called.
    [scope] is a function that acquires a resource, calls back with it, and
    releases it when the callback returns, as [In_channel.with_open_text path]
    and [Eio_main.run] are.

    [scope] comes before the optional arguments, so the partial application
    [scoped Eio_main.run] keeps them. *)

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
    {[
    fun k ->
      let r = setup () in
      match k r with
      | () -> teardown r
      | exception e ->
          teardown r;
          raise e
    ]}
    except that [k r] runs through {!Failure.catch}, so what it never returns
    skips [teardown], and that an [e] other than a {!Failure.Check_failure} or a
    {!Failure.Control} keeps its backtrace. So [teardown] runs iff [setup]
    returned, and then on every outcome of [fn], a skip and a timeout included.
    When the body leaves the callback the runner arms what is left of the limit,
    or the whole limit when none is left, so the teardown of a body that timed
    out is bounded too. *)

(** {1:annotating Annotations}

    An annotation marks the node that it is applied to and leaves its
    declaration site alone. On a group it reaches every test under it, which
    {!flatten} resolves. *)

val focus : t -> t
(** [focus t] is [t] marked as focused. When a suite holds such a node, the
    runner keeps only the tests of its selection that are focused (see
    {!focus_sites}). *)

val xfail : ?reason:string -> t -> t
(** [xfail ?reason t] is [t] marked as expected to fail. The mark is inert here,
    and the runner inverts what counts as failed for such a test (see
    {!Run.result.counted}). Such a test reaches no mutant (see {!Mutate_loop}).
    [reason] is the known defect, and without it the annotation is
    [{ reason = None }].

    The annotation nearest a test is the one that its {!type-case} carries. On
    one node the first one applied stays, so
    [xfail ~reason:"b" (xfail ~reason:"a" t)] keeps ["a"]. *)

val focus_sites : t list -> Loc.t option list
(** [focus_sites tests] is the declaration site of every node of [tests] that
    {!focus} was applied to, in declaration order, a group before what it holds.
    A group counts once whatever it holds, and the entry of a node without a
    known site is [None]. Focus is active iff the list is not empty, and
    {!Run.Focused_in_ci} carries it. *)

(** {1:flattening Flattening} *)

type case = {
  path : string list;  (** The path of the test. It is never empty. *)
  body : body;  (** The stored body. *)
  loc : Loc.t option;
      (** The declaration site, when it is known, to which a failure without a
          location is attributed. *)
  tags : Tag.t;
      (** The effective tags: those of the test and of its ancestors. *)
  focused : bool;
      (** [true] iff the test or one of its ancestors is focused. *)
  timeout : float option;
      (** The limit declared nearest the test, on itself or on an ancestor.
          [None] leaves the default of the runner. *)
  retries : int;
      (** The retries declared nearest the test, and [0] when none are. It is
          never negative. *)
  xfail : xfail option;  (** The {!val-xfail} annotation nearest the test. *)
}
(** The type for flattened tests, with ancestry applied: all that the runner
    needs to select and execute one test. *)

val flatten : t list -> case list
(** [flatten tests] is the tests of [tests] in depth-first declaration order,
    which is the order in which the runner executes them. It runs no body. *)

val path_to_string : string list -> string
(** [path_to_string path] is the components of [path] joined with [" › "]: a
    space, U+203A and a space. The separator is frozen. With another one a
    recorded seed replays other cases, no entry of the last-failed store names a
    test, and every shard bucket changes. *)
