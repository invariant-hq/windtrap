(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Unit, property, stateful and expect tests from one flat interface.

    {!val:test} and {!group} declare tests, the
    {{!section-assertions}assertion verbs} check values inside their bodies, and
    {!run} executes the suite and returns its exit code:

    {[
    open Windtrap

    let parsing =
      group "int_of_string"
        [
          test "reads a decimal literal" (fun () ->
              equal int 42 (int_of_string "42"));
          test "rejects the empty string" (fun () ->
              raises (Failure "int_of_string") (fun () -> int_of_string ""));
        ]

    let reversal =
      group "List.rev"
        [
          prop "is its own inverse"
            Gen.(list int)
            (fun l -> equal (list int) l (List.rev (List.rev l)));
        ]

    let () = exit (run "stdlib" [ parsing; reversal ])
    ]}

    - {{!section-declaring}Declaring tests}
    - {{!section-assertions}Assertions}
    - {{!section-witnesses}Witnesses}
    - {{!section-properties}Properties}
    - {{!section-stateful_tests}Stateful tests}
    - {{!section-baselines}Baselines}
    - {{!section-capture}Captured output} and {{!section-body}the running test}
    - {{!section-running}Running}

    The companion package [ppx_windtrap] adds inline tests, [let%test] and
    [let%expect_test] with its [[%expect]] nodes, accepted through
    [dune promote]. *)

(** {1:types Types} *)

type test = Test_tree.t
(** The type for declared tests and groups of tests. A value is inert, and
    nothing runs until {!run} executes it. *)

type pos = string * int * int * int
(** The type for [__POS__] values: file, line, start column, end column.

    A [?__POS__] argument is the source location a declaration or a failure
    reports. It defaults to a best-effort capture of the call stack, which needs
    debug information ([-g], dune's default).

    The capture finds nothing for an assertion in tail position, whose frame is
    gone when it raises. The failure then reports the [file:line] of the test's
    declaration. [~__POS__] on the assertion gives the assertion's line. An
    assertion that ends a [let%test] or [let%expect_test] body is not in tail
    position once [ppx_windtrap] has expanded the body, and gives its own line.

    A helper that wraps a verb or a constructor reports a line of its own. *)

type 'a printer = Format.formatter -> 'a -> unit
(** The type for value printers. *)

module Testable = Testable
(** Witness constructors and accessors. *)

type 'a testable = 'a Testable.t
(** The type for witnesses of ['a] values: a printer, an equality and an
    optional order. The printer and the equality must be total, and an exception
    from either escapes the verb. The equality is applied to the expected value
    first and the actual value second. *)

(** {1:declaring Declaring tests}

    A test is named by its path: the names of its enclosing groups, then its
    own, printed joined with [" › "]. [-f], or a bare pattern on the command
    line, keeps the tests whose path string contains the pattern, and [-e] drops
    them. {!run} refuses a suite in which two tests have one path.

    The path is also the test's identity. It keys the
    {{!section-properties}seeds} of a property, the last failed tests [--failed]
    selects and the bucket of [--shard]. Renaming or regrouping a test changes
    all three.

    {!val:test}, {!group}, {!slow}, {!cases}, {!bracket} and {!scoped} take the
    same four optional arguments.
    - [__POS__] is the declaration site.
    - [tags] are tag names, added to those of the enclosing groups. A tag named
      by both [--tag] and [--exclude-tag] is excluded.
    - [timeout] is the test's limit in seconds. Defaults to the limit of the
      nearest enclosing group that declares one, then to [--timeout], then to no
      limit.
    - [retries] is the number of extra attempts a failing test gets. Defaults to
      the [retries] of the nearest enclosing group that declares one, then to
      [0].

    {b Timeouts.} The limit covers setup, body and teardown. Setup and body
    share the window, and a teardown gets what is left of it. When they spent it
    all, the teardown gets a fresh window.

    The limit is a [SIGALRM] interval timer, so it has no effect on Windows and
    cannot interrupt a blocked C call. The runner owns [SIGALRM] while a test
    with a limit runs. A domain that the test spawned may take the signal: the
    timeout is then raised on that domain, and ends the test only when it
    reaches the test's domain, as through [Domain.join].

    {b Retries.} Each attempt is a fresh setup, body, teardown and capture. A
    skip is never retried. An {!xfail} test is retried on an unexpected pass,
    never on its expected failure.

    Each of these constructors raises [Invalid_argument], when it is applied, if
    [timeout] is not finite and positive or if [retries] is negative. *)

val test :
  ?__POS__:pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  (unit -> unit) ->
  test
(** [test name fn] is the test [name] with body [fn]. The body passes by
    returning and fails by raising.

    While an attempt runs, the global [Random] state is seeded from the test's
    path, and it is restored afterwards. *)

val group :
  ?__POS__:pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  test list ->
  test
(** [group name tests] is the group [name] over [tests]. A group has no hooks.
    {!bracket}, {!scoped} and {!fixture} scope resources. *)

val slow :
  ?__POS__:pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  (unit -> unit) ->
  test
(** [slow name fn] is {!val:test} with the tag ["slow"] added to [tags]. The
    report lists under [slow tests] every test without the tag that ran for at
    least [--slow-threshold] seconds. *)

val cases :
  ?__POS__:pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  name:('a -> string) ->
  string ->
  'a list ->
  ('a -> unit) ->
  test
(** [cases ~name base inputs fn] is the group [base] whose children, in the
    order of [inputs], run [fn input] under the name [name input]. Every child
    reports the [cases] call as its declaration site. The optional arguments sit
    on the group, so [timeout] and [retries] apply to each child.

    [inputs] is evaluated at declaration, and [name] is applied to each input
    then, outside any test. *)

(** {2:resources Resources} *)

val bracket :
  ?__POS__:pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  setup:(unit -> 'r) ->
  teardown:('r -> unit) ->
  string ->
  ('r -> unit) ->
  test
(** [bracket ~setup ~teardown name fn] is the test [name] whose body [fn]
    receives the resource that [setup ()] returns and [teardown] releases.
    [teardown] runs iff [setup] returned, and then on every outcome of [fn],
    skip and timeout included. A fatal exception ([Sys.Break], [Out_of_memory])
    skips it and ends the run.

    It is {!scoped} over the scope that runs the three in order. What [setup]
    raises is a [[setup]] failure. What [teardown] raises is a [[teardown]]
    failure reported beside the body's. *)

val scoped :
  (('r -> unit) -> unit) ->
  ?__POS__:pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  ('r -> unit) ->
  test
(** [scoped scope name fn] is the test [name] whose body [fn] receives the
    resource that [scope] provides. A scope is a function that acquires a
    resource, hands it to a callback and reclaims it when the callback returns.
    The runner calls [scope] once, with a callback that runs [fn].

    A failure raised by [fn], a {!skip} or a timeout included, is recorded and
    then raised again through [scope]. A scope that swallows it cannot pass the
    test, and a scope that releases on the exception path does so.

    {b Warning.} The runner never holds the resource and guarantees no release.

    [scope] must call its callback once. A scope that returns without calling it
    fails the test. A second call runs nothing and fails the test.

    What [scope] raises before the callback is a [[setup]] failure. A {!skip}
    there skips the test. What [scope] raises after the callback returned is a
    [[teardown]] failure, reported beside the body's.

    [timeout] covers the whole call of [scope]. It is armed again when the body
    leaves the callback. *)

val fixture : ?teardown:('a -> unit) -> (unit -> 'a) -> unit -> 'a
(** [fixture ?teardown create] is an accessor for a resource the run shares.
    Making the accessor runs nothing. The first call inside a test acquires with
    [create ()], and later calls return the same value. A fixture no selected
    test calls is never acquired.

    The outcome of the acquisition is kept for the run. If [create] raises, the
    calling test fails, every later call raises the same exception with its
    first backtrace, and nothing is released. If [create] skips, the skip is
    kept too, and every test that calls the accessor skips with the same reason.

    The runner releases the acquired fixtures after the last test, in reverse
    order of acquisition. It does so on every path where it regains control,
    [-x] included. A [teardown] that raises fails the run with a [[release]]
    failure, and the other releases still run. Without [teardown] nothing is
    released.

    {b Warning.} No timeout covers a release.

    Raises [Invalid_argument] if the accessor is called while no test is
    running. *)

(** {2:annotations Annotations}

    An annotation wraps a declared test or group, which keeps its declaration
    site. On a group it reaches every test under it. The annotation nearest a
    test wins. *)

val focus : test -> test
(** [focus t] is [t] focused. When a suite holds a focused test, only focused
    tests run, within the rest of the selection.

    Under [CI] (see the {{!section-command_line}environment}) a suite that holds
    a [focus], selected or not, is refused. *)

val xfail : ?reason:string -> test -> test
(** [xfail ?reason t] is [t] expected to fail. The test still runs. A failure
    counts as an expected failure in the summary and leaves the exit code, [-x]
    and the last failed tests alone. A test that passes fails. A skip stays a
    skip. In mutation testing it reaches no mutant and kills none. Prefer
    {!skip} when the body must not run. *)

(** {1:assertions Assertions}

    A verb returns when its claim holds. Otherwise it raises one failure, which
    ends the body. Where a verb takes two values, the expected one comes first.
    A verb prints its values only when it fails.

    Every verb takes [?__POS__], the failure's location (see {!type:pos}). Every
    verb but {!fail} and {!failf} takes [?msg], text the report prints above the
    values. A failure keeps at most 64 KiB of a printed value.

    Outside a run a failure or a skip is an uncaught exception. *)

(** {2:comparisons Equality and order}

    {!equal} and {!not_equal} read the witness's equality and never its order.
    The four ordering verbs read its order and never its equality.

    An ordering verb on a witness without order raises [Invalid_argument],
    whether or not its claim holds. The {{!section-witnesses}witnesses} section
    says which witnesses carry an order. *)

val equal : ?__POS__:pos -> ?msg:string -> 'a testable -> 'a -> 'a -> unit
(** [equal t expected actual] asserts that [expected] and [actual] are equal
    under [t]. *)

val not_equal : ?__POS__:pos -> ?msg:string -> 'a testable -> 'a -> 'a -> unit
(** [not_equal t a b] asserts that [a] and [b] are not equal under [t]. *)

val less : ?__POS__:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** [less t ~than v] asserts that [v] is strictly below [than] under [t]'s
    order. *)

val at_most :
  ?__POS__:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** [at_most t ~than v] asserts that [v] is below or equal to [than] under [t]'s
    order. *)

val greater :
  ?__POS__:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** [greater t ~than v] asserts that [v] is strictly above [than] under [t]'s
    order. *)

val at_least :
  ?__POS__:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** [at_least t ~than v] asserts that [v] is above or equal to [than] under
    [t]'s order. *)

(** {2:predicates Booleans and predicates} *)

val is_true : ?__POS__:pos -> ?msg:string -> bool -> unit
(** [is_true b] asserts [b]. The failure can only show [false] where [true] was
    expected, so prefer a verb that prints the values it compares. *)

val is_false : ?__POS__:pos -> ?msg:string -> bool -> unit
(** [is_false b] asserts [not b]. *)

val satisfies :
  ?__POS__:pos ->
  ?msg:string ->
  ?claim:string ->
  'a testable ->
  ('a -> bool) ->
  'a ->
  unit
(** [satisfies ?claim t pred v] asserts [pred v], for a claim that is not an
    order, such as a parity. [pred] must be total. The failure prints [claim],
    by default ["value satisfying the predicate"], and [v]. *)

(** {2:strings Strings and lists}

    The string verbs compare bytes. *)

val starts_with : ?__POS__:pos -> ?msg:string -> affix:string -> string -> unit
(** [starts_with ~affix s] asserts that [s] begins with [affix]. *)

val ends_with : ?__POS__:pos -> ?msg:string -> affix:string -> string -> unit
(** [ends_with ~affix s] asserts that [s] ends with [affix]. *)

val contains : ?__POS__:pos -> ?msg:string -> sub:string -> string -> unit
(** [contains ~sub s] asserts that [sub] occurs in [s]. The empty string occurs
    in every string. *)

val not_contains : ?__POS__:pos -> ?msg:string -> sub:string -> string -> unit
(** [not_contains ~sub s] asserts that [sub] does not occur in [s], so it fails
    on every [s] when [sub] is empty. *)

val in_order : ?__POS__:pos -> ?msg:string -> subs:string list -> string -> unit
(** [in_order ~subs s] asserts that the elements of [subs] occur in [s] in
    order, each match starting at or after the end of the previous one. Matches
    share no byte, so [["aa"; "aa"]] needs four [a]s. An empty element matches
    without advancing.

    Raises [Invalid_argument] if [subs] is empty. *)

val mem : ?__POS__:pos -> ?msg:string -> 'a testable -> 'a -> 'a list -> unit
(** [mem t x xs] asserts that an element of [xs] equals [x] under [t]. [t]'s
    equality receives [x] as the expected value. *)

(** {2:shapes Options and results}

    When a value is on the side a verb does not want, the failure prints its
    payload with [pp], or as [<abstract>] without [pp]. *)

val is_none : ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> 'a option -> unit
(** [is_none o] asserts that [o] is [None]. *)

val is_some : ?__POS__:pos -> ?msg:string -> 'a option -> unit
(** [is_some o] asserts that [o] is [Some _]. *)

val is_ok :
  ?__POS__:pos -> ?msg:string -> ?pp:'e printer -> ('a, 'e) result -> unit
(** [is_ok r] asserts that [r] is [Ok _]. *)

val is_error :
  ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> ('a, 'e) result -> unit
(** [is_error r] asserts that [r] is [Error _]. *)

val require_some : ?__POS__:pos -> ?msg:string -> 'a option -> 'a
(** [require_some o] is [v] when [o] is [Some v], and fails the test otherwise.
*)

val require_ok :
  ?__POS__:pos -> ?msg:string -> ?pp:'e printer -> ('a, 'e) result -> 'a
(** [require_ok r] is [v] when [r] is [Ok v], and fails the test otherwise. *)

val require_error :
  ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> ('a, 'e) result -> 'e
(** [require_error r] is [e] when [r] is [Error e], and fails the test
    otherwise. *)

val require_match :
  ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> ('a -> 'b option) -> 'a -> 'b
(** [require_match extract v] is [b] when [extract v] is [Some b], and fails the
    test otherwise. An exception from [extract] escapes the verb. *)

(** {2:exceptions Exceptions} *)

val raises : ?__POS__:pos -> ?msg:string -> exn -> (unit -> 'a) -> unit
(** [raises e f] asserts that [f ()] raises an exception structurally equal to
    [e].

    An exception that carries what structural equality cannot compare, such as a
    function, needs {!raises_match}. A verb's failure, a {!skip}, a timeout, a
    call to [exit] or an {!assume} that [f] raises is not the exception [raises]
    waits for. It passes through. *)

val raises_match :
  ?__POS__:pos -> ?msg:string -> (exn -> bool) -> (unit -> 'a) -> unit
(** [raises_match pred f] asserts that [f ()] raises an exception that satisfies
    [pred], which must be total. What passes through {!raises} passes through
    it, whatever [pred] says. *)

(** Exception predicates for {!raises_match}.

    A predicate checks the constructor, and the message when [substring] is
    given. An empty [substring] matches every message. *)
module Exn : sig
  val invalid_arg : ?substring:string -> exn -> bool
  (** [invalid_arg ?substring e] is [true] iff [e] is [Invalid_argument m] and
      [m] contains [substring], when given. *)

  val failure : ?substring:string -> exn -> bool
  (** [failure ?substring e] is {!invalid_arg} for [Failure m]. *)

  val sys_error : ?substring:string -> exn -> bool
  (** [sys_error ?substring e] is {!invalid_arg} for [Sys_error m]. *)
end

(** {2:laws Laws}

    A law is a verb that asserts a textbook equation about the functions it is
    given. Its last argument is the value the equation is asserted on, a tuple
    when there are several, as {!Gen.triple} draws one. Partially applied, it is
    the law of a {!prop} and the function of a {!cases}; applied to a value, it
    is a line of a {!val:test}.

    A law returns when its equations hold. Otherwise it raises one failure that
    names the law, states the equation that failed and prints every term
    computed for it, the two sides last and diffed. A term whose function raises
    or fails, {!require_some} included, fails the law.

    A law never skips a case. It meets a premise by construction or asserts it.
    [equivalence], [order], [partial_order], [idempotent], [involutive],
    [monotone] and [ignores] could hold on every case for want of one that tests
    them. Wherever {!cover} registers a demand, in the law of a {!prop} and in a
    {!stateful} test, each also demands such a case, as {!cover} does, under a
    label that starts with the law's name and ends with its [msg] when given.
    Two calls of one law share their demands unless their [msg]s differ, and a
    case of either meets them. Elsewhere, as in a {!val:test}, in a {!cases} row
    or on another domain, a law demands nothing.

    A law takes no tolerance; its witness carries one. A witness with a
    tolerance is no equivalence, and float addition is not associative. *)
module Law : sig
  val equivalence :
    ?__POS__:pos ->
    ?msg:string ->
    ?respell:('a -> 'a) ->
    'a testable ->
    'a * 'a ->
    unit
  (** [equivalence ?respell w (a, b)] asserts that [w]'s equality is reflexive
      and symmetric on [a], [b]. [respell] ([r]) returns an equal value built
      differently, [1.2.0] for [1.2]; it adds [a = r a] both ways,
      [a = r (r a)], and [r a = b] iff [a = b]. Demands a case where [a] and [b]
      are unequal and, given [r], one where [r a] differs structurally from [a].
  *)

  val order :
    ?__POS__:pos ->
    ?msg:string ->
    ?respell:('a -> 'a) ->
    'a testable ->
    'a * 'a * 'a ->
    unit
  (** [order ?respell w (a, b, c)] asserts that [w]'s order [cmp] is total and
      agrees with its equality: [cmp x x = 0]; [cmp x y] and [cmp y x] have
      opposite signs or are both [0]; [cmp x y <= 0] and [cmp y z <= 0] imply
      [cmp x z <= 0], over every ordering of the three; [cmp x y = 0] iff [x]
      and [y] are equal. [respell] ([r]), as in {!equivalence}, adds
      [cmp a (r a) = 0], the agreement over [a] and [r a], and [cmp (r a) b]
      with the sign of [cmp a b]. Demands as {!equivalence}. Raises
      [Invalid_argument] if [w] has no order. *)

  val partial_order :
    ?__POS__:pos ->
    ?msg:string ->
    'a testable ->
    ('a -> 'a -> bool) ->
    'a * 'a * 'a ->
    unit
  (** [partial_order w leq (a, b, c)] asserts that [leq] is reflexive on each
      value, antisymmetric under [w]'s equality ([leq x y] and [leq y x] imply
      [x = y]), and transitive over every ordering of the three. Demands a
      strict chain: an ordering [x], [y], [z] of the three with [leq x y] and
      [leq y z], no two of them equal. Independent draws rarely give one; draw
      [b] and [c] from [a]. A preorder is a partial order under the witness
      whose equality is [leq x y && leq y x]. *)

  val associative :
    ?__POS__:pos ->
    ?msg:string ->
    'a testable ->
    ('a -> 'a -> 'a) ->
    'a * 'a * 'a ->
    unit
  (** [associative w op (a, b, c)] asserts [op (op a b) c = op a (op b c)]. *)

  val commutative :
    ?__POS__:pos ->
    ?msg:string ->
    'a testable ->
    ('a -> 'a -> 'a) ->
    'a * 'a ->
    unit
  (** [commutative w op (a, b)] asserts [op a b = op b a]. *)

  val neutral :
    ?__POS__:pos ->
    ?msg:string ->
    'a testable ->
    ('a -> 'a -> 'a) ->
    'a ->
    'a ->
    unit
  (** [neutral w op e x] asserts [op e x = x] and [op x e = x]. *)

  val absorbing :
    ?__POS__:pos ->
    ?msg:string ->
    'a testable ->
    ('a -> 'a -> 'a) ->
    'a ->
    'a ->
    unit
  (** [absorbing w op z x] asserts [op z x = z] and [op x z = z]. *)

  val invertible :
    ?__POS__:pos ->
    ?msg:string ->
    'a testable ->
    ('a -> 'a -> 'a) ->
    'a ->
    ('a -> 'a) ->
    'a ->
    unit
  (** [invertible w op e inv x] asserts [op x (inv x) = e] and
      [op (inv x) x = e]. [e] is the neutral element of [op], which {!neutral}
      asserts. *)

  val distributive :
    ?__POS__:pos ->
    ?msg:string ->
    'a testable ->
    ('a -> 'a -> 'a) ->
    over:('a -> 'a -> 'a) ->
    'a * 'a * 'a ->
    unit
  (** [distributive w op ~over (a, b, c)] asserts
      [op a (over b c) = over (op a b) (op a c)] and
      [op (over a b) c = over (op a c) (op b c)]. *)

  val idempotent :
    ?__POS__:pos -> ?msg:string -> 'a testable -> ('a -> 'a) -> 'a -> unit
  (** [idempotent w f x] asserts [f (f x) = f x]. Demands a case where [f x]
      differs structurally from [x]. *)

  val involutive :
    ?__POS__:pos -> ?msg:string -> 'a testable -> ('a -> 'a) -> 'a -> unit
  (** [involutive w f x] asserts [f (f x) = x]. Demands a case where [f x]
      differs structurally from [x]. *)

  val commutes :
    ?__POS__:pos ->
    ?msg:string ->
    'a testable ->
    ('a -> 'a) ->
    ('a -> 'a) ->
    'a ->
    unit
  (** [commutes w f g x] asserts [f (g x) = g (f x)]. *)

  val homomorphic :
    ?__POS__:pos ->
    ?msg:string ->
    'a testable ->
    'b testable ->
    ('a -> 'b) ->
    ('a -> 'a -> 'a) ->
    ('b -> 'b -> 'b) ->
    'a * 'a ->
    unit
  (** [homomorphic wa wb f op op' (a, b)] asserts [f (op a b) = op' (f a) (f b)]
      under [wb]. *)

  val round_trip :
    ?__POS__:pos ->
    ?msg:string ->
    'a testable ->
    'b testable ->
    ('a -> 'b) ->
    ('b -> 'a) ->
    'a ->
    unit
  (** [round_trip wa wb f g x] asserts [g (f x) = x] under [wa]. A [g] that
      returns an option or a result, composed with {!require_some} or
      {!require_ok}, fails the law on [None] or [Error _]. From text to a value
      and back, [round_trip string w decode encode s] asserts that [s] is
      canonical. *)

  val monotone :
    ?__POS__:pos ->
    ?msg:string ->
    'a testable ->
    'b testable ->
    ('a -> 'b) ->
    'a * 'a ->
    unit
  (** [monotone wa wb f (a, b)] sorts the pair by [wa] and asserts that [a <= b]
      implies [f a <= f b] under the witnesses' orders. When [wa]'s order
      returns [0] on [a] and [b], each is below the other, and [wb]'s order must
      return [0] on [f a] and [f b]. Demands a strict pair, [a] below [b].
      Raises [Invalid_argument] if [wa] or [wb] has no order. *)

  val ignores :
    ?__POS__:pos ->
    ?msg:string ->
    'a testable ->
    'b testable ->
    ('a -> 'b) ->
    ('a -> 'a) ->
    'a ->
    unit
  (** [ignores wa wb f g x] asserts [f (g x) = f x] under [wb]. Demands a case
      where [g x] differs structurally from [x]. Hash consistency is
      [ignores w int hash r], [r] a respelling as {!equivalence} takes. *)

  val preserves :
    ?__POS__:pos ->
    ?msg:string ->
    'a testable ->
    ('a -> 'a) ->
    ('a -> bool) ->
    'a ->
    unit
  (** [preserves w f inv x] asserts [inv x], then [inv (f x)]. A false [inv x]
      fails the law. The generator of a {!prop} must draw only values that
      satisfy [inv]. *)
end

(** {2:ending Failing and skipping} *)

val fail : ?__POS__:pos -> string -> 'a
(** [fail msg] fails the test with [msg]. *)

val failf : ?__POS__:pos -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [failf fmt ...] is {!fail} with the message [fmt] formats. *)

val skip : ?reason:string -> unit -> 'a
(** [skip ?reason ()] skips the running test. A skip is not a failure, and a
    selection whose every test skipped exits [0]. *)

(** {1:witnesses Witnesses}

    The values of this section are those of {!Testable} under the same names,
    typed as {!type:testable}. The constructors stay in {!Testable}.

    A report computes its diff from the two printed values, so a printer is all
    a witness needs to be diffed.

    The witnesses of base types carry the order of their type's module,
    [Int.compare] for {!int}. {!Testable.structural} carries [Stdlib.compare],
    and {!Testable.contramap} carries the order of its argument. {!pass},
    {!Testable.make}, {!Testable.of_equal} and the container witnesses carry
    none until {!Testable.with_compare} gives one. *)

val unit : unit testable
(** [unit] is the witness for [unit]. *)

val bool : bool testable
(** [bool] is the witness for [bool]. *)

val char : char testable
(** [char] is the witness for [char], printed with [%C]. *)

val string : string testable
(** [string] is the witness for [string], printed with [%S]: quoted, escaped, on
    one line. *)

val text : string testable
(** [text] is {!string} printed verbatim: newlines kept, no quotes, no escapes.
    Multi-line values take [text]. Single-line values take {!string}, whose
    quotes tell [""], [" "] and ["\t"] apart. *)

val bytes : bytes testable
(** [bytes] is the witness for [bytes], printed as a string literal. *)

val int : int testable
(** [int] is the witness for [int]. *)

val int32 : int32 testable
(** [int32] is the witness for [int32]. *)

val int64 : int64 testable
(** [int64] is the witness for [int64]. *)

val nativeint : nativeint testable
(** [nativeint] is the witness for [nativeint]. *)

(** {2:floats Floats}

    The three witnesses order with [Float.compare], whatever the tolerance,
    except that {!float_exact} puts [-0.] below [0.], as its equality tells them
    apart. Under [float 0.5], [1.0] is below [1.2]. NaN is below every float.

    An infinity is equal only to an infinity of the same sign. Under {!float}
    and {!float_rel}, NaN is equal to nothing and [0.] equals [-0.]. Under
    {!float_exact}, every NaN equals every NaN and [0.] differs from [-0.]. *)

val float_exact : float testable
(** [float_exact] compares floats bit for bit. It prints the shortest decimal
    that round-trips to the value. *)

val float : float -> float testable
(** [float eps] compares with absolute tolerance [eps]. [a] and [b] are equal
    when [a = b] or [|a -. b| <= eps]. It prints with [%g]. Raises
    [Invalid_argument] if [eps] is not strictly positive, NaN included. *)

val float_rel : rel:float -> abs:float -> float testable
(** [float_rel ~rel ~abs] compares with relative tolerance [rel] and absolute
    tolerance [abs]. [a] and [b] are equal when [a = b], when [|a -. b| <= abs],
    or when [|a -. b| <= rel *. Float.max (abs_float a) (abs_float b)]. One zero
    bound switches that component off. It prints with [%g]. Raises
    [Invalid_argument] if a bound is negative or NaN, or if both are zero. *)

(** {2:containers Containers}

    A container witness compares and prints its components under the witnesses
    it is given. {!pass} in the place of a component ignores that component. *)

val option : 'a testable -> 'a option testable
(** [option t] is the witness for ['a option] with elements under [t]. *)

val result : 'a testable -> 'e testable -> ('a, 'e) result testable
(** [result ok error] is the witness for [('a, 'e) result], [ok] for the [Ok]
    side and [error] for the [Error] side. *)

val either : 'a testable -> 'b testable -> ('a, 'b) Either.t testable
(** [either left right] is {!result} for [Either.t], [left] for the [Left] side
    and [right] for the [Right] side. *)

val list : 'a testable -> 'a list testable
(** [list t] is the witness for ['a list] with elements under [t]. *)

val array : 'a testable -> 'a array testable
(** [array t] is {!list} for ['a array]. *)

val slist : 'a testable -> ('a -> 'a -> int) -> 'a list testable
(** [slist t cmp] is [Testable.contramap (List.sort cmp) (list t)]. *)

val pair : 'a testable -> 'b testable -> ('a * 'b) testable
(** [pair a b] is the witness for pairs, [a] for the first component and [b] for
    the second. *)

val triple :
  'a testable -> 'b testable -> 'c testable -> ('a * 'b * 'c) testable
(** [triple a b c] is {!pair} for triples. *)

val quad :
  'a testable ->
  'b testable ->
  'c testable ->
  'd testable ->
  ('a * 'b * 'c * 'd) testable
(** [quad a b c d] is {!pair} for quadruples. *)

val pass : 'a testable
(** [pass] is the witness under which all values are equal. It prints every
    value as [<pass>] and carries no order. *)

(** {1:properties Properties}

    A property is a test whose body, the law, must hold for every value a
    {{!Gen}generator} draws. {!prop} runs the law on each generated case and,
    when one fails, shrinks its input to a counterexample.

    {b Seeds.} Every generated value derives from the run's root seed, the
    test's path and the index of the case. [--seed] and its mirror
    [WINDTRAP_SEED] set it.

    The derivation of the case seeds and the stream of bits are frozen under the
    [s1] prefix. What a generator draws from the stream is not, so a seed
    replays every value within one version of windtrap, on another machine,
    under another OCaml version and whatever else the suite holds.

    A report whose failed tests include a generated case of a property ends on
    one line, [replay: <command> --seed <token>] and the run's selection, which
    runs the selected tests again, each failed test on the values it drew. No
    failure block carries its own.

    A law must be deterministic, because the search for a counterexample runs it
    again on candidate inputs.

    Shrinking runs the law at most [10_000] times, accepted and rejected
    candidates alike, and a search stopped there reports that the counterexample
    may not be minimal. It also stops when a function of the generator raises on
    a candidate. A function of the generator that raises while a case is drawn
    fails the case. *)

(**/**)

(* Taken before [Gen] is narrowed below, for [Private]. *)
module Gen_engine = Gen.Engine

(**/**)

module Gen : sig
  (** Random generators with integrated shrinking and printing.

      {b Shrinking.} Every generator shrinks, and a shrink candidate satisfies
      the constraints of its generator.

      {b Printing.} A counterexample prints with its generator's printer. The
      generators of base types print OCaml literals. A container or a choice
      prints each component by that component's rule. {!constant} and {!of_list}
      have no printer. {!map}, {!bind} and the binding operators have none
      either.

      A value that {!map} or {!bind} computed prints as its pre-image. The
      pre-image has the same shape, with each such value replaced by what it was
      computed from, down to the nearest generator that prints.

      {b Validation.} A constructor never raises, so a malformed generator fails
      its own test and never the initialization of the module. The argument, as
      in [one_of []] or [int_range 3 1], raises [Invalid_argument] when the
      generator first samples.

      {b Purity.} The functions given to {!map}, {!bind} and {!such_that} must
      be pure, because the search for a counterexample runs them again. *)

  type 'a t = 'a Gen.t
  (** The type for generators of ['a] values. The equation is not part of the
      contract. *)

  (** {1:numeric Numbers}

      {!int}, {!int_range}, {!int32}, {!int64} and {!nativeint} draw a corner
      case with probability 0.1 and draw uniformly otherwise. The corners of a
      range are its bounds, the point closest to [0] and that point's neighbours
      inside the range. The corners of a whole type are [0], [1], [-1] and the
      type's two extremes. *)

  val int : int t
  (** [int] generates an integer over the whole [int] range. It shrinks toward
      [0]. *)

  val nat : int t
  (** [nat] generates a natural number below [10_000], small values more often:
      50% below [10], 25% below [100], 20% below [1_000], 5% below [10_000]. It
      shrinks toward [0]. *)

  val small_int : int t
  (** [small_int] generates an integer of either sign whose magnitude follows
      {!nat}, so a value in \[[-9_999];[9_999]\]. It shrinks toward [0]. *)

  val int_range : int -> int -> int t
  (** [int_range low high] generates an integer in \[[low];[high]\]. It shrinks
      toward the point of the range closest to [0]. Sampling raises
      [Invalid_argument] if [high < low]. *)

  val int32 : int32 t
  (** [int32] is {!int} for the whole [int32] range. It shrinks toward [0l]. *)

  val int64 : int64 t
  (** [int64] is {!int} for the whole [int64] range. It shrinks toward [0L]. *)

  val nativeint : nativeint t
  (** [nativeint] is {!int} for the whole [nativeint] range. It shrinks toward
      [0n]. *)

  val float : float t
  (** [float] generates a finite float from a uniform IEEE 754 bit pattern, so
      magnitudes spread over the whole exponent range, subnormals included. It
      shrinks toward [0.]. *)

  val float_range : float -> float -> float t
  (** [float_range low high] generates a float in \[[low];[high]\], uniformly.
      It shrinks toward the point of the range closest to [0.]. Sampling raises
      [Invalid_argument] if [high < low], if a bound is not finite, or if
      [high -. low] overflows. *)

  (** {1:base Unit, booleans, characters and strings} *)

  val unit : unit t
  (** [unit] generates [()], which does not shrink and prints as [()]. *)

  val bool : bool t
  (** [bool] generates [true] or [false] with equal probability. [true] shrinks
      to [false]. *)

  val char : char t
  (** [char] generates a byte uniformly: each of the 256 characters, ['\x00']
      and the bytes above 127 included. It shrinks toward ['a']. *)

  val char_range : char -> char -> char t
  (** [char_range low high] generates a character in \[[low];[high]\], in byte
      order, uniformly. It shrinks toward the character of the range closest to
      ['a']: [char_range 'A' 'Z'] toward ['Z'], [char_range '0' '9'] toward
      ['9']. Sampling raises [Invalid_argument] if [high < low]. *)

  val string : string t
  (** [string] is [string_of char]. *)

  val string_of : ?size:int t -> char t -> string t
  (** [string_of ?size char] generates a string whose length follows [size] and
      whose characters follow [char]. [size] defaults to the length of {!list}.
      It shrinks as {!list} does. It always prints, as a quoted string, even
      when [char] has no printer. Sampling raises [Invalid_argument] if [size]
      generates a negative length. *)

  val bytes : bytes t
  (** [bytes] is [bytes_of char]. *)

  val bytes_of : ?size:int t -> char t -> bytes t
  (** [bytes_of ?size char] is {!string_of} for [bytes]. *)

  (** {1:containers Containers} *)

  val list : ?size:int t -> 'a t -> 'a list t
  (** [list ?size gen] generates a list of [gen] values whose length follows
      [size]. Without [size], the length is below [64] and about 5 on average.

      A list shrinks to shorter lists and by shrinking its elements, in an order
      that is not part of the contract. With an explicit [size] the length
      shrinks as [size] does, so [~size:(int_range 2 5)] holds for every
      candidate.

      Sampling raises [Invalid_argument] if [size] generates a negative length.
  *)

  val array : ?size:int t -> 'a t -> 'a array t
  (** [array ?size gen] is {!list} for arrays. *)

  val option : 'a t -> 'a option t
  (** [option gen] generates [None] with probability 0.15 and [Some v]
      otherwise, with [v] drawn from [gen]. [Some v] shrinks first to [None],
      then as [v] does. *)

  val result : 'a t -> 'e t -> ('a, 'e) result t
  (** [result ok error] generates [Ok] of an [ok] value with probability 0.75
      and [Error] of an [error] value otherwise. A payload shrinks with its
      generator, and no candidate changes constructor. *)

  val either : 'a t -> 'b t -> ('a, 'b) Either.t t
  (** [either left right] generates [Left] of a [left] value or [Right] of a
      [right] value with equal probability. It shrinks as {!result} does. *)

  val pair : 'a t -> 'b t -> ('a * 'b) t
  (** [pair a b] generates both components. It shrinks the first component, then
      the second. *)

  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  (** [triple a b c] is {!pair} for three components, shrunk from the left. *)

  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  (** [quad a b c d] is {!pair} for four components, shrunk from the left. *)

  (** {1:choice Constants, choices and filters} *)

  val constant : 'a -> 'a t
  (** [constant v] generates [v], which does not shrink. *)

  val of_list : 'a list -> 'a t
  (** [of_list values] generates an element of [values], each with equal
      probability. It shrinks toward the head of [values]. Sampling raises
      [Invalid_argument] if [values] is empty. *)

  val one_of : 'a t list -> 'a t
  (** [one_of gens] generates with one generator of [gens], each with equal
      probability. The choice shrinks toward the head of [gens], so the simplest
      generator goes first. The value then shrinks with its own generator.
      Sampling raises [Invalid_argument] if [gens] is empty. *)

  val frequency : (int * 'a t) list -> 'a t
  (** [frequency weighted] generates with one generator of [weighted], each with
      a probability proportional to its weight. The value shrinks with the
      generator that drew it, and every candidate is a value of one of the
      generators. Sampling raises [Invalid_argument] if [weighted] is empty, if
      a weight is negative, or if the weights sum to less than [1]. *)

  val such_that : ('a -> bool) -> 'a t -> 'a t
  (** [such_that p gen] generates [gen] values that satisfy [p], in at most 100
      draws. When none of them satisfies [p] the case is discarded, as
      {!Windtrap.reject} discards one. Shrink candidates that fail [p] are
      dropped. It keeps [gen]'s printer.

      Prefer a generator that satisfies a structural constraint by construction.
  *)

  (** {1:composition Composition} *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f gen] generates [f v] for [v] from [gen], and shrinks as [gen] does.
  *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (** [bind gen f] generates [v] with [gen], then a value with [f v]. It shrinks
      [v] first, and generates again with [f] for each candidate. It then
      shrinks the inner value. The value prints as the inner value when [f v]
      prints, and as the pre-image [v -> inner] otherwise. *)

  val with_pp : (Format.formatter -> 'a -> unit) -> 'a t -> 'a t
  (** [with_pp pp gen] is [gen] printing with [pp], which wins over a pre-image
      and over a derived printer. *)

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  (** [let+ x = gen in e] is [map (fun x -> e) gen]. *)

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  (** [gen1 and+ gen2] is [pair gen1 gen2]. *)

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  (** [let* x = gen in e] is [bind gen (fun x -> e)]. *)
end

val prop :
  ?__POS__:pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?count:int ->
  ?max_discard:int ->
  ?examples:'a list ->
  string ->
  'a Gen.t ->
  ('a -> unit) ->
  test
(** [prop name gen law] is the property [name], whose [law] must hold for every
    value of [gen].
    - [count] is the number of generated cases that must pass. Defaults to
      [--prop-count], then to [100]. With [0] the examples run alone.
    - [max_discard] is the number of discarded cases (see {!assume}) past which
      the property gives up and fails. Defaults to twice the effective [count].
      No flag sets it.
    - [examples] are inputs that the law runs on before any generated case. They
      are not shrunk and do not depend on the seed.
    - [timeout] covers generation and shrinking together. When it expires before
      a case failed, the test fails as timed out. When it expires during
      shrinking, the report gives the counterexample found so far and says it
      may not be minimal.

    A property carries the tag ["prop"]. It takes no [retries] argument, but it
    inherits the [retries] of an enclosing group, and every retry replays the
    same cases from the seed. A {!skip} raised by [law] skips the test.

    Raises [Invalid_argument], inside the running test, if [count] or
    [max_discard] is negative. *)

(** {2:discarding Discarding and labelling cases}

    {!assume}, {!reject}, {!collect}, {!classify} and {!cover} work in the law
    of a {!prop}. {!assume} and {!reject} work too in a function given to a
    generator, where a discard drops the case or the shrink candidate. In a
    {!stateful} test the labels work in a command's functions, a [~pre], an
    invariant and a release, where a label counts once per case, and a discard
    fails the case (see {{!section-stateful_tests}stateful tests}). *)

val assume : bool -> unit
(** [assume cond] discards the current case unless [cond] holds. A discarded
    case counts against [max_discard] and another is generated. Outside a
    property, [assume false] fails the test. *)

val reject : unit -> 'a
(** [reject ()] discards the current case. See {!assume}. *)

val collect : string -> unit
(** [collect label] marks the current case with [label]. The report gives the
    distribution of the labels over the passing cases.

    A case counts a label once. Discarded cases, failing cases and shrinking
    count nothing. Raises [Invalid_argument] if no property is running. *)

val classify : string -> bool -> unit
(** [classify label cond] is [collect label] if [cond] and [()] otherwise.
    Raises as {!collect} does, whatever [cond]. *)

val cover : string -> bool -> unit
(** [cover label cond] is [classify label cond] with a demand. The property
    fails unless a passing case marked [label]. The demand is on presence, never
    on a proportion.

    The demand registers when [cover] is called, whatever [cond], so a [cover]
    the law does not always reach may register nothing. It is judged when the
    property has run all its cases. Raises as {!collect} does. *)

(** {1:stateful_tests Stateful tests}

    A stateful test checks an API against a reference. The API is described
    once, as a list of {{!type:command}commands}, each pairing the reference's
    function with the system's under a {{!type:fn}signature}. {!stateful} draws
    programs of calls, runs each call on both sides and fails when the system's
    outcome is not the reference's. A model written for the test is a reference,
    and so are another implementation, an older version and the system itself.

    {[
    module R = Set.Make (Int)

    let set = abstract "s" ~invariant:(fun _ s -> is_true (Fast_set.balanced s))
    let elt = Gen.int_range 0 15

    let commands =
      [
        command "empty"
          (Gen.unit @-> makes set)
          (fun () -> R.empty)
          (fun () -> Fast_set.empty);
        command "add" (elt @-> set ^-> makes set) R.add Fast_set.add;
        command "mem" (elt @-> set ^-> returns bool) R.mem Fast_set.mem;
        command "elements"
          (set ^-> returns (list int))
          R.elements Fast_set.elements;
      ]

    let () = exit (run "fast_set" [ stateful "behaves like Set" commands ])
    ]}

    {b Drawing.} A program is drawn without running anything, and makes at most
    [steps] calls. A command listed twice is drawn twice as often. A command is
    drawn only when every abstract type it takes has a value that an earlier
    call of the program makes. Each case draws from a subset of the commands
    (swarm testing). The subset, the drawing of an abstract argument and the
    order in which shrinking tries candidates are not part of the contract, so
    the calls that a seed draws and the counterexample that shrinking reaches
    can change between versions of windtrap, as every generator's draws can (see
    {{!section-properties}Seeds}).

    {b Values.} Only a call whose signature ends in {!makes} makes a value of an
    {{!type:abstract}abstract type}, and values are never generated. A value
    holds the reference's side and the system's. It is named by its type's
    prefix and a count per prefix, in the order the calls ran: [s1], [s2]. Every
    run starts with no value, so a system comes from a call. A module that keeps
    global state, a counter or a registry, carries it from run to run and shares
    it between the two sides when it is its own reference.

    {b Legality.} A call's abstract arguments resolve, and its [~pre] is asked,
    when the program runs, of the reference as the run left it. A call whose
    arguments do not resolve or whose [~pre] fails is skipped on both sides and
    is absent from the report. No call the reference forbids is made, and a
    program with many preconditions makes fewer than [steps] calls.

    {b A call} runs the system, then the reference, which judges the system's
    outcome: under {!returns} and {!makes} the two outcomes compare, and under
    {!chooses} the reference accepts one. Then the invariant of every abstract
    type runs on each of its values. The first failure ends the program. On
    several domains the calls after the prefix run and are judged differently
    (see {!stateful}).

    {b Outcomes.} An outcome is a result or a raised exception. Two results
    compare under the signature's witness, which must be reflexive on every
    result the API returns: under [float eps] no NaN equals itself, so a NaN
    result takes {!float_exact}. Two exceptions are equal when their constructor
    names match once the module path is removed, so [Stdlib.Queue.Empty] equals
    [Ring.Empty]. Their payloads print and are not compared, unlike under
    {!raises}. A result never equals an exception. To compare a payload, or tell
    two modules' constructors apart, each side wraps its outcome into a result
    and the signature ends in [returns (result w e)]. Normalising a result is
    the witness's job: [slist int compare] for an order the API leaves open,
    {!Testable.contramap} for part of a result, {!pass} to ignore one.

    {b Never outcomes.} A verb's failure, [Assert_failure], [Match_failure] and
    windtrap's controls are never compared.
    - From a system function, a verb's failure or a broken contract fails the
      case at that call, before the reference runs.
    - From a reference function, it breaks the reference, and so does anything a
      [~pre] raises. A case that broke the reference shrinks among the programs
      that break it, and the search of any other failure rejects a candidate
      that breaks it. From a {!chooses} reference a verb's failure or a broken
      contract is the system's mismatch instead.
    - {!assume} and {!reject} in either function fail the case, since a call's
      legality is its [~pre]'s.
    - A {!skip}, a timeout and an [exit] keep their meaning everywhere.

    {b Labels.} {!collect}, {!classify} and {!cover} in a command's functions, a
    [~pre], an invariant or a release count once per case, in the run that
    executes it. Shrinking counts nothing. On several domains the reference runs
    again for every order the judge tries, and those runs count nothing: the
    labels of the calls after the prefix count along the order the judge
    accepted for the case's first run.

    {b The reference behaves the same from run to run}, since shrinking and
    retries run it again. Drift comes from [Random], a [Hashtbl] whose order a
    result or a [~pre] shows, [Weak] and [Ephemeron]. The system must behave the
    same for a counterexample to shrink.

    {b The report.} A failing case prints the program that its failing run
    executed, as it ran, as a table of calls under a header row. A call reads
    [name a1 … an], and [let v = name a1 … an] when it made the value [v]. A
    drawn argument prints as its generator renders it, a printerless {!Gen.map}
    or {!Gen.bind} as its pre-image, in parentheses when it holds a space or
    starts with [-]. An abstract argument prints as its value's name. When an
    argument's abstract type has [~pp], a [reference before] column shows the
    reference side of such arguments before the call. The failing call is the
    last row. Under the table it is named, as [call 3 of 3: push q1 0], above
    the pair of its outcomes, the reference's as [expected], with its command's
    location. A broken reference reads [reference of call 3 of 3: pop q1] above
    its failure, and an invariant's failure [after call 3 of 3, on s2] above the
    verb's lines.

    On several domains the table adds a [domain] column, the branch of each
    parallel call, and a [result] column, the system's outcome in the failing
    run, and [reference before] prints on the prefix's rows only:

    {v
    counterexample (case 3, shrunk 10 steps): 4 calls, 2 in parallel
       #  domain  call                result
       1          let q1 = create ()
       2  1       push q1 0           ()
       3  2       push q1 0           ()
       4          length q1           1
    which failed with:
      no order of the calls gives these results
      the closest order, 2 then 3, differs at call 4: length q1
      expected  2
      actual    1
    v}

    The closest order is the one whose first difference comes latest. A program
    that shrank to no parallel call prints as on one domain. *)

type ('r, 's) abstract
(** The type for abstract types of an API, whose values only calls make. A value
    holds the reference's side ['r] and the system's side ['s]. *)

val abstract :
  ?pp:'r printer ->
  ?invariant:('r -> 's -> unit) ->
  ?release:('s -> unit) ->
  string ->
  ('r, 's) abstract
(** [abstract prefix] is a new abstract type whose values are named [prefix] and
    a count, as [s1] and [s2] under [abstract "s"]. Two calls make two types.
    - [pp] prints a reference side, in the report's [reference before] column.
    - [invariant r s] runs on the two sides of every value of the type after
      every call made on the test's domain while one reference state exists:
      every call on one domain, the prefix's calls on several. It asserts with
      the verbs, and its failure fails the case.
    - [release s] runs when a program ends, whether it passed, failed or was cut
      short, once per physically distinct system side of the type that the
      program made, newest first. Sides are told apart within the type only, so
      a system side that two types hold is released by each. It must accept
      every state the API can reach, a closed or consumed value included. A
      release that fails over a passing program fails the case, and over a
      failing program it is dropped. A fatal exception skips the releases, as it
      skips a {!bracket}'s teardown.

    Reference sides are never released, so a reference must hold nothing that
    the GC does not reclaim, and a system that holds such a resource cannot be
    its own reference.

    {!stateful} raises [Invalid_argument], inside the test, if [prefix] is not a
    lowercase OCaml identifier, if it ends with a digit, or if two abstract
    types of its commands have it. *)

type ('r, 's, 'p) fn
(** The type for signatures: what a command's arguments are and how its outcome
    is compared. ['r] is the type of the reference's function, ['s] the system's
    and ['p] the precondition's, the reference's arguments to [bool].

    A signature follows the functions' argument order, so
    [Set.add : elt -> t -> t] takes [elt @-> set ^-> makes set] and no wrapper.
    It has at least one argument, so an operation without one takes
    [Gen.unit @-> …]. It ends in one result form, {!returns}, {!makes} or
    {!chooses}, and the types keep a result form out of argument position. *)

val ( @-> ) : 'a Gen.t -> ('r, 's, 'p) fn -> ('a -> 'r, 'a -> 's, 'a -> 'p) fn
(** [gen @-> fn] takes an argument drawn from [gen], the same value on both
    sides and in every run of the program, so neither side may mutate it. It
    shrinks as [gen] does.

    A generator that prints nothing, a {!Gen.constant} or a {!Gen.of_list}
    without {!Gen.with_pp}, fails the test at its first draw:
    [push: argument 2 has no printer; attach one with Gen.with_pp]. *)

val ( ^-> ) :
  ('ra, 'sa) abstract -> ('r, 's, 'p) fn -> ('ra -> 'r, 'sa -> 's, 'ra -> 'p) fn
(** [t ^-> fn] takes a value of [t] that an earlier call made: its reference
    side for the reference and [~pre], its system side for the system. It is
    drawn as one of the earlier calls that make a value of [t], and takes the
    value that call made. When that call made none, as when shrinking deleted
    it, it takes the newest value of [t]. It shrinks toward the newest value,
    and deleting other calls never moves it off the value its call made. *)

val returns : 'a testable -> ('a, 'a, bool) fn
(** [returns w] compares the two results under [w]. *)

val makes : ('r, 's) abstract -> ('r, 's, bool) fn
(** [makes t] keeps the two results as a new value of [t]. The value is made
    when the system returns, so the report names it and [~release] releases it
    even when the reference raised. When both sides raise an equal exception, no
    value is made. *)

val chooses : 'a testable -> (('a, exn) result -> 'a, 'a, bool) fn
(** [chooses w] is for an outcome the API leaves open, such as the element that
    a [take_any] returns. The reference receives the system's outcome, [Ok v] or
    [Error e], as its last argument, and returns or raises the outcome it
    accepts, updating its state to follow the choice. That outcome compares with
    the system's as any outcome does, so an illegal choice prints as an
    [expected] and [actual] pair. *)

type command
(** The type for commands: one operation of an API, on the reference and on the
    system. One list holds commands of every signature. *)

val command :
  ?__POS__:pos ->
  ?pre:('a -> 'p) ->
  string ->
  ('a -> 'r, 'b -> 's, 'a -> 'p) fn ->
  ('a -> 'r) ->
  ('b -> 's) ->
  command
(** [command name fn reference system] is the operation [name], whose
    reference's function is [reference] and system's is [system], the expected
    side first as in {!equal}.
    - [pre] is whether a call is legal, given the reference's arguments, an
      abstract argument as its reference side. It must not change them. Defaults
      to a [pre] that always holds.
    - [__POS__] is the location that a failing call reports when its failure
      recorded none, as a mismatch records none. It defaults to a capture at
      this call, never at the failure.
    - [name] names the calls of the command in the report. Its newlines become
      spaces. *)

val stateful :
  ?__POS__:pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?count:int ->
  ?steps:int ->
  ?domains:int ->
  string ->
  command list ->
  test
(** [stateful name commands] is a property test over the programs of [commands].
    Each case draws a program, runs it from no value, and fails at the first
    call whose outcomes differ.
    - [steps] is the most calls a program makes on the test's domain. Defaults
      to [20].
    - [domains] is the number of domains that the middle of a program runs on.
      Defaults to [1] (see below).
    - [count] and [timeout] are {!prop}'s. So are [--prop-count], the seed, the
      bound on shrinking and the [replay:] line.

    Shrinking removes calls, shrinks arguments and, on several domains, moves a
    parallel call out of its branch. It runs every candidate again.

    {b Commands never called.} When every case has passed, a command that a
    passing case could draw and that no passing case ran fails the test with a
    message that starts [never called: "pop" (over 100 passing cases)]. This is
    a demand on presence over the whole run, like {!cover}'s, so a [count] or
    [steps] too small can miss a legal command. A command listed twice is one
    command. Under [~count:0] nothing is judged.

    A stateful test carries the tags ["prop"] and ["stateful"]. Like a {!prop},
    it inherits the [retries] of an enclosing group, and every retry replays the
    same programs.

    {b Several domains.} With [~domains:n] above [1] a program is a short
    prefix, one branch per domain and a short suffix. The prefix and the suffix
    make at most [steps] calls between them. Each branch makes at most five
    calls on two domains, three on three, two on four and one from five, so that
    a program has at most 5040 orders up to seven domains; from eight domains,
    one call each gives [n!] orders, and the search of a failing program grows
    with them. Only the prefix has one reference state, so a command that makes
    a value or has a [~pre] is drawn only there: every branch and the suffix
    choose among the prefix's values, and no call after the prefix is refused.

    The test spawns [n] domains before its first case and joins them when it
    ends. Branch [i] runs its system functions on domain [i], every branch at
    once, and the prefix and the suffix run on the test's domain. Each program
    runs 50 times from no value, and the test fails when no order of the calls,
    each branch keeping its order and the suffix last, replayed on the
    reference, gives every outcome that the system gave. The search follows
    program order and never real time, so every linearizable history passes. The
    invariant runs after the prefix's calls only.

    The contract differs from one domain's in four ways:
    - a replay draws the same programs, not the same schedules, and may pass;
    - the test takes no retries and ignores a group's;
    - under [--mutate] and [--arm] each program runs once on the test's domain,
      the prefix, branch 1 to [n], then the suffix, so a kill does not depend on
      a schedule;
    - a call still running one limit after the test's limit expired fails the
      test as timed out, and the run stops after it, since its domain would run
      this test's code inside the next test. On Windows, where no limit is
      enforced, such a call hangs the run.

    It also carries the tag ["parallel"]. The domains need a processor each
    beside the test's. With fewer, a failure stays sound but fewer schedules are
    tried, and an {!xfail} test may find nothing and fail as an unexpected pass.
    A passing test runs [count] times 50 programs. After such a test has spawned
    its domains, [Unix.fork] raises [Failure] in every later test of the
    process. A spawn that fails fails the test with
    [cannot spawn a worker domain: <message>].

    Without a model the system is its own reference, as in
    [command "add" (h ^-> key @-> nat @-> returns unit) Hashtbl.add Hashtbl.add]:
    the test checks that parallel runs agree with sequential runs of the same
    code. It cannot see a bug that the code also has sequentially, and a module
    with global state shares it between the two sides.

    Raises [Invalid_argument], inside the running test and before any case, if
    [commands] is empty, if [steps] is negative, if [domains] is below [1], if
    the prefixes of the abstract types break the rules of {!val-abstract}, or if
    [domains] is above [1] and every command makes a value or has a [~pre], so
    that no call could run after the prefix. *)

(** {1:baselines Baselines}

    A baseline is reviewed text the source names. It is the literal at an
    {!expect} or {!expect_exact} call, or the file an {!expect_file} call names,
    relative to the project root.

    Checking writes nothing. A mismatch, or a missing file, records a failure
    and the call returns.

    {b Under dune} the [(deps …)] field of the [(test)] stanza names every file
    an {!expect_file} call reads. The source file of a literal needs no entry.
    The stanza's action runs the executable with [--corrected]. It then holds
    one [diff?] for each file that holds a baseline, which includes the source
    file of a literal. The run writes each correction as [<file>.corrected]
    beside dune's copy of the file, and [dune promote] accepts what a [diff?]
    reported. A correction is registered by dune only after an action that exits
    [0].

    {b Without dune} [-u] rewrites the literals and the files in place,
    atomically. Under [-u] a mismatch is accepted and its test passes. A literal
    is compiled into the executable, so after [-u] has rewritten one the
    executable must be built again before the next run.

    [-u] is refused under [CI], and [-u] with [--corrected] is a usage error.

    A correction is kept only for a test whose every failure is a baseline
    mismatch. An assertion failure, another exception or a skip beside the
    mismatch withholds it. An {!xfail} test keeps none. A test is not retried
    past an attempt whose corrections were kept, whatever its [retries].

    A correction that the source cannot take fails its test. The kept
    corrections are written once, after the last test. One that cannot be
    written fails the run. The project root is [WINDTRAP_PROJECT_ROOT] when set,
    else the parent of dune's build directory, else the working directory. *)

val expect : string -> pos * string -> unit
(** [expect actual @@ __POS_OF__ {|…|}] compares [actual] with the literal up to
    whitespace. On both sides every line is trimmed on the right, and the blank
    leading and trailing lines are dropped. The block is then dedented, so only
    relative indentation counts.

    {[
    test "help lists the flags" (fun () ->
        print_string (Tool.help ());
        expect (output ()) @@ __POS_OF__ {|
          usage: tool [OPTIONS]
          |})
    ]}

    A mismatch records a failure located at the line of [__POS_OF__] and
    returns. The position is the compiler's, so a call that moves keeps its
    baseline. Every test that checks one literal, as a {!cases} body does for
    each row, must produce one text. A literal each row carries, as in
    [(input, __POS_OF__ {|…|})], is checked and corrected for its row alone.

    When the source changed since the build, or cannot be read, a correcting run
    keeps no correction for the literal: the expectation fails, under [-u] too.
    A source file that cannot be proven to lie under the project root fails the
    test at once, as an assertion does. Raises [Invalid_argument] if no test is
    running. *)

val expect_exact : string -> pos * string -> unit
(** [expect_exact actual @@ __POS_OF__ {|…|}] is {!expect} comparing byte for
    byte. The correction of an [actual] that holds a CR is a quoted literal,
    with the CR written [\r]. *)

val expect_file : ?__POS__:pos -> string -> string -> unit
(** [expect_file actual path] compares [actual] with the file at [path],
    relative to the project root whatever the working directory. A mismatch
    records a failure and returns. A missing file is a mismatch that [-u]
    corrects by writing the file.

    The comparison is of lines of text. CR and CRLF read as LF, and both sides
    are given a final newline. Text in which those bytes matter must be encoded
    first, as with [String.escaped].

    Under dune the run reads dune's copy of the file. Under [--corrected] a
    missing file gets no correction and fails the run, so a new baseline starts
    as an empty file or is accepted once with [-u].

    [__POS__] is the failure's location (see {!type:pos}). A [path] that cannot
    be proven to lie under the project root fails the test at once, as an
    assertion does. Raises [Sys_error] if the file exists and cannot be read,
    and [Invalid_argument] if no test is running. *)

(** {1:capture Captured output}

    The runner captures what a test writes to standard output and standard
    error, C stubs and child processes included, into
    [<log dir>/<suite>/<groups>/<test>.output]. The log directory is [-o], by
    default [_tests] in dune's build directory and [windtrap] in the system
    temporary directory otherwise. [--stream] turns capture off.

    The block of a failing test shows the last lines that the test wrote after
    its last {!output} call, under [captured output], and names the log. What an
    {!output} call returned is not shown again, so a test whose failure is an
    expectation on [output ()] shows no captured output. *)

val output : unit -> string
(** [output ()] is what the running test wrote to standard output and standard
    error since the previous [output ()], or since the attempt started. The two
    streams are one text, in the order the bytes reached them, and [""] when
    nothing is left. A channel buffers, so text that [print_string] wrote may
    follow a later [prerr_endline], which flushes at once.

    Under [--stream] the call fails the test. When the test's log can no longer
    be opened, the call fails the test. Raises [Invalid_argument] if no test is
    running. *)

(** {1:body The running test}

    Operations on the test that is executing, in its setup, its body and its
    teardown. Each raises [Invalid_argument] when no test is running: at module
    top level, after the run, in the release of a fixture.

    Tests run one at a time, in declaration order, on the domain that called
    {!run}, so the environment and the working directory never race between
    tests. A {!stateful} test with [~domains] above [1] also runs system
    functions on domains that it joins before it ends. Nothing here is
    thread-safe. Every function here, {!output}, the
    {{!section-baselines}baseline} checks, {!collect}, {!classify}, {!cover} and
    a {!fixture}'s accessor raise a failure when called from another domain,
    which fails the running test when it reaches the test's domain, as through
    [Domain.join].

    A directory, a binding or a working directory made here lasts until the
    attempt ends. Each attempt of a retried test starts without them, and all
    the cases of a {!prop} or a {!stateful} test share them. *)

val current_test : unit -> string list
(** [current_test ()] is the path of the running test: the names of its
    enclosing groups, outermost first, then its own. It is never empty, and it
    is the same in every attempt and inside a {!subtest}. *)

val subtest : string -> (unit -> unit) -> unit
(** [subtest name fn] runs [fn ()] as a named part of the running test. A
    failure of [fn], a verb's or any other exception, is recorded and [subtest]
    returns. The subtests after it still run, and the test fails at the end with
    every failure recorded.

    A {!skip}, a timeout or a call to [exit] ends the whole test, which still
    fails on what was recorded, and inside the law of a property an {!assume}
    discards the case. [-f] cannot select a subtest. Inside the law of a
    property a subtest failure is not shrunk. *)

val temp_dir : ?prefix:string -> unit -> string
(** [temp_dir ?prefix ()] is a fresh empty directory under the system temporary
    directory. Each call makes another directory. The runner removes it when the
    attempt ends, on every outcome. The runner first gives the owner read, write
    and search permission on each directory in it, so a directory the test made
    unreadable or read-only is removed too. What the runner still cannot remove,
    such as a directory of another user, stays, and the outcome of the test does
    not change. [prefix] starts its basename and defaults to ["dir"].

    A resource that outlives the test, such as a {!fixture}'s, must not live in
    it. Raises [Unix.Unix_error] if the directory cannot be made. *)

val temp_file : ?suffix:string -> unit -> string
(** [temp_file ?suffix ()] is the path of a fresh empty file, with the lifetime
    of a {!temp_dir}. [suffix] ends its basename, as in [".json"], and defaults
    to [""]. Raises [Unix.Unix_error] if the file cannot be made. *)

val setenv : string -> string option -> unit
(** [setenv name (Some value)] binds the environment variable [name] to [value]
    for the rest of the test. [setenv name None] unbinds it. When the attempt
    ends, on every outcome, the runner restores what [name] held before the
    test's first [setenv] of it. A restoration that fails is a [[teardown]]
    failure of the test.

    {b Warning.} The binding belongs to the process. Threads and child processes
    see it, and a thread still running when the test ends races the restoration.

    Raises [Invalid_argument] if [name] is empty or contains ['=']. *)

val chdir : string -> unit
(** [chdir dir] changes the working directory to [dir] for the rest of the test.
    When the attempt ends, on every outcome, the runner returns to the directory
    the process was in before the test's first [chdir]. If it cannot, as when
    the test deleted it, the test fails with a [[teardown]] failure. The
    directory is restored before the environment, and before the {!temp_dir}s
    are removed.

    The change belongs to the process, as {!setenv}'s does. A relative
    {!expect_file} path does not follow it. Raises [Unix.Unix_error] if [dir]
    cannot be entered. *)

(** {1:running Running} *)

val run : ?argv:string array -> string -> test list -> int
(** [run ?argv suite tests] parses the {{!section-command_line}command line}
    [argv] and executes the selected tests of [tests], one at a time, in
    declaration order. It writes the report to standard output, returns the
    {{!section-exit_codes}exit code} and never exits the
    {{!section-process}process}. Usage errors, refusals, warnings and the line
    of an interrupted run go to standard error behind [windtrap:].

    [suite] names the run in its report, and names the directory of the capture
    logs and of the last failed tests. [argv] defaults to [Sys.argv]. [argv.(0)]
    is not parsed.

    Raises [Invalid_argument] if a run is executing, as when a test body starts
    another run. Two runs one after the other are allowed, and fixtures are
    acquired again in the second. *)

(** {2:exit_codes Exit codes}

    {!run} returns:
    - [0] when no selected test failed. A skip and an expected failure are no
      failure. [--help], [--version] and [-l] print and return [0].
    - [1] when a test failed, or when a fixture's release or the writing of a
      correction failed. It is also the code of a run refused before anything
      executed, under [-l] too. The refusals are two tests with one path, a
      {!focus} under [CI] and [-u] under [CI].
    - [2] when no test ran, as with a mistyped filter, or when the command line
      does not parse. An empty [--shard] bucket and a [--failed] with nothing
      recorded are cases of the first, and the second returns [2] under [-l]
      too.

    Under [--corrected] a test whose failures are all kept corrections leaves
    the code alone. It still stops a run under [-x], and it still enters the
    record that [--failed] reads. *)

(** {2:command_line Command line and environment}

    [--help] lists the flags, each with its mirror when it has one. A mirror is
    a [WINDTRAP_*] variable that sets the flag under [dune runtest]. [-l],
    [--failed], [-x], [-u], [--corrected], [-h] and [-V] have none. The command
    line wins over the environment.

    [--tag] and [--exclude-tag] add up, across repeated flags, across the
    comma-separated list of their mirrors, and across both. [WINDTRAP_FILTER]
    and [WINDTRAP_EXCLUDE] hold one pattern each, commas included, and the
    patterns of [-f] or [-e] on the command line replace that of the mirror.
    Flags that change what prints change no outcome and no exit code.

    Under [dune runtest] a mirror reaches every test stanza of the project, and
    a suite that cannot honour it is not in error.
    - A selection that the mirrors alone give, and that keeps no test of a suite
      that declares some, returns [0]. A selection flag on the command line
      makes it [2].
    - [WINDTRAP_MUTATE] on an executable that has no mutant to test, or whose
      selection keeps no test, runs the suite without mutation. [--mutate] there
      returns [1].
    - A relative path in [WINDTRAP_JUNIT] or [WINDTRAP_OUTPUT] is read from the
      project root, and one on the command line from the working directory.

    {b Warning.} A changed variable does not make dune run a test again. On a
    stanza that already passed, a mirror does nothing without
    [dune runtest --force].

    [--shard K/N], with [1 <= K <= N], keeps bucket [K] of [N] of the selection
    by a frozen hash of each path. The buckets cover every test once, the same
    on every machine and whatever else the suite holds.

    [--failed] selects the last failed tests. The tests a run executes update
    that record, and a test it did not execute, under a filter or after [-x],
    keeps its entry. A run never fails because it cannot read or write the
    record, and a record it cannot read counts as empty.

    Beyond the mirrors {!run} reads [WINDTRAP_PROJECT_ROOT] (see
    {{!section-baselines}baselines}), [CI], [GITHUB_ACTIONS], [INSIDE_DUNE],
    [NO_COLOR], [TERM] and whether standard output is a terminal. [CI] counts as
    set unless it is empty or one of [0], [false], [no], [n] and [off]. Under
    [--color auto] the report is styled on a terminal or under [INSIDE_DUNE],
    unless [NO_COLOR] is set or [TERM] is [dumb]. *)

(** {2:process The process}

    A call to [exit] in code under test does not end the run. It is recorded as
    the failure of its test. A handler that catches every exception around the
    call defeats this. {!run} turns the recording of backtraces on and leaves it
    on.

    While it executes, and not on Windows, {!run} handles [SIGINT], [SIGTERM]
    and [SIGHUP]. It removes the attempt's temporary files and releases the
    fixtures still held. It does not run the teardown of the interrupted test,
    writes no correction and dies by the same signal. *)

(** {1:private Private} *)

(** Windtrap's own composition surface, for its test suite, its command-line
    tool and the runtime of [ppx_windtrap]. It is not part of the public
    interface and carries no stability guarantee. [open Windtrap] brings none of
    it into scope. *)
module Private : sig
  module Baseline = Baseline
  module Capture = Capture
  module Check = Check
  module Cli = Cli
  module Diff = Diff
  module Failure = Failure
  module Gen_engine = Gen_engine
  module Loc = Loc
  module Mutate_loop = Mutate_loop
  module Os = Os
  module Pp = Pp
  module Property = Property
  module Report = Report
  module Report_junit = Report_junit
  module Report_sections = Report_sections
  module Run = Run
  module Seed = Seed
  module Source_patch = Source_patch
  module Stateful = Stateful
  module Test_tree = Test_tree
  module Text = Text
  module Workers = Workers
end
