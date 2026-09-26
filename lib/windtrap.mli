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
    declaration. [~__POS__] on the assertion gives the assertion's line.

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
    with a limit runs.

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

    The three witnesses order with [Float.compare], whatever the tolerance.
    Under [float 0.5], [1.0] is below [1.2]. NaN is below every float.

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

    Shrinking takes at most [10_000] steps. It also stops when a function of the
    generator raises on a candidate. A function of the generator that raises
    while a case is drawn fails the case. *)

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

  (** {1:numeric Numbers} *)

  val int : int t
  (** [int] generates an integer uniformly over the whole [int] range. It
      shrinks toward [0]. *)

  val nat : int t
  (** [nat] generates a natural number below [10_000], small values more often:
      50% below [10], 25% below [100], 20% below [1_000], 5% below [10_000]. It
      shrinks toward [0]. *)

  val small_int : int t
  (** [small_int] generates an integer of either sign whose magnitude follows
      {!nat}, so a value in \[[-9_999];[9_999]\]. It shrinks toward [0]. *)

  val int_range : int -> int -> int t
  (** [int_range low high] generates an integer in \[[low];[high]\], uniformly.
      It shrinks toward the point of the range closest to [0]. Sampling raises
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
      whose characters follow [char]. [size] defaults to {!nat}. It shrinks as
      {!list} does. It always prints, as a quoted string, even when [char] has
      no printer. Sampling raises [Invalid_argument] if [size] generates a
      negative length. *)

  val bytes : bytes t
  (** [bytes] is [bytes_of char]. *)

  val bytes_of : ?size:int t -> char t -> bytes t
  (** [bytes_of ?size char] is {!string_of} for [bytes]. *)

  (** {1:containers Containers} *)

  val list : ?size:int t -> 'a t -> 'a list t
  (** [list ?size gen] generates a list of [gen] values whose length follows
      [size]. [size] defaults to {!nat}.

      With the default [size] a list shrinks by structure first: to the empty
      list, then by the removal of chunks of halving length. It then shrinks
      element by element, from the left. With an explicit [size] the length
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
      a probability proportional to its weight. The choice does not shrink. The
      value shrinks with its generator. Sampling raises [Invalid_argument] if
      [weighted] is empty, if a weight is negative, or if the weights sum to
      less than [1]. *)

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
    of a {!prop}, and in the bodies and the invariant of a {!stateful} test.
    There a discard drops the whole program and a label counts once per program.
    {!assume} and {!reject} work too in a function given to a generator, where a
    discard drops the case or the shrink candidate. *)

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

    A stateful test checks generated sequences of calls on a system against a
    model, a pure value that stands for the state of the system. A
    {{!type:command}command} is one operation of the system. A program is a
    sequence of calls, each legal in the model that the calls before it
    produced. *)

type ('model, 'sut) command
(** The type for operations on a system ['sut] modelled by ['model]. One list
    holds commands whose arguments differ in type. *)

val command :
  ?__POS__:pos ->
  ?pre:('model -> 'arg -> bool) ->
  string ->
  'arg Gen.t ->
  next:('model -> 'arg -> 'model) ->
  ('model -> 'arg -> 'sut -> unit) ->
  ('model, 'sut) command
(** [command name gen ~next body] is the operation [name], whose argument [gen]
    draws. Every function takes the model first, then the argument.
    - [pre m arg] is whether the call is legal in [m]. Defaults to always. The
      call is generated only where [pre] holds.
    - [next m arg] is the model after the call.
    - [body m arg sut] calls the system and asserts with the verbs. [m] is the
      model before the call.
    - [__POS__] is the declaration site, which a failing call reports when its
      assertion recorded no location.

    No call the model forbids is ever made.

    [pre] and [next] must be pure and ['model] persistent, because the model's
    trajectory is computed again whenever a program is drawn, run or printed.

    A [pre] or [next] that raises while a program is drawn fails the case
    unshrunk. One that raises only on a shrink candidate stops the search. *)

val call :
  ?__POS__:pos ->
  ?pre:('model -> bool) ->
  string ->
  next:('model -> 'model) ->
  ('model -> 'sut -> unit) ->
  ('model, 'sut) command
(** [call name ~next body] is {!val:command} for an operation without argument.
*)

val stateful :
  ?__POS__:pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?count:int ->
  ?steps:int ->
  ?pp_model:'model printer ->
  ?invariant:('model -> 'sut -> unit) ->
  string ->
  model:'model ->
  scope:(('sut -> unit) -> unit) ->
  ('model, 'sut) command list ->
  test
(** [stateful name ~model ~scope commands] is a property test over the programs
    of [commands], from the initial model [model]. Each case draws a program,
    runs it against a fresh system, and checks every body and the invariant.
    - [scope] provides the system (see below).
    - [invariant m sut] runs on the fresh system before the first call and after
      every call. The last assertion of an invariant is in tail position.
      Without [~__POS__] its failure is located at the test's declaration.
    - [steps] is the number of calls drawn per case. Defaults to [20]. A drawn
      call whose [pre] fails is dropped, so a program has at most [steps] calls.
    - [pp_model] adds a column to the printed program: the model before each
      call.
    - [count] and [timeout] are {!prop}'s. So are [--prop-count], the seed and
      the bound on shrinking.

    Each drawn call picks its command with equal probability. A command listed
    twice is drawn twice as often. Shrinking removes calls and shrinks
    arguments, and never replaces one operation by another.

    {b The scope.} [scope] takes a callback, calls it once with a fresh system,
    and releases the system whether the callback returns or raises. It runs once
    per case and once per shrink candidate.

    A scope that returns without calling back fails the case. A second call
    raises [Invalid_argument]. What [scope] raises before calling back fails the
    case, and a {!skip} there skips the test.

    A program's failure is raised again through [scope], so a scope that
    swallows it cannot pass the case. A release that raises over a failing
    program is dropped and the counterexample stands, unless the release skips,
    times out, exits or discards, which keeps its meaning. Over a passing
    program it fails the case.

    {b Warning.} A scope must make and remove its own files, under absolute
    paths, and put process state back itself.

    A stateful test carries the tags ["prop"] and ["stateful"]. Like a {!prop},
    it inherits the [retries] of an enclosing group, and every retry replays the
    same programs. The system must behave the same from run to run.

    Raises [Invalid_argument], inside the running test, if [commands] is empty
    or if [steps] is negative. *)

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
    baseline. A call several tests share, as under {!cases}, must produce one
    text.

    When the source changed since the build, or cannot be read, a correcting run
    keeps no correction for the literal: the expectation fails, under [-u] too.
    A source file that cannot be proven to lie under the project root fails the
    test at once, as an assertion does. Raises [Invalid_argument] if no test is
    running. *)

val expect_exact : string -> pos * string -> unit
(** [expect_exact actual @@ __POS_OF__ {|…|}] is {!expect} comparing byte for
    byte. *)

val expect_file : string -> string -> unit
(** [expect_file actual path] compares [actual] with the file at [path],
    relative to the project root whatever the working directory. A mismatch
    records a failure and returns. A missing file is a mismatch whose correction
    is the file.

    The comparison is of lines of text. CR and CRLF read as LF, and both sides
    are given a final newline. Text in which those bytes matter must be encoded
    first, as with [String.escaped].

    Under dune the run reads dune's copy of the file. Promotion never creates a
    file, so a new baseline starts as an empty file or is accepted once with
    [-u].

    A [path] that cannot be proven to lie under the project root fails the test
    at once, as an assertion does. Its failure is located at the call and, from
    tail position, at the test's declaration. Raises [Sys_error] if the file
    exists and cannot be read, and [Invalid_argument] if no test is running. *)

(** {1:capture Captured output}

    The runner captures what a test writes to standard output and standard
    error, C stubs and child processes included, into
    [<log dir>/<suite>/<groups>/<test>.output]. The log directory is [-o], by
    default [_tests] in dune's build directory and [windtrap] in the system
    temporary directory otherwise. [--stream] turns capture off. *)

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

    Tests run one at a time, in one domain, in declaration order, so the
    environment and the working directory never race between tests. Nothing here
    is thread-safe.

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
    attempt ends, on every outcome. [prefix] starts its basename and defaults to
    ["dir"].

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
    keeps its entry.

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

(** Windtrap's own composition surface, for its test suite and its command-line
    tool. It is not part of the public interface and carries no stability
    guarantee. [open Windtrap] brings none of it into scope. *)
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
end
