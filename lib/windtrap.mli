(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** One library for all your OCaml tests.

    Windtrap runs unit, property, stateful and expect tests, inline or not, from
    one flat surface: declare tests with {!test} and {!group}, assert with the
    verbs ({!equal}, {!require_some}, {!raises}, ...), and hand the suite to
    {!run}:

    {[
    open Windtrap

    let () =
      exit
      @@ run "mylib"
           [
             test "addition" (fun () -> equal int 5 (Calc.add 2 3));
             group "parser"
               [
                 test "empty input" (fun () ->
                     raises (Parse_error "empty") (fun () -> Calc.parse ""));
               ];
           ]
    ]}

    Comparisons go through an ['a] {!type:testable}, a printer and an equality,
    so every failure prints a diff of the two values; witnesses for base types
    and containers are re-exported flat ({!int}, {!list}, {!pair}, ...) and
    their constructors live in {!Testable}. {!prop} checks a law over an ['a]
    {!Gen.t} with integrated shrinking; {!stateful} checks sequences of
    {!command}s against a model. {!expect}, {!expect_exact} and {!expect_file}
    compare produced text with a literal or a committed file and print their
    acceptance command on mismatch. {!cases}, {!bracket}, {!scoped} and
    {!fixture} structure tests and scope resources; {!temp_dir}, {!temp_file},
    {!setenv}, {!chdir}, {!output} and {!subtest} act on the running test;
    {!focus} and {!val:xfail} annotate one.

    The companion [ppx_windtrap] package adds inline expect tests
    ([let%expect_test]), accepted through [dune promote], and the
    [ppx_windtrap.coverage] and [ppx_windtrap.mutate] instrumentation backends,
    both reported by the [windtrap] command. The manual under [doc/manual/] is
    the long-form companion to this reference; [doc/cookbook.md] holds the
    recipes windtrap does not absorb; runnable projects live under [examples/].
*)

(** {1:types Types} *)

type test = Test_tree.t
(** The type for declared tests: a leaf test or a named group of tests. Inert
    data: nothing runs until {!run} executes the suite. *)

type pos = string * int * int * int
(** The type of [__POS__] payloads: file, line, start column, end column. Every
    [?__POS__] below overrides the automatic call-stack location, which needs
    debug information ([-g], dune's default). Pass [~__POS__] from a helper that
    wraps a verb or a constructor, and from an assertion in tail position, whose
    frame is gone when it raises; the report then attributes it to the test's
    declaration and says so. When no location is known, reports omit it. *)

type 'a printer = Format.formatter -> 'a -> unit
(** The type for value printers, shared by testables, generators
    ({!Gen.with_pp}) and the [?pp] arguments of the shape assertions. *)

module Testable = Testable
(** Witness constructors: {!Testable.make}, {!Testable.structural}. See
    {!section-testables}. *)

type 'a testable = 'a Testable.t
(** The type for assertion witnesses: a printer and an equality, both total, and
    optionally an order. The equality is applied to [expected] first and
    [actual] second. See {!section-testables}. *)

(** {1:declaring Declaring tests}

    Declaring is data construction: bodies run only when {!run} executes the
    tree, inside a per-test exception boundary. Every constructor takes:

    - [__POS__], the declaration site (defaults to a best-effort call-stack
      capture; see {!type:pos});
    - [tags], extra tag names, unioned with the enclosing groups'; selected with
      [--tag] and [--exclude-tag];
    - [timeout], the per-test limit in seconds (defaults to the runner's
      [--timeout]). Setup and body share the window; teardown gets what is left
      of it, or a fresh window if they consumed it. A {!scoped} test spends it
      on the whole scope call, re-armed as the body leaves the callback; a
      {!prop} spends it on generation and shrinking together;
    - [retries], the number of extra attempts a failing test gets (defaults to
      [0]), each a fresh setup, body, teardown and capture; the run records the
      attempts a test took.

    On a group, [tags] extend every descendant's and [timeout] and [retries] are
    defaults for every test under it; the innermost declaration wins.
    Constructors raise [Invalid_argument] if [retries < 0] or if [timeout] is
    not finite and positive.

    A test is named by its {e path}: enclosing group names, then its own, joined
    with [" › "]. Filters match that string; duplicate paths refuse the run. *)

val test :
  ?__POS__:pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  (unit -> unit) ->
  test
(** [test name fn] declares the test [name] with body [fn]. The body passes by
    returning and fails by raising. *)

val group :
  ?__POS__:pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  test list ->
  test
(** [group name children] declares a group. Groups nest; [name] becomes a path
    component. Groups have no hooks: scope resources with {!bracket}, {!scoped}
    or {!fixture}. *)

val slow :
  ?__POS__:pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  (unit -> unit) ->
  test
(** [slow] is {!test} with the ["slow"] tag pre-applied: the tag exempts the
    test from the slow-test warning, and [--exclude-tag slow] drops it. *)

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
(** [cases ~name base inputs fn] is
    [group base (List.map (fun i -> test (name i) (fun () -> fn i)) inputs)],
    every child recording the [cases] call's declaration site; [tags], [timeout]
    and [retries] sit on the group and apply per child. Each child is selectable
    ([-f "ports parse › 8080"]). [inputs] is evaluated at declaration time,
    outside any test.

    {[
    cases "ports parse" ~name:Fun.id [ "1"; "80"; "8080"; "65535" ]
      (fun input -> ignore (require_ok (parse_port input)))
    ]} *)

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
(** [bracket ~setup ~teardown name fn] declares a test scoping a resource: the
    runner calls [setup ()], passes the resource to [fn], and calls [teardown]
    on it iff [setup] succeeded, on every outcome including skip and timeout,
    except a fatal exception ([Sys.Break], [Out_of_memory], [Stack_overflow]),
    which skips the teardown and ends the run. It is {!scoped} over the scope
    that runs the three in order, so what [setup] raises is a [[setup]] failure
    and what [teardown] raises a [[teardown]] failure reported beside the
    body's. Partial application builds reusable constructors:

    {[
    let with_db = bracket ~setup:Db.connect ~teardown:Db.close
    let tests = [ with_db "count" (fun db -> equal int 0 (Db.count db)) ]
    ]} *)

val scoped :
  (('r -> unit) -> unit) ->
  ?__POS__:pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  ('r -> unit) ->
  test
(** [scoped scope name fn] declares a test whose resource is scoped by [scope],
    a function that acquires a resource, hands it to a callback and reclaims it
    on return ([Eio_main.run], [In_channel.with_open_text path]). The runner
    calls [scope] exactly once with a callback that runs [fn]. [scope] precedes
    the optional arguments, so [scoped Eio_main.run] keeps them.

    {[
    let with_eio = scoped Eio_main.run

    let tests =
      [
        with_eio "reads the config" (fun env ->
            let fs = Eio.Stdenv.fs env in
            equal string "{}" Eio.Path.(load (fs / "config.json")));
      ]
    ]}

    A failure raised by [fn] (an assertion, a {!skip}, a timeout) is recorded
    and re-raised through [scope], so a scope that swallows it cannot pass the
    test. The callback must be called exactly once: a scope that never calls it
    fails the test, one that calls it twice runs the body once and fails the
    test. What [scope] raises before the callback is a [[setup]] failure, after
    it a [[teardown]] failure reported beside the body's; a scope that skips
    reports a skip. [timeout] covers the whole [scope] call and is re-armed as
    the body leaves the callback. *)

(** {2:annotations Annotations}

    An annotation wraps a declared test, keeps its declaration site, and on a
    group reaches every test under it:
    [xfail ~reason:"issue #42" (group "parser" [ ... ])]. *)

val focus : test -> test
(** [focus t] focuses [t]: when any focused test exists, only focused tests run.
    Under [CI] a run containing focused tests refuses to start; outside CI a
    successful focused run prints a warning. *)

val xfail : ?reason:string -> test -> test
(** [xfail t] marks [t] as expected to fail: it still runs, a failing outcome
    reports as [XFAIL] without failing the run, and a passing outcome fails
    (["expected to fail, but the test passed"]). Skips are unaffected. [reason]
    names the known defect for reports. Nested annotations resolve
    innermost-wins. *)

val fixture : ?teardown:('a -> unit) -> (unit -> 'a) -> unit -> 'a
(** [fixture ?teardown create] is an accessor for a run-scoped shared resource.
    Creating the accessor runs nothing; the first call inside a test acquires
    with [create ()], inside that test's failure boundary, and later calls
    return the cached value. The acquisition outcome is cached for the run: a
    [create] that raises fails the acquiring test, every later call re-raises
    the same exception with its original backtrace, and nothing is registered
    for release; a {!skip} raised during acquisition skips every test that
    touches the accessor. A fixture no selected test touches is never acquired.
    Acquired fixtures are released after the last test, in reverse acquisition
    order, on every path where the runner regains control ([-x] included); a
    release failure fails the run. Release runs outside every per-test timeout
    and is announced before it runs, naming the fixture's declaration site.

    Raises [Invalid_argument] when called outside a run. *)

(** {1:assertions Assertions}

    Each verb raises one structured failure that the runner catches at the test
    boundary; the failure records the call site ([?__POS__], else a best-effort
    call-stack capture) and the [?msg] annotation. Expected precedes actual. An
    assertion failing outside any run is an ordinary uncaught exception,
    rendered with the failure's one-line summary. *)

val equal : ?__POS__:pos -> ?msg:string -> 'a testable -> 'a -> 'a -> unit
(** [equal t expected actual] asserts that [expected] and [actual] are equal
    under [t]. The failure renders both with [t]'s printer and diffs them. *)

val not_equal : ?__POS__:pos -> ?msg:string -> 'a testable -> 'a -> 'a -> unit
(** [not_equal t a b] asserts that [a] and [b] are not equal under [t]. The
    failure prints the value once. *)

val less : ?__POS__:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** [less t ~than v] asserts that [v] is strictly below [than] under [t]'s
    order. The failure prints the bound and the value:

    {[
    less int ~than:3 (retries c)
    (* expected  less than 3
       actual    5 *)
    ]}

    The base-type witnesses carry their module's order, {!Testable.structural}
    carries [Stdlib.compare], {!Testable.with_compare} gives one to any witness
    and {!Testable.contramap} orders through its projection. A witness without
    one ({!pass}, {!Testable.of_equal}, a plain {!Testable.make}, every
    container witness) makes the assertion raise [Invalid_argument]. Tolerance
    plays no part: under [float 0.5], [1.0] is less than [1.2]. NaN sorts below
    every float. *)

val at_most :
  ?__POS__:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** [at_most t ~than v] asserts that [v] is below or equal to [than] under [t]'s
    order; see {!less}. *)

val greater :
  ?__POS__:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** [greater t ~than v] asserts that [v] is strictly above [than] under [t]'s
    order; see {!less}. *)

val at_least :
  ?__POS__:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** [at_least t ~than v] asserts that [v] is above or equal to [than] under
    [t]'s order; see {!less}. A range is two assertions:
    [greater t ~than:lo v; less t ~than:hi v]. *)

val is_true : ?__POS__:pos -> ?msg:string -> bool -> unit
(** [is_true b] asserts [b]. *)

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
(** [satisfies t pred v] asserts [pred v]. The failure renders [v] with [t]'s
    printer against [claim], the sentence on the expected side (default
    ["value satisfying the predicate"]). [pred] must be total; the printer runs
    only on failure.

    {[
    satisfies ~claim:"a power of two" int (fun n -> n land (n - 1) = 0) n
    (* expected  a power of two
       actual    12 *)
    ]} *)

val starts_with : ?__POS__:pos -> ?msg:string -> affix:string -> string -> unit
(** [starts_with ~affix s] asserts that [s] begins with [affix]. The failure
    prints the affix and a bounded excerpt of [s], and says where [affix] occurs
    when it occurs elsewhere. *)

val ends_with : ?__POS__:pos -> ?msg:string -> affix:string -> string -> unit
(** [ends_with ~affix s] asserts that [s] ends with [affix]. *)

val mem : ?__POS__:pos -> ?msg:string -> 'a testable -> 'a -> 'a list -> unit
(** [mem t x xs] asserts that [xs] has an element equal to [x] under [t]. The
    failure prints [x] and [xs]. For a byte substring, use {!contains}. *)

val is_none : ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> 'a option -> unit
(** [is_none o] asserts that [o] is [None]. On [Some v] the failure renders [v]
    with [pp] when given and as [<abstract>] otherwise. *)

val is_some : ?__POS__:pos -> ?msg:string -> 'a option -> unit
(** [is_some o] asserts that [o] is [Some _]. *)

val is_ok :
  ?__POS__:pos -> ?msg:string -> ?pp:'e printer -> ('a, 'e) result -> unit
(** [is_ok r] asserts that [r] is [Ok _]. On [Error e] the failure renders [e]
    with [pp] when given and as [<abstract>] otherwise. *)

val is_error :
  ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> ('a, 'e) result -> unit
(** [is_error r] asserts that [r] is [Error _]. On [Ok v] the failure renders
    [v] with [pp] when given and as [<abstract>] otherwise. *)

val contains : ?__POS__:pos -> ?msg:string -> sub:string -> string -> unit
(** [contains ~sub s] asserts that [s] contains [sub] as a byte substring; the
    empty needle is contained in every string. The failure prints the needle and
    a bounded excerpt of [s]. *)

val not_contains : ?__POS__:pos -> ?msg:string -> sub:string -> string -> unit
(** [not_contains ~sub s] asserts that [s] does not contain [sub] as a byte
    substring, so it always fails when [sub] is empty. The failure prints the
    needle, the byte offset of its first occurrence and a bounded excerpt of [s]
    around it. *)

val in_order : ?__POS__:pos -> ?msg:string -> subs:string list -> string -> unit
(** [in_order ~subs s] asserts that each element of [subs] occurs in [s], each
    match beginning at or after the end of the previous match. Matches never
    share bytes, so [["aa"; "aa"]] needs four [a]s; an empty element matches
    without advancing. The failure names the element that broke the chain and
    the byte the search had reached, and says when that element occurs before
    the cursor.

    {[
    in_order ~subs:[ "connect"; "authenticate"; "disconnect" ] session_log
    ]}

    Raises [Invalid_argument] if [subs] is empty. *)

val require_some : ?__POS__:pos -> ?msg:string -> 'a option -> 'a
(** [require_some o] asserts that [o] is [Some v] and returns [v].

    {[
    let user = require_some (Store.find store "alice") in
    equal string "alice" user.name
    ]} *)

val require_ok :
  ?__POS__:pos -> ?msg:string -> ?pp:'e printer -> ('a, 'e) result -> 'a
(** [require_ok r] asserts that [r] is [Ok v] and returns [v]. On [Error e] the
    failure renders [e] with [pp] when given and as [<abstract>] otherwise. *)

val require_error :
  ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> ('a, 'e) result -> 'e
(** [require_error r] asserts that [r] is [Error e] and returns [e]. On [Ok v]
    the failure renders [v] with [pp] when given and as [<abstract>] otherwise.
*)

val require_match :
  ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> ('a -> 'b option) -> 'a -> 'b
(** [require_match extract v] asserts that [extract v] is [Some b] and returns
    [b]. On [None] the failure renders [v] with [pp] when given and as
    [<abstract>] otherwise; the printer runs only on failure. An exception
    raised by [extract] propagates unchanged.

    {[
    let port = require_match (function Tcp p -> Some p | _ -> None) addr
    ]} *)

val raises : ?__POS__:pos -> ?msg:string -> exn -> (unit -> 'a) -> unit
(** [raises e f] asserts that [f ()] raises an exception structurally equal to
    [e]. The failure distinguishes "nothing raised" from "raised a different
    exception", carries the raised exception's backtrace when one was recorded,
    and reads as a message diff when the raised exception has the expected
    constructor but a different message ([Invalid_argument], [Failure],
    [Sys_error]). Payloads structural equality cannot compare need
    {!raises_match}. *)

val raises_match :
  ?__POS__:pos -> ?msg:string -> (exn -> bool) -> (unit -> 'a) -> unit
(** [raises_match pred f] asserts that [f ()] raises an exception satisfying
    [pred]; the failure prints the raised exception. [pred] must be total.
    {!Exn} has the common predicates. *)

(** Exception predicates for {!raises_match}: constructor checks with an
    optional message constraint. Without [~substring] any message passes; with
    it the message must contain the given byte substring.

    {[
    raises_match (Exn.invalid_arg ~substring:"unhandled op") (fun () ->
        Machine.step m op)
    ]} *)
module Exn : sig
  val invalid_arg : ?substring:string -> exn -> bool
  (** [invalid_arg e] is [true] iff [e] is [Invalid_argument m] and [m] contains
      [substring], if given. *)

  val failure : ?substring:string -> exn -> bool
  (** [failure e] is [true] iff [e] is [Failure m] and [m] contains [substring],
      if given. *)

  val sys_error : ?substring:string -> exn -> bool
  (** [sys_error e] is [true] iff [e] is [Sys_error m] and [m] contains
      [substring], if given. *)
end

val fail : ?__POS__:pos -> string -> 'a
(** [fail msg] fails the current test with [msg]. It never returns. *)

val failf : ?__POS__:pos -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [failf fmt ...] is {!fail} with a [Format] message. *)

val skip : ?reason:string -> unit -> 'a
(** [skip ()] skips the current test, which is not a failure; the report shows
    it as skipped with [reason]. A nonempty selection whose every test skipped
    exits [0]. *)

(** {1:testables Testables}

    Witnesses for base types and containers, re-exported flat so that
    [equal (list (pair string int))] reads without qualification. Constructors
    live in {!Testable}: {!Testable.make} for a printer and an equality,
    {!Testable.with_compare} for an order, {!Testable.structural} for
    polymorphic equality and order, {!Testable.contramap} to compare through a
    projection, {!Testable.of_equal} for a type with no rendering. Diffs are
    computed from the printed values, so no witness needs diff support.

    The base-type witnesses carry their module's order; the container witnesses
    carry none, so an ordering assertion over one needs
    {!Testable.with_compare}. *)

val unit : unit testable
val bool : bool testable
val char : char testable

val string : string testable
(** [string] prints with [%S]: quoted, escaped, on one line. *)

val text : string testable
(** [text] prints verbatim, newlines kept, so failures diff it line by line. Use
    it for multi-line text and {!string} for single-line values, where the
    quotes distinguish [""], [" "] and ["\t"]. *)

val bytes : bytes testable
val int : int testable
val int32 : int32 testable
val int64 : int64 testable
val nativeint : nativeint testable

val float_exact : float testable
(** [float_exact] compares floats bit for bit, every NaN equal to every NaN. See
    {!Testable.float_exact}. *)

val float : float -> float testable
(** [float eps] compares with absolute tolerance: [a] and [b] are equal when
    [a = b] or [|a -. b| <= eps]. Raises [Invalid_argument] if [eps] is not
    strictly positive. *)

val float_rel : rel:float -> abs:float -> float testable
(** [float_rel ~rel ~abs] compares with combined tolerance: within [abs] near
    zero, within [rel *. Float.max (abs_float a) (abs_float b)] for large
    values. Raises [Invalid_argument] if either bound is negative or NaN, or if
    both are zero. *)

val option : 'a testable -> 'a option testable
val result : 'a testable -> 'e testable -> ('a, 'e) result testable
val either : 'a testable -> 'b testable -> ('a, 'b) Either.t testable
val list : 'a testable -> 'a list testable
val array : 'a testable -> 'a array testable

val slist : 'a testable -> ('a -> 'a -> int) -> 'a list testable
(** [slist t cmp] is [Testable.contramap (List.sort cmp) (list t)]: lists
    compared, and printed, as multisets. *)

val pair : 'a testable -> 'b testable -> ('a * 'b) testable

val triple :
  'a testable -> 'b testable -> 'c testable -> ('a * 'b * 'c) testable

val quad :
  'a testable ->
  'b testable ->
  'c testable ->
  'd testable ->
  ('a * 'b * 'c * 'd) testable

val pass : 'a testable
(** [pass] considers all values equal, prints [<pass>] and carries no order: for
    ignoring a component of a composed witness, e.g. [pair string pass]. *)

(** {1:properties Properties}

    A property checks a law over generated inputs: {!prop} draws values from an
    ['a] {!Gen.t}, runs the body on each, and on failure shrinks the input to a
    minimal counterexample. Bodies return [unit] and use the assertion verbs.
    Every generated value derives from the run's root seed, the test's path and
    the case index, so the [s1:...] token in the run header replays every
    failure; the failure report prints the replay command. *)

(**/**)

(* The engine under a name the narrowing below leaves alone: from
   [module Gen : sig ... end] on, [Gen] no longer denotes the module it
   narrows, so [Private] reaches the engine through this alias. Hidden
   from the rendered page and no part of the public API. *)
module Gen_engine = Gen.Engine

(**/**)

module Gen : sig
  (** Random value generators with integrated shrinking and printing:
      primitives, containers, choice, {!map}, {!bind}, the binding operators and
      {!with_pp}.

      An ['a t] draws a value from the run's seed, carries the lazy tree of its
      shrink candidates, and knows how values print in counterexamples. Every
      generator shrinks, and candidates satisfy the same constraints as
      generated values.

      {b Printing.} Primitives print; containers and choices derive their
      printer from their components'; {!constant}, {!of_list}, {!map} and
      {!bind} (and so [let+], [and+], [let*]) have none. A counterexample whose
      generator has no printer renders as its {e pre-image}: the same shape,
      with every printerless [map] or [bind] result replaced by what it was
      computed from, down to the nearest generator that prints. Where nothing
      prints at all the counterexample renders as a placeholder naming
      {!with_pp}.

      {b Validation.} Constructors never raise: malformed arguments
      ([one_of []], [int_range 3 1]) raise [Invalid_argument] when the generator
      first samples, inside the running test's boundary.

      Callbacks passed to {!map}, {!bind} and {!such_that} must be pure: the
      shrink search runs them, memoized, when it forces candidates. *)

  (** {1:generators Generators} *)

  type 'a t = 'a Gen.t
  (** The type for generators of values of type ['a]. *)

  (** {1:numeric Numeric generators} *)

  val int : int t
  (** [int] generates a uniformly distributed integer over the full [int] range.
      Candidates shrink toward [0]. *)

  val nat : int t
  (** [nat] generates a natural number below [10_000], biased toward small
      values: 50% below [10], 25% below [100], 20% below [1_000], 5% below
      [10_000]. Candidates shrink toward [0]. Use it for sizes, lengths, and
      counts. *)

  val small_int : int t
  (** [small_int] generates an integer whose magnitude follows {!nat} — inside
      \[[-9_999];[9_999]\], biased toward small magnitudes, either sign.
      Candidates shrink toward [0]. Use it instead of {!int} when full-range
      values would overflow the arithmetic under test. *)

  val int_range : int -> int -> int t
  (** [int_range low high] generates an integer in \[[low];[high]\], uniformly.
      Candidates shrink toward the in-range point closest to [0] and stay in
      range.

      Sampling raises [Invalid_argument] if [high < low]. *)

  val int32 : int32 t
  (** [int32] generates a uniformly distributed [int32] over the full 32-bit
      range. Candidates shrink toward [0l]. *)

  val int64 : int64 t
  (** [int64] generates a uniformly distributed [int64] over the full 64-bit
      range. Candidates shrink toward [0L]. *)

  val nativeint : nativeint t
  (** [nativeint] generates a uniformly distributed [nativeint] over the full
      native word range. Candidates shrink toward [0n]. *)

  val float : float t
  (** [float] generates a finite float by drawing uniform IEEE 754 bit patterns
      and rejecting non-finite ones, so magnitudes spread over the full exponent
      range, including subnormals. Candidates shrink toward [0.]. *)

  val float_range : float -> float -> float t
  (** [float_range low high] generates a float in \[[low];[high]\], uniformly.
      Candidates shrink toward the in-range point closest to [0.] and stay in
      range.

      Sampling raises [Invalid_argument] if [high < low], if either bound is not
      finite, or if [high -. low] overflows to infinity. *)

  (** {1:base Unit, booleans, characters, strings} *)

  val unit : unit t
  (** [unit] generates [()], with no shrink candidates, and prints [()]. *)

  val bool : bool t
  (** [bool] generates [true] or [false] with equal probability. [true] shrinks
      to [false]. *)

  val char : char t
  (** [char] generates a uniformly distributed byte: each of the 256 characters
      — the NUL byte ['\x00'] and bytes above 127 included — appears with
      probability 1/256. Candidates shrink toward ['a']. Use {!char_range} or
      {!of_list} for character subsets. *)

  val char_range : char -> char -> char t
  (** [char_range low high] generates a character in \[[low];[high]\] (byte
      order), uniformly. Candidates shrink toward the in-range character closest
      to ['a'] and stay in range: [char_range 'a' 'z'] shrinks toward ['a'],
      [char_range 'A' 'Z'] toward ['Z'], and [char_range '0' '9'] toward ['9'].

      Sampling raises [Invalid_argument] if [high < low]. *)

  val string : string t
  (** [string] is [string_of char]: a string whose length follows {!nat}'s
      distribution and whose characters follow {!char} — NUL and non-ASCII bytes
      included. Shrinking removes chunks of characters — the empty string is the
      first candidate — then shrinks characters individually toward ['a']. Use
      {!string_of} to control the length or character distribution. *)

  val string_of : ?size:int t -> char t -> string t
  (** [string_of ?size char] generates a string whose length follows [size]
      (default {!nat}) and whose characters are drawn from [char]. It always
      prints, as a quoted string. Shrinking follows {!list}'s rule.

      Sampling raises [Invalid_argument] if [size] produces a negative length.
  *)

  val bytes : bytes t
  (** [bytes] is [bytes_of char]: {!string} converted to [bytes] — same length
      distribution, uniform bytes, same shrinking. *)

  val bytes_of : ?size:int t -> char t -> bytes t
  (** [bytes_of char_gen] is {!string_of} converted to [bytes]: same length and
      alphabet control, same shrinking, and it keeps its printer. *)

  val list : ?size:int t -> 'a t -> 'a list t
  (** [list gen] generates a list of [gen] values whose length follows [size]
      (default {!nat}). With the default size, candidates first shrink the
      structure (the empty list, then removal of contiguous chunks of descending
      power-of-two length), then elements individually left to right. With an
      explicit [size], lengths follow [size]'s own candidates, so a constraint
      such as [~size:(int_range 2 5)] holds for every candidate.

      Sampling raises [Invalid_argument] if [size] produces a negative length.
  *)

  val array : ?size:int t -> 'a t -> 'a array t
  (** [array ?size gen] is [list ?size gen] converted to an array. *)

  val option : 'a t -> 'a option t
  (** [option gen] generates [None] with probability 0.15 and [Some] of a [gen]
      value otherwise. The first candidate of every [Some] is [None]; the
      payload then shrinks with [gen]. *)

  val result : 'a t -> 'e t -> ('a, 'e) result t
  (** [result ok err] generates [Ok] of an [ok] value with probability 0.75 and
      [Error] of an [err] value otherwise. Payloads shrink with their generator;
      a candidate never crosses constructors. *)

  val either : 'a t -> 'b t -> ('a, 'b) Either.t t
  (** [either left right] generates [Left] of a [left] value or [Right] of a
      [right] value with equal probability. Payloads shrink with their
      generator; a candidate never crosses constructors. *)

  val pair : 'a t -> 'b t -> ('a * 'b) t
  (** [pair a b] generates both components. Candidates shrink the left component
      first, then the right. *)

  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  (** [triple a b c] is like {!pair} for three components, shrinking
      left-to-right. *)

  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  (** [quad a b c d] is like {!pair} for four components, shrinking
      left-to-right. *)

  val constant : 'a -> 'a t
  (** [constant v] always generates [v], with no shrink candidates. It has no
      printer until {!with_pp} attaches one. *)

  val of_list : 'a list -> 'a t
  (** [of_list values] generates a value of [values], each with equal
      probability. Candidates shrink toward the head of [values], so order it
      simplest first. No printer until {!with_pp} attaches one.

      Sampling raises [Invalid_argument] if [values] is empty. *)

  val one_of : 'a t list -> 'a t
  (** [one_of gens] picks one generator from [gens] uniformly and generates with
      it. The choice shrinks toward earlier generators, re-generating from the
      same random capital and skipping a branch whose re-generation is rejected;
      the chosen value shrinks with its own generator. When every generator
      prints, a counterexample prints with the branch that drew it and an
      [~examples] value with the first branch's; otherwise the pre-image rule
      applies.

      Sampling raises [Invalid_argument] if [gens] is empty. *)

  val frequency : (int * 'a t) list -> 'a t
  (** [frequency weighted] picks a generator with probability proportional to
      its weight and generates with it. The choice itself does not shrink; the
      chosen value shrinks with its generator. Printing derives as in {!one_of}.

      Sampling raises [Invalid_argument] if [weighted] is empty, if any weight
      is negative, or if the weights sum to less than [1]. *)

  val such_that : ('a -> bool) -> 'a t -> 'a t
  (** [such_that p gen] generates [gen] values satisfying [p], re-sampling up to
      100 times; candidates are filtered by [p]. Keeps [gen]'s printer. If no
      draw satisfies [p], the case is discarded, as {!reject} discards one, and
      counts against the property's [max_discard]. For rare, cheap conditions;
      build structural constraints into the generator instead. *)

  (** {1:composition Composition} *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f gen] generates [f v] for [v] generated by [gen], shrinking wherever
      [gen] shrinks. No printer: a counterexample renders as its pre-image, [v]
      as [gen] renders it, until {!with_pp} attaches one. [f] must be pure. *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (** [bind gen f] generates [v] with [gen], then generates with [f v].
      Candidates first shrink [v], re-generating with [f] on the same random
      capital and skipping a rejected re-generation, then shrink the inner
      value. No printer: a counterexample renders as the inner value when [f v]
      prints and as the pre-image [v -> inner] otherwise. [f] must be pure. *)

  val with_pp : (Format.formatter -> 'a -> unit) -> 'a t -> 'a t
  (** [with_pp pp gen] is [gen] printing with [pp], the assertion vocabulary's
      printer type. An explicit printer wins over a pre-image, a derived printer
      or nothing. *)

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
(** [prop name gen law] declares a property test: [law] must hold for every
    value of [gen].

    - [timeout] bounds the whole property, generation and shrinking included.
      Expiring before any case failed fails the test as timed out; expiring
      during shrinking reports the best counterexample so far, marked as
      possibly not minimal.
    - [count] is the generated-case count; the declaration site wins over
      [--prop-count], which wins over the default of [100].
    - [max_discard] is how many discarded cases ({!assume}, {!reject}) the
      property tolerates before it gives up; defaults to twice the effective
      [count].
    - [examples] are explicit inputs run before any generation, unshrunk.

    Property tests carry the tag ["prop"] and take no [retries]: they replay
    deterministically from the root seed. A run prints its root seed when its
    selection holds any. *)

type ('model, 'sut) command
(** The type for one operation of a system under test: its argument generator,
    its precondition, its model transition and its body. Built with {!command}
    or {!val-call}. *)

val command :
  ?__POS__:pos ->
  ?pre:('model -> 'arg -> bool) ->
  string ->
  'arg Gen.t ->
  next:('model -> 'arg -> 'model) ->
  ('model -> 'arg -> 'sut -> unit) ->
  ('model, 'sut) command
(** [command name gen ~next body] declares an operation named [name] whose
    argument comes from [gen], which moves the model as [next] says and runs
    [body] against the system, asserting with the verbs. Every function takes
    the model first, then the argument, then (for [body]) the system; [body]
    sees the model before its own transition. [pre] (default always-legal) is
    whether the call is legal in the model: an operation is generated only in
    states where it holds. [next] is required; read-only operations pass
    [~next:Fun.const]. [__POS__] is the site a failing step points at.

    [pre] and [next] must be pure and ['model] persistent: the model trajectory
    is folded when the program is drawn, run and printed. One that raises is
    reported unshrunk with its backtrace and the step it raised at. *)

val call :
  ?__POS__:pos ->
  ?pre:('model -> bool) ->
  string ->
  next:('model -> 'model) ->
  ('model -> 'sut -> unit) ->
  ('model, 'sut) command
(** [call] is {!command} for an operation with no generated argument:
    [call "pop" ~pre ~next:List.tl body]. *)

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
(** [stateful name ~model ~scope commands] declares a test over sequences of
    [commands]: each case draws a program, runs it against a fresh system and
    checks it against the model. A failure reports the shrunk program one
    numbered step per line, the step that broke and the expected/actual diff.

    [scope] builds the system and reclaims it, with {!scoped}'s protocol; it
    runs once per generated case and once per shrink candidate. [invariant] runs
    on the fresh system before the first call and after every call. [pp_model]
    adds a column showing the model before each step. [steps] (default [20]) is
    how many calls are drawn per case; preconditions remove some. Shrinking
    removes calls and simplifies arguments, never substitutes an operation.

    Stateful tests carry the tags ["prop"] and ["stateful"] and, like {!prop},
    take no [retries]. *)

val assume : bool -> unit
(** [assume cond] discards the current case unless [cond] holds; discarded cases
    are counted and regenerated, and a property that discards more than its
    [max_discard] gives up and fails. *)

val reject : unit -> 'a
(** [reject ()] discards the current case (see {!assume}). *)

val collect : string -> unit
(** [collect label] marks the current case with [label]; the run reports the
    distribution of labels over passing cases, in every failing property's block
    and under [-v] for a passing one.

    Raises [Invalid_argument] when no property body is running. *)

val classify : string -> bool -> unit
(** [classify label cond] is [collect label] when [cond] holds, and [()]
    otherwise.

    Raises [Invalid_argument] when no property body is running. *)

val cover : string -> bool -> unit
(** [cover label cond] is {!classify}[ label cond] plus a demand: the property
    fails unless at least one passing case marked [label]. The demand registers
    wherever [cover] is written, even on a case where [cond] is false, so put it
    somewhere the body always reaches.

    Raises [Invalid_argument] when no property body is running. *)

(** {1:baselines Baselines}

    A baseline is a reviewed expectation the source names: the literal at an
    {!expect} or {!expect_exact} call, or the file an {!expect_file} call names,
    relative to the project root. Checking is read-only: a mismatch or a missing
    file records a failure with a diff (or, for a missing file, the proposed
    content) and the acceptance command, and the call returns, so one run
    reports every stale expectation. Under dune a [(test)] stanza runs the
    executable with [--corrected], which writes each correction as
    [<file>.corrected] beside dune's copy of the file, and diffs the two, so
    [dune promote] accepts:

    {[
      (test
       (name test_mylib)
       (libraries windtrap mylib)
       (deps help.expected)
       (action
        (progn
         (run %{test} --corrected)
         (diff? test_mylib.ml test_mylib.ml.corrected)
         (diff? help.expected help.expected.corrected))))
    ]}

    Without dune, [-u] rewrites the literals and files in place, atomically; it
    is refused under [CI]. A correction is written only for a test whose every
    failure is a baseline mismatch, and never for a test marked {!xfail}. The
    [expect] family takes the produced text first and the literal last. *)

val expect : string -> pos * string -> unit
(** [expect actual @@ __POS_OF__ {|…|}] compares [actual] with the literal
    whitespace-flexibly: lines trimmed, blank leading and trailing lines
    dropped, the block dedented. A mismatch records the test's failure with the
    diff and returns; the correction rewrites the literal, re-indented to its
    line. The literal's position is the compiler's, so a moved call keeps its
    baseline; a call shared by several tests must produce one text. *)

val expect_exact : string -> pos * string -> unit
(** [expect_exact actual @@ __POS_OF__ {|…|}] is {!expect} comparing byte for
    byte. *)

val expect_file : string -> string -> unit
(** [expect_file actual path] compares [actual] with the file at [path],
    relative to the project root, as line-oriented text: CR and CRLF read as LF
    and a final newline is forced on both sides. A mismatch records the test's
    failure and returns; a missing file is a mismatch whose correction is the
    file. Under dune, a [(diff? path path.corrected)] step makes the file an
    input of the action and promotes the correction onto it; promotion never
    creates a file, so a new one starts empty ([touch]) or is accepted once with
    [-u]. Content where CR bytes or the missing final newline matter must be
    encoded first.

    Raises if [path] cannot be proven to lie under the project root. *)

(** {1:capture Captured output} *)

val output : unit -> string
(** [output ()] consumes the current test's captured output: the bytes written
    to standard output and standard error (C stubs and subprocesses included)
    since the test started or since the previous [output ()] call. Under
    [--stream] there is no capture and the call fails the test with
    ["this test requires capture; rerun without --stream"]. Raises
    [Invalid_argument] outside a run. *)

(** {1:body The running test}

    Ambient operations for the test currently executing, in every phase (setup,
    body, teardown). Each raises [Invalid_argument] when no test is running. *)

val current_test : unit -> string list
(** [current_test ()] is the executing test's full path: enclosing group names
    root first, then the test's own name. Never empty; joined with [" › "] it is
    the string selection filters match. *)

val subtest : string -> (unit -> unit) -> unit
(** [subtest name fn] runs [fn ()] as a named sub-case of the executing test: a
    failure inside [fn] is recorded under the [" › "]-joined path of the test's
    name and the enclosing subtest names, and [subtest] returns, so later
    sub-cases still run and the test fails at the end with every recorded
    failure. Subtests nest. A {!skip} or a timeout aborts the whole test, and
    failures already recorded still fail it. Sub-cases are not selectable with
    [-f]; use {!cases} when they should be.

    {[
    test "backend contract" (fun () ->
        List.iter
          (fun (name, backend) -> subtest name (fun () -> check backend))
          backends)
    ]} *)

val temp_dir : ?prefix:string -> unit -> string
(** [temp_dir ()] is a fresh empty directory under the system temporary
    directory, removed after the test attempt on every outcome. [prefix] is the
    basename prefix (default ["dir"]). A resource that must outlive the test (a
    {!fixture}'s) must not live in it. *)

val temp_file : ?suffix:string -> unit -> string
(** [temp_file ()] is the path of a fresh empty file with the same lifecycle as
    {!temp_dir}; [suffix] is appended to the basename (e.g. [".json"]). *)

val setenv : string -> string option -> unit
(** [setenv name (Some value)] binds the environment variable [name] to [value]
    for the rest of the test; [setenv name None] unbinds it, so
    [Sys.getenv_opt name] is then [None]. The runner restores what [name] held
    before the test's first [setenv] of it when the attempt ends, on every
    outcome. The binding is process-global: threads and child processes see it,
    and a thread still moving at the end of the test races the restoration.

    {[
    test "reads the token from the environment" (fun () ->
        setenv "API_TOKEN" (Some "t-123");
        equal (option string) (Some "t-123") (Config.token ()))
    ]} *)

val chdir : string -> unit
(** [chdir dir] changes the working directory to [dir] for the rest of the test.
    The runner returns the process to the directory it was in before the test's
    first [chdir] when the attempt ends, on every outcome; if it cannot (the
    test deleted it), the test fails naming the directory. Process-global on the
    same terms as {!setenv}. Raises [Unix.Unix_error] when [dir] cannot be
    entered.

    {[
    test "builds in place" (fun () ->
        chdir (temp_dir ());
        Builder.run ();
        is_true (Sys.file_exists "output.txt"))
    ]} *)

(** {1:running Running} *)

val run : ?argv:string array -> string -> test list -> int
(** [run suite tests] parses the command line, executes the selected tests of
    [tests] in declaration order, renders the report to standard output and
    returns the exit code: [0] when no selected test failed, [1] on any failure,
    [2] when nothing ran or the command line does not parse; [--help] and
    [--version] print their page and return [0]. The caller passes the code to
    [exit]:

    {[
      let () = exit @@ run "mylib" [ ... ]
    ]}

    [argv] defaults to [Sys.argv]; [--help] lists the flags and their
    [WINDTRAP_*] environment mirrors. Beyond those, [run] reads only [CI],
    [GITHUB_ACTIONS], [INSIDE_DUNE] and whether standard output is a terminal.
    Duplicate test paths, focused tests under [CI] and [-u] under [CI] refuse
    the run before anything executes. Under [--corrected] a test whose failures
    are all recorded corrections leaves the exit code alone, and a selection
    that runs none of the suite's tests exits [0] rather than [2]; usage errors
    stay [2]. A [--corrected] run that wrote a correction and returns [1] warns
    on standard error, after its summary, that dune promotes a correction only
    from a run that exits [0]. See [doc/manual/running-tests.md] for the flags
    and the report.

    Raises [Invalid_argument] inside an active run. *)

(** {1:private Private} *)

(** Windtrap's own composition surface, for its test suite and its binary. Not
    part of the public API: no stability guarantee, and nothing here enters
    scope on [open Windtrap]. *)
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
