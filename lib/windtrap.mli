(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** One library for all your OCaml tests.

    Windtrap runs unit, property, stateful and expect tests, inline or not, from
    one flat surface: declare tests with {!test} and {!group}, assert with the
    assertion verbs ({!equal}, {!require_some}, {!raises}, ...), and hand the
    suite to {!run}:

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

    Comparisons go through an ['a] {!type:testable} — a printer and an equality
    — so every failure prints a structured diff of the two values. Witnesses for
    base types and containers are re-exported flat ({!int}, {!list}, {!pair},
    ...); their constructors stay behind {!Testable} ({!Testable.make},
    {!Testable.contramap}), which is what keeps those names out of every test
    file's scope.

    Property tests use {!prop} over an ['a] {!Gen.t} generator; shrinking is
    integrated and every failure prints a replay command. {!stateful} takes that
    to sequences of calls: it checks {!command}s against a model and shrinks a
    failure to a minimal program. Baselines ({!expect}, {!expect_file}) compare
    produced text with a literal in the source or a committed file and print
    their acceptance command on mismatch. Structure and resources: {!cases}
    declares one named test per input, {!bracket} scopes a resource the setup
    returns, {!scoped} one a callback receives and {!fixture} one shared by the
    run; {!temp_dir} and {!temp_file} give runner-cleaned scratch paths,
    {!setenv} and {!chdir} bind the environment and the working directory for
    one test with the runner restoring both; {!output} reads back the test's
    captured output; {!subtest} names sub-cases inside a body and {!val:xfail}
    keeps known-bug reproductions in-tree without a red run.

    The companion [ppx_windtrap] package adds inline expect tests
    ([let%expect_test] and its expect blocks), accepted through [dune promote],
    and two dune instrumentation backends: [ppx_windtrap.coverage] for
    expression coverage and [ppx_windtrap.mutate] for mutation testing, both
    reported by the [windtrap] command.

    The manual under [doc/manual/] is the long-form companion to this reference,
    with [doc/cookbook.md] for the recipes windtrap deliberately does not
    absorb; runnable projects live under [examples/]; the [CHANGES.md] 0.2.0
    entry maps the windtrap 0.1.x surface to this one. *)

(** {1:types Types} *)

type test = Test_tree.t
(** The type for declared tests: a leaf test or a named group of tests. Inert
    data — nothing runs until {!run} executes the suite. The representation (the
    internal declaration tree) is not part of the public contract. *)

type pos = string * int * int * int
(** The type of [__POS__] payloads: file, line, start column, end column. Every
    [?__POS__] below overrides the automatic call-stack location, which needs
    the program compiled with debug information ([-g], dune's default); the
    explicit form — [~__POS__], punning with the builtin — is for a helper that
    wraps a verb or a constructor, and for an assertion in tail position, whose
    frame is gone when it raises and which the report otherwise attributes to
    the enclosing test's declaration, saying so in one line under the location
    ([(assertion in tail position: its line is unknown; ~__POS__ names it)]).
    When no location is known at all, reports omit it rather than guess. *)

type 'a printer = Format.formatter -> 'a -> unit
(** The type for value printers: the one printer type used by testables,
    generators ({!Gen.with_pp}), and the [?pp] arguments of the shape
    assertions. *)

module Testable = Testable
(** Witness constructors: {!Testable.make}, {!Testable.structural}. See
    {!section-testables}. *)

type 'a testable = 'a Testable.t
(** The type for assertion witnesses: how {!equal} and kin compare values of
    type ['a] and render them in failure reports, and how {!less} and kin rank
    them. A witness is a printer and an equality, both total, and optionally an
    order; the equality is applied to [expected] first and [actual] second,
    which matters as soon as it is not symmetric — see {!Testable.make}. See
    {!section-testables}. *)

(** {1:declaring Declaring tests}

    Declaring is pure data construction: bodies run only when {!run} executes
    the tree, inside a per-test exception boundary. Five constructors build the
    tree — {!test}, {!group}, {!cases}, {!bracket} and {!scoped} — plus {!slow},
    which is {!test} with the ["slow"] tag; {!focus} and {!val:xfail} wrap a
    test or a group. Every constructor takes:

    - [__POS__], the declaration site (defaults to a best-effort call-stack
      capture; see {!type:pos});
    - [tags], extra tag names, unioned with the enclosing groups' tags; select
      with [--tag]/[--exclude-tag];
    - [timeout], the per-test limit in seconds, covering setup, body and
      teardown (defaults to the runner's [--timeout]). Setup and body share the
      window; teardown is then given whatever is left of it, or a fresh window
      if they consumed it — releasing a resource is not optional, so a body that
      times out still gets bounded cleanup rather than none. A {!scoped} test
      spends the window on the whole scope call, re-armed on the same terms when
      the body leaves the callback; a {!prop} spends it on generation and
      shrinking together;
    - [retries], the number of extra attempts a test gets while it counts as
      failed (defaults to [0]), each a fresh setup, body and teardown with a
      fresh capture; the run records how many attempts a test took.

    A group's [tags] extend every descendant's; its [timeout] and [retries] are
    defaults for every test under it, and the innermost declaration wins — a
    test's own value beats its group's, an inner group's its outer group's.
    Constructors raise [Invalid_argument] if [retries < 0] or if [timeout] is
    not finite and positive.

    A test is named by its {e path} — enclosing group names, then its own —
    rendered with [" › "] between components; filters match that string and
    duplicate full paths are a startup error. *)

val test :
  ?__POS__:pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  (unit -> unit) ->
  test
(** [test name fn] declares the test [name] with body [fn]. The body passes by
    returning and fails by raising — assertion verbs, or any exception. *)

val group :
  ?__POS__:pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  test list ->
  test
(** [group name children] declares a group. Groups nest freely; [name] becomes a
    path component, [tags] extend every descendant's tags, and [timeout] and
    [retries] are defaults for every test under it —
    [group ~timeout:30. "integration" [ ... ]] caps each test in the group that
    declares no limit of its own. Groups have no hooks: scope resources with
    {!bracket}, {!scoped} or {!fixture} instead, so no user code ever runs
    outside a test's exception boundary. *)

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
(** [cases ~name base inputs fn] declares one test per input: a group named
    [base] whose children, in declaration order, run [fn input] under the name
    [name input] — that is
    [group base (List.map (fun i -> test (name i) (fun () -> fn i)) inputs)],
    every child recording the [cases] call's declaration site. Each sub-test is
    individually selectable ([-f "ports parse › 8080"]) and one bad input does
    not mask the rest; [tags], [timeout] and [retries] sit on the group and
    reach every child — per input, not per table.

    {[
    cases "ports parse" ~name:Fun.id [ "1"; "80"; "8080"; "65535" ]
      (fun input -> ignore (require_ok (parse_port input)))
    ]}

    [~name] is required, and a positional [<base>.<i>] default is exactly what
    it is there to prevent: a child's path is its identity — it keys the child's
    per-case property seeds and its entry in the [--failed] store — so inserting
    a row at the front would silently re-key every row after it. The [inputs]
    list is evaluated at declaration time, outside any test: rows are data, so a
    row that needs test-scoped work ({!temp_dir}, {!setenv}, an assertion) is a
    {!subtest} inside one body instead. *)

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
    on it iff [setup] succeeded — on every outcome, including skip and timeout,
    except a fatal exception ([Sys.Break], [Out_of_memory], [Stack_overflow]),
    which skips the teardown and ends the run. It is {!scoped} over the scope
    that runs the three in that order, so its failures are attributed by
    {!scoped}'s phase rule: what [setup] raises is a [[setup]] failure, and a
    teardown failure is a [[teardown]] failure reported beside the body's — two
    entries, neither masking the other. Partial application builds reusable
    constructors:

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
(** [scoped scope name fn] declares a test whose resource is scoped by [scope]:
    a function that acquires a resource, hands it to a callback, and reclaims it
    when that callback returns — the shape most OCaml resources come in
    ([Eio_main.run], [Eio.Switch.run], [In_channel.with_open_text path],
    [Mutex.protect m]). The runner calls [scope] exactly once, with a callback
    that runs [fn]; {!bracket} is this with the scope written from a setup and a
    teardown, so write a scope by hand when the resource is callback-shaped.

    {[
    let with_eio = scoped Eio_main.run

    let tests =
      [
        with_eio "reads the config" (fun env ->
            let fs = Eio.Stdenv.fs env in
            equal string "{}" Eio.Path.(load (fs / "config.json")));
      ]
    ]}

    [scope] precedes the optional arguments so that a partially applied
    constructor keeps them: [with_eio ~timeout:30. "slow" fn] is well typed,
    because applying a positional argument erases only the optionals declared
    before it. The protocol: a failure raised by [fn] — an assertion, a {!skip},
    a timeout — is recorded and then re-raised through [scope], so a scope that
    cleans up on the exception path does so and one that swallows it cannot turn
    the test green; the callback must be called exactly once — a scope that
    returns without calling it fails the test, one that calls it twice runs the
    body on the first call only and fails the test; a scope that raises or skips
    instead of calling back reports as that failure or skip; and what [scope]
    raises {e before} the callback is a [[setup]] failure, {e after} the
    callback returned a [[teardown]] failure, reported beside the body's. A
    [timeout] covers the whole [scope] call and is re-armed as the body leaves
    the callback, so a release that blocks after a body timeout is cut short
    rather than left to hang the run. *)

(** {2:annotations Annotations}

    The two annotations wrap a declared test — its declaration site stays the
    one its constructor captured — and, on a group, reach every test under it:
    [focus (test "the broken one" (fun () -> ...))],
    [xfail ~reason:"issue #42" (group "parser" [ ... ])]. *)

val focus : test -> test
(** [focus t] focuses [t] — and, through a group, every test under it: when any
    focused test exists, only focused tests run. Focus is a debugging tool —
    when [CI] is set a run containing focused tests refuses to start, and
    outside CI a successful focused run prints a warning. *)

val xfail : ?reason:string -> test -> test
(** [xfail t] marks [t] — and, through a group, every test under it — as
    {e expected to fail}. Marked tests still run, but what counts as failed
    inverts: a failing outcome reports as an expected failure ([XFAIL]) without
    failing the run, while a passing outcome fails loudly
    (["expected to fail, but the test passed"]). Skips are unaffected. [reason]
    names the known defect for reports (e.g. ["issue #42"]); nested annotations
    resolve innermost-wins.

    Use [xfail] to keep a known-bug reproduction in-tree without a red run; use
    {!skip} when the body must not run at all. *)

val fixture : ?teardown:('a -> unit) -> (unit -> 'a) -> unit -> 'a
(** [fixture ?teardown create] is an accessor for a run-scoped shared resource.
    Creating the accessor (typically at module toplevel) runs nothing; the first
    call inside a test acquires with [create ()] — inside that test's failure
    boundary — and later calls return the cached value. Acquisition outcomes are
    cached for the whole run, failures included: a [create] that raises fails
    the acquiring test, every later call in the run re-raises that exception
    with the original acquisition backtrace, and nothing is registered for
    release. A [skip] raised during acquisition is cached as a skip, not an
    error — every test that touches the accessor skips with the same reason. A
    fixture no selected test touches is never acquired; acquired fixtures are
    released by the runner after the last test, in reverse acquisition order, on
    every path where the runner regains control (including [-x]). A release
    failure is reported and fails the run.

    Release runs {e outside} every per-test timeout: the tests are over, so
    there is no window to inherit and no limit to fall back on. A [teardown]
    that blocks hangs the run after the last result, where the same code under
    {!bracket} would be cut short by the test's limit — give a [teardown] that
    waits on the outside world its own deadline. Each release is announced
    before it runs, naming the fixture's declaration site, so a hang is
    attributable.

    Calling the accessor outside a run raises [Invalid_argument]. *)

(** {1:assertions Assertions}

    The assertion verbs and the {!Exn} predicates. Each verb raises one
    structured failure that the runner catches at the test boundary; the failure
    records the call site ([?__POS__], else a best-effort call-stack capture)
    and the optional [?msg] annotation. Expected precedes actual, always. An
    assertion failing outside any run surfaces as an ordinary uncaught exception
    rendered with the failure's one-line summary. *)

val equal : ?__POS__:pos -> ?msg:string -> 'a testable -> 'a -> 'a -> unit
(** [equal t expected actual] asserts that [expected] and [actual] are equal
    under [t]. The failure renders both values with [t]'s printer and the report
    shows their diff — for every type, not just strings. *)

val not_equal : ?__POS__:pos -> ?msg:string -> 'a testable -> 'a -> 'a -> unit
(** [not_equal t a b] asserts that [a] and [b] are {e not} equal under [t]. The
    failure prints the value once ([both sides equal: <v>]). *)

val less : ?__POS__:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** [less t ~than v] asserts that [v] is strictly below [than] under [t]'s
    order. The failure prints the bound and the value, both with [t]'s printer,
    where an [is_true (v < bound)] could only report [true] against [false]:

    {[
    less int ~than:3 (retries c)
    (* expected  less than 3
       actual    5 *)
    ]}

    The order is the witness's: the base-type witnesses carry their module's,
    {!Testable.structural} carries [Stdlib.compare], {!Testable.with_compare}
    gives one to any other witness, and {!Testable.contramap} orders through its
    projection. A witness without one — {!pass}, {!Testable.of_equal}, a plain
    {!Testable.make}, and every container witness — makes the assertion raise
    [Invalid_argument] naming the fix, whether or not it would have held.

    Tolerance belongs to equality and plays no part here: under [float 0.5],
    [1.0] and [1.2] are equal {e and} [1.0] is less than [1.2]. NaN sorts below
    every float, as in [Float.compare]; assert a NaN result with
    [equal float_exact]. *)

val at_most :
  ?__POS__:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** [at_most t ~than v] asserts that [v] is below or the same as [than] under
    [t]'s order; the failure reads [expected  at most <than>]. See {!less} for
    the order. *)

val greater :
  ?__POS__:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** [greater t ~than v] asserts that [v] is strictly above [than] under [t]'s
    order; the failure reads [expected  greater than <than>]. See {!less} for
    the order. *)

val at_least :
  ?__POS__:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** [at_least t ~than v] asserts that [v] is above or the same as [than] under
    [t]'s order; the failure reads [expected  at least <than>]. See {!less} for
    the order. A range is two assertions, each naming the bound it breaks:
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
(** [satisfies t pred v] asserts [pred v] — for a claim that is not an order: a
    parity, a shape, a domain predicate. The failure renders [v] with [t]'s
    printer — the data a bare {!is_true} would hide — against [claim], the
    sentence on the expected side (default ["value satisfying the predicate"]):

    {[
    satisfies ~claim:"a power of two" int (fun n -> n land (n - 1) = 0) n
    (* expected  a power of two
       actual    12 *)
    ]}

    [claim] describes [pred] and nothing checks that it does — keep the two next
    to each other. A comparison against a bound is {!less}, {!at_most},
    {!greater} or {!at_least}, whose claim is derived from the bound and cannot
    drift. [pred] must be total; the printer runs only on failure. *)

val starts_with : ?__POS__:pos -> ?msg:string -> affix:string -> string -> unit
(** [starts_with ~affix s] asserts that [s] begins with [affix]. The failure
    prints the affix and a bounded excerpt of [s], and when [affix] occurs
    elsewhere in [s] it says where — "not there at all" and "there, but not at
    the start" are different bugs. *)

val ends_with : ?__POS__:pos -> ?msg:string -> affix:string -> string -> unit
(** [ends_with ~affix s] asserts that [s] ends with [affix]. *)

val mem : ?__POS__:pos -> ?msg:string -> 'a testable -> 'a -> 'a list -> unit
(** [mem t x xs] asserts that [xs] has an element equal to [x] under [t]. The
    failure prints the element it wanted and the whole list — the data an
    [is_true (List.mem x xs)] would have thrown away. For a byte substring of a
    string, use {!contains}. *)

val is_none : ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> 'a option -> unit
(** [is_none o] asserts that [o] is [None]. On [Some v] the failure renders [v]
    with [pp] when given and as [<abstract>] otherwise.

    It takes a printer, not an ['a] {!type:testable}: the assertion never
    compares the value, and demanding a witness for a type it does not inspect
    is what turns call sites into [equal (option pass) None x]. *)

val is_some : ?__POS__:pos -> ?msg:string -> 'a option -> unit
(** [is_some o] asserts that [o] is [Some _] — {!require_some} for callers that
    want the assertion and not the value. No [?pp]: the failing side is [None].
*)

val is_ok :
  ?__POS__:pos -> ?msg:string -> ?pp:'e printer -> ('a, 'e) result -> unit
(** [is_ok r] asserts that [r] is [Ok _] — {!require_ok} for callers that want
    the assertion and not the value. On [Error e] the failure renders [e] with
    [pp] when given and as [<abstract>] otherwise. *)

val is_error :
  ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> ('a, 'e) result -> unit
(** [is_error r] asserts that [r] is [Error _] — {!require_error} for callers
    that want the assertion and not the value. On [Ok v] the failure renders [v]
    with [pp] when given and as [<abstract>] otherwise. *)

val contains : ?__POS__:pos -> ?msg:string -> sub:string -> string -> unit
(** [contains ~sub s] asserts that [s] contains [sub] as a byte substring (the
    empty needle is contained in every string). The failure prints the needle
    and a bounded excerpt of [s], never a bare [false]. *)

val not_contains : ?__POS__:pos -> ?msg:string -> sub:string -> string -> unit
(** [not_contains ~sub s] asserts that [s] does {e not} contain [sub] as a byte
    substring — so it always fails when [sub] is empty. The failure prints the
    needle, the byte offset of its first occurrence, and a bounded excerpt of
    [s] around it. *)

val in_order : ?__POS__:pos -> ?msg:string -> subs:string list -> string -> unit
(** [in_order ~subs s] asserts that each element of [subs] occurs in [s], each
    match beginning at or after the end of the previous element's match — the
    assertion for a log or a transcript, where the order is the claim and a
    chain of {!contains} calls would not check it:

    {[
    in_order ~subs:[ "connect"; "authenticate"; "disconnect" ] session_log
    ]}

    The failure names the element that broke the chain — its index and its value
    — and the byte the search had reached, over an excerpt of the region still
    to be matched. When that element {e is} in the string but before the cursor,
    the failure says so and marks it: "out of order" and "missing" are different
    bugs, and the first is the one you would otherwise read the whole string to
    find.

    Matches never re-use bytes, so [["aa"; "aa"]] needs four [a]s. An empty
    element matches without advancing. [subs] must be non-empty; an empty chain
    raises [Invalid_argument]. *)

val require_some : ?__POS__:pos -> ?msg:string -> 'a option -> 'a
(** [require_some o] asserts that [o] is [Some v] {e and unwraps}: the happy
    path keeps its value.

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
    [b] — {!require_some} for values that are not already options:

    {[
    let port = require_match (function Tcp p -> Some p | _ -> None) addr
    ]}

    On [None] the failure renders [v] with [pp] when given and as [<abstract>]
    otherwise; the printer runs only on failure. An exception raised by
    [extract] propagates unchanged — it is the test's failure, not a match
    failure. *)

val raises : ?__POS__:pos -> ?msg:string -> exn -> (unit -> 'a) -> unit
(** [raises e f] asserts that [f ()] raises an exception structurally equal to
    [e]. The failure distinguishes "nothing raised" from "raised a different
    exception", carries the raised exception's backtrace when the runtime
    recorded one, and — when the raised exception has the expected constructor
    but a different message ([Invalid_argument], [Failure], [Sys_error]) — reads
    as a message diff, not two near-identical renderings. Exceptions whose
    payloads structural equality cannot compare (functional values) need
    {!raises_match}. *)

val raises_match :
  ?__POS__:pos -> ?msg:string -> (exn -> bool) -> (unit -> 'a) -> unit
(** [raises_match pred f] asserts that [f ()] raises an exception satisfying
    [pred]; the failure prints the actually raised exception. [pred] must be
    total. {!Exn} provides the common predicates. *)

(** Exception predicates for {!raises_match}: constructor checks with an
    optional message constraint. Without [~substring] any message passes; with
    it the message must contain the given byte substring. A whole message is
    {!raises}' job — it holds both exceptions, so it reports a message diff
    where a predicate can only reject.

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
(** [fail msg] fails the current test with [msg]. It never returns — use it for
    branches the test must not reach. *)

val failf : ?__POS__:pos -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [failf fmt ...] is {!fail} with a [Format] message. *)

val skip : ?reason:string -> unit -> 'a
(** [skip ()] skips the current test — not a failure; the report shows the test
    as skipped with [reason]. A nonempty selection whose every test skipped
    still exits [0]: skips are deliberate. *)

(** {1:testables Testables}

    An ['a] {!type:testable} tells the equality assertions how to compare values
    of type ['a] and how to render them in failure reports. Witnesses for base
    types and containers are re-exported here flat, so
    [equal (list (pair string int))] reads without qualification; the
    constructors stay in {!Testable} — {!Testable.make} for a printer and an
    equality, {!Testable.with_compare} for the order the ordering verbs need (a
    module with the conventional trio is
    [Testable.make ~pp:M.pp ~equal:M.equal |> Testable.with_compare M.compare]),
    {!Testable.structural} for polymorphic equality and order under their own
    name, {!Testable.contramap} to compare, order and print through a
    projection, {!Testable.of_equal} for a type with no rendering. Diffing needs
    no support from the witness: reports compute diffs from the printed values,
    so every type gets highlighted diffs from its printer alone.

    The base-type witnesses below carry their module's order; the container
    witnesses carry none — an option or a list admits several, and a guessed one
    would be accepted silently — so an ordering assertion over one needs a
    witness given its order with {!Testable.with_compare}. *)

val unit : unit testable
val bool : bool testable
val char : char testable

val string : string testable
(** [string] prints with [%S]: quoted, escaped, on one line. *)

val text : string testable
(** [text] prints verbatim — no quotes, no escapes, newlines kept — so failures
    diff it line by line instead of showing two escaped one-liners with the
    difference buried in [\\n] soup. Use it for multi-line text (rendered
    output, serialized documents, logs) and {!string} for single-line values,
    where the quotes distinguish [""], [" "] and ["\t"]. *)

val bytes : bytes testable
val int : int testable
val int32 : int32 testable
val int64 : int64 testable
val nativeint : nativeint testable

val float_exact : float testable
(** [float_exact] compares floats bit for bit, with every NaN equal to every NaN
    — the witness to assert a NaN result with. {!Testable.float_exact} has the
    IEEE 754 details shared by the three float witnesses. *)

val float : float -> float testable
(** [float eps] compares with absolute tolerance: [a] and [b] are equal when
    [a = b] or [|a -. b| <= eps]. Raises [Invalid_argument] if [eps] is not
    strictly positive: exactness is spelled {!float_exact}. *)

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
(** [pass] considers all values equal, prints [<pass>], and carries no order —
    for ignoring a component of a composed witness, e.g. [pair string pass]. *)

(** {1:properties Properties}

    A property checks a law over generated inputs: {!prop} draws values from an
    ['a] {!Gen.t}, runs the body on each, and on failure shrinks the input to a
    minimal counterexample — shrinking is integrated; there is no shrink
    function to write, ever. Bodies return [unit] and use the ordinary assertion
    vocabulary, so an {!equal} failing inside a property reports its structured
    diff at the shrunk counterexample.

    Every generated value derives deterministically from the run's root seed,
    the test's path, and the case index, so the [s1:...] token printed in the
    run header replays every failure: the failure report prints the exact replay
    command for the way the run was invoked. *)

module Gen = Gen
(** The generator vocabulary:

    - numeric — {!Gen.int}, {!Gen.nat}, {!Gen.small_int}, {!Gen.int_range},
      {!Gen.int32}, {!Gen.int64}, {!Gen.nativeint}, {!Gen.float},
      {!Gen.float_range};
    - base — {!Gen.unit}, {!Gen.bool}, {!Gen.char}, {!Gen.char_range},
      {!Gen.string}, {!Gen.string_of}, {!Gen.bytes}, {!Gen.bytes_of};
    - containers — {!Gen.list}, {!Gen.array}, {!Gen.option}, {!Gen.result},
      {!Gen.either}, {!Gen.pair}, {!Gen.triple}, {!Gen.quad};
    - choice — {!Gen.constant}, {!Gen.of_list}, {!Gen.one_of}, {!Gen.frequency},
      {!Gen.such_that};
    - composition — {!Gen.map}, {!Gen.bind}, the binding operators, and
      {!Gen.with_pp}, which takes the same {!type:printer} the assertion side
      uses.

    A counterexample prints with its generator's printer; containers and choices
    derive one from their components. {!Gen.map} and {!Gen.bind} — so [let+],
    [and+] and [let*] — derive none, and a counterexample built through them
    renders as its {e pre-image}: the input the mapping functions received,
    printed by the generators that drew it, and marked as such in the report.
    Attach {!Gen.with_pp} to print the value itself, or to a {!Gen.constant} or
    {!Gen.of_list} leaf, which has nothing to print. See {!Gen} for each
    generator's distribution, shrink order, and printing. *)

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

    - [timeout] is the per-test limit in seconds (defaults to the runner's
      [--timeout]); it bounds the whole property — generation and shrinking
      included. A timeout that expires before any case has failed fails the test
      as timed out; one that expires during shrinking ends the search and
      reports the best counterexample found so far, marked as possibly not
      minimal.
    - [count] is the generated-case count; the declaration site wins over
      [--prop-count], which wins over the default of [100].
    - [max_discard] is how many discarded cases ({!assume}, {!reject}) the
      property tolerates before it {e gives up}; it defaults to twice the
      effective [count]. Raise it for a law whose precondition is genuinely rare
      — the discard rate is a fact about that law, which is why there is no
      run-wide knob for it — but a generator that produced the precondition by
      construction would not need the budget at all.
    - [examples] are explicit inputs run before any generation, unshrunk (they
      are already the reviewed minimal form) — the home for regressions worth
      keeping forever: [prop ~examples:[ Rect (2., 0.) ] ...].

    [prop] deliberately has no [retries]: a property replays deterministically
    from the root seed, so a retry would re-run the identical failing stream.

    Property tests carry the tag ["prop"] (so [--tag prop] selects them); the
    run header prints the root seed token when the suite declares any. *)

type ('model, 'sut) command
(** The type for one operation of a system under test: how to draw its argument,
    when it is legal, what it does to the model, and what it does to the system
    — four facts in one value, so adding an operation touches one place. Build
    with {!command} or {!val-call}. *)

val command :
  ?__POS__:pos ->
  ?pre:('model -> 'arg -> bool) ->
  string ->
  'arg Gen.t ->
  next:('model -> 'arg -> 'model) ->
  ('model -> 'arg -> 'sut -> unit) ->
  ('model, 'sut) command
(** [command name gen ~next body] declares an operation named [name] whose
    argument comes from [gen], which moves the model as [next] says, and which
    runs [body]. The body calls the real system and asserts with the ordinary
    verbs, so a result is produced and checked in one expression and never needs
    a type of its own.

    Every function takes the model first, then the argument, then (for [body])
    the system. [body] sees the {e pre-state} — the model before its own
    transition, which is what a postcondition needs.

    [pre] defaults to always-legal. It does not only exclude illegal calls, it
    {e selects} a state: an operation interesting only when a queue is full is
    generated only when the model says it is full. The converse is the rule to
    remember — a stateful test never exercises a call its own model forbids.

    [next] is required; read-only operations say so with [~next:Fun.const].

    [__POS__] is the command's declaration site, and it is what a failing step
    points at: a body is idiomatically one assertion in tail position, which
    leaves no frame to capture, so without it the step would report no location
    at all.

    [pre] and [next] must be pure, and ['model] must be persistent: the model
    trajectory is folded when the program is drawn, when it runs, and when a
    counterexample prints, and the three must agree. One that raises is a
    specification bug, reported unshrunk with its backtrace and the operation
    and step it raised at. *)

val call :
  ?__POS__:pos ->
  ?pre:('model -> bool) ->
  string ->
  next:('model -> 'model) ->
  ('model -> 'sut -> unit) ->
  ('model, 'sut) command
(** [call] is {!command} for an operation with no generated argument — most
    operations, in most APIs. [call "pop" ~pre ~next:List.tl body]. *)

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
(** [stateful name ~model ~scope commands] declares a test over {e sequences} of
    [commands]: each case draws a program, runs it against a fresh system, and
    checks it against the model. A failure reports the shrunk program one
    numbered step per line, the step that broke, and the ordinary
    expected/actual diff.

    [scope] builds that system and reclaims it, with {!scoped}'s protocol: it
    takes a callback, calls it exactly once, and releases on the way out whether
    the program passed or failed. It runs once per generated case
    {e and once per shrink candidate}: the search re-runs the program, so a
    shared system would make it meaningless, and a test-scoped {!temp_dir},
    {!setenv} or {!chdir} inside it leaks across cases — the scope mints and
    removes its own. The manual's stateful chapter has the worked examples.

    [invariant] runs on the fresh system before the first call and after every
    call. An operation whose body asserts nothing is checked only by it: bodies
    check what a call {e returns}, the invariant checks what the state {e is}.

    [pp_model] adds a column showing the model before each step — the state the
    call was made in.

    [steps] is how many calls are {e drawn} per case (default [20]);
    preconditions remove some, so a program has at most [steps] calls. Shrinking
    removes calls and simplifies their arguments; it never substitutes one
    operation for another. Cost scales with [steps] and [count] and, on a
    failing test, with the shrink search.

    Stateful tests carry the tags ["prop"] and ["stateful"], so [--tag prop] and
    [--tag stateful] both select them, and — like {!prop} — they have no
    [retries]: a program replays deterministically from the root seed. *)

val assume : bool -> unit
(** [assume cond] discards the current case unless [cond] holds; discarded cases
    are counted and regenerated, and a property that discards too much
    {e gives up} and fails. Discarding is for rare, cheap preconditions — when
    the precondition is structural, constrain the generator instead
    ({!Gen.such_that}, or a generator correct by construction). *)

val reject : unit -> 'a
(** [reject ()] unconditionally discards the current case (see {!assume}). *)

val collect : string -> unit
(** [collect label] marks the current case with [label]; the run reports the
    distribution of labels over passing cases — the tool for checking that a
    generator exercises the interesting regions. A failing property's block
    always includes the distribution; a passing property's prints under
    [--verbose] ([-v]) — run verbose to calibrate, then drop back to the
    one-line transcript.

    Raises [Invalid_argument] when no property body is running. *)

val classify : string -> bool -> unit
(** [classify label cond] is [collect label] when [cond] holds, and [()]
    otherwise.

    Raises [Invalid_argument] when no property body is running. *)

val cover : string -> bool -> unit
(** [cover label cond] is {!classify}[ label cond] plus a demand: the property
    fails unless at least one passing case marked [label]. It is the CI gate on
    generator quality — [classify] prints a distribution a human reads under
    [-v], so a generator that stops reaching the interesting region is otherwise
    silent.

    Presence, not proportion: "this region is reached at all" is the question
    that catches a generator regression, and a percentage gate over a random
    sample flakes near its threshold. The demand registers wherever [cover] is
    written, even on a case where [cond] is false, so put it somewhere the body
    always reaches — a [cover] inside the branch it is meant to police is
    vacuous exactly when it should fire.

    Raises [Invalid_argument] when no property body is running. *)

(** {1:baselines Baselines}

    A baseline is a reviewed expectation the source names: the literal at an
    {!expect} or {!expect_exact} call, or the file an {!expect_file} call names,
    relative to the project root. Checking is read-only: a mismatch or a missing
    file records a failure with a diff (or the proposed content) and the
    acceptance command for the way the run was invoked, and the call returns — a
    checkpoint, not an assertion: where a failed assertion ends the body, a
    mismatch lets it continue, so one run reports every stale expectation and
    one acceptance takes them all. Under dune a [(test)] stanza runs the
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

    Without dune, [-u] rewrites the literals and files in place, atomically, for
    review with [git diff]; it is refused under [CI]. A correction is written
    only for a test whose every failure is a baseline mismatch: an assertion
    failure or a raise beside one withholds it until it is fixed, and a test
    marked {!xfail} never records one, its mismatch being the failure it
    expects. The [expect] family takes the produced text first and the literal
    last, so a [{|…|}] block reads as a block; {!equal} and the assertion verbs
    stay expected-first. *)

val expect : string -> pos * string -> unit
(** [expect actual @@ __POS_OF__ {|…|}] compares [actual] with the literal
    whitespace-flexibly: lines trimmed, blank leading and trailing lines
    dropped, the block dedented. A mismatch records the test's failure with the
    diff and returns; it is corrected by rewriting the literal, re-indented to
    its line. The literal's position is what the compiler recorded for the call,
    so a moved call cannot orphan its baseline; a call shared by several tests
    (a [cases] family) must produce one text, or fail. *)

val expect_exact : string -> pos * string -> unit
(** [expect_exact actual @@ __POS_OF__ {|…|}] is {!expect} comparing byte for
    byte. *)

val expect_file : string -> string -> unit
(** [expect_file actual path] compares [actual] with the file at [path],
    relative to the project root, as line-oriented text: CR and CRLF read as LF
    and a final newline is forced on both sides. A mismatch records the test's
    failure and returns; a missing file is a mismatch whose correction is the
    file. A path that cannot be proven to lie under the project root raises
    instead, since nothing after it is meaningful. Under dune, a
    [(diff? path path.corrected)] step in the stanza makes the file an input of
    the action — the run reads dune's copy of it — and promotes the correction
    onto it; promotion fills a file but never creates one, so a new file starts
    empty ([touch]) or is accepted once with [-u]. Content where CR bytes or the
    missing final newline are significant must be encoded first (e.g.
    [String.escaped]); redaction is ordinary code applied before the call. *)

(** {1:capture Captured output} *)

val output : unit -> string
(** [output ()] consumes the current test's captured output: the bytes written
    to standard output and standard error (C stubs and subprocesses included)
    since the test started or since the previous [output ()] call. Use it to
    assert on printed output — [equal string "hello\n" (output ())] — or feed it
    to {!expect}.

    Under [--stream] there is no captured output; the call fails the test with
    "this test requires capture; rerun without --stream" instead of comparing
    against silence. Raises [Invalid_argument] outside a run. *)

(** {1:body The running test}

    Ambient operations for the test currently executing, available in every
    phase (setup, body, teardown). Each raises [Invalid_argument] when no test
    is running. *)

val current_test : unit -> string list
(** [current_test ()] is the executing test's full path: enclosing group names
    root first, then the test's own name. Never empty; joined with [" › "] it is
    exactly the string selection filters match. Use it to key artifacts by test
    identity instead of duplicating the test's name by hand. *)

val subtest : string -> (unit -> unit) -> unit
(** [subtest name fn] runs [fn ()] as a named sub-case of the executing test: a
    failure inside [fn] is recorded, labeled with the [" › "]-joined path of the
    test's name and the enclosing subtest names, and [subtest] {e returns} — so
    sibling sub-cases after a failing one still run, and the test fails at the
    end with every recorded failure. Subtests nest; labels compose
    ([test › outer › inner]).

    {[
    test "backend contract" (fun () ->
        List.iter
          (fun (name, backend) -> subtest name (fun () -> check backend))
          backends)
    ]}

    A {!skip} and a timeout abort the whole test (failures already recorded
    still fail it). Sub-cases are failure labels, not tests: they are not
    separately selectable with [-f] — reach for {!cases} when they should be. *)

val temp_dir : ?prefix:string -> unit -> string
(** [temp_dir ()] is a fresh empty directory owned by the runner: created under
    the system temporary directory and removed after the test on every outcome —
    failure, skip, and timeout included — so tests never hand-roll temp
    lifecycles. Each call returns a new directory; [prefix] is the basename
    prefix (defaults to ["dir"]). Paths are per test attempt: a resource that
    must outlive the test (one acquired by a {!fixture}) must not live in them.
*)

val temp_file : ?suffix:string -> unit -> string
(** [temp_file ()] is the path of a fresh empty file with the same lifecycle as
    {!temp_dir}; [suffix] is appended to the basename (e.g. [".json"]). *)

val setenv : string -> string option -> unit
(** [setenv name (Some value)] binds the environment variable [name] to [value]
    for the rest of the test; [setenv name None] unbinds it. The runner puts
    [name] back the way it found it when the test ends, on every outcome —
    failure, skip, and timeout included, and per attempt under [?retries].

    {[
    test "reads the token from the environment" (fun () ->
        setenv "API_TOKEN" (Some "t-123");
        equal (option string) (Some "t-123") (Config.token ()))
    ]}

    The unbinding is a real one: [Sys.getenv_opt name] answers [None]
    afterwards, not [Some ""], which is what makes [setenv name None] usable to
    test the code path a missing variable takes. What gets restored is what
    [name] held before the test's {e first} [setenv] of it, so binding a
    variable twice still leaves behind what the test found.

    {b Process-global.} The environment is the process's, so the binding is
    visible to every thread the test spawns and to every child process it starts
    — and a test that changes the environment from a spawned thread races the
    runner's restoration. Windtrap runs tests sequentially in one domain, so
    tests never race {e each other} here; threads within one test are the
    caller's to order. *)

val chdir : string -> unit
(** [chdir dir] changes the working directory to [dir] for the rest of the test.
    The runner returns the process to the directory it was in at the test's
    first [chdir] when the test ends, on every outcome, per attempt.

    {[
    test "builds in place" (fun () ->
        chdir (temp_dir ());
        Builder.run ();
        is_true (Sys.file_exists "output.txt"))
    ]}

    Process-global on the same terms as {!setenv}: threads and child processes
    see it, and the restoration is not ordered against a thread still moving.

    If the directory cannot be restored — the test deleted it — the test fails
    with a message naming it, rather than leaving every later test to run from
    somewhere unexpected. Raises [Unix.Unix_error] when [dir] itself cannot be
    entered. *)

(** {1:running Running} *)

val run : ?argv:string array -> string -> test list -> int
(** [run suite tests] parses the command line, executes the selected tests of
    [tests] in declaration order, renders the report to standard output and
    returns the exit code: [0] when no selected test failed, [1] on any failure,
    [2] when nothing ran (the filter-typo case) or the command line does not
    parse; [--help] and [--version] print their page and return [0]. The caller
    passes the code to [exit]:

    {[
      let () = exit @@ run "mylib" [ ... ]
    ]}

    [argv] is the command line, [Sys.argv] by default; [--help] lists its flags
    and the [WINDTRAP_*] environment mirrors that stand in for them under
    [dune runtest]: every flag that changes what a run does or reports has one,
    and [-l], [--failed], [-x], [-u], [--corrected], [-h] and [-V] have none.
    Beyond those, [run] reads only [CI], [GITHUB_ACTIONS], [INSIDE_DUNE] and
    whether standard output is a terminal. Raises [Invalid_argument] inside an
    active run: a test body cannot start another run.

    Duplicate test paths, focused tests under [CI] and [-u] under [CI] refuse
    the run before anything executes. Under [--corrected] a test whose failures
    are all recorded corrections leaves the exit code alone, and a selection
    that runs none of the suite's tests exits [0] rather than [2]: the run is a
    build action's, whose [WINDTRAP_*] selection spans every stanza and
    partition of the tree, and the [diff?] that follows is the verdict.
    [--shard K/N] partitions the selected tests into [N] buckets by a frozen
    hash of each test's path, so the buckets cover every test exactly once,
    stable across machines and suite composition. Code under test that calls
    [exit] does not end the run: the call is intercepted and recorded as that
    test's failure. A green run prints one line; a run with something to show
    prints its header, then the failure blocks, the slow and flaky blocks and
    the summary; [-v] streams one line per test; see
    [doc/manual/running-tests.md]. *)

(** {1:private Private} *)

(** Internal machinery — windtrap's own composition surface, re-exported for the
    library's per-module test suites (the [test/] directories). Not part of the
    public API: these interfaces move without notice and carry no stability
    guarantee. Everything user-facing is the documented surface above; nothing
    here escapes into scope on [open Windtrap]. *)
module Private : sig
  module Atomic_file = Atomic_file
  module Baseline = Baseline
  module Capture = Capture
  module Check = Check
  module Cli = Cli
  module Clock = Clock
  module Diff = Diff
  module Env = Env
  module Failure = Failure
  module Loc = Loc
  module Mutate_loop = Mutate_loop
  module Path_ops = Path_ops
  module Pp = Pp
  module Property = Property
  module Report = Report
  module Report_junit = Report_junit
  module Report_sections = Report_sections
  module Run = Run
  module Seed = Seed
  module Shrink_tree = Shrink_tree
  module Source_patch = Source_patch
  module Stateful = Stateful
  module Tag = Tag
  module Test_tree = Test_tree
  module Text = Text
end
