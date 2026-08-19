(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** One library for all your OCaml tests.

    Windtrap runs unit, property, and snapshot tests from one flat surface:
    declare tests with {!test} and {!group}, assert with the Twenty-seven verbs
    ({!equal}, {!require_some}, {!raises}, ...), and hand the suite to {!run}:

    {[
      open Windtrap

      let () =
        run "mylib"
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
    ...); build your own with {!Testable.make}.

    Property tests use {!prop} over an ['a] {!Gen.t} generator; shrinking is
    integrated and every failure prints a replay command. Snapshot tests
    ({!snapshot}) compare against committed baselines under [__snapshots__/] and
    print their acceptance command on mismatch. Resources are scoped by
    {!bracket} (one the setup returns), {!scoped} (one a callback receives) and
    {!fixture} (one shared by the run); {!temp_dir} and {!temp_file} give
    runner-cleaned scratch paths, and {!setenv} and {!chdir} bind the
    environment and the working directory for one test with the runner
    restoring both; {!output} reads back the test's captured output;
    {!subtest} names sub-cases inside a body and {!val:xfail} keeps known-bug
    reproductions in-tree without a red run.

    Runnable examples for each feature live under [examples/] in the
    distribution; [doc/cookbook.md] collects the recipes windtrap deliberately
    does not absorb; the [CHANGES.md] 0.2.0 entry maps the windtrap 1.x surface
    to this one. *)

(** {1:types Types} *)

type test = Test_tree.t
(** The type for declared tests: a leaf test or a named group of tests. Inert
    data — nothing runs until {!run} executes the suite. The representation (the
    internal declaration tree) is not part of the public contract. *)

type pos = string * int * int * int
(** The type of [__POS__] payloads: file, line, start column, end column. Every
    [?pos] below overrides the best-effort call-stack location. Without [?pos],
    a failure names the failing call's own line when that call's frame is still
    on the stack; when it is not — a call in tail position leaves no frame — the
    failure is attributed to the enclosing test's declaration instead, and when
    even that is unknown, reports omit the location rather than guess. *)

type 'a printer = Format.formatter -> 'a -> unit
(** The type for value printers: the one printer type used by testables,
    generators ({!Gen.with_pp}), {!snapshot_pp}, and the [?pp_error]/[?pp_ok]
    arguments. *)

module Testable = Testable
(** Witness constructors: {!Testable.make}, {!Testable.structural}. See
    {!section-testables}. *)

type 'a testable = 'a Testable.t
(** The type for assertion witnesses: how {!equal} and kin compare values of
    type ['a] and render them in failure reports. A witness is a printer and an
    equality, both total; the equality is applied to [expected] first and
    [actual] second, which matters as soon as it is not symmetric — see
    {!Testable.make}. See {!section-testables}. *)

(** {1:declaring Declaring tests}

    Declaring is pure data construction: bodies run only when {!run} executes
    the tree, inside a per-test exception boundary. Constructor arguments common
    to several constructors:

    - [pos] records the declaration site (defaults to a best-effort call-stack
      capture); it also scopes {!snapshot} baselines declared in the test.
    - [tags] name extra tags, unioned with enclosing groups' tags; select with
      [--tag]/[--exclude-tag].
    - [timeout] is the per-test limit in seconds, covering setup, body, and
      teardown (defaults to the runner's [--timeout]). Setup and body share the
      window; teardown is then given whatever is left of it, or a fresh window
      if they consumed it — releasing a resource is not optional, so a body that
      times out still gets bounded cleanup rather than none. A {!scoped} test
      spends the window on the whole scope call, re-armed on the same terms when
      the body leaves the callback.
    - [retries] is the number of extra attempts given to a failing test
      (defaults to [0]).

    A test is named by its {e path} — enclosing group names, then its own —
    rendered with [" › "] between components; filters match that string and
    duplicate full paths are a startup error. *)

val test :
  ?pos:pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  (unit -> unit) ->
  test
(** [test name fn] declares the test [name] with body [fn]. The body passes by
    returning and fails by raising — assertion verbs, or any exception. *)

val group : ?pos:pos -> ?tags:string list -> string -> test list -> test
(** [group name children] declares a group. Groups nest freely; [name] becomes a
    path component and [tags] extend every descendant's tags. Groups have no
    hooks: scope resources with {!bracket}, {!scoped} or {!fixture} instead, so
    no user code ever runs outside a test's exception boundary. *)

val ftest :
  ?pos:pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  (unit -> unit) ->
  test
(** [ftest] is {!test} with the focus flag set: when any focused test or group
    exists, only focused tests run. Focus is a debugging tool — when [CI] is set
    a run containing focused tests refuses to start (unless
    [WINDTRAP_ALLOW_FOCUS=1]), and outside CI a successful focused run prints a
    warning. *)

val fgroup : ?pos:pos -> ?tags:string list -> string -> test list -> test
(** [fgroup] is {!group} with the focus flag set: every test under it is focused
    (see {!ftest}). *)

val slow :
  ?pos:pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  (unit -> unit) ->
  test
(** [slow] is {!test} with the ["slow"] tag pre-applied; [--quick] drops
    slow-tagged tests. *)

val cases :
  ?pos:pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  ?name:('a -> string) ->
  string ->
  'a list ->
  ('a -> unit) ->
  test
(** [cases name inputs fn] declares one test per input: a group named [name]
    whose [i]th child (zero-based, declaration order) runs [fn input]. A child
    is named by applying the [?name] function to its input — at declaration time
    — when given, and ["<name>.<i>"] otherwise; either way each sub-test is
    individually selectable ([-f "name › 8080"]) and one bad input does not mask
    the rest. [timeout] and [retries] apply to each child — per input, not per
    table.

    The [inputs] list is evaluated at {e declaration} time, outside any test:
    rows are data, not test code. A row that needs test-scoped work —
    {!temp_dir}, {!setenv}, an assertion, IO against the system under test —
    cannot be a row; keep the list pure and do per-input work inside [fn]. A
    table whose rows must be computed inside a test does not convert to
    [cases]: use {!subtest} within one body instead. *)

val xfail : ?reason:string -> test -> test
(** [xfail t] marks [t] — and, through a group, every test under it — as
    {e expected to fail}. Marked tests still run, but what counts as failed
    inverts: a failing outcome reports as an expected failure ([XFAIL]) without
    failing the run, while a passing outcome fails loudly
    (["expected to fail, but the test passed"]). Skips are unaffected. [reason]
    names the known defect for reports (e.g. ["issue #42"]); nested annotations
    compose innermost-wins.

    Use [xfail] to keep a known-bug reproduction in-tree without a red run; use
    {!skip} when the body must not run at all. *)

val bracket :
  ?pos:pos ->
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
    on it iff [setup] succeeded — on every outcome, including skip and timeout.
    A body failure and a teardown failure are reported independently; neither
    masks the other. Partial application builds reusable constructors:

    {[
      let with_db = bracket ~setup:Db.connect ~teardown:Db.close
      let tests = [ with_db "count" (fun db -> equal int 0 (Db.count db)) ]
    ]} *)

val scoped :
  (('r -> unit) -> unit) ->
  ?pos:pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  ('r -> unit) ->
  test
(** [scoped scope name fn] declares a test whose resource is scoped by [scope]:
    a function that acquires a resource, hands it to a callback, and reclaims it
    when that callback returns. It is the shape most OCaml resources come in —
    [Eio_main.run], [Eio.Switch.run], [In_channel.with_open_text path],
    [Mutex.protect m] — and the one {!bracket} cannot express, because between
    the acquire and the release there is no moment at which a resource can be
    {e returned}.

    {[
      let with_eio = scoped Eio_main.run

      let tests =
        [
          with_eio "reads the config" (fun env ->
              let fs = Eio.Stdenv.fs env in
              equal string "{}" Eio.Path.(load (fs / "config.json")));
        ]
    ]}

    [scope] is positional and comes {e before} the optional arguments, so that a
    partially applied constructor keeps them: [with_eio ~timeout:30. "slow" fn]
    is well typed, because applying a positional argument erases only the
    optionals declared before it.

    The runner calls [scope] exactly once, and that call is all it does — which
    makes the contract differ from {!bracket}'s on four points:

    - {b Cleanup is [scope]'s, not windtrap's.} {!bracket} guarantees [teardown]
      on every outcome; here windtrap guarantees nothing, because it never sees
      the resource. A failure raised by [fn] — an assertion, a {!skip}, a
      timeout — is recorded and then re-raised through [scope], so a scope that
      cancels or cleans up on the exception path does so. Whether it does is
      [scope]'s contract: [Eio_main.run] and anything built on [Fun.protect]
      release on both paths, while [let r = acquire () in fn r; release r] leaks
      whenever the body fails.
    - {b The callback must be called exactly once.} A [scope] that returns
      without calling it fails the test — a body that never ran is not a pass,
      and silently green would be the worst outcome available here. A [scope]
      that calls it twice runs the body on the first call only and fails the
      test: one execution per test is what {!snapshot} registration, {!subtest}
      labels and {!temp_dir} paths are keyed by. To repeat a body, use {!cases}
      or [~retries].
    - {b A [scope] that raises or skips instead of calling back} reports as that
      failure or that skip, not as a missing body — the pattern for a suite
      gated on a resource the machine does not have.
    - {b Failures are attributed by how far the callback got.} What [fn] raises
      is the body's own failure. What [scope] raises {e before} the callback is
      a [[setup]] failure and what it raises {e after} the callback returned is
      a [[teardown]] failure, so a scope that cannot acquire reads differently
      from one that cannot release. A release failure that replaces the body's
      exception is reported alongside it — two entries, as under {!bracket}.

    [timeout] covers the whole [scope] call, acquisition and release included;
    the runner re-arms the window when the body leaves the callback, so a
    release that blocks after a body timeout is cut short rather than left to
    hang the run. *)

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
    every path where the runner regains control (including [--bail]). A release
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

    Twenty-seven verbs and the {!Exn} predicates. Each verb raises one
    structured failure that the runner catches at the test boundary; the
    failure records the call site ([?pos], else a best-effort call-stack
    capture) and the optional [?msg] annotation. Expected precedes actual,
    always. An assertion failing outside any run surfaces as an ordinary
    uncaught exception rendered with the failure's one-line summary. *)

val equal : ?pos:pos -> ?msg:string -> 'a testable -> 'a -> 'a -> unit
(** [equal t expected actual] asserts that [expected] and [actual] are equal
    under [t]. The failure renders both values with [t]'s printer and the report
    shows their diff — for every type, not just strings. *)

val not_equal : ?pos:pos -> ?msg:string -> 'a testable -> 'a -> 'a -> unit
(** [not_equal t a b] asserts that [a] and [b] are {e not} equal under [t]. The
    failure prints the value once ([both sides equal: <v>]). *)

val is_true : ?pos:pos -> ?msg:string -> bool -> unit
(** [is_true b] asserts [b]. *)

val is_false : ?pos:pos -> ?msg:string -> bool -> unit
(** [is_false b] asserts [not b]. *)

val satisfies :
  ?pos:pos ->
  ?msg:string ->
  ?claim:string ->
  'a testable ->
  ('a -> bool) ->
  'a ->
  unit
(** [satisfies t pred v] asserts [pred v]. The failure renders [v] with [t]'s
    printer — the data a bare {!is_true} would hide — against [claim], the
    sentence on the expected side (default ["value satisfying the predicate"]):

    {[
      satisfies ~claim:"greater than 0" int (fun n -> n > 0) n
      (* expected  greater than 0
         actual    0 *)
    ]}

    That is the shape a comparison assertion takes: [is_true (n > 0)] consumes
    both numbers into a boolean and can only report [true] against [false],
    where a claim keeps the bound and the value keeps the value. [claim]
    describes [pred] and nothing checks that it does — keep the two next to
    each other. [pred] must be total; the printer runs only on failure. *)

val starts_with : ?pos:pos -> ?msg:string -> affix:string -> string -> unit
(** [starts_with ~affix s] asserts that [s] begins with [affix]. The failure
    prints the affix and a bounded excerpt of [s], and when [affix] occurs
    elsewhere in [s] it says where — "not there at all" and "there, but not at
    the start" are different bugs. *)

val ends_with : ?pos:pos -> ?msg:string -> affix:string -> string -> unit
(** [ends_with ~affix s] asserts that [s] ends with [affix]. *)

val is_none : ?pos:pos -> ?msg:string -> ?pp:'a printer -> 'a option -> unit
(** [is_none o] asserts that [o] is [None]. On [Some v] the failure renders [v]
    with [pp] when given and as [<abstract>] otherwise.

    It takes a printer, not an ['a] {!type:testable}: the assertion never
    compares the value, and demanding a witness for a type it does not inspect
    is what turns call sites into [equal (option pass) None x]. *)

val contains : ?pos:pos -> ?msg:string -> sub:string -> string -> unit
(** [contains ~sub s] asserts that [s] contains [sub] as a byte substring (the
    empty needle is contained in every string). The failure prints the needle
    and a bounded excerpt of [s], never a bare [false]. *)

val not_contains : ?pos:pos -> ?msg:string -> sub:string -> string -> unit
(** [not_contains ~sub s] asserts that [s] does {e not} contain [sub] as a byte
    substring — so it always fails when [sub] is empty. The failure prints the
    needle, the byte offset of its first occurrence, and a bounded excerpt of
    [s] around it. *)

val in_order : ?pos:pos -> ?msg:string -> subs:string list -> string -> unit
(** [in_order ~subs s] asserts that each element of [subs] occurs in [s], each
    match beginning at or after the end of the previous element's match — the
    assertion for a log or a transcript, where the order is the claim and a
    chain of {!contains} calls would not check it:

    {[
      in_order ~subs:[ "connect"; "authenticate"; "disconnect" ] session_log
    ]}

    The failure names the element that broke the chain — its index and its
    value — and the byte the search had reached, over an excerpt of the region
    still to be matched. When that element {e is} in the string but before the
    cursor, the failure says so and marks it: "out of order" and "missing" are
    different bugs, and the first is the one you would otherwise read the whole
    string to find.

    Matches never re-use bytes, so [["aa"; "aa"]] needs four [a]s. An empty
    element matches without advancing. [subs] must be non-empty; an empty chain
    raises [Invalid_argument]. *)

val require_some : ?pos:pos -> ?msg:string -> 'a option -> 'a
(** [require_some o] asserts that [o] is [Some v] {e and unwraps}: the happy
    path keeps its value.

    {[
      let user = require_some (Store.find store "alice") in
      equal string "alice" user.name
    ]} *)

val require_ok :
  ?pos:pos -> ?msg:string -> ?pp_error:'e printer -> ('a, 'e) result -> 'a
(** [require_ok r] asserts that [r] is [Ok v] and returns [v]. On [Error e] the
    failure renders [e] with [pp_error] when given and as [<abstract>]
    otherwise. *)

val require_error :
  ?pos:pos -> ?msg:string -> ?pp_ok:'a printer -> ('a, 'e) result -> 'e
(** [require_error r] asserts that [r] is [Error e] and returns [e]. On [Ok v]
    the failure renders [v] with [pp_ok] when given and as [<abstract>]
    otherwise. *)

val require_match :
  ?pos:pos -> ?msg:string -> ?pp:'a printer -> ('a -> 'b option) -> 'a -> 'b
(** [require_match extract v] asserts that [extract v] is [Some b] and returns
    [b] — {!require_some} for values that are not already options:

    {[
      let port = require_match (function Tcp p -> Some p | _ -> None) addr
    ]}

    On [None] the failure renders [v] with [pp] when given and as [<abstract>]
    otherwise; the printer runs only on failure. An exception raised by
    [extract] propagates unchanged — it is the test's failure, not a match
    failure. *)

val raises : ?pos:pos -> ?msg:string -> exn -> (unit -> 'a) -> unit
(** [raises e f] asserts that [f ()] raises an exception structurally equal to
    [e]. The failure distinguishes "nothing raised" from "raised a different
    exception", carries the raised exception's backtrace when the runtime
    recorded one, and — when the raised exception has the expected constructor
    but a different message ([Invalid_argument], [Failure], [Sys_error]) — reads
    as a message diff, not two near-identical renderings. Exceptions whose
    payloads structural equality cannot compare (functional values) need
    {!raises_match}. *)

val raises_match :
  ?pos:pos -> ?msg:string -> (exn -> bool) -> (unit -> 'a) -> unit
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
  (** [invalid_arg e] is [true] iff [e] is [Invalid_argument m] and [m]
      contains [substring], if given. *)

  val failure : ?substring:string -> exn -> bool
  (** [failure e] is [true] iff [e] is [Failure m] and [m] contains
      [substring], if given. *)

  val sys_error : ?substring:string -> exn -> bool
  (** [sys_error e] is [true] iff [e] is [Sys_error m] and [m] contains
      [substring], if given. *)
end

val fail : ?pos:pos -> string -> 'a
(** [fail msg] fails the current test with [msg]. It never returns — use it for
    branches the test must not reach. *)

val failf : ?pos:pos -> ('a, Format.formatter, unit, 'b) format4 -> 'a
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
    equality (a module with the conventional trio is
    [Testable.make ~pp:M.pp ~equal:M.equal]), {!Testable.structural} for
    polymorphic equality under its own name, {!Testable.contramap} to compare
    and print through a projection, {!Testable.of_equal} for a type with no
    rendering. Diffing needs no support from the witness: reports compute diffs
    from the printed values, so every type gets highlighted diffs from its
    printer alone. *)

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
(** [float_exact] compares floats exactly: [a] and [b] are equal iff both are
    NaN — every NaN, regardless of payload — or they are bit-for-bit the same
    float. So a test can assert that a function actually returns NaN, unlike
    with IEEE 754 equality; [0.] and [-0.] are {e not} equal; an infinity is
    equal only to an infinity of the same sign. Failures print the shortest
    round-tripping decimal, so unequal floats never render identically. *)

val float : float -> float testable
(** [float eps] compares with absolute tolerance: [a] and [b] are equal when
    [a = b] or [|a -. b| <= eps]. NaN follows IEEE 754 — equal to nothing,
    itself included; assert a NaN result with {!float_exact}. An infinity is
    equal only to an infinity of the same sign; [0.] and [-0.] are equal.

    Raises [Invalid_argument] if [eps] is not strictly positive (NaN
    included): any such [eps] is exact equality wearing a tolerance's syntax
    — exactness is spelled {!float_exact}. *)

val float_rel : rel:float -> abs:float -> float testable
(** [float_rel ~rel ~abs] compares with combined tolerance: within [abs] near
    zero, within [rel *. Float.max (abs_float a) (abs_float b)] for large
    values. NaN and infinities behave as in {!float}.

    Raises [Invalid_argument] if either bound is negative or NaN, or if both
    are zero. One zero bound switches that component off; both zero is exact
    equality in disguise — spell it {!float_exact}. *)

val option : 'a testable -> 'a option testable
val result : 'a testable -> 'e testable -> ('a, 'e) result testable
val either : 'a testable -> 'b testable -> ('a, 'b) Either.t testable
val list : 'a testable -> 'a list testable
val array : 'a testable -> 'a array testable

val slist : 'a testable -> ('a -> 'a -> int) -> 'a list testable
(** [slist t cmp] compares lists as multisets: both sides are sorted with [cmp]
    first, so order is ignored but multiplicity is not. Failures print both
    sides in that sorted order — the order the equality compared — so the diff
    shows the multiset difference, never the incidental arrival order. *)

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
(** [pass] considers all values equal and prints [<pass>] — for ignoring a
    component of a composed witness, e.g. [pair string pass]. *)

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
      {!Gen.int32}, {!Gen.int64}, {!Gen.float}, {!Gen.float_any},
      {!Gen.float_range};
    - base — {!Gen.unit}, {!Gen.bool}, {!Gen.char}, {!Gen.char_range},
      {!Gen.string}, {!Gen.string_of}, {!Gen.bytes}, {!Gen.bytes_of};
    - containers — {!Gen.list}, {!Gen.array}, {!Gen.option}, {!Gen.result},
      {!Gen.pair}, {!Gen.triple}, {!Gen.quad};
    - choice — {!Gen.constant} (alias {!Gen.pure}), {!Gen.of_list},
      {!Gen.one_of}, {!Gen.frequency}, {!Gen.sized}, {!Gen.such_that};
    - composition — {!Gen.map}, {!Gen.bind}, the binding operators, and
      {!Gen.with_pp}, which takes the same {!type:printer} the assertion side
      uses.

    {b Note.} Length- and alphabet-controlled strings are spelled
    {!Gen.string_of}[ ?size char] and {!Gen.bytes_of} — the natural
    [string ?size ?char] spelling cannot exist (optional arguments on a value
    are unerasable, warning 16), so the knobs live on [string_of]/[bytes_of],
    aligned with {!Gen.list}. See {!Gen} for each generator's distribution,
    shrink order, and the printer-derivation law. *)

val prop :
  ?pos:pos ->
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
      [--prop-count], which wins over the default of [100]. A failure under a
      [--prop-count]-supplied count restates it in the replay hint — replaying a
      late case needs at least as many cases as the failing run.
    - [max_discard] is how many discarded cases ({!assume}, {!reject}) the
      property tolerates before it {e gives up}; the declaration site wins over
      [--max-discard], which wins over the default of twice the effective
      [count]. Raise it for a law whose precondition is genuinely rare — but a
      generator that produced the precondition by construction would not need
      the budget at all.
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
  ?pos:pos ->
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

    [next] is required, because an argument is required exactly when its absence
    would be a claim about the system rather than an absence of one: a defaulted
    identity transition on an operation that does change the state leaves the
    model frozen, every other operation's precondition unsatisfiable, and the
    test vacuously green. Read-only operations say so with [~next:Fun.const].

    [pos] is the command's declaration site, and it is what a failing step
    points at: a body is idiomatically one assertion in tail position, which
    leaves no frame to capture, so without it the step would report no location
    at all.

    [pre] and [next] must be pure and total, and ['model] must be persistent:
    the model trajectory is folded three times per case — when the program is
    drawn, when it runs, and when a counterexample prints — and the three must
    agree. One that raises is reported as a specification failure naming the
    operation and step, not as a counterexample. *)

val call :
  ?pos:pos ->
  ?pre:('model -> bool) ->
  string ->
  next:('model -> 'model) ->
  ('model -> 'sut -> unit) ->
  ('model, 'sut) command
(** [call] is {!command} for an operation with no generated argument — most
    operations, in most APIs. [call "pop" ~pre ~next:List.tl body]. *)

val stateful :
  ?pos:pos ->
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

    [scope] builds that system and reclaims it, the way {!scoped} does for a
    test: it takes a callback, and everything before the call acquires, the call
    runs the program, everything after it returns releases. It runs once per
    generated case {e and once per shrink candidate} — the search re-runs the
    program, so a shared system would make it meaningless.

    Taking a callback rather than returning a system is what puts a resource
    that exists only {e inside} a call — an Eio env or switch,
    [In_channel.with_open_text], any [with_]-style API — under test at all:
    there is no moment in those at which the resource could be returned.

    {[
      (* fragment: requires eio_main *)
      stateful "store replays" ~model:Model.empty
        ~scope:(fun run ->
          Eio_main.run @@ fun env ->
          Eio.Switch.run @@ fun sw -> run (Store.open_ ~sw ~env dir))
        commands
    ]}

    The acquire-and-release pair {!bracket} spells is the same shape with the
    release written out — a [~setup:f ~teardown:g] is this [~scope]:

    {[
      let scope run =
        let sut = f () in
        Fun.protect ~finally:(fun () -> g sut) (fun () -> run sut)
    ]}

    That [Fun.protect] is yours — windtrap never sees the resource, so releasing
    on the failing path is the scope's own contract, exactly as under {!scoped}.
    What windtrap guarantees is the failure: the program's exception is
    re-raised {e through} [scope], so a scope that cancels or cleans up on that
    path does so, and a release failure never replaces the counterexample you
    were shown (only a timeout or a fatal exception outranks it — those end the
    run). A scope that raises before calling back propagates as it is, and one
    that skips there skips the test; a scope that returns without running the
    program fails the case, and one that runs it twice raises [Invalid_argument]
    at the second call.

    [temp_dir] is test-scoped and the wrong tool here: a failing test builds
    hundreds of systems, so the scope should mint its own path and remove it on
    the way out. {!setenv} and {!chdir} are test-scoped the same way — the
    runner restores them at the attempt boundary, not between cases or shrink
    candidates — so a scope (or a command body) that moves the process or
    binds a variable carries that state into every later case of the same
    run: use absolute paths, and if the scope must touch process state, it
    puts it back itself, per case.

    [invariant] runs on the fresh system before the first call and after every
    call. An operation whose body asserts nothing is checked only by it: bodies
    check what a call {e returns}, the invariant checks what the state {e is}.

    [pp_model] adds a column showing the model before each step — the state the
    call was made in.

    [steps] is how many calls are {e drawn} per case (default [20]);
    preconditions remove some, so a program has at most [steps] calls. Shrinking
    removes calls and simplifies their arguments; it never substitutes one
    operation for another. Cost scales with [steps] and [count] and, on a
    failing test, with [--max-shrink] — a system that costs a syscall per call
    wants all three lowered.

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

val cover : label:string -> at_least:float -> bool -> unit
(** [cover ~label ~at_least cond] is {!classify}[ label cond] plus a
    requirement: the property fails unless [label] was marked in at least
    [at_least] percent of passing cases — a distribution expectation turned into
    a test outcome.

    Raises [Invalid_argument] when no property body is running, or if [at_least]
    is outside \[[0.];[100.]\]. *)

(** {1:snapshots Snapshots}

    A snapshot compares a produced string against a committed baseline at
    [<src_dir>/__snapshots__/<src_basename>/<name>.snap]. Checking is read-only:
    a missing baseline or a mismatch fails with the proposed content or a diff,
    plus the acceptance command for the way the run was invoked — [-u] on the
    executable ([dune exec <exe> -- -u] under dune), or
    [WINDTRAP_UPDATE=1 dune runtest] for inline suites — then review with
    [git diff]. A green run always means "matched a committed baseline".

    A baseline whose test was deleted or renamed is {e stale}. Only a full,
    clean run can say so — the whole declared suite executed, nothing filtered,
    focused, bailed, skipped or failed — because only such a run knows every
    name the suite claims; after one, stale baselines are listed
    ([stale baseline: <path>]) with the command that removes them.
    [--prune]/[WINDTRAP_PRUNE=1] deletes them after a full, clean update run,
    and [--strict-snapshots]/[WINDTRAP_STRICT_SNAPSHOTS=1] fails the run on them
    (exit [1], paths and way out in the failure block) — the flag that asserts
    the stored baselines are exactly the set the suite checks, so adding or
    removing a case cannot pass unnoticed. It is off by default, and the two
    compose in that order: [--prune] deletes, [--strict-snapshots] judges what
    survived. After any other run both are silently inapplicable, because a
    filtered run cannot tell a stale baseline from one it did not select.

    Baselines are invisible to dune's dependency tracking; add
    [(deps (glob_files_rec __snapshots__/** ))] to the test stanza so editing a
    baseline re-triggers [dune runtest]. *)

val snapshot : ?pos:pos -> string -> string -> unit
(** [snapshot name actual] compares [actual] against the baseline [name]. [name]
    is the baseline's identity: it must match [[A-Za-z0-9._-]+] and be unique,
    case-insensitively, among the snapshots of one source file ({!snapshot} and
    {!snapshot_pp} share the namespace). A duplicate — the same name checked
    again from a different call site, or by a different test when a site is
    unknown — fails at the second check with both locations shown; repeating one
    call site (a loop, a retry, a [cases] family) is a recheck against the same
    baseline, not a duplicate.

    The scoping source file is the file of [?pos] when given, else the file the
    enclosing test was declared in — never the caller's frame, so a snapshot
    reached through a helper in another file does not relocate its baseline.

    Snapshots are line-oriented text: CR/CRLF are normalized to LF and a
    trailing newline is forced on both sides. Content where CR bytes or the
    missing final newline are significant must be encoded first (e.g.
    [String.escaped]); redaction is ordinary code applied before the call
    ([snapshot "log" (mask_timestamps out)]).

    Raises [Invalid_argument] if [name] is empty or contains a character outside
    [[A-Za-z0-9._-]]. *)

val snapshot_pp : ?pos:pos -> string -> 'a printer -> 'a -> unit
(** [snapshot_pp name pp v] is {!snapshot}[ name] of [v] rendered by [pp]. *)

(** {1:capture Captured output} *)

val output : ?pos:pos -> unit -> string
(** [output ()] consumes the current test's captured output: the bytes written
    to standard output and standard error (C stubs and subprocesses included)
    since the test started or since the previous [output ()] call. Use it to
    assert on printed output — [equal string "hello\n" (output ())] — or feed it
    to {!snapshot}.

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

val srandom : unit -> Random.State.t
(** [srandom ()] is a fresh pseudo-random state seeded from the run's root seed
    and the executing test's path: replaying with [--seed]/[WINDTRAP_SEED]
    reproduces it, and neither suite composition nor filters perturb it —
    renaming or regrouping the test intentionally re-keys it. Every call within
    the same test returns an identically seeded state, so draw all of a test's
    randomness from one. Property tests should use {!prop} and {!Gen}; [srandom]
    serves plain tests that want stable stochastic inputs.

    A failing test that drew from [srandom] prints the replay command in its
    failure block ([replay: … --seed <token> -f '<test path>']), so the root
    token is in the log exactly when a stochastic failure needs replaying. *)

val temp_dir : ?prefix:string -> unit -> string
(** [temp_dir ()] is a fresh empty directory owned by the runner: created under
    the system temporary directory and removed after the test on every outcome —
    failure, skip, and timeout included — so tests never hand-roll temp
    lifecycles. Each call returns a new directory; [prefix] is the basename
    prefix (defaults to ["dir"]). Paths are per test attempt: a resource that
    must outlive the test (one acquired by a {!fixture}) must not live in them.
*)

val temp_file : ?prefix:string -> ?suffix:string -> unit -> string
(** [temp_file ()] is the path of a fresh empty file with the same lifecycle as
    {!temp_dir}; [prefix] is the basename prefix as in {!temp_dir} (defaults
    to ["file"]) and [suffix] is appended to the basename (e.g. [".json"]). *)

val setenv : ?pos:pos -> string -> string option -> unit
(** [setenv name (Some value)] binds the environment variable [name] to [value]
    for the rest of the test; [setenv name None] unbinds it. The runner puts
    [name] back the way it found it when the test ends, on every outcome —
    failure, skip, and timeout included, and per attempt under [~retries].

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
    visible to every thread the test spawns and to every child process it
    starts — and a test that changes the environment from a spawned thread
    races the runner's restoration. Windtrap runs tests sequentially in one
    domain, so tests never race {e each other} here; threads within one test
    are the caller's to order. *)

val chdir : ?pos:pos -> string -> unit
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

val run : ?argv:string array -> string -> test list -> unit
(** [run suite tests] parses the command line, executes the selected tests of
    [tests] sequentially in declaration order, renders the report, and
    {b exits the process}: [0] when everything passed, [1] on any failure, [2]
    when nothing ran (the filter-typo case). It never returns. Multi-file suites
    export [val tests : test list] per module and concatenate the lists into one
    [run] call.

    [argv] defaults to [Sys.argv]. Selection, seeds, snapshot acceptance, and
    output are controlled by flags ([-f], [-x], [--tag], [--seed], [-u],
    [--junit PATH], ...) with [WINDTRAP_*] environment mirrors — under
    [dune runtest] the mirrors {e are} the CLI; run the executable with [--help]
    for the full inventory. [run]'s only other inputs are ambient CI and
    terminal detection — [CI], [GITHUB_ACTIONS], [INSIDE_DUNE], and whether
    standard output is a terminal — so a test binary re-executing itself as a
    subprocess worker stays unaffected (see [doc/cookbook.md]). [--shard K/N]
    ([WINDTRAP_SHARD] mirror) deterministically partitions the selected tests
    into [N] buckets by a frozen hash of each test's path and runs bucket [K]:
    [N] concurrent partitions cover every test exactly once, stable across
    machines and suite composition. Reports go to standard output, styled when
    it is a terminal (or forced with [--color]); under GitHub Actions failures
    are also emitted as annotations. Terminal verbosity is one three-level axis:
    by default one glyph per test with failures replayed in full at the end,
    [-v] ([WINDTRAP_VERBOSE]) for one status line per test, [-q]
    ([WINDTRAP_QUIET]) for the failure blocks and summary only. The default
    level prints its header and glyph row only when the run is noteworthy — any
    failure, or any test not tagged ["slow"] exceeding the slow threshold
    ([--slow-threshold] seconds, [WINDTRAP_SLOW_THRESHOLD] mirror; default [1],
    [0] disables); a green, healthy run is exactly one line
    ([mylib: 48 passed in 1.2s.]), and tests over the threshold are listed
    slowest-first in a [slow tests (n):] block before the summary. Levels change
    what prints, never outcomes or exit codes.

    Duplicate test paths, focused tests under [CI], and a snapshot update
    request under [CI] refuse the run before anything executes. Calling [run]
    from inside a test body raises [Invalid_argument], failing the calling test.

    Code under test that calls [exit] — from a body, a setup, a teardown, a
    {!scoped} scope, or a fixture release — does not terminate the runner: the
    exit attempt is intercepted and recorded as that test's (or that release's)
    failure, and the run continues to its own exit code — the runner owns the
    process exit. A handler that catches all exceptions around the exiting call
    defeats the interception, exactly as it would swallow an assertion failure;
    to assert on exit behavior, run the exiting code in a subprocess. *)

(** {1:private Private} *)

(** Internal machinery — windtrap's own composition surface, re-exported for the
    library's per-module test suites (the [test/] directories) and for the
    co-versioned client library ([ppx_windtrap]'s runtime). Not part of the
    public API: these interfaces move
    without notice and carry no stability guarantee. Everything user-facing is
    the documented surface above; nothing here escapes into scope on
    [open Windtrap]. *)
module Private : sig
  module Atomic_file = Atomic_file
  module Capture = Capture
  module Check = Check
  module Cli = Cli
  module Clock = Clock
  module Diff = Diff
  module Driver = Driver
  module Env = Env
  module Failure = Failure
  module Loc = Loc
  module Mutate_loop = Mutate_loop
  module Path_ops = Path_ops
  module Pp = Pp
  module Property = Property
  module Render = Render
  module Render_github = Render_github
  module Render_junit = Render_junit

  module Registry = Registry
  (** The Law 16d armed hooks — the one cross-package registration cell;
      [ppx_windtrap]'s runtime registers its hook here at load. *)

  module Run = Run
  module Runner = Runner
  module Seed = Seed
  module Shrink_tree = Shrink_tree
  module Snapshot = Snapshot
  module Stateful = Stateful
  module Tag = Tag
  module Test_tree = Test_tree
  module Text = Text
end
