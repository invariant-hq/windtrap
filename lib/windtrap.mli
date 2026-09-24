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

    A verb compares its values under a {{!type:testable}witness}, and a failure
    prints both values and their diff. The witnesses of base types and
    containers are values of this module ({!int}, {!list}, {!pair}), and
    {!Testable} builds the others.

    - {{!section-declaring}Declaring tests}: {!val:test}, {!group} and {!cases},
      the resources of {!bracket}, {!scoped} and {!fixture}, {!focus} and
      {!xfail}.
    - {{!section-assertions}Assertions}: the verbs, expected value first.
    - {{!section-witnesses}Witnesses}: base types, floats, containers.
    - {{!section-properties}Properties}: {!prop}, its {{!Gen}generators}, and
      the shrinking of a failing input.
    - {{!section-stateful_tests}Stateful tests}: {!stateful}, over generated
      sequences of {{!type:command}commands} checked against a model.
    - {{!section-baselines}Baselines}: {!expect}, {!expect_exact} and
      {!expect_file}, for produced text against a literal or a committed file.
    - {{!section-capture}Captured output} and {{!section-body}the running test}:
      {!output}, {!subtest}, {!temp_dir}, {!temp_file}, {!setenv}, {!chdir}.
    - {{!section-running}Running}: {!run}, its exit codes and its command line.

    The companion package [ppx_windtrap] adds inline tests, [let%test] and
    [let%expect_test] with its [[%expect]] nodes, accepted through
    [dune promote]. It also holds two dune instrumentation backends,
    [ppx_windtrap.coverage] for expression coverage and [ppx_windtrap.mutate]
    for mutation testing, both reported by the [windtrap] command. *)

(** {1:types Types} *)

type test = Test_tree.t
(** The type for declared tests and groups of tests. A value is inert, and
    nothing runs until {!run} executes it. *)

type pos = string * int * int * int
(** The type for [__POS__] values: file, line, start column, end column.

    A [?__POS__] argument is the source location a declaration or a failure
    reports. It defaults to a best-effort capture of the call stack, which needs
    debug information ([-g], dune's default). Without debug information and
    without [~__POS__], a report has no location to print. The label puns with
    the builtin, so [~__POS__] alone passes the location of the call and no
    value is written.

    The capture finds nothing for an assertion in tail position, whose frame is
    gone when it raises. The failure then reports the [file:line] of the test's
    declaration, and nothing in the report marks the substitution. A verb's
    failure located at the line of its {!val:test} was raised from tail
    position. [~__POS__] on the assertion gives the assertion's line.

    A helper that wraps a verb or a constructor reports a line of its own. When
    the wrapped call is the helper's last expression, it reports the line of the
    helper's caller. A helper that takes [?__POS__] and passes it on reports the
    location its caller passes. *)

type 'a printer = Format.formatter -> 'a -> unit
(** The type for value printers, shared by the witnesses, by {!Gen.with_pp} and
    by the [?pp] arguments of the verbs. *)

module Testable = Testable
(** Witness constructors and accessors. See {{!section-witnesses}witnesses}. *)

type 'a testable = 'a Testable.t
(** The type for witnesses of ['a] values: a printer, an equality and an
    optional order. The printer and the equality must be total, and an exception
    from either escapes the verb. A printer that raises while a verb fails
    replaces the failure, and the block then shows [uncaught exception:] with
    that exception and no value. The equality is applied to the expected value
    first and the actual value second, which matters once it is not symmetric
    (see {!Testable.make}). See {{!section-witnesses}witnesses}. *)

(** {1:declaring Declaring tests}

    A test is named by its path: the names of its enclosing groups, then its
    own, printed joined with [" › "]. [-f], or a bare pattern on the command
    line, keeps the tests whose path string contains the pattern, and [-e] drops
    them. Patterns add up: a test is kept when it contains one of the [-f]
    patterns, and dropped when it contains one of the [-e] patterns. {!run}
    refuses a suite in which two tests have one path.

    The path is also the test's identity. It keys the
    {{!section-properties}seeds} of a property, the last failed tests [--failed]
    selects and the bucket of [--shard] (see the
    {{!section-command_line}command line}). Renaming or regrouping a test
    changes all three.

    {!val:test}, {!group}, {!slow}, {!cases}, {!bracket} and {!scoped} take the
    same four optional arguments. {!prop} and {!stateful} take the first three
    and no [retries].
    - [__POS__] is the declaration site. Defaults to a call-stack capture (see
      {!type:pos}).
    - [tags] are tag names, added to those of the enclosing groups. [--tag]
      keeps the tests that carry every named tag, and [--exclude-tag] drops
      those that carry any. A tag named by both flags is excluded.
    - [timeout] is the test's limit in seconds. Defaults to the limit of the
      nearest enclosing group that declares one, then to [--timeout], then to no
      limit.
    - [retries] is the number of extra attempts a failing test gets. Defaults to
      the [retries] of the nearest enclosing group that declares one, then to
      [0].

    {b Timeouts.} The limit covers setup, body and teardown. Setup and body
    share the window, and a teardown gets what is left of it. When they spent it
    all, the teardown gets a fresh window, so a body that times out still gets a
    bounded teardown. A timeout fails the phase it interrupted with
    [timed out after <limit>s], located at the test's declaration.

    The limit is a [SIGALRM] interval timer, so it has no effect on Windows and
    cannot interrupt a blocked C call. The runner owns [SIGALRM] while a test
    with a limit runs.

    {b Retries.} Each attempt is a fresh setup, body, teardown and capture. A
    test that fails and then passes on a later attempt is listed under
    [flaky tests] and counted [(N flaky)] in the summary. A skip is never
    retried. An {!xfail} test is retried on an unexpected pass, never on its
    expected failure.

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
    returning and fails by raising. A verb's failure fails it, and so does any
    other exception, which is reported with the first lines of its backtrace.

    While an attempt runs, the global [Random] state is seeded from the test's
    path, and it is restored afterwards. A body that uses [Random] sees the same
    stream on every run and disturbs no later test. *)

val group :
  ?__POS__:pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  test list ->
  test
(** [group name tests] is the group [name] over [tests]. Groups nest, and [name]
    is a component of the path of every test under it.
    [group ~timeout:30. "integration" tests] gives a limit of 30 seconds to each
    test of [tests] that declares none.

    A group has no hooks. {!bracket}, {!scoped} and {!fixture} scope resources,
    so no user code runs outside a test's exception boundary. *)

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
    least [--slow-threshold] seconds. The threshold defaults to [1], and [0]
    turns the list off. [--exclude-tag slow] drops the tagged tests. ["slow"] is
    an ordinary tag, and [~tags:["slow"]] on any constructor or enclosing group
    has the same effect. *)

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
(** [cases ~name base inputs fn] is one test per element of [inputs]. It is the
    group [base] whose children, in the order of [inputs], run [fn input] under
    the name [name input]. Every child reports the [cases] call as its
    declaration site. The optional arguments sit on the group, so [timeout] and
    [retries] apply to each child.

    {[
    cases "ports parse" ~name:Fun.id [ "1"; "80"; "8080"; "65535" ]
      (fun input -> ignore (require_ok (parse_port input)))
    ]}

    A child that fails does not stop the others, and each child can be selected
    alone, as in [-f "ports parse › 8080"]. Two inputs with one name have one
    path, and {!run} refuses the suite.

    [inputs] is evaluated at declaration, and [name] is applied to each input
    then, outside any test. {!temp_dir} and {!setenv} raise there, and a verb
    that fails there ends the program. A table whose rows need them is a list of
    {!subtest}s inside one body. *)

(** {2:resources Resources}

    {!bracket} scopes a resource its [setup] returns, {!scoped} one a callback
    receives, and {!fixture} one the whole run shares. A failure outside the
    body carries the tag of its phase before its location: [[setup]],
    [[teardown]] or [[release]]. *)

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
    receives the resource that [setup ()] returns and [teardown] releases. The
    runner calls [setup ()], passes the resource to [fn], then calls [teardown]
    on it. [teardown] runs iff [setup] returned, and then on every outcome of
    [fn], skip and timeout included. A fatal exception ([Sys.Break],
    [Out_of_memory]) skips it and ends the run.

    It is {!scoped} over the scope that runs the three in order. What [setup]
    raises is a [[setup]] failure. What [teardown] raises is a [[teardown]]
    failure reported beside the body's: two entries, neither hiding the other.
    Partial application makes a constructor:

    {[
    let with_db = bracket ~setup:Db.connect ~teardown:Db.close

    let empty =
      with_db "a fresh database is empty" (fun db -> equal int 0 (Db.count db))
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
(** [scoped scope name fn] is the test [name] whose body [fn] receives the
    resource that [scope] provides. A scope is a function that acquires a
    resource, hands it to a callback and reclaims it when the callback returns.
    [In_channel.with_open_text path] and [Eio_main.run] are scopes. The runner
    calls [scope] once, with a callback that runs [fn].

    {!bracket} writes the scope from a setup and a teardown. Prefer [scoped]
    when the resource comes as such a function. [scope] precedes the optional
    arguments, so a partial application keeps them:

    {[
    let with_config = scoped (In_channel.with_open_text "config.json")

    let config =
      with_config "the configuration is an object" (fun ic ->
          equal (option string) (Some "{") (In_channel.input_line ic))
    ]}

    A failure raised by [fn], a {!skip} or a timeout included, is recorded and
    then raised again through [scope]. A scope that swallows it cannot pass the
    test, and a scope that releases on the exception path does so.

    {b Warning.} The runner never holds the resource and guarantees no release.
    A scope written [let r = acquire () in fn r; release r] leaks when the body
    fails.

    [scope] must call its callback once. A scope that returns without calling it
    fails the test. A second call runs nothing and fails the test.

    What [scope] raises before the callback is a [[setup]] failure. A {!skip}
    there skips the test, so a scope that skips when the machine lacks the
    resource gates every test it declares. What [scope] raises after the
    callback returned is a [[teardown]] failure, reported beside the body's.

    [timeout] covers the whole call of [scope]. It is armed again when the body
    leaves the callback, so a release that blocks after a body timeout is cut
    short. *)

val fixture : ?teardown:('a -> unit) -> (unit -> 'a) -> unit -> 'a
(** [fixture ?teardown create] is an accessor for a resource the run shares.
    Making the accessor runs nothing. It is made once, at module top level. The
    first call inside a test acquires with [create ()], and later calls return
    the same value. A fixture no selected test calls is never acquired.

    {[
    let db = fixture ~teardown:Db.close Db.connect

    let empty =
      test "a fresh database is empty" (fun () ->
          equal int 0 (Db.count (db ())))
    ]}

    The outcome of the acquisition is kept for the run. If [create] raises, the
    calling test fails, every later call raises the same exception with its
    first backtrace, and nothing is released. If [create] skips, the skip is
    kept too, and every test that calls the accessor skips with the same reason.
    A resource the machine lacks then skips its tests and fails none.

    The runner releases the acquired fixtures after the last test, in reverse
    order of acquisition. It does so on every path where it regains control,
    [-x] included. A [teardown] that raises fails the run with a [[release]]
    failure under the path [fixture release], and the other releases still run.
    Without [teardown] nothing is released.

    {b Warning.} No timeout covers a release. A [teardown] that waits on the
    outside world must set a deadline of its own, where {!bracket}'s is cut
    short by the test's limit.

    Under [-v] the runner prints [releasing fixture (<file:line>)] before each
    release, with the site where [fixture] was applied. Without [-v] it shows
    that line while the release runs, only on a terminal whose report is styled
    and not under [--stream]. It prints nothing elsewhere. A run interrupted
    during a release names the fixture.

    Raises [Invalid_argument] if the accessor is called while no test is
    running. *)

(** {2:annotations Annotations}

    An annotation wraps a declared test or group, which keeps its declaration
    site. On a group it reaches every test under it, as in
    [xfail ~reason:"issue #42" (group "parser" tests)]. The annotation nearest a
    test wins. *)

val focus : test -> test
(** [focus t] is [t] focused. When a suite holds a focused test, only focused
    tests run, within the rest of the selection. A focused run warns on standard
    error that focus is active, whatever its exit code, and a focus that leaves
    no test to run is named in the sentence that says no tests ran.

    Under [CI] (see the {{!section-command_line}environment}) a suite that holds
    a [focus], selected or not, is refused. {!run} names the [focus] sites on
    standard error and returns [1]. *)

val xfail : ?reason:string -> test -> test
(** [xfail ?reason t] is [t] expected to fail. The test still runs. A failure
    counts as an expected failure in the summary and leaves the exit code, [-x]
    and the last failed tests alone. A test that passes fails, with a message
    saying that it was expected to fail. A skip stays a skip.

    [reason] names the known defect, as in ["issue #42"]. It prints on the
    [XFAIL] line that [-v] gives an expected failure, and in the message of an
    unexpected pass. A JUnit file ([--junit]) has no expected failure: it
    records one as a skipped test whose message is [expected failure: <reason>].

    Under [-v] the failure prints dim under the [XFAIL] line, as the block of a
    failing test would but with no [accept:] or [replay:] line, so a test that
    fails for another cause than its known defect can be told apart. A run
    without [-v] prints only the count.

    [xfail] keeps the test of a known defect running. Prefer {!skip} when the
    body must not run. *)

(** {1:assertions Assertions}

    A verb returns when its claim holds. Otherwise it raises one failure, which
    ends the body and which the runner reports with the location of the call.
    Where a verb takes two values, the expected one comes first. A verb prints
    its values only when it fails.

    Every verb takes [?__POS__], the failure's location (see {!type:pos}). Every
    verb but {!fail} and {!failf} takes [?msg], text the report prints above the
    values. {!skip} takes neither. A failure keeps at most 64 KiB of a printed
    value, and a value it cut ends with [... (truncated; N bytes total)]. The
    report shortens a long value further.

    Outside a run a failed assertion is an uncaught exception, printed as
    [windtrap assertion failure:] and the failure's first line. *)

(** {2:comparisons Equality and order}

    {!equal} and {!not_equal} read the witness's equality and never its order.
    The four ordering verbs read its order and never its equality.

    An ordering verb on a witness without order raises [Invalid_argument],
    whether or not its claim holds. The test fails on it as on any uncaught
    exception. The {{!section-witnesses}witnesses} section says which witnesses
    carry an order. *)

val equal : ?__POS__:pos -> ?msg:string -> 'a testable -> 'a -> 'a -> unit
(** [equal t expected actual] asserts that [expected] and [actual] are equal
    under [t]. The failure prints both values with [t]'s printer, and their
    diff. *)

val not_equal : ?__POS__:pos -> ?msg:string -> 'a testable -> 'a -> 'a -> unit
(** [not_equal t a b] asserts that [a] and [b] are not equal under [t]. When it
    fails the two values are equal under [t], so the failure prints one of them,
    [a]. *)

val less : ?__POS__:pos -> ?msg:string -> 'a testable -> than:'a -> 'a -> unit
(** [less t ~than v] asserts that [v] is strictly below [than] under [t]'s
    order. The failure prints the bound and the value with [t]'s printer:

    {[
    less int ~than:3 (List.length attempts)
    ]}
    {v
    expected  less than 3
    actual    5
    v} *)

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
    [t]'s order. A range takes two assertions, as in
    [at_least t ~than:lo v; less t ~than:hi v]. *)

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
    order, such as a parity. [pred] must be total. The failure prints [claim] on
    the expected side and [v], with [t]'s printer, on the actual side. [t]'s
    equality and order are not read.

    {[
    satisfies ~claim:"a power of two" int (fun n -> n land (n - 1) = 0) n
    ]}
    {v
    expected  a power of two
    actual    12
    v}

    [claim] defaults to ["value satisfying the predicate"]. Nothing checks that
    it describes [pred]. Prefer {!less}, {!at_most}, {!greater} or {!at_least}
    for a comparison with a bound, because their claim derives from the bound.
*)

(** {2:strings Strings and lists}

    The string verbs compare bytes. A failure prints the wanted string and an
    excerpt of the searched one. The excerpt is a window of 8 KiB around the
    byte offset the failure reports. When it reports none, the excerpt is the
    first 10 lines or 1 KiB, and for {!ends_with} the last. *)

val starts_with : ?__POS__:pos -> ?msg:string -> affix:string -> string -> unit
(** [starts_with ~affix s] asserts that [s] begins with [affix]. When [affix]
    occurs elsewhere in [s], the failure gives the byte offset of its first
    occurrence. *)

val ends_with : ?__POS__:pos -> ?msg:string -> affix:string -> string -> unit
(** [ends_with ~affix s] asserts that [s] ends with [affix]. When [affix] occurs
    elsewhere in [s], the failure gives the byte offset of its first occurrence.
*)

val contains : ?__POS__:pos -> ?msg:string -> sub:string -> string -> unit
(** [contains ~sub s] asserts that [sub] occurs in [s]. The empty string occurs
    in every string. *)

val not_contains : ?__POS__:pos -> ?msg:string -> sub:string -> string -> unit
(** [not_contains ~sub s] asserts that [sub] does not occur in [s], so it fails
    on every [s] when [sub] is empty. The failure gives the byte offset of the
    first occurrence and marks it in the excerpt. *)

val in_order : ?__POS__:pos -> ?msg:string -> subs:string list -> string -> unit
(** [in_order ~subs s] asserts that the elements of [subs] occur in [s] in
    order, each match starting at or after the end of the previous one. It is
    the verb for a log or a transcript, where a chain of {!contains} does not
    check the order. Matches share no byte, so [["aa"; "aa"]] needs four [a]s.
    An empty element matches without advancing.

    The failure names the element that broke the chain, by its value and its
    index from [0], and the byte the search had reached. When that element
    occurs before that byte, the failure says that it is out of order and gives
    the byte offset of that occurrence.

    Raises [Invalid_argument] if [subs] is empty. *)

val mem : ?__POS__:pos -> ?msg:string -> 'a testable -> 'a -> 'a list -> unit
(** [mem t x xs] asserts that an element of [xs] equals [x] under [t]. [t]'s
    equality receives [x] as the expected value. The failure prints [x] and
    [xs]. {!contains} is the verb for a substring. *)

(** {2:shapes Options and results}

    When a value is on the side a verb does not want, the failure prints its
    payload with [pp], or as [<abstract>] without [pp]. *)

val is_none : ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> 'a option -> unit
(** [is_none o] asserts that [o] is [None]. [pp] prints the payload of a [Some].
*)

val is_some : ?__POS__:pos -> ?msg:string -> 'a option -> unit
(** [is_some o] asserts that [o] is [Some _]. It is {!require_some} without the
    value. *)

val is_ok :
  ?__POS__:pos -> ?msg:string -> ?pp:'e printer -> ('a, 'e) result -> unit
(** [is_ok r] asserts that [r] is [Ok _]. It is {!require_ok} without the value.
*)

val is_error :
  ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> ('a, 'e) result -> unit
(** [is_error r] asserts that [r] is [Error _]. It is {!require_error} without
    the value. *)

val require_some : ?__POS__:pos -> ?msg:string -> 'a option -> 'a
(** [require_some o] is [v] when [o] is [Some v], and fails the test otherwise:

    {[
    let user = require_some (Store.find store "alice") in
    equal string "alice" user.name
    ]} *)

val require_ok :
  ?__POS__:pos -> ?msg:string -> ?pp:'e printer -> ('a, 'e) result -> 'a
(** [require_ok r] is [v] when [r] is [Ok v], and fails the test otherwise. [pp]
    prints the error. *)

val require_error :
  ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> ('a, 'e) result -> 'e
(** [require_error r] is [e] when [r] is [Error e], and fails the test
    otherwise. [pp] prints the [Ok] value. *)

val require_match :
  ?__POS__:pos -> ?msg:string -> ?pp:'a printer -> ('a -> 'b option) -> 'a -> 'b
(** [require_match extract v] is [b] when [extract v] is [Some b], and fails the
    test otherwise. It is {!require_some} for a value that is not an option.
    [pp] prints [v]. An exception from [extract] escapes the verb.

    {[
    let port = require_match (function Tcp p -> Some p | _ -> None) addr
    ]} *)

(** {2:exceptions Exceptions} *)

val raises : ?__POS__:pos -> ?msg:string -> exn -> (unit -> 'a) -> unit
(** [raises e f] asserts that [f ()] raises an exception structurally equal to
    [e]. The result of [f ()] is dropped. The failure says whether [f] returned
    or raised another exception, and prints that exception with its backtrace
    when one was recorded. When both exceptions are [Invalid_argument], both
    [Failure] or both [Sys_error], and their messages differ, the failure reads
    as a diff of the messages.

    An exception that carries what structural equality cannot compare, such as a
    function, needs {!raises_match}. A verb's failure, a {!skip}, a timeout, a
    call to [exit] or an {!assume} that [f] raises is not the exception [raises]
    waits for. It passes through and keeps its meaning. *)

val raises_match :
  ?__POS__:pos -> ?msg:string -> (exn -> bool) -> (unit -> 'a) -> unit
(** [raises_match pred f] asserts that [f ()] raises an exception that satisfies
    [pred], which must be total. The failure prints the raised exception, or
    says that [f] returned. What passes through {!raises} passes through it,
    whatever [pred] says. {!Exn} has predicates for the standard exceptions. *)

(** Exception predicates for {!raises_match}.

    A predicate checks the constructor, and the message when [substring] is
    given. An empty [substring] matches every message. Prefer {!raises} to check
    a whole message, because it holds both exceptions and reports a diff of the
    messages.

    {[
    raises_match (Exn.invalid_arg ~substring:"unhandled op") (fun () ->
        Machine.step m op)
    ]} *)
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
(** [fail msg] fails the test with [msg] and never returns. It marks a branch
    the test must not reach. *)

val failf : ?__POS__:pos -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [failf fmt ...] is {!fail} with the message [fmt] formats. *)

val skip : ?reason:string -> unit -> 'a
(** [skip ?reason ()] skips the running test and never returns. The summary
    counts the test as skipped, and [-v] prints its [SKIP] line with [reason]. A
    skip is not a failure, and a selection whose every test skipped exits [0].

    A skip may come from any phase of a test. {!scoped}, {!fixture}, {!subtest},
    {!prop} and {!stateful} say what it does there. Outside a run a skip is an
    uncaught exception, printed as [windtrap skip:] and [reason]. *)

(** {1:witnesses Witnesses}

    The values of this section are those of {!Testable} under the same names,
    typed as {!type:testable}, so [equal (list (pair string int)) a b] needs no
    qualification. The constructors stay in {!Testable}:
    - {!Testable.make} builds a witness from a printer and an equality, and
      {!Testable.with_compare} gives a witness an order. A module [M] with the
      three is
      [Testable.make ~pp:M.pp ~equal:M.equal |> Testable.with_compare M.compare].
    - {!Testable.structural} builds one from a printer alone, as
      [Testable.structural ~pp], under polymorphic equality and comparison.
    - {!Testable.contramap} compares, orders and prints through a projection.
      [Testable.contramap String.length int] is a witness for strings by their
      length.
    - {!Testable.of_equal} is for a type without a printer.

    A report computes its diff from the two printed values, so a printer is all
    a witness needs to be diffed. A printer that shows less than the equality
    compares leaves nothing to diff, and the failure's block then gives the one
    rendering under [both sides render as:].

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
    A failure on [text] diffs line by line and marks a difference that is only
    trailing whitespace. When two texts differ only by a final newline, the
    block says so and names the side that has it. Multi-line values take [text].
    Single-line values take {!string}, whose quotes tell [""], [" "] and ["\t"]
    apart. *)

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
    that round-trips to the value, [0.1 +. 0.2] as [0.30000000000000004], so two
    unequal floats never print alike. It is the witness that asserts a NaN
    result, as [equal float_exact nan v]. *)

val float : float -> float testable
(** [float eps] compares with absolute tolerance [eps]. [a] and [b] are equal
    when [a = b] or [|a -. b| <= eps]. It prints with [%g]. {!float_exact} is
    the witness for exact equality. Raises [Invalid_argument] if [eps] is not
    strictly positive, NaN included. *)

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
(** [slist t cmp] is [Testable.contramap (List.sort cmp) (list t)]. It compares
    and prints lists as multisets, sorted with [cmp]. *)

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
    value as [<pass>] and carries no order. [pair string pass] compares pairs by
    their first component. *)

(** {1:properties Properties}

    A property is a test whose body, the law, must hold for every value a
    {{!Gen}generator} draws. {!prop} runs the law on each generated case and,
    when one fails, shrinks its input to a counterexample. Shrinking belongs to
    the generator, and there is no shrink function to write. A law returns
    [unit] and asserts with the verbs, so a failing {!equal} reports its diff at
    the counterexample.

    {b Seeds.} Every generated value derives from the run's root seed, the
    test's path and the index of the case. A run whose selection holds a
    property prints the seed as [(seed s1:<16 hex digits>)]. The seed is on the
    header line, or on the summary line of a run that prints no header. [--seed]
    and its mirror [WINDTRAP_SEED] (see the
    {{!section-command_line}command line}) set it.

    The derivation and the value streams are frozen under the [s1] prefix. A
    seed replays every value on another machine, under another OCaml version and
    whatever else the suite holds.

    A law must be deterministic, because the search for a counterexample runs it
    again on candidate inputs. Every test sees a fixed stream of the global
    [Random] state (see {!val:test}), so a law that uses it stays deterministic.

    {b Reports.} A property that fails prints one block, the same with and
    without [-v]. The block holds the counterexample and the failure of the law
    on it. A property that gave up, or that missed a {!cover}, prints its
    message there. The labels of {!collect} follow, under
    [labels (N passing cases):], when a label was collected. The hits of each
    {!cover} label follow, under [covered labels:], when the property has
    several such labels and one is unmarked.

    A counterexample found in a generated case ends its block with a [replay:]
    line, the command that runs the test again under the same seed. The command
    is spelled for the way the run was started. No other failure prints one: a
    failing example, a property that gave up, an unmet {!cover}, a timeout
    before any case failed.

    Shrinking takes at most [10_000] steps. It also stops when a function of the
    generator raises on a candidate, and the block then names that exception, as
    in [shrinking stopped after 3 steps: a candidate raised Not_found]. A block
    whose search stopped either way says that shrinking stopped and that the
    counterexample may not be minimal. A function of the generator that raises
    while a case is drawn fails the case, and the block prints its exception.

    A property that passes prints nothing. Under [-v] it prints its line and,
    under it, the labels it collected. The number of discarded cases prints only
    in the message of a property that gave up. *)

(**/**)

(* Taken before [Gen] is narrowed below, for [Private]. *)
module Gen_engine = Gen.Engine

(**/**)

module Gen : sig
  (** Random generators with integrated shrinking and printing.

      A generator draws a value from the run's seed, shrinks it, and prints it
      in a counterexample. The generators are named after the witnesses of the
      same types, and [Gen.int] generates what {!Windtrap.int} compares. Under
      [open Windtrap] the unqualified name is the witness, and [Gen.(list int)]
      opens the generators locally.

      {b Shrinking.} Every generator shrinks, and a shrink candidate satisfies
      the constraints of its generator.

      {b Printing.} A counterexample prints with its generator's printer. The
      generators of base types print OCaml literals ([3l], ['a'], ["s"]), which
      paste into the [~examples] of {!Windtrap.prop}. A container or a choice
      prints each component by that component's rule. {!constant} and {!of_list}
      have no printer. {!map}, {!bind} and the binding operators have none
      either.

      A value that {!map} or {!bind} computed prints as its pre-image. The
      pre-image has the same shape, with each such value replaced by what it was
      computed from, down to the nearest generator that prints. The report marks
      it as [computed from <pre-image>], and shrinking shrinks the pre-image
      with the value.

      {[
      let* shape = gen_shape in
      let+ a = gen_tensor shape and+ b = gen_tensor shape in
      (a, b)
      ]}

      This generator prints as [shape -> (a, b)] without any {!with_pp}. [shape]
      prints with [gen_shape]'s printer, and each side of the pair by
      [gen_tensor]'s rule. {!with_pp} shows a generator that prints its value.

      A {!constant} or {!of_list} leaf without {!with_pp} has nothing to print,
      and the whole counterexample then prints as
      [<no printer: attach one with Gen.with_pp>]. {!with_pp} on the value, or
      on the leaf, is the remedy.

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
      shrinks toward [0]. It is the generator for sizes, lengths and counts. *)

  val small_int : int t
  (** [small_int] generates an integer of either sign whose magnitude follows
      {!nat}, so a value in \[[-9_999];[9_999]\]. It shrinks toward [0]. Prefer
      it to {!int} when full-range values overflow the arithmetic under test. *)

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
  (** [unit] generates [()], which does not shrink and prints as [()]. Prefer it
      to [constant ()], which has no printer, for the arm of a choice that
      carries no value. *)

  val bool : bool t
  (** [bool] generates [true] or [false] with equal probability. [true] shrinks
      to [false]. *)

  val char : char t
  (** [char] generates a byte uniformly: each of the 256 characters, ['\x00']
      and the bytes above 127 included. It shrinks toward ['a']. {!char_range}
      and {!of_list} generate subsets. *)

  val char_range : char -> char -> char t
  (** [char_range low high] generates a character in \[[low];[high]\], in byte
      order, uniformly. It shrinks toward the character of the range closest to
      ['a']: [char_range 'A' 'Z'] toward ['Z'], [char_range '0' '9'] toward
      ['9']. Sampling raises [Invalid_argument] if [high < low]. *)

  val string : string t
  (** [string] is [string_of char]. Lengths follow {!nat} and characters follow
      {!char}, ['\x00'] and the bytes above 127 included. *)

  val string_of : ?size:int t -> char t -> string t
  (** [string_of ?size char] generates a string whose length follows [size] and
      whose characters follow [char]. [size] defaults to {!nat}. It shrinks as
      {!list} does. It always prints, as a quoted string, even when [char] has
      no printer. Sampling raises [Invalid_argument] if [size] generates a
      negative length. *)

  val bytes : bytes t
  (** [bytes] is [bytes_of char]. *)

  val bytes_of : ?size:int t -> char t -> bytes t
  (** [bytes_of ?size char] is {!string_of} for [bytes]. It keeps a printer,
      which [map Bytes.of_string] over {!string_of} loses. *)

  (** {1:containers Containers} *)

  val list : ?size:int t -> 'a t -> 'a list t
  (** [list ?size gen] generates a list of [gen] values whose length follows
      [size]. [size] defaults to {!nat}. It prints as [[a; b; c]].

      With the default [size] a list shrinks by structure first: to the empty
      list, then by the removal of chunks of halving length. It then shrinks
      element by element, from the left. With an explicit [size] the length
      shrinks as [size] does, so [~size:(int_range 2 5)] holds for every
      candidate. Elements still shrink one by one.

      Sampling raises [Invalid_argument] if [size] generates a negative length.
  *)

  val array : ?size:int t -> 'a t -> 'a array t
  (** [array ?size gen] is {!list} for arrays. It prints as [[|a; b; c|]]. *)

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
  (** [constant v] generates [v], which does not shrink. It has no printer.
      [with_pp pp (constant v)] prints [v] with [pp], on its own and inside a
      container or a choice. *)

  val of_list : 'a list -> 'a t
  (** [of_list values] generates an element of [values], each with equal
      probability. It shrinks toward the head of [values]. The head is the first
      candidate and the elements in between come next, so the simplest value
      belongs at the head. Like {!constant}, it has no printer. Sampling raises
      [Invalid_argument] if [values] is empty. *)

  val one_of : 'a t list -> 'a t
  (** [one_of gens] generates with one generator of [gens], each with equal
      probability. The choice shrinks toward the head of [gens], so the simplest
      generator goes first. The value then shrinks with its own generator.

      A counterexample prints the way the generator that drew it prints. An
      [~examples] value prints with the first branch's printer when every branch
      has one. Sampling raises [Invalid_argument] if [gens] is empty. *)

  val frequency : (int * 'a t) list -> 'a t
  (** [frequency weighted] generates with one generator of [weighted], each with
      a probability proportional to its weight. The choice does not shrink. The
      value shrinks with its generator, and it prints as under {!one_of}.
      Sampling raises [Invalid_argument] if [weighted] is empty, if a weight is
      negative, or if the weights sum to less than [1]. *)

  val such_that : ('a -> bool) -> 'a t -> 'a t
  (** [such_that p gen] generates [gen] values that satisfy [p], in at most 100
      draws. When none of them satisfies [p] the case is discarded, as
      {!Windtrap.reject} discards one. Shrink candidates that fail [p] are
      dropped. It keeps [gen]'s printer.

      It is for a rare and cheap condition. Prefer a generator that satisfies a
      structural constraint by construction. *)

  (** {1:composition Composition} *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f gen] generates [f v] for [v] from [gen], and shrinks as [gen] does.
      It has no printer. Until {!with_pp} gives it one, [f v] prints as its
      pre-image, which is [v] as [gen] prints it. *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (** [bind gen f] generates [v] with [gen], then a value with [f v]. It shrinks
      [v] first, and generates again with [f] for each candidate. It then
      shrinks the inner value. It has no printer. The value prints as the inner
      value when [f v] prints, and as the pre-image [v -> inner] otherwise. *)

  val with_pp : (Format.formatter -> 'a -> unit) -> 'a t -> 'a t
  (** [with_pp pp gen] is [gen] printing with [pp], which wins over a pre-image
      and over a derived printer. [pp] has the type {!Windtrap.printer}, so a
      witness's printer fits, as in [with_pp (Testable.pp t) gen]. A generator
      of records prints its values this way:

      {[
      let user =
        Gen.(
          with_pp pp_user
            (let+ name = string and+ age = int_range 0 120 in
             { name; age }))
      ]} *)

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
      the property gives up and fails. Its message gives the discards and the
      cases that passed. Defaults to twice the effective [count]. No flag sets
      it. A law whose precondition is rare needs a larger budget, and a
      generator that builds the precondition in needs none.
    - [examples] are inputs that the law runs on before any generated case, as
      in [~examples:[ Rect (2., 0.) ]]. They are not shrunk and do not depend on
      the seed. It is the place for a counterexample kept as a regression. A
      failing one reports as [example N] and prints with [gen]'s printer, or as
      the placeholder of {!Gen} when [gen] has none.
    - [timeout] covers generation and shrinking together. When it expires before
      a case failed, the test fails as timed out. When it expires during
      shrinking, the report gives the counterexample found so far and says it
      may not be minimal.

    A property carries the tag ["prop"], so [--tag prop] selects the properties.
    It takes no [retries] argument, but it inherits the [retries] of an
    enclosing group, and every retry replays the same cases from the seed. A
    case is found again by its index, so the [replay:] line restates a count
    that came from [--prop-count]. A {!skip} raised by [law] skips the test. The
    {{!section-properties}section preamble} says what a property reports, and
    when.

    Raises [Invalid_argument], inside the running test, if [count] or
    [max_discard] is negative. *)

(** {2:discarding Discarding and labelling cases}

    {!assume} and {!reject} discard the current case. {!collect}, {!classify}
    and {!cover} label it. All five work in the law of a {!prop}, and in the
    bodies and the invariant of a {!stateful} test. There a discard drops the
    whole program and a label counts once per program. The
    {{!section-properties}reports} of a property say where labels and discards
    are printed. {!assume} and {!reject} work too in a function given to a
    generator, where a discard drops the case or the shrink candidate. *)

val assume : bool -> unit
(** [assume cond] discards the current case unless [cond] holds. A discarded
    case counts against [max_discard] and another is generated. It is for a rare
    and cheap precondition. Prefer {!Gen.such_that}, or a generator correct by
    construction, when the precondition is structural. Outside a property,
    [assume false] fails the test with the message
    [assume or reject was called outside a property]. *)

val reject : unit -> 'a
(** [reject ()] discards the current case. See {!assume}. *)

val collect : string -> unit
(** [collect label] marks the current case with [label]. The report gives the
    distribution of the labels over the passing cases. It shows whether a
    generator reaches the regions of interest.

    A case counts a label once. Discarded cases, failing cases and shrinking
    count nothing. Raises [Invalid_argument] if no property is running. *)

val classify : string -> bool -> unit
(** [classify label cond] is [collect label] if [cond] and [()] otherwise.
    Raises as {!collect} does, whatever [cond]. *)

val cover : string -> bool -> unit
(** [cover label cond] is [classify label cond] with a demand. The property
    fails with [never covered: "label"] unless a passing case marked [label].
    The demand is on presence, never on a proportion. A generator that stops
    reaching a region fails the property under [cover]. Under {!classify} it
    only changes what [-v] prints.

    The demand registers when [cover] is called, whatever [cond], so a [cover]
    the law does not always reach may register nothing. It is judged when the
    property has run all its cases. Raises as {!collect} does. *)

(** {1:stateful_tests Stateful tests}

    A stateful test checks generated sequences of calls on a system against a
    model, a pure value that stands for the state of the system. A
    {{!type:command}command} is one operation of the system. A program is a
    sequence of calls, each legal in the model that the calls before it
    produced. {!stateful} draws programs and shrinks one that fails.

    {[
    let commands =
      [
        command "push" Gen.small_int
          ~next:(fun m x -> m @ [ x ])
          (fun _ x q -> Queue.push x q);
        call "pop"
          ~pre:(fun m -> m <> [])
          ~next:List.tl
          (fun m q -> equal int (List.hd m) (Queue.pop q));
      ]

    let fifo =
      stateful "a queue pops in the order of its pushes" ~model:[]
        ~scope:(fun run -> run (Queue.create ()))
        ~invariant:(fun m q -> equal int (List.length m) (Queue.length q))
        commands
    ]} *)

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
    - [next m arg] is the model after the call. It is required, and a read-only
      operation passes [~next:Fun.const].
    - [body m arg sut] calls the system and asserts with the verbs. [m] is the
      model before the call.
    - [__POS__] is the declaration site, which a failing call reports when its
      assertion recorded no location. A body that is one assertion in tail
      position records none (see {!type:pos}).

    An operation that matters only on a full queue is generated only in the
    states where [pre] says the queue is full. No call the model forbids is ever
    made. The test that an illegal call raises is a command whose [pre] selects
    the illegal state and whose body asserts the raise. A body that asserts
    nothing is checked by {!stateful}'s [invariant] alone.

    [pre] and [next] must be pure and ['model] persistent, because the model's
    trajectory is computed again whenever a program is drawn, run or printed. A
    mutable model returned unchanged corrupts generation, so a hash table must
    be modelled as a [Map].

    A [pre] or [next] that raises while a program is drawn fails the case
    unshrunk, reported as [call <k>: <name>, ~pre raised <exn>] with its
    backtrace. One that raises only on a shrink candidate stops the search (see
    the {{!section-properties}reports} of a property). The block then shows the
    last program the search accepted and says that shrinking stopped, naming the
    exception as [call <k>: <name>, ~pre raised <exn>]. *)

val call :
  ?__POS__:pos ->
  ?pre:('model -> bool) ->
  string ->
  next:('model -> 'model) ->
  ('model -> 'sut -> unit) ->
  ('model, 'sut) command
(** [call name ~next body] is {!val:command} for an operation without argument.
    [pre], [next] and [body] take none, and a read-only operation passes
    [~next:Fun.id]. Its call prints as its name alone. *)

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
      every call. Bodies check what a call returns, and the invariant checks
      what the state is. The last assertion of an invariant is in tail position.
      Without [~__POS__] its failure is located at the test's declaration.
    - [steps] is the number of calls drawn per case. Defaults to [20]. A drawn
      call whose [pre] fails is dropped, so a program has at most [steps] calls.
    - [pp_model] adds a column to the printed program: the model before each
      call.
    - [count] and [timeout] are {!prop}'s. So are [--prop-count], the seed, the
      bound on shrinking and the {{!section-properties}reports} of a property,
      with the program in the place of the counterexample.

    Each drawn call picks its command with equal probability. The order of
    [commands] means nothing. A command listed twice is drawn twice as often,
    and that is the way to weight one. Shrinking removes calls and shrinks
    arguments, and never replaces one operation by another.

    {b The scope.} [scope] takes a callback, calls it once with a fresh system,
    and releases the system whether the callback returns or raises. It runs once
    per case and once per shrink candidate, because a search that runs programs
    again proves nothing on a system two runs share. A function such as
    [In_channel.with_open_text path] is a scope as it stands. A scope written by
    hand must release on both paths:

    {[
    let scope run =
      let db = Db.connect () in
      match run db with
      | () -> Db.close db
      | exception e ->
          Db.close db;
          raise e
    ]}

    A scope that returns without calling back fails the case. A second call
    raises [Invalid_argument], and the report then shows the empty program with
    that exception. What [scope] raises before calling back fails the case, and
    a {!skip} there skips the test.

    A program's failure is raised again through [scope], so a scope that
    swallows it cannot pass the case. A release that raises over a failing
    program is dropped and the counterexample stands, unless the release skips,
    times out, exits or discards, which keeps its meaning. Over a passing
    program it fails the case.

    {b Warning.} {!temp_dir}, {!temp_file}, {!setenv} and {!chdir} last for the
    attempt, never for a case (see {{!section-body}the running test}). A scope
    must make and remove its own files, under absolute paths, and put process
    state back itself.

    {b The report.} A failure prints the shrunk program as a table, one numbered
    call per row. Then come the failing call and its failure, under
    [call K of N: <name>], [invariant after call K of N: <name>] or
    [invariant on the fresh system]. The empty program prints as
    [(no commands)]. It still tests something, since the invariant runs on the
    fresh system.

    An argument prints with its generator's printer. It has no pre-image, and
    without a printer it prints as the placeholder of {!Gen}.

    A stateful test carries the tags ["prop"] and ["stateful"]. It takes no
    [retries], no [examples] and no [max_discard]. Like a {!prop}, it inherits
    the [retries] of an enclosing group, and every retry replays the same
    programs. A program cannot be written by hand, so a regression is kept by
    copying the shrunk program into a {!val:test}. The system must behave the
    same from run to run.

    The cost grows with [steps] and [count] and, on a failing test, with the
    search. Each shrink candidate is a whole program and one call of [scope].

    Raises [Invalid_argument], inside the running test, if [commands] is empty
    or if [steps] is negative. *)

(** {1:baselines Baselines}

    A baseline is reviewed text the source names. It is the literal at an
    {!expect} or {!expect_exact} call, or the file an {!expect_file} call names,
    relative to the project root. The three take the produced text first and the
    baseline last, so a [{|…|}] literal reads as a block that closes the call.

    Checking writes nothing. A mismatch, or a missing file, records a failure
    and the call returns. Where a failed assertion ends the body, a mismatch
    lets it continue, so one run reports every stale expectation. The failure
    carries the diff, or the content proposed for a missing file, and an
    [accept:] line spelled for the way the run was started.

    {b Under dune} the [(deps …)] field of the [(test)] stanza names every file
    an {!expect_file} call reads, as in [(deps help.expected)]. The source file
    of a literal needs no entry. The stanza's action runs the executable with
    [--corrected]. It then holds one [diff?] for each file that holds a
    baseline, which includes the source file of a literal:

    {v
    (action
     (progn
      (run %{test} --corrected)
      (diff? test_mylib.ml test_mylib.ml.corrected)
      (diff? help.expected help.expected.corrected)))
    v}

    The run writes each correction as [<file>.corrected] beside dune's copy of
    the file, and [dune promote] accepts what a [diff?] reported. The action
    stops at its first failing [diff?], so one [dune promote] accepts one stale
    file. The [accept:] line of another file does nothing until a later run
    reaches that file's [diff?]. A correction is registered by dune only after
    an action that exits [0]. A [--corrected] run that wrote one and returns [1]
    says so on standard error.

    {b Without dune} [-u] rewrites the literals and the files in place,
    atomically, for review with [git diff]. Under [-u] a mismatch is accepted
    and its test passes, and one run accepts every stale expectation. A literal
    is compiled into the executable, so after [-u] has rewritten one the
    executable must be built again before the next run. An accepted file needs
    nothing.

    [-u] is refused under [CI] (see the {{!section-command_line}environment}),
    and [-u] with [--corrected] is a usage error.

    A correction is kept only for a test whose every failure is a baseline
    mismatch. An assertion failure, another exception or a skip beside the
    mismatch withholds it until that is fixed. The failure then carries no
    [accept:] line and says why. An {!xfail} test keeps none, since its mismatch
    is the failure it expects. A test is not retried past an attempt whose
    corrections were kept, whatever its [retries].

    A correction that the source cannot take fails its test and says why. The
    kept corrections are written once, after the last test. One that cannot be
    written fails the run. The project root is [WINDTRAP_PROJECT_ROOT] when set,
    else the parent of dune's build directory, else the working directory. *)

val expect : string -> pos * string -> unit
(** [expect actual @@ __POS_OF__ {|…|}] compares [actual] with the literal up to
    whitespace. On both sides every line is trimmed on the right, and the blank
    leading and trailing lines are dropped. The block is then dedented, so only
    relative indentation counts. The second argument is what [__POS_OF__]
    builds: the position of the literal and its value.

    {[
    test "help lists the flags" (fun () ->
        print_string (Tool.help ());
        expect (output ()) @@ __POS_OF__ {|
          usage: tool [OPTIONS]
          |})
    ]}

    A mismatch records a failure located at the line of [__POS_OF__] and
    returns. Its correction rewrites the literal, each line indented two columns
    past the indentation of that line. The position is the compiler's, so a call
    that moves keeps its baseline. A call several tests share, as under
    {!cases}, must produce one text. Once a text is accepted, another is a
    mismatch.

    When the source changed since the build, or cannot be read, a correcting run
    keeps no correction for the literal: the expectation fails, under [-u] too,
    and says why. A source file that cannot be proven to lie under the project
    root fails the test at once, as an assertion does. Raises [Invalid_argument]
    if no test is running. *)

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
    first, as with [String.escaped]. Redaction is code applied before the call.

    Under dune the run reads dune's copy of the file (see
    {{!section-baselines}baselines}). Promotion never creates a file, so a new
    baseline starts as an empty file or is accepted once with [-u].

    A [path] that cannot be proven to lie under the project root fails the test
    at once, as an assertion does. [expect_file] takes no [?__POS__]. Its
    failure is located at the call and, from tail position, at the test's
    declaration. Raises [Sys_error] if the file exists and cannot be read, and
    [Invalid_argument] if no test is running. *)

(** {1:capture Captured output}

    The runner captures what a test writes to standard output and standard
    error, C stubs and child processes included, into
    [<log dir>/<suite>/<groups>/<test>.output]. A group or test name is made a
    file name there: a character other than a letter, a digit, [-], [_] or [.]
    becomes [_] and a hash of the name is appended, and a name past 80
    characters is cut. The log directory is [-o], by default [_tests] in dune's
    build directory and [windtrap] in the system temporary directory otherwise.
    A failing test's block shows the last lines of its output under
    [captured output], then [full log: <path>]. The block of a test that wrote
    nothing has no such part. [--stream] turns capture off. *)

val output : unit -> string
(** [output ()] is what the running test wrote to standard output and standard
    error since the previous [output ()], or since the attempt started. The two
    streams are one text, in the order the bytes reached them, and [""] when
    nothing is left. A channel buffers, so text that [print_string] wrote may
    follow a later [prerr_endline], which flushes at once. [flush stdout] before
    the write to standard error keeps the order.

    [equal string "hello\n" (output ())] asserts on printed output, and
    {!expect} takes [output ()] as its produced text. What [output ()] returned
    still shows in a failure's block, which reads the whole log. Under
    [--stream] the call fails the test with
    [this test requires capture; rerun without --stream]. When the test's log
    can no longer be opened, the call fails the test with a message that names
    the log. Raises [Invalid_argument] if no test is running. *)

(** {1:body The running test}

    Operations on the test that is executing, in its setup, its body and its
    teardown. Each raises [Invalid_argument] when no test is running: at module
    top level, after the run, in the release of a fixture. So do {!output}, the
    {{!section-baselines}expectations}, {!collect}, {!classify}, {!cover} and
    the accessors of {!fixture}. The assertion verbs do not (see
    {{!section-assertions}assertions}).

    Tests run one at a time, in one domain, in declaration order, so the
    environment and the working directory never race between tests. Nothing here
    is thread-safe.

    A directory, a binding or a working directory made here lasts until the
    attempt ends. Each attempt of a retried test starts without them, and all
    the cases of a {!prop} or a {!stateful} test share them. *)

val current_test : unit -> string list
(** [current_test ()] is the path of the running test: the names of its
    enclosing groups, outermost first, then its own. It is never empty, and it
    is the same in every attempt and inside a {!subtest}. Joined with [" › "] it
    is the string [-f] matches. A file or a log entry named with it carries the
    test's name without the name being written twice. *)

val subtest : string -> (unit -> unit) -> unit
(** [subtest name fn] runs [fn ()] as a named part of the running test. A
    failure of [fn], a verb's or any other exception, is recorded and [subtest]
    returns. The failure's block carries a [subtest] line with the names of the
    enclosing subtests joined with [" › "], as in [subtest   outer › inner]. The
    subtests after it still run, and the test fails at the end with every
    failure recorded.

    A {!skip}, a timeout or a call to [exit] ends the whole test, which still
    fails on what was recorded, and inside the law of a property an {!assume}
    discards the case. A subtest names a failure and is not a test. [-f] cannot
    select a subtest, where it can select a child of {!cases}. Inside the law of
    a property a subtest failure is not shrunk.

    {[
    test "every backend honours the contract" (fun () ->
        List.iter
          (fun (name, backend) -> subtest name (fun () -> check backend))
          backends)
    ]} *)

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
    for the rest of the test. [setenv name None] unbinds it, and
    [Sys.getenv_opt name] is then [None]. When the attempt ends, on every
    outcome, the runner restores what [name] held before the test's first
    [setenv] of it. A restoration that fails is a [[teardown]] failure of the
    test.

    {b Warning.} The binding belongs to the process. Threads and child processes
    see it, and a thread still running when the test ends races the restoration.

    Raises [Invalid_argument] if [name] is empty or contains ['=']. *)

val chdir : string -> unit
(** [chdir dir] changes the working directory to [dir] for the rest of the test.
    When the attempt ends, on every outcome, the runner returns to the directory
    the process was in before the test's first [chdir]. If it cannot, as when
    the test deleted it, the test fails with a [[teardown]] failure that names
    the directory. The directory is restored before the environment, and before
    the {!temp_dir}s are removed, so a test may change into one of its own:

    {[
    test "the build writes its output in place" (fun () ->
        chdir (temp_dir ());
        Builder.run ();
        is_true (Sys.file_exists "output.txt"))
    ]}

    The change belongs to the process, as {!setenv}'s does. A relative
    {!expect_file} path does not follow it. Raises [Unix.Unix_error] if [dir]
    cannot be entered. *)

(** {1:running Running} *)

val run : ?argv:string array -> string -> test list -> int
(** [run ?argv suite tests] parses the {{!section-command_line}command line}
    [argv] and executes the selected tests of [tests], one at a time, in
    declaration order. It writes the {{!section-report}report} to standard
    output, returns the {{!section-exit_codes}exit code} and never exits the
    {{!section-process}process}. The caller does, on the last line of the file:

    {[
    let () = exit (run "mylib" [ parsing; reversal ])
    ]}

    Under dune the stanza [(test (name test_mylib) (libraries windtrap))] builds
    [test_mylib.ml] and runs it on [dune runtest]. In a suite over several files
    each module exports its groups and one [run] lists them. A test left out of
    the list does not run. [-l] prints the selected paths, one per line, and
    runs nothing. A selection that keeps no test of a suite that declares some
    prints no path and says [windtrap: no tests ran: <reason>.] on standard
    error.

    [suite] names the run in its report, and names the directory of the capture
    logs and of the last failed tests. [argv] defaults to [Sys.argv]. [argv.(0)]
    is not parsed and names the program in the commands a report prints. Under
    dune ([INSIDE_DUNE] set) such a command is [dune exec <program> --], with
    [--instrument-with ppx_windtrap.mutate] before the program when it holds
    mutants, and elsewhere it is [argv.(0)], quoted where a shell would split
    it. Under [--corrected], or when [argv.(0)] is empty, it spells the flags'
    variables in front of [dune runtest].

    Raises [Invalid_argument] if a run is executing, as when a test body starts
    another run. Two runs one after the other are allowed, and fixtures are
    acquired again in the second. *)

(** {2:exit_codes Exit codes}

    {!run} returns:
    - [0] when no selected test failed. A skip and an expected failure are no
      failure. [--help], [--version] and [-l] print and return [0]. [--version]
      prints [windtrap <version>].
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
    record that [--failed] reads. A selection that keeps no test of a suite that
    declares some returns [0]. Under [dune runtest] the mirrors reach every test
    stanza of the project, so a filter meant for one suite empties the others.
    The [diff?] that follows the run is the verdict there. *)

(** {2:command_line Command line and environment}

    [--help] lists the flags, each with its mirror when it has one. A mirror is
    a [WINDTRAP_*] variable that sets the flag under [dune runtest], where the
    executable gets no command line. [-l], [--failed], [-x], [-u],
    [--corrected], [-h] and [-V] have none. The command line wins over the
    environment.

    A mirror is [WINDTRAP_] and the long flag in capitals, with [_] for [-], as
    [WINDTRAP_PROP_COUNT] is for [--prop-count]. The one exception is [--arm],
    whose mirror is [WINDTRAP_MUTATE_ARM].

    [--tag] and [--exclude-tag] add up, across repeated flags, across the
    comma-separated list of their mirrors, and across both. [WINDTRAP_FILTER]
    and [WINDTRAP_EXCLUDE] hold one pattern each, commas included, and the
    patterns of [-f] or [-e] on the command line replace that of the mirror.
    Flags that change what prints change no outcome and no exit code.

    {b Warning.} A changed variable does not make dune run a test again. On a
    stanza that already passed, a mirror does nothing without
    [dune runtest --force]. Nothing prints and the exit code is [0].

    [--shard K/N], with [1 <= K <= N], keeps bucket [K] of [N] of the selection
    by a frozen hash of each path. The buckets cover every test once, the same
    on every machine and whatever else the suite holds.

    [--failed] selects the last failed tests. The tests a run executes update
    that record, and a test it did not execute, under a filter or after [-x],
    keeps its entry.

    Beyond the mirrors {!run} reads [WINDTRAP_PROJECT_ROOT] (see
    {{!section-baselines}baselines}), [CI], [GITHUB_ACTIONS], [INSIDE_DUNE],
    [NO_COLOR], [TERM] and whether standard output is a terminal. [CI] counts as
    set unless it is empty or one of [0], [false], [no], [n] and [off].
    [GITHUB_ACTIONS] with [CI] adds a workflow annotation per failure and folds
    the transcript in a group. Under [--color auto] the report is styled on a
    terminal or under [INSIDE_DUNE], unless [NO_COLOR] is set or [TERM] is
    [dumb]. *)

(** {2:report The report}

    A run with nothing to show prints one line, its summary. A test that passes,
    skips or fails as expected prints nothing, and the summary counts it. A
    failure prints as a block when its test finishes, the same with and without
    [-v]. The run ends on the [slow tests] and [flaky tests] blocks and on its
    summary. [-v] prints a line per test, with a failure's block under its line.

    Usage errors, refusals, warnings and the line of an interrupted run go to
    standard error behind [windtrap:]. The manual covers the workflows. *)

(** {2:process The process}

    A call to [exit] in code under test does not end the run. It is recorded as
    the failure of its test. A handler that catches every exception around the
    call defeats this, as it hides an assertion's failure, a {!skip}, a timeout
    and an {!assume}. Code that exits must be tested in a child process. {!run}
    turns the recording of backtraces on and leaves it on.

    While it executes, and not on Windows, {!run} handles [SIGINT], [SIGTERM]
    and [SIGHUP]. It prints what it interrupted, as in
    [windtrap: interrupted in <path>], then the summary. It removes the
    attempt's temporary files and releases the fixtures still held. It does not
    run the teardown of the interrupted test, writes no correction and dies by
    the same signal. *)

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
