(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Windtrap - One library for all your OCaml tests.

    Unit tests, property-based tests, snapshot tests, and expect tests in a
    single library. Tests are organized as a tree of cases and groups, run with
    {!run}.

    {1 Quick Start}

    {[
      open Windtrap

      let () =
        run "MyTests"
          [
            group "Calculator"
              [
                test "addition" (fun () -> equal Testable.int 5 (add 2 3));
                test "subtraction" (fun () -> equal Testable.int 1 (sub 3 2));
              ];
            test "standalone" (fun () -> is_true true);
          ]
    ]} *)

(** {1 Modules} *)

module Testable = Testable
(** Type-safe equality witnesses. See {!module:Testable}. *)

module Tag = Tag
(** Test metadata for filtering and categorization. *)

module Pp = Pp
(** Lightweight pretty-printing with ANSI styling. *)

module Ppx_runtime = Ppx_runtime
(** {b Internal.} Runtime support for [windtrap.ppx]. Used by PPX-generated
    code; not intended for direct use. *)

(** {1 Test Creation} *)

type test
(** An abstract test or group of tests. *)

val test :
  ?pos:Test.pos ->
  ?tags:Tag.t ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  (unit -> 'a) ->
  test
(** [test name fn] creates a test case. The return value of [fn] is ignored.

    @param timeout
      Maximum time in seconds for the test to complete. If exceeded, the test
      fails with a timeout error.
    @param retries
      Number of retry attempts on failure (default: 0). Use sparingly for flaky
      tests. *)

val group :
  ?pos:Test.pos ->
  ?tags:Tag.t ->
  ?setup:(unit -> unit) ->
  ?teardown:(unit -> unit) ->
  ?before_each:(unit -> unit) ->
  ?after_each:(unit -> unit) ->
  string ->
  test list ->
  test
(** [group name children] creates a test group. Tags are inherited by children.

    @param setup Runs once before any child test.
    @param teardown Runs once after all child tests, even on failure.
    @param before_each
      Runs before every individual test in this group. Hooks from nested groups
      stack: outer [before_each] runs first.
    @param after_each
      Runs after every individual test in this group (even on failure). Hooks
      from nested groups stack: inner [after_each] runs first. *)

val ftest :
  ?pos:Test.pos ->
  ?tags:Tag.t ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  (unit -> 'a) ->
  test
(** [ftest name fn] creates a focused test case. When any focused test or group
    exists, only focused tests run. A warning is printed to remind you to remove
    focus markers before committing. *)

val fgroup :
  ?pos:Test.pos ->
  ?tags:Tag.t ->
  ?setup:(unit -> unit) ->
  ?teardown:(unit -> unit) ->
  ?before_each:(unit -> unit) ->
  ?after_each:(unit -> unit) ->
  string ->
  test list ->
  test
(** [fgroup name children] creates a focused test group. All tests inside a
    focused group are treated as focused. *)

val slow :
  ?pos:Test.pos ->
  ?tags:Tag.t ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  (unit -> 'a) ->
  test
(** [slow name fn] creates a test tagged with {!Tag.Slow}, skipped when
    [~quick:true] is passed to {!run}. The return value of [fn] is ignored. *)

val bracket :
  ?pos:Test.pos ->
  ?tags:Tag.t ->
  ?timeout:float ->
  ?retries:int ->
  setup:(unit -> 'a) ->
  teardown:('a -> unit) ->
  string ->
  ('a -> 'b) ->
  test
(** [bracket ~setup ~teardown name fn] creates a test with setup and teardown.
    [setup] runs before the test to acquire a resource, which is passed to [fn].
    [teardown] runs after the test (even on failure) to release the resource.

    Partial application creates reusable test constructors:
    {[
      let with_db = bracket ~setup:connect_db ~teardown:close_db

      group "Database" [
        with_db "query works" (fun db -> ...);
        with_db "insert works" (fun db -> ...);
      ]
    ]} *)

val cases : 'a Testable.t -> 'a list -> string -> ('a -> unit) -> test
(** [cases testable cases name fn] creates a group with one test per case.

    Test names are generated as "[name] (case_value)" using the testable's
    pretty-printer.

    {[
      cases
        Testable.(triple int int int)
        [ (2, 3, 5); (0, 0, 0); (-1, 1, 0) ]
        "addition"
        (fun (a, b, expected) -> equal Testable.int expected (a + b))
    ]} *)

val fixture : (unit -> 'a) -> unit -> 'a
(** [fixture create] returns a lazy accessor. The resource is created on first
    call and cached for subsequent calls.

    {[
      let get_db = fixture (fun () -> Db.connect ())

      group "Database"
        [ test "query" (fun () -> let db = get_db () in ...);
          test "insert" (fun () -> let db = get_db () in ...);
        ]
    ]} *)

(** {1 Running Tests} *)

type format =
  | Compact  (** One character per test (dot reporter, default). *)
  | Verbose  (** Tree-style hierarchical output. *)
  | Tap  (** {{:https://testanything.org/} TAP} (Test Anything Protocol). *)
  | Junit  (** JUnit XML to stdout. *)

val run :
  ?quick:bool ->
  ?bail:int ->
  ?fail_fast:bool ->
  ?output_dir:string ->
  ?stream:bool ->
  ?update:bool ->
  ?snapshot_dir:string ->
  ?filter:string ->
  ?exclude:string ->
  ?failed:bool ->
  ?list_only:bool ->
  ?format:format ->
  ?junit:string ->
  ?seed:int ->
  ?timeout:float ->
  ?prop_count:int ->
  ?tags:string list ->
  ?exclude_tags:string list ->
  ?argv:string array ->
  string ->
  test list ->
  unit
(** [run name tests] runs the test suite and exits the process.

    - Exit 0: all tests passed (at least one ran).
    - Exit 1: at least one test failed.
    - Exit 2: no tests ran (all filtered out). Helps catch typos in filters.

    Tests with the "disabled" label are automatically skipped.

    Configuration is resolved with the following priority (highest first):
    programmatic arguments, CLI flags, environment variables, defaults.

    @param quick Skip tests tagged as slow (default: false)
    @param bail
      Stop the suite after [N] failures. CLI: [--bail N]. Useful for large
      suites where you want to stop early without aborting on the very first
      failure.
    @param fail_fast
      Stop after first test failure (default: false). Sugar for [~bail:1]. CLI:
      [-x], [--fail-fast].
    @param output_dir Directory for test logs (default: "_build/_tests")
    @param stream
      Stream test output to console instead of capturing to files. Useful for
      debugging. CLI: [-s], env: [WINDTRAP_STREAM=1].
    @param update Update snapshots instead of comparing (default: false)
    @param snapshot_dir Overrides snapshot root directory
    @param filter
      Filter tests by name (substring match). CLI: [-f PATTERN] or positional.
    @param exclude
      Exclude tests by name (substring match). CLI: [-e PATTERN],
      [--exclude PATTERN], env: [WINDTRAP_EXCLUDE].
    @param failed
      Rerun only tests that failed in the last run. CLI: [--failed]. Reads from
      a [.last-failed] file in the output directory.
    @param list_only List test names without running. CLI: [-l].
    @param format
      Output format: Compact, Verbose, Tap, or Junit (default: Compact)
    @param junit Path to write JUnit XML report
    @param seed
      Random seed for property tests. CLI: [--seed N], env: [WINDTRAP_SEED].
    @param timeout
      Default timeout in seconds for tests without a per-test timeout. CLI:
      [--timeout N], env: [WINDTRAP_TIMEOUT].
    @param prop_count
      Number of test cases per property test (default: 100). CLI:
      [--prop-count N], env: [WINDTRAP_PROP_COUNT].
    @param tags
      Required labels. Only tests with all listed labels will run. CLI:
      [--tag LABEL] (repeatable), env: [WINDTRAP_TAG] (comma-separated).
    @param exclude_tags
      Excluded labels. Tests with any listed label will be skipped. CLI:
      [--exclude-tag LABEL] (repeatable), env: [WINDTRAP_EXCLUDE_TAG]
      (comma-separated).
    @param argv Command line arguments to parse (default: [Sys.argv]) *)

(** {1 Equality Assertions} *)

val equal :
  ?here:Check.here ->
  ?pos:Check.pos ->
  ?msg:string ->
  'a Testable.t ->
  'a ->
  'a ->
  unit
(** [equal testable expected actual] asserts that [expected] equals [actual]. *)

val not_equal :
  ?here:Check.here ->
  ?pos:Check.pos ->
  ?msg:string ->
  'a Testable.t ->
  'a ->
  'a ->
  unit
(** [not_equal testable a b] asserts that [a] does not equal [b]. *)

(** {1 Boolean Assertions} *)

val is_true : ?here:Check.here -> ?pos:Check.pos -> ?msg:string -> bool -> unit
(** [is_true b] asserts that [b] is [true]. *)

val is_false : ?here:Check.here -> ?pos:Check.pos -> ?msg:string -> bool -> unit
(** [is_false b] asserts that [b] is [false]. *)

(** {1 Option Assertions} *)

val is_some :
  ?here:Check.here -> ?pos:Check.pos -> ?msg:string -> 'a option -> unit
(** [is_some opt] asserts that [opt] is [Some _]. *)

val is_none :
  ?here:Check.here -> ?pos:Check.pos -> ?msg:string -> 'a option -> unit
(** [is_none opt] asserts that [opt] is [None]. *)

val some :
  ?here:Check.here ->
  ?pos:Check.pos ->
  ?msg:string ->
  'a Testable.t ->
  'a ->
  'a option ->
  unit
(** [some testable expected opt] asserts that [opt] is [Some expected]. *)

(** {1 Result Assertions} *)

val is_ok :
  ?here:Check.here -> ?pos:Check.pos -> ?msg:string -> ('a, 'b) result -> unit
(** [is_ok r] asserts that [r] is [Ok _]. *)

val is_error :
  ?here:Check.here -> ?pos:Check.pos -> ?msg:string -> ('a, 'b) result -> unit
(** [is_error r] asserts that [r] is [Error _]. *)

val ok :
  ?here:Check.here ->
  ?pos:Check.pos ->
  ?msg:string ->
  'a Testable.t ->
  'a ->
  ('a, 'b) result ->
  unit
(** [ok testable expected r] asserts that [r] is [Ok expected]. *)

val error :
  ?here:Check.here ->
  ?pos:Check.pos ->
  ?msg:string ->
  'b Testable.t ->
  'b ->
  ('a, 'b) result ->
  unit
(** [error testable expected r] asserts that [r] is [Error expected]. *)

(** {1 Exception Assertions} *)

val raises :
  ?here:Check.here ->
  ?pos:Check.pos ->
  ?msg:string ->
  exn ->
  (unit -> 'a) ->
  unit
(** [raises exn fn] asserts that [fn ()] raises [exn].

    Uses structural equality (the [=] operator) to compare exceptions. This
    works for most exceptions but may not behave as expected for exceptions
    carrying mutable data, closures, or abstract types. Use {!raises_match} for
    such cases. *)

val raises_match :
  ?here:Check.here ->
  ?pos:Check.pos ->
  ?msg:string ->
  (exn -> bool) ->
  (unit -> 'a) ->
  unit
(** [raises_match pred fn] asserts that [fn ()] raises an exception matching
    [pred]. *)

val no_raise :
  ?here:Check.here -> ?pos:Check.pos -> ?msg:string -> (unit -> 'a) -> 'a
(** [no_raise fn] asserts that [fn ()] does not raise, and returns its result.
*)

(** {1 Custom Failures} *)

val fail : ?here:Check.here -> ?pos:Check.pos -> string -> 'a
(** [fail msg] fails the test with message [msg]. *)

val failf :
  ?here:Check.here ->
  ?pos:Check.pos ->
  ('a, Format.formatter, unit, 'b) format4 ->
  'a
(** [failf fmt ...] fails the test with a formatted message. *)

(** {1 Test Control} *)

val skip : ?reason:string -> unit -> 'a
(** [skip ?reason ()] skips the current test. Use when a test cannot run due to
    environment constraints.

    {[
      test "windows only" (fun () ->
          if not Sys.win32 then skip ~reason:"not on Windows" ()
            (* ... windows-specific test ... *))
    ]} *)

(** {1 Snapshot Testing} *)

val snapshot :
  ?here:Check.here -> ?pos:Check.pos -> ?name:string -> string -> unit
(** [snapshot ~pos:__POS__ actual] compares [actual] against a stored snapshot.
    Create snapshots with [WINDTRAP_UPDATE=1] or [~update:true]. *)

val snapshot_pp :
  ?here:Check.here -> ?pos:Check.pos -> ?name:string -> 'a Pp.t -> 'a -> unit
(** [snapshot_pp ~pos:__POS__ pp value] pretty-prints [value] and snapshots the
    result. *)

val snapshotf :
  ?here:Check.here ->
  ?pos:Check.pos ->
  ?name:string ->
  ('a, Format.formatter, unit, unit) format4 ->
  'a
(** [snapshotf ~pos:__POS__ "format" ...] formats and snapshots the result. *)

(** {1 Output Testing} *)

val output : unit -> string
(** [output ()] returns and consumes stdout since last expect/output call. Use
    for post-processing non-deterministic output before comparison.

    {[
      test "timing" (fun () ->
          slow_operation ();
          let out = output () in
          let masked = mask_timing out in
          expect masked "Done in XXms")
    ]} *)

val expect : string -> unit
(** [expect expected] compares stdout (normalized) against [expected].

    Normalization strips trailing whitespace per line and removes
    leading/trailing blank lines. This matches ppx_expect's default behavior.

    {[
      test "greet" (fun () ->
          print_endline "  Hello  ";
          expect "Hello")
    ]}

    Multiple expects consume output incrementally:

    {[
      test "multi" (fun () ->
          print_endline "First";
          expect "First";
          print_endline "Second";
          expect "Second")
    ]} *)

val expect_exact : string -> unit
(** [expect_exact expected] compares stdout exactly, with no normalization. Use
    when whitespace is semantically significant. *)

val capture : (unit -> 'a) -> string -> 'a
(** [capture fn expected] runs [fn], captures stdout, compares (normalized).

    {[
      test "compute" (fun () ->
          let x =
            capture
              (fun () ->
                print_string "ok";
                42)
              "ok"
          in
          equal Testable.int 42 x)
    ]} *)

val capture_exact : (unit -> 'a) -> string -> 'a
(** [capture_exact fn expected] is like {!capture} but with no normalization. *)

(** {1 Property-Based Testing}

    Property tests generate random inputs and check that properties hold for all
    generated values. On failure, counterexamples are automatically shrunk to
    find minimal failing cases.

    Testables with generators (like [Testable.int], [Testable.list], etc.) can
    be used directly for property testing:

    {[
      open Windtrap

      let () =
        run "Properties"
          [
            prop "reverse is involutive"
              Testable.(list int)
              (fun l -> List.rev (List.rev l) = l);
            prop "append length"
              Testable.(pair (list int) (list int))
              (fun (l1, l2) ->
                List.length (l1 @ l2) = List.length l1 + List.length l2);
          ]
    ]} *)

module Gen = Windtrap_prop.Gen
(** Random value generators with integrated shrinking. For advanced use cases
    where you need to create custom generators. *)

val prop :
  ?config:Windtrap_prop.Prop.config ->
  ?pos:Test.pos ->
  ?tags:Tag.t ->
  ?timeout:float ->
  string ->
  'a Testable.t ->
  ('a -> bool) ->
  test
(** [prop name testable law] creates a property test.

    Generates random values using [testable]'s generator and checks that [law]
    holds for all. If a counterexample is found, it is automatically shrunk to
    find a minimal failing case.

    {[
      prop "reverse is involutive"
        Testable.(list int)
        (fun l -> List.rev (List.rev l) = l)
    ]}

    @param config Property test configuration (count, seed, etc.)
    @param timeout Maximum time for all test cases.
    @raise Invalid_argument
      if [testable] has no generator. Built-in testables like [Testable.int]
      always have generators; custom testables need [Testable.make ~gen]. *)

val prop' :
  ?config:Windtrap_prop.Prop.config ->
  ?pos:Test.pos ->
  ?tags:Tag.t ->
  ?timeout:float ->
  string ->
  'a Testable.t ->
  ('a -> unit) ->
  test
(** [prop' name testable fn] is like {!prop} but uses Windtrap assertions.

    The property passes if [fn] completes without raising. Use Windtrap
    assertions like {!equal} inside [fn].

    {[
      prop' "sorted list is sorted"
        Testable.(list int)
        (fun l ->
          let sorted = List.sort Int.compare l in
          (* Use Windtrap assertions *)
          is_true (is_sorted sorted))
    ]} *)

val prop2 :
  ?config:Windtrap_prop.Prop.config ->
  ?pos:Test.pos ->
  ?tags:Tag.t ->
  ?timeout:float ->
  string ->
  'a Testable.t ->
  'b Testable.t ->
  ('a -> 'b -> bool) ->
  test
(** [prop2 name t1 t2 law] is a convenience for two-argument properties. *)

val prop3 :
  ?config:Windtrap_prop.Prop.config ->
  ?pos:Test.pos ->
  ?tags:Tag.t ->
  ?timeout:float ->
  string ->
  'a Testable.t ->
  'b Testable.t ->
  'c Testable.t ->
  ('a -> 'b -> 'c -> bool) ->
  test
(** [prop3 name t1 t2 t3 law] is a convenience for three-argument properties. *)

val assume : bool -> unit
(** [assume b] discards the current test case if [b] is false.

    Use to filter out invalid inputs. Prefer constrained generators when
    possible, as excessive discarding can cause tests to give up.

    {[
      prop "division"
        Testable.(pair int int)
        (fun (a, b) ->
          assume (b <> 0);
          (a / b * b) + (a mod b) = a)
    ]} *)

val reject : unit -> 'a
(** [reject ()] unconditionally discards the current test case. *)

val collect : string -> unit
(** [collect label] records [label] for the current property case.

    Labels are reported as a distribution over successful (non-discarded)
    property cases. Calling [collect] multiple times with the same label in one
    case counts once. *)

val classify : string -> bool -> unit
(** [classify label cond] is [if cond then collect label]. *)

val cover : label:string -> at_least:float -> bool -> unit
(** [cover ~label ~at_least cond] declares a required minimum coverage for
    [label], and records a hit when [cond] is true.

    Coverage is measured over successful (non-discarded) cases and checked once
    the property has generated all required cases. If unmet, the property fails
    with a coverage report.

    @raise Invalid_argument
      if [at_least] is not in [[0.0, 100.0]], or if the same [label] is used
      with conflicting thresholds in one property run. *)
