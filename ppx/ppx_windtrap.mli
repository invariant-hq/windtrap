(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The inline-test rewriter.

    The module exports no value, and linking it into a driver is its whole
    interface. When the module is initialized it registers, under the name
    [ppx_windtrap], the expansion of the extensions [expect_test] and [test] on
    structure items, and a check of the whole implementation file that runs
    after them. Generated code names the modules [Windtrap],
    [Ppx_windtrap_runtime] and [Expect_test_config], from the libraries
    [windtrap], [ppx_windtrap.runtime] and [ppx_windtrap.config] that every
    stanza preprocessed with [ppx_windtrap] links, and it compiles without a
    warning under [-w +a -warn-error +a].

    {1:expect_test Expect tests}

    [let%expect_test NAME = BODY] registers a test when the structure that holds
    it is evaluated: at module load, or at each application of an enclosing
    functor. The test goes in the group of the enclosing [module%test], else in
    its file's group, and its declaration site is the extension point. [NAME] is
    a string literal, or [_] for the name [line_N], [N] the line of the
    extension point. [[@tags "a"]] or [[@tags ("a", "b")]] on [NAME] gives the
    test its tags; every other attribute of [NAME] or of the binding is dropped,
    [[@@tags]] included. [BODY] has type [unit].

    [BODY] is evaluated out of tail position, as by
    [match (BODY : unit) with () -> () | exception e -> raise_notrace e], so an
    assertion that ends it is located at its own line. An exception from [BODY]
    keeps the backtrace that [BODY] recorded.

    [Expect_test_config.run], applied at the type [(unit -> unit) -> unit],
    receives [fun () -> BODY]. The name is not qualified: the definition of
    [Expect_test_config] in scope where the test is written governs it, and a
    [run] of another type is a type error located at the test.

    Inside [BODY]:
    - [[%expect LIT]] and [[%expect_exact LIT]], [LIT] a string literal, mark
      the node reached, then are [Windtrap.expect] and [Windtrap.expect_exact]
      of [Expect_test_config.sanitize (Windtrap.output ())] against [LIT] at the
      node's position. A bare [[%expect]] is the empty literal.
    - [[%expect.output]] is [Expect_test_config.sanitize (Windtrap.output ())].

    Each node's attributes are carried onto the expression that replaces it.

    When [BODY] returns, the output that it wrote after its last node, or after
    its last [Windtrap.output] call, is read as [[%expect.output]] reads it and
    checked as the payload of an absent [[%expect]] node
    ([Ppx_windtrap_runtime.Ppx_runtime.expect_test]). Blank output passes. Other
    output fails the test, and a correcting run appends [;] and an [[%expect]]
    node that holds it to [BODY]. When [BODY] raises, the exception is the
    test's failure and nothing more is checked.

    Then every [[%expect]] and [[%expect_exact]] node of [BODY] must have been
    reached by this run of the test. A node that was not fails the test, located
    at the first such node, and the test then keeps no correction. Each
    application of an enclosing functor registers a test of its own, which
    reaches its nodes or fails alone.

    {1:test Tests and groups}

    [let%test NAME = BODY] registers a test as [let%expect_test] does, with the
    same [NAME] and [[@tags]], and [BODY] does not go through
    [Expect_test_config.run].

    [module%test Name = M] defines the module [Name] and registers a group
    [Name] around it: the tests that [M]'s initialization registers, and the
    groups of nested [module%test], are its members. [[@@tags]] on the binding
    gives the group its tags; its other attributes stay on the module.

    {1:cookie The [inline_tests] cookie}

    Under the cookie [inline_tests] ["disabled"] or ["ignored"], each of the
    three forms expands to nothing, and a [module%test] defines no module. A
    dropped form is still refused for a bad [NAME], shape or [[@tags]], and a
    [let%expect_test] for a bad node; the rest of a dropped body is not checked.
    Under ["enabled"], and without the cookie, they expand as above.

    {1:library The [library-name] cookie}

    Under the cookie [library-name], which dune sets for a library stanza, each
    registration names that library, and only the inline runner of that library
    runs its tests. A process that links the library for another purpose, a test
    executable over its interface included, neither runs them nor fails for
    them. Without the cookie the tests belong to no library, and the executable
    that links them runs them through the runner protocol or exits [2].

    {1:refusals Refusals}

    Each of these is a compile error. A bad [NAME] or item shape is located at
    the whole test, a bad cookie at the command line, and the others at the
    attribute, node or name at fault:
    - A [NAME] that is not a string literal or [_]:
      [Expected let%expect_test "name" = ... or let%expect_test _ = ...], and
      [let%test] in place of [let%expect_test] for a [let%test].
    - A [let%expect_test] that is not one non-recursive binding:
      [Expected let%expect_test <name> = <expr>]. A [%test] item that is not one
      non-recursive binding or one named module, [module%test _] included:
      [Expected let%test "name" = ..., let%test _ = ..., or module%test Name =
       ...].
    - A [[@tags]] payload of another shape:
      [Expected [@tags "..."] or [@tags ("...", ...)]].
    - An [[%expect]] or [[%expect_exact]] payload that is not a string literal:
      [Expected a string literal payload]. An [[%expect.output]] with a payload:
      [[%expect.output] takes no payload].
    - [[%expect]], [[%expect_exact]] or [[%expect.output]] outside the body of a
      [let%expect_test]: [[%NAME] must appear inside a let%expect_test body].
    - An extension named [expectation], [expectation.X], or [expect.X] other
      than [expect.output], anywhere:
      [[%NAME] is not supported by ppx_windtrap].
    - An attribute named [expect], [expect_exact], [expectation], [expect.X] or
      [expectation.X], anywhere:
      [attribute NAME is not supported by ppx_windtrap].
    - Three of these refusals then name what to write instead:
      [; call Windtrap.fail at the point the body must not reach] for
      [[%expect.unreachable]], [; use [%expect], which must be reached] for
      [[%expect.if_reached]], and
      [; catch and print the exception before an [%expect]] for an
      [expect.uncaught_exn] attribute.
    - An [inline_tests] cookie of another string value:
      [invalid 'inline_tests' cookie (VALUE), expected one of: enabled, disabled
       or ignored]. *)
