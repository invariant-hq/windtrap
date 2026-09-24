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

    [Expect_test_config.run], applied at the type [(unit -> unit) -> unit],
    receives [fun () -> BODY]. The name is not qualified: the definition of
    [Expect_test_config] in scope where the test is written governs it, and a
    [run] of another type is a type error located at the test.

    Inside [BODY]:
    - [[%expect LIT]] and [[%expect_exact LIT]], [LIT] a string literal, are
      [Windtrap.expect] and [Windtrap.expect_exact] of
      [Expect_test_config.sanitize (Windtrap.output ())] against [LIT] at the
      node's position. A bare [[%expect]] is the empty literal.
    - [[%expect.output]] is [Expect_test_config.sanitize (Windtrap.output ())].

    Each node's attributes are carried onto the expression that replaces it.
    Nothing is checked after [BODY]: output written after its last node, and a
    node it never reaches, fail nothing.

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

    {1:refusals Refusals}

    Each of these is a compile error. A bad [NAME] or item shape is located at
    the whole test, a bad cookie at the command line, and the others at the
    attribute, node or name at fault:
    - A [NAME] that is not a string literal or [_]:
      [Expected let%expect_test "name" = ... or let%expect_test _ = ...], for a
      [let%test] too.
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
      [expectation.X], anywhere: [[@@NAME] is not supported by ppx_windtrap],
      spelled with [@@] whatever its placement.
    - An [inline_tests] cookie of another string value:
      [invalid 'inline_tests' cookie (VALUE), expected one of: enabled, disabled
       or ignored]. *)
