(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The default [Expect_test_config].

    The code that [ppx_windtrap] generates for a [let%expect_test] names the
    module [Expect_test_config] unqualified. Every stanza preprocessed with
    [ppx_windtrap] links this library, so the name is this module unless a
    module of the same name in scope shadows it. A project shadows it to wrap
    the bodies of its expect tests with {!run} and to rewrite their captured
    output with {!sanitize}.

    The shadowing is lexical, so a definition governs the expect tests below it
    in its file. The generated code names both values, so a shadowing module
    must define both, which it does by including this one.

    {[
    module Expect_test_config = struct
      include Expect_test_config

      let sanitize s = String.concat "/" (String.split_on_char '\\' s)
    end
    ]}

    The body of a [let%expect_test] is synchronous. The generated code applies
    {!run} at the type [(unit -> unit) -> unit], so a [run] of another type does
    not compile, and the error is located at the [let%expect_test]. *)

val run : (unit -> unit) -> unit
(** [run f] is [f ()]. The generated code passes the body of each
    [let%expect_test] to [run] as [f], inside the test, so an override may call
    what a running test may call. The body of a [let%test] does not go through
    [run].

    An override must call [f] once. If it never calls [f], the test fails at the
    first node of the body, which it never reached, and a body without a node
    passes with nothing checked. *)

val sanitize : string -> string
(** [sanitize s] is [s]. The generated code applies [sanitize] to the captured
    output that each [[%expect]], [[%expect_exact]] and [[%expect.output]]
    reads. [s] is the output as the test wrote it, before [Windtrap.expect]
    normalizes whitespace. An override removes what changes from one run to the
    next, such as a timestamp or a temporary path.

    The result is the text that is compared, and also the text that a correction
    writes as the new baseline, so an override must be a function of [s] alone.
    A body that calls [Windtrap.output] itself reads the output unsanitized. *)
