(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Test assertions.

    All assertions raise {!Failure.Check_failure} on failure. The optional
    [?msg] overrides the default failure message, and [?here]/[?pos] attach
    source locations to diagnostics. *)

(** {1 Source Position Types} *)

type pos = Failure.pos
(** Source position: [(filename, line, start_char, end_char)]. Use with
    [__POS__]. *)

type here = Failure.here
(** Source position from [Lexing.position]. *)

(** {1 Equality Assertions} *)

val equal :
  ?here:here -> ?pos:pos -> ?msg:string -> 'a Testable.t -> 'a -> 'a -> unit
(** [equal testable expected actual] asserts that [expected] equals [actual]
    using [testable]'s equality. When the testable provides a custom [check]
    function, structured diff output is produced on failure. *)

val not_equal :
  ?here:here -> ?pos:pos -> ?msg:string -> 'a Testable.t -> 'a -> 'a -> unit
(** [not_equal testable a b] asserts that [a] does not equal [b]. *)

(** {1 Boolean Assertions} *)

val is_true : ?here:here -> ?pos:pos -> ?msg:string -> bool -> unit
(** [is_true b] asserts that [b] is [true]. *)

val is_false : ?here:here -> ?pos:pos -> ?msg:string -> bool -> unit
(** [is_false b] asserts that [b] is [false]. *)

(** {1 Option Assertions} *)

val is_some : ?here:here -> ?pos:pos -> ?msg:string -> 'a option -> unit
(** [is_some opt] asserts that [opt] is [Some _]. *)

val is_none : ?here:here -> ?pos:pos -> ?msg:string -> 'a option -> unit
(** [is_none opt] asserts that [opt] is [None]. *)

val some :
  ?here:here ->
  ?pos:pos ->
  ?msg:string ->
  'a Testable.t ->
  'a ->
  'a option ->
  unit
(** [some testable expected opt] asserts that [opt] is [Some expected]. *)

(** {1 Result Assertions} *)

val is_ok : ?here:here -> ?pos:pos -> ?msg:string -> ('a, 'b) result -> unit
(** [is_ok r] asserts that [r] is [Ok _]. *)

val is_error : ?here:here -> ?pos:pos -> ?msg:string -> ('a, 'b) result -> unit
(** [is_error r] asserts that [r] is [Error _]. *)

val ok :
  ?here:here ->
  ?pos:pos ->
  ?msg:string ->
  'a Testable.t ->
  'a ->
  ('a, 'b) result ->
  unit
(** [ok testable expected r] asserts that [r] is [Ok expected]. *)

val error :
  ?here:here ->
  ?pos:pos ->
  ?msg:string ->
  'b Testable.t ->
  'b ->
  ('a, 'b) result ->
  unit
(** [error testable expected r] asserts that [r] is [Error expected]. *)

(** {1 Exception Assertions} *)

val raises :
  ?here:here -> ?pos:pos -> ?msg:string -> exn -> (unit -> 'a) -> unit
(** [raises exn fn] asserts that [fn ()] raises [exn]. Exceptions are compared
    with structural equality. *)

val raises_match :
  ?here:here -> ?pos:pos -> ?msg:string -> (exn -> bool) -> (unit -> 'a) -> unit
(** [raises_match pred fn] asserts that [fn ()] raises an exception satisfying
    [pred]. *)

val no_raise : ?here:here -> ?pos:pos -> ?msg:string -> (unit -> 'a) -> 'a
(** [no_raise fn] asserts that [fn ()] does not raise, and returns its result.
*)

val raises_invalid_arg :
  ?here:here -> ?pos:pos -> ?msg:string -> string -> (unit -> 'a) -> unit
(** [raises_invalid_arg expected_msg fn] asserts that [fn ()] raises
    [Invalid_argument] with message [expected_msg]. On failure, shows the
    expected vs actual exception message. *)

val raises_failure :
  ?here:here -> ?pos:pos -> ?msg:string -> string -> (unit -> 'a) -> unit
(** [raises_failure expected_msg fn] asserts that [fn ()] raises [Failure] with
    message [expected_msg]. On failure, shows the expected vs actual exception
    message. *)

(** {1 Custom Failures} *)

val fail : ?here:here -> ?pos:pos -> string -> 'a
(** [fail msg] fails the current test unconditionally with message [msg]. *)

val failf :
  ?here:here -> ?pos:pos -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [failf fmt ...] fails the current test with a formatted message. *)
