(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Test failure representation and reporting.

    This module defines the structured failure type used by all Windtrap
    assertions. Failures carry a message, optional expected/actual values, an
    optional inline diff, and source location information. *)

(** {1 Source Locations} *)

type pos = string * int * int * int
(** Source position: [(filename, line, start_char, end_char)]. Compatible with
    [__POS__]. *)

type here = Lexing.position
(** Source position from [ppx_here] or similar. *)

type location =
  | Pos of pos
  | Here of here
  | No_location
      (** Source location attached to a failure. [No_location] when no position
          information is available. *)

(** {1 Diff Output} *)

type diff_part = { text : string; highlight : bool }
(** A fragment of a diff. When [highlight] is [true], the fragment represents a
    changed region. *)

type diff_output = {
  expected_parts : diff_part list;
  actual_parts : diff_part list;
}
(** An inline diff between expected and actual values, split into highlighted
    and unhighlighted fragments. *)

(** {1 Failures} *)

type t = {
  message : string;
  expected : string option;
  actual : string option;
  diff : diff_output option;
  location : location;
}
(** A structured test failure. When both [diff] and [expected]/[actual] are
    present, [diff] takes precedence during pretty-printing. *)

exception Check_failure of t
(** Raised when a test assertion fails. *)

exception Skip_test of string option
(** Raised to skip a test at runtime. The optional string is the reason. *)

exception Timeout of float
(** Raised when a test exceeds its timeout. The float is the limit in seconds.
*)

(** {1 Constructors} *)

val make :
  ?here:here ->
  ?pos:pos ->
  ?expected:string ->
  ?actual:string ->
  ?diff:diff_output ->
  string ->
  t
(** [make ?here ?pos ?expected ?actual ?diff message] builds a failure value. If
    both [here] and [pos] are given, [here] takes precedence. *)

val raise_failure :
  ?here:here ->
  ?pos:pos ->
  ?expected:string ->
  ?actual:string ->
  ?diff:diff_output ->
  string ->
  'a
(** [raise_failure ...] is equivalent to [raise (Check_failure (make ...))].

    @raise Check_failure always. *)

val skip : ?reason:string -> unit -> 'a
(** [skip ?reason ()] skips the current test. Use when a test cannot run due to
    environment constraints (e.g., OS-specific tests).

    @raise Skip_test always. *)

(** {1 Pretty-printing} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] prints the failure to [ppf]. Outputs the source location (if
    any), the message, and either the inline diff or the expected/actual values.
    Uses ANSI colors: green for expected, red for actual. *)
