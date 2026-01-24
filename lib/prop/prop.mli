(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Property-based testing runner.

    This module provides the core property checking logic. It generates random
    inputs, tests properties, and shrinks failing cases to find minimal
    counterexamples. *)

exception Reject
(** Raised to discard the current test case. Prefer {!assume} or {!reject}
    instead of raising directly. *)

val assume : bool -> unit
(** [assume b] discards the current test case if [b] is false. Prefer
    constrained generators when possible, as excessive discarding causes the
    test to return {!result.Gave_up}. *)

val reject : unit -> 'a
(** [reject ()] unconditionally discards the current test case. *)

(** {1 Configuration} *)

type config = {
  count : int;  (** Number of successful tests required. Default: 100. *)
  max_gen : int;
      (** Maximum generation attempts (including discards). Default: 300. *)
  max_shrink : int;  (** Maximum shrink steps. Default: 100. *)
  seed : int option;
      (** Random seed. [None] reads the [WINDTRAP_SEED] environment variable,
          falling back to a random seed. *)
}
(** Property test configuration. *)

val default_config : config
(** Default configuration: 100 tests, 300 max gen, 100 max shrink, no fixed
    seed. *)

val set_default_seed : int option -> unit
(** [set_default_seed seed] sets a global default seed used when [config.seed]
    is [None] and [WINDTRAP_SEED] is not set. Called by {!Windtrap.run} to
    propagate the CLI [--seed] flag to property tests. *)

(** {1 Results} *)

(** Result of running a property test. *)
type result =
  | Success of { count : int; discarded : int }
      (** Property passed for [count] inputs, discarding [discarded] cases. *)
  | Failed of {
      count : int;
      discarded : int;
      seed : int;
      counterexample : string;
      shrunk_counterexample : string;
      shrink_steps : int;
    }  (** Property failed. Shows original and shrunk counterexamples. *)
  | Error of {
      count : int;
      seed : int;
      counterexample : string;
      exn : exn;
      backtrace : string;
    }  (** Property raised an unexpected exception. *)
  | Gave_up of { count : int; discarded : int; seed : int }
      (** Generation attempts reached [max_gen] before [count] tests passed. *)

(** {1 Running Properties} *)

val check :
  ?config:config ->
  ?rand:Random.State.t ->
  'a Arbitrary.t ->
  ('a -> bool) ->
  result
(** [check arb prop] tests that [prop] holds for all values from [arb].

    On failure, attempts to shrink the counterexample to find a minimal failing
    case. Returns detailed result with counterexample and shrink info.

    @param config Test configuration. Uses {!default_config} if not provided.
    @param rand
      Random state. If not provided, derives from config seed or environment. *)
