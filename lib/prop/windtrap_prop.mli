(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Property-based testing for Windtrap.

    This library provides generators and a property runner for property-based
    testing. Use with the main [Windtrap] library for full test integration.

    {1 Quick Start}

    With the main Windtrap library, use [Testable] values directly for property
    testing:

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

module Gen = Gen
(** Random value generators with integrated shrinking. *)

module Arbitrary = Arbitrary
(** Arbitrary values: generators bundled with printers. Used internally by
    [Prop.check]. Most users should use [Testable] values with [Windtrap.prop]
    instead. *)

module Prop = Prop
(** Property runner and configuration. *)

val assume : bool -> unit
(** [assume b] discards the current test case if [b] is false. Prefer
    constrained generators when possible, as excessive discarding causes the
    test to give up. *)

val reject : unit -> 'a
(** [reject ()] unconditionally discards the current test case. *)
