(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The list is built here, uninstrumented, so the guard of [Grower] is the
   only site its test passes, and that once. Past the guard, a negative size
   counts away from zero and the list grows by 24 bytes a step. It stops at
   [floor], some 1.2 GB on a 64-bit system, more than any memory ceiling of
   this suite: should the ceiling fail to stop it, the test fails and the
   mutant reads killed, in a second or two, where it would otherwise grow
   until the deadline or the machine's memory ends it. *)

open Windtrap

let floor = -50_000_000

let rec build n acc =
  if n = 0 || n = floor then acc else build (n - 1) (n :: acc)

let list n = Mutate_loop_grower.Grower.checked n (fun n -> build n [])

let () =
  exit
  @@ run "grow"
       [
         test "a list of three has three elements" (fun () ->
             equal int 3 (List.length (list 3)));
         test "a list of zero is empty" (fun () ->
             equal int 0 (List.length (list 0)));
         test "a negative size is refused" (fun () ->
             raises (Invalid_argument "Grower.checked: negative size")
               (fun () -> ignore (list (-1))));
       ]
