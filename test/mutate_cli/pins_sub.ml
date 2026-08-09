(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The sibling of pins_add.ml, and its mirror image: it pins [sub] and
   merely reaches [add], so its own report calls [add]'s mutant a
   survivor. Neither executable is wrong about what it ran; only the
   merge knows that every mutant but one is killed somewhere. *)

open Windtrap
module Calc = Mutcli_fixture.Calc

let () =
  run "pins_sub"
    [
      group "sub"
        [
          test "subtracts two positives" (fun () -> equal int 6 (Calc.sub 10 4));
          test "subtracts to zero" (fun () -> equal int 0 (Calc.sub 4 4));
          test "subtracts a negative" (fun () ->
              equal int 4 (Calc.sub (-1) (-5)));
        ];
      group "add"
        [ test "add is nonzero" (fun () -> is_true (Calc.add 3 4 <> 0)) ];
      group "shared"
        [ test "shared is not 99" (fun () -> is_true (Calc.shared 1 2 <> 99)) ];
    ]
