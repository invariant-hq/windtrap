(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* One of the two test executables over Mutcli_fixture.Calc. It pins
   [add] and merely reaches [sub], so its own mutation report calls
   [sub]'s mutant a survivor — which is a lie about the project, because
   the sibling executable kills it. *)

open Windtrap
module Calc = Mutcli_fixture.Calc

let () =
  run "pins_add"
    [
      group "add"
        [
          test "adds two positives" (fun () -> equal int 7 (Calc.add 3 4));
          test "adds zeroes" (fun () -> equal int 0 (Calc.add 0 0));
          test "adds across zero" (fun () -> equal int 4 (Calc.add (-1) 5));
        ];
      (* Reaches [sub] and pins nothing about it. *)
      group "sub"
        [ test "sub is nonzero" (fun () -> is_true (Calc.sub 10 4 <> 0)) ];
      (* Reaches [shared] and pins nothing about it, as the sibling also
         does: a survivor everywhere, whose witnesses are the union. *)
      group "shared"
        [ test "shared is nonzero" (fun () -> is_true (Calc.shared 3 4 <> 0)) ];
    ]
