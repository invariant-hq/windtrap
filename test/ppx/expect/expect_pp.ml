(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Standalone ppxlib driver over ppx_windtrap, for the golden expansion
   tests: [pp.exe --impl fixture.ml] prints the rewritten source. *)

let () = Ppxlib.Driver.standalone ()
