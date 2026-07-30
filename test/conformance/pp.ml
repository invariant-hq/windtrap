(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Standalone ppxlib driver over ppx_windtrap for the rejected set:
   [pp.exe --impl fixture.ml] must exit 1 with the explicit
   "not supported by ppx_windtrap" diagnostic. *)

let () = Ppxlib.Driver.standalone ()
