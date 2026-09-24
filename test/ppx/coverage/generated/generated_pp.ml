(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Standalone ppxlib driver over the coverage rewriter, behind the stand-in
   deriver of ../../generated: [generated_pp.exe --impl fixture.ml] prints
   the rewritten source. *)

let () = Ppxlib.Driver.standalone ()
