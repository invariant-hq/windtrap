(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Standalone ppxlib driver over both rewriters, in the order of the driver
   that dune builds for a stanza naming both backends: the mutation rewriter
   runs first and the coverage rewriter receives its guards. *)

let () = Ppxlib.Driver.standalone ()
