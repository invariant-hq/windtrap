(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Standalone ppxlib driver over both rewriters, linked in the order that
   runs the coverage rewriter first: the mutation rewriter receives its
   visits. *)

let () = Ppxlib.Driver.standalone ()
