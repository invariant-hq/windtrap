(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A standalone ppxlib driver over the two instrumenting rewriters, linked so
   that under [-apply] the coverage rewriter runs first and the mutation
   rewriter receives its visits. *)

let () = Ppxlib.Driver.standalone ()
