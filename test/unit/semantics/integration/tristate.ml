(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A [None] that shadowed.ml opens: declared in another file, so the
   rewriter does not see it. *)
type t = None | Partial
