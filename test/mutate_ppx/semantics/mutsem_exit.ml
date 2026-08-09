(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The instrumented half of the exit-code pair: it runs the INSTRUMENTED
   copy of mutsem_boom.ml. baseline/mutsem_exit.ml is the same line
   against [Mutsem_baseline], and dune diffs both outputs against one
   golden. See mutsem_boom.ml for what the golden pins and why the code
   under test lives in a library rather than here. *)

let () = Mutsem_fixtures.Mutsem_boom.main ()
