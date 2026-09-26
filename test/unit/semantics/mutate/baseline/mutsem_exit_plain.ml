(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The plain half of the exit-code pair: the same line as
   ../mutsem_exit.ml, against the UNINSTRUMENTED copy of mutsem_boom.ml.
   The two are written out rather than shared by copy_files because they
   differ in exactly one token - the library they call - and that token
   is the whole difference the pair exists to measure. *)

let () = Mutsem_baseline.Mutsem_boom.main ()
