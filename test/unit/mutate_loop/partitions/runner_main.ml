(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The runner of a two-partition inline library, as the inline_tests
   backend generates it. The aliases force link order: both partitions
   register their tests before the protocol runs. *)

module _ = Part_a
module _ = Part_b

let () = Ppx_windtrap_runtime.Ppx_runtime.init Sys.argv
let () = Ppx_windtrap_runtime.Ppx_runtime.exit ()
