(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The main that the inline_tests backend generates. Each alias links a
   fixture, which registers its tests as it initialises, before the protocol
   runs. *)

module _ = Crash
module _ = Masked
module _ = Release
module _ = Sanitized
module _ = Stale
module _ = Tail
module _ = Trailing
module _ = Unreached

let () = Ppx_windtrap_runtime.Ppx_runtime.init Sys.argv
let () = Ppx_windtrap_runtime.Ppx_runtime.exit ()
