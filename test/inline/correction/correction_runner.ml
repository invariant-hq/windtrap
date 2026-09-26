(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The runner main of this directory, as the inline_tests backend generates
   it. The aliases link the fixtures, which register their tests, before the
   protocol runs. *)

module _ = Sanitized
module _ = Trailing
module _ = Unreached

let () = Ppx_windtrap_runtime.Ppx_runtime.init Sys.argv
let () = Ppx_windtrap_runtime.Ppx_runtime.exit ()
