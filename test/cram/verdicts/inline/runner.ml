(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The main that the inline_tests backend generates, over hello.ml, whose
   one expectation was never recorded. *)

module _ = Hello

let () = Ppx_windtrap_runtime.Ppx_runtime.init Sys.argv
let () = Ppx_windtrap_runtime.Ppx_runtime.exit ()
