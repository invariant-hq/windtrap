(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* An ordinary suite linking no instrumented module: the control for
   everything the mutation seam must not do. Asking it to mutate is the
   commonest misconfiguration of all — the backend on nothing — and the
   answer has to be a sentence, not a green run with no report. *)

open Windtrap

let () = run "plain" [ test "arithmetic" (fun () -> equal int 4 (2 + 2)) ]
