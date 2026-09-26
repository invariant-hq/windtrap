(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The main that the inline_tests backend generates for linked_scene. The
   reference links linked_scene, and with it linked_shapes, whose tests
   register in this process too. *)

let _ = Linked_scene.Scene.total
let () = Ppx_windtrap_runtime.Ppx_runtime.init Sys.argv
let () = Ppx_windtrap_runtime.Ppx_runtime.exit ()
