(* Windtrap-authored runner main for quarantined divergent fixtures
   (see ../../../RESULTS.md). *)

module _ = Control_chars

let () = Ppx_windtrap_runtime.Ppx_runtime.init Sys.argv
let () = Ppx_windtrap_runtime.Ppx_runtime.exit ()
