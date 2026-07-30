(* Windtrap-authored runner main for quarantined divergent fixtures
   (see ../../../RESULTS.md). *)

module _ = Similar_distinct_outputs

let () = Ppx_windtrap_runtime.Ppx_runtime.init Sys.argv
let () = Ppx_windtrap_runtime.Ppx_runtime.exit ()
