(* Windtrap-authored runner main for the corrected chdir.ml alone: its
   test leaves the process in a directory it deleted. *)

module _ = Chdir

let () = Ppx_windtrap_runtime.Ppx_runtime.init Sys.argv
let () = Ppx_windtrap_runtime.Ppx_runtime.exit ()
