(* Windtrap-authored runner main for chdir.ml alone (mirrors the
   inline_tests backend's generated runner). Its test leaves the process
   in a directory it deleted, so no other fixture shares its process. *)

module _ = Chdir

let () = Ppx_windtrap_runtime.Ppx_runtime.init Sys.argv
let () = Ppx_windtrap_runtime.Ppx_runtime.exit ()
