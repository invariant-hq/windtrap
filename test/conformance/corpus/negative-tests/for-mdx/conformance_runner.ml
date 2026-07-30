(* Windtrap-authored runner main for this corpus directory (mirrors the
   inline_tests backend's generated runner). *)

module _ = Foo

let () = Ppx_windtrap_runtime.Ppx_runtime.init Sys.argv
let () = Ppx_windtrap_runtime.Ppx_runtime.exit ()
