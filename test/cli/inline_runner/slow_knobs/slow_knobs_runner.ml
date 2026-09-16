(* Windtrap-authored runner main for the slow-knobs fixture (mirrors the
   inline_tests backend's generated runner, as the conformance corpus
   does). The module alias forces link order: the fixture module
   initializes — registering its tests — before the protocol runs. *)

module _ = Inline_slow_knobs

let () = Ppx_windtrap_runtime.Ppx_runtime.init Sys.argv
let () = Ppx_windtrap_runtime.Ppx_runtime.exit ()
