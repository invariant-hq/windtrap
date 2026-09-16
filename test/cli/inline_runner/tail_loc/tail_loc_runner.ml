(* Windtrap-authored runner main for the tail-loc fixture (mirrors the
   inline_tests backend's generated runner, as the conformance corpus
   does). The module alias forces link order: the fixture module
   initializes — registering its test — before the protocol runs. *)

module _ = Inline_tail_loc

let () = Ppx_windtrap_runtime.Ppx_runtime.init Sys.argv
let () = Ppx_windtrap_runtime.Ppx_runtime.exit ()
