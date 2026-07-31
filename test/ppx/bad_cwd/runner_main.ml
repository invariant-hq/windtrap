(* Windtrap-authored runner main for the bad-cwd fixture (mirrors the
   inline_tests backend's generated runner, as the conformance corpus
   does). The module alias forces link order: the fixture module
   initializes — registering its test — before the protocol runs. *)

module _ = Inline_bad_cwd

let () = Ppx_windtrap_runtime.Ppx_runtime.init Sys.argv
let () = Ppx_windtrap_runtime.Ppx_runtime.exit ()
