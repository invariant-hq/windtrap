(* Windtrap-authored runner main for the armed-inline fixture, mirroring
   the inline_tests backend's generated runner as the conformance corpus
   does. The module alias forces link order: the fixture registers its
   test before the protocol runs. *)

module _ = Inline_armed

let () = Ppx_windtrap_runtime.Ppx_runtime.init Sys.argv
let () = Ppx_windtrap_runtime.Ppx_runtime.exit ()
