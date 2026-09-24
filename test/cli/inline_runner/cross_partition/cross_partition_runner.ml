(* Windtrap-authored runner main for the cross-partition fixture (mirrors
   the inline_tests backend's generated runner, as masked_failure and the
   conformance corpus do). The module aliases force link order: both
   fixture modules initialize (registering their tests, and their
   partitions) before the protocol runs. *)

module _ = Crash
module _ = Stale

let () = Ppx_windtrap_runtime.Ppx_runtime.init Sys.argv
let () = Ppx_windtrap_runtime.Ppx_runtime.exit ()
