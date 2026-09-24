(* Windtrap-authored runner main (mirrors the inline_tests backend's
   generated runner): one fixture per process, so its exit code and
   transcript are its own. *)

module _ = Control_chars

let () = Ppx_windtrap_runtime.Ppx_runtime.init Sys.argv
let () = Ppx_windtrap_runtime.Ppx_runtime.exit ()
