(* Windtrap-authored runner main for the undriven fixture (mirrors the
   inline_tests backend's generated runner, as the conformance corpus
   does). The [link] reference forces the fixture unit into the link, so
   it initializes — registering its test — before the protocol runs. *)

let () = Undriven_inline_lib.Inline_undriven.link
let () = Ppx_windtrap_runtime.Ppx_runtime.init Sys.argv
let () = Ppx_windtrap_runtime.Ppx_runtime.exit ()
