(* The runner main that the inline_tests backend generates, over
   linked_scene. The reference links the library, and with it
   linked_shapes, whose tests register in this process too. *)

let _ = Linked_scene.Scene.total
let () = Ppx_windtrap_runtime.Ppx_runtime.init Sys.argv
let () = Ppx_windtrap_runtime.Ppx_runtime.exit ()
