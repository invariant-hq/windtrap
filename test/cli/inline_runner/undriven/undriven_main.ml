(* The defect shape exactly: an executable that links
   ppx_windtrap-preprocessed test code and whose main never speaks the
   runner protocol. The [link] reference forces the fixture unit into
   the link, so its initializers run (registering its test) and then
   nothing drives the registry: no (inline_tests) stanza built this
   binary, no Ppx_runtime.init runs in it. Before the guard this
   process exited 0 in silence, its goldens never checked against
   anything. *)

let () = Undriven_inline_lib.Inline_undriven.link
