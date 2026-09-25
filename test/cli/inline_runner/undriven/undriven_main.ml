(* The defect shape exactly: an executable that links
   ppx_windtrap-preprocessed test code and whose main never speaks the
   runner protocol. The [link] reference forces the fixture unit into
   the link, so its initializers run (registering its test) and then
   nothing drives the registry: an executable has no inline runner, and
   no Ppx_runtime.init runs in it. Before the guard this
   process exited 0 in silence, its goldens never checked against
   anything. *)

let () = Inline_undriven.link
