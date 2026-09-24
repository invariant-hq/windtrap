(* Windtrap-authored runner main for the corrected escaped_strings.ml
   alone, the correction known not to converge (see dune). *)

module _ = Escaped_strings

let () = Ppx_windtrap_runtime.Ppx_runtime.init Sys.argv
let () = Ppx_windtrap_runtime.Ppx_runtime.exit ()
