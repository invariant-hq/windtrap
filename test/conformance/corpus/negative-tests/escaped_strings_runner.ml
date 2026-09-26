(* Windtrap-authored runner main for escaped_strings.ml alone (mirrors the
   inline_tests backend's generated runner). Its golden holds from OCaml 5.2
   on, so its rules are gated and no other fixture shares its process. *)

module _ = Escaped_strings

let () = Ppx_windtrap_runtime.Ppx_runtime.init Sys.argv
let () = Ppx_windtrap_runtime.Ppx_runtime.exit ()
