(* Windtrap-authored runner main over the corpus's corrected sources
   (mirrors the inline_tests backend's generated runner). The module
   aliases force link order: every module initializes, registering its
   tests, before the protocol runs. *)

module _ = Exact
module _ = Flexible
module _ = Missing
module _ = Normal_strings
module _ = Semicolon
module _ = Spacing
module _ = String_extension_syntax
module _ = String_padding
module _ = Trailing
module _ = Unidiomatic_syntax
module _ = Similar_distinct_outputs
module _ = Foo
module _ = Nine

let () = Ppx_windtrap_runtime.Ppx_runtime.init Sys.argv
let () = Ppx_windtrap_runtime.Ppx_runtime.exit ()
