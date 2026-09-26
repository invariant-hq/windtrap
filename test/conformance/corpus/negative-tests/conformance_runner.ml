(* Windtrap-authored runner main for this corpus directory (mirrors the
   inline_tests backend's generated runner). The module aliases force
   link order: every fixture module initializes (registering its tests)
   before the protocol runs. Import_test pulls in Export_test. Chdir
   and Escaped_strings run in processes of their own (chdir_runner.ml,
   escaped_strings_runner.ml). *)

module _ = Exact
module _ = Flexible
module _ = Import_test
module _ = Missing
module _ = Normal_strings
module _ = Semicolon
module _ = Spacing
module _ = String_extension_syntax
module _ = String_padding
module _ = Trailing
module _ = Unidiomatic_syntax

let () = Ppx_windtrap_runtime.Ppx_runtime.init Sys.argv
let () = Ppx_windtrap_runtime.Ppx_runtime.exit ()
