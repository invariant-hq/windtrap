(* The tool under test: output worth pinning as a snapshot baseline. *)

let help () =
  String.concat "\n"
    [
      "Usage: mytool [OPTIONS] COMMAND";
      "";
      "Commands:";
      "  build    Build the project";
      "  test     Run the tests";
      "";
      "Options:";
      "  --help   Show this help";
    ]

let report ~rows = Printf.sprintf "processed %d rows\nstatus: ok" rows
