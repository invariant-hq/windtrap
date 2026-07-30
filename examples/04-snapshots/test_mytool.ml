(* Snapshot tests: identity is the mandatory name, storage is
   __snapshots__/<file>/<name>.snap next to this source file. Checking is
   read-only — a green run always means "matched a committed baseline";
   accept changes with WINDTRAP_UPDATE=1 dune runtest and review the diff. *)

open Windtrap

let () =
  run "cli"
    [
      test "cli help" (fun () -> snapshot "help" (Mytool.help ()));
      test "report" (fun () -> snapshot "report" (Mytool.report ~rows:42));
    ]
