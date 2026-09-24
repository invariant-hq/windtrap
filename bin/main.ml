(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The `windtrap` binary: subcommand dispatch only. Both subcommands
   merge instrumentation data and render it. Test executables are their
   own runners, so nothing else lives here. *)

module Os = Windtrap.Private.Os

let usage = "usage: windtrap <command> [OPTIONS]"

let commands =
  {|COMMANDS:
  coverage
      Merge .coverage files and report; --min gates, --json exports.

  mutants
      Merge .mutants verdict files and report the project's survivors.

OPTIONS:
  -h, --help
      Print this help and exit.

See `windtrap <command> --help` for a subcommand's options.|}

let help =
  "windtrap - reports merged from instrumented test runs\n\n" ^ usage ^ "\n\n"
  ^ commands

(* A wrong command is answered with the ones there are. *)
let refuse message =
  Os.say message;
  prerr_endline (usage ^ "\n\n" ^ commands);
  exit 2

let () =
  match Array.to_list Sys.argv with
  | _ :: "coverage" :: args -> exit (Coverage_cmd.run args)
  | _ :: "mutants" :: args -> exit (Mutate_cmd.run args)
  | _ :: ("-h" | "--help" | "-help") :: _ ->
      print_endline help;
      exit 0
  | _ :: command :: _ -> refuse (Printf.sprintf "unknown command '%s'" command)
  | _ -> refuse "no command given"
