(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The [windtrap] binary dispatches to its two commands, which merge the data
   files of instrumented test runs and report them. *)

module Os = Windtrap.Private.Os

let usage =
  {|usage: windtrap <command> [OPTIONS]

COMMANDS:
  coverage
      Merge .coverage files and report; --min gates, --json exports.

  mutants
      Merge .mutants verdict files and report the project's survivors.

OPTIONS:
  -h, --help
      Print this help and exit.

See `windtrap <command> --help` for a subcommand's options.|}

let help = "windtrap - reports merged from instrumented test runs\n\n" ^ usage

(* A wrong command is answered with the commands there are. *)
let refuse message =
  Os.say message;
  prerr_endline usage;
  2

let () =
  exit
  @@
  match Array.to_list Sys.argv with
  | _ :: "coverage" :: args -> Coverage_cmd.run args
  | _ :: "mutants" :: args -> Mutate_cmd.run args
  | _ :: ("-h" | "--help" | "-help") :: _ ->
      print_endline help;
      0
  | _ :: command :: _ -> refuse (Printf.sprintf "unknown command '%s'" command)
  | _ -> refuse "no command given"
