(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The `windtrap` binary: subcommand dispatch only. The one subcommand is
   [coverage] — test executables are their own runners,
   so nothing else lives here. *)

let usage =
  {|usage: windtrap <command> [OPTIONS]

COMMANDS:
  coverage    Merge .coverage files and report; --min gates, --json exports

OPTIONS:
  -h, --help  Print this help and exit

See `windtrap coverage --help` for the subcommand's options.|}

let () =
  match Array.to_list Sys.argv with
  | _ :: "coverage" :: args -> exit (Coverage_cmd.run args)
  | _ :: ("-h" | "--help" | "-help") :: _ ->
      print_endline usage;
      exit 0
  | _ :: command :: _ ->
      Printf.eprintf "windtrap: unknown command '%s'\n%s\n" command usage;
      exit 2
  | _ ->
      Printf.eprintf "windtrap: no command given\n%s\n" usage;
      exit 2
