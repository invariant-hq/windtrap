(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let usage =
  {|Usage: windtrap <command> [options]

Commands:
  coverage    Print coverage summary

Options:
  --help      Show this help message|}

let () =
  match Array.to_list Sys.argv |> List.tl with
  | "coverage" :: rest -> Coverage_cmd.run rest
  | ("--help" | "-help" | "-h") :: _ ->
      print_endline usage;
      exit 0
  | cmd :: _ ->
      Printf.eprintf "Error: unknown command '%s'\n\n%s\n" cmd usage;
      exit 1
  | [] ->
      Printf.eprintf "Error: no command specified\n\n%s\n" usage;
      exit 1
