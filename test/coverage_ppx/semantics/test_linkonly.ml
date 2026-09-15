(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The runtime-only link contract (RFC Law 12, interchange contract):
   instrumented user code links against nothing but the [windtrap.runtime]
   runtime, injected by dune through the rewriter's ppx_runtime_libraries -
   never the windtrap core. This executable's dune stanza lists
   [covsem_fixtures] alone (itself a zero-dependency library), and this file
   never names a coverage or windtrap module: that it links and runs at all
   is the test. The checks below are a smoke check that the instrumented
   code still computes, including its registration at module load. Nothing
   here may reach windtrap's Pp/Env/Report, so the output is plain stdlib
   printing and not the suite dialect. *)

let failed = ref []
let check name cond = if not cond then failed := name :: !failed

let () =
  check "instrumented countdown computes"
    (String.equal (Covsem_fixtures.countdown 1_000) "done");
  check "instrumented while loop computes" (Covsem_fixtures.sum_while 10 = 55);
  check "instrumented try arm computes" (Covsem_fixtures.safe_div 7 0 = 0);
  match List.rev !failed with
  | [] -> print_endline "linkonly: ok"
  | names ->
      List.iter (Printf.printf "linkonly: FAILED %s\n") names;
      exit 1
