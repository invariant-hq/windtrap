(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A real windtrap run over the genuinely instrumented Covcli_fixture:
   the test exercises every path of the fixture, yet its raising call's
   out-edge can never fire - the at_exit dump must report the file at
   2/3. Callers must set WINDTRAP_COVERAGE_FILE so the dump lands in
   scratch, never in the real build directory. *)

let () =
  exit
  @@ Windtrap.run "raise-child"
       [
         Windtrap.test "trip raises" (fun () ->
             Windtrap.raises Exit (fun () -> Covcli_fixture.trip ()));
       ]
