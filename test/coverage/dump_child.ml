(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Stands in for an instrumented executable: registers one file, visits
   some blocks, and exits, letting the at_exit handler dump. The parent
   test points WINDTRAP_COVERAGE_FILE at a scratch path and inspects it.
   Modes: [silent] registers nothing (no file must be written); [first]
   visits block 0 once; [second] additionally visits block 1 twice;
   [conflict] additionally registers the same file with a differing table
   — which must warn and be ignored, never crash the process. *)

let () =
  match Sys.argv.(1) with
  | "silent" -> ()
  | mode ->
      let counts = Array.make 3 0 in
      Windtrap_coverage.register ~file:"lib/child.ml"
        ~points:
          [|
            { Windtrap_coverage.start_ofs = 0; end_ofs = 5 };
            { start_ofs = 6; end_ofs = 9 };
            { start_ofs = 10; end_ofs = 20 };
          |]
        ~counts;
      Windtrap_coverage.visit counts 0;
      if mode = "second" then begin
        Windtrap_coverage.visit counts 1;
        Windtrap_coverage.visit counts 1
      end;
      if mode = "conflict" then begin
        let other = Array.make 1 0 in
        Windtrap_coverage.register ~file:"lib/child.ml"
          ~points:[| { Windtrap_coverage.start_ofs = 0; end_ofs = 99 } |]
          ~counts:other;
        (* The dropped module still runs its visits; they must count for
           nothing and harm nothing. *)
        Windtrap_coverage.visit other 0
      end
