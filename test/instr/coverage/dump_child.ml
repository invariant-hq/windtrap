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
   — which must warn and be ignored, never crash the process.

   Three modes act around the first registration, which is when the
   runtime decides where the dump goes: [moved DIR] registers as [first]
   and then moves to DIR and points WINDTRAP_COVERAGE_FILE there, which
   must change nothing; [fork] registers, visits block 0, and forks a
   child that visits block 1 and leaves through [exit] while the parent
   waits for it and then visits block 2; [cwd-gone] removes its own
   directory, and its copy of itself in it, before it registers. *)

module C = Windtrap_runtime.Coverage

let register () =
  let counts = Array.make 3 0 in
  C.register ~file:"lib/child.ml"
    ~points:
      [|
        { C.start_ofs = 0; end_ofs = 5 };
        { start_ofs = 6; end_ofs = 9 };
        { start_ofs = 10; end_ofs = 20 };
      |]
    ~counts;
  counts

let () =
  match Array.to_list Sys.argv |> List.tl with
  | [ "silent" ] -> ()
  | [ "moved"; dir ] ->
      let counts = register () in
      C.visit counts 0;
      Sys.chdir dir;
      Unix.putenv "WINDTRAP_COVERAGE_FILE"
        (Filename.concat dir "moved.coverage")
  | [ "fork" ] -> (
      let counts = register () in
      C.visit counts 0;
      match Unix.fork () with
      | 0 ->
          C.visit counts 1;
          exit 0
      | pid ->
          ignore (Unix.waitpid [] pid);
          C.visit counts 2)
  | [ "cwd-gone" ] ->
      let here = Sys.getcwd () in
      Sys.remove (Filename.basename Sys.executable_name);
      Sys.rmdir here;
      let counts = register () in
      C.visit counts 0
  | [ mode ] ->
      let counts = register () in
      C.visit counts 0;
      if mode = "second" then begin
        C.visit counts 1;
        C.visit counts 1
      end;
      if mode = "conflict" then begin
        let other = Array.make 1 0 in
        C.register ~file:"lib/child.ml"
          ~points:[| { C.start_ofs = 0; end_ofs = 99 } |]
          ~counts:other;
        (* The dropped module still runs its visits; they must count for
           nothing and harm nothing. *)
        C.visit other 0
      end
  | _ -> failwith "usage: dump_child.exe MODE"
