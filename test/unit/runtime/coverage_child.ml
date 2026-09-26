(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* An instrumented executable in miniature: it registers [lib/child.ml], a
   table of three points, visits some of them and exits. The dump is written by
   an at_exit function and its destination fixed at the first registration, so
   only a process of its own can show them. Each mode is one scenario of
   test_coverage.ml. *)

module Coverage = Windtrap_runtime.Coverage

let register () =
  let counts = Array.make 3 0 in
  Coverage.register ~file:"lib/child.ml"
    ~points:
      [|
        { Coverage.start_ofs = 0; end_ofs = 5 };
        { start_ofs = 6; end_ofs = 11 };
        { start_ofs = 12; end_ofs = 17 };
      |]
    ~counts;
  counts

let () =
  match List.tl (Array.to_list Sys.argv) with
  | [ "silent" ] -> ()
  | [ "first" ] -> Coverage.visit (register ()) 0
  | [ "second" ] ->
      let counts = register () in
      Coverage.visit counts 0;
      Coverage.visit counts 1;
      Coverage.visit counts 1
  | [ "conflict" ] ->
      let counts = register () in
      Coverage.visit counts 0;
      let other = Array.make 1 0 in
      Coverage.register ~file:"lib/child.ml"
        ~points:[| { Coverage.start_ofs = 0; end_ofs = 99 } |]
        ~counts:other;
      (* The dropped table's module still runs its visits. *)
      Coverage.visit other 0
  | [ "duplicate" ] ->
      let first = register () in
      let second = register () in
      Coverage.visit first 0;
      Coverage.visit second 0
  | [ "files" ] ->
      Coverage.register ~file:"lib/zero.ml" ~points:[||] ~counts:[||];
      Coverage.visit (register ()) 0
  | [ "moved"; dir ] ->
      Coverage.visit (register ()) 0;
      Sys.chdir dir;
      Unix.putenv "WINDTRAP_COVERAGE_FILE"
        (Filename.concat dir "moved.coverage")
  | [ "fork" ] -> (
      let counts = register () in
      Coverage.visit counts 0;
      match Unix.fork () with
      | 0 ->
          Coverage.visit counts 1;
          exit 0
      | pid ->
          ignore (Unix.waitpid [] pid);
          Coverage.visit counts 2)
  | [ "cwd-gone" ] ->
      let here = Sys.getcwd () in
      Sys.remove (Filename.basename Sys.executable_name);
      Sys.rmdir here;
      Coverage.visit (register ()) 0
  | _ -> failwith "usage: coverage_child.exe MODE"
