(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Conformance-corpus corrections driver:
   [drive.exe PREFIX RUNNER FIXTURE...] spawns RUNNER with the
   inline-test-runner protocol argv, captures its combined output to
   [PREFIX-log], writes its exit code to [PREFIX-exit], and materializes
   a placeholder [<FIXTURE>.corrected] for every fixture the runner
   produced no correction for — so the per-fixture diff rules always
   have a file to compare and a divergence reads as a diff, never a
   missing-target build error. *)

let placeholder = "=== no correction produced ===\n"

let write_file path contents =
  let oc = open_out_bin path in
  output_string oc contents;
  close_out oc

let () =
  match Array.to_list Sys.argv with
  | _ :: prefix :: runner :: fixtures ->
    let cmd =
      Printf.sprintf "%s inline-test-runner conformance > %s 2>&1"
        (Filename.quote runner)
        (Filename.quote (prefix ^ "-log"))
    in
    let code = Sys.command cmd in
    write_file (prefix ^ "-exit") (string_of_int code ^ "\n");
    List.iter
      (fun f ->
        let corrected = f ^ ".corrected" in
        if not (Sys.file_exists corrected) then
          write_file corrected placeholder)
      fixtures
  | _ ->
    prerr_endline "usage: drive.exe PREFIX RUNNER FIXTURE.ml...";
    exit 2
