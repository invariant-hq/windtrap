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

(* The runner's environment is stated, never inherited. The corpus is a
   corrections harness: what it pins is the promotion protocol's exit
   code and the corrections a plain run writes, and a runner that
   inherited a WINDTRAP_* mirror from the invoking shell - the tree-wide
   mutation run sets WINDTRAP_MUTATE=1 for every stanza - would run the
   mutation loop in place of that protocol, and refuse it on a corpus
   that fails on purpose. Nothing survives but what a process needs to
   start. *)
let environment =
  String.concat " "
    ("env" :: "-i"
    :: List.concat_map
         (fun name ->
           match Sys.getenv_opt name with
           | Some value -> [ Filename.quote (name ^ "=" ^ value) ]
           | None -> [])
         [ "PATH"; "HOME"; "TMPDIR"; "LANG"; "LC_ALL" ])

let write_file path contents =
  let oc = open_out_bin path in
  output_string oc contents;
  close_out oc

let () =
  match Array.to_list Sys.argv with
  | _ :: prefix :: runner :: fixtures ->
    let cmd =
      Printf.sprintf "%s %s inline-test-runner conformance > %s 2>&1"
        environment
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
