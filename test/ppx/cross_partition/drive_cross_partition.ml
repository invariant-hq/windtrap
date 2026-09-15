(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Cross-partition fixture driver: [drive_cross_partition.exe RUNNER]
   spawns RUNNER once per partition, under the harness's scrubbed
   environment, and records each run's transcript, exit code, and what it
   wrote — which is the whole of what dune reads when it decides whether
   a correction is promotable. The one directory with a driver of its
   own: the two runs must share one process. Two rules in one dune
   directory share a build directory and may run concurrently, so each
   would see the other's .corrected files and the attribution would be a
   race.

   1. [crash.ml]: exit 1, no .corrected. Under dune this exit code is a
      veto over the whole library's corrections, siblings included.
   2. [stale.ml]: exit 0 and stale.ml.corrected, left in place as a
      declared target. Last, so the .corrected that survives is
      unambiguously this run's. *)

let masks = [ Drive_harness.Full_log; Drive_harness.Backtrace ]

let environment extra =
  Drive_harness.environment
    (("WINDTRAP_SLOW_THRESHOLD", "0") :: ("WINDTRAP_COVERAGE", "off") :: extra)

(* .corrected files in the rule's directory, which is where the run
   writes them: beside dune's copy of the source, under the build root
   the runner started in. *)
let corrected_files () =
  Sys.readdir "." |> Array.to_list
  |> List.filter (fun name -> Filename.check_suffix name ".ml.corrected")
  |> List.sort compare

let clear_corrected () =
  List.iter
    (fun name -> try Sys.remove name with Sys_error _ -> ())
    (corrected_files ())

let run ~runner ~partition ~name =
  ignore
    (Drive_harness.record ~name ~exe:runner
       ~args:
         [ "inline-test-runner"; "cross_partition"; "-partition"; partition ]
       ~env:(environment []) ~masks ())

let () =
  match Sys.argv with
  | [| _; runner |] ->
      (* 1. The crashing partition: the veto, and the empty .corrected set
         that makes it a veto over somebody else's work. *)
      run ~runner ~partition:"crash.ml" ~name:"crash";
      Drive_harness.write_file "crash-corrected"
        (String.concat "" (List.map (fun n -> n ^ "\n") (corrected_files ())));
      clear_corrected ();
      (* 2. The stale partition: exit 0 and the .corrected dune would have
         diffed, left in place as a declared target. *)
      run ~runner ~partition:"stale.ml" ~name:"stale"
  | _ ->
      prerr_endline "usage: drive_cross_partition.exe RUNNER";
      exit 2
