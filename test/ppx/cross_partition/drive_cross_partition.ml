(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Cross-partition fixture driver: [drive_cross_partition.exe RUNNER]
   spawns RUNNER once per partition, under the harness's scrubbed
   environment, and records each run's transcript, exit code, and what it
   wrote — which is the whole of what dune reads when it decides whether
   a correction is promotable. The one directory with a driver of its
   own: four runs must share one process (see below) and two of them
   stage a project root for the run to rewrite, which no argv can say.

   Four runs, in one process and one rule. Two rules in one dune
   directory share a build directory and may run concurrently, so each
   would see the other's .corrected files and the attribution would be a
   race.

   1. [crash.ml] plain: exit 1, no .corrected. Under dune this exit code
      is a veto over the whole library's corrections, siblings included.
   2. [stale.ml] under WINDTRAP_UPDATE: the correction goes to the source
      tree, not through dune, and the partition exits 0.
   3. [crash.ml] under WINDTRAP_UPDATE: still exit 1, and still nothing
      written anywhere — the proof that the update channel did not make a
      crash promotable.
   4. [stale.ml] plain: exit 0 and stale.ml.corrected, left in place as a
      declared target. Last, so the .corrected that survives is
      unambiguously this run's.

   The update runs get their own project root: a scratch tree outside the
   build directory, carrying a copy of the fixtures at the same
   context-relative path the PPX recorded. Without WINDTRAP_PROJECT_ROOT
   the runner would resolve the real repository root and rewrite the
   committed fixture. The rewritten copies are reported back as targets —
   which files changed, and to what — rather than left in the scratch
   tree, so the goldens can pin them. *)

let masks = [ Drive_harness.Full_log; Drive_harness.Backtrace ]

let environment extra =
  Drive_harness.environment
    (("WINDTRAP_SLOW_THRESHOLD", "0") :: ("WINDTRAP_COVERAGE", "off") :: extra)

(* .corrected files in the rule's directory, which is where the runtime
   writes them: the module-load cwd, next to the copied source. *)
let corrected_files () =
  Sys.readdir "." |> Array.to_list
  |> List.filter (fun name -> Filename.check_suffix name ".ml.corrected")
  |> List.sort compare

let clear_corrected () =
  List.iter
    (fun name -> try Sys.remove name with Sys_error _ -> ())
    (corrected_files ())

let run ~runner ~partition ~name ~extra_env =
  ignore
    (Drive_harness.record ~name ~exe:runner
       ~args:
         [ "inline-test-runner"; "cross_partition"; "-partition"; partition ]
       ~env:(environment extra_env) ~masks ())

(* The path the PPX recorded for the fixtures is the source path as the
   compiler saw it, which under dune is relative to the build context
   root — "test/ppx/cross_partition/stale.ml". The runtime reconstructs
   its source-tree target from exactly that, so a scratch project root
   has to carry the same relative layout under it. Derived from the
   rule's own cwd (<...>/_build/<context>/<this dir>) rather than spelled
   out, so moving this directory moves the fixture with it. *)
let context_relative_dir () =
  let rec drop = function
    | "_build" :: _context :: rest -> rest
    | _ :: rest -> drop rest
    | [] -> []
  in
  match drop (String.split_on_char '/' (Sys.getcwd ())) with
  | [] ->
      prerr_endline
        "drive_cross_partition: cwd is not inside a dune build directory";
      exit 2
  | comps -> String.concat "/" comps

let rec mkdir_p path =
  if path <> "" && path <> "/" && not (Sys.file_exists path) then begin
    mkdir_p (Filename.dirname path);
    try Unix.mkdir path 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

(* One update run: stage the fixtures in a scratch project root, run the
   partition against it, and report which of them the run rewrote. The
   staged copies are byte-copies of the ones the runtime reads, so the
   drift guard passes and the write is the one the guard was written to
   allow. *)
let update_run ~runner ~partition ~name ~fixtures =
  let root = Filename.temp_file "windtrap-cross-partition-" ".dir" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let rel = context_relative_dir () in
  mkdir_p (Filename.concat root rel);
  let staged =
    List.map
      (fun fixture ->
        let path = Filename.concat root (Filename.concat rel fixture) in
        let contents = Drive_harness.read_file fixture in
        Drive_harness.write_file path contents;
        (fixture, path, contents))
      fixtures
  in
  run ~runner ~partition ~name
    ~extra_env:[ ("WINDTRAP_UPDATE", "1"); ("WINDTRAP_PROJECT_ROOT", root) ];
  let rewritten =
    List.filter
      (fun (_, path, contents) ->
        not (String.equal (Drive_harness.read_file path) contents))
      staged
  in
  Drive_harness.write_file (name ^ "-rewritten")
    (String.concat ""
       (List.map (fun (f, _, _) -> rel ^ "/" ^ f ^ "\n") rewritten));
  let contents =
    String.concat ""
      (List.map (fun (_, path, _) -> Drive_harness.read_file path) rewritten)
  in
  Drive_harness.remove_tree root;
  contents

let () =
  match Sys.argv with
  | [| _; runner |] ->
      let fixtures = [ "crash.ml"; "stale.ml" ] in
      (* 1. The crashing partition, plain: the veto, and the empty
         .corrected set that makes it a veto over somebody else's work. *)
      run ~runner ~partition:"crash.ml" ~name:"crash" ~extra_env:[];
      Drive_harness.write_file "crash-corrected"
        (String.concat "" (List.map (fun n -> n ^ "\n") (corrected_files ())));
      clear_corrected ();
      (* 2. The stale partition under WINDTRAP_UPDATE: the source tree
         gets the correction, with no help from dune. *)
      Drive_harness.write_file "update-source"
        (update_run ~runner ~partition:"stale.ml" ~name:"update" ~fixtures);
      clear_corrected ();
      (* 3. The crashing partition under WINDTRAP_UPDATE: nothing. *)
      ignore
        (update_run ~runner ~partition:"crash.ml" ~name:"update-crash"
           ~fixtures);
      clear_corrected ();
      (* 4. The stale partition, plain: exit 0 and the .corrected dune
         would have diffed, left in place as a declared target. *)
      run ~runner ~partition:"stale.ml" ~name:"stale" ~extra_env:[]
  | _ ->
      prerr_endline "usage: drive_cross_partition.exe RUNNER";
      exit 2
