(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Cross-partition fixture driver: [drive_cross_partition.exe RUNNER]
   spawns RUNNER once per partition, under a scrubbed environment, and
   records each run's transcript, exit code, and what it wrote — which is
   the whole of what dune reads when it decides whether a correction is
   promotable.

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

let write_file path contents =
  let oc = open_out_bin path in
  output_string oc contents;
  close_out oc

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

(* Masks the digits of every [in <seconds>s] duration token:
   ["1 failed in 0.0021s."] becomes ["1 failed in <duration>s."]. *)
let mask_durations s =
  let n = String.length s in
  let b = Buffer.create n in
  let is_num = function '0' .. '9' | '.' -> true | _ -> false in
  let i = ref 0 in
  while !i < n do
    if !i + 4 <= n && String.equal (String.sub s !i 4) " in " then begin
      Buffer.add_string b " in ";
      let j = !i + 4 in
      let k = ref j in
      while !k < n && is_num s.[!k] do
        incr k
      done;
      if !k > j && !k < n && s.[!k] = 's' then begin
        Buffer.add_string b "<duration>s";
        i := !k + 1
      end
      else i := j
    end
    else begin
      Buffer.add_char b s.[!i];
      incr i
    end
  done;
  Buffer.contents b

(* Masks the tail of every [full log: <path>] line: the path names a
   random per-run directory. *)
let mask_full_log s =
  let lines = String.split_on_char '\n' s in
  let mask line =
    let marker = "full log: " in
    let mlen = String.length marker in
    let rec find i =
      if i + mlen > String.length line then None
      else if String.equal (String.sub line i mlen) marker then Some i
      else find (i + 1)
    in
    match find 0 with
    | None -> line
    | Some i -> String.sub line 0 (i + mlen) ^ "<log>"
  in
  String.concat "\n" (List.map mask lines)

(* Backtrace frames, collapsed to one marker

   The crashing partition's report carries a real backtrace, and its
   frames name file:line inside ppx/runtime/ppx_runtime.ml — so an
   unmasked golden would break on every future edit to the runtime, over
   a line number that is not what this directory pins. No other golden in
   the tree carries a backtrace. The marker keeps the fact that one was
   printed, which is the part that matters here: a crash is reported as a
   crash, not as a correction. *)
let mask_backtrace s =
  let leading line =
    let n = String.length line in
    let rec go i = if i < n && line.[i] = ' ' then go (i + 1) else i in
    go 0
  in
  let is_frame line =
    let trimmed = String.trim line in
    List.exists
      (fun prefix -> String.starts_with ~prefix trimmed)
      [ "Raised at "; "Re-raised at "; "Called from "; "Raised by primitive " ]
  in
  let rec go acc = function
    | [] -> List.rev acc
    | line :: rest when is_frame line ->
        let rec skip = function l :: r when is_frame l -> skip r | r -> r in
        go ((String.make (leading line) ' ' ^ "<backtrace>") :: acc) (skip rest)
    | line :: rest -> go (line :: acc) rest
  in
  String.concat "\n" (go [] (String.split_on_char '\n' s))

(* The mutation discovery line, dropped

   lib/ carries an (instrumentation (backend ppx_windtrap.mutate)) stanza,
   so under --instrument-with every run this driver spawns ends with
   "mutants: N in M files ...". That line is a true statement about the
   build and it is not what this golden is about — the golden pins the
   RUNNER's transcript. There is no environment knob for it on purpose
   (WINDTRAP_MUTATE=off still announces; test/mutate_loop pins that), so
   the driver drops it here rather than the run suppressing it. *)
let drop_discovery s =
  String.split_on_char '\n' s
  |> List.filter (fun line -> not (String.starts_with ~prefix:"mutants: " line))
  |> String.concat "\n"

let scrubbed_environment extra =
  let dropped name =
    String.starts_with ~prefix:"WINDTRAP_" name
    || List.mem name
         [ "CI"; "GITHUB_ACTIONS"; "NO_COLOR"; "CLICOLOR"; "CLICOLOR_FORCE" ]
  in
  let keep binding =
    match String.index_opt binding '=' with
    | Some eq -> not (dropped (String.sub binding 0 eq))
    | None -> true
  in
  Array.append
    (Array.of_list (List.filter keep (Array.to_list (Unix.environment ()))))
    (Array.append
       [|
         "WINDTRAP_SLOW_THRESHOLD=0"; "WINDTRAP_COLOR=never";
         "WINDTRAP_COVERAGE=off";
       |]
       extra)

(* .corrected files in the rule's directory, which is where the runtime
   writes them: the module-load cwd, next to the copied source. *)
let corrected_files () =
  Sys.readdir "." |> Array.to_list
  |> List.filter (fun name -> Filename.check_suffix name ".ml.corrected")
  |> List.sort compare

let clear_corrected () =
  List.iter (fun name -> try Sys.remove name with Sys_error _ -> ())
    (corrected_files ())

let run ~runner ~partition ~prefix ~extra_env =
  let log = prefix ^ "-log" in
  let fd =
    Unix.openfile log [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644
  in
  let pid =
    Unix.create_process_env runner
      [|
        runner;
        "inline-test-runner";
        "cross_partition";
        "-partition";
        partition;
      |]
      (scrubbed_environment extra_env)
      Unix.stdin fd fd
  in
  Unix.close fd;
  let _, status = Unix.waitpid [] pid in
  let code =
    match status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED signal -> 128 + signal
    | Unix.WSTOPPED _ -> 255
  in
  write_file log
    (drop_discovery
       (mask_backtrace (mask_full_log (mask_durations (read_file log)))));
  write_file (prefix ^ "-exit") (string_of_int code ^ "\n")

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

let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Array.iter
        (fun name -> remove_tree (Filename.concat path name))
        (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

(* One update run: stage the fixtures in a scratch project root, run the
   partition against it, and report which of them the run rewrote. The
   staged copies are byte-copies of the ones the runtime reads, so the
   drift guard passes and the write is the one the guard was written to
   allow. *)
let update_run ~runner ~partition ~prefix ~fixtures =
  let root = Filename.temp_file "windtrap-cross-partition-" ".dir" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let rel = context_relative_dir () in
  mkdir_p (Filename.concat root rel);
  let staged =
    List.map
      (fun name ->
        let path = Filename.concat root (Filename.concat rel name) in
        let contents = read_file name in
        write_file path contents;
        (name, path, contents))
      fixtures
  in
  run ~runner ~partition ~prefix
    ~extra_env:[| "WINDTRAP_UPDATE=1"; "WINDTRAP_PROJECT_ROOT=" ^ root |];
  let rewritten =
    List.filter
      (fun (_, path, contents) -> not (String.equal (read_file path) contents))
      staged
  in
  write_file (prefix ^ "-rewritten")
    (String.concat ""
       (List.map (fun (name, _, _) -> rel ^ "/" ^ name ^ "\n") rewritten));
  let contents =
    String.concat "" (List.map (fun (_, path, _) -> read_file path) rewritten)
  in
  remove_tree root;
  contents

let () =
  match Sys.argv with
  | [| _; runner |] ->
      let fixtures = [ "crash.ml"; "stale.ml" ] in
      (* 1. The crashing partition, plain: the veto, and the empty
         .corrected set that makes it a veto over somebody else's work. *)
      run ~runner ~partition:"crash.ml" ~prefix:"crash" ~extra_env:[||];
      write_file "crash-corrected"
        (String.concat "" (List.map (fun n -> n ^ "\n") (corrected_files ())));
      clear_corrected ();
      (* 2. The stale partition under WINDTRAP_UPDATE: the source tree
         gets the correction, with no help from dune. *)
      write_file "update-source"
        (update_run ~runner ~partition:"stale.ml" ~prefix:"update" ~fixtures);
      clear_corrected ();
      (* 3. The crashing partition under WINDTRAP_UPDATE: nothing. *)
      ignore
        (update_run ~runner ~partition:"crash.ml" ~prefix:"update-crash"
           ~fixtures);
      clear_corrected ();
      (* 4. The stale partition, plain: exit 0 and the .corrected dune
         would have diffed, left in place as a declared target. *)
      run ~runner ~partition:"stale.ml" ~prefix:"stale" ~extra_env:[||]
  | _ ->
      prerr_endline "usage: drive_cross_partition.exe RUNNER";
      exit 2
