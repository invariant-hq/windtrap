(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The transcript driver every test/ppx fixture directory runs.

   Usage:

     drive.exe [GLOBAL]... --run NAME EXE [RUN]... [ARG]... [--run ...]

   Each [--run NAME EXE ARG...] spawns EXE with ARGs under the scrubbed
   environment and records [NAME-log] (its masked combined output) and
   [NAME-exit] (its exit code), which the directory's runtest rules diff
   against committed goldens.

     --env K=V       a binding for the child; before the first --run it
                     applies to every run, after one only to that run
     --mask M        full-log | slow | verbose | backtrace: what varies
                     between machines and runs in this directory's output
     --scratch-cwd   run from a fresh empty directory, removed afterwards
                     and masked as <scratch> — the cwd a runner cannot
                     resolve its sources from
     --probe FILE    append whether FILE exists in the child's cwd, the
                     one thing a transcript cannot say
     --mkdir DIR     create DIR before the runs — a per-run scratch a
                     child needs somewhere to write into

   Everything a directory pins is therefore in its dune file, not in a
   program of its own. *)

type run = {
  name : string;
  exe : string;
  args : string list;  (** reversed while parsing *)
  env : (string * string) list;  (** reversed while parsing *)
}

let usage () =
  prerr_endline
    "usage: drive.exe [--env K=V] [--mask M] [--mkdir DIR] [--scratch-cwd] \
     [--probe FILE] --run NAME EXE [ARG]...";
  exit 2

let binding s =
  match String.index_opt s '=' with
  | Some i ->
      (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
  | None -> usage ()

let mask_of_string = function
  | "full-log" -> Drive_harness.Full_log
  | "slow" -> Drive_harness.Slow_column
  | "verbose" -> Drive_harness.Verbose_timing
  | "backtrace" -> Drive_harness.Backtrace
  | _ -> usage ()

let parse argv =
  let masks = ref [] and shared = ref [] and scratch = ref false in
  let probe = ref None and dirs = ref [] and runs = ref [] in
  let rec go = function
    | [] -> ()
    | "--run" :: name :: exe :: rest ->
        runs := { name; exe; args = []; env = [] } :: !runs;
        go rest
    | "--mask" :: m :: rest ->
        masks := mask_of_string m :: !masks;
        go rest
    | "--scratch-cwd" :: rest ->
        scratch := true;
        go rest
    | "--probe" :: file :: rest ->
        probe := Some file;
        go rest
    | "--mkdir" :: dir :: rest ->
        dirs := dir :: !dirs;
        go rest
    | "--env" :: b :: rest ->
        (match !runs with
        | [] -> shared := binding b :: !shared
        | run :: others ->
            runs := { run with env = binding b :: run.env } :: others);
        go rest
    | arg :: rest ->
        (match !runs with
        | [] -> usage ()
        | run :: others ->
            runs := { run with args = arg :: run.args } :: others);
        go rest
  in
  go (List.tl (Array.to_list argv));
  if !runs = [] then usage ();
  let runs =
    List.rev_map
      (fun run ->
        {
          run with
          args = List.rev run.args;
          env = List.rev_append run.env (List.rev !shared);
        })
      !runs
  in
  (List.rev !masks, !scratch, !probe, List.rev !dirs, runs)

let () =
  let masks, scratch, probe, dirs, runs = parse Sys.argv in
  List.iter
    (fun dir ->
      try Unix.mkdir dir 0o755
      with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
    dirs;
  let start_dir = Sys.getcwd () in
  let absolute p =
    if Filename.is_relative p then Filename.concat start_dir p else p
  in
  let dir, decorate =
    if not scratch then (None, Fun.id)
    else begin
      let dir =
        Filename.concat
          (Filename.get_temp_dir_name ())
          (Printf.sprintf "windtrap-drive-%d" (Unix.getpid ()))
      in
      Unix.mkdir dir 0o755;
      Sys.chdir dir;
      (* The OS may report a resolved spelling of the scratch path (macOS
         resolves /var to /private/var): mask both. *)
      let resolved = Sys.getcwd () in
      ( Some dir,
        fun s ->
          Drive_harness.replace ~pattern:dir ~by:"<scratch>"
            (Drive_harness.replace ~pattern:resolved ~by:"<scratch>" s) )
    end
  in
  let probe () =
    match probe with
    | None -> ""
    | Some file ->
        Printf.sprintf ".corrected in scratch cwd: %s\n"
          (if Sys.file_exists file then "yes" else "no")
  in
  List.iter
    (fun { name; exe; args; env } ->
      let env = Drive_harness.environment env in
      ignore
        (Drive_harness.record ~probe ~decorate ~name:(absolute name)
           ~exe:(absolute exe) ~args ~env ~masks ()))
    runs;
  match dir with
  | None -> ()
  | Some dir ->
      Sys.chdir start_dir;
      Drive_harness.remove_tree dir
