(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The transcript driver of every fixture directory under
   test/cli/inline_runner and of the conformance corpus's runners.

   Usage:

     drive.exe [GLOBAL]... --run NAME EXE [RUN]... [ARG]... [--run ...]

   Each [--run NAME EXE ARG...] spawns EXE with ARGs in a stated
   environment and records [NAME-log] (its masked standard output, then
   its masked standard error under a [--- stderr ---] line) and
   [NAME-exit] (its exit code), which the directory's runtest rules diff
   against committed goldens.

     --env K=V       a binding for the child; before the first --run it
                     applies to every run, after one only to that run
     --mask M        full-log | slow | verbose | backtrace: what varies
                     between machines and runs in this directory's output
     --scratch-cwd   run from a fresh empty directory, removed afterwards
                     and masked as <scratch>
     --scratch-exe   with --scratch-cwd, run a copy of each EXE placed in
                     that directory, with INSIDE_DUNE bound empty: a binary
                     outside any build directory, run from outside any
                     project and not by dune, is the one that cannot
                     resolve its sources
     --probe FILE    append whether FILE exists in the child's cwd, the
                     one thing a transcript cannot say
     --placeholder F after the runs, write F.corrected as the line
                     [=== no correction produced ===] unless a run wrote
                     it, so that a diff rule always has a file to compare
                     and a run that corrects nothing reads as that line

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
    "usage: drive.exe [--env K=V] [--mask M] [--scratch-cwd] [--scratch-exe] \
     [--probe FILE] [--placeholder FILE] --run NAME EXE [ARG]...";
  exit 2

let binding s =
  match String.index_opt s '=' with
  | Some i -> (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
  | None -> usage ()

let mask_of_string = function
  | "full-log" -> Drive_harness.Full_log
  | "slow" -> Drive_harness.Slow_column
  | "verbose" -> Drive_harness.Verbose_timing
  | "backtrace" -> Drive_harness.Backtrace
  | _ -> usage ()

let parse argv =
  let masks = ref [] and shared = ref [] and scratch = ref false in
  let scratch_exe = ref false in
  let probe = ref None and placeholders = ref [] and runs = ref [] in
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
    | "--scratch-exe" :: rest ->
        scratch_exe := true;
        go rest
    | "--probe" :: file :: rest ->
        probe := Some file;
        go rest
    | "--placeholder" :: file :: rest ->
        placeholders := file :: !placeholders;
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
        | run :: others -> runs := { run with args = arg :: run.args } :: others);
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
  if !scratch_exe && not !scratch then usage ();
  (List.rev !masks, !scratch, !scratch_exe, !probe, List.rev !placeholders, runs)

(* A byte copy with the executable bit: the copy's own path carries no
   build directory, so with INSIDE_DUNE unset the runner's root rule
   falls through to its cwd. *)
let copy_executable ~src ~dst =
  let contents = In_channel.with_open_bin src In_channel.input_all in
  Out_channel.with_open_gen [ Open_wronly; Open_creat; Open_trunc; Open_binary ]
    0o755 dst (fun oc -> Out_channel.output_string oc contents)

let placeholder = "=== no correction produced ===\n"

let () =
  let masks, scratch, scratch_exe, probe, placeholders, runs = parse Sys.argv in
  let start_dir = Sys.getcwd () in
  let absolute p =
    if Filename.is_relative p then Filename.concat start_dir p else p
  in
  let decorate =
    if not scratch then Fun.id
    else begin
      let dir = Windtrap_test_support.Scratch.dir "windtrap-drive-" in
      Sys.chdir dir;
      (* The OS may report a resolved spelling of the scratch path (macOS
         resolves /var to /private/var): mask both. *)
      let resolved = Sys.getcwd () in
      fun s ->
        Drive_harness.replace ~pattern:dir ~by:"<scratch>"
          (Drive_harness.replace ~pattern:resolved ~by:"<scratch>" s)
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
      (* Empty reads as unset for every windtrap variable: the copy must
         not inherit the build context this driver itself runs in. *)
      let env = if scratch_exe then ("INSIDE_DUNE", "") :: env else env in
      let exe =
        if not scratch_exe then absolute exe
        else begin
          let copy = Filename.concat (Sys.getcwd ()) (Filename.basename exe) in
          copy_executable ~src:(absolute exe) ~dst:copy;
          copy
        end
      in
      Drive_harness.record ~probe ~decorate ~name:(absolute name) ~exe ~args
        ~env ~masks ())
    runs;
  Sys.chdir start_dir;
  List.iter
    (fun file ->
      let corrected = file ^ ".corrected" in
      if not (Sys.file_exists corrected) then
        Drive_harness.write_file corrected placeholder)
    placeholders
