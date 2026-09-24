(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [send_signal.exe SIGNAL EXE ARG...] starts EXE with ARGs, its standard
   output in [out] and its standard error in [err], waits until EXE
   creates [ready] in the working directory, sends it SIGNAL (INT, TERM or
   HUP), and prints how it ended.

   A shell cannot do this job: a command it starts in the background
   ignores SIGINT, and the runner keeps a signal its process was started
   ignoring ignored. This program starts EXE with the dispositions it has
   itself and holds EXE's pid, so the signal reaches the process it
   names. EXE's environment is this program's. *)

let signal_of_name = function
  | "INT" -> Sys.sigint
  | "TERM" -> Sys.sigterm
  | "HUP" -> Sys.sighup
  | name -> invalid_arg ("send_signal: unknown signal " ^ name)

let name_of_signal signal =
  if signal = Sys.sigint then "SIGINT"
  else if signal = Sys.sigterm then "SIGTERM"
  else if signal = Sys.sighup then "SIGHUP"
  else if signal = Sys.sigkill then "SIGKILL"
  else string_of_int signal

let file name =
  Unix.openfile name
    [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_CLOEXEC ]
    0o644

(* A gate, not a clock: the child is signalled the moment it says it is
   ready. The bound only turns a child that never gets there into a
   visible verdict instead of a session that hangs. *)
let rec await_ready tries =
  if Sys.file_exists "ready" then true
  else if tries = 0 then false
  else begin
    Unix.sleepf 0.01;
    await_ready (tries - 1)
  end

let () =
  match Array.to_list Sys.argv with
  | _ :: signal :: exe :: args ->
      let signal = signal_of_name signal in
      if Sys.file_exists "ready" then Sys.remove "ready";
      let out = file "out" and err = file "err" in
      let pid =
        Unix.create_process exe (Array.of_list (exe :: args)) Unix.stdin out err
      in
      Unix.close out;
      Unix.close err;
      if await_ready 6000 then Unix.kill pid signal
      else begin
        print_endline "never ready";
        Unix.kill pid Sys.sigkill
      end;
      (match snd (Unix.waitpid [] pid) with
      | Unix.WSIGNALED s -> Printf.printf "killed by %s\n" (name_of_signal s)
      | Unix.WEXITED code -> Printf.printf "exited %d\n" code
      | Unix.WSTOPPED s -> Printf.printf "stopped by %s\n" (name_of_signal s));
      exit 0
  | _ ->
      prerr_endline "usage: send_signal.exe SIGNAL EXE [ARG]...";
      exit 2
