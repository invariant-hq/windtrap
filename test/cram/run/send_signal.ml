(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [send_signal.exe [--ignore SIGNAL] PLAN EXE ARG...] starts EXE with ARGs,
   its standard output in [out] and its standard error in [err], follows
   PLAN, and prints how EXE ended. PLAN is [-] (send nothing) or a
   comma-separated list of steps [SIGNAL] or [SIGNAL@FILE]: wait until EXE
   creates FILE ([ready] by default) in the working directory, then send it
   SIGNAL (INT, TERM or HUP). After each step but the last, EXE is given
   0.2 s, and the program prints whether it is still running. [--ignore]
   starts EXE with SIGNAL ignored.

   A shell cannot do this job: a command it starts in the background
   ignores SIGINT, and the runner keeps a signal its process was started
   ignoring ignored. This program starts EXE with the dispositions it has
   itself and holds EXE's pid, so the signal reaches the process it names.
   EXE's environment is this program's. *)

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

let step text =
  match String.index_opt text '@' with
  | None -> (signal_of_name text, "ready")
  | Some i ->
      ( signal_of_name (String.sub text 0 i),
        String.sub text (i + 1) (String.length text - i - 1) )

let plan = function
  | "-" -> []
  | text -> List.map step (String.split_on_char ',' text)

let file name =
  Unix.openfile name
    [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_CLOEXEC ]
    0o644

let ended = function
  | Unix.WSIGNALED s -> "killed by " ^ name_of_signal s
  | Unix.WEXITED code -> "exited " ^ string_of_int code
  | Unix.WSTOPPED s -> "stopped by " ^ name_of_signal s

(* A gate, not a clock: the child is signalled the moment it says it is
   ready, however long its first launch takes. [Some status] is a child
   that ended before it got there. *)
let rec await pid name =
  match Unix.waitpid [ Unix.WNOHANG ] pid with
  | 0, _ ->
      if Sys.file_exists name then None
      else begin
        Unix.sleepf 0.01;
        await pid name
      end
  | _, status -> Some status

let rec follow pid = function
  | [] -> snd (Unix.waitpid [] pid)
  | (signal, name) :: rest -> (
      match await pid name with
      | Some status ->
          Printf.printf "ended before %s\n" name;
          status
      | None -> (
          Unix.kill pid signal;
          match rest with
          | [] -> snd (Unix.waitpid [] pid)
          | _ :: _ -> (
              Unix.sleepf 0.2;
              match Unix.waitpid [ Unix.WNOHANG ] pid with
              | 0, _ ->
                  Printf.printf "running after %s\n" (name_of_signal signal);
                  follow pid rest
              | _, status -> status)))

let start ~ignored steps exe args =
  List.iter (fun s -> Sys.set_signal s Sys.Signal_ignore) ignored;
  List.iter
    (fun (_, name) -> if Sys.file_exists name then Sys.remove name)
    steps;
  let out = file "out" and err = file "err" in
  let pid =
    Unix.create_process exe (Array.of_list (exe :: args)) Unix.stdin out err
  in
  Unix.close out;
  Unix.close err;
  print_endline (ended (follow pid steps))

let () =
  match List.tl (Array.to_list Sys.argv) with
  | "--ignore" :: signal :: steps :: exe :: args ->
      start ~ignored:[ signal_of_name signal ] (plan steps) exe args
  | steps :: exe :: args -> start ~ignored:[] (plan steps) exe args
  | [] | [ _ ] ->
      prerr_endline "usage: send_signal.exe [--ignore SIGNAL] PLAN EXE [ARG]...";
      exit 2
