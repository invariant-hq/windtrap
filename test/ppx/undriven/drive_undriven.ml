(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Undriven-registration fixture driver: [drive_undriven.exe PREFIX
   UNDRIVEN RUNNER] spawns four processes under a scrubbed environment
   (no WINDTRAP_* mirror, CI, or color variable can reshape the pinned
   transcripts; the slow threshold is pinned to 0 so a loaded machine
   cannot add a slow warning), captures each one's combined output with
   the wall-clock durations masked, and records each exit code — so the
   runtest rules can diff every transcript byte-for-byte against a
   committed golden.

   The four runs, and what each pins:

   - [undriven]: UNDRIVEN (the plain executable that links the
     preprocessed test module and drives nothing) with no arguments.
     The defect run: the guard's diagnostic and exit 2. Before the
     guard, this process exited 0 in silence.
   - [driven]: RUNNER with the inline-test-runner protocol argv. The
     ordinary partition run: normal transcript, exit 0, and no guard
     diagnostic — [init] claimed the registry.
   - [list]: RUNNER with [-list-partitions]. The enumeration pass dune
     makes before any partition runs: the partition list, exit 0, no
     diagnostic — [init] claims in that mode too.
   - [byhand]: RUNNER with no protocol arguments. The generated runner
     invoked by hand does nothing, by documented contract — a deliberate
     invocation is not a silent one, so the guard stays quiet. *)

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
   ["1 passed in 0.0021s."] becomes ["1 passed in <duration>s."]. *)
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

(* The mutation discovery line, dropped: lib/ carries an
   (instrumentation (backend ppx_windtrap.mutate)) stanza, so under
   --instrument-with every suite-running process this driver spawns ends
   with "mutants: N in M files ...". True, and not what these goldens
   are about. There is no environment knob for it on purpose
   (WINDTRAP_MUTATE=off still announces; test/mutate_loop pins that), so
   the driver drops it here rather than the run suppressing it. *)
let drop_discovery s =
  String.split_on_char '\n' s
  |> List.filter (fun line -> not (String.starts_with ~prefix:"mutants: " line))
  |> String.concat "\n"

let scrubbed_environment () =
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
    [|
      "WINDTRAP_SLOW_THRESHOLD=0";
      "WINDTRAP_COLOR=never";
      "WINDTRAP_COVERAGE=off";
    |]

let run_once ~exe ~args ~log ~exit_file =
  let fd =
    Unix.openfile log [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644
  in
  let pid =
    Unix.create_process_env exe
      (Array.append [| exe |] args)
      (scrubbed_environment ()) Unix.stdin fd fd
  in
  Unix.close fd;
  let _, status = Unix.waitpid [] pid in
  let code =
    match status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED signal -> 128 + signal
    | Unix.WSTOPPED _ -> 255
  in
  write_file log (drop_discovery (mask_durations (read_file log)));
  write_file exit_file (string_of_int code ^ "\n")

let () =
  match Sys.argv with
  | [| _; prefix; undriven; runner |] ->
      let run tag ~exe ~args =
        run_once ~exe ~args
          ~log:(prefix ^ "-" ^ tag ^ "-log")
          ~exit_file:(prefix ^ "-" ^ tag ^ "-exit")
      in
      run "undriven" ~exe:undriven ~args:[||];
      run "driven" ~exe:runner ~args:[| "inline-test-runner"; "undriven" |];
      run "list" ~exe:runner
        ~args:[| "inline-test-runner"; "undriven"; "-list-partitions" |];
      run "byhand" ~exe:runner ~args:[||]
  | _ ->
      prerr_endline "usage: drive_undriven.exe PREFIX UNDRIVEN RUNNER";
      exit 2
