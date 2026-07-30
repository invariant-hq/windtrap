(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tail-loc fixture driver: [drive_tail_loc.exe PREFIX RUNNER] spawns
   RUNNER with the inline-test-runner protocol argv under a scrubbed
   environment (no WINDTRAP_* mirror, CI, or color variable can reshape
   the pinned transcript; the slow threshold is pinned to 0 so a loaded
   machine cannot add a slow warning), captures its combined output with
   the wall-clock durations masked, and records its exit code — so the
   runtest rules can diff the full failing transcript byte-for-byte
   against a committed golden. *)

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
    [| "WINDTRAP_SLOW_THRESHOLD=0"; "WINDTRAP_COLOR=never" |]

let () =
  match Sys.argv with
  | [| _; prefix; runner |] ->
      let log = prefix ^ "-log" in
      let fd =
        Unix.openfile log [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644
      in
      let pid =
        Unix.create_process_env runner
          [| runner; "inline-test-runner"; "tail_loc" |]
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
      write_file log (mask_durations (read_file log));
      write_file (prefix ^ "-exit") (string_of_int code ^ "\n")
  | _ ->
      prerr_endline "usage: drive_tail_loc.exe PREFIX RUNNER";
      exit 2
