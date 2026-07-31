(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Bad-cwd fixture driver: [drive_bad_cwd.exe PREFIX RUNNER] spawns
   RUNNER with the inline-test-runner protocol argv from a fresh scratch
   directory that does not contain the copied source — the failure mode
   of a runner whose cwd cannot resolve the partition's sources. The
   fixture's expect test fails, so a correction is recorded, but it can
   never be written there. The pinned transcript and exit code prove the
   run stays loud: a stderr line names the unwritable source and its
   reason, no .corrected materializes (the trailing probe line), and the
   exit is nonzero — never a silent pass that dune would record as
   PASSED. The environment is scrubbed as in ../tail_loc; wall-clock
   durations, the machine-specific scratch path (rewritten to
   <scratch>), and the per-run "full log:" path (random run directory)
   are masked. The runner roots its _build/_tests log tree at the
   scratch cwd, so the scratch is removed recursively. *)

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
   random per-run directory under the scratch cwd. *)
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

(* Replaces every occurrence of [pattern] with [by]. *)
let replace ~pattern ~by s =
  let plen = String.length pattern and n = String.length s in
  let b = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    if !i + plen <= n && String.equal (String.sub s !i plen) pattern then begin
      Buffer.add_string b by;
      i := !i + plen
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
      let start_dir = Sys.getcwd () in
      let absolute p =
        if Filename.is_relative p then Filename.concat start_dir p else p
      in
      let log = absolute (prefix ^ "-log") in
      let runner = absolute runner in
      let scratch =
        Filename.concat
          (Filename.get_temp_dir_name ())
          (Printf.sprintf "windtrap-bad-cwd-%d" (Unix.getpid ()))
      in
      Unix.mkdir scratch 0o755;
      Sys.chdir scratch;
      (* The OS may report a resolved spelling of the scratch path (macOS
         resolves /var to /private/var): mask both. *)
      let scratch_resolved = Sys.getcwd () in
      let fd =
        Unix.openfile log [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644
      in
      let pid =
        Unix.create_process_env runner
          [| runner; "inline-test-runner"; "bad_cwd" |]
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
      let corrected = Sys.file_exists "inline_bad_cwd.ml.corrected" in
      Sys.chdir start_dir;
      let rec remove_tree path =
        match (Unix.lstat path).Unix.st_kind with
        | Unix.S_DIR ->
            Array.iter
              (fun entry -> remove_tree (Filename.concat path entry))
              (Sys.readdir path);
            Unix.rmdir path
        | _ -> Sys.remove path
      in
      remove_tree scratch;
      let transcript =
        replace ~pattern:scratch ~by:"<scratch>"
          (replace ~pattern:scratch_resolved ~by:"<scratch>"
             (mask_full_log (mask_durations (read_file log))))
      in
      write_file log
        (transcript
        ^ Printf.sprintf ".corrected in scratch cwd: %s\n"
            (if corrected then "yes" else "no"));
      write_file (absolute (prefix ^ "-exit")) (string_of_int code ^ "\n")
  | _ ->
      prerr_endline "usage: drive_bad_cwd.exe PREFIX RUNNER";
      exit 2
