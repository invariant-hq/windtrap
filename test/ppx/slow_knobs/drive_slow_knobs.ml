(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Slow-knobs fixture driver (ppx/F-4): [drive_slow_knobs.exe PREFIX
   RUNNER] spawns RUNNER twice with the inline-test-runner protocol argv
   under a scrubbed environment — once compact, once with
   [WINDTRAP_VERBOSE=1] — with [WINDTRAP_SLOW_THRESHOLD] pinned to 1ns so
   the untagged test always warns and the tagged test's missing warning
   can only be the ["slow"] exemption. Durations (the [in <n>s.] summary,
   the [took <n>] slow warnings, and the verbose per-test timing column,
   padding included) are masked, so the runtest rules can diff both full
   transcripts byte-for-byte against committed goldens. A dead mirror
   reshapes the transcript: no [WINDTRAP_SLOW_THRESHOLD] → no slow line
   and a non-noteworthy one-line run; no [WINDTRAP_VERBOSE] → a glyph row
   instead of per-test lines; no tag exemption → a second slow line. *)

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
   ["2 passed in 0.0021s."] becomes ["2 passed in <duration>s."]. *)
let mask_summary_durations s =
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

let find_sub line sub =
  let n = String.length line and m = String.length sub in
  let rec go i =
    if i + m > n then None
    else if String.equal (String.sub line i m) sub then Some i
    else go (i + 1)
  in
  go 0

(* The slow block's entries lead with a right-aligned duration column
   (["  1.3ms  <path>"]); the value and the alignment padding both vary with
   the measurement, so the column is masked whole. An entry is an indented
   line whose first non-blank character is a digit — the verbose per-test
   lines lead with their status tag instead, and the heading with a letter. *)
let mask_slow_entry line =
  let n = String.length line in
  let rec skip_spaces i =
    if i < n && line.[i] = ' ' then skip_spaces (i + 1) else i
  in
  let start = skip_spaces 0 in
  if start < 2 || start >= n || line.[start] < '0' || line.[start] > '9' then
    line
  else
    let rec gap i =
      if i + 1 >= n then None
      else if line.[i] = ' ' && line.[i + 1] = ' ' then Some i
      else gap (i + 1)
    in
    match gap start with
    | Some i -> "  <duration>  " ^ String.sub line (i + 2) (n - i - 2)
    | None -> line

(* Verbose per-test lines right-pad the name and end with the timing; both
   widths vary with the measured duration, so the whole tail from the
   first double-space run after the tag column is masked. *)
let mask_verbose_timing line =
  let tag = "  PASS  " in
  if String.starts_with ~prefix:tag line then begin
    let n = String.length line in
    let rec pad_start i =
      if i + 1 >= n then None
      else if line.[i] = ' ' && line.[i + 1] = ' ' then Some i
      else pad_start (i + 1)
    in
    match pad_start (String.length tag) with
    | Some i -> String.sub line 0 i ^ "  <duration>"
    | None -> line
  end
  else line

let mask s =
  String.concat "\n"
    (List.map
       (fun line -> mask_verbose_timing (mask_slow_entry line))
       (String.split_on_char '\n' (mask_summary_durations s)))

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
       [| "WINDTRAP_SLOW_THRESHOLD=0.000000001"; "WINDTRAP_COLOR=never" |]
       extra)

let run_once ~runner ~env ~log =
  let fd =
    Unix.openfile log [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644
  in
  let pid =
    Unix.create_process_env runner
      [| runner; "inline-test-runner"; "slow_knobs" |]
      env Unix.stdin fd fd
  in
  Unix.close fd;
  let _, status = Unix.waitpid [] pid in
  let code =
    match status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED signal -> 128 + signal
    | Unix.WSTOPPED _ -> 255
  in
  write_file log (mask (read_file log));
  code

let () =
  match Sys.argv with
  | [| _; prefix; runner |] ->
      let record name env =
        let log = prefix ^ "-" ^ name ^ "-log" in
        let code = run_once ~runner ~env ~log in
        write_file (prefix ^ "-" ^ name ^ "-exit") (string_of_int code ^ "\n")
      in
      record "compact" (scrubbed_environment [||]);
      record "verbose" (scrubbed_environment [| "WINDTRAP_VERBOSE=1" |])
  | _ ->
      prerr_endline "usage: drive_slow_knobs.exe PREFIX RUNNER";
      exit 2
