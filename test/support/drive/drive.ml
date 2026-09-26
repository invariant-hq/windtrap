(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The transcript driver of the conformance corpus's runners. A corpus rule
   runs a runner and diffs what it printed against a golden, and a diff rule
   needs its file even when the runner corrected nothing, which a cram
   session does not give.

   Usage:

     drive.exe [OPTION]... --run NAME EXE [ARG]...

   spawns EXE with ARGs in a stated environment and records [NAME-log] (its
   masked standard output, then its masked standard error under a
   [--- stderr ---] line) and [NAME-exit] (its exit code).

     --env K=V       a binding for the child
     --mask full-log mask the [full log: <path>] tail, a per-run directory
     --placeholder F after the run, write F.corrected as the line
                     [=== no correction produced ===] unless the run wrote
                     it *)

let strf = Printf.sprintf

let usage () =
  prerr_endline
    "usage: drive.exe [--env K=V] [--mask full-log] [--placeholder FILE] --run \
     NAME EXE [ARG]...";
  exit 2

let write_file path contents =
  Out_channel.with_open_bin path (fun oc ->
      Out_channel.output_string oc contents)

(* Masks every [in <number><unit>] duration token, the unit included since
   it varies with the measurement: ["1 failed in 2.1ms."] becomes
   ["1 failed in <duration>."]. *)
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
      let stop =
        if !k + 1 < n && s.[!k] = 'm' && s.[!k + 1] = 's' then !k + 2
        else if !k < n && s.[!k] = 's' then !k + 1
        else !k
      in
      if !k > j && stop > !k then begin
        Buffer.add_string b "<duration>";
        i := stop
      end
      else i := j
    end
    else begin
      Buffer.add_char b s.[!i];
      incr i
    end
  done;
  Buffer.contents b

let mask_full_log line =
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

let transcript ~full_log s =
  let lines = String.split_on_char '\n' (mask_durations s) in
  let lines = if full_log then List.map mask_full_log lines else lines in
  String.concat "\n" lines

(* Which stream carried a line is part of what a golden pins, and a report
   never starts a line with the marker. *)
let log ~full_log ~out ~err =
  let out = transcript ~full_log out and err = transcript ~full_log err in
  let out =
    if out = "" || String.ends_with ~suffix:"\n" out then out else out ^ "\n"
  in
  out ^ "--- stderr ---\n" ^ err

let status_line = function
  | Unix.WEXITED code -> string_of_int code
  | Unix.WSIGNALED signal -> strf "killed by signal %d" signal
  | Unix.WSTOPPED signal -> strf "stopped by signal %d" signal

(* [INSIDE_DUNE] is passed through: a runner started by a build rule finds
   the build directory it was started in by it. *)
let environment env =
  match Sys.getenv_opt "INSIDE_DUNE" with
  | Some value -> ("INSIDE_DUNE", value) :: env
  | None -> env

let placeholder = "=== no correction produced ===\n"

let () =
  let rec parse ~env ~full_log ~placeholders = function
    | "--env" :: binding :: rest -> (
        match String.index_opt binding '=' with
        | Some i ->
            let name = String.sub binding 0 i in
            let value =
              String.sub binding (i + 1) (String.length binding - i - 1)
            in
            parse ~env:((name, value) :: env) ~full_log ~placeholders rest
        | None -> usage ())
    | "--mask" :: "full-log" :: rest ->
        parse ~env ~full_log:true ~placeholders rest
    | "--placeholder" :: file :: rest ->
        parse ~env ~full_log ~placeholders:(file :: placeholders) rest
    | "--run" :: name :: exe :: args ->
        (List.rev env, full_log, List.rev placeholders, name, exe, args)
    | _ -> usage ()
  in
  let env, full_log, placeholders, name, exe, args =
    parse ~env:[] ~full_log:false ~placeholders:[]
      (List.tl (Array.to_list Sys.argv))
  in
  let exe =
    if Filename.is_relative exe then Filename.concat (Sys.getcwd ()) exe
    else exe
  in
  let result =
    Windtrap_test_support.Child.run ~env:(environment env) exe args
  in
  write_file (name ^ "-log") (log ~full_log ~out:result.out ~err:result.err);
  write_file (name ^ "-exit") (status_line result.status ^ "\n");
  List.iter
    (fun file ->
      let corrected = file ^ ".corrected" in
      if not (Sys.file_exists corrected) then write_file corrected placeholder)
    placeholders
