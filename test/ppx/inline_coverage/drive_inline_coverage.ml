(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Inline-coverage fixture driver (T2 named change (a)): the inline (ppx)
   runner's coverage follows the library runner's rules through the shared
   Driver seam. [drive_inline_coverage.exe PREFIX RUNNER] spawns RUNNER
   with the inline-test-runner protocol argv under a scrubbed environment
   — once with the default mode (the summary line), once with
   [WINDTRAP_COVERAGE=report] — pointing [WINDTRAP_COVERAGE_FILE] at a
   fresh per-run scratch directory so the at_exit dump never lands in the
   real [_build/_coverage] and sibling detection stays inert. Summary
   durations are masked; the runtest rules diff both full transcripts
   byte-for-byte against committed goldens. A dead seam reshapes the
   transcript: no snapshot → no [coverage:] line at all; a dead
   [WINDTRAP_COVERAGE] mirror → no report block. *)

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
         "WINDTRAP_COLOR=never";
         (* The one driver that cannot answer this with
            WINDTRAP_COVERAGE=off, because the coverage line IS what it
            pins. lib/ carries an (instrumentation) stanza, so under
            --instrument-with the runner this spawns links an
            instrumented core and its line would report ~6900 points of
            windtrap instead of covlib's three. Scope the child to the
            fixture; the goldens then read the same instrumented or
            not. *)
         "WINDTRAP_COVERAGE_ONLY=test/ppx/inline_coverage/covlib.ml";
       |]
       extra)

let run_once ~runner ~env ~log =
  let fd =
    Unix.openfile log [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644
  in
  let pid =
    Unix.create_process_env runner
      [| runner; "inline-test-runner"; "inline_cov" |]
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
  write_file log (drop_discovery (mask_summary_durations (read_file log)));
  code

let () =
  match Sys.argv with
  | [| _; prefix; runner |] ->
      let record name extra =
        let dump_dir = prefix ^ "-" ^ name ^ "-dump" in
        (try Unix.mkdir dump_dir 0o755
         with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
        let dump =
          Filename.concat
            (Filename.concat (Sys.getcwd ()) dump_dir)
            "self.coverage"
        in
        let env =
          scrubbed_environment
            (Array.append [| "WINDTRAP_COVERAGE_FILE=" ^ dump |] extra)
        in
        let log = prefix ^ "-" ^ name ^ "-log" in
        let code = run_once ~runner ~env ~log in
        write_file (prefix ^ "-" ^ name ^ "-exit") (string_of_int code ^ "\n")
      in
      record "summary" [||];
      record "report" [| "WINDTRAP_COVERAGE=report" |]
  | _ ->
      prerr_endline "usage: drive_inline_coverage.exe PREFIX RUNNER";
      exit 2
