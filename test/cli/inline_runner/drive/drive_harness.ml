(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The process harness behind every transcript golden under
   test/cli/inline_runner and every runner run of test/conformance.

   Each of these rules asks the same question — what does a real
   generated runner print, on which stream, and what does it exit with —
   and each answer is diffed byte-for-byte against a committed golden.
   Only two things can make such a golden lie: an environment variable
   the developer's shell happens to carry, and a number that is measured
   rather than computed. [environment] answers the first by stating the
   child's whole environment, so that only the bindings the rule names
   reach it; [transcript] answers the second by masking what varies. *)

type mask =
  | Full_log  (** the [full log: <path>] tail — a random per-run directory *)
  | Slow_column  (** the slow block's right-aligned duration column *)
  | Verbose_timing  (** the verbose per-test line's timing tail *)
  | Backtrace  (** backtrace frames, which name lines inside the runtime *)
  | Os_reason
      (** the system's own words ending a [windtrap: could not] line, which
          differ between platforms *)

let write_file path contents =
  let oc = open_out_bin path in
  output_string oc contents;
  close_out oc

(* Masks every [in <number><unit>] duration token, the unit included
   since it varies with the measurement: ["1 failed in 2.1ms."] becomes
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

(* Offset of the first double-space run at or after [from] — the boundary
   both timing maskers cut on, since every column in these transcripts is
   separated by two spaces. *)
let gap_from line from =
  let n = String.length line in
  let rec go i =
    if i + 1 >= n then None
    else if line.[i] = ' ' && line.[i + 1] = ' ' then Some i
    else go (i + 1)
  in
  go from

(* The slow block's entries lead with a right-aligned duration column
   (["  1.3ms  <path>"]); the value and the alignment padding both vary with
   the measurement, so the column is masked whole. An entry is an indented
   line whose first non-blank character is a digit — the verbose per-test
   lines lead with their status tag instead, and the heading with a letter. *)
let mask_slow_column line =
  let n = String.length line in
  let rec skip_spaces i =
    if i < n && line.[i] = ' ' then skip_spaces (i + 1) else i
  in
  let start = skip_spaces 0 in
  if start < 2 || start >= n || line.[start] < '0' || line.[start] > '9' then
    line
  else
    match gap_from line start with
    | Some i -> "  <duration>  " ^ String.sub line (i + 2) (n - i - 2)
    | None -> line

(* Verbose per-test lines right-pad the name and end with the timing; both
   widths vary with the measured duration, so the whole tail from the
   first double-space run after the tag column is masked. *)
let mask_verbose_timing line =
  let tag = "  PASS  " in
  if String.starts_with ~prefix:tag line then
    match gap_from line (String.length tag) with
    | Some i -> String.sub line 0 i ^ "  <duration>"
    | None -> line
  else line

(* Backtrace frames, collapsed to one marker

   A crashing partition's report carries a real backtrace whose frames
   name file:line inside ppx/runtime/ppx_runtime.ml — an unmasked golden
   would break on every future edit to the runtime, over a line number
   that is not what the directory pins. The marker keeps the fact that a
   backtrace was printed, which is the part that matters: a crash is
   reported as a crash, not as a correction. *)
let mask_backtrace lines =
  let leading line =
    let n = String.length line in
    let rec go i = if i < n && line.[i] = ' ' then go (i + 1) else i in
    go 0
  in
  let is_frame line =
    let trimmed = String.trim line in
    List.exists
      (fun prefix -> String.starts_with ~prefix trimmed)
      [ "Raised at "; "Re-raised at "; "Called from "; "Raised by primitive " ]
  in
  let rec go acc = function
    | [] -> List.rev acc
    | line :: rest when is_frame line ->
        let rec skip = function l :: r when is_frame l -> skip r | r -> r in
        go ((String.make (leading line) ' ' ^ "<backtrace>") :: acc) (skip rest)
    | line :: rest -> go (line :: acc) rest
  in
  go [] lines

(* A refusal to write names the file, then the system's reason after the
   last [": "]; the reason is the platform's text ([strerror] here, its
   own wording on Windows), so it is masked and the file kept. *)
let mask_os_reason line =
  let prefix = "windtrap: could not " in
  if not (String.starts_with ~prefix line) then line
  else
    let rec last_sep i =
      if i < String.length prefix then None
      else if line.[i] = ':' && i + 1 < String.length line && line.[i + 1] = ' '
      then Some i
      else last_sep (i - 1)
    in
    match last_sep (String.length line - 2) with
    | Some i -> String.sub line 0 i ^ ": <reason>"
    | None -> line

let transcript masks s =
  let masked m = List.mem m masks in
  let lines = String.split_on_char '\n' (mask_durations s) in
  let lines = if masked Backtrace then mask_backtrace lines else lines in
  let line_mask line =
    let line = if masked Full_log then mask_full_log line else line in
    let line = if masked Slow_column then mask_slow_column line else line in
    let line = if masked Os_reason then mask_os_reason line else line in
    if masked Verbose_timing then mask_verbose_timing line else line
  in
  String.concat "\n" (List.map line_mask lines)

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

(* The child's environment is stated, never inherited: what
   [Windtrap_test_support.Child] passes, then [INSIDE_DUNE] as dune set it
   for this driver, then [extra], which is the whole of what a rule pins.
   [INSIDE_DUNE] is the one variable passed through: it is how a runner
   started by a build rule finds the build directory it was started in,
   and a rule that means a runner outside any build says so by binding it
   empty. A later binding for a name replaces an earlier one. *)
let environment extra =
  let inside_dune =
    match Sys.getenv_opt "INSIDE_DUNE" with
    | Some value -> [ ("INSIDE_DUNE", value) ]
    | None -> []
  in
  inside_dune @ extra

(* The exit file's line: the code, or the signal that ended the child. *)
let status_line = function
  | Unix.WEXITED code -> string_of_int code
  | Unix.WSIGNALED signal -> Printf.sprintf "killed by signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "stopped by signal %d" signal

(* The two streams, masked apart and written one after the other under a
   line that names the second: which stream carried a line is part of
   what a golden pins, and the report's own output never starts a line
   with that marker. *)
let log masks ~out ~err =
  let out = transcript masks out and err = transcript masks err in
  let out =
    if out = "" || String.ends_with ~suffix:"\n" out then out else out ^ "\n"
  in
  out ^ "--- stderr ---\n" ^ err

(* One run, as the goldens read it: [NAME-log] holds the masked output and
   [NAME-exit] how the child ended. [probe] runs in the child's cwd after
   it ended, the one observation a transcript cannot carry (whether the
   run left a file behind), and its text is appended under a line of its
   own. [decorate] rewrites the log. *)
let record ?(probe = fun () -> "") ?(decorate = Fun.id) ~name ~exe ~args ~env
    ~masks () =
  let result =
    Windtrap_test_support.Child.run ~env:(environment env) exe args
  in
  let probed =
    match probe () with "" -> "" | text -> "--- probe ---\n" ^ text
  in
  write_file (name ^ "-log")
    (decorate (log masks ~out:result.out ~err:result.err) ^ probed);
  write_file (name ^ "-exit") (status_line result.status ^ "\n")
