(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The process harness behind every transcript golden under test/ppx.

   Each of these directories asks the same question — what does a real
   generated runner print, and what does it exit with — and each answer
   is diffed byte-for-byte against a committed golden. Only two things
   can make such a golden lie: an environment variable the developer's
   shell happens to carry, and a number that is measured rather than
   computed. [environment] answers the first by scrubbing every
   WINDTRAP_* mirror and every CI/color variable out of the child's
   environment, leaving only the bindings the fixture's own rule names;
   [transcript] answers the second by masking what varies.

   Eight copies of this file used to sit next to their fixtures, so a
   change to the masking convention took eight edits and one of the eight
   still carried its neighbour's title. *)

type mask =
  | Full_log  (** the [full log: <path>] tail — a random per-run directory *)
  | Slow_column  (** the slow block's right-aligned duration column *)
  | Verbose_timing  (** the verbose per-test line's timing tail *)
  | Backtrace  (** backtrace frames, which name lines inside the runtime *)

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

(* The mutation discovery line, dropped

   lib/ carries an (instrumentation (backend ppx_windtrap.mutate)) stanza,
   so under --instrument-with every run these drivers spawn ends with
   "mutants: N in M files ...". That line is a true statement about the
   build and it is not what these goldens are about — they pin the
   RUNNER's transcript. There is no environment knob for it on purpose
   (WINDTRAP_MUTATE=off still announces; test/mutate_loop pins that), so
   the harness drops it here rather than the run suppressing it. *)
let drop_discovery s =
  String.split_on_char '\n' s
  |> List.filter (fun line -> not (String.starts_with ~prefix:"mutants: " line))
  |> String.concat "\n"

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

let transcript masks s =
  let masked m = List.mem m masks in
  let lines = String.split_on_char '\n' (mask_durations s) in
  let lines = if masked Backtrace then mask_backtrace lines else lines in
  let line_mask line =
    let line = if masked Full_log then mask_full_log line else line in
    let line = if masked Slow_column then mask_slow_column line else line in
    if masked Verbose_timing then mask_verbose_timing line else line
  in
  drop_discovery (String.concat "\n" (List.map line_mask lines))

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

(* The child's environment: this process's, minus every variable that
   could reshape a pinned transcript, plus [extra] — which is the whole
   of what a fixture's rule pins, so the rule reads as the environment
   the golden was recorded under. A later binding for a name replaces an
   earlier one; [WINDTRAP_COLOR=never] is the one default, since no
   golden carries escape sequences. *)
let environment extra =
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
  let bindings =
    List.fold_left
      (fun acc (name, value) -> (name, value) :: List.remove_assoc name acc)
      [ ("WINDTRAP_COLOR", "never") ]
      extra
  in
  Array.append
    (Array.of_list (List.filter keep (Array.to_list (Unix.environment ()))))
    (Array.of_list
       (List.rev_map (fun (name, value) -> name ^ "=" ^ value) bindings))

let spawn ~exe ~args ~env ~log =
  let fd =
    Unix.openfile log [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644
  in
  let pid =
    Unix.create_process_env exe
      (Array.of_list (exe :: args))
      env Unix.stdin fd fd
  in
  Unix.close fd;
  let _, status = Unix.waitpid [] pid in
  match status with
  | Unix.WEXITED code -> code
  | Unix.WSIGNALED signal -> 128 + signal
  | Unix.WSTOPPED _ -> 255

(* One run, as the goldens read it: [NAME-log] holds the masked combined
   output and [NAME-exit] the exit code. [probe] runs in the child's cwd
   before the log is rewritten — the one observation a transcript cannot
   carry, whether the run left a file behind — and its text is appended.
   [decorate] rewrites the masked transcript. *)
let record ?(probe = fun () -> "") ?(decorate = Fun.id) ~name ~exe ~args ~env
    ~masks () =
  let log = name ^ "-log" in
  let code = spawn ~exe ~args ~env ~log in
  let probed = probe () in
  write_file log (decorate (transcript masks (read_file log)) ^ probed);
  write_file (name ^ "-exit") (string_of_int code ^ "\n");
  code

let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Array.iter
        (fun name -> remove_tree (Filename.concat path name))
        (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
