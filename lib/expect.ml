(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let current_trap : Log_trap.t option ref = ref None
let set_trap t = current_trap := Some t
let clear_trap () = current_trap := None

(* Force all buffered output through to the OS file descriptor so that
   Log_trap.get_unconsumed sees everything. Format buffers and channel
   buffers are independent layers that must both be flushed. *)
let drain_formatters () =
  Format.pp_print_flush Format.std_formatter ();
  Format.pp_print_flush Format.err_formatter ();
  flush stdout;
  flush stderr

(* ───── Normalization ───── *)

let rstrip s =
  let len = String.length s in
  let rec find_end i =
    if i < 0 then 0
    else match s.[i] with ' ' | '\t' | '\r' -> find_end (i - 1) | _ -> i + 1
  in
  let end_pos = find_end (len - 1) in
  if end_pos = len then s else String.sub s 0 end_pos

let leading_spaces s =
  let len = String.length s in
  let rec count i = if i >= len || s.[i] <> ' ' then i else count (i + 1) in
  count 0

let dedent lines =
  let non_empty = List.filter (fun s -> String.length s > 0) lines in
  match non_empty with
  | [] -> lines
  | _ ->
      let min_indent =
        List.fold_left
          (fun acc s -> min acc (leading_spaces s))
          max_int non_empty
      in
      if min_indent = 0 then lines
      else
        List.map
          (fun s ->
            let len = String.length s in
            if len = 0 then s else String.sub s min_indent (len - min_indent))
          lines

let normalize s =
  let s = Text.normalize_newlines s in
  let lines = String.split_on_char '\n' s in
  let lines = List.map rstrip lines in
  let rec drop_leading = function
    | [] -> []
    | "" :: rest -> drop_leading rest
    | lines -> lines
  in
  let lines = drop_leading lines in
  let lines = List.rev (drop_leading (List.rev lines)) in
  let lines = dedent lines in
  String.concat "\n" lines

(* ───── Output Retrieval ───── *)

let output () =
  match !current_trap with
  | None -> ""
  | Some trap ->
      drain_formatters ();
      let out = Log_trap.get_unconsumed trap in
      Log_trap.consume trap;
      out

(* ───── Expect Functions ───── *)

let raise_output_mismatch message ~expected ~actual =
  let diff =
    Myers.diff ~expected_label:"expected" ~actual_label:"actual" expected actual
    |> Diff_display.colorize_unified_diff
  in
  Failure.raise_failure (Pp.str "%s@.@.%s" message diff)

let expect_exact expected =
  match !current_trap with
  | None ->
      if expected <> "" then
        raise_output_mismatch "Output mismatch (no capture context)" ~expected
          ~actual:""
  | Some trap ->
      drain_formatters ();
      let actual = Log_trap.get_unconsumed trap in
      Log_trap.consume trap;
      if actual <> expected then raise_output_mismatch "Output mismatch" ~expected ~actual

let expect expected =
  match !current_trap with
  | None ->
      let expected_norm = normalize expected in
      if expected_norm <> "" then
        raise_output_mismatch "Output mismatch (no capture context)"
          ~expected:expected_norm ~actual:""
  | Some trap ->
      drain_formatters ();
      let actual = Log_trap.get_unconsumed trap in
      Log_trap.consume trap;
      let actual_norm = normalize actual in
      let expected_norm = normalize expected in
      if actual_norm <> expected_norm then
        raise_output_mismatch "Output mismatch" ~expected:expected_norm
          ~actual:actual_norm

(* ───── Capture Functions ───── *)

(* Standalone capture: redirects stdout/stderr to a temp file rather than
   relying on Log_trap. Used by [capture] and [capture_exact] which operate
   outside the runner's log infrastructure. *)
let capture_impl ~normalize_output fn expected =
  let tmp = Filename.temp_file "windtrap_capture" ".out" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove tmp with _ -> ())
    (fun () ->
      let fd =
        Unix.openfile tmp [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600
      in
      let old_stdout = Unix.dup Unix.stdout in
      let old_stderr = Unix.dup Unix.stderr in
      Fun.protect
        ~finally:(fun () ->
          Unix.dup2 old_stdout Unix.stdout;
          Unix.dup2 old_stderr Unix.stderr;
          Unix.close old_stdout;
          Unix.close old_stderr;
          Unix.close fd)
        (fun () ->
          Unix.dup2 fd Unix.stdout;
          Unix.dup2 fd Unix.stderr;
          let result = fn () in
          drain_formatters ();
          let ic = open_in tmp in
          let actual =
            Fun.protect
              ~finally:(fun () -> close_in ic)
              (fun () -> really_input_string ic (in_channel_length ic))
          in
          let actual_cmp, expected_cmp =
            if normalize_output then (normalize actual, normalize expected)
            else (actual, expected)
          in
          if actual_cmp <> expected_cmp then
            raise_output_mismatch "Output mismatch" ~expected:expected_cmp
              ~actual:actual_cmp;
          result))

let capture fn expected = capture_impl ~normalize_output:true fn expected
let capture_exact fn expected = capture_impl ~normalize_output:false fn expected
