(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Retention is file-based: the log of an attempt is complete, and only the
   tail that a failure carries is bounded, with the count of the bytes it
   leaves out. *)

(* Capture state *)

type enabled = {
  root : string; (* the log root, e.g. _build/_tests *)
  suite : string; (* sanitized suite component *)
  mutable current : string option;
      (* The current test's log file: set by [with_capture] and kept after it
         returns so the runner can read the attempt's output post-mortem;
         replaced by the next attempt. *)
  mutable consumed : int;
      (* Byte offset up to which [output] has consumed the current file. *)
  mutable saved : (Unix.file_descr * Unix.file_descr) option;
      (* The real descriptors 1 and 2 while an attempt is redirected. *)
}

type t = Disabled | Enabled of enabled

let create ~log_dir ~suite () =
  Enabled
    {
      root = log_dir;
      suite = Os.sanitize_component suite;
      current = None;
      consumed = 0;
      saved = None;
    }

let disabled = Disabled

(* Capturing *)

(* C stdio buffers independently of OCaml channels: stubs calling printf
   without fflush would otherwise escape fd-level capture at consumption
   points (ppx_expect flushes the same way in its collector). *)
external flush_c_stdio : unit -> unit = "ocaml_windtrap_capture_flush_c_stdio"

(* Force buffered output through to the OS descriptors: Format buffers,
   channel buffers, and C stdio buffers are independent layers, all three
   must be flushed before the capture file is read or the descriptors are
   switched. *)
let drain () =
  Format.pp_print_flush Format.std_formatter ();
  Format.pp_print_flush Format.err_formatter ();
  flush stdout;
  flush stderr;
  flush_c_stdio ()

let output_path e ~groups ~test_name =
  let dir =
    List.fold_left Filename.concat
      (Filename.concat e.root e.suite)
      (List.map Os.sanitize_component groups)
  in
  Filename.concat dir (Os.sanitize_component test_name ^ ".output")

(* Swap descriptors 1-2 to [fd], returning the saved originals. A partial
   failure (dup exhaustion, dup2 error) undoes whatever did switch and
   re-raises, so a raise here never leaves a descriptor redirected or a
   saved dup leaked. The saved dups are close-on-exec: a subprocess the test
   execs inherits the redirected descriptors 1-2 (captured), never the real
   ones — a child that outlives the run must not hold the runner's stdout
   open, or a piped reader (`suite.exe | cat`, dune runtest) waits on it
   after the suite finished (cli/F-5). *)
let redirect_into e fd =
  let old_stdout = Unix.dup ~cloexec:true Unix.stdout in
  let old_stderr =
    try Unix.dup ~cloexec:true Unix.stderr
    with e ->
      Unix.close old_stdout;
      raise e
  in
  (* Recorded before the switch: [abandon] must find them from the first
     redirected byte on. *)
  e.saved <- Some (old_stdout, old_stderr);
  try
    Unix.dup2 fd Unix.stdout;
    Unix.dup2 fd Unix.stderr
  with exn ->
    e.saved <- None;
    (* Best effort: the original error is the one worth reporting. *)
    (try Unix.dup2 old_stdout Unix.stdout with Unix.Unix_error _ -> ());
    Unix.close old_stdout;
    Unix.close old_stderr;
    raise exn

let restore e =
  match e.saved with
  | None -> ()
  | Some (old_stdout, old_stderr) ->
      e.saved <- None;
      Unix.dup2 old_stdout Unix.stdout;
      Unix.dup2 old_stderr Unix.stderr;
      Unix.close old_stdout;
      Unix.close old_stderr

let with_capture t ~groups ~test_name fn =
  match t with
  | Disabled -> fn ()
  | Enabled e -> (
      (* Reset first: if setup fails below, the previous attempt's file must
         not be readable as this attempt's output. *)
      e.current <- None;
      e.consumed <- 0;
      let path = output_path e ~groups ~test_name in
      Os.mkdir_p (Filename.dirname path);
      (* O_TRUNC is the per-attempt reset: a retry reuses the file, so the
         report shows the final attempt's output only. O_CLOEXEC for the
         same reason the saved dups below are close-on-exec (cli/F-5): a
         subprocess the test execs writes through the redirected fds 1-2
         and must never inherit the log fd itself. *)
      let fd =
        Unix.openfile path Unix.[ O_WRONLY; O_CREAT; O_TRUNC; O_CLOEXEC ] 0o660
      in
      (* Output buffered before this attempt belongs to the real streams,
         not to this test: drain before redirecting. *)
      (try
         drain ();
         redirect_into e fd
       with exn ->
         Unix.close fd;
         raise exn);
      e.current <- Some path;
      (* Drain before restoring so buffered test output reaches the capture
         file, not the restored descriptors, and restore even when the drain
         fails. The drain's error is the answer only when [fn] returned:
         what [fn] raised is the attempt's outcome, and no cleanup error may
         replace it. *)
      let close () =
        let drained =
          match drain () with
          | () -> Ok ()
          | exception (Sys_error _ as exn) ->
              Error (exn, Printexc.get_raw_backtrace ())
        in
        restore e;
        Unix.close fd;
        drained
      in
      match fn () with
      | value -> (
          match close () with
          | Ok () -> value
          | Error (exn, backtrace) ->
              Printexc.raise_with_backtrace exn backtrace)
      | exception exn ->
          let backtrace = Printexc.get_raw_backtrace () in
          ignore (close ());
          Printexc.raise_with_backtrace exn backtrace)

let abandon = function
  | Disabled -> ()
  | Enabled e ->
      (try drain () with Sys_error _ -> ());
      restore e

(* Reading captured output *)

let with_file_in path f =
  match open_in_bin path with
  | exception Sys_error _ -> None
  | ic ->
      Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> Some (f ic))

(* Under [--stream] no captured bytes exist. A silent [""] would make an
   expectation on [output ()] pass against nothing, so the call fails the
   test. *)
let stream_error = "this test requires capture; rerun without --stream"

let output ?__POS__ t =
  match t with
  | Disabled ->
      raise
        (Failure.Check_failure
           (Failure.message ?loc:(Loc.resolve ?__POS__ ()) stream_error))
  | Enabled e -> (
      drain ();
      match e.current with
      | None -> ""
      | Some path -> (
          let read ic =
            let len = in_channel_length ic in
            if e.consumed >= len then ""
            else begin
              seek_in ic e.consumed;
              really_input_string ic (len - e.consumed)
            end
          in
          match with_file_in path read with
          | None -> ""
          | Some s ->
              e.consumed <- e.consumed + String.length s;
              s))

let is_continuation c = Char.code c land 0xC0 = 0x80

(* A suffix read can start inside a UTF-8 sequence; skip its trailing
   continuation bytes (at most 3) so the retained tail starts on a boundary.
   Best effort: when no lead byte follows within 3 bytes the data is not
   valid UTF-8 and is kept verbatim. *)
let utf8_head_skip s =
  let len = String.length s in
  let limit = min 3 len in
  let rec go i = if i < limit && is_continuation s.[i] then go (i + 1) else i in
  let cut = go 0 in
  if cut < len && not (is_continuation s.[cut]) then cut else 0

let output_tail t =
  match t with
  | Disabled -> None
  | Enabled e -> (
      match e.current with
      | None -> None
      | Some path ->
          drain ();
          let read ic =
            let len = in_channel_length ic in
            (* Failure.tail owns the report bound; reading exactly that many
               final bytes fills a report without loading the whole log. *)
            let want = min len Failure.tail_bytes in
            let start = len - want in
            seek_in ic start;
            let s = really_input_string ic want in
            let skip = if start > 0 then utf8_head_skip s else 0 in
            let s =
              if skip = 0 then s else String.sub s skip (String.length s - skip)
            in
            Failure.tail ~log_path:path ~omitted_bytes:(start + skip) s
          in
          with_file_in path read)
