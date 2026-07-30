(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Capture mechanics adapted from v1's lib/log_trap.ml (fd-level dup2
   redirection, formatter draining before descriptor restoration, symlink
   retry loops) and the live half of v1's lib/expect.ml (incremental
   consumption). Retention is bounded-tail (tail + exact drop count) over a
   simple file-based design: the file is complete, only the failure report
   is bounded. *)

(* Retention limits *)

type limits = { tail_bytes : int }

let limits ~tail_bytes =
  if tail_bytes < 0 then invalid_arg "Capture.limits: tail_bytes is negative";
  { tail_bytes }

let default_limits = { tail_bytes = 8_192 }
let tail_bytes l = l.tail_bytes

(* Capture state *)

type enabled = {
  root : string; (* the log root, e.g. _build/_tests *)
  suite : string; (* sanitized suite component *)
  run_id : string;
  limits : limits;
  mutable current : string option;
      (* The current test's log file: set by [with_capture] and kept after it
         returns so the runner can read the attempt's output post-mortem;
         replaced by the next attempt. *)
  mutable consumed : int;
      (* Byte offset up to which [output] has consumed the current file. *)
}

type t = Disabled | Enabled of enabled

(* Run ids are 8 base-36 characters (0-9, A-Z) — v1's format — drawn from one
   OS-entropy word so no module-level PRNG state exists. *)
let generate_run_id () =
  let bytes = Bytes.create 8 in
  let v = ref (Seed.random ()) in
  for i = 0 to 7 do
    let d = Int64.to_int (Int64.unsigned_rem !v 36L) in
    v := Int64.unsigned_div !v 36L;
    Bytes.set bytes i
      (if d < 10 then Char.chr (Char.code '0' + d)
       else Char.chr (Char.code 'A' + d - 10))
  done;
  Bytes.to_string bytes

let create ?(limits = default_limits) ~log_dir ~suite () =
  Enabled
    {
      root = log_dir;
      suite = Path_ops.sanitize_component suite;
      run_id = generate_run_id ();
      limits;
      current = None;
      consumed = 0;
    }

let disabled = Disabled
let run_dir_of e = Filename.concat e.root (Filename.concat e.suite e.run_id)
let run_dir = function Disabled -> None | Enabled e -> Some (run_dir_of e)

(* Capturing *)

(* C stdio buffers independently of OCaml channels: stubs calling printf
   without fflush would otherwise escape fd-level capture at consumption
   points (ppx_expect flushes the same way in its collector). *)
external flush_c_stdio : unit -> unit = "ocaml_windtrap_capture_flush_c_stdio"

(* Force buffered output through to the OS descriptors: Format buffers,
   channel buffers, and C stdio buffers are independent layers, all three
   must be flushed before the capture file is read or the descriptors are
   switched. *)
let drain_formatters () =
  Format.pp_print_flush Format.std_formatter ();
  Format.pp_print_flush Format.err_formatter ();
  flush stdout;
  flush stderr;
  flush_c_stdio ()

let output_path e ~groups ~test_name =
  let dir =
    List.fold_left Filename.concat (run_dir_of e)
      (List.map Path_ops.sanitize_component groups)
  in
  Filename.concat dir (Path_ops.sanitize_component test_name ^ ".output")

(* Swap descriptors 1-2 to [fd], returning the saved originals. A partial
   failure (dup exhaustion, dup2 error) undoes whatever did switch and
   re-raises, so a raise here never leaves a descriptor redirected or a
   saved dup leaked. The saved dups are close-on-exec: a subprocess the test
   execs inherits the redirected descriptors 1-2 (captured), never the real
   ones — a child that outlives the run must not hold the runner's stdout
   open, or a piped reader (`suite.exe | cat`, dune runtest) waits on it
   after the suite finished (cli/F-5). *)
let redirect_into fd =
  let old_stdout = Unix.dup ~cloexec:true Unix.stdout in
  let old_stderr =
    try Unix.dup ~cloexec:true Unix.stderr
    with e ->
      Unix.close old_stdout;
      raise e
  in
  (try
     Unix.dup2 fd Unix.stdout;
     Unix.dup2 fd Unix.stderr
   with e ->
     (* Best effort: the original error is the one worth reporting. *)
     (try Unix.dup2 old_stdout Unix.stdout with Unix.Unix_error _ -> ());
     Unix.close old_stdout;
     Unix.close old_stderr;
     raise e);
  (old_stdout, old_stderr)

let restore_from (old_stdout, old_stderr) =
  Unix.dup2 old_stdout Unix.stdout;
  Unix.dup2 old_stderr Unix.stderr;
  Unix.close old_stdout;
  Unix.close old_stderr

let with_capture t ~groups ~test_name fn =
  match t with
  | Disabled -> fn ()
  | Enabled e ->
      (* Reset first: if setup fails below, the previous attempt's file must
         not be readable as this attempt's output. *)
      e.current <- None;
      e.consumed <- 0;
      let path = output_path e ~groups ~test_name in
      Path_ops.mkdir_p (Filename.dirname path);
      (* O_TRUNC is the per-attempt reset: a retry reuses the file, so the
         report shows the final attempt's output only. O_CLOEXEC for the
         same reason the saved dups below are close-on-exec (cli/F-5): a
         subprocess the test execs writes through the redirected fds 1-2
         and must never inherit the log fd itself. *)
      let fd =
        Unix.openfile path Unix.[ O_WRONLY; O_CREAT; O_TRUNC; O_CLOEXEC ] 0o660
      in
      let saved =
        (* Output buffered before this attempt belongs to the real streams,
           not to this test: drain before redirecting. *)
        try
          drain_formatters ();
          redirect_into fd
        with e ->
          Unix.close fd;
          raise e
      in
      e.current <- Some path;
      Fun.protect
        ~finally:(fun () ->
          (* Drain before restoring so buffered test output reaches the
             capture file, not the restored descriptors — and restore even
             when the drain fails (its error still propagates). *)
          Fun.protect
            ~finally:(fun () ->
              restore_from saved;
              Unix.close fd)
            drain_formatters)
        fn

(* Reading captured output *)

let with_file_in path f =
  match open_in_bin path with
  | exception Sys_error _ -> None
  | ic ->
      Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> Some (f ic))

let stream_error = "this test requires capture; rerun without --stream"

let output ?pos t =
  match t with
  | Disabled ->
      raise
        (Failure.Check_failure
           (Failure.message ?loc:(Loc.resolve ?pos ()) stream_error))
  | Enabled e -> (
      drain_formatters ();
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
          drain_formatters ();
          let read ic =
            let len = in_channel_length ic in
            let want = min len e.limits.tail_bytes in
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

(* Latest links *)

(* Retry loops handle EINTR (interrupted syscall) and EEXIST (race with a
   concurrent run re-pointing the same link); other errors abandon the link —
   these are conveniences, never worth failing a run over. *)

let unlink_with_retry path =
  let rec loop retries =
    try Unix.unlink path with
    | Unix.Unix_error (Unix.ENOENT, _, _) -> ()
    | Unix.Unix_error (Unix.EINTR, _, _) when retries < 5 -> loop (retries + 1)
    | Unix.Unix_error _ -> ()
  in
  loop 0

let symlink_with_retry ~target ~link_name =
  let rec loop retries =
    try Unix.symlink target link_name with
    | Unix.Unix_error (Unix.EEXIST, _, _) when retries < 5 ->
        unlink_with_retry link_name;
        loop (retries + 1)
    | Unix.Unix_error (Unix.EINTR, _, _) when retries < 5 -> loop (retries + 1)
    | Unix.Unix_error _ -> ()
  in
  loop 0

let link_latest t =
  match t with
  | Disabled -> ()
  (* Windows lacks reliable symlink support without elevated privileges. *)
  | Enabled _ when Sys.win32 -> ()
  | Enabled e -> (
      try
        let suite_dir = Filename.concat e.root e.suite in
        Path_ops.mkdir_p suite_dir;
        let suite_latest = Filename.concat suite_dir "latest" in
        unlink_with_retry suite_latest;
        symlink_with_retry ~target:e.run_id ~link_name:suite_latest;
        let root_latest = Filename.concat e.root "latest" in
        unlink_with_retry root_latest;
        symlink_with_retry
          ~target:(Filename.concat e.suite e.run_id)
          ~link_name:root_latest
      with Unix.Unix_error _ | Sys_error _ -> ())
