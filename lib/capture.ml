(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Capture state *)

(* The log of an attempt stays current after [with_capture] returns, so the
   runner reads the output of a finished attempt; the next attempt replaces
   it. *)
type capturing = {
  dir : string; (* [<log_dir>/<suite>], which holds the logs *)
  mutable log : string option; (* the current log *)
  mutable cursor : int; (* the bytes of [log] that [output] returned *)
  mutable saved : (Unix.file_descr * Unix.file_descr) option;
      (* the real descriptors 1 and 2, while an attempt is redirected *)
}

type t = Disabled | Capturing of capturing

let create ~log_dir ~suite () =
  let dir = Filename.concat log_dir (Os.sanitize_component suite) in
  Capturing { dir; log = None; cursor = 0; saved = None }

let disabled = Disabled

(* Capturing *)

(* The C stdio streams buffer apart from the OCaml channels: what a stub
   prints without [fflush] reaches no descriptor until they are flushed. *)
external flush_c_stdio : unit -> unit = "ocaml_windtrap_capture_flush_c_stdio"

let drain () =
  Format.pp_print_flush Format.std_formatter ();
  Format.pp_print_flush Format.err_formatter ();
  flush stdout;
  flush stderr;
  flush_c_stdio ()

let log_path c ~groups ~test_name =
  let dir =
    List.fold_left Filename.concat c.dir (List.map Os.sanitize_component groups)
  in
  Filename.concat dir (Os.sanitize_component test_name ^ ".output")

let restore c =
  match c.saved with
  | None -> ()
  | Some (stdout, stderr) ->
      c.saved <- None;
      Unix.dup2 stdout Unix.stdout;
      Unix.dup2 stderr Unix.stderr;
      Unix.close stdout;
      Unix.close stderr

(* The saved descriptors are close-on-exec: a child that outlives the run
   must not hold the real standard output open, or a reader of the run's pipe
   waits for it after the suite has finished. They are recorded before the
   switch so that [abandon] finds them from the first redirected byte on. *)
let redirect c log =
  let stdout = Unix.dup ~cloexec:true Unix.stdout in
  let stderr =
    try Unix.dup ~cloexec:true Unix.stderr
    with exn ->
      Unix.close stdout;
      raise exn
  in
  c.saved <- Some (stdout, stderr);
  try
    Unix.dup2 log Unix.stdout;
    Unix.dup2 log Unix.stderr
  with exn ->
    (* Best effort: the switch's error is the one worth reporting. *)
    (try restore c with Unix.Unix_error _ -> ());
    raise exn

(* The descriptors come back even when the drain fails, and the drain's error
   is returned for the caller to weigh against the attempt's own outcome. *)
let release c =
  let drained =
    match drain () with
    | () -> Ok ()
    | exception (Sys_error _ as exn) ->
        Error (exn, Printexc.get_raw_backtrace ())
  in
  restore c;
  drained

let with_capture t ~groups ~test_name fn =
  match t with
  | Disabled -> fn ()
  | Capturing c -> (
      (* Reset before any step that can raise, so a failed setup leaves no log
         of the attempt before. *)
      c.log <- None;
      c.cursor <- 0;
      let path = log_path c ~groups ~test_name in
      Os.mkdir_p (Filename.dirname path);
      let log =
        Unix.openfile path Unix.[ O_WRONLY; O_CREAT; O_TRUNC; O_CLOEXEC ] 0o660
      in
      (* Once redirected, descriptors 1 and 2 alone hold the log open. *)
      Fun.protect
        ~finally:(fun () -> Unix.close log)
        (fun () ->
          drain ();
          redirect c log);
      c.log <- Some path;
      match fn () with
      | value -> (
          match release c with
          | Ok () -> value
          | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt)
      | exception exn ->
          let bt = Printexc.get_raw_backtrace () in
          ignore (release c);
          Printexc.raise_with_backtrace exn bt)

let abandon = function Disabled -> () | Capturing c -> ignore (release c)

(* Reading captured output *)

let read_log path read =
  match open_in_bin path with
  | exception Sys_error reason -> Error reason
  | ic ->
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () -> Ok (read ic))

(* A silent [""] would let an expectation on [output ()] pass against output
   that was never captured, so a call with no bytes to return fails the test. *)
let no_bytes ?__POS__ message =
  raise
    (Failure.Check_failure
       (Failure.message ?loc:(Loc.resolve ?__POS__ ()) message))

let output ?__POS__ t =
  match t with
  | Disabled ->
      no_bytes ?__POS__ "this test requires capture; rerun without --stream"
  | Capturing c -> (
      drain ();
      match c.log with
      | None -> ""
      | Some path -> (
          (* The test can truncate its own log below the cursor. *)
          let unread ic =
            let length = in_channel_length ic in
            seek_in ic c.cursor;
            really_input_string ic (max 0 (length - c.cursor))
          in
          match read_log path unread with
          | Error reason ->
              no_bytes ?__POS__
                ("this test's captured output cannot be read: " ^ reason)
          | Ok s ->
              c.cursor <- c.cursor + String.length s;
              s))

(* No drain: the attempt drained its buffers into the log before the real
   descriptors came back, and a drain now would flush those, whose failure is
   no fact about the test. *)
let output_tail = function
  | Disabled -> None
  | Capturing c ->
      let tail path ic =
        let length = in_channel_length ic in
        let start = max 0 (length - Failure.tail_bytes) in
        seek_in ic start;
        let s = really_input_string ic (length - start) in
        (* A cut inside a UTF-8 sequence moves past it, by three bytes at
           most, and bytes with no lead in reach are kept. A cut log fills
           [s] with [Failure.tail_bytes] bytes, so [s.[3]] exists. *)
        let rec lead i =
          if i > 3 then 0
          else if Char.code s.[i] land 0xC0 = 0x80 then lead (i + 1)
          else i
        in
        let skip = if start = 0 then 0 else lead 0 in
        let text = String.sub s skip (String.length s - skip) in
        Failure.tail ~log_path:path ~omitted_bytes:(start + skip) text
      in
      Option.bind c.log (fun path ->
          Result.to_option (read_log path (tail path)))
