(*--------------------------------------------------------------------------
  Copyright (c) 2026 Thibaut Mattio. All rights reserved.
  SPDX-License-Identifier: ISC

  Adapted from windtrap-next's atomic_file.ml: exclusive temporary creation,
  EINTR-safe writes, and rename-based replacement are kept; the durability
  (fsync) ceremony and the structured per-operation error catalogue are
  dropped per the v3 RFC.
  --------------------------------------------------------------------------*)

let temp_prefix = ".tmp-"
let is_temp_name name = String.starts_with ~prefix:temp_prefix name
let temp_serial = Atomic.make 0
let temp_attempts = 256

let describe = function
  | Unix.Unix_error (error, _, _) -> Unix.error_message error
  | Sys_error message | Failure message -> message
  | exception_value -> Printexc.to_string exception_value

let fail path step exception_value =
  raise
    (Sys_error
       (Printf.sprintf "%s: %s: %s" path step (describe exception_value)))

(* Runs one step, translating failures to the module's Sys_error. Asynchronous
   and resource-exhaustion exceptions pass through unwrapped so callers still
   observe interrupts as interrupts. *)
let step path name callback =
  try callback () with
  | (Sys.Break | Out_of_memory | Stack_overflow) as exception_value ->
      let backtrace = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace exception_value backtrace
  | exception_value -> fail path name exception_value

let rec open_temp path flags perm =
  try Unix.openfile path flags perm
  with Unix.Unix_error (Unix.EINTR, _, _) -> open_temp path flags perm

(* EINTR on close leaves the descriptor state formally unspecified, but every
   supported platform closes it; retrying could close a reused descriptor. *)
let close_fd fd =
  try Unix.close fd with Unix.Unix_error (Unix.EINTR, _, _) -> ()

let create_temp directory perm =
  let rec attempt remaining =
    let name =
      Printf.sprintf "%s%x-%x" temp_prefix (Unix.getpid ())
        (Atomic.fetch_and_add temp_serial 1)
    in
    let temp = Filename.concat directory name in
    match open_temp temp Unix.[ O_WRONLY; O_CREAT; O_EXCL; O_CLOEXEC ] perm with
    | fd -> (temp, fd)
    | exception Unix.Unix_error (Unix.EEXIST, _, _) when remaining > 1 ->
        attempt (remaining - 1)
  in
  attempt temp_attempts

let rec write_all fd contents offset =
  if offset < String.length contents then
    match
      Unix.single_write_substring fd contents offset
        (String.length contents - offset)
    with
    | 0 -> failwith "write returned zero"
    | written -> write_all fd contents (offset + written)
    | exception Unix.Unix_error (Unix.EINTR, _, _) ->
        write_all fd contents offset

let write ?(perm = 0o666) ~path contents =
  if perm land lnot 0o777 <> 0 then
    invalid_arg "Atomic_file.write: perm must contain only bits within 0o777";
  let directory = Filename.dirname path in
  let temp, fd =
    step path "cannot create temporary file" (fun () ->
        create_temp directory perm)
  in
  let closed = ref false in
  let close_once () =
    if not !closed then begin
      closed := true;
      close_fd fd
    end
  in
  let cleanup () =
    (try close_once () with _ -> ());
    try Unix.unlink temp with _ -> ()
  in
  let publish () =
    step path "cannot write" (fun () -> write_all fd contents 0);
    step path "cannot close" close_once;
    step path "cannot replace" (fun () -> Unix.rename temp path)
  in
  try publish ()
  with exception_value ->
    let backtrace = Printexc.get_raw_backtrace () in
    cleanup ();
    Printexc.raise_with_backtrace exception_value backtrace
