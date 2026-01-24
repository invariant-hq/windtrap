(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* ───── Types ───── *)

type t = {
  root : string;
  suite_name : string;
  run_id : string;
  enabled : bool;
  mutable current_path : string option;
  (* Byte offset into the capture file up to which output has been consumed
     by expect assertions. This enables incremental reads: [get_unconsumed]
     returns only bytes after this offset. *)
  mutable consumed : int;
}

let create ~root ~suite_name ~run_id ~enabled =
  { root; suite_name; run_id; enabled; current_path = None; consumed = 0 }

(* ───── Expect Support ───── *)

(* Force all buffered output through to the OS file descriptor so that
   subsequent reads of the capture file see everything written so far. *)
let drain_formatters () =
  Format.pp_print_flush Format.std_formatter ();
  Format.pp_print_flush Format.err_formatter ();
  flush stdout;
  flush stderr

let reset_for_test t =
  t.current_path <- None;
  t.consumed <- 0

let get_unconsumed t =
  drain_formatters ();
  match t.current_path with
  | None -> ""
  | Some path ->
      if not (Sys.file_exists path) then ""
      else
        let ic = open_in_bin path in
        Fun.protect
          ~finally:(fun () -> close_in ic)
          (fun () ->
            let len = in_channel_length ic in
            if t.consumed >= len then ""
            else begin
              seek_in ic t.consumed;
              really_input_string ic (len - t.consumed)
            end)

let consume t =
  drain_formatters ();
  match t.current_path with
  | None -> ()
  | Some path ->
      if Sys.file_exists path then
        let ic = open_in_bin path in
        Fun.protect
          ~finally:(fun () -> close_in ic)
          (fun () -> t.consumed <- in_channel_length ic)

(* ───── Capture ───── *)

let log_dir t = Filename.concat t.root (Filename.concat t.suite_name t.run_id)

let test_output_path t ~groups ~test_name =
  let dir =
    List.fold_left Filename.concat (log_dir t)
      (List.map Path_ops.sanitize_component groups)
  in
  Filename.concat dir (Path_ops.sanitize_component test_name ^ ".output")

(* Redirects stdout/stderr at the file-descriptor level using dup2 so that
   output from C stubs and transitive dependencies is also captured, not
   just OCaml channel writes. *)
let with_capture t ~groups ~test_name fn =
  if not t.enabled then begin
    t.current_path <- None;
    t.consumed <- 0;
    fn ()
  end
  else begin
    let path = test_output_path t ~groups ~test_name in
    Path_ops.mkdir_p (Filename.dirname path);
    t.current_path <- Some path;
    t.consumed <- 0;
    let fd = Unix.openfile path Unix.[ O_WRONLY; O_CREAT; O_TRUNC ] 0o660 in
    let old_stdout = Unix.dup Unix.stdout in
    let old_stderr = Unix.dup Unix.stderr in
    Unix.dup2 fd Unix.stdout;
    Unix.dup2 fd Unix.stderr;
    Fun.protect
      ~finally:(fun () ->
        drain_formatters ();
        Unix.dup2 old_stdout Unix.stdout;
        Unix.dup2 old_stderr Unix.stderr;
        Unix.close old_stdout;
        Unix.close old_stderr;
        Unix.close fd;
        t.current_path <- None)
      fn
  end

(* ───── Symlinks ───── *)

(* Retry loops handle EINTR (interrupted syscall) and EEXIST (race with
   concurrent test processes creating the same symlink). *)

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

let setup_symlinks t =
  if not t.enabled then ()
    (* Windows lacks reliable symlink support without elevated privileges. *)
  else if Sys.win32 then ()
  else begin
    let suite_dir = Filename.concat t.root t.suite_name in
    let suite_latest = Filename.concat suite_dir "latest" in
    unlink_with_retry suite_latest;
    symlink_with_retry ~target:t.run_id ~link_name:suite_latest;
    let root_latest = Filename.concat t.root "latest" in
    let target = Filename.concat t.suite_name t.run_id in
    unlink_with_retry root_latest;
    symlink_with_retry ~target ~link_name:root_latest
  end

(* ───── Reporting ───── *)

let print_location t =
  if t.enabled then begin
    let path = Path_ops.collapse_home (log_dir t ^ "/") in
    Pp.pr "@.%a@."
      (Pp.styled `Faint (fun ppf () ->
           Pp.pf ppf "Test output saved to %a" (Pp.styled `Cyan Pp.string) path))
      ()
  end
