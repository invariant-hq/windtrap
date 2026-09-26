(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Failure = Windtrap.Private.Failure
module Os = Windtrap.Private.Os
module Run = Windtrap.Private.Run
module Test_tree = Windtrap.Private.Test_tree
module Scratch = Windtrap_test_support.Scratch

let seed = 0x5eedL

(* The stated environment *)

let is_windtrap binding = String.starts_with ~prefix:"WINDTRAP_" binding

let unset () =
  let windtrap =
    List.filter_map
      (fun binding ->
        match String.index_opt binding '=' with
        | Some i when is_windtrap binding -> Some (String.sub binding 0 i)
        | Some _ | None -> None)
      (Array.to_list (Unix.environment ()))
  in
  [ "CI"; "GITHUB_ACTIONS"; "INSIDE_DUNE"; "NO_COLOR"; "TERM" ] @ windtrap

let with_environment env fn =
  let names = List.sort_uniq String.compare (unset () @ List.map fst env) in
  let saved = List.map (fun name -> (name, Sys.getenv_opt name)) names in
  let restore () = List.iter (fun (name, v) -> Os.setenv name v) saved in
  List.iter (fun name -> Os.setenv name None) names;
  List.iter (fun (name, v) -> Os.setenv name (Some v)) env;
  Fun.protect ~finally:restore fn

(* Recorded calls *)

type 'a t = {
  returned : ('a, exn) result;
  out : string;
  err : string;
  log_dir : string;
}

(* The runner's capture moves the same two descriptors around every test and
   restores what it saved, which is these files. *)
let with_output_in dir fn =
  let flush_all () =
    Format.pp_print_flush Format.std_formatter ();
    Format.pp_print_flush Format.err_formatter ();
    flush stdout;
    flush stderr
  in
  let redirect name fd =
    let path = Filename.concat dir name in
    let file =
      Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600
    in
    let saved = Unix.dup ~cloexec:true fd in
    Unix.dup2 file fd;
    Unix.close file;
    (path, saved)
  in
  flush_all ();
  let out, saved_out = redirect "stdout" Unix.stdout in
  let err, saved_err = redirect "stderr" Unix.stderr in
  let restore () =
    flush_all ();
    Unix.dup2 saved_out Unix.stdout;
    Unix.dup2 saved_err Unix.stderr;
    Unix.close saved_out;
    Unix.close saved_err
  in
  let v = Fun.protect ~finally:restore fn in
  let read path = In_channel.with_open_bin path In_channel.input_all in
  (v, read out, read err)

(* The log directory is left for the call to make, since [list_selection]
   promises to make none. *)
let record ~env call =
  let root = Scratch.dir "windtrap-recorded-" in
  let log_dir = Filename.concat root "logs" in
  let returned, out, err =
    with_environment (env log_dir) @@ fun () ->
    with_output_in root @@ fun () ->
    match call log_dir with v -> Ok v | exception e -> Error e
  in
  { returned; out; err; log_dir }

let returned t =
  match t.returned with
  | Ok v -> v
  | Error e ->
      Windtrap.failf "the recorded call raised %s" (Failure.exn_to_string e)

let escaped t = match t.returned with Ok _ -> None | Error e -> Some e
let out t = t.out
let err t = t.err
let log_dir t = t.log_dir

(* Recorded executions *)

type execution = (Run.outcome, Run.startup_error) result t

let configured config log_dir =
  config { (Run.default_config ()) with Run.seed; log_dir }

let execute ?(env = []) ?(config = Fun.id) ?on_event ?allowlist
    ?(suite = "suite") tests =
  record ~env:(Fun.const env) @@ fun log_dir ->
  Run.execute ?on_event ?allowlist (configured config log_dir) ~suite tests

let list_selection ?(env = []) ?(config = Fun.id) ?(suite = "suite") tests =
  record ~env:(Fun.const env) @@ fun log_dir ->
  Run.list_selection (configured config log_dir) ~suite tests

(* Projections *)

let outcome (t : execution) =
  let pp ppf e = Format.pp_print_string ppf (Run.startup_message e) in
  Windtrap.require_ok ~msg:"the recorded run started" ~pp (returned t)

let executed_row t path =
  let at (r : Run.result) = List.equal String.equal r.path path in
  match List.find_opt at (Run.results (outcome t).run) with
  | Some r -> r
  | None -> Windtrap.failf "no test ran at %s" (Test_tree.path_to_string path)

let phase = function
  | Failure.Setup -> "setup"
  | Failure.Body -> "body"
  | Failure.Teardown -> "teardown"
  | Failure.Release -> "release"

let row t path =
  let r = executed_row t path in
  match r.outcome with
  | Failure.Pass -> "pass"
  | Failure.Skip None -> "skip"
  | Failure.Skip (Some reason) -> "skip " ^ reason
  | Failure.Fail failures ->
      let phases = List.map (fun (f : Failure.t) -> phase f.phase) failures in
      (if r.counted then "fail " else "xfail ") ^ String.concat ", " phases

let failures t path =
  match (executed_row t path).outcome with
  | Failure.Fail failures -> failures
  | Failure.Pass | Failure.Skip _ -> []

let executed t =
  List.map
    (fun (r : Run.result) -> Test_tree.path_to_string r.path)
    (Run.results (outcome t).run)

let exit_code t = (outcome t).exit_code
