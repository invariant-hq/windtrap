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

(* Recorded executions *)

type t = { result : (Run.outcome, Run.startup_error) result; log_dir : string }

let execute ?(env = []) ?(config = Fun.id) ?on_event ?allowlist
    ?(suite = "suite") tests =
  let log_dir = Scratch.dir "windtrap-recorded-" in
  with_environment env @@ fun () ->
  let base = { (Run.default_config ()) with Run.seed; log_dir } in
  {
    result = Run.execute ?on_event ?allowlist (config base) ~suite tests;
    log_dir;
  }

let result t = t.result
let log_dir t = t.log_dir

let outcome t =
  let pp ppf e = Format.pp_print_string ppf (Run.startup_message e) in
  Windtrap.require_ok ~msg:"the recorded run started" ~pp t.result

(* Projections *)

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
