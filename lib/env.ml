(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* ───── Parsing Helpers ───── *)

let parse_bool s =
  match String.lowercase_ascii (String.trim s) with
  | "1" | "true" | "yes" | "y" | "on" -> Some true
  | "0" | "false" | "no" | "n" | "off" -> Some false
  | _ -> None

let parse_int s = int_of_string_opt (String.trim s)
let parse_float s = float_of_string_opt (String.trim s)
let get_bool name = Option.bind (Sys.getenv_opt name) parse_bool
let get_int name = Option.bind (Sys.getenv_opt name) parse_int
let get_float name = Option.bind (Sys.getenv_opt name) parse_float
let get_string name = Sys.getenv_opt name
let is_set name = Option.is_some (Sys.getenv_opt name)

(* ───── Terminal Detection ───── *)

let is_tty_stdout = lazy (Unix.isatty Unix.stdout)
let is_tty_stderr = lazy (Unix.isatty Unix.stderr)
let inside_dune = lazy (is_set "INSIDE_DUNE")

(* ───── CI Detection ───── *)

type ci_mode = Disabled | Github_actions | Unknown_ci

let ci_mode =
  lazy
    (let ci = is_set "CI" in
     let github_actions = is_set "GITHUB_ACTIONS" in
     match (ci, github_actions) with
     | true, true -> Github_actions
     | true, false -> Unknown_ci
     | _ -> Disabled)

let in_ci () = Lazy.force ci_mode <> Disabled
let in_github_actions () = Lazy.force ci_mode = Github_actions

(* ───── Color Support ───── *)

(* Fallback: enable color when on a TTY or running inside dune (which
   captures output but passes INSIDE_DUNE so tools can still emit ANSI). *)
let detect_color ~is_tty =
  lazy
    (match get_string "WINDTRAP_COLOR" with
    | Some "always" -> true
    | Some "never" -> false
    | Some "auto" | Some _ | None -> Lazy.force is_tty || Lazy.force inside_dune)

let use_color = detect_color ~is_tty:is_tty_stdout
let use_color_stderr = detect_color ~is_tty:is_tty_stderr

let setup_color () =
  Pp.use_ansi_stdout := Lazy.force use_color;
  Pp.use_ansi_stderr := Lazy.force use_color_stderr

(* ───── WINDTRAP_* Environment Variables ───── *)

let stream () = get_bool "WINDTRAP_STREAM"
let format () = get_string "WINDTRAP_FORMAT"

let columns () =
  match get_int "WINDTRAP_COLUMNS" with
  | Some n when n > 0 -> Some n
  | _ -> None

let tail_errors () = get_int "WINDTRAP_TAIL_ERRORS"
let seed () = get_int "WINDTRAP_SEED"

(* ───── Snapshot Settings ───── *)

let update () = get_bool "WINDTRAP_UPDATE"
let snapshot_diff_context () = get_int "WINDTRAP_SNAPSHOT_DIFF_CONTEXT"
let snapshot_max_bytes () = get_int "WINDTRAP_SNAPSHOT_MAX_BYTES"
let snapshot_report () = get_bool "WINDTRAP_SNAPSHOT_REPORT"
let snapshot_dir () = get_string "WINDTRAP_SNAPSHOT_DIR"
let project_root () = get_string "WINDTRAP_PROJECT_ROOT"

(* ───── Test Filtering ───── *)

let filter () = get_string "WINDTRAP_FILTER"
let timeout () = get_float "WINDTRAP_TIMEOUT"

(* ───── Tag Filtering ───── *)

let split_comma s =
  String.split_on_char ',' s
  |> List.filter (fun s -> String.length (String.trim s) > 0)
  |> List.map String.trim

let tag () =
  match get_string "WINDTRAP_TAG" with Some s -> split_comma s | None -> []

let exclude_tag () =
  match get_string "WINDTRAP_EXCLUDE_TAG" with
  | Some s -> split_comma s
  | None -> []
