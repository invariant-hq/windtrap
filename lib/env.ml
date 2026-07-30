(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Adapted from windtrap 0.1's lib/env.ml. v3 drops the color globals
   (renderers make the ANSI decision explicitly) and adds the
   WINDTRAP_ALLOW_FOCUS / WINDTRAP_PRUNE / WINDTRAP_COVERAGE variables and
   the [force] update mode. *)

(* Parsing helpers *)

(* An empty value counts as unset: it lets callers clear a variable in
   environments without unsetenv, and `VAR= cmd` reads as "not set". *)
let get_raw name =
  match Sys.getenv_opt name with Some "" | None -> None | Some s -> Some s

let parse_bool s =
  match String.lowercase_ascii (String.trim s) with
  | "1" | "true" | "yes" | "y" | "on" -> Some true
  | "0" | "false" | "no" | "n" | "off" -> Some false
  | _ -> None

let get_bool name = Option.bind (get_raw name) parse_bool

let get_int name =
  Option.bind (get_raw name) (fun s -> int_of_string_opt (String.trim s))

let get_string = get_raw
let is_truthy name = Option.value ~default:false (get_bool name)

(* Set-and-not-falsy: `CI=false` must not count as CI. *)
let is_flagged name =
  match get_raw name with
  | None -> false
  | Some s -> ( match parse_bool s with Some b -> b | None -> true)

(* Platform detection *)

let inside_dune () = is_flagged "INSIDE_DUNE"
let is_tty_stdout () = Unix.isatty Unix.stdout
let is_tty_stderr () = Unix.isatty Unix.stderr

(* TERM=dumb is the near-universal "no escape sequences" convention (git,
   cargo, Emacs M-x shell); the exact spelling, like git's check. *)
let term_dumb () =
  match get_raw "TERM" with Some "dumb" -> true | Some _ | None -> false

(* CI detection *)

type ci = Not_ci | Github_actions | Other_ci

let ci () =
  if not (is_flagged "CI") then Not_ci
  else if is_flagged "GITHUB_ACTIONS" then Github_actions
  else Other_ci

let in_ci () = ci () <> Not_ci
let in_github_actions () = ci () = Github_actions

(* Color *)

type color_mode = Always | Never | Auto

let color_mode () =
  match get_string "WINDTRAP_COLOR" with
  | Some s -> (
      match String.lowercase_ascii (String.trim s) with
      | "always" -> Always
      | "never" -> Never
      | _ -> Auto)
  | None -> Auto

let resolve_color mode ~tty ~inside_dune ~term_dumb =
  match mode with
  | Always -> true
  | Never -> false
  | Auto -> (tty || inside_dune) && not term_dumb

let use_color_stdout () =
  resolve_color (color_mode ()) ~tty:(is_tty_stdout ())
    ~inside_dune:(inside_dune ()) ~term_dumb:(term_dumb ())

let use_color_stderr () =
  resolve_color (color_mode ()) ~tty:(is_tty_stderr ())
    ~inside_dune:(inside_dune ()) ~term_dumb:(term_dumb ())

(* Run control *)

let seed () = get_string "WINDTRAP_SEED"
let filter () = get_string "WINDTRAP_FILTER"
let exclude () = get_string "WINDTRAP_EXCLUDE"

let split_comma s =
  String.split_on_char ',' s |> List.map String.trim
  |> List.filter (fun s -> s <> "")

let comma_list name =
  match get_string name with Some s -> split_comma s | None -> []

let tags () = comma_list "WINDTRAP_TAG"
let exclude_tags () = comma_list "WINDTRAP_EXCLUDE_TAG"

(* Like [seed]: raw tokens, the CLI layer owns validation — a malformed
   winning mirror must be a loud usage error, never a silent default
   (prop/F-4). *)
let timeout () = get_string "WINDTRAP_TIMEOUT"
let slow_threshold () = get_string "WINDTRAP_SLOW_THRESHOLD"
let prop_count () = get_string "WINDTRAP_PROP_COUNT"
let shard () = get_string "WINDTRAP_SHARD"
let stream () = get_bool "WINDTRAP_STREAM"
let verbose () = get_bool "WINDTRAP_VERBOSE"
let quiet () = get_bool "WINDTRAP_QUIET"

let columns () =
  match get_int "WINDTRAP_COLUMNS" with
  | Some n when n > 0 -> Some n
  | _ -> None

let tail_errors () = get_int "WINDTRAP_TAIL_ERRORS"
let allow_focus () = is_truthy "WINDTRAP_ALLOW_FOCUS"

(* Snapshot control *)

type update = No_update | Update | Force_update

let update () =
  match get_string "WINDTRAP_UPDATE" with
  | None -> No_update
  | Some s -> (
      if String.lowercase_ascii (String.trim s) = "force" then Force_update
      else match parse_bool s with Some true -> Update | _ -> No_update)

let prune () = is_truthy "WINDTRAP_PRUNE"
let project_root () = get_string "WINDTRAP_PROJECT_ROOT"

(* Coverage *)

let coverage () = get_string "WINDTRAP_COVERAGE"
