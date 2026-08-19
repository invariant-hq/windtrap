(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Adapted from windtrap 0.1's lib/env.ml. v3 drops the color globals
   (renderers make the ANSI decision explicitly) and adds the
   WINDTRAP_PRUNE / WINDTRAP_COVERAGE variables and the [force] update
   mode. *)

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

(* Comma-separated lists (WINDTRAP_TAG and its kind): trimmed items, empties
   dropped, so `a, b ,,c ` reads as the three labels it obviously means. *)
let split_comma s =
  String.split_on_char ',' s |> List.map String.trim
  |> List.filter (fun s -> s <> "")

(* Set-and-not-falsy: `CI=false` must not count as CI. *)
let is_flagged name =
  match get_raw name with
  | None -> false
  | Some s -> ( match parse_bool s with Some b -> b | None -> true)

(* Writing

   [Unix.putenv] is only the binding half: the stdlib has no unsetenv, and
   binding to "" is not unbinding — [Sys.getenv_opt] answers [Some ""],
   which is a different fact to every program that asks. The unbinding half
   is C's (see env_stubs.c), so [Run.setenv] can put back a variable the
   test found unset. The name check sits here rather than in each
   platform's error path, so one bad name reads the same everywhere. *)

external unsetenv : string -> unit = "ocaml_windtrap_unsetenv"

let set name value =
  if name = "" || String.contains name '=' then
    invalid_arg
      (* No non-ASCII here: [Printexc.to_string] renders the payload with
         [%S], so anything outside ASCII reaches reports as escaped bytes. *)
      (Printf.sprintf
         "windtrap: %S is not a usable environment variable name: a name is \
          non-empty and contains no '='"
         name);
  match value with Some v -> Unix.putenv name v | None -> unsetenv name

(* Platform detection *)

let inside_dune () = is_flagged "INSIDE_DUNE"
let is_tty_stdout () = Unix.isatty Unix.stdout

(* TERM=dumb is the near-universal "no escape sequences" convention (git,
   cargo, Emacs M-x shell); the exact spelling, like git's check. *)
let term_dumb () =
  match get_raw "TERM" with Some "dumb" -> true | Some _ | None -> false

(* CI detection

   The three-way value is not exported: no caller wants to tell [Other_ci]
   from [Github_actions] except through the two predicates below, and
   deriving both from one classification is what keeps a GITHUB_ACTIONS
   without a CI from counting as GitHub Actions. *)

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

(* The only colour decision this module makes: a caller names the sink by
   passing its terminal status, so nothing here sniffs a renderer's sink
   on its behalf. NO_COLOR is the one thing read here rather than passed:
   it is the de-facto standard for "this environment wants no escape
   codes at all" (any non-empty value, whatever it says), it is a fact
   about the environment and not about one sink, and reading it here is
   what makes every caller honour it — the two reporting commands
   included, neither of which has a --color flag. An explicit [Always]
   still wins: the user asked. *)
let resolve_color mode ~tty ~inside_dune ~term_dumb =
  match mode with
  | Always -> true
  | Never -> false
  | Auto ->
      (tty || inside_dune) && (not term_dumb) && get_raw "NO_COLOR" = None

(* Settings with no command-line flag

   The flag mirrors are not here: each is declared beside its flag in
   [Cli]'s table and read through the generic readers above, which is what
   keeps a mirror from parsing differently from the flag it mirrors — and
   so is the flagless setting the resolution itself consumes
   (WINDTRAP_TAIL_ERRORS). What remains are the variables read below the
   CLI layer or beside it. *)

let project_root () = get_string "WINDTRAP_PROJECT_ROOT"

(* Which files the run's own coverage number speaks about. The registry is
   process-global — every instrumented library linked into the executable
   is in it, including ones the reader did not write — so a percentage over
   all of it can be a number about somebody else's code. *)
let coverage_only () =
  match get_string "WINDTRAP_COVERAGE_ONLY" with
  | None -> []
  | Some s -> split_comma s

(* Which mutants a run considers at all. Unlike coverage's, this is not a
   reporting filter: the loop forks once per mutant in the catalogue, so
   narrowing the catalogue narrows the WORK. *)
let mutate_only () =
  match get_string "WINDTRAP_MUTATE_ONLY" with
  | None -> []
  | Some s -> split_comma s

(* Snapshot update modes

   The one mirror still parsed here, because its vocabulary is wider than
   its flag's: [-u] cannot spell [force]. *)

type update = No_update | Update | Force_update

let update () =
  match get_string "WINDTRAP_UPDATE" with
  | None -> No_update
  | Some s -> (
      if String.lowercase_ascii (String.trim s) = "force" then Force_update
      else match parse_bool s with Some true -> Update | _ -> No_update)
