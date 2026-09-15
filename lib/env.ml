(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Adapted from windtrap 0.1's lib/env.ml. v3 drops the color globals
   (renderers make the ANSI decision explicitly). *)

(* Reading *)

(* An empty value counts as unset: it lets callers clear a variable in
   environments without unsetenv, and `VAR= cmd` reads as "not set". *)
let get_string name =
  match Sys.getenv_opt name with Some "" | None -> None | Some s -> Some s

(* The one boolean vocabulary: every valueless flag's mirror and every
   switch with no flag reads it, and a mirror refuses anything outside it
   — a typo must not read as "off". *)
let bool_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "1" | "true" | "yes" | "y" | "on" -> Some true
  | "0" | "false" | "no" | "n" | "off" -> Some false
  | _ -> None

let bool_expected = "a boolean: 1/0, true/false, yes/no or on/off"

(* Comma-separated lists (WINDTRAP_TAG and its kind): trimmed items, empties
   dropped, so `a, b ,,c ` reads as the three labels it obviously means. *)
let split_comma s =
  String.split_on_char ',' s |> List.map String.trim
  |> List.filter (fun s -> s <> "")

(* Set-and-not-falsy: `CI=false` must not count as CI. *)
let is_flagged name =
  match get_string name with
  | None -> false
  | Some s -> ( match bool_of_string s with Some b -> b | None -> true)

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
  match get_string "TERM" with Some "dumb" -> true | Some _ | None -> false

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

(* The one colour vocabulary, [--color]'s: its mirror and the flagless
   commands' read of WINDTRAP_COLOR go through the flag's parser, so an
   unknown word is refused there rather than read as [Auto] here. *)
let color_mode_of_string s =
  match String.lowercase_ascii s with
  | "always" -> Some Always
  | "never" -> Some Never
  | "auto" -> Some Auto
  | _ -> None

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
      (tty || inside_dune) && (not term_dumb) && get_string "NO_COLOR" = None

(* Settings with no command-line flag

   The flag mirrors are not here: each is declared beside its flag in
   [Cli]'s table and read through [get_string] and the flag's own parser,
   which is what keeps a mirror from parsing differently from the flag it
   mirrors. What remains are the variables read below the CLI layer or
   beside it. *)

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
   reporting filter: the loop forks once per mutant it considers, so
   narrowing the scope narrows the WORK. It applies to the population the
   loop forks over, not to registration: every instrumented file still
   registers, and an armed identifier arms whatever the executable holds. *)
let mutate_only () =
  match get_string "WINDTRAP_MUTATE_ONLY" with
  | None -> []
  | Some s -> split_comma s
