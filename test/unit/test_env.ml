(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Env = Windtrap.Private.Env

(* The readers treat the empty string as unset, which is what makes these
   tests deterministic regardless of the ambient environment (e.g.
   INSIDE_DUNE under dune runtest) — so they clear with putenv rather than
   through [Env.set], whose real unbinding is proven on its own below. *)
let set = Unix.putenv
let clear name = Unix.putenv name ""

(* The readers are generic over the variable name — a mirror is named in
   [Cli]'s flag table, not here — so each test names a real variable and
   exercises the reader its mirror uses. *)
let string_of = Env.get_string
let bool_of = Env.get_bool

let tests =
  [
    test "set binds, and unbinds for real" (fun () ->
        let var = "WINDTRAP_TEST_ENV_SET" in
        Env.set var (Some "bound");
        equal ~msg:"the binding reaches the stdlib, not just Env's readers"
          (option string) (Some "bound") (Sys.getenv_opt var);
        Env.set var None;
        equal ~msg:"unbinding removes the variable" (option string) None
          (Sys.getenv_opt var);
        (* The whole reason the unbinding half is a C stub: the spelling
           [Unix] can manage leaves the variable set to the empty string,
           which is a different fact to every program that asks. *)
        Unix.putenv var "";
        equal ~msg:"an empty binding is not an unbinding" (option string)
          (Some "") (Sys.getenv_opt var);
        Env.set var None);
    test "set refuses the names POSIX refuses" (fun () ->
        let refused = Exn.invalid_arg ~substring:"environment variable name" in
        raises_match ~msg:"a name carrying '='" refused (fun () ->
            Env.set "WINDTRAP=BAD" (Some "x"));
        raises_match ~msg:"the empty name" refused (fun () -> Env.set "" None));
    test "empty value reads as unset" (fun () ->
        clear "WINDTRAP_FILTER";
        equal (option string) None (string_of "WINDTRAP_FILTER");
        set "WINDTRAP_FILTER" "users";
        equal ~msg:"set value is returned" (option string) (Some "users")
          (string_of "WINDTRAP_FILTER");
        clear "WINDTRAP_FILTER";
        clear "WINDTRAP_EXCLUDE";
        equal ~msg:"exclude unset" (option string) None
          (string_of "WINDTRAP_EXCLUDE");
        set "WINDTRAP_EXCLUDE" "slow suite";
        equal ~msg:"exclude set" (option string) (Some "slow suite")
          (string_of "WINDTRAP_EXCLUDE");
        clear "WINDTRAP_EXCLUDE");
    cases ~name:Fun.id "truthy bool spellings"
      [ "1"; "true"; "TRUE"; "yes"; "Y"; "on" ]
      (fun v ->
        set "WINDTRAP_STREAM" v;
        equal (option bool) (Some true) (bool_of "WINDTRAP_STREAM");
        clear "WINDTRAP_STREAM");
    cases ~name:Fun.id "falsy bool spellings"
      [ "0"; "false"; "no"; "N"; "off"; "OFF" ]
      (fun v ->
        set "WINDTRAP_STREAM" v;
        equal (option bool) (Some false) (bool_of "WINDTRAP_STREAM");
        clear "WINDTRAP_STREAM");
    test "bool parsing edges" (fun () ->
        set "WINDTRAP_STREAM" "bogus";
        equal ~msg:"unparseable bool reads as unset" (option bool) None
          (bool_of "WINDTRAP_STREAM");
        set "WINDTRAP_STREAM" " true ";
        equal ~msg:"bool value is trimmed" (option bool) (Some true)
          (bool_of "WINDTRAP_STREAM");
        clear "WINDTRAP_STREAM";
        equal ~msg:"stream unset" (option bool) None (bool_of "WINDTRAP_STREAM"));
    test "numeric readers" (fun () ->
        (* The flagless numeric settings (WINDTRAP_COLUMNS,
           WINDTRAP_TAIL_ERRORS) are rows of Cli's table and their
           vocabularies are pinned there; this pins the generic reader
           they go through. *)
        set "WINDTRAP_COLUMNS" "100";
        equal ~msg:"get_int parses" (option int) (Some 100)
          (Env.get_int "WINDTRAP_COLUMNS");
        set "WINDTRAP_COLUMNS" " 25 ";
        equal ~msg:"the value is trimmed" (option int) (Some 25)
          (Env.get_int "WINDTRAP_COLUMNS");
        set "WINDTRAP_COLUMNS" "wide";
        equal ~msg:"an unparseable value reads as unset" (option int) None
          (Env.get_int "WINDTRAP_COLUMNS");
        clear "WINDTRAP_COLUMNS");
    test "value mirrors are passed through unparsed, like the seed" (fun () ->
        (* The CLI layer owns validation (prop/F-4): a malformed winning
           token must reach it verbatim so it can error naming the
           variable, never vanish into a silent default. *)
        set "WINDTRAP_PROP_COUNT" "500";
        equal ~msg:"prop_count raw" (option string) (Some "500")
          (string_of "WINDTRAP_PROP_COUNT");
        set "WINDTRAP_PROP_COUNT" "1O0";
        equal ~msg:"malformed prop_count is passed through" (option string)
          (Some "1O0")
          (string_of "WINDTRAP_PROP_COUNT");
        clear "WINDTRAP_PROP_COUNT";
        equal ~msg:"prop_count unset" (option string) None
          (string_of "WINDTRAP_PROP_COUNT");
        set "WINDTRAP_TIMEOUT" "2.5";
        equal ~msg:"timeout raw" (option string) (Some "2.5")
          (string_of "WINDTRAP_TIMEOUT");
        set "WINDTRAP_TIMEOUT" "soon";
        equal ~msg:"malformed timeout is passed through" (option string)
          (Some "soon")
          (string_of "WINDTRAP_TIMEOUT");
        clear "WINDTRAP_TIMEOUT";
        equal ~msg:"timeout unset" (option string) None
          (string_of "WINDTRAP_TIMEOUT");
        set "WINDTRAP_SEED" "s1:7be1d2c904aa31f5";
        equal ~msg:"seed raw" (option string) (Some "s1:7be1d2c904aa31f5")
          (string_of "WINDTRAP_SEED");
        clear "WINDTRAP_SEED";
        equal ~msg:"seed unset" (option string) None (string_of "WINDTRAP_SEED"));
    test "comma lists split, trim, and drop empties" (fun () ->
        equal ~msg:"tags split and trimmed" (list string) [ "a"; "b"; "c" ]
          (Env.split_comma "a, b ,,c ");
        equal ~msg:"a lone label is a one-item list" (list string) [ "slow" ]
          (Env.split_comma "slow");
        equal ~msg:"separators alone are no labels" (list string) []
          (Env.split_comma " , "));
    test "update mode: 1/truthy, force, everything else off" (fun () ->
        clear "WINDTRAP_UPDATE";
        is_true ~msg:"update unset is No_update" (Env.update () = Env.No_update);
        set "WINDTRAP_UPDATE" "1";
        is_true ~msg:"update 1 is Update" (Env.update () = Env.Update);
        set "WINDTRAP_UPDATE" "true";
        is_true ~msg:"update true is Update" (Env.update () = Env.Update);
        set "WINDTRAP_UPDATE" "force";
        is_true ~msg:"update force is Force_update"
          (Env.update () = Env.Force_update);
        set "WINDTRAP_UPDATE" "FORCE";
        is_true ~msg:"update FORCE is Force_update"
          (Env.update () = Env.Force_update);
        set "WINDTRAP_UPDATE" "0";
        is_true ~msg:"update 0 is No_update" (Env.update () = Env.No_update);
        set "WINDTRAP_UPDATE" "sometimes";
        is_true ~msg:"unknown update value is No_update"
          (Env.update () = Env.No_update);
        clear "WINDTRAP_UPDATE");
    test "settings with no flag" (fun () ->
        set "WINDTRAP_PROJECT_ROOT" "/tmp/proj";
        equal ~msg:"project_root passed through" (option string)
          (Some "/tmp/proj") (Env.project_root ());
        clear "WINDTRAP_PROJECT_ROOT");
    test "CI detection: CI must be set and not falsy" (fun () ->
        clear "CI";
        clear "GITHUB_ACTIONS";
        is_false ~msg:"no CI" (Env.in_ci ());
        is_false ~msg:"no GitHub Actions" (Env.in_github_actions ());
        set "CI" "true";
        is_true ~msg:"in_ci true" (Env.in_ci ());
        is_false ~msg:"CI alone is not GitHub Actions"
          (Env.in_github_actions ());
        set "GITHUB_ACTIONS" "true";
        is_true ~msg:"CI plus GITHUB_ACTIONS" (Env.in_github_actions ());
        set "CI" "false";
        is_false ~msg:"CI=false does not count as CI" (Env.in_ci ());
        is_false ~msg:"GITHUB_ACTIONS without CI is not GitHub Actions"
          (Env.in_github_actions ());
        set "CI" "woodpecker";
        is_true ~msg:"non-boolean CI value counts as set" (Env.in_ci ());
        is_true ~msg:"a non-boolean CI still resolves GitHub Actions"
          (Env.in_github_actions ());
        clear "CI";
        clear "GITHUB_ACTIONS");
    test "INSIDE_DUNE" (fun () ->
        let saved = try Sys.getenv "INSIDE_DUNE" with Not_found -> "" in
        clear "INSIDE_DUNE";
        is_false ~msg:"inside_dune false when cleared" (Env.inside_dune ());
        set "INSIDE_DUNE" "1";
        is_true ~msg:"inside_dune true when set" (Env.inside_dune ());
        set "INSIDE_DUNE" "false";
        is_false ~msg:"INSIDE_DUNE=false does not count" (Env.inside_dune ());
        set "INSIDE_DUNE" saved);
    test "color mode parsing and the pure resolution rule" (fun () ->
        clear "WINDTRAP_COLOR";
        is_true ~msg:"color defaults to Auto" (Env.color_mode () = Env.Auto);
        set "WINDTRAP_COLOR" "always";
        is_true ~msg:"color always" (Env.color_mode () = Env.Always);
        set "WINDTRAP_COLOR" "NEVER";
        is_true ~msg:"color parsing is case-insensitive"
          (Env.color_mode () = Env.Never);
        set "WINDTRAP_COLOR" "auto";
        is_true ~msg:"color auto" (Env.color_mode () = Env.Auto);
        set "WINDTRAP_COLOR" "sometimes";
        is_true ~msg:"unknown color mode is Auto" (Env.color_mode () = Env.Auto);
        is_true ~msg:"always ignores tty"
          (Env.resolve_color Env.Always ~tty:false ~inside_dune:false
             ~term_dumb:false);
        is_false ~msg:"never ignores tty"
          (Env.resolve_color Env.Never ~tty:true ~inside_dune:true
             ~term_dumb:false);
        is_true ~msg:"auto on tty"
          (Env.resolve_color Env.Auto ~tty:true ~inside_dune:false
             ~term_dumb:false);
        is_true ~msg:"auto under dune"
          (Env.resolve_color Env.Auto ~tty:false ~inside_dune:true
             ~term_dumb:false);
        is_false ~msg:"auto plain pipe"
          (Env.resolve_color Env.Auto ~tty:false ~inside_dune:false
             ~term_dumb:false);
        (* TERM=dumb disables ANSI in Auto mode only (render/F-4): a dumb
           terminal renders no escape sequences, but an explicit request
           still wins. *)
        is_false ~msg:"auto on a dumb tty"
          (Env.resolve_color Env.Auto ~tty:true ~inside_dune:false
             ~term_dumb:true);
        is_false ~msg:"auto under dune with a dumb terminal"
          (Env.resolve_color Env.Auto ~tty:false ~inside_dune:true
             ~term_dumb:true);
        is_true ~msg:"always beats a dumb terminal"
          (Env.resolve_color Env.Always ~tty:true ~inside_dune:false
             ~term_dumb:true);
        (* Composing the two — a mode read from the environment applied to
           a named sink — is the caller's job, not this module's:
           [Driver.renderer] does it for the runner and [coverage_cmd] for
           the coverage command, and both are pinned end to end by child
           runs that pass --color and compare bytes. *)
        clear "WINDTRAP_COLOR");
    test "TERM=dumb detection" (fun () ->
        let saved = try Sys.getenv "TERM" with Not_found -> "" in
        set "TERM" "dumb";
        is_true ~msg:"TERM=dumb detected" (Env.term_dumb ());
        set "TERM" "xterm-256color";
        is_false ~msg:"a capable TERM is not dumb" (Env.term_dumb ());
        set "TERM" "";
        is_false ~msg:"unset TERM is not dumb" (Env.term_dumb ());
        set "TERM" saved);
  ]
