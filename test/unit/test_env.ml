(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Env = Windtrap.Private.Env

(* Unix.putenv cannot remove a variable, but Env treats the empty string
   as unset, which is what makes these tests deterministic regardless of
   the ambient environment (e.g. INSIDE_DUNE under dune runtest). *)
let set = Unix.putenv
let clear name = Unix.putenv name ""

let tests =
  [
    test "empty value reads as unset" (fun () ->
        clear "WINDTRAP_FILTER";
        equal (option string) None (Env.filter ());
        set "WINDTRAP_FILTER" "users";
        equal ~msg:"set value is returned" (option string) (Some "users")
          (Env.filter ());
        clear "WINDTRAP_FILTER";
        clear "WINDTRAP_EXCLUDE";
        equal ~msg:"exclude unset" (option string) None (Env.exclude ());
        set "WINDTRAP_EXCLUDE" "slow suite";
        equal ~msg:"exclude set" (option string) (Some "slow suite")
          (Env.exclude ());
        clear "WINDTRAP_EXCLUDE");
    cases "truthy bool spellings" [ "1"; "true"; "TRUE"; "yes"; "Y"; "on" ]
      (fun v ->
        set "WINDTRAP_STREAM" v;
        equal (option bool) (Some true) (Env.stream ());
        clear "WINDTRAP_STREAM");
    cases "falsy bool spellings" [ "0"; "false"; "no"; "N"; "off"; "OFF" ]
      (fun v ->
        set "WINDTRAP_STREAM" v;
        equal (option bool) (Some false) (Env.stream ());
        clear "WINDTRAP_STREAM");
    test "bool parsing edges" (fun () ->
        set "WINDTRAP_STREAM" "bogus";
        equal ~msg:"unparseable bool reads as unset" (option bool) None
          (Env.stream ());
        set "WINDTRAP_STREAM" " true ";
        equal ~msg:"bool value is trimmed" (option bool) (Some true)
          (Env.stream ());
        clear "WINDTRAP_STREAM";
        equal ~msg:"stream unset" (option bool) None (Env.stream ()));
    test "numeric readers" (fun () ->
        set "WINDTRAP_COLUMNS" "100";
        equal ~msg:"columns parses" (option int) (Some 100) (Env.columns ());
        set "WINDTRAP_COLUMNS" "0";
        equal ~msg:"non-positive columns ignored" (option int) None
          (Env.columns ());
        set "WINDTRAP_COLUMNS" "-3";
        equal ~msg:"negative columns ignored" (option int) None (Env.columns ());
        set "WINDTRAP_COLUMNS" "wide";
        equal ~msg:"unparseable columns ignored" (option int) None
          (Env.columns ());
        clear "WINDTRAP_COLUMNS";
        set "WINDTRAP_TAIL_ERRORS" "25";
        equal ~msg:"tail_errors parses" (option int) (Some 25)
          (Env.tail_errors ());
        clear "WINDTRAP_TAIL_ERRORS");
    test "numeric mirrors are passed through unparsed, like the seed" (fun () ->
        (* The CLI layer owns validation (prop/F-4): a malformed winning
           token must reach it verbatim so it can error naming the
           variable, never vanish into a silent default. *)
        set "WINDTRAP_PROP_COUNT" "500";
        equal ~msg:"prop_count raw" (option string) (Some "500")
          (Env.prop_count ());
        set "WINDTRAP_PROP_COUNT" "1O0";
        equal ~msg:"malformed prop_count is passed through" (option string)
          (Some "1O0") (Env.prop_count ());
        clear "WINDTRAP_PROP_COUNT";
        equal ~msg:"prop_count unset" (option string) None (Env.prop_count ());
        set "WINDTRAP_TIMEOUT" "2.5";
        equal ~msg:"timeout raw" (option string) (Some "2.5") (Env.timeout ());
        set "WINDTRAP_TIMEOUT" "soon";
        equal ~msg:"malformed timeout is passed through" (option string)
          (Some "soon") (Env.timeout ());
        clear "WINDTRAP_TIMEOUT";
        equal ~msg:"timeout unset" (option string) None (Env.timeout ());
        set "WINDTRAP_SLOW_THRESHOLD" "0.5";
        equal ~msg:"slow_threshold raw" (option string) (Some "0.5")
          (Env.slow_threshold ());
        set "WINDTRAP_SLOW_THRESHOLD" "fast";
        equal ~msg:"malformed slow_threshold is passed through" (option string)
          (Some "fast") (Env.slow_threshold ());
        clear "WINDTRAP_SLOW_THRESHOLD";
        equal ~msg:"slow_threshold unset" (option string) None
          (Env.slow_threshold ());
        set "WINDTRAP_SHARD" "2/4";
        equal ~msg:"shard raw" (option string) (Some "2/4") (Env.shard ());
        set "WINDTRAP_SHARD" "5/2";
        equal ~msg:"malformed shard is passed through" (option string)
          (Some "5/2") (Env.shard ());
        clear "WINDTRAP_SHARD";
        equal ~msg:"shard unset" (option string) None (Env.shard ()));
    test "output-level mirrors parse as booleans" (fun () ->
        clear "WINDTRAP_VERBOSE";
        clear "WINDTRAP_QUIET";
        equal ~msg:"verbose unset" (option bool) None (Env.verbose ());
        equal ~msg:"quiet unset" (option bool) None (Env.quiet ());
        set "WINDTRAP_VERBOSE" "1";
        equal ~msg:"verbose truthy" (option bool) (Some true) (Env.verbose ());
        set "WINDTRAP_QUIET" "on";
        equal ~msg:"quiet truthy" (option bool) (Some true) (Env.quiet ());
        set "WINDTRAP_QUIET" "off";
        equal ~msg:"quiet falsy" (option bool) (Some false) (Env.quiet ());
        set "WINDTRAP_VERBOSE" "maybe";
        equal ~msg:"unparseable verbose reads as unset" (option bool) None
          (Env.verbose ());
        clear "WINDTRAP_VERBOSE";
        clear "WINDTRAP_QUIET");
    test "seed is passed through unparsed" (fun () ->
        set "WINDTRAP_SEED" "s1:7be1d2c904aa31f5";
        equal (option string) (Some "s1:7be1d2c904aa31f5") (Env.seed ());
        clear "WINDTRAP_SEED";
        equal ~msg:"seed unset" (option string) None (Env.seed ()));
    test "tag lists split on commas, trim, and drop empties" (fun () ->
        set "WINDTRAP_TAG" "a, b ,,c ";
        equal ~msg:"tags split and trimmed" (list string) [ "a"; "b"; "c" ]
          (Env.tags ());
        clear "WINDTRAP_TAG";
        equal ~msg:"tags default to empty" (list string) [] (Env.tags ());
        set "WINDTRAP_EXCLUDE_TAG" "slow";
        equal ~msg:"exclude_tags split" (list string) [ "slow" ]
          (Env.exclude_tags ());
        clear "WINDTRAP_EXCLUDE_TAG");
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
    test "flag variables" (fun () ->
        clear "WINDTRAP_PRUNE";
        is_false ~msg:"prune defaults to false" (Env.prune ());
        set "WINDTRAP_PRUNE" "1";
        is_true ~msg:"prune 1 is true" (Env.prune ());
        clear "WINDTRAP_PRUNE";
        clear "WINDTRAP_ALLOW_FOCUS";
        is_false ~msg:"allow_focus defaults to false" (Env.allow_focus ());
        set "WINDTRAP_ALLOW_FOCUS" "1";
        is_true ~msg:"allow_focus 1 is true" (Env.allow_focus ());
        clear "WINDTRAP_ALLOW_FOCUS";
        clear "WINDTRAP_COVERAGE";
        equal ~msg:"coverage unset" (option string) None (Env.coverage ());
        set "WINDTRAP_COVERAGE" "report";
        equal ~msg:"coverage raw value" (option string) (Some "report")
          (Env.coverage ());
        clear "WINDTRAP_COVERAGE";
        set "WINDTRAP_PROJECT_ROOT" "/tmp/proj";
        equal ~msg:"project_root passed through" (option string)
          (Some "/tmp/proj") (Env.project_root ());
        clear "WINDTRAP_PROJECT_ROOT");
    test "CI detection: CI must be set and not falsy" (fun () ->
        clear "CI";
        clear "GITHUB_ACTIONS";
        is_true ~msg:"no CI" (Env.ci () = Env.Not_ci);
        is_false ~msg:"in_ci false" (Env.in_ci ());
        set "CI" "true";
        is_true ~msg:"CI alone is Other_ci" (Env.ci () = Env.Other_ci);
        is_true ~msg:"in_ci true" (Env.in_ci ());
        is_false ~msg:"not github actions" (Env.in_github_actions ());
        set "GITHUB_ACTIONS" "true";
        is_true ~msg:"CI plus GITHUB_ACTIONS" (Env.ci () = Env.Github_actions);
        is_true ~msg:"in_github_actions true" (Env.in_github_actions ());
        set "CI" "false";
        is_true ~msg:"CI=false does not count as CI" (Env.ci () = Env.Not_ci);
        set "CI" "woodpecker";
        is_true ~msg:"non-boolean CI value counts as set"
          (Env.ci () = Env.Github_actions);
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
        (* End-to-end for the modes that do not depend on the actual tty. *)
        set "WINDTRAP_COLOR" "never";
        is_false ~msg:"use_color_stdout honors never" (Env.use_color_stdout ());
        is_false ~msg:"use_color_stderr honors never" (Env.use_color_stderr ());
        set "WINDTRAP_COLOR" "always";
        is_true ~msg:"use_color_stdout honors always" (Env.use_color_stdout ());
        is_true ~msg:"use_color_stderr honors always" (Env.use_color_stderr ());
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
