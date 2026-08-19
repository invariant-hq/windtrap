(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Cli: the flag table (every flag, both spellings, value
   validation), positional-filter handling, typed parse errors, the
   generated help/usage text, and resolution precedence (programmatic > CLI
   > env > default, additive tags, the WINDTRAP_SEED and WINDTRAP_SHARD
   error paths, env-only settings). Parsing and resolution are pure over
   argv and env, so each test clears the windtrap variables it touches. *)

open Windtrap
open Windtrap.Private

(* Each [let () = reg name @@ fun () -> ...] block below registers one
   windtrap test; [tests] collects them in declaration order. *)
let registered = ref []
let reg name body = registered := Windtrap.test name body :: !registered
let check name cond = is_true ~msg:name cond
let check_string name ~expected ~actual = equal ~msg:name string expected actual
let contains needle haystack = Text.contains_substring ~pattern:needle haystack

(* No toplevel clear: module initialization must not clear the hosting
   runner's own environment. Each resolution test clears what it reads.
   The variable inventory is the harness's ([Harness.windtrap_vars]) —
   one list, one owner, so a mirror added there is cleared here by
   construction. INSIDE_DUNE and WINDTRAP_PROJECT_ROOT stay untouched:
   they configure the hosting run itself, not [Cli] resolution. *)
let clear_env () =
  List.iter
    (fun var ->
      if var <> "INSIDE_DUNE" && var <> "WINDTRAP_PROJECT_ROOT" then
        Unix.putenv var "")
    Harness.windtrap_vars

let parse args = Cli.parse (Array.of_list ("windtrap-test" :: args))

let expect_ok name args f =
  match parse args with
  | Ok parsed -> f parsed
  | Error error -> fail (name ^ ": parse error: " ^ Cli.error_message error)

let expect_error name args pred =
  match parse args with
  | Ok _ -> check (name ^ " (should not parse)") false
  | Error error -> check name (pred error)

let () =
  reg "no arguments parse to the empty record" @@ fun () ->
  expect_ok "no arguments parse to the empty record" [] (fun p ->
      check "empty record" (p = Cli.empty));
  expect_ok "argv with only a program name is empty" [] (fun p ->
      check "still empty" (p = Cli.empty))

let () =
  reg "every flag in one vector" @@ fun () ->
  expect_ok "every flag in one vector"
    [
      "-f";
      "pat";
      "-e";
      "ex";
      "--tag";
      "a";
      "--tag";
      "b";
      "--exclude-tag";
      "c";
      "--failed";
      "-l";
      "--bail";
      "3";
      "-s";
      "-u";
      "--seed";
      "s1:00000000000000ff";
      "--timeout";
      "2.5";
      "--prop-count";
      "50";
      "--quiet";
      "--junit";
      "out.xml";
      "--color";
      "never";
      "-o";
      "logs";
    ] (fun p ->
      check "filter" (p.Cli.filter = Some "pat");
      check "exclude" (p.Cli.exclude = Some "ex");
      check "tags accumulate in order" (p.Cli.tags = [ "a"; "b" ]);
      check "exclude_tags" (p.Cli.exclude_tags = [ "c" ]);
      check "failed_only" (p.Cli.failed_only = Some true);
      check "list_only" (p.Cli.list_only = Some true);
      check "bail" (p.Cli.bail = Some 3);
      check "stream" (p.Cli.stream = Some true);
      check "update" (p.Cli.update = Some Env.Update);
      check "seed" (p.Cli.seed = Some 0xffL);
      check "timeout" (p.Cli.timeout = Some 2.5);
      check "prop_count" (p.Cli.prop_count = Some 50);
      check "quiet" (p.Cli.output = Some `Quiet);
      check "junit" (p.Cli.junit = Some "out.xml");
      check "color" (p.Cli.color = Some Env.Never);
      check "log_dir" (p.Cli.log_dir = Some "logs");
      check "help off" (not p.Cli.help);
      check "version off" (not p.Cli.version))

let () =
  reg "long spellings and inline values" @@ fun () ->
  expect_ok "long spellings and --flag=value"
    [ "--filter=abc"; "--exclude=xyz"; "--bail=7"; "--color=ALWAYS" ] (fun p ->
      check "--filter=" (p.Cli.filter = Some "abc");
      check "--exclude=" (p.Cli.exclude = Some "xyz");
      check "--bail=" (p.Cli.bail = Some 7);
      check "--color= is case-insensitive" (p.Cli.color = Some Env.Always));
  expect_ok "-x is --bail 1" [ "-x" ] (fun p ->
      check "-x" (p.Cli.bail = Some 1));
  expect_ok "--fail-fast is --bail 1" [ "--fail-fast" ] (fun p ->
      check "--fail-fast" (p.Cli.bail = Some 1));
  expect_ok "later occurrence of a single-valued flag wins"
    [ "-f"; "first"; "-f"; "second" ] (fun p ->
      check "last wins" (p.Cli.filter = Some "second"));
  expect_ok "repeatable flags accept the inline spelling"
    [ "--tag=a"; "--exclude-tag=b"; "--tag=c" ] (fun p ->
      check "inline tags accumulate"
        (p.Cli.tags = [ "a"; "c" ] && p.Cli.exclude_tags = [ "b" ]))

(* Parsing: the output level (-q ⊂ default ⊂ -v) *)

let () =
  reg "output level parsing" @@ fun () ->
  expect_ok "-q parses as quiet" [ "-q" ] (fun p ->
      check "-q" (p.Cli.output = Some `Quiet));
  expect_ok "--quiet parses" [ "--quiet" ] (fun p ->
      check "--quiet" (p.Cli.output = Some `Quiet));
  expect_ok "-v parses as verbose" [ "-v" ] (fun p ->
      check "-v" (p.Cli.output = Some `Verbose));
  expect_ok "--verbose parses" [ "--verbose" ] (fun p ->
      check "--verbose" (p.Cli.output = Some `Verbose));
  expect_ok "one axis: the last flag wins" [ "-q"; "-v" ] (fun p ->
      check "-q -v is verbose" (p.Cli.output = Some `Verbose));
  expect_ok "one axis: the last flag wins (reversed)" [ "-v"; "-q" ] (fun p ->
      check "-v -q is quiet" (p.Cli.output = Some `Quiet));
  expect_ok "--exclude-tag is selection only, not the output level"
    [ "--exclude-tag"; "slow" ] (fun p ->
      check "--exclude-tag"
        (p.Cli.exclude_tags = [ "slow" ] && p.Cli.output = None))

(* Parsing: positionals *)

let () =
  reg "positionals" @@ fun () ->
  expect_ok "a bare argument is the filter" [ "somepattern" ] (fun p ->
      check "positional filter" (p.Cli.filter = Some "somepattern"));
  expect_ok "arguments after -- are positionals" [ "--"; "-weird" ] (fun p ->
      check "post -- positional" (p.Cli.filter = Some "-weird"));
  expect_error "two positionals are rejected" [ "one"; "two" ] (function
    | Cli.Extra_positional { filter = "one"; extra = "two" } -> true
    | _ -> false);
  expect_error "-f plus a positional is rejected" [ "-f"; "one"; "two" ]
    (function
    | Cli.Extra_positional { filter = "one"; extra = "two" } -> true
    | _ -> false);
  expect_error "two positionals after -- are rejected" [ "--"; "a"; "b" ]
    (function
    | Cli.Extra_positional { filter = "a"; extra = "b" } -> true
    | _ -> false);
  expect_ok "a lone dash is an ordinary positional" [ "-" ] (fun p ->
      check "dash filter" (p.Cli.filter = Some "-"))

(* Parsing: help and version stop early *)

let () =
  reg "help and version stop early" @@ fun () ->
  expect_ok "-h sets help" [ "-h" ] (fun p -> check "help" p.Cli.help);
  expect_ok "-V sets version" [ "-V" ] (fun p -> check "version" p.Cli.version);
  expect_ok "--help wins over later garbage" [ "--help"; "--bogus" ] (fun p ->
      check "help despite garbage" p.Cli.help)

(* Parsing: typed errors *)

let () =
  reg "unknown flags suggest the near miss" @@ fun () ->
  let message args =
    match parse args with
    | Ok _ -> fail "expected a parse error"
    | Error e -> Cli.error_message e
  in
  let suggests typo expected =
    let m = message [ typo ] in
    check
      (Printf.sprintf "%s should suggest %s, got: %s" typo expected m)
      (contains (Printf.sprintf "did you mean '%s'?" expected) m)
  in
  suggests "--fliter" "--filter";
  suggests "--colour" "--color";
  suggests "--tags" "--tag";
  (* Transposition is one edit, not two: plain Levenshtein ties --juint
     between --junit and --quiet, and the tie goes to table order. *)
  suggests "--juint" "--junit";
  let silent typo =
    let m = message [ typo ] in
    check
      (Printf.sprintf "%s should suggest nothing, got: %s" typo m)
      (not (contains "did you mean" m))
  in
  (* Too far to be a slip. *)
  silent "--completely-different";
  (* Any two short flags are one edit apart, so any suggestion would be
     arbitrary; a confident wrong one is worse than none. *)
  silent "-Z";
  check "the bare error is still there"
    (contains "unknown option '-Z'" (message [ "-Z" ]))

let () =
  reg "typed parse errors" @@ fun () ->
  expect_error "unknown long flag" [ "--bogus" ] (function
    | Cli.Unknown_flag "--bogus" -> true
    | _ -> false);
  expect_error "unknown short flag" [ "-z" ] (function
    | Cli.Unknown_flag "-z" -> true
    | _ -> false);
  expect_error "missing value at end of line" [ "--filter" ] (function
    | Cli.Missing_value "--filter" -> true
    | _ -> false);
  expect_error "flag refuses an inline value" [ "--list=x" ] (function
    | Cli.Invalid_value { source = "--list"; _ } -> true
    | _ -> false);
  expect_error "--bail rejects zero" [ "--bail"; "0" ] (function
    | Cli.Invalid_value { source = "--bail"; value = "0"; _ } -> true
    | _ -> false);
  expect_error "--bail rejects garbage" [ "--bail"; "many" ] (function
    | Cli.Invalid_value { source = "--bail"; _ } -> true
    | _ -> false);
  expect_error "--timeout rejects a negative number" [ "--timeout"; "-1" ]
    (function
    | Cli.Invalid_value { source = "--timeout"; _ } -> true
    | _ -> false);
  expect_error "--prop-count rejects zero" [ "--prop-count"; "0" ] (function
    | Cli.Invalid_value { source = "--prop-count"; _ } -> true
    | _ -> false);
  expect_error "--slow-threshold rejects a negative number"
    [ "--slow-threshold"; "-1" ] (function
    | Cli.Invalid_value { source = "--slow-threshold"; value = "-1"; _ } -> true
    | _ -> false);
  expect_error "--slow-threshold rejects garbage" [ "--slow-threshold"; "fast" ]
    (function
    | Cli.Invalid_value { source = "--slow-threshold"; _ } -> true
    | _ -> false);
  expect_error "--seed rejects a decimal" [ "--seed"; "42" ] (function
    | Cli.Invalid_value { source = "--seed"; value = "42"; _ } -> true
    | _ -> false);
  expect_error "--color rejects unknown modes" [ "--color"; "sometimes" ]
    (function
    | Cli.Invalid_value { source = "--color"; _ } -> true
    | _ -> false)

let () =
  reg "error messages" @@ fun () ->
  let messages =
    [
      Cli.Unknown_flag "--bogus";
      Cli.Missing_value "--filter";
      Cli.Invalid_value { source = "--bail"; value = "x"; expected = "an int" };
      Cli.Extra_positional { filter = "a"; extra = "b" };
    ]
  in
  List.iter
    (fun error ->
      let message = Cli.error_message error in
      check "error messages are non-empty" (String.length message > 0))
    messages;
  check "error message names the flag"
    (contains "--bogus" (Cli.error_message (Cli.Unknown_flag "--bogus")))

(* --slow-threshold *)

let () =
  reg "--slow-threshold parsing" @@ fun () ->
  expect_ok "--slow-threshold parses a decimal" [ "--slow-threshold"; "2.5" ]
    (fun p -> check "threshold value" (p.Cli.slow_threshold = Some 2.5));
  expect_ok "--slow-threshold accepts zero (disable)"
    [ "--slow-threshold"; "0" ] (fun p ->
      check "zero threshold" (p.Cli.slow_threshold = Some 0.0));
  expect_ok "--slow-threshold=SECS parses inline" [ "--slow-threshold=0.5" ]
    (fun p -> check "inline threshold" (p.Cli.slow_threshold = Some 0.5))

(* --shard (amendment B14) *)

let () =
  reg "--shard parsing (B14)" @@ fun () ->
  expect_ok "--shard K/N parses" [ "--shard"; "2/4" ] (fun p ->
      check "shard pair" (p.Cli.shard = Some (2, 4)));
  expect_ok "--shard=K/N parses inline" [ "--shard=1/1" ] (fun p ->
      check "inline shard" (p.Cli.shard = Some (1, 1)));
  List.iter
    (fun value ->
      expect_error (Printf.sprintf "--shard rejects %S" value)
        [ "--shard"; value ] (function
        | Cli.Invalid_value { source = "--shard"; value = v; _ } -> v = value
        | _ -> false))
    [
      "0/4";
      "5/4";
      "2";
      "2/";
      "/4";
      "a/b";
      "-1/4";
      "2/0";
      (* Decimal numerals only: int_of_string's leniency must not leak into
         a frozen CI-facing spelling. *)
      "0x1/4";
      "+1/4";
      "1_0/20";
      " 1/4";
    ]

(* Help and usage *)

(* --help is the CLI's whole user-facing surface and it had no golden:
   the list below pins that each flag is MENTIONED, which a help text
   could satisfy while its columns, ordering, wording and ENVIRONMENT
   section drifted freely. The snapshot pins the bytes; the list stays,
   because it says which flags must exist and reads as the contract. *)
let () =
  reg "help text, whole" @@ fun () ->
  Windtrap.snapshot "help" (Cli.help ~prog:"/some/path/mytests.exe")

let () =
  reg "help and usage text" @@ fun () ->
  let help = Cli.help ~prog:"/some/path/mytests.exe" in
  check "help names the program" (contains "mytests.exe" help);
  List.iter
    (fun flag -> check ("help lists " ^ flag) (contains flag help))
    [
      "--filter";
      "--exclude";
      "--tag";
      "--exclude-tag";
      "--shard";
      "--failed";
      "--list";
      "--fail-fast";
      "--bail";
      "--timeout";
      "--slow-threshold";
      "--seed";
      "--prop-count";
      "--update";
      "--stream";
      "--verbose";
      "--quiet";
      "--junit";
      "--color";
      "--output";
      "--version";
      "--help";
    ];
  List.iter
    (fun var -> check ("help lists " ^ var) (contains var help))
    [
      "WINDTRAP_FILTER";
      "WINDTRAP_SEED";
      "WINDTRAP_SHARD";
      "WINDTRAP_UPDATE";
      "WINDTRAP_QUIET";
      "WINDTRAP_VERBOSE";
      "WINDTRAP_SLOW_THRESHOLD";
      "WINDTRAP_COLUMNS";
      "WINDTRAP_TAIL_ERRORS";
      "WINDTRAP_PROJECT_ROOT";
      "WINDTRAP_COVERAGE";
    ];
  check_string "usage is one line with the basename"
    ~expected:"usage: mytests.exe [OPTIONS] [PATTERN]"
    ~actual:(Cli.usage ~prog:"/some/path/mytests.exe")

(* Resolution: defaults *)

let resolve parsed =
  match Cli.settings parsed with
  | Ok s -> s.Cli.config
  | Error error ->
      check "settings succeeds" false;
      Printf.printf "  settings error: %s\n%!" (Cli.error_message error);
      Run.default_config ()

(* The renderer half of the resolution: the four presentation knobs land
   in [settings]'s render field, not in [Run.config]. *)
let render_settings parsed =
  match Cli.settings parsed with
  | Ok s -> s.Cli.render
  | Error error ->
      check "settings succeeds" false;
      Printf.printf "  settings error: %s\n%!" (Cli.error_message error);
      Render.default_settings

let () =
  reg "resolution defaults" @@ fun () ->
  clear_env ();
  let config = resolve Cli.empty in
  check "default: no filters"
    (config.Run.filter = None && config.Run.exclude = None);
  check "default: no tags" (config.Run.tags = [] && config.Run.exclude_tags = []);
  check "default: flags off"
    ((not config.Run.failed_only)
    && (not config.Run.stream)
    && not config.Run.allow_focus);
  check "default: update off" (config.Run.update = Env.No_update);
  check "default: no bail/timeout/prop-count/junit"
    (config.Run.bail = None && config.Run.timeout = None
    && config.Run.prop_count = None
    && config.Run.junit = None);
  let render = render_settings Cli.empty in
  check "default: color auto" (render.Render.color = Env.Auto);
  check "default: env-only settings unset"
    (render.Render.columns = None && render.Render.tail_errors = None);
  check "default: log dir non-empty" (String.length config.Run.log_dir > 0)

(* Resolution: precedence *)

let () =
  reg "resolution precedence: CLI > env" @@ fun () ->
  clear_env ();
  Unix.putenv "WINDTRAP_FILTER" "envpat";
  let config = resolve Cli.empty in
  check "env fills an absent flag" (config.Run.filter = Some "envpat");
  let config = resolve { Cli.empty with Cli.filter = Some "clipat" } in
  check "CLI beats env" (config.Run.filter = Some "clipat");
  clear_env ()

let () =
  reg "tags are additive across layers" @@ fun () ->
  clear_env ();
  Unix.putenv "WINDTRAP_TAG" "e1, e2";
  Unix.putenv "WINDTRAP_EXCLUDE_TAG" "x1 ,, x2 ";
  let config =
    resolve { Cli.empty with Cli.tags = [ "c" ]; exclude_tags = [ "xc" ] }
  in
  check "tags are additive across layers, the CLI's first"
    (config.Run.tags = [ "c"; "e1"; "e2" ]);
  check "exclude tags are additive too, commas split and trimmed"
    (config.Run.exclude_tags = [ "xc"; "x1"; "x2" ]);
  clear_env ()

let () =
  reg "update precedence" @@ fun () ->
  clear_env ();
  Unix.putenv "WINDTRAP_UPDATE" "force";
  let config = resolve Cli.empty in
  check "WINDTRAP_UPDATE=force" (config.Run.update = Env.Force_update);
  let config = resolve { Cli.empty with Cli.update = Some Env.Update } in
  check "an explicit -u beats the env value" (config.Run.update = Env.Update);
  Unix.putenv "WINDTRAP_UPDATE" "1";
  let config = resolve Cli.empty in
  check "WINDTRAP_UPDATE=1" (config.Run.update = Env.Update);
  clear_env ()

let () =
  reg "seed precedence and malformed env seeds" @@ fun () ->
  clear_env ();
  Unix.putenv "WINDTRAP_SEED" "s1:00000000000000aa";
  let config = resolve Cli.empty in
  check "env seed is parsed" (config.Run.seed = 0xaaL);
  Unix.putenv "WINDTRAP_SEED" "not-a-seed";
  (match Cli.settings Cli.empty with
  | Error (Cli.Invalid_value { source = "WINDTRAP_SEED"; value; _ }) ->
      check "malformed env seed errors with its source" (value = "not-a-seed")
  | Ok _ | Error _ -> check "malformed env seed errors" false);
  let config = resolve { Cli.empty with Cli.seed = Some 7L } in
  check "a CLI seed leaves a malformed env seed unread" (config.Run.seed = 7L);
  clear_env ()

let () =
  reg "env-only settings" @@ fun () ->
  clear_env ();
  Unix.putenv "WINDTRAP_STREAM" "1";
  Unix.putenv "WINDTRAP_TIMEOUT" "1.5";
  Unix.putenv "WINDTRAP_PROP_COUNT" "7";
  Unix.putenv "WINDTRAP_MAX_SHRINK" "40";
  Unix.putenv "WINDTRAP_COLUMNS" "100";
  Unix.putenv "WINDTRAP_TAIL_ERRORS" "3";
  Unix.putenv "WINDTRAP_EXCLUDE" "skipme";
  let config = resolve Cli.empty in
  check "WINDTRAP_STREAM" config.Run.stream;
  check "WINDTRAP_TIMEOUT" (config.Run.timeout = Some 1.5);
  check "WINDTRAP_PROP_COUNT" (config.Run.prop_count = Some 7);
  check "WINDTRAP_MAX_SHRINK" (config.Run.max_shrink = Some 40);
  let render = render_settings Cli.empty in
  check "WINDTRAP_COLUMNS" (render.Render.columns = Some 100);
  check "WINDTRAP_TAIL_ERRORS" (render.Render.tail_errors = Some 3);
  check "WINDTRAP_EXCLUDE" (config.Run.exclude = Some "skipme");
  clear_env ()

(* The flagless rows keep their own tolerant vocabularies — no flag
   exists for a lenient reading to drift from, so a hostile or
   unparseable value counts as unset rather than refusing the run. *)
let () =
  reg "flagless settings vocabulary" @@ fun () ->
  clear_env ();
  Unix.putenv "WINDTRAP_COLUMNS" "0";
  check "non-positive columns count as unset"
    ((render_settings Cli.empty).Render.columns = None);
  Unix.putenv "WINDTRAP_COLUMNS" "-3";
  check "negative columns count as unset"
    ((render_settings Cli.empty).Render.columns = None);
  Unix.putenv "WINDTRAP_COLUMNS" "wide";
  check "unparseable columns count as unset"
    ((render_settings Cli.empty).Render.columns = None);
  clear_env ()

(* The mirrors that only existed as flags. Under `dune runtest` the mirrors
   *are* the CLI, so a flag without one is a documented feature no dune user
   can reach — `--junit`, which the CI guide recommends, most of all. *)
let () =
  reg "env-only settings: the CI and feedback-loop mirrors" @@ fun () ->
  clear_env ();
  Unix.putenv "WINDTRAP_BAIL" "3";
  Unix.putenv "WINDTRAP_FAILED" "1";
  Unix.putenv "WINDTRAP_JUNIT" "reports/junit.xml";
  Unix.putenv "WINDTRAP_OUTPUT" "custom-logs";
  let config = resolve Cli.empty in
  check "WINDTRAP_BAIL" (config.Run.bail = Some 3);
  check "WINDTRAP_FAILED" config.Run.failed_only;
  check "WINDTRAP_JUNIT" (config.Run.junit = Some "reports/junit.xml");
  (* Absolutized like [-o], for the same reason: a test that chdirs must
     not move the rest of the run's logs. *)
  check "WINDTRAP_OUTPUT"
    (Filename.is_relative config.Run.log_dir = false
    && Filename.basename config.Run.log_dir = "custom-logs");
  clear_env ()

let () =
  reg "the new mirrors lose to their flags" @@ fun () ->
  clear_env ();
  Unix.putenv "WINDTRAP_BAIL" "3";
  Unix.putenv "WINDTRAP_JUNIT" "from-env.xml";
  let config =
    resolve { Cli.empty with Cli.bail = Some 1; junit = Some "from-cli.xml" }
  in
  check "flag beats WINDTRAP_BAIL" (config.Run.bail = Some 1);
  check "flag beats WINDTRAP_JUNIT" (config.Run.junit = Some "from-cli.xml");
  (* A malformed mirror is a usage error naming the *variable* — the
     WINDTRAP_PROP_COUNT rule, not a silent default. *)
  clear_env ();
  Unix.putenv "WINDTRAP_BAIL" "0";
  (match Cli.settings Cli.empty with
  | Ok _ -> check "WINDTRAP_BAIL=0 is rejected" false
  | Error e ->
      check "the error names the variable, not the flag"
        (contains "WINDTRAP_BAIL" (Cli.error_message e)));
  (* A losing layer stays unread: a valid flag shadows a malformed mirror. *)
  Unix.putenv "WINDTRAP_BAIL" "not-a-number";
  (match Cli.settings { Cli.empty with Cli.bail = Some 2 } with
  | Ok s ->
      check "a valid flag shadows a malformed mirror"
        (s.Cli.config.Run.bail = Some 2)
  | Error e ->
      check
        ("malformed mirror leaked past the flag: " ^ Cli.error_message e)
        false);
  clear_env ()

let () =
  reg "parsed values land in the config" @@ fun () ->
  clear_env ();
  let config =
    resolve
      {
        Cli.empty with
        Cli.stream = Some true;
        bail = Some 2;
        log_dir = Some "custom-logs";
      }
  in
  check "parsed booleans and values land in the config"
    (config.Run.stream && config.Run.bail = Some 2
    (* Absolutized at resolve time so a test that chdirs cannot move the
       run's logs; the relative spelling is still what it ends with. *)
    && Filename.is_relative config.Run.log_dir = false
    && Filename.basename config.Run.log_dir = "custom-logs")

(* Resolution: a mirror is validated by its flag's own parser *)

let () =
  reg "numeric mirrors are validated by their flag's parser" @@ fun () ->
  clear_env ();
  Unix.putenv "WINDTRAP_TIMEOUT" "-5";
  (match Cli.settings Cli.empty with
  | Error (Cli.Invalid_value { source = "WINDTRAP_TIMEOUT"; value = "-5"; _ })
    ->
      check "a negative env timeout errors with its source" true
  | Ok _ | Error _ ->
      check "a negative env timeout errors with its source" false);
  let config = resolve { Cli.empty with Cli.timeout = Some 1.0 } in
  check "a valid CLI timeout shadows the bad env value"
    (config.Run.timeout = Some 1.0);
  clear_env ();
  Unix.putenv "WINDTRAP_PROP_COUNT" "0";
  (match Cli.settings Cli.empty with
  | Error (Cli.Invalid_value { source = "WINDTRAP_PROP_COUNT"; value = "0"; _ })
    ->
      check "a zero env prop count errors with its source" true
  | Ok _ | Error _ -> check "a zero env prop count errors with its source" false);
  clear_env ();
  (* Malformed mirror tokens error like their flags (prop/F-4): same knob,
     same garbage, same loud refusal in every layer. *)
  Unix.putenv "WINDTRAP_PROP_COUNT" "1O0";
  (match Cli.settings Cli.empty with
  | Error
      (Cli.Invalid_value { source = "WINDTRAP_PROP_COUNT"; value = "1O0"; _ })
    ->
      check "a malformed winning env prop count errors with its source" true
  | Ok _ | Error _ ->
      check "a malformed winning env prop count errors with its source" false);
  let config = resolve { Cli.empty with Cli.prop_count = Some 50 } in
  check "a valid CLI prop count leaves a malformed mirror unread"
    (config.Run.prop_count = Some 50);
  clear_env ();
  Unix.putenv "WINDTRAP_TIMEOUT" "banana";
  (match Cli.settings Cli.empty with
  | Error
      (Cli.Invalid_value { source = "WINDTRAP_TIMEOUT"; value = "banana"; _ })
    ->
      check "a malformed winning env timeout errors with its source" true
  | Ok _ | Error _ ->
      check "a malformed winning env timeout errors with its source" false);
  let config = resolve { Cli.empty with Cli.timeout = Some 2.0 } in
  check "a valid CLI timeout leaves a malformed mirror unread"
    (config.Run.timeout = Some 2.0);
  clear_env ();
  (* A mirror quotes the token as typed, exactly as its flag does: "-5.0"
     is what the user wrote and "-5.0" is what the message must show, not
     the shortest spelling of the float it parsed to. *)
  Unix.putenv "WINDTRAP_TIMEOUT" "-5.0";
  (match Cli.settings Cli.empty with
  | Error (Cli.Invalid_value { source = "WINDTRAP_TIMEOUT"; value; _ }) ->
      check "a mirror quotes the token as written" (value = "-5.0")
  | Ok _ | Error _ -> check "a mirror quotes the token as written" false);
  Unix.putenv "WINDTRAP_TIMEOUT" "1e400";
  (match Cli.settings Cli.empty with
  | Error (Cli.Invalid_value { source = "WINDTRAP_TIMEOUT"; value; _ }) ->
      check "an overflowing token is quoted, not printed as 'inf'"
        (value = "1e400")
  | Ok _ | Error _ ->
      check "an overflowing token is quoted, not printed as 'inf'" false);
  clear_env ()

let () =
  reg "color precedence" @@ fun () ->
  clear_env ();
  Unix.putenv "WINDTRAP_COLOR" "never";
  let render = render_settings Cli.empty in
  check "WINDTRAP_COLOR fills the default" (render.Render.color = Env.Never);
  let render = render_settings { Cli.empty with Cli.color = Some Env.Always } in
  check "--color beats WINDTRAP_COLOR" (render.Render.color = Env.Always);
  clear_env ()

(* Resolution: the inline coverage line *)

let () =
  reg "coverage line resolution" @@ fun () ->
  clear_env ();
  let enabled () =
    match Cli.settings Cli.empty with
    | Ok s -> s.Cli.coverage
    | Error error ->
        check ("settings succeeds: " ^ Cli.error_message error) false;
        true
  in
  check "an unset WINDTRAP_COVERAGE prints the line" (enabled ());
  Unix.putenv "WINDTRAP_COVERAGE" "OFF";
  check "a falsy WINDTRAP_COVERAGE silences it, case-insensitively"
    (not (enabled ()));
  Unix.putenv "WINDTRAP_COVERAGE" " 1 ";
  check "the truthy spellings are Env's, trimmed" (enabled ());
  Unix.putenv "WINDTRAP_COVERAGE" "report";
  (match Cli.settings Cli.empty with
  | Error
      (Cli.Invalid_value
         { source = "WINDTRAP_COVERAGE"; value = "report"; expected }) ->
      check "a retired mode word errors with its source" true;
      check "and the message names the reporting command"
        (contains "windtrap coverage" expected)
  | Ok _ | Error _ -> check "a retired mode word errors with its source" false);
  clear_env ()

(* Resolution: the output level *)

let () =
  reg "output level resolution" @@ fun () ->
  clear_env ();
  let level parsed =
    match Cli.settings parsed with
    | Ok s -> s.Cli.output_level
    | Error error ->
        check ("settings succeeds: " ^ Cli.error_message error) false;
        `Compact
  in
  check "default level is compact" (level Cli.empty = `Compact);
  Unix.putenv "WINDTRAP_QUIET" "1";
  check "WINDTRAP_QUIET reaches quiet (the dune runtest path)"
    (level Cli.empty = `Quiet);
  check "CLI -v beats WINDTRAP_QUIET"
    (level { Cli.empty with Cli.output = Some `Verbose } = `Verbose);
  Unix.putenv "WINDTRAP_VERBOSE" "1";
  check "verbose wins within the env layer" (level Cli.empty = `Verbose);
  clear_env ();
  Unix.putenv "WINDTRAP_VERBOSE" "maybe";
  check "an unparseable boolean counts as unset" (level Cli.empty = `Compact);
  clear_env ();
  Unix.putenv "WINDTRAP_QUIET" " 1 ";
  check "boolean spellings are trimmed, as WINDTRAP_STREAM's"
    (level Cli.empty = `Quiet);
  clear_env ()

(* Resolution: the one call both drivers make *)

let settings parsed =
  match Cli.settings parsed with
  | Ok settings -> settings
  | Error error ->
      check "settings succeeds" false;
      Printf.printf "  settings error: %s\n%!" (Cli.error_message error);
      {
        Cli.config = Run.default_config ();
        render = Render.default_settings;
        coverage = true;
        output_level = `Compact;
      }

let () =
  reg "settings resolves both layers in one call" @@ fun () ->
  clear_env ();
  Unix.putenv "WINDTRAP_SEED" "s1:0123456789abcdef";
  let s = settings { Cli.empty with Cli.filter = Some "geo" } in
  check "the flag reaches the config field"
    (s.Cli.config.Run.filter = Some "geo");
  check "the mirror reaches it too" (s.Cli.config.Run.seed = 0x0123456789abcdefL);
  check "the render field defaults" (s.Cli.render = Render.default_settings);
  check "the coverage field defaults to on" s.Cli.coverage;
  check "the level field defaults to compact" (s.Cli.output_level = `Compact);
  Unix.putenv "WINDTRAP_COVERAGE" "off";
  Unix.putenv "WINDTRAP_QUIET" "1";
  let s = settings Cli.empty in
  check "WINDTRAP_COVERAGE reaches the coverage field" (not s.Cli.coverage);
  check "WINDTRAP_QUIET reaches the level field" (s.Cli.output_level = `Quiet);
  clear_env ()

let () =
  reg "settings reports the configuration error" @@ fun () ->
  clear_env ();
  Unix.putenv "WINDTRAP_COVERAGE" "sideways";
  (match Cli.settings Cli.empty with
  | Error (Cli.Invalid_value { source = "WINDTRAP_COVERAGE"; _ }) ->
      check "a malformed WINDTRAP_COVERAGE is an error" true
  | Ok _ | Error _ -> check "a malformed WINDTRAP_COVERAGE is an error" false);
  clear_env ();
  Unix.putenv "WINDTRAP_SEED" "garbage";
  (match Cli.settings Cli.empty with
  | Error (Cli.Invalid_value { source = "WINDTRAP_SEED"; _ }) ->
      check "a malformed seed mirror is an error naming the variable" true
  | Ok _ | Error _ ->
      check "a malformed seed mirror is an error naming the variable" false);
  clear_env ()

(* Resolution: --slow-threshold and WINDTRAP_SLOW_THRESHOLD *)

let () =
  reg "--slow-threshold resolution" @@ fun () ->
  clear_env ();
  let render = render_settings Cli.empty in
  check "the built-in default is one second" (render.Render.slow_threshold = 1.0);
  Unix.putenv "WINDTRAP_SLOW_THRESHOLD" "3";
  let render = render_settings Cli.empty in
  check "WINDTRAP_SLOW_THRESHOLD fills an absent flag"
    (render.Render.slow_threshold = 3.0);
  let render =
    render_settings { Cli.empty with Cli.slow_threshold = Some 0.5 }
  in
  check "--slow-threshold beats the env mirror"
    (render.Render.slow_threshold = 0.5);
  Unix.putenv "WINDTRAP_SLOW_THRESHOLD" "-2";
  (match Cli.settings Cli.empty with
  | Error
      (Cli.Invalid_value { source = "WINDTRAP_SLOW_THRESHOLD"; value = "-2"; _ })
    ->
      check "a negative winning env threshold errors with its source" true
  | Ok _ | Error _ ->
      check "a negative winning env threshold errors with its source" false);
  let render =
    render_settings { Cli.empty with Cli.slow_threshold = Some 1.5 }
  in
  check "a CLI threshold shadows the bad env value"
    (render.Render.slow_threshold = 1.5);
  Unix.putenv "WINDTRAP_SLOW_THRESHOLD" "soon";
  (match Cli.settings Cli.empty with
  | Error
      (Cli.Invalid_value
         { source = "WINDTRAP_SLOW_THRESHOLD"; value = "soon"; _ }) ->
      check "a malformed winning env threshold errors, as WINDTRAP_TIMEOUT's"
        true
  | Ok _ | Error _ ->
      check "a malformed winning env threshold errors, as WINDTRAP_TIMEOUT's"
        false);
  clear_env ()

(* Resolution: --shard and WINDTRAP_SHARD (amendment B14) *)

let () =
  reg "--shard resolution (B14)" @@ fun () ->
  clear_env ();
  let config = resolve Cli.empty in
  check "no layer means no shard" (config.Run.shard = None);
  Unix.putenv "WINDTRAP_SHARD" "2/3";
  let config = resolve Cli.empty in
  check "WINDTRAP_SHARD fills an absent flag" (config.Run.shard = Some (2, 3));
  let config = resolve { Cli.empty with Cli.shard = Some (1, 2) } in
  check "--shard beats WINDTRAP_SHARD" (config.Run.shard = Some (1, 2));
  Unix.putenv "WINDTRAP_SHARD" "9/2";
  (match Cli.settings Cli.empty with
  | Error (Cli.Invalid_value { source = "WINDTRAP_SHARD"; value = "9/2"; _ }) ->
      check "a malformed winning env shard errors with its source" true
  | Ok _ | Error _ ->
      check "a malformed winning env shard errors with its source" false);
  let config = resolve { Cli.empty with Cli.shard = Some (1, 2) } in
  check "a CLI shard leaves a malformed env shard unread"
    (config.Run.shard = Some (1, 2));
  clear_env ()

(* Suite *)

let tests = List.rev !registered
