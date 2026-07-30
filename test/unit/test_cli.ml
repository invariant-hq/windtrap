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
      "--quick";
      "--failed";
      "-l";
      "--bail";
      "3";
      "-s";
      "-u";
      "--prune";
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
      check "quick" (p.Cli.quick = Some true);
      check "failed_only" (p.Cli.failed_only = Some true);
      check "list_only" (p.Cli.list_only = Some true);
      check "bail" (p.Cli.bail = Some 3);
      check "stream" (p.Cli.stream = Some true);
      check "update" (p.Cli.update = Some Env.Update);
      check "prune" (p.Cli.prune = Some true);
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
  expect_ok "--quick is selection only, not the output level" [ "--quick" ]
    (fun p -> check "--quick" (p.Cli.quick = Some true && p.Cli.output = None))

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
      "--quick";
      "--failed";
      "--list";
      "--fail-fast";
      "--bail";
      "--timeout";
      "--slow-threshold";
      "--seed";
      "--prop-count";
      "--update";
      "--prune";
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
      "WINDTRAP_PRUNE";
      "WINDTRAP_QUIET";
      "WINDTRAP_VERBOSE";
      "WINDTRAP_SLOW_THRESHOLD";
      "WINDTRAP_ALLOW_FOCUS";
      "WINDTRAP_COLUMNS";
      "WINDTRAP_TAIL_ERRORS";
      "WINDTRAP_PROJECT_ROOT";
      "WINDTRAP_COVERAGE";
    ];
  check_string "usage is one line with the basename"
    ~expected:"usage: mytests.exe [OPTIONS] [PATTERN]"
    ~actual:(Cli.usage ~prog:"/some/path/mytests.exe")

(* Resolution: defaults *)

let resolve ?overrides parsed =
  match Cli.resolve ?overrides parsed with
  | Ok config -> config
  | Error error ->
      check "resolve succeeds" false;
      Printf.printf "  resolve error: %s\n%!" (Cli.error_message error);
      Run.default_config ()

let () =
  reg "resolution defaults" @@ fun () ->
  clear_env ();
  let config = resolve Cli.empty in
  check "default: no filters"
    (config.Run.filter = None && config.Run.exclude = None);
  check "default: no tags" (config.Run.tags = [] && config.Run.exclude_tags = []);
  check "default: flags off"
    ((not config.Run.quick)
    && (not config.Run.failed_only)
    && (not config.Run.list_only) && (not config.Run.stream)
    && (not config.Run.prune) && not config.Run.allow_focus);
  check "default: update off" (config.Run.update = Env.No_update);
  check "default: no bail/timeout/prop-count/junit"
    (config.Run.bail = None && config.Run.timeout = None
    && config.Run.prop_count = None
    && config.Run.junit = None);
  check "default: color auto" (config.Run.color = Env.Auto);
  check "default: env-only settings unset"
    (config.Run.columns = None && config.Run.tail_errors = None);
  check "default: log dir non-empty" (String.length config.Run.log_dir > 0)

(* Resolution: precedence *)

let () =
  reg "resolution precedence: programmatic > CLI > env" @@ fun () ->
  clear_env ();
  Unix.putenv "WINDTRAP_FILTER" "envpat";
  let config = resolve Cli.empty in
  check "env fills an absent flag" (config.Run.filter = Some "envpat");
  let config = resolve { Cli.empty with Cli.filter = Some "clipat" } in
  check "CLI beats env" (config.Run.filter = Some "clipat");
  let config =
    resolve
      ~overrides:{ Cli.empty with Cli.filter = Some "progpat" }
      { Cli.empty with Cli.filter = Some "clipat" }
  in
  check "programmatic beats CLI" (config.Run.filter = Some "progpat");
  clear_env ()

let () =
  reg "tags are additive across layers" @@ fun () ->
  clear_env ();
  Unix.putenv "WINDTRAP_TAG" "e1, e2";
  let config =
    resolve
      ~overrides:{ Cli.empty with Cli.tags = [ "o" ] }
      { Cli.empty with Cli.tags = [ "c" ] }
  in
  check "tags are additive across layers, overrides first"
    (config.Run.tags = [ "o"; "c"; "e1"; "e2" ]);
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
  (match Cli.resolve Cli.empty with
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
  Unix.putenv "WINDTRAP_PRUNE" "yes";
  Unix.putenv "WINDTRAP_TIMEOUT" "1.5";
  Unix.putenv "WINDTRAP_PROP_COUNT" "7";
  Unix.putenv "WINDTRAP_ALLOW_FOCUS" "1";
  Unix.putenv "WINDTRAP_COLUMNS" "100";
  Unix.putenv "WINDTRAP_TAIL_ERRORS" "3";
  Unix.putenv "WINDTRAP_EXCLUDE" "skipme";
  let config = resolve Cli.empty in
  check "WINDTRAP_STREAM" config.Run.stream;
  check "WINDTRAP_PRUNE" config.Run.prune;
  check "WINDTRAP_TIMEOUT" (config.Run.timeout = Some 1.5);
  check "WINDTRAP_PROP_COUNT" (config.Run.prop_count = Some 7);
  check "WINDTRAP_ALLOW_FOCUS" config.Run.allow_focus;
  check "WINDTRAP_COLUMNS" (config.Run.columns = Some 100);
  check "WINDTRAP_TAIL_ERRORS" (config.Run.tail_errors = Some 3);
  check "WINDTRAP_EXCLUDE" (config.Run.exclude = Some "skipme");
  clear_env ()

let () =
  reg "parsed values land in the config" @@ fun () ->
  clear_env ();
  let config =
    resolve
      {
        Cli.empty with
        Cli.quick = Some true;
        bail = Some 2;
        log_dir = Some "custom-logs";
      }
  in
  check "parsed booleans and values land in the config"
    (config.Run.quick && config.Run.bail = Some 2
    && config.Run.log_dir = "custom-logs")

(* Resolution: numeric limits stay validated past the parser *)

let () =
  reg "numeric limits stay validated past the parser" @@ fun () ->
  clear_env ();
  Unix.putenv "WINDTRAP_TIMEOUT" "-5";
  (match Cli.resolve Cli.empty with
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
  (match Cli.resolve Cli.empty with
  | Error (Cli.Invalid_value { source = "WINDTRAP_PROP_COUNT"; value = "0"; _ })
    ->
      check "a zero env prop count errors with its source" true
  | Ok _ | Error _ -> check "a zero env prop count errors with its source" false);
  clear_env ();
  (* Malformed mirror tokens error like their flags (prop/F-4): same knob,
     same garbage, same loud refusal in every layer. *)
  Unix.putenv "WINDTRAP_PROP_COUNT" "1O0";
  (match Cli.resolve Cli.empty with
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
  (match Cli.resolve Cli.empty with
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
  (match
     Cli.resolve ~overrides:{ Cli.empty with Cli.bail = Some 0 } Cli.empty
   with
  | Error (Cli.Invalid_value { source = "--bail"; value = "0"; _ }) ->
      check "a non-positive programmatic bail errors" true
  | Ok _ | Error _ -> check "a non-positive programmatic bail errors" false);
  match
    Cli.resolve
      ~overrides:{ Cli.empty with Cli.timeout = Some infinity }
      Cli.empty
  with
  | Error (Cli.Invalid_value { source = "--timeout"; _ }) ->
      check "an infinite programmatic timeout errors" true
  | Ok _ | Error _ -> check "an infinite programmatic timeout errors" false

let () =
  reg "color precedence" @@ fun () ->
  clear_env ();
  Unix.putenv "WINDTRAP_COLOR" "never";
  let config = resolve Cli.empty in
  check "WINDTRAP_COLOR fills the default" (config.Run.color = Env.Never);
  let config = resolve { Cli.empty with Cli.color = Some Env.Always } in
  check "--color beats WINDTRAP_COLOR" (config.Run.color = Env.Always);
  clear_env ()

(* Resolution: the output level *)

let () =
  reg "output level resolution" @@ fun () ->
  clear_env ();
  check "default level is compact" (Cli.output_level Cli.empty = `Compact);
  Unix.putenv "WINDTRAP_QUIET" "1";
  check "WINDTRAP_QUIET reaches quiet (the dune runtest path)"
    (Cli.output_level Cli.empty = `Quiet);
  check "CLI -v beats WINDTRAP_QUIET"
    (Cli.output_level { Cli.empty with Cli.output = Some `Verbose } = `Verbose);
  Unix.putenv "WINDTRAP_VERBOSE" "1";
  check "verbose wins within the env layer"
    (Cli.output_level Cli.empty = `Verbose);
  clear_env ();
  Unix.putenv "WINDTRAP_VERBOSE" "maybe";
  check "an unparseable boolean counts as unset"
    (Cli.output_level Cli.empty = `Compact);
  clear_env ();
  Unix.putenv "WINDTRAP_QUIET" " 1 ";
  check "boolean spellings are trimmed, as WINDTRAP_STREAM's"
    (Cli.output_level Cli.empty = `Quiet);
  clear_env ();
  check "programmatic override beats CLI"
    (Cli.output_level
       ~overrides:{ Cli.empty with Cli.output = Some `Quiet }
       { Cli.empty with Cli.output = Some `Verbose }
    = `Quiet)

(* Resolution: --slow-threshold and WINDTRAP_SLOW_THRESHOLD *)

let () =
  reg "--slow-threshold resolution" @@ fun () ->
  clear_env ();
  let config = resolve Cli.empty in
  check "the built-in default is one second" (config.Run.slow_threshold = 1.0);
  Unix.putenv "WINDTRAP_SLOW_THRESHOLD" "3";
  let config = resolve Cli.empty in
  check "WINDTRAP_SLOW_THRESHOLD fills an absent flag"
    (config.Run.slow_threshold = 3.0);
  let config = resolve { Cli.empty with Cli.slow_threshold = Some 0.5 } in
  check "--slow-threshold beats the env mirror" (config.Run.slow_threshold = 0.5);
  Unix.putenv "WINDTRAP_SLOW_THRESHOLD" "-2";
  (match Cli.resolve Cli.empty with
  | Error
      (Cli.Invalid_value { source = "WINDTRAP_SLOW_THRESHOLD"; value = "-2"; _ })
    ->
      check "a negative winning env threshold errors with its source" true
  | Ok _ | Error _ ->
      check "a negative winning env threshold errors with its source" false);
  let config = resolve { Cli.empty with Cli.slow_threshold = Some 1.5 } in
  check "a CLI threshold shadows the bad env value"
    (config.Run.slow_threshold = 1.5);
  Unix.putenv "WINDTRAP_SLOW_THRESHOLD" "soon";
  (match Cli.resolve Cli.empty with
  | Error
      (Cli.Invalid_value
         { source = "WINDTRAP_SLOW_THRESHOLD"; value = "soon"; _ }) ->
      check "a malformed winning env threshold errors, as WINDTRAP_TIMEOUT's"
        true
  | Ok _ | Error _ ->
      check "a malformed winning env threshold errors, as WINDTRAP_TIMEOUT's"
        false);
  clear_env ();
  match
    Cli.resolve
      ~overrides:{ Cli.empty with Cli.slow_threshold = Some Float.infinity }
      Cli.empty
  with
  | Error (Cli.Invalid_value { source = "--slow-threshold"; _ }) ->
      check "a non-finite programmatic threshold errors" true
  | Ok _ | Error _ -> check "a non-finite programmatic threshold errors" false

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
  (match Cli.resolve Cli.empty with
  | Error (Cli.Invalid_value { source = "WINDTRAP_SHARD"; value = "9/2"; _ }) ->
      check "a malformed winning env shard errors with its source" true
  | Ok _ | Error _ ->
      check "a malformed winning env shard errors with its source" false);
  let config = resolve { Cli.empty with Cli.shard = Some (1, 2) } in
  check "a CLI shard leaves a malformed env shard unread"
    (config.Run.shard = Some (1, 2));
  clear_env ();
  match
    Cli.resolve ~overrides:{ Cli.empty with Cli.shard = Some (0, 4) } Cli.empty
  with
  | Error (Cli.Invalid_value { source = "--shard"; value = "0/4"; _ }) ->
      check "an out-of-range programmatic shard errors" true
  | Ok _ | Error _ -> check "an out-of-range programmatic shard errors" false

(* Suite *)

let tests = List.rev !registered
