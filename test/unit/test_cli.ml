(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Cli: the flag table (every flag, both spellings, value
   validation), positional-filter handling, typed parse errors, the
   generated help/usage text, the argument grammar's optional-value kind,
   and resolution precedence (CLI > env > default, additive tags, the two
   mirror reading rules, the WINDTRAP_SEED and WINDTRAP_SHARD error
   paths). Parsing and resolution are pure over argv and env, so each test
   clears the windtrap variables it touches. *)

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
      "-x";
      "-s";
      "-u";
      "--seed";
      "s1:00000000000000ff";
      "--timeout";
      "2.5";
      "--prop-count";
      "50";
      "-v";
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
      check "bail" (p.Cli.bail = Some true);
      check "stream" (p.Cli.stream = Some true);
      check "update" (p.Cli.update = Some true);
      check "seed" (p.Cli.seed = Some 0xffL);
      check "timeout" (p.Cli.timeout = Some 2.5);
      check "prop_count" (p.Cli.prop_count = Some 50);
      check "verbose" (p.Cli.verbose = Some true);
      check "junit" (p.Cli.junit = Some "out.xml");
      check "color" (p.Cli.color = Some Env.Never);
      check "log_dir" (p.Cli.log_dir = Some "logs");
      check "help off" (not p.Cli.help);
      check "version off" (not p.Cli.version))

let () =
  reg "long spellings and inline values" @@ fun () ->
  expect_ok "long spellings and --flag=value"
    [ "--filter=abc"; "--exclude=xyz"; "--prop-count=7"; "--color=ALWAYS" ]
    (fun p ->
      check "--filter=" (p.Cli.filter = Some "abc");
      check "--exclude=" (p.Cli.exclude = Some "xyz");
      check "--prop-count=" (p.Cli.prop_count = Some 7);
      check "--color= is case-insensitive" (p.Cli.color = Some Env.Always));
  expect_ok "-x is a boolean" [ "-x" ] (fun p ->
      check "-x" (p.Cli.bail = Some true));
  expect_ok "--fail-fast is -x" [ "--fail-fast" ] (fun p ->
      check "--fail-fast" (p.Cli.bail = Some true));
  expect_error "-x takes no value" [ "--fail-fast=2" ] (function
    | Cli.Invalid_value { source = "--fail-fast"; value = "2"; _ } -> true
    | _ -> false);
  expect_ok "later occurrence of a single-valued flag wins"
    [ "-f"; "first"; "-f"; "second" ] (fun p ->
      check "last wins" (p.Cli.filter = Some "second"));
  expect_ok "repeatable flags accept the inline spelling"
    [ "--tag=a"; "--exclude-tag=b"; "--tag=c" ] (fun p ->
      check "inline tags accumulate"
        (p.Cli.tags = [ "a"; "c" ] && p.Cli.exclude_tags = [ "b" ]))

(* Parsing: the output level (default ⊂ -v) *)

let () =
  reg "output level parsing" @@ fun () ->
  expect_ok "-v parses as verbose" [ "-v" ] (fun p ->
      check "-v" (p.Cli.verbose = Some true));
  expect_ok "--verbose parses" [ "--verbose" ] (fun p ->
      check "--verbose" (p.Cli.verbose = Some true));
  expect_ok "--exclude-tag is selection only, not the output level"
    [ "--exclude-tag"; "slow" ] (fun p ->
      check "--exclude-tag"
        (p.Cli.exclude_tags = [ "slow" ] && p.Cli.verbose = None))

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
     between --junit and --update, and the tie goes to table order. *)
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

(* The knobs that went — a failure count for -x, a shrink budget — are
   unknown flags like any other, with no near miss to suggest: nothing in
   the inventory is one slip from either. *)
let () =
  reg "the cut knobs are unknown flags" @@ fun () ->
  expect_error "--bail is unknown" [ "--bail"; "3" ] (function
    | Cli.Unknown_flag "--bail" -> true
    | _ -> false);
  expect_error "--max-shrink is unknown" [ "--max-shrink"; "5" ] (function
    | Cli.Unknown_flag "--max-shrink" -> true
    | _ -> false);
  List.iter
    (fun typo ->
      check
        (typo ^ " suggests nothing")
        (not
           (contains "did you mean" (Cli.error_message (Cli.Unknown_flag typo)))))
    [ "--bail"; "--max-shrink" ]

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
  expect_error "--prop-count rejects zero" [ "--prop-count"; "0" ] (function
    | Cli.Invalid_value { source = "--prop-count"; value = "0"; _ } -> true
    | _ -> false);
  expect_error "--prop-count rejects garbage" [ "--prop-count"; "many" ]
    (function
    | Cli.Invalid_value { source = "--prop-count"; _ } -> true
    | _ -> false);
  expect_error "--timeout rejects a negative number" [ "--timeout"; "-1" ]
    (function
    | Cli.Invalid_value { source = "--timeout"; _ } -> true
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
      Cli.Invalid_value
        { source = "--prop-count"; value = "x"; expected = "an int" };
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

(* The argument grammar: the optional-value kind, over a synthetic row.
   No flag uses it yet — [--mutate[=PREFIX,...]] will — and this pins what
   that flag gets, so adopting it adds a row and nothing else. The row
   stores into [junit], which nothing else in a one-row table touches. *)

let probe_row : Cli.entry =
  {
    Cli.short = Some "-p";
    long = "--probe";
    arg =
      Cli.Optional_value
        {
          metavar = "V";
          set =
            (fun ~source acc value ->
              match value with
              | None -> Ok { acc with Cli.junit = Some "<bare>" }
              | Some "bad" ->
                  Error
                    (Cli.Invalid_value
                       { source; value = "bad"; expected = "anything but bad" })
              | Some v -> Ok { acc with Cli.junit = Some v });
        };
    doc = "Probe the grammar";
    mirror =
      Some
        {
          Cli.var = "WINDTRAP_PROBE";
          layering = Cli.Single (fun p -> p.Cli.junit = None);
        };
  }

let () =
  reg "grammar: an optional-value flag on the command line" @@ fun () ->
  let parse args =
    Cli.parse_entries [ probe_row ] (Array.of_list ("windtrap-test" :: args))
  in
  let junit name args =
    match parse args with
    | Ok p -> p.Cli.junit
    | Error e -> fail (name ^ ": " ^ Cli.error_message e)
  in
  check "the help heading spells the value as optional"
    (Cli.flag_heading probe_row = "-p, --probe[=V]");
  check "the bare long flag" (junit "bare" [ "--probe" ] = Some "<bare>");
  check "the bare short flag" (junit "short" [ "-p" ] = Some "<bare>");
  check "an inline value"
    (junit "inline" [ "--probe=lib/a.ml,lib/b.ml" ] = Some "lib/a.ml,lib/b.ml");
  (match parse [ "--probe"; "next" ] with
  | Ok p ->
      check "a bare flag never consumes the next argument"
        (p.Cli.junit = Some "<bare>" && p.Cli.filter = Some "next")
  | Error e -> fail (Cli.error_message e));
  match parse [ "--probe=bad" ] with
  | Error (Cli.Invalid_value { source = "--probe"; value = "bad"; _ }) ->
      check "the row's parser refuses, naming the flag" true
  | Ok _ | Error _ -> check "the row's parser refuses, naming the flag" false

let () =
  reg "grammar: an optional-value flag's mirror" @@ fun () ->
  let layered cli =
    match Cli.layer_entries [ probe_row ] cli with
    | Ok p -> Ok p.Cli.junit
    | Error e -> Error e
  in
  Unix.putenv "WINDTRAP_PROBE" "1";
  check "a truthy value is the bare flag"
    (layered Cli.empty = Ok (Some "<bare>"));
  Unix.putenv "WINDTRAP_PROBE" "off";
  check "a falsy value is absence" (layered Cli.empty = Ok None);
  Unix.putenv "WINDTRAP_PROBE" " lib/a.ml ";
  check "anything else is the value, trimmed"
    (layered Cli.empty = Ok (Some "lib/a.ml"));
  Unix.putenv "WINDTRAP_PROBE" "bad";
  (match layered Cli.empty with
  | Error (Cli.Invalid_value { source = "WINDTRAP_PROBE"; value = "bad"; _ }) ->
      check "the mirror refuses through the same parser, naming the variable"
        true
  | Ok _ | Error _ ->
      check "the mirror refuses through the same parser, naming the variable"
        false);
  check "the command line shadows the mirror, unread"
    (layered { Cli.empty with Cli.junit = Some "cli" } = Ok (Some "cli"));
  Unix.putenv "WINDTRAP_PROBE" ""

(* Help and usage *)

(* --help is the CLI's whole user-facing surface, and the baseline pins
   every byte of it: its columns, its ordering, its wording, and which
   flags and variables exist at all. A second list asserting that each
   flag is MENTIONED said less than the golden already says. *)
let () =
  reg "help text, whole" @@ fun () ->
  expect_file
    (Cli.help ~prog:"/some/path/mytests.exe")
    "test/unit/expected/test_cli/help.expected"

let () =
  reg "usage line" @@ fun () ->
  check_string "usage is one line with the basename"
    ~expected:"usage: mytests.exe [OPTIONS] [PATTERN]"
    ~actual:(Cli.usage ~prog:"/some/path/mytests.exe")

(* Resolution: defaults *)

let settings parsed =
  match Cli.settings parsed with
  | Ok settings -> settings
  | Error error ->
      check "settings succeeds" false;
      Printf.printf "  settings error: %s\n%!" (Cli.error_message error);
      Run.default_config ()

let resolve = settings

let () =
  reg "resolution defaults" @@ fun () ->
  clear_env ();
  let config = resolve Cli.empty in
  check "default: no filters"
    (config.Run.filter = None && config.Run.exclude = None);
  check "default: no tags" (config.Run.tags = [] && config.Run.exclude_tags = []);
  check "default: flags off"
    ((not config.Run.failed_only)
    && (not config.Run.bail) && (not config.Run.stream)
    && not config.Run.allow_focus);
  check "default: baselines are checked" (config.Run.baseline = Baseline.Check);
  check "default: no timeout/prop-count"
    (config.Run.timeout = None && config.Run.prop_count = None);
  check "default: no JUnit report" (config.Run.junit = None);
  check "default: color auto" (config.Run.color = Env.Auto);
  check "default: the slow threshold is one second"
    (config.Run.slow_threshold = 1.0);
  check "default: compact" (not config.Run.verbose);
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

(* Resolution: the two reading rules *)

let () =
  reg "reading rules: a plain value is one token, trimmed" @@ fun () ->
  clear_env ();
  Unix.putenv "WINDTRAP_FILTER" "  parser ";
  Unix.putenv "WINDTRAP_SHARD" " 2/4 ";
  Unix.putenv "WINDTRAP_PROP_COUNT" " 12 ";
  Unix.putenv "WINDTRAP_JUNIT" " out.xml ";
  let s = settings Cli.empty in
  check "a pattern is trimmed" (s.Run.filter = Some "parser");
  check "a shard is trimmed" (s.Run.shard = Some (2, 4));
  check "a count is trimmed" (s.Run.prop_count = Some 12);
  check "a path is trimmed" (s.Run.junit = Some "out.xml");
  clear_env ()

let () =
  reg "reading rules: a valueless flag's mirror is a boolean" @@ fun () ->
  clear_env ();
  Unix.putenv "WINDTRAP_STREAM" "yes";
  Unix.putenv "WINDTRAP_VERBOSE" " OFF ";
  let s = settings Cli.empty in
  check "a truthy spelling applies the flag" s.Run.stream;
  check "a falsy spelling is absence, trimmed and case-insensitively"
    (not s.Run.verbose);
  Unix.putenv "WINDTRAP_STREAM" "maybe";
  (match Cli.settings Cli.empty with
  | Error
      (Cli.Invalid_value
         { source = "WINDTRAP_STREAM"; value = "maybe"; expected }) ->
      check "anything else is refused, naming the variable and the vocabulary"
        (contains "1/0" expected)
  | Ok _ | Error _ ->
      check "anything else is refused, naming the variable and the vocabulary"
        false);
  check "a flag on the command line shadows the bad value, unread"
    (resolve { Cli.empty with Cli.stream = Some true }).Run.stream;
  clear_env ()

(* The two acceptance flags have no mirror: a build action accepts nothing
   through its environment. Neither do the three feedback-loop flags. *)
let () =
  reg "acceptance flags: no mirror, one mode each, never both" @@ fun () ->
  clear_env ();
  Unix.putenv "WINDTRAP_UPDATE" "1";
  check "WINDTRAP_UPDATE is not a mirror"
    ((resolve Cli.empty).Run.baseline = Baseline.Check);
  clear_env ();
  check "-u resolves to Update"
    ((resolve { Cli.empty with Cli.update = Some true }).Run.baseline
   = Baseline.Update);
  check "--corrected resolves to Corrected"
    ((resolve { Cli.empty with Cli.corrected = Some true }).Run.baseline
   = Baseline.Corrected);
  expect_ok "--corrected parses" [ "--corrected" ] (fun p ->
      check "corrected" (p.Cli.corrected = Some true && p.Cli.update = None));
  expect_error "-u and --corrected together are refused" [ "-u"; "--corrected" ]
    (function
    | Cli.Incompatible_flags ("-u", "--corrected") -> true
    | _ -> false);
  expect_error "the refusal reads either order" [ "--corrected"; "--update" ]
    (function
    | Cli.Incompatible_flags _ -> true
    | _ -> false);
  Windtrap.contains ~msg:"the message names both flags"
    ~sub:"'-u' and '--corrected'"
    (Cli.error_message (Cli.Incompatible_flags ("-u", "--corrected")))

let () =
  reg "feedback-loop flags: no mirror" @@ fun () ->
  clear_env ();
  Unix.putenv "WINDTRAP_BAIL" "1";
  Unix.putenv "WINDTRAP_FAILED" "1";
  Unix.putenv "WINDTRAP_LIST" "1";
  let config = resolve Cli.empty in
  check "WINDTRAP_BAIL is not a mirror" (not config.Run.bail);
  check "WINDTRAP_FAILED is not a mirror" (not config.Run.failed_only);
  check "-x resolves to bail"
    (resolve { Cli.empty with Cli.bail = Some true }).Run.bail;
  Unix.putenv "WINDTRAP_BAIL" "";
  Unix.putenv "WINDTRAP_FAILED" "";
  Unix.putenv "WINDTRAP_LIST" "";
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
  Unix.putenv "WINDTRAP_EXCLUDE" "skipme";
  let config = resolve Cli.empty in
  check "WINDTRAP_STREAM" config.Run.stream;
  check "WINDTRAP_TIMEOUT" (config.Run.timeout = Some 1.5);
  check "WINDTRAP_PROP_COUNT" (config.Run.prop_count = Some 7);
  check "WINDTRAP_EXCLUDE" (config.Run.exclude = Some "skipme");
  clear_env ()

(* The mirrors that only existed as flags. Under `dune runtest` the mirrors
   *are* the CLI, so a flag without one is a documented feature no dune user
   can reach — `--junit`, which the CI guide recommends, most of all. *)
let () =
  reg "env-only settings: the CI mirrors" @@ fun () ->
  clear_env ();
  Unix.putenv "WINDTRAP_JUNIT" "reports/junit.xml";
  Unix.putenv "WINDTRAP_OUTPUT" "custom-logs";
  let config = resolve Cli.empty in
  check "WINDTRAP_JUNIT"
    ((settings Cli.empty).Run.junit = Some "reports/junit.xml");
  (* Absolutized like [-o], for the same reason: a test that chdirs must
     not move the rest of the run's logs. *)
  check "WINDTRAP_OUTPUT"
    (Filename.is_relative config.Run.log_dir = false
    && Filename.basename config.Run.log_dir = "custom-logs");
  clear_env ()

let () =
  reg "the mirrors lose to their flags" @@ fun () ->
  clear_env ();
  Unix.putenv "WINDTRAP_PROP_COUNT" "3";
  Unix.putenv "WINDTRAP_JUNIT" "from-env.xml";
  let cli =
    { Cli.empty with Cli.prop_count = Some 1; junit = Some "from-cli.xml" }
  in
  check "flag beats WINDTRAP_PROP_COUNT" ((resolve cli).Run.prop_count = Some 1);
  check "flag beats WINDTRAP_JUNIT"
    ((settings cli).Run.junit = Some "from-cli.xml");
  (* A malformed mirror is a usage error naming the *variable*, not a
     silent default. *)
  clear_env ();
  Unix.putenv "WINDTRAP_PROP_COUNT" "0";
  (match Cli.settings Cli.empty with
  | Ok _ -> check "WINDTRAP_PROP_COUNT=0 is rejected" false
  | Error e ->
      check "the error names the variable, not the flag"
        (contains "WINDTRAP_PROP_COUNT" (Cli.error_message e)));
  (* A losing layer stays unread: a valid flag shadows a malformed mirror. *)
  Unix.putenv "WINDTRAP_PROP_COUNT" "not-a-number";
  (match Cli.settings { Cli.empty with Cli.prop_count = Some 2 } with
  | Ok s ->
      check "a valid flag shadows a malformed mirror" (s.Run.prop_count = Some 2)
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
        bail = Some true;
        log_dir = Some "custom-logs";
      }
  in
  check "parsed booleans and values land in the config"
    (config.Run.stream && config.Run.bail
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

(* WINDTRAP_COLOR is a mirror like every other: --color's parser reads
   it, so a word the flag refuses is refused here too, never read as
   auto. The flagless commands read the variable through the same
   parser. *)
let () =
  reg "color precedence, and the mirror refuses what the flag refuses"
  @@ fun () ->
  clear_env ();
  Unix.putenv "WINDTRAP_COLOR" "never";
  let render = settings Cli.empty in
  check "WINDTRAP_COLOR fills the default" (render.Run.color = Env.Never);
  let render = settings { Cli.empty with Cli.color = Some Env.Always } in
  check "--color beats WINDTRAP_COLOR" (render.Run.color = Env.Always);
  Unix.putenv "WINDTRAP_COLOR" " Never ";
  check "the mirror is trimmed and case-insensitive, as --color is"
    ((settings Cli.empty).Run.color = Env.Never);
  check "color_mode reads the same variable the same way"
    (Cli.color_mode () = Ok Env.Never);
  Unix.putenv "WINDTRAP_COLOR" "sometimes";
  (match Cli.settings Cli.empty with
  | Error
      (Cli.Invalid_value
         { source = "WINDTRAP_COLOR"; value = "sometimes"; expected }) ->
      check "a bad WINDTRAP_COLOR is refused with --color's wording"
        (expected = "always, never or auto")
  | Ok _ | Error _ ->
      check "a bad WINDTRAP_COLOR is refused with --color's wording" false);
  (match Cli.color_mode () with
  | Error
      (Cli.Invalid_value { source = "WINDTRAP_COLOR"; value = "sometimes"; _ })
    ->
      check "color_mode refuses it too, naming the variable" true
  | Ok _ | Error _ ->
      check "color_mode refuses it too, naming the variable" false);
  check "a --color on the command line shadows the bad value, unread"
    ((settings { Cli.empty with Cli.color = Some Env.Auto }).Run.color
   = Env.Auto);
  clear_env ();
  check "color_mode defaults to auto" (Cli.color_mode () = Ok Env.Auto)

(* Resolution: the inline coverage line *)

let () =
  reg "coverage line resolution" @@ fun () ->
  clear_env ();
  let enabled () =
    match Cli.settings Cli.empty with
    | Ok s -> s.Run.coverage
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
    | Ok s -> if s.Run.verbose then `Verbose else `Compact
    | Error error ->
        check ("settings succeeds: " ^ Cli.error_message error) false;
        `Compact
  in
  check "default level is compact" (level Cli.empty = `Compact);
  Unix.putenv "WINDTRAP_VERBOSE" "1";
  check "WINDTRAP_VERBOSE reaches verbose (the dune runtest path)"
    (level Cli.empty = `Verbose);
  clear_env ();
  Unix.putenv "WINDTRAP_VERBOSE" "maybe";
  (match Cli.settings Cli.empty with
  | Error
      (Cli.Invalid_value { source = "WINDTRAP_VERBOSE"; value = "maybe"; _ }) ->
      check "an unparseable boolean is refused, naming the variable" true
  | Ok _ | Error _ ->
      check "an unparseable boolean is refused, naming the variable" false);
  clear_env ();
  Unix.putenv "WINDTRAP_VERBOSE" " 1 ";
  check "boolean spellings are trimmed, as WINDTRAP_STREAM's"
    (level Cli.empty = `Verbose);
  clear_env ()

(* Resolution: the one call the facade makes *)

let () =
  reg "settings resolves both layers in one call" @@ fun () ->
  clear_env ();
  Unix.putenv "WINDTRAP_SEED" "s1:0123456789abcdef";
  let s = settings { Cli.empty with Cli.filter = Some "geo" } in
  check "the flag reaches the config field" (s.Run.filter = Some "geo");
  check "the mirror reaches it too" (s.Run.seed = 0x0123456789abcdefL);
  check "the presentation fields default"
    (s.Run.color = Env.Auto && s.Run.slow_threshold = 1.0);
  check "the coverage field defaults to on" s.Run.coverage;
  check "the level field defaults to compact" (not s.Run.verbose);
  Unix.putenv "WINDTRAP_COVERAGE" "off";
  Unix.putenv "WINDTRAP_VERBOSE" "1";
  let s = settings Cli.empty in
  check "WINDTRAP_COVERAGE reaches the coverage field" (not s.Run.coverage);
  check "WINDTRAP_VERBOSE reaches the level field" s.Run.verbose;
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
  let render = settings Cli.empty in
  check "the built-in default is one second" (render.Run.slow_threshold = 1.0);
  Unix.putenv "WINDTRAP_SLOW_THRESHOLD" "3";
  let render = settings Cli.empty in
  check "WINDTRAP_SLOW_THRESHOLD fills an absent flag"
    (render.Run.slow_threshold = 3.0);
  let render = settings { Cli.empty with Cli.slow_threshold = Some 0.5 } in
  check "--slow-threshold beats the env mirror" (render.Run.slow_threshold = 0.5);
  Unix.putenv "WINDTRAP_SLOW_THRESHOLD" "-2";
  (match Cli.settings Cli.empty with
  | Error
      (Cli.Invalid_value { source = "WINDTRAP_SLOW_THRESHOLD"; value = "-2"; _ })
    ->
      check "a negative winning env threshold errors with its source" true
  | Ok _ | Error _ ->
      check "a negative winning env threshold errors with its source" false);
  let render = settings { Cli.empty with Cli.slow_threshold = Some 1.5 } in
  check "a CLI threshold shadows the bad env value"
    (render.Run.slow_threshold = 1.5);
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
let () = exit @@ Windtrap.run "cli" tests
