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
let contains needle haystack = Text.contains_substring ~pattern:needle haystack

(* No toplevel clear: module initialization must not clear the hosting
   runner's own environment. Each resolution test clears what it reads,
   through the runner's [setenv], which restores every variable when the
   attempt ends, so a test that fails half-way leaves nothing behind. The
   variable inventory is the harness's ([Harness.windtrap_vars]). One
   list, one owner, so a mirror added there is cleared here by
   construction. INSIDE_DUNE and WINDTRAP_PROJECT_ROOT stay untouched:
   they configure the hosting run itself, not [Cli] resolution. *)
let clear_env () =
  List.iter
    (fun var ->
      if var <> "INSIDE_DUNE" && var <> "WINDTRAP_PROJECT_ROOT" then
        setenv var (Some ""))
    Harness.windtrap_vars

let parse args = Cli.parse (Array.of_list ("windtrap-test" :: args))

let expect_ok name args f =
  match parse args with
  | Ok parsed -> f parsed
  | Error error -> fail (name ^ ": parse error: " ^ Cli.error_message error)

let expect_error name args pred =
  match parse args with
  | Ok _ -> is_true ~msg:(name ^ " (should not parse)") false
  | Error error -> is_true ~msg:name (pred error)

let () =
  reg "no arguments parse to the empty record" @@ fun () ->
  expect_ok "no arguments parse to the empty record" [] (fun p ->
      is_true ~msg:"empty record" (p = Cli.empty));
  is_true ~msg:"an empty argv, no program name either, is empty"
    (Cli.parse [||] = Ok Cli.empty)

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
      "--mutate=lib/a.ml,lib/b.ml";
      "--arm";
      "lib/a.ml:1:0:add";
    ] (fun p ->
      is_true ~msg:"filter" (p.Cli.filter = [ "pat" ]);
      is_true ~msg:"exclude" (p.Cli.exclude = [ "ex" ]);
      is_true ~msg:"tags accumulate in order" (p.Cli.tags = [ "a"; "b" ]);
      is_true ~msg:"exclude_tags" (p.Cli.exclude_tags = [ "c" ]);
      is_true ~msg:"failed_only" (p.Cli.failed_only = Some true);
      is_true ~msg:"list_only" (p.Cli.list_only = Some true);
      is_true ~msg:"bail" (p.Cli.bail = Some true);
      is_true ~msg:"stream" (p.Cli.stream = Some true);
      is_true ~msg:"update" (p.Cli.update = Some true);
      is_true ~msg:"seed" (p.Cli.seed = Some 0xffL);
      is_true ~msg:"timeout" (p.Cli.timeout = Some 2.5);
      is_true ~msg:"prop_count" (p.Cli.prop_count = Some 50);
      is_true ~msg:"verbose" (p.Cli.verbose = Some true);
      is_true ~msg:"junit" (p.Cli.junit = Some "out.xml");
      is_true ~msg:"color" (p.Cli.color = Some Os.Never);
      is_true ~msg:"log_dir" (p.Cli.log_dir = Some "logs");
      is_true ~msg:"mutate" (p.Cli.mutate = Some [ "lib/a.ml"; "lib/b.ml" ]);
      is_true ~msg:"arm" (p.Cli.arm = Some "lib/a.ml:1:0:add");
      is_true ~msg:"help off" (not p.Cli.help);
      is_true ~msg:"version off" (not p.Cli.version))

let () =
  reg "long spellings and inline values" @@ fun () ->
  expect_ok "long spellings and --flag=value"
    [ "--filter=abc"; "--exclude=xyz"; "--prop-count=7"; "--color=ALWAYS" ]
    (fun p ->
      is_true ~msg:"--filter=" (p.Cli.filter = [ "abc" ]);
      is_true ~msg:"--exclude=" (p.Cli.exclude = [ "xyz" ]);
      is_true ~msg:"--prop-count=" (p.Cli.prop_count = Some 7);
      is_true ~msg:"--color= is case-insensitive" (p.Cli.color = Some Os.Always));
  expect_ok "-x is a boolean" [ "-x" ] (fun p ->
      is_true ~msg:"-x" (p.Cli.bail = Some true));
  expect_ok "--fail-fast is -x" [ "--fail-fast" ] (fun p ->
      is_true ~msg:"--fail-fast" (p.Cli.bail = Some true));
  expect_error "-x takes no value" [ "--fail-fast=2" ] (function
    | Cli.Invalid_value { source = "--fail-fast"; value = "2"; _ } -> true
    | _ -> false);
  expect_ok "later occurrence of a single-valued flag wins"
    [ "--junit"; "first"; "--junit"; "second" ] (fun p ->
      is_true ~msg:"last wins" (p.Cli.junit = Some "second"));
  expect_ok "a second -f adds a pattern, it does not replace the first"
    [ "-f"; "first"; "--filter"; "second"; "-e"; "x"; "--exclude=y" ] (fun p ->
      equal ~msg:"the patterns, in the order given" (list string)
        [ "first"; "second" ] p.Cli.filter;
      equal ~msg:"-e likewise" (list string) [ "x"; "y" ] p.Cli.exclude);
  expect_ok "repeatable flags accept the inline spelling"
    [ "--tag=a"; "--exclude-tag=b"; "--tag=c" ] (fun p ->
      is_true ~msg:"inline tags accumulate"
        (p.Cli.tags = [ "a"; "c" ] && p.Cli.exclude_tags = [ "b" ]))

(* Parsing: the output level (default ⊂ -v) *)

let () =
  reg "output level parsing" @@ fun () ->
  expect_ok "-v parses as verbose" [ "-v" ] (fun p ->
      is_true ~msg:"-v" (p.Cli.verbose = Some true));
  expect_ok "--verbose parses" [ "--verbose" ] (fun p ->
      is_true ~msg:"--verbose" (p.Cli.verbose = Some true));
  expect_ok "--exclude-tag is selection only, not the output level"
    [ "--exclude-tag"; "slow" ] (fun p ->
      is_true ~msg:"--exclude-tag"
        (p.Cli.exclude_tags = [ "slow" ] && p.Cli.verbose = None))

(* Parsing: positionals *)

let () =
  reg "positionals" @@ fun () ->
  expect_ok "a bare argument is the filter" [ "somepattern" ] (fun p ->
      equal ~msg:"positional filter" (list string) [ "somepattern" ]
        p.Cli.filter);
  expect_ok "arguments after -- are positionals" [ "--"; "-weird" ] (fun p ->
      equal ~msg:"post -- positional" (list string) [ "-weird" ] p.Cli.filter);
  expect_ok "a second positional is a second pattern" [ "one"; "two" ] (fun p ->
      equal ~msg:"both, in order" (list string) [ "one"; "two" ] p.Cli.filter);
  expect_ok "positionals and -f add up, in the order given"
    [ "one"; "-f"; "two"; "three"; "--"; "-four" ] (fun p ->
      equal ~msg:"every pattern" (list string)
        [ "one"; "two"; "three"; "-four" ]
        p.Cli.filter);
  expect_ok "a lone dash is an ordinary positional" [ "-" ] (fun p ->
      equal ~msg:"dash filter" (list string) [ "-" ] p.Cli.filter)

(* Parsing: help and version stop early *)

let () =
  reg "help and version stop early" @@ fun () ->
  expect_ok "-h sets help" [ "-h" ] (fun p -> is_true ~msg:"help" p.Cli.help);
  expect_ok "-V sets version" [ "-V" ] (fun p ->
      is_true ~msg:"version" p.Cli.version);
  expect_ok "--help wins over later garbage" [ "--help"; "--bogus" ] (fun p ->
      is_true ~msg:"help despite garbage" p.Cli.help)

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
    is_true
      ~msg:(Printf.sprintf "%s should suggest %s, got: %s" typo expected m)
      (contains (Printf.sprintf "did you mean '%s'?" expected) m)
  in
  suggests "--fliter" "--filter";
  suggests "--colour" "--color";
  suggests "--tags" "--tag";
  (* Transposition is one edit, not two: plain Levenshtein ties --juint
     between --junit and --update, and the tie goes to table order. *)
  suggests "--juint" "--junit";
  (* The verb a reader reaches for, one letter from the flag. *)
  suggests "--mutant" "--mutate";
  let silent typo =
    let m = message [ typo ] in
    is_true
      ~msg:(Printf.sprintf "%s should suggest nothing, got: %s" typo m)
      (not (contains "did you mean" m))
  in
  (* The rule at its boundary: at most [max 2 (n / 3)] edits from a long
     flag, [n] the typed flag's length. Eight bytes allow two edits... *)
  suggests "--fxltxr" "--filter";
  silent "--fxxtxr";
  (* ... and twelve allow four. *)
  suggests "--prxx-cxxnt" "--prop-count";
  silent "--prxx-xxxnt";
  (* Too far to be a slip. *)
  silent "--completely-different";
  (* Three edits from [--tag], [--list] and [--arm], though "lt" and "ti"
     each share a letter with a transposition. *)
  silent "--lti";
  (* Two edits from both [--verbose] and [--version]: the table's order
     breaks the tie. *)
  suggests "--verbon" "--verbose";
  (* Any two short flags are one edit apart, so any suggestion would be
     arbitrary; a confident wrong one is worse than none. *)
  silent "-Z";
  is_true ~msg:"the bare error is still there"
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
  expect_error "--timeout rejects zero" [ "--timeout"; "0" ] (function
    | Cli.Invalid_value { source = "--timeout"; value = "0"; _ } -> true
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
    ]
  in
  List.iter
    (fun error ->
      let message = Cli.error_message error in
      is_true ~msg:"error messages are non-empty" (String.length message > 0))
    messages;
  is_true ~msg:"error message names the flag"
    (contains "--bogus" (Cli.error_message (Cli.Unknown_flag "--bogus")));
  (* One sentence each, to go behind [windtrap:]; the facade's cram pins
     them on stderr. *)
  List.iter
    (fun (error, expected) ->
      equal ~msg:expected string expected (Cli.error_message error))
    [
      (Cli.Unknown_flag "--bogus", "unknown option '--bogus'");
      ( Cli.Unknown_flag "--filtre",
        "unknown option '--filtre'; did you mean '--filter'?" );
      (Cli.Missing_value "--junit", "option '--junit' requires an argument");
      ( Cli.Invalid_value
          { source = "--prop-count"; value = "x"; expected = "an int" },
        "invalid value 'x' for --prop-count: expected an int" );
      ( Cli.Incompatible_flags ("--mutate", "--arm"),
        "options '--mutate' and '--arm' cannot be combined" );
    ];
  let parse_error args =
    match parse args with
    | Ok _ -> fail "expected a parse error"
    | Error e -> Cli.error_message e
  in
  equal ~msg:"a seed's accepted form is spelled in full" string
    "invalid value 'nope' for --seed: expected an s1: token with 16 lowercase \
     hexadecimal digits"
    (parse_error [ "--seed"; "nope" ]);
  equal ~msg:"a shard's accepted form carries its example" string
    "invalid value '9/2' for --shard: expected K/N with 1 <= K <= N (e.g. 2/4)"
    (parse_error [ "--shard"; "9/2" ]);
  equal ~msg:"a flag that takes no argument says so" string
    "invalid value 'x' for --list: expected no argument"
    (parse_error [ "--list=x" ])

(* --slow-threshold *)

let () =
  reg "--slow-threshold parsing" @@ fun () ->
  expect_ok "--slow-threshold parses a decimal" [ "--slow-threshold"; "2.5" ]
    (fun p -> is_true ~msg:"threshold value" (p.Cli.slow_threshold = Some 2.5));
  expect_ok "--slow-threshold accepts zero (disable)"
    [ "--slow-threshold"; "0" ] (fun p ->
      is_true ~msg:"zero threshold" (p.Cli.slow_threshold = Some 0.0));
  expect_ok "--slow-threshold=SECS parses inline" [ "--slow-threshold=0.5" ]
    (fun p -> is_true ~msg:"inline threshold" (p.Cli.slow_threshold = Some 0.5))

(* --shard *)

let () =
  reg "--shard parsing" @@ fun () ->
  expect_ok "--shard K/N parses" [ "--shard"; "2/4" ] (fun p ->
      is_true ~msg:"shard pair" (p.Cli.shard = Some (2, 4)));
  expect_ok "--shard=K/N parses inline" [ "--shard=1/1" ] (fun p ->
      is_true ~msg:"inline shard" (p.Cli.shard = Some (1, 1)));
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
   [--mutate[=PREFIX,...]] is the flag that uses it, and this pins what
   the kind gives any row, apart from what that flag makes of its value.
   The row stores into [junit], which nothing else in a one-row table
   touches. *)

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
  equal ~msg:"the help heading spells the value as optional, then the mirror"
    string "-p, --probe[=V] (env WINDTRAP_PROBE)"
    (Cli.flag_heading probe_row);
  is_true ~msg:"the bare long flag" (junit "bare" [ "--probe" ] = Some "<bare>");
  is_true ~msg:"the bare short flag" (junit "short" [ "-p" ] = Some "<bare>");
  is_true ~msg:"an inline value"
    (junit "inline" [ "--probe=lib/a.ml,lib/b.ml" ] = Some "lib/a.ml,lib/b.ml");
  (match parse [ "--probe"; "next" ] with
  | Ok p ->
      is_true ~msg:"a bare flag never consumes the next argument"
        (p.Cli.junit = Some "<bare>" && p.Cli.filter = [ "next" ])
  | Error e -> fail (Cli.error_message e));
  match parse [ "--probe=bad" ] with
  | Error (Cli.Invalid_value { source = "--probe"; value = "bad"; _ }) ->
      is_true ~msg:"the row's parser refuses, naming the flag" true
  | Ok _ | Error _ ->
      is_true ~msg:"the row's parser refuses, naming the flag" false

let () =
  reg "grammar: an optional-value flag's mirror" @@ fun () ->
  let layered cli =
    match Cli.layer_entries [ probe_row ] cli with
    | Ok p -> Ok p.Cli.junit
    | Error e -> Error e
  in
  setenv "WINDTRAP_PROBE" (Some "1");
  is_true ~msg:"a truthy value is the bare flag"
    (layered Cli.empty = Ok (Some "<bare>"));
  setenv "WINDTRAP_PROBE" (Some "off");
  is_true ~msg:"a falsy value is absence" (layered Cli.empty = Ok None);
  setenv "WINDTRAP_PROBE" (Some " lib/a.ml ");
  is_true ~msg:"anything else is the value, trimmed"
    (layered Cli.empty = Ok (Some "lib/a.ml"));
  setenv "WINDTRAP_PROBE" (Some "bad");
  (match layered Cli.empty with
  | Error (Cli.Invalid_value { source = "WINDTRAP_PROBE"; value = "bad"; _ }) ->
      is_true
        ~msg:"the mirror refuses through the same parser, naming the variable"
        true
  | Ok _ | Error _ ->
      is_true
        ~msg:"the mirror refuses through the same parser, naming the variable"
        false);
  is_true ~msg:"the command line shadows the mirror, unread"
    (layered { Cli.empty with Cli.junit = Some "cli" } = Ok (Some "cli"))

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

(* The meta harness clears the runner's variables by name, and a name it
   misses is a setting the scripted runs silently take from the shell
   running them. The page names every variable a user sets: the mirrors
   after their flags, the rest under ENVIRONMENT. *)
let () =
  reg "the harness clears every variable the help page names" @@ fun () ->
  let page = Cli.help ~prog:"mytests.exe" in
  let is_name_char c = (c >= 'A' && c <= 'Z') || c = '_' in
  let rec names i acc =
    if i >= String.length page then acc
    else if is_name_char page.[i] then begin
      let j = ref i in
      while !j < String.length page && is_name_char page.[!j] do
        incr j
      done;
      let word = String.sub page i (!j - i) in
      let acc =
        if String.starts_with ~prefix:"WINDTRAP_" word || word = "NO_COLOR" then
          word :: acc
        else acc
      in
      names !j acc
    end
    else names (i + 1) acc
  in
  let on_the_page = List.sort_uniq String.compare (names 0 []) in
  let read_but_not_listed =
    (* Set by the host, never by a user of the page: the CI service and
       dune. *)
    [ "CI"; "GITHUB_ACTIONS"; "INSIDE_DUNE" ]
  in
  let bound_by_this_suite =
    [
      "WINDTRAP_UPDATE";
      "WINDTRAP_BAIL";
      "WINDTRAP_FAILED";
      "WINDTRAP_LIST";
      "WINDTRAP_PROBE";
    ]
  in
  equal ~msg:"the harness's list is the page's names and the named extras"
    (slist string String.compare)
    (on_the_page @ read_but_not_listed @ bound_by_this_suite)
    Harness.windtrap_vars

let () =
  reg "usage line" @@ fun () ->
  equal ~msg:"usage is one line with the basename" string
    "usage: mytests.exe [OPTIONS] [PATTERN...]"
    (Cli.usage ~prog:"/some/path/mytests.exe")

(* Resolution: defaults *)

let settings parsed =
  match Cli.settings parsed with
  | Ok settings -> settings
  | Error error ->
      is_true ~msg:"settings succeeds" false;
      Printf.printf "  settings error: %s\n%!" (Cli.error_message error);
      Run.default_config ()

let resolve = settings

let () =
  reg "resolution defaults" @@ fun () ->
  clear_env ();
  let config = resolve Cli.empty in
  is_true ~msg:"default: no filters"
    (config.Run.filter = [] && config.Run.exclude = []);
  is_true ~msg:"default: no tags"
    (config.Run.tags = [] && config.Run.exclude_tags = []);
  is_true ~msg:"default: flags off"
    ((not config.Run.failed_only)
    && (not config.Run.bail) && (not config.Run.stream)
    && not config.Run.allow_focus);
  is_true ~msg:"default: baselines are checked"
    (config.Run.baseline = Baseline.Check);
  is_true ~msg:"default: no timeout/prop-count"
    (config.Run.timeout = None && config.Run.prop_count = None);
  is_true ~msg:"default: no JUnit report" (config.Run.junit = None);
  is_true ~msg:"default: color auto" (config.Run.color = Os.Auto);
  is_true ~msg:"default: the slow threshold is one second"
    (config.Run.slow_threshold = 1.0);
  is_true ~msg:"default: compact" (not config.Run.verbose);
  is_true ~msg:"default: not a mutation run"
    (config.Run.mutation = Run.No_mutation);
  is_true ~msg:"default: log dir non-empty"
    (String.length config.Run.log_dir > 0)

(* Resolution: precedence *)

let () =
  reg "resolution precedence: CLI > env" @@ fun () ->
  clear_env ();
  setenv "WINDTRAP_FILTER" (Some "envpat");
  let config = resolve Cli.empty in
  is_true ~msg:"env fills an absent flag" (config.Run.filter = [ "envpat" ]);
  let config = resolve { Cli.empty with Cli.filter = [ "clipat" ] } in
  is_true ~msg:"CLI beats env" (config.Run.filter = [ "clipat" ])

(* The patterns repeat as the tags do, but their mirrors hold one pattern,
   so the command line's patterns replace the mirror's instead of adding
   to it. *)
let () =
  reg "patterns: the command line replaces the mirror" @@ fun () ->
  clear_env ();
  setenv "WINDTRAP_FILTER" (Some " a, b ");
  setenv "WINDTRAP_EXCLUDE" (Some "c,d");
  let config = resolve Cli.empty in
  equal ~msg:"a comma is part of the filter pattern" (list string) [ "a, b" ]
    config.Run.filter;
  equal ~msg:"and of the exclusion pattern" (list string) [ "c,d" ]
    config.Run.exclude;
  let config =
    resolve { Cli.empty with Cli.filter = [ "x"; "y" ]; exclude = [ "z" ] }
  in
  equal ~msg:"the command line's filter patterns, the mirror's dropped"
    (list string) [ "x"; "y" ] config.Run.filter;
  equal ~msg:"likewise for the exclusion" (list string) [ "z" ]
    config.Run.exclude

let () =
  reg "tags are additive across layers" @@ fun () ->
  clear_env ();
  setenv "WINDTRAP_TAG" (Some "e1, e2");
  setenv "WINDTRAP_EXCLUDE_TAG" (Some "x1 ,, x2 ");
  let config =
    resolve { Cli.empty with Cli.tags = [ "c" ]; exclude_tags = [ "xc" ] }
  in
  is_true ~msg:"tags are additive across layers, the CLI's first"
    (config.Run.tags = [ "c"; "e1"; "e2" ]);
  is_true ~msg:"exclude tags are additive too, commas split and trimmed"
    (config.Run.exclude_tags = [ "xc"; "x1"; "x2" ])

(* Resolution: the two reading rules *)

let () =
  reg "reading rules: a plain value is one token, trimmed" @@ fun () ->
  clear_env ();
  setenv "WINDTRAP_FILTER" (Some "  parser ");
  setenv "WINDTRAP_SHARD" (Some " 2/4 ");
  setenv "WINDTRAP_PROP_COUNT" (Some " 12 ");
  setenv "WINDTRAP_JUNIT" (Some " out.xml ");
  let s = settings Cli.empty in
  is_true ~msg:"a pattern is trimmed" (s.Run.filter = [ "parser" ]);
  is_true ~msg:"a shard is trimmed" (s.Run.shard = Some (2, 4));
  is_true ~msg:"a count is trimmed" (s.Run.prop_count = Some 12);
  is_true ~msg:"a path is trimmed"
    (s.Run.junit = Some (Filename.concat (Os.project_root ()) "out.xml"))

let () =
  reg "reading rules: a valueless flag's mirror is a boolean" @@ fun () ->
  clear_env ();
  setenv "WINDTRAP_STREAM" (Some "yes");
  setenv "WINDTRAP_VERBOSE" (Some " OFF ");
  let s = settings Cli.empty in
  is_true ~msg:"a truthy spelling applies the flag" s.Run.stream;
  is_true ~msg:"a falsy spelling is absence, trimmed and case-insensitively"
    (not s.Run.verbose);
  setenv "WINDTRAP_STREAM" (Some "maybe");
  (match Cli.settings Cli.empty with
  | Error
      (Cli.Invalid_value
         { source = "WINDTRAP_STREAM"; value = "maybe"; expected }) ->
      is_true
        ~msg:"anything else is refused, naming the variable and the vocabulary"
        (contains "1/0" expected)
  | Ok _ | Error _ ->
      is_true
        ~msg:"anything else is refused, naming the variable and the vocabulary"
        false);
  is_true ~msg:"a flag on the command line shadows the bad value, unread"
    (resolve { Cli.empty with Cli.stream = Some true }).Run.stream

(* The two acceptance flags have no mirror: a build action accepts nothing
   through its environment. Neither do the three feedback-loop flags. *)
let () =
  reg "acceptance flags: no mirror, one mode each, never both" @@ fun () ->
  clear_env ();
  setenv "WINDTRAP_UPDATE" (Some "1");
  is_true ~msg:"WINDTRAP_UPDATE is not a mirror"
    ((resolve Cli.empty).Run.baseline = Baseline.Check);
  clear_env ();
  is_true ~msg:"-u resolves to Update"
    ((resolve { Cli.empty with Cli.update = Some true }).Run.baseline
   = Baseline.Update);
  is_true ~msg:"--corrected resolves to Corrected"
    ((resolve { Cli.empty with Cli.corrected = Some true }).Run.baseline
   = Baseline.Corrected);
  expect_ok "--corrected parses" [ "--corrected" ] (fun p ->
      is_true ~msg:"corrected"
        (p.Cli.corrected = Some true && p.Cli.update = None));
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
  setenv "WINDTRAP_BAIL" (Some "1");
  setenv "WINDTRAP_FAILED" (Some "1");
  setenv "WINDTRAP_LIST" (Some "1");
  let config = resolve Cli.empty in
  is_true ~msg:"WINDTRAP_BAIL is not a mirror" (not config.Run.bail);
  is_true ~msg:"WINDTRAP_FAILED is not a mirror" (not config.Run.failed_only);
  is_true ~msg:"-x resolves to bail"
    (resolve { Cli.empty with Cli.bail = Some true }).Run.bail

let () =
  reg "seed precedence and malformed env seeds" @@ fun () ->
  clear_env ();
  setenv "WINDTRAP_SEED" (Some "s1:00000000000000aa");
  let config = resolve Cli.empty in
  is_true ~msg:"env seed is parsed" (config.Run.seed = 0xaaL);
  setenv "WINDTRAP_SEED" (Some "not-a-seed");
  (match Cli.settings Cli.empty with
  | Error (Cli.Invalid_value { source = "WINDTRAP_SEED"; value; _ }) ->
      is_true ~msg:"malformed env seed errors with its source"
        (value = "not-a-seed")
  | Ok _ | Error _ -> is_true ~msg:"malformed env seed errors" false);
  let config = resolve { Cli.empty with Cli.seed = Some 7L } in
  is_true ~msg:"a CLI seed leaves a malformed env seed unread"
    (config.Run.seed = 7L)

let () =
  reg "env-only settings" @@ fun () ->
  clear_env ();
  setenv "WINDTRAP_STREAM" (Some "1");
  setenv "WINDTRAP_TIMEOUT" (Some "1.5");
  setenv "WINDTRAP_PROP_COUNT" (Some "7");
  setenv "WINDTRAP_EXCLUDE" (Some "skipme");
  let config = resolve Cli.empty in
  is_true ~msg:"WINDTRAP_STREAM" config.Run.stream;
  is_true ~msg:"WINDTRAP_TIMEOUT" (config.Run.timeout = Some 1.5);
  is_true ~msg:"WINDTRAP_PROP_COUNT" (config.Run.prop_count = Some 7);
  is_true ~msg:"WINDTRAP_EXCLUDE" (config.Run.exclude = [ "skipme" ])

(* The mirrors that only existed as flags. Under `dune runtest` the mirrors
   *are* the CLI, so a flag without one is a documented feature no dune user
   can reach (`--junit`, which the CI guide recommends, most of all). *)
let () =
  reg "env-only settings: the CI mirrors" @@ fun () ->
  clear_env ();
  setenv "WINDTRAP_JUNIT" (Some "/reports/junit.xml");
  setenv "WINDTRAP_OUTPUT" (Some "/custom-logs");
  let config = resolve Cli.empty in
  is_true ~msg:"WINDTRAP_JUNIT" (config.Run.junit = Some "/reports/junit.xml");
  is_true ~msg:"WINDTRAP_OUTPUT" (config.Run.log_dir = "/custom-logs")

(* A mirror reaches every stanza of a project, each run from its own build
   directory, so its relative path is read from the project root; the
   command line's is read from the working directory. Both are made
   absolute before a test can chdir. *)
let () =
  reg "a relative path is read from the project root or the working directory"
  @@ fun () ->
  clear_env ();
  setenv "WINDTRAP_PROJECT_ROOT" (Some "/somewhere/project");
  setenv "WINDTRAP_JUNIT" (Some "_build/junit");
  setenv "WINDTRAP_OUTPUT" (Some "logs");
  let config = resolve Cli.empty in
  let root = "/somewhere/project" in
  equal ~msg:"WINDTRAP_JUNIT, from the project root" (option string)
    (Some (Filename.concat root "_build/junit"))
    config.Run.junit;
  equal ~msg:"WINDTRAP_OUTPUT, from the project root" string
    (Filename.concat root "logs")
    config.Run.log_dir;
  let config =
    resolve { Cli.empty with Cli.junit = Some "out"; log_dir = Some "logs" }
  in
  let cwd = Sys.getcwd () in
  equal ~msg:"--junit, from the working directory" (option string)
    (Some (Filename.concat cwd "out"))
    config.Run.junit;
  equal ~msg:"-o, from the working directory" string
    (Filename.concat cwd "logs")
    config.Run.log_dir

let () =
  reg "the mirrors lose to their flags" @@ fun () ->
  clear_env ();
  setenv "WINDTRAP_PROP_COUNT" (Some "3");
  setenv "WINDTRAP_JUNIT" (Some "/from-env.xml");
  let cli =
    { Cli.empty with Cli.prop_count = Some 1; junit = Some "/from-cli.xml" }
  in
  is_true ~msg:"flag beats WINDTRAP_PROP_COUNT"
    ((resolve cli).Run.prop_count = Some 1);
  is_true ~msg:"flag beats WINDTRAP_JUNIT"
    ((settings cli).Run.junit = Some "/from-cli.xml");
  (* A malformed mirror is a usage error naming the *variable*, not a
     silent default. *)
  clear_env ();
  setenv "WINDTRAP_PROP_COUNT" (Some "0");
  (match Cli.settings Cli.empty with
  | Ok _ -> is_true ~msg:"WINDTRAP_PROP_COUNT=0 is rejected" false
  | Error e ->
      is_true ~msg:"the error names the variable, not the flag"
        (contains "WINDTRAP_PROP_COUNT" (Cli.error_message e)));
  (* A losing layer stays unread: a valid flag shadows a malformed mirror. *)
  setenv "WINDTRAP_PROP_COUNT" (Some "not-a-number");
  match Cli.settings { Cli.empty with Cli.prop_count = Some 2 } with
  | Ok s ->
      is_true ~msg:"a valid flag shadows a malformed mirror"
        (s.Run.prop_count = Some 2)
  | Error e ->
      is_true
        ~msg:("malformed mirror leaked past the flag: " ^ Cli.error_message e)
        false

(* The selection is the environment's only when the command line selects
   nothing: one selection flag typed makes an empty selection a typo
   again. *)
let () =
  reg "a selection is broadcast when the mirrors alone give it" @@ fun () ->
  let broadcast cli = (resolve cli).Run.broadcast.Run.selection in
  clear_env ();
  is_false ~msg:"no selection at all" (broadcast Cli.empty);
  List.iter
    (fun (var, value) ->
      clear_env ();
      setenv var (Some value);
      is_true ~msg:var (broadcast Cli.empty))
    [
      ("WINDTRAP_FILTER", "parse");
      ("WINDTRAP_EXCLUDE", "slow");
      ("WINDTRAP_TAG", "gpu");
      ("WINDTRAP_EXCLUDE_TAG", "gpu");
      ("WINDTRAP_SHARD", "1/2");
    ];
  clear_env ();
  setenv "WINDTRAP_TAG" (Some "gpu");
  List.iter
    (fun (flag, cli) -> is_false ~msg:("beside " ^ flag) (broadcast cli))
    [
      ("-f", { Cli.empty with Cli.filter = [ "parse" ] });
      ("-e", { Cli.empty with Cli.exclude = [ "slow" ] });
      ("--tag", { Cli.empty with Cli.tags = [ "cpu" ] });
      ("--exclude-tag", { Cli.empty with Cli.exclude_tags = [ "cpu" ] });
      ("--shard", { Cli.empty with Cli.shard = Some (1, 2) });
      ("--failed", { Cli.empty with Cli.failed_only = Some true });
    ];
  clear_env ();
  is_false ~msg:"a command-line selection alone"
    (broadcast { Cli.empty with Cli.filter = [ "parse" ] })

let () =
  reg "a mutation run is broadcast when WINDTRAP_MUTATE alone asks for it"
  @@ fun () ->
  let broadcast cli = (resolve cli).Run.broadcast.Run.mutate in
  clear_env ();
  is_false ~msg:"no mutation run" (broadcast Cli.empty);
  is_false ~msg:"--mutate" (broadcast { Cli.empty with Cli.mutate = Some [] });
  setenv "WINDTRAP_MUTATE" (Some "1");
  is_true ~msg:"WINDTRAP_MUTATE=1" (broadcast Cli.empty);
  is_false ~msg:"--mutate beside WINDTRAP_MUTATE"
    (broadcast { Cli.empty with Cli.mutate = Some [ "lib/" ] });
  setenv "WINDTRAP_MUTATE" (Some "0");
  is_false ~msg:"WINDTRAP_MUTATE=0" (broadcast Cli.empty);
  clear_env ();
  setenv "WINDTRAP_MUTATE_ARM" (Some "lib/a.ml:1:0:add");
  is_false ~msg:"WINDTRAP_MUTATE_ARM" (broadcast Cli.empty)

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
  is_true ~msg:"parsed booleans and values land in the config"
    (config.Run.stream && config.Run.bail
    (* Absolutized at resolve time so a test that chdirs cannot move the
       run's logs; the relative spelling is still what it ends with. *)
    && Filename.is_relative config.Run.log_dir = false
    && Filename.basename config.Run.log_dir = "custom-logs")

(* Resolution: a mirror is validated by its flag's own parser *)

let () =
  reg "numeric mirrors are validated by their flag's parser" @@ fun () ->
  clear_env ();
  setenv "WINDTRAP_TIMEOUT" (Some "-5");
  (match Cli.settings Cli.empty with
  | Error (Cli.Invalid_value { source = "WINDTRAP_TIMEOUT"; value = "-5"; _ })
    ->
      is_true ~msg:"a negative env timeout errors with its source" true
  | Ok _ | Error _ ->
      is_true ~msg:"a negative env timeout errors with its source" false);
  let config = resolve { Cli.empty with Cli.timeout = Some 1.0 } in
  is_true ~msg:"a valid CLI timeout shadows the bad env value"
    (config.Run.timeout = Some 1.0);
  clear_env ();
  setenv "WINDTRAP_PROP_COUNT" (Some "0");
  (match Cli.settings Cli.empty with
  | Error (Cli.Invalid_value { source = "WINDTRAP_PROP_COUNT"; value = "0"; _ })
    ->
      is_true ~msg:"a zero env prop count errors with its source" true
  | Ok _ | Error _ ->
      is_true ~msg:"a zero env prop count errors with its source" false);
  clear_env ();
  (* Malformed mirror tokens error like their flags: same knob,
     same garbage, same loud refusal in every layer. *)
  setenv "WINDTRAP_PROP_COUNT" (Some "1O0");
  (match Cli.settings Cli.empty with
  | Error
      (Cli.Invalid_value { source = "WINDTRAP_PROP_COUNT"; value = "1O0"; _ })
    ->
      is_true ~msg:"a malformed winning env prop count errors with its source"
        true
  | Ok _ | Error _ ->
      is_true ~msg:"a malformed winning env prop count errors with its source"
        false);
  let config = resolve { Cli.empty with Cli.prop_count = Some 50 } in
  is_true ~msg:"a valid CLI prop count leaves a malformed mirror unread"
    (config.Run.prop_count = Some 50);
  clear_env ();
  setenv "WINDTRAP_TIMEOUT" (Some "banana");
  (match Cli.settings Cli.empty with
  | Error
      (Cli.Invalid_value { source = "WINDTRAP_TIMEOUT"; value = "banana"; _ })
    ->
      is_true ~msg:"a malformed winning env timeout errors with its source" true
  | Ok _ | Error _ ->
      is_true ~msg:"a malformed winning env timeout errors with its source"
        false);
  let config = resolve { Cli.empty with Cli.timeout = Some 2.0 } in
  is_true ~msg:"a valid CLI timeout leaves a malformed mirror unread"
    (config.Run.timeout = Some 2.0);
  clear_env ();
  (* A mirror quotes the token as typed, exactly as its flag does: "-5.0"
     is what the user wrote and "-5.0" is what the message must show, not
     the shortest spelling of the float it parsed to. *)
  setenv "WINDTRAP_TIMEOUT" (Some "-5.0");
  (match Cli.settings Cli.empty with
  | Error (Cli.Invalid_value { source = "WINDTRAP_TIMEOUT"; value; _ }) ->
      is_true ~msg:"a mirror quotes the token as written" (value = "-5.0")
  | Ok _ | Error _ -> is_true ~msg:"a mirror quotes the token as written" false);
  setenv "WINDTRAP_TIMEOUT" (Some "1e400");
  match Cli.settings Cli.empty with
  | Error (Cli.Invalid_value { source = "WINDTRAP_TIMEOUT"; value; _ }) ->
      is_true ~msg:"an overflowing token is quoted, not printed as 'inf'"
        (value = "1e400")
  | Ok _ | Error _ ->
      is_true ~msg:"an overflowing token is quoted, not printed as 'inf'" false

(* WINDTRAP_COLOR is a mirror like every other: --color's parser reads
   it, so a word the flag refuses is refused here too, never read as
   auto. The flagless commands read the variable through the same
   parser. *)
let () =
  reg "color precedence, and the mirror refuses what the flag refuses"
  @@ fun () ->
  clear_env ();
  setenv "WINDTRAP_COLOR" (Some "never");
  let render = settings Cli.empty in
  is_true ~msg:"WINDTRAP_COLOR fills the default" (render.Run.color = Os.Never);
  let render = settings { Cli.empty with Cli.color = Some Os.Always } in
  is_true ~msg:"--color beats WINDTRAP_COLOR" (render.Run.color = Os.Always);
  setenv "WINDTRAP_COLOR" (Some " Never ");
  is_true ~msg:"the mirror is trimmed and case-insensitive, as --color is"
    ((settings Cli.empty).Run.color = Os.Never);
  is_true ~msg:"color_mode reads the same variable the same way"
    (Cli.color_mode () = Ok Os.Never);
  setenv "WINDTRAP_COLOR" (Some "sometimes");
  (match Cli.settings Cli.empty with
  | Error
      (Cli.Invalid_value
         { source = "WINDTRAP_COLOR"; value = "sometimes"; expected }) ->
      is_true ~msg:"a bad WINDTRAP_COLOR is refused with --color's wording"
        (expected = "always, never or auto")
  | Ok _ | Error _ ->
      is_true ~msg:"a bad WINDTRAP_COLOR is refused with --color's wording"
        false);
  (match Cli.color_mode () with
  | Error
      (Cli.Invalid_value { source = "WINDTRAP_COLOR"; value = "sometimes"; _ })
    ->
      is_true ~msg:"color_mode refuses it too, naming the variable" true
  | Ok _ | Error _ ->
      is_true ~msg:"color_mode refuses it too, naming the variable" false);
  is_true ~msg:"a --color on the command line shadows the bad value, unread"
    ((settings { Cli.empty with Cli.color = Some Os.Auto }).Run.color = Os.Auto);
  clear_env ();
  is_true ~msg:"color_mode defaults to auto" (Cli.color_mode () = Ok Os.Auto)

(* Resolution: the mutation switches *)

let () =
  reg "mutation switches: the flags, their mirrors, and both at once"
  @@ fun () ->
  clear_env ();
  let mutation cli = (settings cli).Run.mutation in
  let parsed args =
    match parse args with Ok p -> p | Error e -> fail (Cli.error_message e)
  in
  is_true ~msg:"the bare flag surveys every mutant"
    (mutation (parsed [ "--mutate" ]) = Run.Loop []);
  is_true ~msg:"a value is the comma-separated prefixes"
    (mutation (parsed [ "--mutate=lib/a.ml, lib/b.ml" ])
    = Run.Loop [ "lib/a.ml"; "lib/b.ml" ]);
  is_true ~msg:"the bare flag never consumes the next argument"
    ((parsed [ "--mutate"; "lib/a.ml" ]).Cli.filter = [ "lib/a.ml" ]);
  is_true ~msg:"--arm takes the identifier, unparsed"
    (mutation (parsed [ "--arm"; "lib/a.ml:9:12:add" ])
    = Run.Armed "lib/a.ml:9:12:add");
  is_true ~msg:"--arm=ID spells the same"
    (mutation (parsed [ "--arm=x" ]) = Run.Armed "x");
  (* WINDTRAP_MUTATE reads both ways: a boolean is the bare flag or its
     absence, anything else the prefixes, so a CI recipe's `1` and a
     developer's file name both keep working. *)
  setenv "WINDTRAP_MUTATE" (Some "1");
  is_true ~msg:"WINDTRAP_MUTATE=1 is the bare flag"
    (mutation Cli.empty = Run.Loop []);
  setenv "WINDTRAP_MUTATE" (Some "off");
  is_true ~msg:"a falsy WINDTRAP_MUTATE is no mutation run"
    (mutation Cli.empty = Run.No_mutation);
  setenv "WINDTRAP_MUTATE" (Some " lib/calc.ml ");
  is_true ~msg:"any other WINDTRAP_MUTATE is the prefixes, trimmed"
    (mutation Cli.empty = Run.Loop [ "lib/calc.ml" ]);
  setenv "WINDTRAP_MUTATE" (Some "lib/a.ml,lib/b.ml");
  is_true ~msg:"and splits on commas as the flag does"
    (mutation Cli.empty = Run.Loop [ "lib/a.ml"; "lib/b.ml" ]);
  is_true ~msg:"the flag shadows the mirror"
    (mutation (parsed [ "--mutate=lib/x.ml" ]) = Run.Loop [ "lib/x.ml" ]);
  clear_env ();
  setenv "WINDTRAP_MUTATE_ARM" (Some "lib/a.ml:9:12:add");
  is_true ~msg:"WINDTRAP_MUTATE_ARM mirrors --arm"
    (mutation Cli.empty = Run.Armed "lib/a.ml:9:12:add");
  (* Both at once, whichever layer each arrived by: the loop arms each
     mutant itself, so an armed parent would mutate its own dry run. *)
  let refused cli =
    match Cli.settings cli with
    | Error (Cli.Incompatible_flags ("--mutate", "--arm")) -> true
    | Ok _ | Error _ -> false
  in
  is_true ~msg:"--arm with WINDTRAP_MUTATE_ARM's sibling --mutate is refused"
    (refused (parsed [ "--mutate" ]));
  clear_env ();
  is_true ~msg:"--mutate and --arm on one command line are refused"
    (refused (parsed [ "--mutate"; "--arm"; "x" ]));
  setenv "WINDTRAP_MUTATE" (Some "1");
  is_true ~msg:"WINDTRAP_MUTATE=1 with --arm is refused"
    (refused (parsed [ "--arm"; "x" ]));
  is_true ~msg:"the message names both flags"
    (contains "'--mutate' and '--arm' cannot be combined"
       (Cli.error_message (Cli.Incompatible_flags ("--mutate", "--arm"))))

(* Resolution: --slow-threshold and WINDTRAP_SLOW_THRESHOLD *)

let () =
  reg "--slow-threshold resolution" @@ fun () ->
  clear_env ();
  let render = settings Cli.empty in
  is_true ~msg:"the built-in default is one second"
    (render.Run.slow_threshold = 1.0);
  setenv "WINDTRAP_SLOW_THRESHOLD" (Some "3");
  let render = settings Cli.empty in
  is_true ~msg:"WINDTRAP_SLOW_THRESHOLD fills an absent flag"
    (render.Run.slow_threshold = 3.0);
  let render = settings { Cli.empty with Cli.slow_threshold = Some 0.5 } in
  is_true ~msg:"--slow-threshold beats the env mirror"
    (render.Run.slow_threshold = 0.5);
  setenv "WINDTRAP_SLOW_THRESHOLD" (Some "-2");
  (match Cli.settings Cli.empty with
  | Error
      (Cli.Invalid_value { source = "WINDTRAP_SLOW_THRESHOLD"; value = "-2"; _ })
    ->
      is_true ~msg:"a negative winning env threshold errors with its source"
        true
  | Ok _ | Error _ ->
      is_true ~msg:"a negative winning env threshold errors with its source"
        false);
  let render = settings { Cli.empty with Cli.slow_threshold = Some 1.5 } in
  is_true ~msg:"a CLI threshold shadows the bad env value"
    (render.Run.slow_threshold = 1.5);
  setenv "WINDTRAP_SLOW_THRESHOLD" (Some "soon");
  match Cli.settings Cli.empty with
  | Error
      (Cli.Invalid_value
         { source = "WINDTRAP_SLOW_THRESHOLD"; value = "soon"; _ }) ->
      is_true
        ~msg:"a malformed winning env threshold errors, as WINDTRAP_TIMEOUT's"
        true
  | Ok _ | Error _ ->
      is_true
        ~msg:"a malformed winning env threshold errors, as WINDTRAP_TIMEOUT's"
        false

(* Resolution: --shard and WINDTRAP_SHARD *)

let () =
  reg "--shard resolution" @@ fun () ->
  clear_env ();
  let config = resolve Cli.empty in
  is_true ~msg:"no layer means no shard" (config.Run.shard = None);
  setenv "WINDTRAP_SHARD" (Some "2/3");
  let config = resolve Cli.empty in
  is_true ~msg:"WINDTRAP_SHARD fills an absent flag"
    (config.Run.shard = Some (2, 3));
  let config = resolve { Cli.empty with Cli.shard = Some (1, 2) } in
  is_true ~msg:"--shard beats WINDTRAP_SHARD" (config.Run.shard = Some (1, 2));
  setenv "WINDTRAP_SHARD" (Some "9/2");
  (match Cli.settings Cli.empty with
  | Error (Cli.Invalid_value { source = "WINDTRAP_SHARD"; value = "9/2"; _ }) ->
      is_true ~msg:"a malformed winning env shard errors with its source" true
  | Ok _ | Error _ ->
      is_true ~msg:"a malformed winning env shard errors with its source" false);
  let config = resolve { Cli.empty with Cli.shard = Some (1, 2) } in
  is_true ~msg:"a CLI shard leaves a malformed env shard unread"
    (config.Run.shard = Some (1, 2))

(* Parsing: the edges of the argument grammar *)

let () =
  reg "an argument of two bytes that starts with a dash is a flag" @@ fun () ->
  List.iter
    (fun (arg, typed) ->
      expect_error (Printf.sprintf "%s is an unknown flag" arg) [ arg ]
        (function
        | Cli.Unknown_flag f -> f = typed
        | _ -> false))
    [ ("-1", "-1"); ("-xv", "-xv"); ("--bogus=1", "--bogus") ]

let () =
  reg "a short flag takes no inline value" @@ fun () ->
  (* The payload is the argument whole: without its value, [-f] would name
     a flag that exists. *)
  List.iter
    (fun arg ->
      expect_error (Printf.sprintf "%s is an unknown flag" arg) [ arg ]
        (function
        | Cli.Unknown_flag f -> f = arg
        | _ -> false))
    [ "-f=x"; "-fx" ]

let () =
  reg "a flag takes the next argument whatever it looks like" @@ fun () ->
  expect_ok "-f --verbose" [ "-f"; "--verbose" ] (fun p ->
      equal ~msg:"the filter is the flag-shaped word" (list string)
        [ "--verbose" ] p.Cli.filter;
      equal ~msg:"and --verbose was not read as a flag" (option bool) None
        p.Cli.verbose)

let () =
  reg "an error before --help wins over it" @@ fun () ->
  expect_error "an unknown flag before --help" [ "--bogus"; "--help" ] (function
    | Cli.Unknown_flag "--bogus" -> true
    | _ -> false);
  expect_error "-u --corrected --help" [ "-u"; "--corrected"; "--help" ]
    (function
    | Cli.Incompatible_flags ("-u", "--corrected") -> true
    | _ -> false)

let () =
  reg "the acceptance refusal names -u first in either order" @@ fun () ->
  expect_error "--corrected --update" [ "--corrected"; "--update" ] (function
    | Cli.Incompatible_flags ("-u", "--corrected") -> true
    | _ -> false)

let () =
  reg "parse reads no environment" @@ fun () ->
  clear_env ();
  setenv "WINDTRAP_FILTER" (Some "from-env");
  setenv "WINDTRAP_STREAM" (Some "1");
  is_true ~msg:"no argument is the empty record, the mirrors set"
    (parse [] = Ok Cli.empty)

(* Command lines made of whole arguments: every flag that takes no value in
   both spellings, value flags with a good value, and the misspellings and
   separators, so most vectors parse and some do not. Whatever a vector
   holds, [parse] returns. *)
let argv_chunks =
  List.map
    (fun a -> [ a ])
    [
      "--failed";
      "-l";
      "--list";
      "-x";
      "--fail-fast";
      "-s";
      "--stream";
      "-u";
      "--update";
      "--corrected";
      "-v";
      "--verbose";
      "--mutate";
    ]
  @ [
      [ "-f"; "p" ];
      [ "--tag=a" ];
      [ "--shard"; "2/4" ];
      [ "--seed"; "s1:00000000000000ff" ];
      [ "--color"; "never" ];
      [ "--list=1" ];
      [ "--prop-count"; "0" ];
      [ "-xv" ];
      [ "-fx" ];
      [ "-1" ];
      [ "--" ];
      [ "-h" ];
      [ "" ];
    ]

let () =
  registered :=
    prop "parse never raises and never gives Some false"
      Gen.(map List.concat (list (of_list argv_chunks)))
      (fun args ->
        match parse args with
        | Error _ -> ()
        | Ok p ->
            let flags =
              [
                p.Cli.failed_only;
                p.Cli.list_only;
                p.Cli.bail;
                p.Cli.stream;
                p.Cli.update;
                p.Cli.corrected;
                p.Cli.verbose;
              ]
            in
            is_true ~msg:"a boolean flag is None or Some true"
              (List.for_all (fun b -> b <> Some false) flags))
    :: !registered

(* Resolution: the order of the mirrors, and the edges of each layer *)

let invalid_source = function
  | Error (Cli.Invalid_value { source; _ }) -> Some source
  | Ok _ | Error _ -> None

let () =
  reg "the mirrors are read in help order and the first error wins" @@ fun () ->
  clear_env ();
  (* Help order is SHARD, STREAM, COLOR; the alphabet puts COLOR first. *)
  setenv "WINDTRAP_COLOR" (Some "sometimes");
  setenv "WINDTRAP_SHARD" (Some "9/2");
  equal ~msg:"the earlier row's variable is named" (option string)
    (Some "WINDTRAP_SHARD")
    (invalid_source (Cli.settings Cli.empty));
  setenv "WINDTRAP_SHARD" None;
  setenv "WINDTRAP_STREAM" (Some "maybe");
  equal ~msg:"then the next bad one in help order" (option string)
    (Some "WINDTRAP_STREAM")
    (invalid_source (Cli.settings Cli.empty))

let () =
  reg "a bad mirror wins over --mutate with --arm" @@ fun () ->
  clear_env ();
  setenv "WINDTRAP_COLOR" (Some "sometimes");
  equal ~msg:"the incompatibility is checked after every mirror" (option string)
    (Some "WINDTRAP_COLOR")
    (invalid_source
       (Cli.settings
          { Cli.empty with Cli.mutate = Some []; arm = Some "lib/a.ml:1:0:add" }))

let () =
  reg "WINDTRAP_CORRECTED is not a mirror" @@ fun () ->
  clear_env ();
  setenv "WINDTRAP_CORRECTED" (Some "1");
  is_true ~msg:"the baselines are checked"
    ((resolve Cli.empty).Run.baseline = Baseline.Check)

let () =
  reg "a relative -o is kept as given when the directory cannot be read"
  @@ fun () ->
  if Sys.win32 then skip ~reason:"POSIX only" ();
  clear_env ();
  let gone = Filename.concat (temp_dir ()) "gone" in
  Unix.mkdir gone 0o700;
  chdir gone;
  Unix.rmdir gone;
  (match Sys.getcwd () with
  | _ -> skip ~reason:"this system reads a removed working directory" ()
  | exception Sys_error _ -> ());
  equal ~msg:"the log dir is the relative spelling" string "logs"
    (resolve { Cli.empty with Cli.log_dir = Some "logs" }).Run.log_dir

let () =
  reg "settings: github from the environment, the mirrors' spelling"
  @@ fun () ->
  clear_env ();
  let c = resolve Cli.empty in
  is_true ~msg:"outside GitHub Actions" (not c.Run.github);
  is_true ~msg:"commands are spelled with the mirrors"
    (c.Run.invocation = `Mirrors);
  setenv "CI" (Some "true");
  setenv "GITHUB_ACTIONS" (Some "true");
  is_true ~msg:"under GitHub Actions" (resolve Cli.empty).Run.github

let () =
  reg "settings draws a fresh seed on every call" @@ fun () ->
  clear_env ();
  let a = (resolve Cli.empty).Run.seed and b = (resolve Cli.empty).Run.seed in
  is_true ~msg:"two calls, two seeds" (a <> b)

(* Suite *)

let tests = List.rev !registered
let () = exit @@ Windtrap.run "cli" tests
