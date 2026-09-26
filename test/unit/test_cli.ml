(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A parsed record and a configuration are compared as rows of the fields a
   claim changes: a record as the flags it holds, a configuration as the
   fields where it departs from [Run.default_config ()], which [settings]
   falls back on. The seed is left out of the row, since the default draws
   one at random. *)

open Windtrap
module Baseline = Windtrap.Private.Baseline
module Cli = Windtrap.Private.Cli
module Os = Windtrap.Private.Os
module Run = Windtrap.Private.Run
module Seed = Windtrap.Private.Seed

let strf = Printf.sprintf
let words items = "[" ^ String.concat "; " items ^ "]"
let joined = function [] -> "no argument" | args -> String.concat " " args

let color_name = function
  | Os.Always -> "always"
  | Os.Never -> "never"
  | Os.Auto -> "auto"

let error_row = function
  | Cli.Unknown_flag flag -> "unknown " ^ flag
  | Cli.Missing_value flag -> "missing value " ^ flag
  | Cli.Invalid_value { source; value; expected = _ } ->
      strf "invalid %s %S" source value
  | Cli.Incompatible_flags (first, second) ->
      strf "incompatible %s %s" first second

let pp_error ppf e = Format.pp_print_string ppf (error_row e)

let expected_of = function
  | Error (Cli.Invalid_value { expected; _ }) -> Some expected
  | _ -> None

(* A [bool option] prints [false] beside its name, so a [Some false] shows. *)
let flags (p : Cli.parsed) =
  let list name = function [] -> [] | l -> [ name ^ " " ^ words l ] in
  let some name show = function
    | None -> []
    | Some v -> [ name ^ " " ^ show v ]
  in
  let switch name = function
    | None -> []
    | Some true -> [ name ]
    | Some false -> [ name ^ " false" ]
  in
  let bool name b = if b then [ name ] else [] in
  match
    List.concat
      [
        list "filter" p.filter;
        list "exclude" p.exclude;
        list "tags" p.tags;
        list "exclude_tags" p.exclude_tags;
        some "shard" (fun (k, n) -> strf "%d/%d" k n) p.shard;
        switch "failed_only" p.failed_only;
        switch "list_only" p.list_only;
        switch "bail" p.bail;
        switch "stream" p.stream;
        switch "update" p.update;
        switch "corrected" p.corrected;
        some "seed" Seed.to_string p.seed;
        some "timeout" (strf "%g") p.timeout;
        some "slow_threshold" (strf "%g") p.slow_threshold;
        some "prop_count" string_of_int p.prop_count;
        switch "verbose" p.verbose;
        some "junit" Fun.id p.junit;
        some "color" color_name p.color;
        some "log_dir" Fun.id p.log_dir;
        some "mutate" words p.mutate;
        some "arm" Fun.id p.arm;
        bool "help" p.help;
        bool "version" p.version;
      ]
  with
  | [] -> "empty"
  | fields -> String.concat ", " fields

let parsed_argv argv =
  match Cli.parse argv with Ok p -> flags p | Error e -> error_row e

let parse args = Cli.parse (Array.of_list ("windtrap-test" :: args))
let parsed args = parsed_argv (Array.of_list ("windtrap-test" :: args))

let mutation_name = function
  | Run.No_mutation -> "none"
  | Run.Loop prefixes -> "loop " ^ words prefixes
  | Run.Armed id -> "armed " ^ id

let baseline_name = function
  | Baseline.Check -> "check"
  | Baseline.Update -> "update"
  | Baseline.Corrected -> "corrected"

let invocation_name = function
  | `Mirrors -> "mirrors"
  | `Exe cmd -> "exe " ^ cmd

let changes (c : Run.config) =
  let d = Run.default_config () in
  let field name show get =
    match show (get c) with
    | v when String.equal v (show (get d)) -> []
    | "true" -> [ name ]
    | v -> [ name ^ " " ^ v ]
  in
  let opt show = function None -> "none" | Some v -> show v in
  let bool = string_of_bool in
  match
    List.concat
      [
        field "filter" words (fun c -> c.Run.filter);
        field "exclude" words (fun c -> c.Run.exclude);
        field "tags" words (fun c -> c.Run.tags);
        field "exclude_tags" words (fun c -> c.Run.exclude_tags);
        field "shard"
          (opt (fun (k, n) -> strf "%d/%d" k n))
          (fun c -> c.Run.shard);
        field "failed_only" bool (fun c -> c.Run.failed_only);
        field "bail" bool (fun c -> c.Run.bail);
        field "stream" bool (fun c -> c.Run.stream);
        field "baseline" baseline_name (fun c -> c.Run.baseline);
        field "timeout" (opt (strf "%g")) (fun c -> c.Run.timeout);
        field "prop_count" (opt string_of_int) (fun c -> c.Run.prop_count);
        field "log_dir" Fun.id (fun c -> c.Run.log_dir);
        field "allow_focus" bool (fun c -> c.Run.allow_focus);
        field "color" color_name (fun c -> c.Run.color);
        field "slow_threshold" (strf "%g") (fun c -> c.Run.slow_threshold);
        field "verbose" bool (fun c -> c.Run.verbose);
        field "junit" (opt Fun.id) (fun c -> c.Run.junit);
        field "mutation" mutation_name (fun c -> c.Run.mutation);
        field "github" bool (fun c -> c.Run.github);
        field "invocation" invocation_name (fun c -> c.Run.invocation);
        field "broadcast selection" bool (fun c ->
            c.Run.broadcast.Run.selection);
        field "broadcast mutate" bool (fun c -> c.Run.broadcast.Run.mutate);
      ]
  with
  | [] -> "defaults"
  | fields -> String.concat ", " fields

(* [settings] reads every [WINDTRAP_*] variable, [CI] and [GITHUB_ACTIONS], and
   [INSIDE_DUNE] through the defaults. Each is unset, then [env] is bound; the
   runner restores them when the test ends. *)
let stated env =
  let windtrap =
    List.filter_map
      (fun binding ->
        match String.index_opt binding '=' with
        | Some i when String.starts_with ~prefix:"WINDTRAP_" binding ->
            Some (String.sub binding 0 i)
        | Some _ | None -> None)
      (Array.to_list (Unix.environment ()))
  in
  List.iter
    (fun var -> setenv var None)
    ([ "CI"; "GITHUB_ACTIONS"; "INSIDE_DUNE" ] @ windtrap);
  List.iter (fun (var, value) -> setenv var (Some value)) env

let settings ?(env = []) p =
  stated env;
  Cli.settings p

let settled ?env p =
  match settings ?env p with Ok c -> changes c | Error e -> error_row e

let resolved ?env args =
  match parse args with Ok p -> settled ?env p | Error e -> error_row e

let config ?env args =
  require_ok ~pp:pp_error (settings ?env (require_ok ~pp:pp_error (parse args)))

(* Mirrors *)

let mirror_rows name rows =
  cases name
    ~name:(fun (var, value, _) -> strf "%s=%S" var value)
    rows
    (fun (var, value, row) ->
      equal string row (resolved ~env:[ (var, value) ] []))

(* A mirror's value is trimmed and a flag's is not, so no row holds a space. *)
let refused_alike (var, flag, value) =
  let expected = require_match expected_of (parse [ flag; value ]) in
  let refusal = settings ~env:[ (var, value) ] Cli.empty in
  equal (pair string string)
    (strf "invalid %s %S" var value, expected)
    ( Result.fold ~ok:changes ~error:error_row refusal,
      require_match expected_of refusal )

let boolean_wording () =
  let refusal = settings ~env:[ ("WINDTRAP_STREAM", "maybe") ] Cli.empty in
  equal string Os.bool_expected (require_match expected_of refusal)

let mirrors =
  group "Mirrors"
    [
      mirror_rows
        "a mirror is WINDTRAP_ and its long flag in capitals, with _ for -, \
         and gives that flag's field"
        [
          ("WINDTRAP_FILTER", "parse", "filter [parse], broadcast selection");
          ("WINDTRAP_EXCLUDE", "slow", "exclude [slow], broadcast selection");
          ("WINDTRAP_TAG", "gpu", "tags [gpu], broadcast selection");
          ( "WINDTRAP_EXCLUDE_TAG",
            "gpu",
            "exclude_tags [gpu], broadcast selection" );
          ("WINDTRAP_SHARD", "2/3", "shard 2/3, broadcast selection");
          ("WINDTRAP_TIMEOUT", "1.5", "timeout 1.5");
          ("WINDTRAP_SLOW_THRESHOLD", "3", "slow_threshold 3");
          ("WINDTRAP_PROP_COUNT", "7", "prop_count 7");
          ("WINDTRAP_STREAM", "1", "stream");
          ("WINDTRAP_VERBOSE", "1", "verbose");
          ("WINDTRAP_JUNIT", "/reports/junit.xml", "junit /reports/junit.xml");
          ("WINDTRAP_COLOR", "never", "color never");
          ("WINDTRAP_OUTPUT", "/custom-logs", "log_dir /custom-logs");
        ];
      test "the mirror of --arm is WINDTRAP_MUTATE_ARM" (fun () ->
          equal string "mutation armed lib/a.ml:1:0:add"
            (resolved ~env:[ ("WINDTRAP_MUTATE_ARM", "lib/a.ml:1:0:add") ] []));
      cases
        "a mirror refuses what its flag refuses, naming the variable and the \
         value as typed, in the flag's words"
        ~name:(fun (var, _, value) -> strf "%s=%S" var value)
        [
          ("WINDTRAP_SHARD", "--shard", "9/2");
          ("WINDTRAP_TIMEOUT", "--timeout", "-5");
          ("WINDTRAP_TIMEOUT", "--timeout", "banana");
          ("WINDTRAP_TIMEOUT", "--timeout", "-5.0");
          ("WINDTRAP_TIMEOUT", "--timeout", "1e400");
          ("WINDTRAP_SLOW_THRESHOLD", "--slow-threshold", "-2");
          ("WINDTRAP_SLOW_THRESHOLD", "--slow-threshold", "soon");
          ("WINDTRAP_SEED", "--seed", "not-a-seed");
          ("WINDTRAP_PROP_COUNT", "--prop-count", "0");
          ("WINDTRAP_PROP_COUNT", "--prop-count", "1O0");
          ("WINDTRAP_COLOR", "--color", "sometimes");
        ]
        refused_alike;
      mirror_rows "a mirror set to the empty string counts as unset"
        [
          ("WINDTRAP_FILTER", "", "defaults");
          ("WINDTRAP_TAG", "", "defaults");
          ("WINDTRAP_SHARD", "", "defaults");
          ("WINDTRAP_STREAM", "", "defaults");
          ("WINDTRAP_MUTATE", "", "defaults");
        ];
      mirror_rows
        "the mirror of a flag that takes a value is one token, trimmed, so a \
         comma belongs to a pattern"
        [
          ( "WINDTRAP_FILTER",
            "  parser ",
            "filter [parser], broadcast selection" );
          ("WINDTRAP_FILTER", " a, b ", "filter [a, b], broadcast selection");
          ("WINDTRAP_EXCLUDE", "c,d", "exclude [c,d], broadcast selection");
          ("WINDTRAP_SHARD", " 2/4 ", "shard 2/4, broadcast selection");
          ("WINDTRAP_PROP_COUNT", " 12 ", "prop_count 12");
          ("WINDTRAP_COLOR", " Never ", "color never");
          ("WINDTRAP_JUNIT", " /out.xml ", "junit /out.xml");
        ];
      mirror_rows
        "the mirror of --tag or --exclude-tag is a comma-separated list, \
         trimmed, without empty items"
        [
          ("WINDTRAP_TAG", "e1, e2", "tags [e1; e2], broadcast selection");
          ( "WINDTRAP_EXCLUDE_TAG",
            "x1 ,, x2 ",
            "exclude_tags [x1; x2], broadcast selection" );
          ("WINDTRAP_TAG", " , ", "defaults");
        ];
      mirror_rows
        "the mirror of a flag that takes no value is a boolean, true the flag \
         and false its absence"
        [
          ("WINDTRAP_STREAM", "yes", "stream");
          ("WINDTRAP_VERBOSE", " OFF ", "defaults");
          ("WINDTRAP_STREAM", "maybe", {|invalid WINDTRAP_STREAM "maybe"|});
        ];
      test "a boolean mirror refuses another word in Os.bool_expected's words"
        boolean_wording;
      mirror_rows
        "the mirror of --mutate is the bare flag or its absence for a boolean, \
         and else the prefixes"
        [
          ("WINDTRAP_MUTATE", "1", "mutation loop [], broadcast mutate");
          ("WINDTRAP_MUTATE", "on", "mutation loop [], broadcast mutate");
          ("WINDTRAP_MUTATE", "off", "defaults");
          ("WINDTRAP_MUTATE", "0", "defaults");
          ( "WINDTRAP_MUTATE",
            " lib/calc.ml ",
            "mutation loop [lib/calc.ml], broadcast mutate" );
          ( "WINDTRAP_MUTATE",
            "lib/a.ml,lib/b.ml",
            "mutation loop [lib/a.ml; lib/b.ml], broadcast mutate" );
        ];
      mirror_rows "--failed, -x, -u and --corrected have no mirror"
        [
          ("WINDTRAP_FAILED", "1", "defaults");
          ("WINDTRAP_FAIL_FAST", "1", "defaults");
          ("WINDTRAP_BAIL", "1", "defaults");
          ("WINDTRAP_UPDATE", "1", "defaults");
          ("WINDTRAP_CORRECTED", "1", "defaults");
        ];
    ]

(* Parsed flags *)

let parsed_flags =
  group "Parsed flags"
    [
      test "empty is the record with every flag absent" (fun () ->
          equal string "empty" (flags Cli.empty));
    ]

(* Errors *)

let messages () =
  let constructed =
    [
      Cli.Unknown_flag "--bogus";
      Cli.Unknown_flag "--filtre";
      Cli.Unknown_flag "-Z";
      Cli.Missing_value "--junit";
      Cli.Invalid_value
        { source = "--prop-count"; value = "x"; expected = "an int" };
      Cli.Incompatible_flags ("-u", "--corrected");
      Cli.Incompatible_flags ("--mutate", "--arm");
    ]
  in
  let refused =
    List.map
      (fun args -> require_error (parse args))
      [
        [ "--seed"; "nope" ];
        [ "--shard"; "9/2" ];
        [ "--list=x" ];
        [ "--timeout"; "0" ];
        [ "--slow-threshold"; "-1" ];
        [ "--prop-count"; "0" ];
        [ "--color"; "sometimes" ];
      ]
  in
  let mirror =
    require_error (settings ~env:[ ("WINDTRAP_PROP_COUNT", "0") ] Cli.empty)
  in
  expect
    (String.concat "\n"
       (List.map Cli.error_message (constructed @ refused @ [ mirror ])))
  @@ __POS_OF__
       {|
    unknown option '--bogus'
    unknown option '--filtre'; did you mean '--filter'?
    unknown option '-Z'
    option '--junit' requires an argument
    invalid value 'x' for --prop-count: expected an int
    options '-u' and '--corrected' cannot be combined
    options '--mutate' and '--arm' cannot be combined
    invalid value 'nope' for --seed: expected an s1: token with 16 lowercase hexadecimal digits
    invalid value '9/2' for --shard: expected K/N with 1 <= K <= N (e.g. 2/4)
    invalid value 'x' for --list: expected no argument
    invalid value '0' for --timeout: expected a positive number
    invalid value '-1' for --slow-threshold: expected a non-negative number
    invalid value '0' for --prop-count: expected a positive integer
    invalid value 'sometimes' for --color: expected always, never or auto
    invalid value '0' for WINDTRAP_PROP_COUNT: expected a positive integer
    |}

let suggestion flag =
  match
    String.split_on_char '\'' (Cli.error_message (Cli.Unknown_flag flag))
  with
  | [ _; _; _; name; "?" ] -> Some name
  | _ -> None

let errors =
  group "Errors"
    [
      test "error_message is one sentence that names the flag or the variable"
        messages;
      cases
        "an unknown long flag's message suggests the nearest long flag when \
         one is near"
        ~name:fst
        [
          ("--fliter", Some "--filter");
          ("--colour", Some "--color");
          ("--tags", Some "--tag");
          (* A transposition is one edit, which puts --juint nearer --junit
             than --update. *)
          ("--juint", Some "--junit");
          ("--mutant", Some "--mutate");
          (* Two edits from --verbose and from --version: the first in the
             table wins. *)
          ("--verbon", Some "--verbose");
          (* Near is within a third of the typed length, or two edits. *)
          ("--fxltxr", Some "--filter");
          ("--fxxtxr", None);
          ("--prxx-cxxnt", Some "--prop-count");
          ("--prxx-xxxnt", None);
          ("--lti", None);
          ("--completely-different", None);
        ]
        (fun (typo, near) -> equal (option string) near (suggestion typo));
      test "an unknown short flag's message suggests nothing" (fun () ->
          equal (option string) None (suggestion "-Z"));
    ]

(* Parsing *)

let parse_rows name rows =
  cases name
    ~name:(fun (args, _) -> joined args)
    rows
    (fun (args, row) -> equal string row (parsed args))

let every_flag () =
  equal string
    "filter [pat], exclude [ex], tags [a; b], exclude_tags [c], shard 2/4, \
     failed_only, list_only, bail, stream, update, seed s1:00000000000000ff, \
     timeout 2.5, slow_threshold 3, prop_count 50, verbose, junit out.xml, \
     color never, log_dir logs, mutate [lib/a.ml; lib/b.ml], arm \
     lib/a.ml:1:0:add"
    (parsed
       (String.split_on_char ' '
          "-f pat -e ex --tag a --tag b --exclude-tag c --shard 2/4 --failed \
           -l -x -s -u --seed s1:00000000000000ff --timeout 2.5 \
           --slow-threshold 3 --prop-count 50 -v --junit out.xml --color never \
           -o logs --mutate=lib/a.ml,lib/b.ml --arm lib/a.ml:1:0:add"))

(* Whole arguments: each valueless flag in both spellings, value flags with a
   good value, and misspellings and separators, so that most vectors parse
   and some do not. *)
let argv_chunks =
  List.map
    (fun a -> [ a ])
    (String.split_on_char ' '
       "--failed -l --list -x --fail-fast -s --stream -u --update --corrected \
        -v --verbose --mutate")
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

let never_some_false args =
  match parse args with
  | Error _ -> ()
  | Ok p ->
      equal
        (list (option bool))
        []
        (List.filter
           (Option.equal Bool.equal (Some false))
           [
             p.Cli.failed_only;
             p.list_only;
             p.bail;
             p.stream;
             p.update;
             p.corrected;
             p.verbose;
           ])

let refused_value (flag, value) =
  equal string (strf "invalid %s %S" flag value) (parsed [ flag; value ])

let parsing =
  group "Parsing"
    [
      parse_rows "each flag gives its field, in each of its spellings"
        [
          ([ "-f"; "pat" ], "filter [pat]");
          ([ "--filter"; "pat" ], "filter [pat]");
          ([ "--filter=abc" ], "filter [abc]");
          ([ "-e"; "ex" ], "exclude [ex]");
          ([ "--exclude"; "ex" ], "exclude [ex]");
          ([ "--exclude=xyz" ], "exclude [xyz]");
          ([ "--tag"; "a" ], "tags [a]");
          ([ "--tag=a" ], "tags [a]");
          ([ "--exclude-tag"; "slow" ], "exclude_tags [slow]");
          ([ "--exclude-tag=b" ], "exclude_tags [b]");
          ([ "--shard"; "2/4" ], "shard 2/4");
          ([ "--shard=1/1" ], "shard 1/1");
          ([ "--shard"; "4/4" ], "shard 4/4");
          ([ "--failed" ], "failed_only");
          ([ "-l" ], "list_only");
          ([ "--list" ], "list_only");
          ([ "-x" ], "bail");
          ([ "--fail-fast" ], "bail");
          ([ "--timeout"; "2.5" ], "timeout 2.5");
          ([ "--slow-threshold"; "2.5" ], "slow_threshold 2.5");
          ([ "--slow-threshold"; "0" ], "slow_threshold 0");
          ([ "--slow-threshold=0.5" ], "slow_threshold 0.5");
          ([ "--seed"; "s1:00000000000000ff" ], "seed s1:00000000000000ff");
          ([ "--prop-count"; "50" ], "prop_count 50");
          ([ "--prop-count=7" ], "prop_count 7");
          ([ "-u" ], "update");
          ([ "--update" ], "update");
          ([ "--corrected" ], "corrected");
          ([ "-s" ], "stream");
          ([ "--stream" ], "stream");
          ([ "-v" ], "verbose");
          ([ "--verbose" ], "verbose");
          ([ "--junit"; "out.xml" ], "junit out.xml");
          ([ "--color"; "never" ], "color never");
          ([ "--color=ALWAYS" ], "color always");
          ([ "--color"; "Auto" ], "color auto");
          ([ "-o"; "logs" ], "log_dir logs");
          ([ "--output=logs" ], "log_dir logs");
          ([ "--mutate" ], "mutate []");
          ([ "--mutate=lib/a.ml, lib/b.ml" ], "mutate [lib/a.ml; lib/b.ml]");
          ([ "--arm"; "lib/a.ml:1:0:add" ], "arm lib/a.ml:1:0:add");
          ([ "--arm=x" ], "arm x");
          ([ "-h" ], "help");
          ([ "--help" ], "help");
          ([ "-V" ], "version");
          ([ "--version" ], "version");
        ];
      test "every flag fits one command line" every_flag;
      test "argv.(0) is not read, and an empty argv is empty" (fun () ->
          equal (pair string string) ("empty", "empty")
            (parsed_argv [||], parsed_argv [| "--bogus" |]));
      test "parse reads no environment" (fun () ->
          stated [ ("WINDTRAP_FILTER", "from-env"); ("WINDTRAP_STREAM", "1") ];
          equal string "empty" (parsed []));
      parse_rows
        "a repeated flag keeps its last value, and -f, -e, --tag and \
         --exclude-tag accumulate in order"
        [
          ([ "--junit"; "first"; "--junit"; "second" ], "junit second");
          ([ "--shard"; "1/2"; "--shard=2/2" ], "shard 2/2");
          ( [ "-f"; "first"; "--filter"; "second"; "-e"; "x"; "--exclude=y" ],
            "filter [first; second], exclude [x; y]" );
          ( [ "--tag=a"; "--exclude-tag=b"; "--tag=c" ],
            "tags [a; c], exclude_tags [b]" );
        ];
      parse_rows
        "an argument of two bytes or more that starts with - is a flag, \
         unknown as typed but for a long flag's =value"
        [
          ([ "--bogus" ], "unknown --bogus");
          ([ "-z" ], "unknown -z");
          ([ "-1" ], "unknown -1");
          ([ "-xv" ], "unknown -xv");
          ([ "--bogus=1" ], "unknown --bogus");
        ];
      parse_rows
        "a short flag takes no inline value, so -f=x and -fx are unknown"
        [ ([ "-f=x" ], "unknown -f=x"); ([ "-fx" ], "unknown -fx") ];
      parse_rows
        "a flag that takes a value takes the next argument, whatever it is"
        [
          ([ "-f"; "--verbose" ], "filter [--verbose]");
          ([ "--arm"; "--mutate" ], "arm --mutate");
        ];
      parse_rows
        "a flag that takes a value and ends the command line is missing it"
        [
          ([ "--filter" ], "missing value --filter");
          ([ "-f"; "a"; "--seed" ], "missing value --seed");
        ];
      test "a bare --mutate never takes the next argument" (fun () ->
          equal string "filter [lib/a.ml], mutate []"
            (parsed [ "--mutate"; "lib/a.ml" ]));
      parse_rows "a flag that takes no value refuses one"
        [
          ([ "--list=x" ], {|invalid --list "x"|});
          ([ "--fail-fast=2" ], {|invalid --fail-fast "2"|});
          ([ "--verbose=1" ], {|invalid --verbose "1"|});
        ];
      test "a flag that takes no value refuses one with expected no argument"
        (fun () ->
          equal string "no argument"
            (require_match expected_of (parse [ "--verbose=1" ])));
      cases
        "a flag refuses a value outside its field's description, naming the \
         value as typed"
        ~name:(fun (flag, value) -> strf "%s %S" flag value)
        [
          ("--shard", "0/4");
          ("--shard", "5/4");
          ("--shard", "9/2");
          ("--shard", "2");
          ("--shard", "2/");
          ("--shard", "/4");
          ("--shard", "a/b");
          ("--shard", "-1/4");
          ("--shard", "2/0");
          (* Decimal numerals only: the spelling is frozen for CI. *)
          ("--shard", "0x1/4");
          ("--shard", "+1/4");
          ("--shard", "1_0/20");
          ("--shard", " 1/4");
          ("--timeout", "0");
          ("--timeout", "-1");
          ("--timeout", "nan");
          ("--timeout", "inf");
          ("--timeout", "1e400");
          ("--slow-threshold", "-1");
          ("--slow-threshold", "fast");
          ("--slow-threshold", "inf");
          ("--prop-count", "0");
          ("--prop-count", "-3");
          ("--prop-count", "many");
          ("--seed", "42");
          ("--color", "sometimes");
        ]
        refused_value;
      parse_rows "parse is the first error from the left"
        [
          ([ "--bogus"; "--prop-count"; "0" ], "unknown --bogus");
          ([ "--prop-count"; "0"; "--bogus" ], {|invalid --prop-count "0"|});
        ];
      parse_rows
        "a bare argument adds a pattern to filter, as does every argument \
         after --"
        [
          ([ "somepattern" ], "filter [somepattern]");
          ([ "one"; "two" ], "filter [one; two]");
          ( [ "one"; "-f"; "two"; "three"; "--"; "-four" ],
            "filter [one; two; three; -four]" );
          ([ "--"; "-weird" ], "filter [-weird]");
          ([ "--"; "--help" ], "filter [--help]");
          ([ "-" ], "filter [-]");
        ];
      parse_rows
        "parsing stops at --help and --version, and an error before them wins"
        [
          ([ "--help"; "--bogus" ], "help");
          ([ "-V"; "--prop-count"; "0" ], "version");
          ([ "--bogus"; "--help" ], "unknown --bogus");
          ([ "-u"; "--corrected"; "--help" ], "incompatible -u --corrected");
        ];
      parse_rows "-u with --corrected is refused, whatever the order"
        [
          ([ "-u"; "--corrected" ], "incompatible -u --corrected");
          ([ "--corrected"; "--update" ], "incompatible -u --corrected");
        ];
      prop "parse never raises and never gives Some false"
        Gen.(map List.concat (list (of_list argv_chunks)))
        never_some_false;
    ]

(* Resolution *)

let resolve_rows name rows =
  cases name
    ~name:(fun (env, args, _) ->
      match List.map fst env @ args with
      | [] -> "no variable and no flag"
      | words -> String.concat " " words)
    rows
    (fun (env, args, row) -> equal string row (resolved ~env args))

let from_the_working_directory () =
  stated [];
  chdir (temp_dir ());
  let cwd = Sys.getcwd () in
  let c = config [ "--junit"; "out"; "-o"; "logs" ] in
  equal
    (pair (option string) string)
    (Some (Filename.concat cwd "out"), Filename.concat cwd "logs")
    (c.Run.junit, c.Run.log_dir)

let unreadable_directory () =
  if Sys.win32 then skip ~reason:"POSIX only" ();
  let gone = Filename.concat (temp_dir ()) "gone" in
  Unix.mkdir gone 0o700;
  chdir gone;
  Unix.rmdir gone;
  (match Sys.getcwd () with
  | _ -> skip ~reason:"this system reads a removed working directory" ()
  | exception Sys_error _ -> ());
  equal string "logs" (config [ "-o"; "logs" ]).Run.log_dir

let seed_of env args = Seed.to_string (config ~env args).Run.seed

let resolution =
  group "Resolution"
    [
      test "with no flag and no mirror, every field but the seed is the default"
        (fun () -> equal string "defaults" (resolved []));
      test "allow_focus is false and invocation is `Mirrors" (fun () ->
          let c = config [] in
          equal (pair bool string) (false, "mirrors")
            (c.allow_focus, invocation_name c.invocation));
      test "list_only, help and version are ignored" (fun () ->
          equal string "defaults"
            (settled
               {
                 Cli.empty with
                 list_only = Some true;
                 help = true;
                 version = true;
               }));
      resolve_rows "a flag's value is its field's"
        [
          ([], [ "--failed" ], "failed_only");
          ([], [ "-x" ], "bail");
          ([], [ "-s" ], "stream");
          ([], [ "-v" ], "verbose");
        ];
      resolve_rows "a flag given on the command line wins over its mirror"
        [
          ([ ("WINDTRAP_SHARD", "2/3") ], [ "--shard"; "1/2" ], "shard 1/2");
          ([ ("WINDTRAP_TIMEOUT", "3") ], [ "--timeout"; "1" ], "timeout 1");
          ( [ ("WINDTRAP_SLOW_THRESHOLD", "3") ],
            [ "--slow-threshold"; "0.5" ],
            "slow_threshold 0.5" );
          ( [ ("WINDTRAP_PROP_COUNT", "3") ],
            [ "--prop-count"; "1" ],
            "prop_count 1" );
          ( [ ("WINDTRAP_JUNIT", "/from-env.xml") ],
            [ "--junit"; "/from-cli.xml" ],
            "junit /from-cli.xml" );
          ( [ ("WINDTRAP_COLOR", "never") ],
            [ "--color"; "always" ],
            "color always" );
          ( [ ("WINDTRAP_OUTPUT", "/env-logs") ],
            [ "-o"; "/cli-logs" ],
            "log_dir /cli-logs" );
          ( [ ("WINDTRAP_MUTATE", "lib/a.ml") ],
            [ "--mutate=lib/x.ml" ],
            "mutation loop [lib/x.ml]" );
          ( [ ("WINDTRAP_MUTATE_ARM", "a") ],
            [ "--arm"; "b" ],
            "mutation armed b" );
        ];
      resolve_rows
        "a mirror whose flag was given is not read, so a valid flag hides a \
         malformed mirror"
        [
          ([ ("WINDTRAP_SHARD", "9/2") ], [ "--shard"; "1/2" ], "shard 1/2");
          ([ ("WINDTRAP_TIMEOUT", "-5") ], [ "--timeout"; "1" ], "timeout 1");
          ([ ("WINDTRAP_TIMEOUT", "banana") ], [ "--timeout=2" ], "timeout 2");
          ( [ ("WINDTRAP_SLOW_THRESHOLD", "-2") ],
            [ "--slow-threshold"; "1.5" ],
            "slow_threshold 1.5" );
          ( [ ("WINDTRAP_SEED", "not-a-seed") ],
            [ "--seed"; "s1:0000000000000007" ],
            "defaults" );
          ( [ ("WINDTRAP_PROP_COUNT", "1O0") ],
            [ "--prop-count"; "50" ],
            "prop_count 50" );
          ( [ ("WINDTRAP_PROP_COUNT", "not-a-number") ],
            [ "--prop-count=2" ],
            "prop_count 2" );
          ([ ("WINDTRAP_STREAM", "maybe") ], [ "-s" ], "stream");
          ( [ ("WINDTRAP_COLOR", "sometimes") ],
            [ "--color"; "auto" ],
            "defaults" );
        ];
      resolve_rows
        "the mirrors are read in the order of help, and the first error ends \
         the resolution"
        [
          ( [ ("WINDTRAP_COLOR", "sometimes"); ("WINDTRAP_SHARD", "9/2") ],
            [],
            {|invalid WINDTRAP_SHARD "9/2"|} );
          ( [ ("WINDTRAP_COLOR", "sometimes"); ("WINDTRAP_STREAM", "maybe") ],
            [],
            {|invalid WINDTRAP_STREAM "maybe"|} );
        ];
      resolve_rows
        "tags add up, the command line's first, and the command line's \
         patterns replace the mirror's"
        [
          ([ ("WINDTRAP_TAG", "e") ], [ "--tag"; "c" ], "tags [c; e]");
          ( [ ("WINDTRAP_EXCLUDE_TAG", "x") ],
            [ "--exclude-tag"; "xc" ],
            "exclude_tags [xc; x]" );
          ( [ ("WINDTRAP_FILTER", " a, b ") ],
            [ "-f"; "x"; "y" ],
            "filter [x; y]" );
          ([ ("WINDTRAP_EXCLUDE", "c,d") ], [ "-e"; "z" ], "exclude [z]");
        ];
      cases
        "baseline is Update under update, else Corrected under corrected, else \
         Check"
        ~name:fst
        [
          ("update", ({ Cli.empty with update = Some true }, "baseline update"));
          ( "corrected",
            ({ Cli.empty with corrected = Some true }, "baseline corrected") );
          ( "both",
            ( { Cli.empty with update = Some true; corrected = Some true },
              "baseline update" ) );
        ]
        (fun (_, (p, row)) -> equal string row (settled p));
      resolve_rows "mutation is Loop of --mutate and Armed of --arm"
        [
          ([], [ "--mutate" ], "mutation loop []");
          ( [],
            [ "--mutate=lib/a.ml, lib/b.ml" ],
            "mutation loop [lib/a.ml; lib/b.ml]" );
          ( [],
            [ "--arm"; "lib/a.ml:9:12:add" ],
            "mutation armed lib/a.ml:9:12:add" );
        ];
      resolve_rows "--mutate with --arm is refused, whichever layer gave each"
        [
          ([], [ "--mutate"; "--arm"; "x" ], "incompatible --mutate --arm");
          ( [ ("WINDTRAP_MUTATE_ARM", "x") ],
            [ "--mutate" ],
            "incompatible --mutate --arm" );
          ( [ ("WINDTRAP_MUTATE", "1") ],
            [ "--arm"; "x" ],
            "incompatible --mutate --arm" );
          ( [ ("WINDTRAP_MUTATE", "1"); ("WINDTRAP_MUTATE_ARM", "x") ],
            [],
            "incompatible --mutate --arm" );
        ];
      test "--mutate with --arm is checked after every mirror" (fun () ->
          equal string {|invalid WINDTRAP_COLOR "sometimes"|}
            (resolved
               ~env:[ ("WINDTRAP_COLOR", "sometimes") ]
               [ "--mutate"; "--arm"; "x" ]));
      test "a mirror's relative path is read from the project root" (fun () ->
          let root = "/somewhere/project" in
          equal string
            (Printf.sprintf "log_dir %s, junit %s"
               (Filename.concat root "logs")
               (Filename.concat root "_build/junit"))
            (resolved
               ~env:
                 [
                   ("WINDTRAP_PROJECT_ROOT", root);
                   ("WINDTRAP_JUNIT", "_build/junit");
                   ("WINDTRAP_OUTPUT", "logs");
                 ]
               []));
      test "a flag's relative path is read from the working directory"
        from_the_working_directory;
      test
        "a relative -o is kept as given when the working directory cannot be \
         read"
        unreadable_directory;
      test "github is Os.in_github_actions ()" (fun () ->
          equal string "github"
            (resolved ~env:[ ("CI", "true"); ("GITHUB_ACTIONS", "true") ] []));
      resolve_rows "a selection is broadcast when the mirrors alone give it"
        [
          ( [ ("WINDTRAP_TAG", "gpu") ],
            [ "-f"; "parse" ],
            "filter [parse], tags [gpu]" );
          ( [ ("WINDTRAP_TAG", "gpu") ],
            [ "-e"; "slow" ],
            "exclude [slow], tags [gpu]" );
          ([ ("WINDTRAP_TAG", "gpu") ], [ "--tag"; "cpu" ], "tags [cpu; gpu]");
          ( [ ("WINDTRAP_TAG", "gpu") ],
            [ "--exclude-tag"; "cpu" ],
            "tags [gpu], exclude_tags [cpu]" );
          ( [ ("WINDTRAP_TAG", "gpu") ],
            [ "--shard"; "1/2" ],
            "tags [gpu], shard 1/2" );
          ( [ ("WINDTRAP_TAG", "gpu") ],
            [ "--failed" ],
            "tags [gpu], failed_only" );
          ([], [ "-f"; "parse" ], "filter [parse]");
        ];
      test "a mutation run is broadcast when WINDTRAP_MUTATE alone asks for it"
        (fun () ->
          equal string "mutation loop [lib/]"
            (resolved ~env:[ ("WINDTRAP_MUTATE", "1") ] [ "--mutate=lib/" ]));
      cases "the seed is the command line's, else WINDTRAP_SEED's"
        ~name:(fun (env, args, _) -> joined (List.map snd env @ args))
        [
          ( [ ("WINDTRAP_SEED", "s1:00000000000000aa") ],
            [],
            "s1:00000000000000aa" );
          ( [ ("WINDTRAP_SEED", "s1:00000000000000aa") ],
            [ "--seed"; "s1:0000000000000007" ],
            "s1:0000000000000007" );
        ]
        (fun (env, args, seed) -> equal string seed (seed_of env args));
      test "without a seed in any layer, each call draws another" (fun () ->
          not_equal string (seed_of [] []) (seed_of [] []));
      cases "color_mode reads WINDTRAP_COLOR by the parser of --color"
        ~name:(fun (value, _) ->
          Option.fold ~none:"unset" ~some:(strf "%S") value)
        [
          (None, "auto");
          (Some "", "auto");
          (Some " Never ", "never");
          (Some "ALWAYS", "always");
          (Some "sometimes", {|invalid WINDTRAP_COLOR "sometimes"|});
        ]
        (fun (value, row) ->
          stated
            (Option.fold ~none:[]
               ~some:(fun v -> [ ("WINDTRAP_COLOR", v) ])
               value);
          equal string row
            (Result.fold ~ok:color_name ~error:error_row (Cli.color_mode ())));
      test "color_mode refuses a word in --color's words" (fun () ->
          stated [ ("WINDTRAP_COLOR", "sometimes") ];
          equal string
            (require_match expected_of (parse [ "--color"; "sometimes" ]))
            (require_match expected_of (Cli.color_mode ())));
    ]

(* Help *)

let longest_line page =
  List.fold_left
    (fun longest line -> max longest (String.length line))
    0
    (String.split_on_char '\n' page)

let help =
  group "Help"
    [
      test "usage is one line with the basename of prog" (fun () ->
          equal string "usage: mytests.exe [OPTIONS] [PATTERN...]"
            (Cli.usage ~prog:"/some/path/mytests.exe"));
      test
        "help is the page of every flag, then of every setting no flag spells"
        (fun () ->
          expect_file
            (Cli.help ~prog:"/some/path/mytests.exe")
            "test/unit/expected/test_cli/help.expected");
      test "every line of help fits 80 columns" (fun () ->
          at_most int ~than:80 (longest_line (Cli.help ~prog:"mytests.exe")));
      test "help ends with a newline" (fun () ->
          let page = Cli.help ~prog:"mytests.exe" in
          equal char '\n' page.[String.length page - 1]);
    ]

let () =
  exit (run "cli" [ mirrors; parsed_flags; errors; parsing; resolution; help ])
