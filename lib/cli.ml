(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Parsed flags *)

(* Each flag sets the configuration's field directly, over the defaults, and
   the scan keeps the long name of every flag it met, so that resolution
   knows which mirrors the command line hides and which layer gave a value.
   The other fields hold what no field of the configuration does. *)
type parsed = {
  config : Run.config;
  given : string list;
  lines : Lines.t option;
  diff : string option;
  case : string option;
  list_only : bool;
  help : bool;
  version : bool;
}

let start () =
  {
    config = Run.default_config ();
    given = [];
    lines = None;
    diff = None;
    case = None;
    list_only = false;
    help = false;
    version = false;
  }

let gave p long = List.mem long p.given

let mark long p =
  if gave p long then p else { p with given = p.given @ [ long ] }

(* Errors *)

type error =
  | Unknown_flag of string
  | Missing_value of string
  | Invalid_value of { source : string; value : string; expected : string }
  | Incompatible_flags of string * string
  | Requires of string * string

let ( let* ) = Result.bind

let invalid ~source ~value ~expected =
  Error (Invalid_value { source; value; expected })

(* The flag table *)

(* What a flag takes on the command line. A [Value] is the next argument or
   inline, as in [--flag=value]. An [Optional_value] is inline or absent, so
   a bare flag never takes the next argument. *)
type arg =
  | Flag of (parsed -> parsed)
  | Value of {
      metavar : string;
      set : source:string -> parsed -> string -> (parsed, error) result;
    }
  | Optional_value of {
      metavar : string;
      set : source:string -> parsed -> string option -> (parsed, error) result;
    }

(* How a mirror layers under the command line. A [Single] mirror is read
   only when the command line did not give its flag, which hides it unread. A
   [Repeatable] mirror adds its items to the command line's. *)
type layering = Single | Repeatable
type mirror = { var : string; layering : layering }

type entry = {
  short : string option;
  long : string;
  arg : arg;
  doc : string;
  mirror : mirror option;
}

let row ?short long arg ~mirror doc = { short; long; arg; doc; mirror }
let mirrored var = Some { var; layering = Single }
let repeatable var = Some { var; layering = Repeatable }

(* [read] gives the value to [store], or the [expected] clause of the error
   that refuses it. *)
let value metavar read store =
  let set ~source p value =
    match read value with
    | Ok v -> Ok (store p v)
    | Error expected -> invalid ~source ~value ~expected
  in
  Value { metavar; set }

let any value = Ok value

let positive_int value =
  match int_of_string_opt value with
  | Some n when n > 0 -> Ok n
  | _ -> Error "a positive integer"

let seconds ~expected accepts value =
  match float_of_string_opt value with
  | Some s when accepts s && Float.is_finite s -> Ok s
  | _ -> Error expected

(* The spelling is frozen for CI, so [K] and [N] are plain decimal numerals,
   without the sign, [0x] or [_] that [int_of_string] reads. *)
let shard value =
  let expected = "K/N with 1 <= K <= N (e.g. 2/4)" in
  let decimal s =
    s <> "" && String.for_all (function '0' .. '9' -> true | _ -> false) s
  in
  match String.split_on_char '/' value with
  | [ k; n ] when decimal k && decimal n -> (
      match (int_of_string_opt k, int_of_string_opt n) with
      | Some k, Some n when 1 <= k && k <= n -> Ok (k, n)
      | _ -> Error expected)
  | _ -> Error expected

let seed value =
  Result.map_error
    (fun _ -> "an s1: token with 16 lowercase hexadecimal digits")
    (Seed.of_string value)

let replay_token value = Result.map (fun _ -> value) (Choice.of_token value)

(* Most flags set a field of the configuration. *)
let field set p v = { p with config = set p.config v }

let add_filter =
  field (fun (c : Run.config) pattern ->
      { c with filter = c.filter @ [ pattern ] })

(* [color_mode] reads this row alone, so WINDTRAP_COLOR means one thing for
   every command. *)
let color_entry =
  row "--color"
    (value "MODE"
       (fun mode ->
         Option.to_result ~none:"always, never or auto"
           (Os.color_mode_of_string mode))
       (field (fun c color -> { c with color })))
    ~mirror:(mirrored "WINDTRAP_COLOR")
    "Color output: always, never or auto."

let switch set = Flag (fun p -> field (fun c () -> set c) p ())

(* The order is [--help]'s and the order the mirrors are read in. *)
let entries =
  [
    row ~short:"-f" "--filter"
      (value "PATTERN" any add_filter)
      ~mirror:(mirrored "WINDTRAP_FILTER")
      "Run only tests whose path contains PATTERN (repeatable: any of them).";
    row ~short:"-e" "--exclude"
      (value "PATTERN" any
         (field (fun c pattern -> { c with exclude = c.exclude @ [ pattern ] })))
      ~mirror:(mirrored "WINDTRAP_EXCLUDE")
      "Skip tests whose path contains PATTERN (repeatable).";
    row "--tag"
      (value "TAG" any
         (field (fun c tag -> { c with tags = c.tags @ [ tag ] })))
      ~mirror:(repeatable "WINDTRAP_TAG")
      "Run only tests tagged TAG (repeatable: all of them).";
    row "--exclude-tag"
      (value "TAG" any
         (field (fun c tag ->
              { c with exclude_tags = c.exclude_tags @ [ tag ] })))
      ~mirror:(repeatable "WINDTRAP_EXCLUDE_TAG")
      "Skip tests tagged TAG (repeatable).";
    row "--shard"
      (value "K/N" shard (field (fun c kn -> { c with shard = Some kn })))
      ~mirror:(mirrored "WINDTRAP_SHARD")
      "Run only the Kth of N deterministic path-hash buckets.";
    (* Under dune a mirror of the loop flags would change nothing: dune
       reruns a failed action on every invocation and keeps a passed one
       cached whatever variable is set. *)
    row "--failed"
      (switch (fun c -> { c with failed_only = true }))
      ~mirror:None "Run only the last failed tests.";
    row ~short:"-l" "--list"
      (Flag (fun p -> { p with list_only = true }))
      ~mirror:None "List selected tests without running them.";
    row ~short:"-x" "--fail-fast"
      (switch (fun c -> { c with bail = true }))
      ~mirror:None "Stop after the first failure.";
    row "--timeout"
      (value "SECONDS"
         (seconds ~expected:"a positive number" (fun s -> s > 0.))
         (field (fun c limit -> { c with timeout = Some limit })))
      ~mirror:(mirrored "WINDTRAP_TIMEOUT")
      "Limit each attempt of a test that declares no timeout to SECONDS \
       (default 600; none under --in-process).";
    row "--slow-threshold"
      (value "SECONDS"
         (seconds ~expected:"a non-negative number" (fun s -> s >= 0.))
         (field (fun c slow_threshold -> { c with slow_threshold })))
      ~mirror:(mirrored "WINDTRAP_SLOW_THRESHOLD")
      "Warn when an untagged test runs longer than SECONDS (0 disables).";
    row "--seed"
      (value "TOKEN" seed (field (fun c seed -> { c with seed })))
      ~mirror:(mirrored "WINDTRAP_SEED")
      "Root seed for property tests (s1:<16 hex>).";
    row "--prop-count"
      (value "N" positive_int
         (field (fun c n -> { c with prop_count = Some n })))
      ~mirror:(mirrored "WINDTRAP_PROP_COUNT")
      "Raise every property's generated cases to at least N.";
    row "--replay"
      (value "TOKEN" replay_token
         (field (fun c t -> { c with replay = Some t })))
      ~mirror:(mirrored "WINDTRAP_REPLAY")
      "Check each selected property on the one case of TOKEN alone.";
    (* One case of one test: a mirror would reach every stanza, and refuse
       each that selects more than one test. *)
    row "--case"
      (value "PATH" any (fun p path -> { p with case = Some path }))
      ~mirror:None
      "Check the one selected property on the one case of the bytes of PATH.";
    row ~short:"-u" "--update"
      (switch (fun c -> { c with baseline = Baseline.Update }))
      ~mirror:None "Accept baseline changes in place (refused under CI).";
    row "--corrected"
      (switch (fun c -> { c with baseline = Baseline.Corrected }))
      ~mirror:None "Write corrections as <file>.corrected, for dune promote.";
    row ~short:"-s" "--stream"
      (switch (fun c -> { c with stream = true }))
      ~mirror:(mirrored "WINDTRAP_STREAM")
      "Stream test output instead of capturing it.";
    row ~short:"-j" "--jobs"
      (value "N" positive_int (field (fun c jobs -> { c with jobs })))
      ~mirror:(mirrored "WINDTRAP_JOBS")
      "Run the tests in N worker processes at once (default 1).";
    (* A debugger's gesture at one test, never a mode of a stanza, so no
       mirror. *)
    row "--in-process"
      (switch (fun c -> { c with in_process = true }))
      ~mirror:None "Run the one selected test in this process, for a debugger.";
    row ~short:"-v" "--verbose"
      (switch (fun c -> { c with verbose = true }))
      ~mirror:(mirrored "WINDTRAP_VERBOSE")
      "One status line per test.";
    row "--junit"
      (value "PATH" any (field (fun c path -> { c with junit = Some path })))
      ~mirror:(mirrored "WINDTRAP_JUNIT")
      "Also write a JUnit XML report to PATH, or to PATH/<suite>.xml when PATH \
       does not end in .xml.";
    row "--json"
      (value "PATH" any (field (fun c path -> { c with json = Some path })))
      ~mirror:(mirrored "WINDTRAP_JSON")
      "Also write a JSON report of the results and their scope to PATH, or to \
       PATH/<suite>.json when PATH does not end in .json.";
    color_entry;
    row ~short:"-o" "--output"
      (value "DIR" any (field (fun c log_dir -> { c with log_dir })))
      ~mirror:(mirrored "WINDTRAP_OUTPUT")
      "Root directory for capture logs.";
    row "--mutate"
      (Optional_value
         {
           metavar = "PREFIX,...";
           set =
             (fun ~source:_ p prefixes ->
               let prefixes =
                 Option.fold ~none:[] ~some:Os.split_comma prefixes
               in
               Ok
                 (field
                    (fun c () ->
                      { c with mutation = Run.Loop { prefixes; lines = None } })
                    p ()));
         })
      ~mirror:(mirrored "WINDTRAP_MUTATE")
      "Test this executable's mutants, all or those under PREFIX.";
    row "--lines"
      (value "FILE:A-B,..." Lines.of_spec (fun p lines ->
           { p with lines = Some lines }))
      ~mirror:(mirrored "WINDTRAP_LINES")
      "Test only the mutants on these lines of these files (with --mutate).";
    row "--diff"
      (value "FILE" any (fun p file -> { p with diff = Some file }))
      ~mirror:(mirrored "WINDTRAP_DIFF")
      "Test only the mutants on the lines that FILE, the output of git diff \
       -U0, adds or changes (with --mutate).";
    row "--arm"
      (value "ID" any (field (fun c id -> { c with mutation = Run.Armed id })))
      ~mirror:(mirrored "WINDTRAP_MUTATE_ARM")
      "Run once with mutant ID armed.";
    row ~short:"-V" "--version"
      (Flag (fun p -> { p with version = true }))
      ~mirror:None "Print the version and exit.";
    row ~short:"-h" "--help"
      (Flag (fun p -> { p with help = true }))
      ~mirror:None "Print this help and exit.";
  ]

(* The settings no flag spells, which [help] lists after the flags. Their
   owners read them: [Os] the project root, and the coverage runtime its
   dump's path. NO_COLOR is the convention every command-line tool honours. *)
let variables =
  [
    ("WINDTRAP_PROJECT_ROOT", "Project root that baseline paths resolve under.");
    ( "WINDTRAP_COVERAGE_FILE",
      "Where an instrumented run writes its coverage dump." );
    ("NO_COLOR", "Any value: never style output (--color auto).");
  ]

(* Error messages *)

(* The Damerau-Levenshtein distance, where a transposition is one edit: it
   is the typo people make, and it puts [--juint] nearer [--junit] than
   [--update]. *)
let edit_distance a b =
  let la = String.length a and lb = String.length b in
  let d = Array.make_matrix (la + 1) (lb + 1) 0 in
  for i = 0 to la do
    d.(i).(0) <- i
  done;
  for j = 0 to lb do
    d.(0).(j) <- j
  done;
  for i = 1 to la do
    for j = 1 to lb do
      let substitution = if a.[i - 1] = b.[j - 1] then 0 else 1 in
      let best =
        min
          (min (d.(i).(j - 1) + 1) (d.(i - 1).(j) + 1))
          (d.(i - 1).(j - 1) + substitution)
      in
      d.(i).(j) <-
        (if i > 1 && j > 1 && a.[i - 1] = b.[j - 2] && a.[i - 2] = b.[j - 1]
         then min best (d.(i - 2).(j - 2) + 1)
         else best)
    done
  done;
  d.(la).(lb)

(* A short flag gets no suggestion: any two are one edit apart. A long one
   gets the nearest long flag, the first in the table on a tie, within a
   third of its length or two edits: farther is another word, not a slip. *)
let nearest_flag flag =
  if not (String.starts_with ~prefix:"--" flag) then None
  else
    let budget = max 2 (String.length flag / 3) in
    let nearer best entry =
      let d = edit_distance flag entry.long in
      match best with
      | Some (_, best_d) when best_d <= d -> best
      | _ when d <= budget -> Some (entry.long, d)
      | _ -> best
    in
    Option.map fst (List.fold_left nearer None entries)

let error_message = function
  | Unknown_flag flag -> (
      let base = Pp.str "unknown option '%s'" flag in
      match nearest_flag flag with
      | Some name -> Pp.str "%s; did you mean '%s'?" base name
      | None -> base)
  | Missing_value flag -> Pp.str "option '%s' requires an argument" flag
  | Invalid_value { source; value; expected } ->
      Pp.str "invalid value '%s' for %s: expected %s" value source expected
  | Incompatible_flags (first, second) ->
      Pp.str "options '%s' and '%s' cannot be combined" first second
  | Requires (flag, needed) ->
      Pp.str "option '%s' scopes '%s', which is not given" flag needed

(* Parsing *)

let find entries name =
  List.find_opt (fun e -> e.long = name || e.short = Some name) entries

let split_inline arg =
  match String.index_opt arg '=' with
  | None -> (arg, None)
  | Some eq ->
      ( String.sub arg 0 eq,
        Some (String.sub arg (eq + 1) (String.length arg - eq - 1)) )

(* [apply entry ~source ~inline p rest] is [p] with the flag of [entry] set,
   and the arguments after it. *)
let apply entry ~source ~inline p rest =
  let p = mark entry.long p in
  match (entry.arg, inline, rest) with
  | Flag _, Some value, _ -> invalid ~source ~value ~expected:"no argument"
  | Flag set, None, rest -> Ok (set p, rest)
  | Value { set; _ }, Some value, rest | Value { set; _ }, None, value :: rest
    ->
      let* p = set ~source p value in
      Ok (p, rest)
  | Value _, None, [] -> Error (Missing_value source)
  | Optional_value { set; _ }, inline, rest ->
      let* p = set ~source p inline in
      Ok (p, rest)

(* A bare pattern is given as [-f] gives one. *)
let positional p pattern = mark "--filter" (add_filter p pattern)

let rec parse_args entries p = function
  | [] -> Ok p
  | "--" :: patterns -> Ok (List.fold_left positional p patterns)
  | arg :: rest when String.length arg > 1 && arg.[0] = '-' -> (
      let name, inline =
        if String.starts_with ~prefix:"--" arg then split_inline arg
        else (arg, None)
      in
      match find entries name with
      | None -> Error (Unknown_flag name)
      | Some entry ->
          let* p, rest = apply entry ~source:name ~inline p rest in
          if p.help || p.version then Ok p else parse_args entries p rest)
  | pattern :: rest -> parse_args entries (positional p pattern) rest

let parse_entries entries argv =
  let args = match Array.to_list argv with [] -> [] | _prog :: args -> args in
  let* p = parse_args entries (start ()) args in
  if gave p "--update" && gave p "--corrected" then
    Error (Incompatible_flags ("-u", "--corrected"))
  else Ok p

let parse argv = parse_entries entries argv

(* Resolution *)

let rec fold_ok f acc = function
  | [] -> Ok acc
  | x :: xs ->
      let* acc = f acc x in
      fold_ok f acc xs

(* A mirror reaches the record through its flag's own [set], with the
   variable as the source, so it cannot drift from its flag, and a mirror
   that set something counts as given. A valueless flag's mirror refuses any
   word but a boolean: a typo must not read as off. *)
let contribute p entry { var = source; layering } raw =
  let given p = Ok (mark entry.long p) in
  match (entry.arg, layering) with
  | Flag set, _ -> (
      match Os.bool_of_string raw with
      | Some true -> given (set p)
      | Some false -> Ok p
      | None -> invalid ~source ~value:raw ~expected:Os.bool_expected)
  | Value { set; _ }, Single ->
      Result.bind (set ~source p (String.trim raw)) given
  | Value { set; _ }, Repeatable -> (
      match Os.split_comma raw with
      | [] -> Ok p
      | items -> Result.bind (fold_ok (set ~source) p items) given)
  | Optional_value { set; _ }, _ -> (
      match Os.bool_of_string raw with
      | Some true -> Result.bind (set ~source p None) given
      | Some false -> Ok p
      | None -> Result.bind (set ~source p (Some (String.trim raw))) given)

(* A flag the command line gave hides its single mirror. *)
let layer_entries entries cli =
  let layer p entry =
    match entry.mirror with
    | None -> Ok p
    | Some { layering = Single; _ } when gave cli entry.long -> Ok p
    | Some mirror -> (
        match Os.getenv mirror.var with
        | None -> Ok p
        | Some raw -> contribute p entry mirror raw)
  in
  fold_ok layer cli entries

let selection =
  [ "--filter"; "--exclude"; "--tag"; "--exclude-tag"; "--shard"; "--failed" ]

let selects p = List.exists (gave p) selection

(* A relative path is made absolute before a test can chdir: the command
   line's against the working directory, and a mirror's against the project
   root, since each stanza runs in its own build directory. *)
let absolute_path ~cli long path =
  let base = if gave cli long then Sys.getcwd else Os.project_root in
  if not (Filename.is_relative path) then path
  else
    match base () with
    | base -> Filename.concat base path
    | exception Sys_error _ -> path

(* The file is read before any test runs, which could change the directory
   or the file, and read whole: its bytes are the case. *)
let case_file ~cli ~(layered : parsed) =
  match (cli.case, layered.config.replay) with
  | None, _ -> Ok None
  | Some _, Some _ -> Error (Incompatible_flags ("--replay", "--case"))
  | Some file, None -> (
      match In_channel.with_open_bin file In_channel.input_all with
      | bytes -> Ok (Some { Run.file; choices = Choice.of_bytes bytes })
      | exception Sys_error _ ->
          Error
            (Invalid_value
               { source = "--case"; value = file; expected = "a readable file" })
      )

(* The root under [--mutate] and [--arm], so that the dry run, the probe,
   every mutant and every later run of the binary draw the same cases: a
   random root made a mutant that some cases kill flip from run to run. *)
let mutation_seed = 0x6d7574616e747321L (* "mutants!" *)

(* A line scope from the command line without [--mutate] would scope
   nothing; one from a mirror reaches every stanza, and the stanzas that do
   not mutate ignore it. *)
let line_scope ~cli ~layered =
  let given = Option.value layered.lines ~default:Lines.empty in
  match (gave cli "--lines", gave cli "--diff", gave layered "--mutate") with
  | true, _, false -> Error (Requires ("--lines", "--mutate"))
  | false, true, false -> Error (Requires ("--diff", "--mutate"))
  | _ -> (
      match Option.map (absolute_path ~cli "--diff") layered.diff with
      | None -> Ok layered.lines
      | Some path -> (
          match In_channel.with_open_bin path In_channel.input_all with
          | text -> Ok (Some (Lines.union given (Lines.of_diff text)))
          | exception Sys_error _ ->
              Error
                (Invalid_value
                   {
                     source =
                       (if gave cli "--diff" then "--diff" else "WINDTRAP_DIFF");
                     value = Option.value layered.diff ~default:path;
                     expected = "a readable file, the output of git diff -U0";
                   })))

(* Every value arrived through its flag's parser, so nothing is range-checked
   here. [broadcast] keeps what the mirrors alone asked for: a mirror reaches
   every stanza of a project, and a stanza that cannot honour it is not in
   error. *)
let settings cli =
  let* layered = layer_entries entries cli in
  let* lines = line_scope ~cli ~layered in
  let* () =
    if gave layered "--mutate" && gave layered "--arm" then
      Error (Incompatible_flags ("--mutate", "--arm"))
    else Ok ()
  in
  let* case_file = case_file ~cli ~layered in
  let c = layered.config in
  let path long = absolute_path ~cli long in
  Ok
    {
      c with
      seed =
        (match c.mutation with
        | (Run.Loop _ | Run.Armed _) when not (gave layered "--seed") ->
            mutation_seed
        | Run.Loop _ | Run.Armed _ | Run.No_mutation -> c.seed);
      mutation =
        (match c.mutation with
        | Run.Loop { prefixes; _ } -> Run.Loop { prefixes; lines }
        | (Run.Armed _ | Run.No_mutation) as m -> m);
      case_file;
      log_dir = path "--output" c.log_dir;
      junit = Option.map (path "--junit") c.junit;
      json = Option.map (path "--json") c.json;
      github = Os.in_github_actions ();
      broadcast =
        {
          Run.selection = selects layered && not (selects cli);
          mutate = gave layered "--mutate" && not (gave cli "--mutate");
        };
    }

let color_mode () =
  let* p = layer_entries [ color_entry ] (start ()) in
  Ok p.config.color

let parse_color value =
  let* p, _ =
    apply color_entry ~source:"--color" ~inline:(Some value) (start ()) []
  in
  Ok p.config.color

(* Help *)

let usage ~prog =
  Pp.str "usage: %s [OPTIONS] [PATTERN...]" (Filename.basename prog)

(* cmdliner's spelling, which [parse] reads back: a value follows a short
   name after a space and a long name after [=]. *)
let flag_heading entry =
  let short_value, long_value =
    match entry.arg with
    | Flag _ -> ("", "")
    | Value { metavar; _ } -> (" " ^ metavar, "=" ^ metavar)
    | Optional_value { metavar; _ } -> ("", Pp.str "[=%s]" metavar)
  in
  let names =
    match entry.short with
    | None -> entry.long ^ long_value
    | Some short -> Pp.str "%s%s, %s%s" short short_value entry.long long_value
  in
  match entry.mirror with
  | None -> names
  | Some { var; _ } -> Pp.str "%s (env %s)" names var

(* [text] filled greedily into lines of at most 80 columns behind [indent]
   spaces. A word is never split. *)
let fill ~indent text =
  let margin = String.make indent ' ' in
  let add lines word =
    match lines with
    | line :: rest when Text.length_utf8 line + 1 + Text.length_utf8 word <= 80
      ->
        (line ^ " " ^ word) :: rest
    | lines -> (margin ^ word) :: lines
  in
  List.rev (List.fold_left add [] (String.split_on_char ' ' text))

let described ~heading doc = (("  " ^ heading) :: fill ~indent:6 doc) @ [ "" ]

let help ~prog =
  let flags =
    List.concat_map (fun e -> described ~heading:(flag_heading e) e.doc) entries
  in
  let environment =
    List.concat_map (fun (var, doc) -> described ~heading:var doc) variables
  in
  String.concat "\n"
    (List.concat
       [
         [
           Pp.str "%s - windtrap test runner" (Filename.basename prog);
           "";
           usage ~prog;
           "";
         ];
         fill ~indent:0
           "Each bare PATTERN is read as one -f PATTERN. (env VAR) after an \
            option names the variable that sets it for a run with no command \
            line, such as dune runtest.";
         [ ""; "OPTIONS:" ];
         flags;
         "ENVIRONMENT (no flag):" :: environment;
       ])
