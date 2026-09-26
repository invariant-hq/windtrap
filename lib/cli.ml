(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Parsed flags *)

type parsed = {
  filter : string list;
  exclude : string list;
  tags : string list;
  exclude_tags : string list;
  shard : (int * int) option;
  failed_only : bool option;
  list_only : bool option;
  bail : bool option;
  stream : bool option;
  update : bool option;
  corrected : bool option;
  seed : Seed.seed option;
  timeout : float option;
  slow_threshold : float option;
  prop_count : int option;
  verbose : bool option;
  junit : string option;
  color : Os.color_mode option;
  log_dir : string option;
  mutate : string list option;
  arm : string option;
  help : bool;
  version : bool;
}

let empty =
  {
    filter = [];
    exclude = [];
    tags = [];
    exclude_tags = [];
    shard = None;
    failed_only = None;
    list_only = None;
    bail = None;
    stream = None;
    update = None;
    corrected = None;
    seed = None;
    timeout = None;
    slow_threshold = None;
    prop_count = None;
    verbose = None;
    junit = None;
    color = None;
    log_dir = None;
    mutate = None;
    arm = None;
    help = false;
    version = false;
  }

(* Errors *)

type error =
  | Unknown_flag of string
  | Missing_value of string
  | Invalid_value of { source : string; value : string; expected : string }
  | Incompatible_flags of string * string

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

(* How a mirror layers under the command line. A [Single absent] mirror is
   read only while [absent] holds of the record, so a flag given on the
   command line hides its mirror unread. A [Repeatable] mirror adds its items
   to the command line's. *)
type layering = Single of (parsed -> bool) | Repeatable
type mirror = { var : string; layering : layering }

type entry = {
  short : string option;
  long : string;
  arg : arg;
  doc : string;
  mirror : mirror option;
}

let row ?short long arg ~mirror doc = { short; long; arg; doc; mirror }
let mirrored var absent = Some { var; layering = Single absent }
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

let add_filter p pattern = { p with filter = p.filter @ [ pattern ] }

(* [color_mode] reads this row alone, so WINDTRAP_COLOR means one thing for
   every command. *)
let color_entry =
  row "--color"
    (value "MODE"
       (fun mode ->
         Option.to_result ~none:"always, never or auto"
           (Os.color_mode_of_string mode))
       (fun p mode -> { p with color = Some mode }))
    ~mirror:(mirrored "WINDTRAP_COLOR" (fun p -> p.color = None))
    "Color output: always, never or auto."

(* The order is [--help]'s and the order the mirrors are read in. *)
let entries =
  [
    row ~short:"-f" "--filter"
      (value "PATTERN" any add_filter)
      ~mirror:(mirrored "WINDTRAP_FILTER" (fun p -> p.filter = []))
      "Run only tests whose path contains PATTERN (repeatable: any of them).";
    row ~short:"-e" "--exclude"
      (value "PATTERN" any (fun p pattern ->
           { p with exclude = p.exclude @ [ pattern ] }))
      ~mirror:(mirrored "WINDTRAP_EXCLUDE" (fun p -> p.exclude = []))
      "Skip tests whose path contains PATTERN (repeatable).";
    row "--tag"
      (value "TAG" any (fun p tag -> { p with tags = p.tags @ [ tag ] }))
      ~mirror:(repeatable "WINDTRAP_TAG")
      "Run only tests tagged TAG (repeatable: all of them).";
    row "--exclude-tag"
      (value "TAG" any (fun p tag ->
           { p with exclude_tags = p.exclude_tags @ [ tag ] }))
      ~mirror:(repeatable "WINDTRAP_EXCLUDE_TAG")
      "Skip tests tagged TAG (repeatable).";
    row "--shard"
      (value "K/N" shard (fun p kn -> { p with shard = Some kn }))
      ~mirror:(mirrored "WINDTRAP_SHARD" (fun p -> p.shard = None))
      "Run only the Kth of N deterministic path-hash buckets.";
    (* Under dune a mirror of the loop flags would change nothing: dune
       reruns a failed action on every invocation and keeps a passed one
       cached whatever variable is set. *)
    row "--failed"
      (Flag (fun p -> { p with failed_only = Some true }))
      ~mirror:None "Run only the last failed tests.";
    row ~short:"-l" "--list"
      (Flag (fun p -> { p with list_only = Some true }))
      ~mirror:None "List selected tests without running them.";
    row ~short:"-x" "--fail-fast"
      (Flag (fun p -> { p with bail = Some true }))
      ~mirror:None "Stop after the first failure.";
    row "--timeout"
      (value "SECONDS"
         (seconds ~expected:"a positive number" (fun s -> s > 0.))
         (fun p limit -> { p with timeout = Some limit }))
      ~mirror:(mirrored "WINDTRAP_TIMEOUT" (fun p -> p.timeout = None))
      "Default per-test timeout in seconds.";
    row "--slow-threshold"
      (value "SECONDS"
         (seconds ~expected:"a non-negative number" (fun s -> s >= 0.))
         (fun p limit -> { p with slow_threshold = Some limit }))
      ~mirror:
        (mirrored "WINDTRAP_SLOW_THRESHOLD" (fun p -> p.slow_threshold = None))
      "Warn when an untagged test runs longer than SECONDS (0 disables).";
    row "--seed"
      (value "TOKEN" seed (fun p s -> { p with seed = Some s }))
      ~mirror:(mirrored "WINDTRAP_SEED" (fun p -> p.seed = None))
      "Root seed for property tests (s1:<16 hex>).";
    row "--prop-count"
      (value "N" positive_int (fun p n -> { p with prop_count = Some n }))
      ~mirror:(mirrored "WINDTRAP_PROP_COUNT" (fun p -> p.prop_count = None))
      "Generated cases per property.";
    row ~short:"-u" "--update"
      (Flag (fun p -> { p with update = Some true }))
      ~mirror:None "Accept baseline changes in place (refused under CI).";
    row "--corrected"
      (Flag (fun p -> { p with corrected = Some true }))
      ~mirror:None "Write corrections as <file>.corrected, for dune promote.";
    row ~short:"-s" "--stream"
      (Flag (fun p -> { p with stream = Some true }))
      ~mirror:(mirrored "WINDTRAP_STREAM" (fun p -> p.stream = None))
      "Stream test output instead of capturing it.";
    row ~short:"-v" "--verbose"
      (Flag (fun p -> { p with verbose = Some true }))
      ~mirror:(mirrored "WINDTRAP_VERBOSE" (fun p -> p.verbose = None))
      "One status line per test.";
    row "--junit"
      (value "PATH" any (fun p path -> { p with junit = Some path }))
      ~mirror:(mirrored "WINDTRAP_JUNIT" (fun p -> p.junit = None))
      "Also write a JUnit XML report to PATH.";
    color_entry;
    row ~short:"-o" "--output"
      (value "DIR" any (fun p dir -> { p with log_dir = Some dir }))
      ~mirror:(mirrored "WINDTRAP_OUTPUT" (fun p -> p.log_dir = None))
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
               Ok { p with mutate = Some prefixes });
         })
      ~mirror:(mirrored "WINDTRAP_MUTATE" (fun p -> p.mutate = None))
      "Test this executable's mutants, all or those under PREFIX.";
    row "--arm"
      (value "ID" any (fun p id -> { p with arm = Some id }))
      ~mirror:(mirrored "WINDTRAP_MUTATE_ARM" (fun p -> p.arm = None))
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

let rec parse_args entries p = function
  | [] -> Ok p
  | "--" :: patterns -> Ok (List.fold_left add_filter p patterns)
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
  | pattern :: rest -> parse_args entries (add_filter p pattern) rest

let parse_entries entries argv =
  let args = match Array.to_list argv with [] -> [] | _prog :: args -> args in
  let* p = parse_args entries empty args in
  if p.update = Some true && p.corrected = Some true then
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
   variable as the source, so it cannot drift from its flag. A valueless
   flag's mirror refuses any word but a boolean: a typo must not read as
   off. *)
let contribute p entry { var = source; layering } raw =
  match (entry.arg, layering) with
  | Flag set, _ -> (
      match Os.bool_of_string raw with
      | Some true -> Ok (set p)
      | Some false -> Ok p
      | None -> invalid ~source ~value:raw ~expected:Os.bool_expected)
  | Value { set; _ }, Single _ -> set ~source p (String.trim raw)
  | Value { set; _ }, Repeatable -> fold_ok (set ~source) p (Os.split_comma raw)
  | Optional_value { set; _ }, _ -> (
      match Os.bool_of_string raw with
      | Some true -> set ~source p None
      | Some false -> Ok p
      | None -> set ~source p (Some (String.trim raw)))

let layer_entries entries cli =
  let layer p entry =
    match entry.mirror with
    | None -> Ok p
    | Some { layering = Single absent; _ } when not (absent p) -> Ok p
    | Some mirror -> (
        match Os.getenv mirror.var with
        | None -> Ok p
        | Some raw -> contribute p entry mirror raw)
  in
  fold_ok layer cli entries

let selects p =
  p.filter <> [] || p.exclude <> [] || p.tags <> [] || p.exclude_tags <> []
  || p.shard <> None || p.failed_only = Some true

(* A relative path is made absolute before a test can chdir: the command
   line's against the working directory, and a mirror's against the project
   root, since each stanza runs in its own build directory. *)
let absolute_path ~cli ~layered field =
  let against base path =
    if not (Filename.is_relative path) then path
    else
      match base () with
      | base -> Filename.concat base path
      | exception Sys_error _ -> path
  in
  match (field cli, field layered) with
  | Some path, _ -> Some (against Sys.getcwd path)
  | None, Some path -> Some (against Os.project_root path)
  | None, None -> None

(* Every value arrived through its flag's parser, so nothing is range-checked
   here. [broadcast] keeps what the mirrors alone asked for: a mirror reaches
   every stanza of a project, and a stanza that cannot honour it is not in
   error. *)
let settings cli =
  let* layered = layer_entries entries cli in
  let* mutation =
    match (layered.mutate, layered.arm) with
    | None, None -> Ok Run.No_mutation
    | Some prefixes, None -> Ok (Run.Loop prefixes)
    | None, Some id -> Ok (Run.Armed id)
    | Some _, Some _ -> Error (Incompatible_flags ("--mutate", "--arm"))
  in
  let defaults = Run.default_config () in
  Ok
    {
      Run.seed = Option.value layered.seed ~default:defaults.Run.seed;
      filter = layered.filter;
      exclude = layered.exclude;
      tags = layered.tags;
      exclude_tags = layered.exclude_tags;
      shard = layered.shard;
      failed_only = layered.failed_only = Some true;
      bail = layered.bail = Some true;
      stream = layered.stream = Some true;
      baseline =
        (match (layered.update, layered.corrected) with
        | Some true, _ -> Baseline.Update
        | _, Some true -> Baseline.Corrected
        | _ -> Baseline.Check);
      timeout = layered.timeout;
      prop_count = layered.prop_count;
      log_dir =
        Option.value
          (absolute_path ~cli ~layered (fun p -> p.log_dir))
          ~default:defaults.Run.log_dir;
      (* Only a forked mutation child focuses, through [Run.for_subset]. *)
      allow_focus = false;
      color = Option.value layered.color ~default:defaults.Run.color;
      slow_threshold =
        Option.value layered.slow_threshold ~default:defaults.Run.slow_threshold;
      verbose = layered.verbose = Some true;
      junit = absolute_path ~cli ~layered (fun p -> p.junit);
      mutation;
      github = Os.in_github_actions ();
      (* The facade, which alone holds argv, computes it. *)
      invocation = `Mirrors;
      broadcast =
        {
          Run.selection = selects layered && not (selects cli);
          mutate = cli.mutate = None && layered.mutate <> None;
        };
    }

let color_mode () =
  let* p = layer_entries [ color_entry ] empty in
  Ok (Option.value p.color ~default:Os.Auto)

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
