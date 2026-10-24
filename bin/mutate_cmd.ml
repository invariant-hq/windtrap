(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Instr = Windtrap_runtime.Instr
module Mutate = Windtrap_runtime.Mutate
module Verdicts = Windtrap_runtime.Verdicts
module Sections = Windtrap.Private.Report_sections
module Os = Windtrap.Private.Os
module Cli = Windtrap.Private.Cli
module Pp = Windtrap.Private.Pp
module Run = Windtrap.Private.Run
module Test_tree = Windtrap.Private.Test_tree
module Lines = Windtrap.Private.Lines

let strf = Printf.sprintf
let ( let* ) = Result.bind

(* A step of [run] that ends the command says why on standard error and is
   [Error] of the exit code. *)
let fail ~code message =
  Os.say message;
  Error code

(* Files *)

type file = {
  label : string; (* the executable, as the witness column names it *)
  invocation : Run.invocation; (* how that executable is run again *)
  verdicts : Verdicts.t;
  refused : string option; (* why it could not test its mutants *)
}

(* Dune's inline-test runner is [inline-test-runner.exe] in every library's
   [.<lib>.inline-tests] directory, so the library is what tells one from
   another. *)
let inline_library exe =
  let dir = Filename.basename (Filename.dirname exe) in
  let suffix = ".inline-tests" in
  if
    Filename.basename exe = "inline-test-runner.exe"
    && String.length dir > String.length suffix + 1
    && dir.[0] = '.'
    && Filename.check_suffix dir suffix
  then Some (String.sub dir 1 (String.length dir - 1 - String.length suffix))
  else None

let label ~path = function
  | None -> Filename.basename path
  | Some ({ exe; _ } : Verdicts.identity) ->
      Option.value (inline_library exe) ~default:(Filename.basename exe)

(* [dune exec] takes a target below the build context, which is the first
   component of a relative identity. *)
let invocation = function
  | None -> `Mirrors
  | Some ({ exe; _ } : Verdicts.identity) ->
      if Option.is_some (inline_library exe) then `Mirrors
      else if Filename.is_relative exe then
        let target =
          match String.index_opt exe '/' with
          | Some i -> String.sub exe (i + 1) (String.length exe - i - 1)
          | None -> exe
        in
        Sections.dune_exec ~mutate:true target
      else `Exe (Sections.shell_word exe)

let no_data =
  "no .mutants files found\n\
   Instrument the library under test with ppx_windtrap.mutate and run every \
   suite with its mutants (--mutate) first; every mutation run writes its \
   verdicts under the build directory's _mutants or under _windtrap/mutants."

let all_excluded excluded =
  Data_files.all_excluded ~ext:"mutants" (List.map snd excluded)
  ^ "\n\
    \  A verdict is written only by a run asked to test its mutants, and it is\n\
    \  invalidated by any later build of the executable that wrote it."

(* A verdict file under a build directory was written by a suite that dune
   ran, whose command follows the remedy on a line of its own. Elsewhere the
   remedy is in words: this command does not know how the suite is run, and a
   run its build tool replays from the cache writes no verdict. *)
let remedy excluded =
  let under_build_dir (path, _) = Option.is_some (Instr.build_dir ~path) in
  if List.for_all under_build_dir excluded then
    "re-run every suite with its mutants, then merge again; delete the files \
     whose executable no longer exists\n\
    \  WINDTRAP_MUTATE=1 dune runtest --force --instrument-with \
     ppx_windtrap.mutate"
  else
    "re-run every suite with its mutants (--mutate, instrumented with \
     ppx_windtrap.mutate, forcing the runs your build tool cached), then merge \
     again; delete the files whose executable no longer exists"

let fresh files =
  let load path =
    match Verdicts.load path with
    | Ok { Verdicts.verdicts; identity; refused } ->
        Ok
          {
            label = label ~path identity;
            invocation = invocation identity;
            verdicts;
            refused;
          }
    | Error error -> Error (Pp.to_string Verdicts.pp_error error)
  in
  match Data_files.load_fresh Verdicts.format ~load files with
  | Error message -> fail ~code:1 message
  | Ok (kept, excluded) -> (
      List.iter Os.say (Data_files.warnings excluded);
      match (kept, excluded) with
      | [], [] -> fail ~code:1 no_data
      | [], excluded ->
          Os.say (all_excluded excluded);
          fail ~code:1 (remedy excluded)
      | kept, excluded ->
          if excluded <> [] then Os.say (remedy excluded);
          Ok kept)

(* Survivor sources are read once per file, under the first root that holds
   it. *)
let source ~roots =
  let sources = Hashtbl.create 16 in
  fun file ->
    match Hashtbl.find_opt sources file with
    | Some source -> source
    | None ->
        let read root = Instr.read_file (Filename.concat root file) in
        let source =
          List.find_map (fun root -> Result.to_option (read root)) roots
        in
        Hashtbl.add sources file source;
        source

(* The merge *)

module Ids = Map.Make (String)

(* The reaching tests of each mutant that survived in [verdicts], by the
   spelling of its identifier. *)
let survived_in verdicts =
  List.fold_left
    (fun acc (r : Verdicts.record) ->
      match r.verdict with
      | Survived { first; others } ->
          Ids.add (Mutate.id_to_string r.id) (first :: others) acc
      | Killed | Unknown _ | Not_evaluated | Outside_tests | Unreached -> acc)
    Ids.empty
    (Verdicts.records verdicts)

let verdicts_by_id f =
  List.fold_left
    (fun acc (r : Verdicts.record) ->
      Ids.add (Mutate.id_to_string r.id) r.verdict acc)
    Ids.empty
    (Verdicts.records f.verdicts)

(* [Verdicts.merge] folds away which executable ran a reaching test, so a
   survivor's reaching tests are read from the files in which it survived. *)
let merge ~source files =
  let merged =
    List.fold_left
      (fun acc f -> Verdicts.merge acc f.verdicts)
      Verdicts.empty files
  in
  let survivals = List.map (fun f -> (f, survived_in f.verdicts)) files in
  let by_id = List.map (fun f -> (f, verdicts_by_id f)) files in
  let verdict_in f id = Ids.find_opt id (List.assq f by_id) in
  let mutant (r : Verdicts.record) : Sections.mutant =
    {
      id = Mutate.id_to_string r.id;
      line = r.id.line;
      before = r.before;
      after = r.after;
      source = source r.id.file;
    }
  in
  let survivor (r : Verdicts.record) : Sections.survivor =
    let id = Mutate.id_to_string r.id in
    let witnesses =
      List.concat_map
        (fun (f, survived) ->
          match Ids.find_opt id survived with
          | Some tests ->
              List.map
                (fun test -> (f.label, Test_tree.path_to_string test))
                tests
          | None -> [])
        survivals
    in
    {
      mutant = mutant r;
      witnesses =
        List.map
          (fun (exe, test) -> { Sections.test; loc = None; exe = Some exe })
          (List.sort_uniq compare witnesses);
    }
  in
  (* The first executable, in the order of [files], that did not evaluate
     the mutant is the one its command runs. A merge is [Not_evaluated] only
     when a file is. *)
  let not_evaluated (r : Verdicts.record) : Sections.not_evaluated =
    let id = Mutate.id_to_string r.id in
    let missed f = verdict_in f id = Some Verdicts.Not_evaluated in
    let invocation =
      match List.find_opt missed files with
      | Some f -> f.invocation
      | None -> assert false
    in
    { mutant = mutant r; invocation }
  in
  (* A merge is [Unknown] only when a file is. The first executable that ran
     the mutant and could not judge it names it, with its reason; a mutant
     that only refused suites hold counts under the first of them. *)
  let unknown (r : Verdicts.record) =
    let id = Mutate.id_to_string r.id in
    let reason f =
      match verdict_in f id with
      | Some (Unknown reason) -> Some reason
      | _ -> None
    in
    let tested = List.filter (fun f -> Option.is_none f.refused) files in
    match
      List.find_map (fun f -> Option.map (fun r -> (f, r)) (reason f)) tested
    with
    | Some (f, reason) ->
        Either.Left
          {
            Sections.mutant = mutant r;
            reason = strf "%s: %s" f.label reason;
            invocation = f.invocation;
          }
    | None -> (
        match List.find_opt (fun f -> Option.is_some (reason f)) files with
        | Some f -> Either.Right f
        | None -> assert false)
  in
  let records = Verdicts.records merged in
  let pick f = List.filter_map f records in
  let site (r : Verdicts.record) = (r.id.file, r.id.line) in
  let survivors =
    pick (fun r ->
        match r.verdict with Survived _ -> Some (survivor r) | _ -> None)
  in
  let not_evaluated =
    pick (fun r ->
        match r.verdict with
        | Not_evaluated -> Some (not_evaluated r)
        | _ -> None)
  in
  let unknown, untested =
    List.partition_map Fun.id
      (pick (fun r ->
           match r.verdict with Unknown _ -> Some (unknown r) | _ -> None))
  in
  let unreached =
    pick (fun r ->
        match r.verdict with Unreached -> Some (site r) | _ -> None)
  in
  let outside_tests =
    pick (fun r ->
        match r.verdict with Outside_tests -> Some (site r) | _ -> None)
  in
  let killed =
    List.length (pick (fun r -> if r.verdict = Killed then Some () else None))
  in
  let refused =
    List.filter_map
      (fun f ->
        Option.map
          (fun reason ->
            {
              Sections.executable = f.label;
              reason;
              mutants = List.length (List.filter (( == ) f) untested);
            })
          f.refused)
      files
  in
  (* The survivor the most tests watched is the one a reader can act on
     soonest. The sort is stable, so identifier order holds within a count. *)
  let survivors =
    List.stable_sort
      (fun (a : Sections.survivor) (b : Sections.survivor) ->
        compare (List.length b.witnesses) (List.length a.witnesses))
      survivors
  in
  (* Only dune runs an inline-test runner, and it runs every suite. *)
  let invocation =
    match survivors with
    | [] -> `Mirrors
    | { mutant; witnesses } :: _ ->
        let runs (w : Sections.witness) =
          List.find_map
            (fun (f, survived) ->
              if
                Option.equal String.equal w.exe (Some f.label)
                && Ids.mem mutant.id survived
              then Some f.invocation
              else None)
            survivals
        in
        let alone w =
          match runs w with
          | Some (`Exe _ as exe) -> Some exe
          | Some `Mirrors | None -> None
        in
        Option.value ~default:`Mirrors (List.find_map alone witnesses)
  in
  ( {
      Sections.survivors;
      not_evaluated;
      unknown;
      refused;
      unreached;
      outside_tests;
      killed;
      not_tested = 0;
      scope = Executables (List.length files);
    },
    invocation )

(* A line scope keeps the records on its lines, in every file, before the
   merge: what it reports is what a run under the same scope would. *)
let on_lines files lines =
  let keep (r : Verdicts.record) =
    Lines.mem lines ~file:r.id.file ~line:r.id.line
  in
  List.map
    (fun f ->
      {
        f with
        verdicts =
          List.fold_left Verdicts.add Verdicts.empty
            (List.filter keep (Verdicts.records f.verdicts));
      })
    files

(* Running *)

let usage = "usage: windtrap mutants [OPTIONS] [PATH...]"

let help =
  "windtrap mutants - merge .mutants verdict files and report the survivors\n\n"
  ^ usage
  ^ {|

Merges the .mutants verdict files written by mutation runs and reports the
mutants that survived every test executable. Without PATH arguments the files
are found under the build directory's _mutants (or _windtrap/mutants in a tree
built without one), walking up from the current directory to the enclosing
project root; PATH arguments (.mutants files, or directories searched
recursively) replace that default.

Runs no tests and drives no build.
Exits 1 when any mutant survived every executable that reached it, or when an
executable could not mutation-test its suite.

OPTIONS:
  --lines=FILE:A-B,...
      Report only the mutants on these lines of these files.

  --diff=FILE
      Report only the mutants on the lines that FILE, the output of git diff
      -U0, adds or changes.

  --color=MODE (env WINDTRAP_COLOR)
      Color output: always, never or auto.

  -h, --help
      Print this help and exit.

ENVIRONMENT (no flag):
  NO_COLOR
      Any value: never style output (--color auto).|}

(* [--color] and the help flags are the options; any other argument that
   starts with [-] is refused, and the rest are PATHs. *)
let options args =
  let usage_error message =
    Os.say message;
    prerr_endline usage;
    Error 2
  in
  let add_lines lines more =
    Some (Lines.union (Option.value lines ~default:Lines.empty) more)
  in
  let rec parse color lines paths = function
    | [] -> Ok (color, lines, List.rev paths)
    | "--lines" :: value :: args -> (
        match Lines.of_spec value with
        | Ok more -> parse color (add_lines lines more) paths args
        | Error expected ->
            usage_error
              (strf "invalid value '%s' for --lines: expected %s" value expected)
        )
    | "--diff" :: file :: args -> (
        match In_channel.with_open_bin file In_channel.input_all with
        | text -> parse color (add_lines lines (Lines.of_diff text)) paths args
        | exception Sys_error reason ->
            usage_error (strf "cannot read --diff %s: %s" file reason))
    | [ (("--lines" | "--diff") as flag) ] ->
        usage_error (strf "option '%s' requires an argument" flag)
    | ("-h" | "--help" | "-help") :: _ ->
        print_endline help;
        Error 0
    | "--color" :: value :: args -> (
        match Cli.parse_color value with
        | Ok color -> parse (Some color) lines paths args
        | Error error -> usage_error (Cli.error_message error))
    | [ "--color" ] -> usage_error "option '--color' requires an argument"
    | arg :: _ when String.starts_with ~prefix:"-" arg ->
        usage_error (strf "unknown option '%s'" arg)
    | path :: args -> parse color lines (path :: paths) args
  in
  (* [--flag=VALUE] is [--flag VALUE]. *)
  let split arg =
    match String.index_opt arg '=' with
    | Some i
      when List.mem (String.sub arg 0 i) [ "--color"; "--lines"; "--diff" ] ->
        [
          String.sub arg 0 i; String.sub arg (i + 1) (String.length arg - i - 1);
        ]
    | Some _ | None -> [ arg ]
  in
  parse None None [] (List.concat_map split args)

(* [WINDTRAP_COLOR] is read when [--color] is absent, refused on the runner's
   terms and never read as [auto] out of a typo. *)
let ansi color =
  let color =
    match color with Some color -> Ok color | None -> Cli.color_mode ()
  in
  match color with
  | Error error -> fail ~code:2 (Cli.error_message error)
  | Ok color ->
      Ok
        (Os.resolve_color color ~tty:(Os.is_tty_stdout ())
           ~inside_dune:(Os.inside_dune ()) ~term_dumb:(Os.term_dumb ()))

let run args =
  let code =
    let* color, lines, paths = options args in
    let* ansi = ansi color in
    let* files, roots =
      match Data_files.discover Verdicts.format paths with
      | Ok found -> Ok found
      | Error message -> fail ~code:1 message
    in
    let* files = fresh files in
    let files = Option.fold ~none:files ~some:(on_lines files) lines in
    let mutation, invocation = merge ~source:(source ~roots) files in
    Sections.print ~out:Format.std_formatter ~ansi
      (Sections.mutation_report ~invocation mutation);
    Ok (if mutation.survivors = [] && mutation.refused = [] then 0 else 1)
  in
  match code with Ok code | Error code -> code
