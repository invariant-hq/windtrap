(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* ───── Initialization ───── *)

(* Capture initial directory at module load time, like ppx_expect. Forced on
   first use to get the cwd before any test changes it. *)
let initial_dir = lazy (Sys.getcwd ())
let () = Env.setup_color ()
let already_initialized = Atomic.make false

(* ───── Command Line Configuration ───── *)

let source_tree_root = ref None
let diff_cmd = ref None
let current_lib = ref None
let partition = ref None
let am_test_runner_flag = ref false

(* ───── Test State ───── *)

let tests_ran = ref 0
let tests_failed = ref 0

type location = { file : string; line : int; start_col : int; end_col : int }
type correction = { loc : location; expected : string option; actual : string }

type registered_test = {
  file : string;
  line : int;
  name : string option;
  fn : unit -> unit;
}

let registered_tests : registered_test list ref = ref []
let register_mutex = Mutex.create ()

let register_test ~file ~line ~name ~fn =
  Mutex.lock register_mutex;
  registered_tests := { file; line; name; fn } :: !registered_tests;
  Mutex.unlock register_mutex

(* ───── Corrections ───── *)

let corrections : (string, correction list) Hashtbl.t = Hashtbl.create 16
let corrections_mutex = Mutex.create ()

let record_correction ~file correction =
  Mutex.lock corrections_mutex;
  let existing =
    Hashtbl.find_opt corrections file |> Option.value ~default:[]
  in
  Hashtbl.replace corrections file (correction :: existing);
  Mutex.unlock corrections_mutex

let get_corrections ~file =
  Mutex.lock corrections_mutex;
  let corrs = Hashtbl.find_opt corrections file |> Option.value ~default:[] in
  Mutex.unlock corrections_mutex;
  List.rev corrs

let clear_corrections ~file =
  Mutex.lock corrections_mutex;
  Hashtbl.remove corrections file;
  Mutex.unlock corrections_mutex

(* Get the absolute path to a source file, exactly like ppx_expect. When running
   tests via dune, the file path from the PPX is relative to the build root
   (e.g., "example/example_ppx.ml"), but the initial_dir captured at runtime is
   the library's build directory (e.g., "_build/default/example"). Using
   basename avoids path duplication. *)
let absolute_path filename =
  if Filename.is_relative filename then
    Filename.concat (Lazy.force initial_dir) (Filename.basename filename)
  else filename

let am_test_runner () = !am_test_runner_flag

let make_absolute path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

(* ───── Argument Parsing ───── *)

let init argv =
  if Atomic.exchange already_initialized true then
    failwith "Windtrap: test runner can only be initialized once"
  else begin
    let args = Array.to_list argv in
    let rec parse = function
      | [] -> ()
      | "inline-test-runner" :: lib :: rest ->
          am_test_runner_flag := true;
          current_lib := Some lib;
          parse rest
      | "-source-tree-root" :: root :: rest ->
          (* The source_tree_root should be used as-is - it's relative to where
             dune promote expects to find corrected files. When running in a
             sandbox, this relative path needs to escape the sandbox, so we
             DON'T make it absolute. *)
          source_tree_root := Some root;
          parse rest
      | "-diff-cmd" :: cmd :: rest ->
          diff_cmd := Some cmd;
          parse rest
      | "-partition" :: p :: rest ->
          partition := Some p;
          Pp.epr
            "Warning: -partition flag is not yet supported by windtrap, \
             ignoring@.";
          parse rest
      | _ :: rest -> parse rest
    in
    parse args
  end

let set_lib lib = current_lib := Some lib

(* ───── Output Capture ───── *)

let current_trap : Log_trap.t option ref = ref None

(* Flush all formatters and raw channels, then drain unconsumed trap output. *)
let drain_trap () =
  match !current_trap with
  | None -> ""
  | Some trap ->
      Format.pp_print_flush Format.std_formatter ();
      Format.pp_print_flush Format.err_formatter ();
      flush stdout;
      flush stderr;
      let out = Log_trap.get_unconsumed trap in
      Log_trap.consume trap;
      out

let expect ~loc ~expected =
  let actual_raw = drain_trap () in
  let actual = Expect.normalize actual_raw in
  let expected_norm = Option.map Expect.normalize expected in
  let matches =
    match expected_norm with None -> actual = "" | Some exp -> actual = exp
  in
  if not matches then begin
    let corr : correction = { loc; expected; actual = actual_raw } in
    record_correction ~file:loc.file corr;
    let expected_str = Option.value ~default:"" expected_norm in
    Failure.raise_failure ~expected:expected_str ~actual "Output mismatch"
  end

let expect_exact ~loc ~expected =
  let actual = drain_trap () in
  let matches =
    match expected with None -> actual = "" | Some exp -> actual = exp
  in
  if not matches then begin
    let corr : correction = { loc; expected; actual } in
    record_correction ~file:loc.file corr;
    let expected_str = Option.value ~default:"" expected in
    Failure.raise_failure ~expected:expected_str ~actual
      "Output mismatch (exact)"
  end

let output () = drain_trap ()

let generate_run_id () =
  let t = Unix.gettimeofday () in
  let tm = Unix.localtime t in
  Printf.sprintf "%04d%02d%02d_%02d%02d%02d" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec

let run_single_test ~trap ~name ~line ~fn =
  current_trap := Some trap;
  Log_trap.reset_for_test trap;
  let test_name = Option.value ~default:(Printf.sprintf "line_%d" line) name in
  Log_trap.with_capture trap ~groups:[] ~test_name (fun () ->
      Expect.set_trap trap;
      Fun.protect
        ~finally:(fun () ->
          Expect.clear_trap ();
          current_trap := None)
        fn)

(* ───── Test Execution ───── *)

let execute_test { file; line; name; fn } =
  incr tests_ran;
  let test_name = Option.value ~default:(Printf.sprintf "line_%d" line) name in
  let run_id = generate_run_id () in
  let trap =
    Log_trap.create
      ~root:(Path_ops.default_log_dir ())
      ~suite_name:"inline" ~run_id ~enabled:true
  in
  begin try run_single_test ~trap ~name ~line ~fn with
  | Failure.Check_failure f ->
      incr tests_failed;
      Pp.epr "@[<v>FAIL %s:%d %s@,@,%a@]@." file line test_name Failure.pp f
  | Failure.Skip_test reason ->
      let reason_str = Option.value ~default:"" reason in
      Pp.epr "SKIP %s:%d %s%s@." file line test_name
        (if reason_str = "" then "" else ": " ^ reason_str)
  | exn ->
      incr tests_failed;
      Pp.epr "FAIL %s:%d %s@.Exception: %s@." file line test_name
        (Printexc.to_string exn)
  end

(* Always register unconditionally; am_test_runner is checked in [exit]. *)
let run_test ~file ~line ~name ~fn = register_test ~file ~line ~name ~fn

(* ───── Correction File Writing ───── *)

(* Find a safe delimiter tag for quoted strings. If the content contains |}
   (or the open/close delimiters for any candidate tag), we append "xxx" to the
   tag until no conflict remains. This matches ppx_expect's approach. *)
let find_safe_tag content =
  let rec try_tag tag =
    let open_delim = Printf.sprintf "{%s|" tag in
    let close_delim = Printf.sprintf "|%s}" tag in
    if
      Text.contains_substring ~pattern:open_delim content
      || Text.contains_substring ~pattern:close_delim content
    then try_tag (tag ^ "xxx")
    else tag
  in
  try_tag ""

(* Write a .corrected file for a single source file by splicing actual output
   into the expect nodes. Reads from the sandbox and writes alongside it so
   dune's diff action can pick up the corrected version. *)
let write_corrected_file ~file =
  let corrs = get_corrections ~file in
  if corrs = [] then ()
  else begin
    let abs_file = absolute_path file in
    let ic = open_in abs_file in
    let content =
      Fun.protect
        ~finally:(fun () -> close_in ic)
        (fun () -> really_input_string ic (in_channel_length ic))
    in
    let lines = String.split_on_char '\n' content in
    let lines_arr = Array.of_list lines in
    (* Sort corrections in reverse line order so that earlier corrections
       (which may insert multi-line content) don't shift later line numbers *)
    let sorted_corrs =
      List.sort (fun a b -> compare b.loc.line a.loc.line) corrs
    in
    List.iter
      (fun { loc; actual; _ } ->
        if loc.line > 0 && loc.line <= Array.length lines_arr then begin
          let line_content = lines_arr.(loc.line - 1) in
          let before = String.sub line_content 0 loc.start_col in
          let after =
            if loc.end_col < String.length line_content then
              String.sub line_content loc.end_col
                (String.length line_content - loc.end_col)
            else ""
          in
          let tag = find_safe_tag actual in
          let actual_escaped = Printf.sprintf "{%s|%s|%s}" tag actual tag in
          lines_arr.(loc.line - 1) <- before ^ actual_escaped ^ after
        end)
      sorted_corrs;
    (* Write to cwd (the sandbox) as basename + ".corrected". Dune's diff
       action expects the corrected file adjacent to the source inside the
       sandbox, e.g. _build/.sandbox/<hash>/default/example/foo.ml.corrected *)
    let corrected_file = Filename.basename file ^ ".corrected" in
    let oc = open_out corrected_file in
    Fun.protect
      ~finally:(fun () -> close_out oc)
      (fun () ->
        Array.iteri
          (fun i line ->
            output_string oc line;
            if i < Array.length lines_arr - 1 then output_char oc '\n')
          lines_arr);
    clear_corrections ~file
  end

(* ───── Entry Point ───── *)

let exit () =
  if not (am_test_runner ()) then Stdlib.exit 0
  else begin
    (* Tests are prepended during registration; reverse to run in source order *)
    let tests = List.rev !registered_tests in
    List.iter execute_test tests;

    (* Restore initial cwd so .corrected files land where dune expects them *)
    Sys.chdir (Lazy.force initial_dir);
    Mutex.lock corrections_mutex;
    let files = Hashtbl.fold (fun file _ acc -> file :: acc) corrections [] in
    Mutex.unlock corrections_mutex;
    List.iter (fun file -> write_corrected_file ~file) files;

    (* Exit with appropriate code. IMPORTANT: When corrections exist, we MUST
       exit with 0 so that dune's Action.progn continues to the diff actions.
       The diff action is what registers files for promotion. If we exit
       non-zero, progn stops and the diff never runs. *)
    let has_corrections = files <> [] in
    let exit_code =
      if has_corrections then 0 (* Let diff action run and handle promotion *)
      else if !tests_failed > 0 then 1 (* Test failures without corrections *)
      else 0
    in
    Stdlib.exit exit_code
  end

(* ───── Module-Based Test Syntax ───── *)

(* A frame in the test group tree being built *)
type test_frame = { name : string; mutable tests : Test.t list }

(* Stack of group frames - top is current group *)
let group_stack : test_frame list ref = ref []

(* Top-level tests (outside any group) *)
let top_level_tests : Test.t list ref = ref []

let add_test name fn =
  let t = Test.test name fn in
  match !group_stack with
  | [] -> top_level_tests := t :: !top_level_tests
  | frame :: _ -> frame.tests <- t :: frame.tests

let enter_group name = group_stack := { name; tests = [] } :: !group_stack

let leave_group () =
  match !group_stack with
  | [] -> failwith "Windtrap.Ppx_runtime.leave_group: no group to leave"
  | frame :: rest -> (
      let group = Test.group frame.name (List.rev frame.tests) in
      group_stack := rest;
      match rest with
      | [] -> top_level_tests := group :: !top_level_tests
      | parent :: _ -> parent.tests <- group :: parent.tests)

let run_tests name =
  if !group_stack <> [] then
    failwith "Windtrap.Ppx_runtime.run_tests: unclosed test groups";
  let tests = List.rev !top_level_tests in
  top_level_tests := [];

  let cli = Cli.parse Sys.argv in
  if cli.list_only = Some true then begin
    Runner.list_tests name tests;
    Stdlib.exit 0
  end;

  let config = Cli.resolve_config cli in
  let result = Runner.run ~config name tests in
  if result.Runner.failed > 0 then Stdlib.exit 1
