(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* ───── Initialization ───── *)

(* Capture the cwd eagerly at module load time (before any test code runs).
   Tests may chdir during execution; we need the original cwd later to write
   .corrected files where dune expects them. The lazy wrapper is kept so that
   downstream code can continue using [Lazy.force]; the actual Sys.getcwd ()
   call happens here, at module initialisation. *)
let initial_dir =
  let dir = Sys.getcwd () in
  lazy dir

let () = Env.setup_color ()
let already_initialized = Atomic.make false

(* ───── Command Line Configuration ───── *)

let source_tree_root = ref None
let diff_cmd = ref None
let current_lib = ref None
let partition = ref None
let list_partitions_only = ref false
let am_test_runner_flag = ref false
let inline_suite_name = ref None

(* Must detect inline-test-runner mode eagerly: [%%run_tests] executes at
   module load time and needs am_test_runner_flag to decide whether to run
   tests immediately or defer to exit(). Full arg parsing happens later in
   init(), but this flag must already be set. *)
let () =
  let args = Array.to_list Sys.argv in
  let rec detect = function
    | "inline-test-runner" :: _ -> am_test_runner_flag := true
    | _ :: rest -> detect rest
    | [] -> ()
  in
  detect args

(* ───── Corrections ───── *)

type location = {
  file : string;
  line : int;
  start_col : int;
  end_line : int;
  end_col : int;
}

type correction =
  | Update_expect_payload of { loc : location; actual : string }
  | Replace_node of { loc : location; replacement : string }

let raise_output_mismatch message ~expected ~actual =
  let diff =
    Myers.diff ~expected_label:"expected" ~actual_label:"actual" expected actual
    |> Diff_display.colorize_unified_diff
  in
  Failure.raise_failure (Pp.str "%s@.@.%s" message diff)

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

module String_set = Set.Make (String)

let discovered_partitions : String_set.t ref = ref String_set.empty
let expected_must_reach_count = ref 0
let reached_must_reach_count = ref 0

(* When running via dune, the PPX-emitted file path is relative to the build
   root (e.g., "example/example_ppx.ml"), but initial_dir is the library's
   build directory (e.g., "_build/default/example"). Using basename avoids
   path duplication like "_build/default/example/example/example_ppx.ml". *)
let absolute_path filename =
  if Filename.is_relative filename then
    Filename.concat (Lazy.force initial_dir) (Filename.basename filename)
  else filename

let am_test_runner () = !am_test_runner_flag

let should_record_corrections () =
  !am_test_runner_flag || Lazy.force Env.inside_dune

let list_partitions () =
  String_set.elements !discovered_partitions |> List.iter print_endline

(* ───── Argument Parsing ───── *)

let init argv =
  if Atomic.exchange already_initialized true then ()
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
          parse rest
      | "-list-partitions" :: rest ->
          list_partitions_only := true;
          parse rest
      | _ :: rest -> parse rest
    in
    parse args
  end

let set_lib lib = current_lib := Some lib

(* ───── Expectation Checking ───── *)

let expect_internal ~count_towards_reachability ~loc ~expected =
  if count_towards_reachability then incr reached_must_reach_count;
  let actual_raw = Expect.output () in
  let actual = Expect.normalize actual_raw in
  let expected_norm = Option.map Expect.normalize expected in
  let matches =
    match expected_norm with None -> actual = "" | Some exp -> actual = exp
  in
  if not matches then begin
    if should_record_corrections () then begin
      let corr : correction = Update_expect_payload { loc; actual } in
      record_correction ~file:loc.file corr
    end;
    let expected_str = Option.value ~default:"" expected_norm in
    raise_output_mismatch "Output mismatch" ~expected:expected_str ~actual
  end

let expect ~loc ~expected =
  expect_internal ~count_towards_reachability:true ~loc ~expected

let expect_exact ~loc ~expected =
  incr reached_must_reach_count;
  let actual = Expect.output () in
  let matches =
    match expected with None -> actual = "" | Some exp -> actual = exp
  in
  if not matches then begin
    if should_record_corrections () then begin
      let corr : correction = Update_expect_payload { loc; actual } in
      record_correction ~file:loc.file corr
    end;
    let expected_str = Option.value ~default:"" expected in
    raise_output_mismatch "Output mismatch (exact)" ~expected:expected_str
      ~actual
  end

let output () = Expect.output ()

(* ───── Correction File Writing ───── *)

(* Find a delimiter tag for quoted strings that doesn't conflict with the
   content. If the content contains |} or the delimiters for any candidate
   tag, we append "xxx" until no conflict remains. *)
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

let loc_of_correction = function
  | Update_expect_payload { loc; _ } -> loc
  | Replace_node { loc; _ } -> loc

let format_expect_node actual =
  let tag = find_safe_tag actual in
  let payload = Printf.sprintf "{%s|%s|%s}" tag actual tag in
  Printf.sprintf "[%%expect %s]" payload

let check_trailing_output ~trailing_loc =
  let trailing_raw = Expect.output () in
  let trailing_norm = Expect.normalize trailing_raw in
  if trailing_norm <> "" then begin
    if should_record_corrections () then begin
      let replacement = ";\n  " ^ format_expect_node trailing_norm in
      record_correction ~file:trailing_loc.file
        (Replace_node { loc = trailing_loc; replacement })
    end;
    raise_output_mismatch "Trailing output not matched by [%expect]"
      ~expected:"" ~actual:trailing_norm
  end

let check_must_reach_expect_nodes () =
  if !reached_must_reach_count <> !expected_must_reach_count then
    Failure.raise_failure
      (Printf.sprintf
         "Expected %d reachable [%%expect] / [%%expect_exact] nodes, but \
          reached %d"
         !expected_must_reach_count !reached_must_reach_count)

let run_expect_test ~must_reach_count ~trailing_loc fn =
  expected_must_reach_count := must_reach_count;
  reached_must_reach_count := 0;
  Fun.protect
    ~finally:(fun () ->
      expected_must_reach_count := 0;
      reached_must_reach_count := 0)
    (fun () ->
      fn ();
      check_trailing_output ~trailing_loc;
      check_must_reach_expect_nodes ())

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
    let line_offsets =
      let offsets = ref [ 0 ] in
      String.iteri
        (fun i ch -> if ch = '\n' then offsets := (i + 1) :: !offsets)
        content;
      Array.of_list (List.rev !offsets)
    in
    let content_len = String.length content in
    let to_abs_offset ~line col =
      if line <= 0 || line > Array.length line_offsets then None
      else
        let line_start = line_offsets.(line - 1) in
        let abs = line_start + col in
        Some (max 0 (min content_len abs))
    in
    let edits =
      List.filter_map
        (fun correction ->
          let loc, replacement =
            match correction with
            | Update_expect_payload { loc; actual } ->
                let tag = find_safe_tag actual in
                let actual_escaped =
                  Printf.sprintf "{%s|%s|%s}" tag actual tag
                in
                (loc, actual_escaped)
            | Replace_node { loc; replacement } -> (loc, replacement)
          in
          match
            ( to_abs_offset ~line:loc.line loc.start_col,
              to_abs_offset ~line:loc.end_line loc.end_col )
          with
          | Some start_pos, Some end_pos when start_pos <= end_pos ->
              Some (start_pos, end_pos, replacement)
          | _ -> None)
        corrs
      |> List.sort (fun (a, _, _) (b, _, _) -> compare a b)
    in
    let patched =
      let buf = Buffer.create (String.length content) in
      let rec apply cursor = function
        | [] ->
            if cursor < content_len then
              Buffer.add_substring buf content cursor (content_len - cursor)
        | (start_pos, end_pos, replacement) :: rest ->
            if start_pos < cursor then apply cursor rest
            else begin
              Buffer.add_substring buf content cursor (start_pos - cursor);
              Buffer.add_string buf replacement;
              apply end_pos rest
            end
      in
      apply 0 edits;
      Buffer.contents buf
    in
    (* Write to cwd (the sandbox) as basename + ".corrected". Dune's diff
       action expects the corrected file adjacent to the source inside the
       sandbox, e.g. _build/.sandbox/<hash>/default/example/foo.ml.corrected *)
    let corrected_file = Filename.basename file ^ ".corrected" in
    let oc = open_out corrected_file in
    Fun.protect
      ~finally:(fun () -> close_out oc)
      (fun () -> output_string oc patched);
    clear_corrections ~file
  end

(* ───── Module-Based Test Syntax ───── *)

let module_name_of_file file =
  let base = Filename.basename file in
  match String.index_opt base '.' with
  | Some i -> String.capitalize_ascii (String.sub base 0 i)
  | None -> String.capitalize_ascii base

(* A frame in the test group tree being built *)
type test_frame = {
  name : string;
  tags : Tag.t;
  file_module : string option;
  partition_name : string option;
  mutable tests : Test.t list;
}

let group_stack : test_frame list ref = ref []

type test_entry = {
  file_module : string option;
  partition_name : string option;
  test : Test.t;
}

let top_level_tests : test_entry list ref = ref []

let add_test ?file ?(tags = []) name fn =
  let partition_name = Option.map Filename.basename file in
  Option.iter
    (fun p -> discovered_partitions := String_set.add p !discovered_partitions)
    partition_name;
  let t = Test.test ~tags:(Tag.labels tags) name fn in
  let file_module = Option.map module_name_of_file file in
  match !group_stack with
  | [] ->
      top_level_tests :=
        { file_module; partition_name; test = t } :: !top_level_tests
  | frame :: _ -> frame.tests <- t :: frame.tests

let enter_group ?file ?(tags = []) name =
  let file_module = Option.map module_name_of_file file in
  let partition_name = Option.map Filename.basename file in
  Option.iter
    (fun p -> discovered_partitions := String_set.add p !discovered_partitions)
    partition_name;
  group_stack :=
    { name; tags = Tag.labels tags; file_module; partition_name; tests = [] }
    :: !group_stack

let leave_group () =
  match !group_stack with
  | [] -> failwith "Windtrap.Ppx_runtime.leave_group: no group to leave"
  | frame :: rest -> (
      let group =
        Test.group ~tags:frame.tags frame.name (List.rev frame.tests)
      in
      group_stack := rest;
      match rest with
      | [] ->
          top_level_tests :=
            {
              file_module = frame.file_module;
              partition_name = frame.partition_name;
              test = group;
            }
            :: !top_level_tests
      | parent :: _ -> parent.tests <- group :: parent.tests)

(* Collect registered tests, grouping top-level tests by their source file
   module. Tests from the same file are wrapped in a [Test.group] named after
   the OCaml module (e.g., "my_file.ml" → "My_file"). Tests without file
   info (manual registrations) remain ungrouped at the top level.

   Partition filtering is applied here (not at registration time) because
   init() — which sets the partition ref — runs after test modules have loaded
   and registered their tests via add_test. *)
let collect_tests () =
  let entries = List.rev !top_level_tests in
  top_level_tests := [];
  let entries =
    match !partition with
    | None -> entries
    | Some wanted ->
        List.filter
          (fun { partition_name; _ } ->
            match partition_name with
            | None -> true
            | Some p -> String.equal wanted p)
          entries
  in
  let file_map : (string, Test.t list ref) Hashtbl.t = Hashtbl.create 16 in
  let file_order = ref [] in
  let ungrouped = ref [] in
  List.iter
    (fun { file_module; test; _ } ->
      match file_module with
      | None -> ungrouped := test :: !ungrouped
      | Some m -> (
          match Hashtbl.find_opt file_map m with
          | Some tests -> tests := test :: !tests
          | None ->
              file_order := m :: !file_order;
              Hashtbl.add file_map m (ref [ test ])))
    entries;
  let file_groups =
    List.rev !file_order
    |> List.map (fun m ->
        let tests = List.rev !(Hashtbl.find file_map m) in
        Test.group m tests)
  in
  List.rev !ungrouped @ file_groups

(* ───── Correction Flushing ───── *)

(* Write all pending .corrected files and return whether any were written.
   Restores cwd to initial_dir so files land where dune expects them. *)
let flush_corrections () =
  Sys.chdir (Lazy.force initial_dir);
  Mutex.lock corrections_mutex;
  let files = Hashtbl.fold (fun file _ acc -> file :: acc) corrections [] in
  Mutex.unlock corrections_mutex;
  List.iter (fun file -> write_corrected_file ~file) files;
  files <> []

(* ───── Test Execution ───── *)

(* Auto-execute tests when the program exits without an explicit [%%run_tests].
   Safe no-ops: inline mode (exit() handles it), [%%run_tests] was called
   (top_level_tests already cleared), no PPX tests registered (empty list). *)
let () =
  at_exit (fun () ->
      if am_test_runner () then ()
      else
        let tests = collect_tests () in
        if tests <> [] then begin
          let name = Option.value ~default:"Tests" !inline_suite_name in
          let cli = Cli.parse Sys.argv in
          if cli.list_only = Some true then Runner.list_tests name tests
          else begin
            let config = Cli.resolve_config cli in
            let result = Runner.run ~config name tests in
            if Lazy.force Env.inside_dune then begin
              let has_corrections = flush_corrections () in
              if (not has_corrections) && result.Runner.failed > 0 then
                Stdlib.exit 1
            end
            else if result.Runner.failed > 0 then Stdlib.exit 1
          end
        end)

let run_tests name =
  if !group_stack <> [] then
    failwith "Windtrap.Ppx_runtime.run_tests: unclosed test groups";
  if !list_partitions_only then begin
    list_partitions ();
    Stdlib.exit 0
  end;
  if am_test_runner () then
    (* Inline mode: store suite name for exit() to use. Cli.parse cannot handle
       the inline-test-runner flags, so we defer execution to exit(). *)
    inline_suite_name := Some name
  else begin
    let tests = collect_tests () in

    let cli = Cli.parse Sys.argv in
    if cli.list_only = Some true then begin
      Runner.list_tests name tests;
      Stdlib.exit 0
    end;

    let config = Cli.resolve_config cli in
    let result = Runner.run ~config name tests in
    if Lazy.force Env.inside_dune then begin
      let has_corrections = flush_corrections () in
      if (not has_corrections) && result.Runner.failed > 0 then Stdlib.exit 1
    end
    else if result.Runner.failed > 0 then Stdlib.exit 1
  end

(* ───── Entry Point ───── *)

let exit () =
  if not (am_test_runner ()) then Stdlib.exit 0
  else begin
    if !list_partitions_only then begin
      list_partitions ();
      Stdlib.exit 0
    end;
    let tests = collect_tests () in
    if tests = [] then Stdlib.exit 0;

    let suite_name = Option.value ~default:"Tests" !inline_suite_name in
    let config = Cli.resolve_config Cli.empty in
    let result = Runner.run ~config suite_name tests in

    let has_corrections = flush_corrections () in

    (* IMPORTANT: When corrections exist, we MUST exit with 0 so that dune's
       Action.progn continues to the diff actions. The diff action is what
       registers files for promotion. If we exit non-zero, progn stops and the
       diff never runs. *)
    let exit_code =
      if has_corrections then 0 (* Let diff action run and handle promotion *)
      else if result.Runner.failed > 0 then 1
      else 0
    in
    Stdlib.exit exit_code
  end
