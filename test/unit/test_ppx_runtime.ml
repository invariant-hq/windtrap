(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Os = Windtrap.Private.Os
module Test_tree = Windtrap.Private.Test_tree
module Tag = Windtrap.Private.Test_tree.Tag
module Ppx_runtime = Ppx_windtrap_runtime.Ppx_runtime
module Scratch = Windtrap_test_support.Scratch

let strf = Printf.sprintf
let pos file = (file, 1, 0, 0)

let add ?library ?(tags = []) ~file name fn =
  Ppx_runtime.add_test ?library ~file ~pos:(pos file) ~tags name fn

let read path = In_channel.with_open_bin path In_channel.input_all

(* Children *)

(* [exit] ends the process, so what it does is judged on a forked child. Every
   child is forked as the module initialises, before this process registers
   anything, so it starts with an empty registry and no guard installed. *)

type child = { dir : string; status : string; out : string; err : string }

let status = function
  | Unix.WEXITED code -> strf "exit %d" code
  | Unix.WSIGNALED signal -> strf "killed by %d" signal
  | Unix.WSTOPPED signal -> strf "stopped by %d" signal

let redirect path fd =
  let file = Unix.openfile path [ Unix.O_WRONLY; O_CREAT; O_TRUNC ] 0o600 in
  Unix.dup2 file fd;
  Unix.close file

(* The stated environment: every [WINDTRAP_*] variable, [CI],
   [GITHUB_ACTIONS], [INSIDE_DUNE], [NO_COLOR] and [TERM] unset. *)
let state_the_environment () =
  let windtrap binding =
    match String.index_opt binding '=' with
    | Some i when String.starts_with ~prefix:"WINDTRAP_" binding ->
        Some (String.sub binding 0 i)
    | Some _ | None -> None
  in
  List.iter
    (fun name -> Os.setenv name None)
    ([ "CI"; "GITHUB_ACTIONS"; "INSIDE_DUNE"; "NO_COLOR"; "TERM" ]
    @ List.filter_map windtrap (Array.to_list (Unix.environment ())))

(* The buffers are flushed before the fork, so the child does not write them
   again. A scenario ends the child itself; one that returns exits 3. *)
let child scenario =
  if Sys.win32 then None
  else begin
    let dir = Scratch.dir "windtrap-inline-" in
    let out = Filename.concat dir "out" and err = Filename.concat dir "err" in
    flush_all ();
    match Unix.fork () with
    | 0 ->
        state_the_environment ();
        redirect out Unix.stdout;
        redirect err Unix.stderr;
        scenario dir;
        exit 3
    | pid ->
        let _, ended = Unix.waitpid [] pid in
        Some { dir; status = status ended; out = read out; err = read err }
  end

let ended = function
  | Some child -> child
  | None -> skip ~reason:"no fork on Windows" ()

let speak argv =
  Ppx_runtime.init (Array.of_list ("child" :: argv));
  Ppx_runtime.exit ()

let logs dir = Filename.concat dir "logs"
let output_to dir = Os.setenv "WINDTRAP_OUTPUT" (Some (logs dir))

let listing =
  child (fun _ ->
      add ~file:"src/b.ml" "b" ignore;
      add ~file:"src/a.ml" "a" ignore;
      add ~file:"src/a.ml" "a2" ignore;
      add ~library:"lib" ~file:"src/c.ml" "c" ignore;
      add ~library:"dep" ~file:"src/d.ml" "d" ignore;
      speak [ "inline-test-runner"; "lib"; "-list-partitions" ])

let listing_files =
  child (fun _ ->
      add ~file:"src/dup.ml" "one" ignore;
      add ~file:"test/dup.ml" "two" ignore;
      add ~file:"gen/x.pp.ml" "three" ignore;
      Ppx_runtime.enter_group ~file:"host.ml" ~tags:[] "G";
      add ~file:"guest.ml" "t" ignore;
      Ppx_runtime.leave_group ();
      add ~file:"kept.ml" "t" ignore;
      ignore (Ppx_runtime.collect ());
      speak [ "inline-test-runner"; "lib"; "-list-partitions" ])

let partition =
  child (fun dir ->
      output_to dir;
      add ~file:"a.ml" "passes" ignore;
      add ~file:"a.ml" "fails" (fun () -> equal int 1 2);
      add ~file:"b.ml" "other partition" (fun () -> fail "unrun");
      speak [ "inline-test-runner"; "lib"; "-partition"; "a.ml" ])

let junit =
  child (fun dir ->
      output_to dir;
      Os.setenv "WINDTRAP_JUNIT" (Some (Filename.concat dir "junit"));
      add ~file:"a.ml" "passes" ignore;
      speak [ "inline-test-runner"; "lib"; "-partition"; "a.ml" ])

let never_initialised = child (fun _ -> Ppx_runtime.exit ())

let listed_by_hand =
  child (fun _ ->
      add ~file:"a.ml" "t" ignore;
      speak [ "-list-partitions" ])

let bad_mirror =
  child (fun _ ->
      Os.setenv "WINDTRAP_TIMEOUT" (Some "banana");
      add ~file:"a.ml" "t" ignore;
      speak [ "inline-test-runner"; "lib"; "-partition"; "a.ml" ])

let empty_partition =
  child (fun dir ->
      output_to dir;
      add ~file:"a.ml" "t" ignore;
      speak [ "inline-test-runner"; "lib"; "-partition"; "c.ml" ])

let undriven =
  child (fun _ ->
      add ~file:"a.ml" "never driven" ignore;
      Stdlib.exit 0)

let undriven_files =
  child (fun _ ->
      add ~file:"src/b.ml" "one" ignore;
      add ~file:"a.ml" "two" ignore;
      add ~file:"a.ml" "three" ignore;
      add ~library:"lib" ~file:"c.ml" "the library's" ignore;
      Ppx_runtime.enter_group ~file:"g.ml" ~tags:[] "G";
      Ppx_runtime.leave_group ();
      Stdlib.exit 0)

let undriven_failing =
  child (fun _ ->
      add ~file:"a.ml" "never driven" ignore;
      Stdlib.exit 1)

let drained =
  child (fun _ ->
      add ~file:"a.ml" "drained" ignore;
      ignore (Ppx_runtime.collect ());
      Stdlib.exit 0)

let drain_raised =
  child (fun _ ->
      add ~file:"a.ml" "drained" ignore;
      Ppx_runtime.enter_group ~file:"a.ml" ~tags:[] "Open";
      (try ignore (Ppx_runtime.collect ()) with Invalid_argument _ -> ());
      Stdlib.exit 0)

let own_suite =
  child (fun dir ->
      output_to dir;
      add ~file:"a.ml" "undrained" ignore;
      Stdlib.exit (run ~argv:[| "own" |] "own" [ test "ran" ignore ]))

let linked_library =
  child (fun dir ->
      output_to dir;
      add ~library:"lib" ~file:"a.ml" "the library's" ignore;
      Ppx_runtime.enter_group ~library:"lib" ~file:"a.ml" ~tags:[] "G";
      add ~library:"lib" ~file:"a.ml" "in a group" ignore;
      Ppx_runtime.leave_group ();
      Stdlib.exit (run ~argv:[| "suite" |] "suite" [ test "ran" ignore ]))

let forks =
  child (fun _ ->
      add ~file:"a.ml" "never driven" ignore;
      (match Unix.fork () with
      | 0 -> Stdlib.exit 0
      | pid ->
          let _, ended = Unix.waitpid [] pid in
          print_endline ("forked child: " ^ status ended));
      speak [])

(* Registered before the guard, so it runs after it. *)
let earlier_at_exit =
  child (fun _ ->
      at_exit (fun () -> print_endline "the earlier at_exit function ran");
      add ~file:"a.ml" "never driven" ignore;
      Stdlib.exit 0)

(* Registration *)

(* The registry is the process's: each test starts from a drained registry,
   outside the runner mode and with no partition. *)
let reset () =
  Ppx_runtime.init [| "test_ppx_runtime" |];
  ignore (Ppx_runtime.collect ())

let paths tests =
  List.map
    (fun (case : Test_tree.case) -> Test_tree.path_to_string case.path)
    (Test_tree.flatten tests)

let collected () = paths (Ppx_runtime.collect ())

let in_a_group ?library ~file ?(tags = []) name register =
  Ppx_runtime.enter_group ?library ~file ~tags name;
  register ();
  Ppx_runtime.leave_group ()

let files_group_under_their_module () =
  reset ();
  add ~file:"src/parser.ml" "first" ignore;
  add ~file:"src/lexer.ml" "second" ignore;
  add ~file:"src/parser.ml" "third" ignore;
  equal (list string)
    [ "Parser › first"; "Parser › third"; "Lexer › second" ]
    (collected ())

let a_basename_is_one_module () =
  reset ();
  add ~file:"src/dup.ml" "one" ignore;
  add ~file:"test/dup.ml" "two" ignore;
  add ~file:"gen/x.pp.ml" "three" ignore;
  equal (list string) [ "Dup › one"; "Dup › two"; "X › three" ] (collected ())

let a_taken_name_is_numbered () =
  reset ();
  add ~file:"f.ml" "instance" ignore;
  add ~file:"f.ml" "instance" ignore;
  add ~file:"f.ml" "instance" ignore;
  in_a_group ~file:"f.ml" "G" (fun () ->
      add ~file:"f.ml" "instance" ignore;
      add ~file:"f.ml" "instance" ignore);
  equal (list string)
    [
      "F › instance";
      "F › instance (2)";
      "F › instance (3)";
      "F › G › instance";
      "F › G › instance (2)";
    ]
    (collected ())

let a_taken_group_name_is_numbered () =
  reset ();
  in_a_group ~file:"twice.ml" "G" (fun () -> add ~file:"twice.ml" "t" ignore);
  in_a_group ~file:"twice.ml" "G" (fun () -> add ~file:"twice.ml" "t" ignore);
  equal (list string) [ "Twice › G › t"; "Twice › G (2) › t" ] (collected ())

let nested () =
  in_a_group ~file:"n.ml" ~tags:[ "outer" ] "Outer" (fun () ->
      add ~file:"n.ml" "in outer" ignore;
      in_a_group ~file:"n.ml" "Inner" (fun () ->
          add ~tags:[ "own" ] ~file:"n.ml" "in inner" ignore));
  add ~file:"n.ml" "after" ignore

let groups_nest () =
  reset ();
  nested ();
  equal (list string)
    [ "N › Outer › in outer"; "N › Outer › Inner › in inner"; "N › after" ]
    (collected ())

let tags_reach_the_tests_below () =
  reset ();
  nested ();
  let row (case : Test_tree.case) =
    let carried =
      List.filter (fun t -> Tag.mem t case.tags) [ "outer"; "own" ]
    in
    (Test_tree.path_to_string case.path, carried)
  in
  equal
    (list (pair string (list string)))
    [
      ("N › Outer › in outer", [ "outer" ]);
      ("N › Outer › Inner › in inner", [ "outer"; "own" ]);
      ("N › after", []);
    ]
    (List.map row (Test_tree.flatten (Ppx_runtime.collect ())))

let a_group_holds_its_tests_whatever_their_file () =
  reset ();
  in_a_group ~file:"host.ml" "G" (fun () -> add ~file:"guest.ml" "t" ignore);
  equal (list string) [ "Host › G › t" ] (collected ())

let a_group_lands_in_its_own_library () =
  reset ();
  in_a_group ~library:"dep" ~file:"dep.ml" "G" (fun () ->
      add ~file:"own.ml" "in dep's group" ignore);
  add ~file:"own.ml" "own" ignore;
  Ppx_runtime.init [| "runner"; "inline-test-runner"; "lib" |];
  equal (list string) [ "Own › own" ] (collected ())

let registration =
  group "Registration"
    [
      test "a file's tests group under its module, in first-registration order"
        files_group_under_their_module;
      test
        "the module is the basename up to its first dot, whatever the directory"
        a_basename_is_one_module;
      test "a name its scope holds is numbered, at the top level and in a group"
        a_taken_name_is_numbered;
      test "a group name its scope holds is numbered"
        a_taken_group_name_is_numbered;
      test "groups nest under the file's module" groups_nest;
      test "a group's tags reach the tests under it, beside their own"
        tags_reach_the_tests_below;
      test "a test lands in the open group whatever its file"
        a_group_holds_its_tests_whatever_their_file;
      test "a group lands in the library that enter_group named"
        a_group_lands_in_its_own_library;
      test "leave_group raises Invalid_argument when no group is open"
        (fun () ->
          reset ();
          raises_match Exn.invalid_arg Ppx_runtime.leave_group);
    ]

(* Collecting *)

let a_second_collection_is_empty () =
  reset ();
  add ~file:"a.ml" "t" ignore;
  ignore (Ppx_runtime.collect ());
  equal (list string) [] (collected ());
  add ~file:"a.ml" "t" ignore;
  equal (list string) [ "A › t" ] (collected ())

let an_open_group_refuses_collect () =
  reset ();
  Ppx_runtime.enter_group ~file:"u.ml" ~tags:[] "Open";
  add ~file:"u.ml" "t" ignore;
  let refused =
    match Ppx_runtime.collect () with _ -> None | exception e -> Some e
  in
  Ppx_runtime.leave_group ();
  raises_match Exn.invalid_arg (fun () -> Option.iter raise refused);
  equal (list string) [ "U › Open › t" ] (collected ())

let untagged_name_of_a_module =
  [ "Plain"; "plain"; "plain.ml"; "src/plain.ml"; "prop"; "slow"; "" ]

let the_module_adds_no_tag name =
  reset ();
  add ~file:"src/plain.ml" "untagged" ignore;
  let only = function [ case ] -> Some case | _ -> None in
  let case = require_match only (Test_tree.flatten (Ppx_runtime.collect ())) in
  is_false (Tag.mem name case.Test_tree.tags)

let libraries () =
  add ~library:"dep" ~file:"src/shared.ml" "same" ignore;
  add ~library:"lib" ~file:"src/shared.ml" "same" ignore;
  add ~file:"src/own.ml" "own" ignore

let a_runner_keeps_its_library () =
  reset ();
  libraries ();
  Ppx_runtime.init [| "runner"; "inline-test-runner"; "lib" |];
  equal (list string) [ "Shared › same"; "Own › own" ] (collected ())

let outside_a_runner_no_library () =
  reset ();
  libraries ();
  Ppx_runtime.init [| "main" |];
  equal (list string) [ "Own › own" ] (collected ())

let files () =
  add ~file:"src/zeta.ml" "z" ignore;
  add ~file:"src/alpha.ml" "a" ignore;
  add ~file:"src/alpha.ml" "a2" ignore

let a_partition_keeps_its_file argv () =
  reset ();
  files ();
  Ppx_runtime.init argv;
  equal (list string) [ "Alpha › a"; "Alpha › a2" ] (collected ())

let a_later_init_drops_the_partition () =
  reset ();
  Ppx_runtime.init
    [| "runner"; "inline-test-runner"; "lib"; "-partition"; "alpha.ml" |];
  Ppx_runtime.init [| "runner"; "inline-test-runner"; "lib"; "--unknown" |];
  files ();
  equal (list string) [ "Zeta › z"; "Alpha › a"; "Alpha › a2" ] (collected ())

let collecting =
  group "Collecting"
    [
      test "a second collection is empty until new registrations arrive"
        a_second_collection_is_empty;
      test "collect raises while a group is open, and collects it once closed"
        an_open_group_refuses_collect;
      prop "the module's group adds no tag" ~examples:untagged_name_of_a_module
        Gen.string the_module_adds_no_tag;
      test "a runner keeps its library's registrations and those of no library"
        a_runner_keeps_its_library;
      test "outside a runner only the registrations of no library are kept"
        outside_a_runner_no_library;
      test "-partition keeps one file's registrations"
        (a_partition_keeps_its_file
           [| "runner"; "inline-test-runner"; "lib"; "-partition"; "alpha.ml" |]);
      test "-partition keeps one file's registrations outside the runner mode"
        (a_partition_keeps_its_file [| "main"; "-partition"; "alpha.ml" |]);
      test "a later init reads argv afresh, and drops the partition"
        a_later_init_drops_the_partition;
    ]

(* The runner protocol *)

let a_partition_runs_under_its_suite () =
  let c = ended partition in
  equal string "exit 1" c.status;
  contains ~sub:"lib/a.ml: 2 tests" c.out;
  equal string "" c.err

let a_partition_runs_its_file_alone () =
  let c = ended partition in
  contains ~sub:"FAIL  A › fails" c.out;
  not_contains ~sub:"other partition" c.out

let the_logs_are_keyed_by_the_partition () =
  let c = ended partition in
  let keyed = Filename.concat (logs c.dir) (Os.sanitize_component "lib/a.ml") in
  is_true (Sys.file_exists keyed)

let a_junit_directory_has_a_file_per_partition () =
  let c = ended junit in
  let file = Os.sanitize_component "lib/a.ml" ^ ".xml" in
  equal string "exit 0" c.status;
  equal (list string) [ file ]
    (Array.to_list (Sys.readdir (Filename.concat c.dir "junit")))

let runner_protocol =
  group "The runner protocol"
    [
      test
        "-list-partitions prints the sorted basenames of no library and of the \
         runner's library" (fun () ->
          let c = ended listing in
          equal string "exit 0" c.status;
          equal text "a.ml\nb.ml\nc.ml\n" c.out;
          equal string "" c.err);
      test
        "-list-partitions prints a basename once, a group's file and its \
         tests' files, drained ones included" (fun () ->
          equal text "dup.ml\nguest.ml\nhost.ml\nkept.ml\nx.pp.ml\n"
            (ended listing_files).out);
      test "a partition exits with the code of its run, the report on stdout"
        a_partition_runs_under_its_suite;
      test "a partition runs its file's tests alone, under the module's group"
        a_partition_runs_its_file_alone;
      test "the capture logs are keyed by the partition's suite"
        the_logs_are_keyed_by_the_partition;
      test "a JUnit directory gets a file per partition"
        a_junit_directory_has_a_file_per_partition;
      cases "outside the runner mode exit exits 0 and prints nothing" ~name:fst
        [
          ("init never called", never_initialised);
          ("-list-partitions by hand", listed_by_hand);
        ]
        (fun (_, c) ->
          let c = ended c in
          equal
            (triple string string string)
            ("exit 0", "", "") (c.status, c.out, c.err));
      test "a malformed mirror exits 2 under --corrected, with argv.(0)'s usage"
        (fun () ->
          let c = ended bad_mirror in
          equal string "exit 2" c.status;
          contains ~sub:"usage: child [OPTIONS] [PATTERN...]" c.err);
      test "a partition that declares no test exits 2" (fun () ->
          equal string "exit 2" (ended empty_partition).status);
    ]

(* The undriven guard *)

let the_diagnostic_names_the_files () =
  expect_exact (ended undriven_files).err
  @@ __POS_OF__
       {|windtrap: registered inline tests were never driven: this executable links ppx_windtrap-preprocessed test code of no library (a.ml, b.ml, g.ml) and nothing ran it.
windtrap: move the tests into a library stanza with (inline_tests), whose inline runner dune builds and drives, or drive the runner protocol yourself (Ppx_windtrap_runtime.Ppx_runtime.init/exit). Exiting 2: nothing ran.
|}

let a_suite_that_never_drains () =
  let c = ended own_suite in
  equal string "exit 2" c.status;
  contains ~sub:"own: 1 passed" c.out;
  contains ~sub:"never driven" c.err

let a_suite_over_a_library () =
  let c = ended linked_library in
  equal string "exit 0" c.status;
  starts_with ~affix:"suite: 1 passed" c.out;
  equal string "" c.err

let a_forked_child_is_silent () =
  let c = ended forks in
  equal
    (triple string string string)
    ("exit 0", "forked child: exit 0\n", "")
    (c.status, c.out, c.err)

let guard =
  group "The undriven guard"
    [
      cases "registrations that nothing drives exit 2, nothing on stdout"
        ~name:fst
        [ ("one file", undriven); ("several files", undriven_files) ]
        (fun (_, c) ->
          let c = ended c in
          equal (pair string string) ("exit 2", "") (c.status, c.out));
      test "the diagnostic names the one file that registered" (fun () ->
          contains ~sub:"of no library (a.ml)" (ended undriven).err);
      test "the diagnostic names the files of no library once, sorted"
        the_diagnostic_names_the_files;
      cases "a main that drained the registry exits as it chose, silently"
        ~name:fst
        [ ("collect returned", drained); ("collect raised", drain_raised) ]
        (fun (_, c) ->
          let c = ended c in
          equal
            (triple string string string)
            ("exit 0", "", "") (c.status, c.out, c.err));
      test "an unclaimed registry turns an exit 1 into 2" (fun () ->
          let c = ended undriven_failing in
          equal string "exit 2" c.status;
          contains ~sub:"never driven" c.err);
      test "a suite that never drains its registry reports, then exits 2"
        a_suite_that_never_drains;
      test "a suite that links a library's registrations runs its own alone"
        a_suite_over_a_library;
      test "a forked child that leaves through exit is silent"
        a_forked_child_is_silent;
      test
        "the guard exits through Stdlib.exit, so the other at_exit functions \
         run" (fun () ->
          let c = ended earlier_at_exit in
          equal string "exit 2" c.status;
          contains ~sub:"the earlier at_exit function ran" c.out);
    ]

(* The default Expect_test_config *)

module Shadowed = struct
  include Expect_test_config

  let sanitize = String.map (fun c -> if c = 'a' then 'b' else c)
end

let config =
  group "The default Expect_test_config"
    [
      prop "sanitize is the identity" Gen.string (fun s ->
          equal string s (Expect_test_config.sanitize s));
      test "run applies its function once" (fun () ->
          let calls = ref 0 in
          Expect_test_config.run (fun () -> incr calls);
          equal int 1 !calls);
      test "a module that includes it and overrides sanitize compiles"
        (fun () -> equal string "bb" (Shadowed.sanitize "ab"));
    ]

let () =
  exit
    (run "ppx_runtime"
       [ registration; collecting; runner_protocol; guard; config ])
