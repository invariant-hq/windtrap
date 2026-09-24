(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Ppx_runtime: the module-load registry generated code fills,
   the inline-test-runner protocol, and the undriven-registration guard.
   The PPX is not involved — registrations are made by hand, exactly as
   generated code makes them. The registry and the protocol's parsing are
   checked in-process through [collect]; what [exit] does is checked on a
   re-exec'd child, since it ends the process. The transcripts and
   corrections of real generated runners are pinned by the fixture
   directories under test/cli/inline_runner. *)

open Harness
module Ppx_runtime = Ppx_windtrap_runtime.Ppx_runtime
module Test_tree = Windtrap.Private.Test_tree
module Tag = Windtrap.Private.Test_tree.Tag

let pos file = (file, 1, 0, 0)

let add ?(tags = []) ~file name fn =
  Ppx_runtime.add_test ~file ~pos:(pos file) ~tags name fn

(* The children: [--child SCENARIO ARG...] registers as generated code
   would, speaks the protocol, and lets [exit] (or, for the undriven
   scenario, a normal termination) end the process. Logs go where the
   parent says, through the mirror the fixed argv leaves open. *)
let () =
  match Array.to_list Sys.argv with
  | _ :: "--child" :: scenario :: args -> (
      clear_env ();
      let run_protocol argv =
        Ppx_runtime.init (Array.of_list ("child" :: argv));
        Ppx_runtime.exit ()
      in
      match (scenario, args) with
      | "list", [] ->
          add ~file:"src/b.ml" "b" ignore;
          add ~file:"src/a.ml" "a" ignore;
          add ~file:"src/a.ml" "a2" ignore;
          run_protocol [ "inline-test-runner"; "lib"; "-list-partitions" ]
      | "list-files", [] ->
          (* One basename from two directories, a name with two dots, a
             group's file and the file of a test inside it, and a file
             whose registrations were collected before the listing. *)
          add ~file:"src/dup.ml" "one" ignore;
          add ~file:"test/dup.ml" "two" ignore;
          add ~file:"gen/x.pp.ml" "three" ignore;
          Ppx_runtime.enter_group ~file:"host.ml" ~tags:[] "G";
          add ~file:"guest.ml" "t" ignore;
          Ppx_runtime.leave_group ();
          add ~file:"kept.ml" "t" ignore;
          ignore (Ppx_runtime.collect ());
          run_protocol [ "inline-test-runner"; "lib"; "-list-partitions" ]
      | "run", [ log_dir ] ->
          Unix.putenv "WINDTRAP_OUTPUT" log_dir;
          add ~file:"a.ml" "passes" ignore;
          add ~file:"a.ml" "fails" (fun () -> Windtrap.equal Windtrap.int 1 2);
          add ~file:"b.ml" "other partition" (fun () -> Windtrap.fail "unrun");
          run_protocol [ "inline-test-runner"; "lib"; "-partition"; "a.ml" ]
      | "undriven", [] ->
          (* Registered, then a normal exit with nothing driving it: the
             guard's [at_exit] handler turns this [0] into a [2]. *)
          add ~file:"a.ml" "never driven" ignore;
          Stdlib.exit 0
      | "undriven-exit-1", [] ->
          add ~file:"a.ml" "never driven" ignore;
          Stdlib.exit 1
      | "drained", [] ->
          (* A hand-written main that drains the registry owns it. *)
          add ~file:"a.ml" "drained" ignore;
          ignore (Ppx_runtime.collect ());
          Stdlib.exit 0
      | "drain-raises", [] ->
          add ~file:"a.ml" "drained" ignore;
          Ppx_runtime.enter_group ~file:"a.ml" ~tags:[] "Open";
          (try ignore (Ppx_runtime.collect ()) with Invalid_argument _ -> ());
          Stdlib.exit 0
      | "no-init", [] -> Ppx_runtime.exit ()
      | "bad-mirror", [] ->
          Unix.putenv "WINDTRAP_TIMEOUT" "banana";
          add ~file:"a.ml" "t" ignore;
          run_protocol [ "inline-test-runner"; "lib"; "-partition"; "a.ml" ]
      | "empty-partition", [ log_dir ] ->
          Unix.putenv "WINDTRAP_OUTPUT" log_dir;
          add ~file:"a.ml" "t" ignore;
          run_protocol [ "inline-test-runner"; "lib"; "-partition"; "c.ml" ]
      | "junit", [ log_dir; junit ] ->
          Unix.putenv "WINDTRAP_OUTPUT" log_dir;
          Unix.putenv "WINDTRAP_JUNIT" junit;
          add ~file:"a.ml" "passes" ignore;
          run_protocol [ "inline-test-runner"; "lib"; "-partition"; "a.ml" ]
      | "own-suite", [ log_dir ] ->
          (* Runs a suite of its own and never drains what it registered. *)
          Unix.putenv "WINDTRAP_OUTPUT" log_dir;
          add ~file:"a.ml" "undrained" ignore;
          Stdlib.exit
            (Windtrap.run ~argv:[| "own" |] "own"
               [ Windtrap.test "ran" ignore ])
      | "fork", [] ->
          add ~file:"a.ml" "never driven" ignore;
          (match Unix.fork () with
          | 0 -> Stdlib.exit 0
          | pid -> (
              match Unix.waitpid [] pid with
              | _, Unix.WEXITED n ->
                  Printf.printf "forked child exited %d\n%!" n
              | _ -> print_endline "forked child died"));
          run_protocol []
      | "at-exit", [] ->
          (* Registered before the guard, so it runs after it. *)
          at_exit (fun () ->
              print_endline "the earlier at_exit function ran";
              flush stdout);
          add ~file:"a.ml" "never driven" ignore;
          Stdlib.exit 0
      | _ ->
          prerr_endline "unknown child scenario";
          exit 3)
  | _ -> ()

let () = init "ppx_runtime"

(* Re-exec this executable with [args] in a stated environment, returning
   its exit code, standard output and standard error, apart. *)
let spawn_child args =
  let module Child = Windtrap_test_support.Child in
  let r = Child.run Sys.executable_name args in
  (Child.exit_code r, r.Child.out, r.Child.err)

let paths tests =
  List.map
    (fun (case : Test_tree.case) -> Test_tree.path_to_string case.path)
    (Test_tree.flatten tests)

let check_paths name ~expected tests =
  check_string name
    ~expected:(String.concat "\n" expected)
    ~actual:(String.concat "\n" (paths tests))

(* Registration and collection *)

let () =
  (* Files group under their module name, in first-registration order;
     a second collection is empty until new registrations arrive. *)
  add ~file:"src/parser.ml" "first" ignore;
  add ~file:"src/lexer.ml" "second" ignore;
  add ~file:"src/parser.ml" "third" ignore;
  check_paths "files group under their module, first-registration order"
    ~expected:[ "Parser › first"; "Parser › third"; "Lexer › second" ]
    (Ppx_runtime.collect ());
  check "a second collection is empty" (Ppx_runtime.collect () = [])

let () =
  (* A functor instantiated twice registers one name twice: later
     duplicates are renamed, at the top level and inside a group. *)
  add ~file:"f.ml" "instance" ignore;
  add ~file:"f.ml" "instance" ignore;
  add ~file:"f.ml" "instance" ignore;
  Ppx_runtime.enter_group ~file:"f.ml" ~tags:[] "G";
  add ~file:"f.ml" "instance" ignore;
  add ~file:"f.ml" "instance" ignore;
  Ppx_runtime.leave_group ();
  check_paths "duplicate names are renamed per scope"
    ~expected:
      [
        "F › instance";
        "F › instance (2)";
        "F › instance (3)";
        "F › G › instance";
        "F › G › instance (2)";
      ]
    (Ppx_runtime.collect ())

let () =
  (* Groups nest, and a group's tags reach its descendants, as do a
     test's own. *)
  Ppx_runtime.enter_group ~file:"n.ml" ~tags:[ "outer" ] "Outer";
  add ~file:"n.ml" "in outer" ignore;
  Ppx_runtime.enter_group ~file:"n.ml" ~tags:[] "Inner";
  add ~tags:[ "own" ] ~file:"n.ml" "in inner" ignore;
  Ppx_runtime.leave_group ();
  Ppx_runtime.leave_group ();
  add ~file:"n.ml" "after" ignore;
  let tests = Ppx_runtime.collect () in
  check_paths "groups nest under the file's module"
    ~expected:
      [ "N › Outer › in outer"; "N › Outer › Inner › in inner"; "N › after" ]
    tests;
  let cases = Test_tree.flatten tests in
  let tags_of path =
    (List.find
       (fun (case : Test_tree.case) ->
         Test_tree.path_to_string case.path = path)
       cases)
      .tags
  in
  check "a group's tags reach its tests"
    (Tag.mem "outer" (tags_of "N › Outer › in outer"));
  check "an inner test unions its own tags with its groups'"
    (Tag.mem "outer" (tags_of "N › Outer › Inner › in inner")
    && Tag.mem "own" (tags_of "N › Outer › Inner › in inner"));
  check "a test after the group carries none of its tags"
    (not (Tag.mem "outer" (tags_of "N › after")))

let () =
  expect_invalid_arg "leave_group with no open group" (fun () ->
      Ppx_runtime.leave_group ());
  Ppx_runtime.enter_group ~file:"u.ml" ~tags:[] "Open";
  expect_invalid_arg "collect with a group still open" (fun () ->
      Ppx_runtime.collect ());
  Ppx_runtime.leave_group ();
  check "the unclosed group collects once closed"
    (List.length (Ppx_runtime.collect ()) = 1)

(* The protocol: partitions and the -partition filter *)

let () =
  add ~file:"src/zeta.ml" "z" ignore;
  add ~file:"src/alpha.ml" "a" ignore;
  add ~file:"src/alpha.ml" "a2" ignore;
  Ppx_runtime.init
    [| "runner"; "inline-test-runner"; "lib"; "-partition"; "alpha.ml" |];
  check_paths "-partition keeps one file's registrations"
    ~expected:[ "Alpha › a"; "Alpha › a2" ]
    (Ppx_runtime.collect ());
  (* A later init parses afresh: the partition is gone with it. *)
  Ppx_runtime.init [| "runner"; "inline-test-runner"; "lib"; "--unknown" |];
  add ~file:"src/zeta.ml" "z" ignore;
  add ~file:"src/alpha.ml" "a" ignore;
  check_paths "init without -partition collects every file"
    ~expected:[ "Zeta › z"; "Alpha › a" ]
    (Ppx_runtime.collect ())

(* exit, on a child *)

let () =
  let code, out, err = spawn_child [ "--child"; "list" ] in
  check_int "-list-partitions exits 0" ~expected:0 ~actual:code;
  check_string
    "-list-partitions prints the sorted basenames on stdout, each once"
    ~expected:"a.ml\nb.ml\n" ~actual:out;
  check_string "and nothing on stderr" ~expected:"" ~actual:err;
  let _, out, _ = spawn_child [ "--child"; "list-files" ] in
  check_string
    "a partition per basename, a group's file and its tests' files are \
     partitions, and collect keeps them"
    ~expected:"dup.ml\nguest.ml\nhost.ml\nkept.ml\nx.pp.ml\n" ~actual:out

let () =
  with_temp_root (fun log_dir ->
      let code, out, err = spawn_child [ "--child"; "run"; log_dir ] in
      check_string "the transcript is all on stdout" ~expected:"" ~actual:err;
      check_int "a partition with a failing test exits 1" ~expected:1
        ~actual:code;
      (* The suite is named per partition: dune runs a library's
         partitions concurrently, and a suite named for the library alone
         would have every partition share one JUnit file, one capture log
         directory and one last-failed store. *)
      check_contains "the transcript names the library and the partition"
        ~sub:"lib/a.ml: 2 tests" out;
      check_contains "the failure names the test under its module"
        ~sub:"FAIL  A › fails" out;
      check "the other partition did not run"
        (not (contains "other partition" out));
      check "the capture logs are keyed by the partition's suite name"
        (Sys.file_exists
           (Filename.concat log_dir
              (Windtrap.Private.Os.sanitize_component "lib/a.ml"))))

let () =
  let code, out, err = spawn_child [ "--child"; "undriven" ] in
  check_int "registrations nothing drives exit 2" ~expected:2 ~actual:code;
  check_string "the guard writes nothing on stdout" ~expected:"" ~actual:out;
  check_contains "the guard names the registered file, on stderr"
    ~sub:
      "never driven: this executable links ppx_windtrap-preprocessed test code \
       (a.ml)"
    err;
  check_contains "the guard names the remedy" ~sub:"add (inline_tests)" err

(* Registration: files, groups and their names *)

let () =
  (* One basename, one partition and one group; the module name stops at
     the first dot. *)
  add ~file:"src/dup.ml" "one" ignore;
  add ~file:"test/dup.ml" "two" ignore;
  add ~file:"gen/x.pp.ml" "three" ignore;
  check_paths "one basename is one group, named up to its first dot"
    ~expected:[ "Dup \u{203a} one"; "Dup \u{203a} two"; "X \u{203a} three" ]
    (Ppx_runtime.collect ())

let () =
  Ppx_runtime.enter_group ~file:"twice.ml" ~tags:[] "G";
  add ~file:"twice.ml" "t" ignore;
  Ppx_runtime.leave_group ();
  Ppx_runtime.enter_group ~file:"twice.ml" ~tags:[] "G";
  add ~file:"twice.ml" "t" ignore;
  Ppx_runtime.leave_group ();
  check_paths "a group name its scope holds is renamed"
    ~expected:
      [ "Twice \u{203a} G \u{203a} t"; "Twice \u{203a} G (2) \u{203a} t" ]
    (Ppx_runtime.collect ())

let () =
  Ppx_runtime.enter_group ~file:"host.ml" ~tags:[] "G";
  add ~file:"guest.ml" "t" ignore;
  Ppx_runtime.leave_group ();
  check_paths "inside a group the test lands in the group, whatever its file"
    ~expected:[ "Host \u{203a} G \u{203a} t" ]
    (Ppx_runtime.collect ())

let () =
  add ~file:"plain.ml" "untagged" ignore;
  match Test_tree.flatten (Ppx_runtime.collect ()) with
  | [ case ] -> check "the module's group adds no tag" (case.tags = Tag.empty)
  | _ -> check "one case" false

(* exit and the guard, on children *)

let () =
  let code, out, err = spawn_child [ "--child"; "drained" ] in
  check_int "a main that drains the registry exits as it chose" ~expected:0
    ~actual:code;
  check_string "and the guard is silent" ~expected:"" ~actual:(out ^ err);
  let code, out, err = spawn_child [ "--child"; "drain-raises" ] in
  check_int "a collect that raised still claimed" ~expected:0 ~actual:code;
  check_string "silently" ~expected:"" ~actual:(out ^ err)

let () =
  let code, out, err = spawn_child [ "--child"; "no-init" ] in
  check_int "exit without init exits 0" ~expected:0 ~actual:code;
  check_string "and prints nothing" ~expected:"" ~actual:(out ^ err)

let () =
  let code, _, err = spawn_child [ "--child"; "bad-mirror" ] in
  check_int "a malformed mirror exits 2 under --corrected" ~expected:2
    ~actual:code;
  check_contains "the usage line names argv.(0)"
    ~sub:"usage: child [OPTIONS] [PATTERN]" err;
  with_temp_root (fun log_dir ->
      let code, _, _ = spawn_child [ "--child"; "empty-partition"; log_dir ] in
      check_int "a partition that declares no test exits 2" ~expected:2
        ~actual:code)

let () =
  with_temp_root (fun log_dir ->
      let junit = Filename.concat log_dir "junit" in
      let code, _, _ = spawn_child [ "--child"; "junit"; log_dir; junit ] in
      check_int "the partition passes" ~expected:0 ~actual:code;
      check "a JUnit directory holds one file per partition"
        (Sys.file_exists
           (Filename.concat junit
              (Windtrap.Private.Os.sanitize_component "lib/a.ml" ^ ".xml"))))

let () =
  let code, _, err = spawn_child [ "--child"; "undriven-exit-1" ] in
  check_int "an unclaimed registry turns an exit 1 into 2" ~expected:2
    ~actual:code;
  check_contains "with the diagnostic" ~sub:"add (inline_tests)" err

let () =
  with_temp_root (fun log_dir ->
      let code, out, err = spawn_child [ "--child"; "own-suite"; log_dir ] in
      check_int "a suite that leaves the registry undrained exits 2" ~expected:2
        ~actual:code;
      check_contains "after its report" ~sub:"own: 1 passed" out;
      check_contains "and the diagnostic" ~sub:"add (inline_tests)" err)

let () =
  if Sys.win32 then skip_scenario ~reason:"POSIX only" __POS__
  else
    let code, out, err = spawn_child [ "--child"; "fork" ] in
    check_string "a forked child leaves through exit, silent" ~expected:""
      ~actual:err;
    check_string "with its own code" ~expected:"forked child exited 0\n"
      ~actual:out;
    check_int "and the parent claims and exits 0" ~expected:0 ~actual:code

let () =
  let code, out, _ = spawn_child [ "--child"; "at-exit" ] in
  check_int "the guard exits 2" ~expected:2 ~actual:code;
  check_contains "through Stdlib.exit: the earlier at_exit function runs"
    ~sub:"the earlier at_exit function ran" out

(* The ambient config module *)

let () =
  (* The default config is the identity in both components generated code
     consumes; its shape must keep include-and-override configs compiling. *)
  check_string "Expect_test_config.sanitize is the identity" ~expected:"x"
    ~actual:(Expect_test_config.sanitize "x");
  let ran = ref false in
  Expect_test_config.run (fun () -> ran := true);
  check "Expect_test_config.run applies the body" !ran;
  let module Shadow = struct
    include Expect_test_config

    let sanitize s = String.map (fun c -> if c = 'a' then 'b' else c) s
  end in
  check_string "include-and-override shadowing compiles and overrides"
    ~expected:"bb" ~actual:(Shadow.sanitize "ab")

(* Summary *)

let () = finish ()
