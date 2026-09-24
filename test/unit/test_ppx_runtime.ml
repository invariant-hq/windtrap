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
   directories under test/ppx. *)

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
      | "byhand", [] ->
          add ~file:"a.ml" "never runs" (fun () -> print_string "ran");
          run_protocol []
      | "list", [] ->
          add ~file:"src/b.ml" "b" ignore;
          add ~file:"src/a.ml" "a" ignore;
          run_protocol [ "inline-test-runner"; "lib"; "-list-partitions" ]
      | "run", [ log_dir ] ->
          Unix.putenv "WINDTRAP_OUTPUT" log_dir;
          add ~file:"a.ml" "passes" ignore;
          add ~file:"a.ml" "fails" (fun () -> Windtrap.equal Windtrap.int 1 2);
          add ~file:"b.ml" "other partition" (fun () -> Windtrap.fail "unrun");
          run_protocol [ "inline-test-runner"; "lib"; "-partition"; "a.ml" ]
      | "corrected", [ root ] ->
          Unix.putenv "WINDTRAP_OUTPUT" (Filename.concat root "logs");
          Unix.putenv "WINDTRAP_PROJECT_ROOT" root;
          (* The literal at t.ml's second line, as the rewriter would pass
             it: the node's position and the payload as written. *)
          add ~file:"t.ml" "stale" (fun () ->
              print_string "fresh";
              Windtrap.expect (Windtrap.output ())
                (("t.ml", 2, 2, 23), " stale "));
          run_protocol [ "inline-test-runner"; "lib" ]
      | "undriven", [] ->
          (* Registered, then a normal exit with nothing driving it: the
             guard's [at_exit] handler turns this [0] into a [2]. *)
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
  check "partitions are the sorted basenames of every file seen"
    (List.for_all
       (fun p -> List.mem p (Ppx_runtime.partitions ()))
       [ "alpha.ml"; "zeta.ml" ]
    && Ppx_runtime.partitions () = List.sort compare (Ppx_runtime.partitions ())
    );
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
  let code, out, err = spawn_child [ "--child"; "byhand" ] in
  check_int "invoked by hand, the runner exits 0" ~expected:0 ~actual:code;
  check_string "invoked by hand, the runner prints nothing" ~expected:""
    ~actual:(out ^ err)

let () =
  let code, out, err = spawn_child [ "--child"; "list" ] in
  check_int "-list-partitions exits 0" ~expected:0 ~actual:code;
  check_string "-list-partitions prints the sorted basenames on stdout"
    ~expected:"a.ml\nb.ml\n" ~actual:out;
  check_string "and nothing on stderr" ~expected:"" ~actual:err

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
  with_temp_root (fun root ->
      let source = Filename.concat root "t.ml" in
      let write path contents =
        Out_channel.with_open_bin path (fun oc -> output_string oc contents)
      in
      write source "let%expect_test \"stale\" =\n  [%expect {| stale |}]\n";
      let code, out, _ = spawn_child [ "--child"; "corrected"; root ] in
      check_int "a run whose only failure is a recorded correction exits 0"
        ~expected:0 ~actual:code;
      check_contains "the mismatch is reported with dune's acceptance"
        ~sub:"accept: dune promote" out;
      let corrected = source ^ ".corrected" in
      check "the correction is written beside the source"
        (Sys.file_exists corrected);
      if Sys.file_exists corrected then
        check_string "the correction rewrites the literal in place"
          ~expected:"let%expect_test \"stale\" =\n  [%expect {| fresh |}]\n"
          ~actual:(In_channel.with_open_bin corrected In_channel.input_all);
      check "the source itself is untouched"
        (In_channel.with_open_bin source In_channel.input_all
        = "let%expect_test \"stale\" =\n  [%expect {| stale |}]\n"))

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
