(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The unit suite: one group per lib module, run by windtrap itself.
   Suites that must drive Runner.execute from a test body (test_run,
   test_runner, test_ppx_runtime, test_windtrap) cannot nest under an
   active run and are separate executables in this directory. *)

(* test_atomic_file's concurrency tests and test_capture's cloexec test
   re-exec this executable with a child marker in argv; dispatch never
   returns for those. *)
let () = Test_atomic_file.dispatch_child ()
let () = Test_capture.dispatch_child ()

let () =
  Windtrap.run "unit"
    [
      Windtrap.group "pp" Test_pp.tests;
      Windtrap.group "text" Test_text.tests;
      Windtrap.group "clock" Test_clock.tests;
      Windtrap.group "tag" Test_tag.tests;
      Windtrap.group "loc" Test_loc.tests;
      Windtrap.group "env" Test_env.tests;
      Windtrap.group "path_ops" Test_path_ops.tests;
      Windtrap.group "seed" Test_seed.tests;
      Windtrap.group "shrink_tree" Test_shrink_tree.tests;
      Windtrap.group "atomic_file" Test_atomic_file.tests;
      Windtrap.group "testable" Test_testable.tests;
      Windtrap.group "failure" Test_failure.tests;
      Windtrap.group "diff" Test_diff.tests;
      Windtrap.group "check" Test_check.tests;
      Windtrap.group "gen" Test_gen.tests;
      Windtrap.group "capture" Test_capture.tests;
      Windtrap.group "property" Test_property.tests;
      Windtrap.group "snapshot" Test_snapshot.tests;
      Windtrap.group "test_tree" Test_test_tree.tests;
      Windtrap.group "render" Test_render.tests;
      Windtrap.group "render_junit" Test_render_junit.tests;
      Windtrap.group "render_github" Test_render_github.tests;
      Windtrap.group "cli" Test_cli.tests;
      Windtrap.group "driver" Test_driver.tests;
    ]
