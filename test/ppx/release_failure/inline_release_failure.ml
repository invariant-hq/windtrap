(* A fixture whose release raises, touched by a test that PASSES.

   Releases run after the last test, so the failure never enters
   Run.results — only Driver.results_with_releases carries it to the
   renderer. An inline runner that projected Run.results alone would print
   a clean transcript and still exit 1, which is the defect
   Driver.results_with_releases exists to close (Law 8: body and release
   failures are both reported). The library runner's end of this is pinned
   in test/unit/test_windtrap.ml; this is the inline runner's. *)

let leaky =
  Windtrap.fixture ~teardown:(fun () -> failwith "release-boom") (fun () -> ())

let%test "touches the fixture" = ignore (leaky ())
