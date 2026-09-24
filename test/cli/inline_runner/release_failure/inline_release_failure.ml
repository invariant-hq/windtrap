(* A fixture whose release raises, touched by a test that PASSES.

   Releases run after the last test; the runner carries the failure on
   the outcome, and every sink of a finished run takes it as a required
   argument, so no sink can forget it. An inline runner that dropped it
   would print a clean transcript and still exit 1, which is the defect
   this suite exists to catch.
   The library runner's end of this is pinned in
   test/unit/test_windtrap.ml; this is the inline runner's. *)

let leaky =
  Windtrap.fixture ~teardown:(fun () -> failwith "release-boom") (fun () -> ())

let%test "touches the fixture" = ignore (leaky ())
