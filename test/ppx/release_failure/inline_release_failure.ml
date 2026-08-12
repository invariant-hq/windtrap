(* A fixture whose release raises, touched by a test that PASSES.

   Releases run after the last test; the runner records the failure as a
   result row the moment it happens (one result model), and every sink
   projects the one recorded list. An inline runner that dropped the row
   would print a clean transcript and still exit 1, which is the defect
   the recorded row exists to close (Law 8: body and release failures are
   both reported). The library runner's end of this is pinned in
   test/unit/test_windtrap.ml; this is the inline runner's. *)

let leaky =
  Windtrap.fixture ~teardown:(fun () -> failwith "release-boom") (fun () -> ())

let%test "touches the fixture" = ignore (leaky ())
