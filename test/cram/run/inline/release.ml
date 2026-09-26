(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A fixture is released after the last test, so the test that acquires it
   passes and the release fails the run. *)

let leaky =
  Windtrap.fixture ~teardown:(fun () -> failwith "release-boom") (fun () -> ())

let%test "acquires the fixture" = leaky ()
