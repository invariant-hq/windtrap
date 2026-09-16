(* Both tests exceed the driver's 1ns threshold by construction (a ~1ms
   spin), so the tagged test's missing warning can only be the ["slow"]
   exemption — never a fast clock reading. *)

open Windtrap

let spin_a_millisecond () =
  let t0 = Sys.time () in
  while Sys.time () -. t0 < 0.001 do
    ignore (Sys.opaque_identity 0)
  done

let%test "untagged" =
  spin_a_millisecond ();
  is_true true

let%test ("tagged slow" [@tags "slow"]) =
  spin_a_millisecond ();
  is_true true
