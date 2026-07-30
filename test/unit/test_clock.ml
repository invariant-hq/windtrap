(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap

let tests =
  [
    test "count is non-negative and monotonic" (fun () ->
        let c = Clock.counter () in
        is_true ~msg:"count is non-negative" (Clock.count c >= 0L);
        let a = Clock.count c in
        let b = Clock.count c in
        is_true ~msg:"count is monotonic" (b >= a));
    test "sleeping is measured" (fun () ->
        (* 5ms of sleep reads as at least 1ms elapsed (loose bound to avoid
           scheduler flakiness). *)
        let c = Clock.counter () in
        Unix.sleepf 0.005;
        is_true (Clock.count c >= 1_000_000L));
    test "count_s agrees with count within float rounding" (fun () ->
        let c = Clock.counter () in
        Unix.sleepf 0.001;
        let ns = Clock.count c in
        let s = Clock.count_s c in
        is_true ~msg:"count_s is seconds"
          (s >= Int64.to_float ns /. 1_000_000_000.);
        is_true ~msg:"count_s within a second here" (s < 1.));
  ]
