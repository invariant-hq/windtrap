(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = Square of float | Rectangle of float * float

let area = function Square side -> side *. side | Rectangle (w, h) -> w *. h
let%test "a unit square has area 1" = assert (area (Square 1.) = 1.)
