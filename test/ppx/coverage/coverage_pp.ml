(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Standalone ppxlib driver over the coverage instrumenter, for the golden
   expansion tests: [pp.exe --impl fixture.ml] prints the instrumented
   source (instrumentation transformations linked into a driver always
   apply). *)

let () = Ppxlib.Driver.standalone ()
