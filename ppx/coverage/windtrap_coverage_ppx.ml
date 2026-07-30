(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let () =
  let instrument =
    Ppxlib.Driver.Instrument.V2.make Instrument.transform_impl_file
      ~position:After
  in
  Ppxlib.Driver.register_transformation ~instrument "windtrap_coverage"
