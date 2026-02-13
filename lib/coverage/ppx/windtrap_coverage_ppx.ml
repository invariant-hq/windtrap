(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let () =
  let impl ctxt ast =
    (new Instrument.instrumenter)#transform_impl_file ctxt ast
  in
  let instrument = Ppxlib.Driver.Instrument.V2.make impl ~position:After in
  Ppxlib.Driver.register_transformation ~instrument "windtrap_coverage"
