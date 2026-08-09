(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Registers {!Instrument.transform_impl_file} with the ppxlib driver as the
    [windtrap_mutate] whole-file instrumentation, positioned after all other
    rewriters so mutants describe the code that actually runs. Linking this
    library into a driver is the only API. *)
