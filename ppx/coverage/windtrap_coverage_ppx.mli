(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The registration of the coverage rewriter with the ppxlib driver.

    The module exports no value, and linking it into a driver is its whole
    interface. When the module is initialized it registers
    [Instrument.transform_impl_file] with the ppxlib driver, under the name
    [windtrap_coverage], as a whole-file instrumentation of implementation
    files.

    The instrumentation is positioned after every rewriter that is not itself an
    instrumentation, so the function receives the expanded file and its points
    describe the code that runs. The one runtime dependency of the library is
    [windtrap.runtime].

    A driver may link this library and [ppx_windtrap.mutate] together. Of two
    such instrumentations ppxlib applies first the one that registered last. The
    driver that dune builds for a stanza naming both backends registers this
    library first, so the mutation rewriter runs first and this one receives its
    guards. Neither population is then the one that a single backend gives, and
    [Instrument] states what changes. *)
