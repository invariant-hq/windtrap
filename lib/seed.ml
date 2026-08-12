(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The core's spelling of windtrap.gen's Seed (lib/gen/seed.ml, where the
   module and its docs live). The include keeps every internal reference
   — Cli's token parsing, the runner's derivations, Run's root — and
   [Private]'s re-export on the name and types they had before the module
   moved out of the core. *)

include Windtrap_gen.Seed
