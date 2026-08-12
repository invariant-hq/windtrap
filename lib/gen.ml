(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The core's spelling of windtrap.gen's Gen (lib/gen/gen.ml, where the
   module and its docs live). The include keeps the property engine's
   references — [sample], [Rejected], the renderers — on the name and
   types they had before the module moved out of the core; the public
   [Windtrap.Gen] alias points at the sublibrary directly. *)

include Windtrap_gen.Gen
