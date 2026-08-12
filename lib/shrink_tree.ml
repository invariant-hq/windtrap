(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The core's spelling of windtrap.gen's Shrink_tree (lib/gen/
   shrink_tree.ml, where the module and its docs live). The include keeps
   the property engine's references and [Private]'s re-export on the name
   and types they had before the module moved out of the core. *)

include Windtrap_gen.Shrink_tree
