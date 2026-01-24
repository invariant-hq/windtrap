(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Run ID generation for test sessions.

    Each test run is assigned a unique 8-character alphanumeric identifier for
    organizing output files. *)

val generate : unit -> string
(** [generate ()] returns a fresh 8-character string from the alphabet [A-Z0-9].
    Uses a self-initializing PRNG; not cryptographically secure. *)
