(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* One guard, evaluated once per call. Deleting it hands [build] a negative
   size, which its count never brings down to zero: the program then
   allocates without evaluating a site again, so no hit budget ends it, and
   only the memory ceiling does. *)

let checked n build =
  if n < 0 then invalid_arg "Grower.checked: negative size";
  build n
