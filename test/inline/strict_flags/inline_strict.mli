(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Intentionally empty: the fixture exports nothing. Its tests register
   through module-initialization side effects, and -w +a -warn-error +a
   (see ./dune) makes a missing interface fatal (warning 70). *)
