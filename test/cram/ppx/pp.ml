(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A standalone ppxlib driver over the three rewriters and the stand-in
   deriver of generated.ml: [pp.exe -apply NAMES --impl FILE] prints [FILE]
   as the rewriters named by [NAMES] rewrite it. *)

let () = Ppxlib.Driver.standalone ()
