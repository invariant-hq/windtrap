(*---------------------------------------------------------------------------
   Copyright (c) 2015 The mtime programmers. All rights reserved.
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

external elapsed_ns : unit -> int64 = "ocaml_windtrap_clock_elapsed_ns"

(* Force a call at module load time to initialize the C-side clock origin. *)
let () = ignore (elapsed_ns ())

type counter = int64

let counter () = elapsed_ns ()

let count start =
  let now = elapsed_ns () in
  Int64.sub now start

let count_s start =
  let ns = count start in
  Int64.to_float ns /. 1_000_000_000.
