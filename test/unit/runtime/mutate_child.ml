(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A fresh process is the only one whose epoch counter no test has moved, so
   only here does module initialization run before any window opens. The
   binding is the one ppx_windtrap.mutate generates: one [register] per file,
   whose result is the file's guard. *)

module Mutate = Windtrap_runtime.Mutate

let guard =
  Mutate.register ~file:"lib/child.ml"
    ~sites:
      [|
        {
          Mutate.line = 3;
          col = 10;
          rewrite = "lt";
          before = "a < b";
          after = "not (b < a)";
          dismissed = None;
        };
        {
          Mutate.line = 7;
          col = 4;
          rewrite = "add";
          before = "a + b";
          after = "a - b";
          dismissed = None;
        };
      |]

let () = ignore (guard 0 : bool)

let () =
  let drained label =
    let row (r : Mutate.reached) =
      Printf.sprintf " %s x%d" (Mutate.id_to_string r.mutant.id) r.hits
    in
    print_endline (label ^ String.concat "" (List.map row (Mutate.drain ())))
  in
  drained "initialization:";
  Mutate.next_epoch ();
  ignore (guard 0 : bool);
  drained "window:";
  Printf.printf "guards: %b %b\n" (guard 0) (guard 1)
