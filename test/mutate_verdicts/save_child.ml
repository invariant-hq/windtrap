(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Stands in for the writing side of a mutation run: builds a one-record
   collection and saves it under its own writer identity, exercising the
   atomic write from a process the parent test does not share a heap
   with - which is the shape the loop writes in. Usage: save <path>. *)

module M = Windtrap_runtime.Mutate
module V = Windtrap_runtime.Verdicts

let () =
  match Array.to_list Sys.argv with
  | _ :: "save" :: path :: _ ->
      let id =
        { M.file = "lib/child.ml"; line = 3; col = 10; rewrite = "lt" }
      in
      let t =
        V.add V.empty
          {
            V.id;
            before = "l < r";
            after = "not (r < l)";
            verdict = V.survived [ [ "child"; "less" ] ];
          }
      in
      V.save ?identity:(V.writer_identity ~exe:Sys.executable_name) path t
  | _ ->
      prerr_endline "save_child: expected save <path>";
      exit 2
