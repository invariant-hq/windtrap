(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Mutation-instrumented code links against [windtrap.runtime] alone, which
   the rewriter's ppx_runtime_libraries inject: the stanza lists
   [mutsem_fixtures] and nothing else, and this file names no windtrap
   module, so that it links and registers at load is the claim. That the core
   is absent from the binary is ppx/mutate/dune's field; no program can ask
   its own link closure. It cannot link the core to use windtrap's verbs, so
   it checks with the standard library and exits 1 on a wrong result. With
   nothing armed, the witnesses evaluate their operands right to left, as
   the uninstrumented twin does in test_mutate_semantics.ml. *)

let wrong = ref []
let check name ok = if not ok then wrong := name :: !wrong

let () =
  let module Covsem_fixtures = Mutsem_fixtures.Covsem_fixtures in
  let module Mutsem_order = Mutsem_fixtures.Mutsem_order in
  check "countdown 1_000"
    (String.equal (Covsem_fixtures.countdown 1_000) "done");
  check "sum_while 10" (Covsem_fixtures.sum_while 10 = 55);
  check "safe_div 7 0" (Covsem_fixtures.safe_div 7 0 = 0);
  check "cmp_lt 1 2"
    (String.equal (Mutsem_order.show (Mutsem_order.cmp_lt 1 2)) "t | r,l");
  check "ari_add 1 2"
    (String.equal (Mutsem_order.show (Mutsem_order.ari_add 1 2)) "3 | r,l");
  check "con_and false true"
    (String.equal (Mutsem_order.show (Mutsem_order.con_and false true)) "f | l");
  match List.rev !wrong with
  | [] -> print_endline "linkonly: ok"
  | names ->
      List.iter (Printf.printf "linkonly: wrong result of %s\n") names;
      exit 1
