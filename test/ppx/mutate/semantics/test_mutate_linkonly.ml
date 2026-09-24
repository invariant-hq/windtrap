(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The runtime-only link contract (the package map's containment rule):
   mutation-instrumented user code links against nothing but the
   [windtrap.runtime] library, injected by dune through the rewriter's
   ppx_runtime_libraries - never the windtrap core. This executable's
   dune stanza lists [mutsem_fixtures] alone, and this file never names a
   mutation or windtrap module: that it links and runs at all is the
   test - which is also why it cannot be a windtrap suite and stays
   plain, printing with the stdlib alone.

   What it proves: the generated preamble calls [Windtrap_runtime.Mutate.register]
   at module load, so an executable that links only the instrumented
   library must still resolve that call - and does, without the consumer
   naming the runtime. What it does not prove: that the core is ABSENT
   rather than merely unnecessary. Nothing an OCaml program can ask about
   its own link closure would say so; the guarantee lives in
   ppx/mutate/dune's ppx_runtime_libraries field, and this executable is
   the check that the field is doing its job at all. Absence was checked
   out of band instead, and held: the only windtrap symbols in the linked
   binary are [camlWindtrap_runtime__Mutate...] ones. Not automated here, because
   a dune rule shelling out to nm would be a build dependency on a
   toolchain this project does not otherwise need.

   The checks below are a smoke check that the instrumented code still
   computes, including its registration at module load, and one guarantee 12
   claim that needs no baseline to state: with nothing armed, the
   fixture's own witnesses evaluate their operands right to left, which
   is the order the uninstrumented twin uses in test_mutate_semantics.ml. *)

let failed = ref []
let check name cond = if not cond then failed := name :: !failed

let () =
  let module C = Mutsem_fixtures.Covsem_fixtures in
  let module O = Mutsem_fixtures.Mutsem_order in
  check "instrumented countdown computes"
    (String.equal (C.countdown 1_000) "done");
  check "instrumented while loop computes" (C.sum_while 10 = 55);
  check "instrumented try arm computes" (C.safe_div 7 0 = 0);
  check "a disarmed cmp guard evaluates its operands right to left"
    (String.equal (O.show (O.cmp_lt 1 2)) "t | r,l");
  check "a disarmed ari guard evaluates its operands right to left"
    (String.equal (O.show (O.ari_add 1 2)) "3 | r,l");
  check "a disarmed con guard still short-circuits"
    (String.equal (O.show (O.con_and false true)) "f | l");
  match List.rev !failed with
  | [] -> print_endline "linkonly: ok"
  | names ->
      List.iter (Printf.printf "linkonly: FAILED %s\n") names;
      exit 1
