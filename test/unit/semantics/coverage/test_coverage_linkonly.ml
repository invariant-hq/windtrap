(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Instrumented code links against [windtrap.runtime] alone, which the
   rewriter's ppx_runtime_libraries inject: the stanza lists [covsem_fixtures]
   and nothing else, and this file names no windtrap module, so that it links
   is the claim. It cannot link the core to use windtrap's verbs, so it checks
   with the standard library and exits 1 on a wrong result. *)

let wrong = ref []
let check name ok = if not ok then wrong := name :: !wrong

let () =
  check "countdown 1_000"
    (String.equal (Covsem_fixtures.countdown 1_000) "done");
  check "sum_while 10" (Covsem_fixtures.sum_while 10 = 55);
  check "safe_div 7 0" (Covsem_fixtures.safe_div 7 0 = 0);
  match List.rev !wrong with
  | [] -> print_endline "linkonly: ok"
  | names ->
      List.iter (Printf.printf "linkonly: wrong result of %s\n") names;
      exit 1
