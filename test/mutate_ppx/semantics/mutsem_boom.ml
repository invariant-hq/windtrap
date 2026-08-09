(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Law 16(a) ends with "and exit code", and an exit code is the one
   observable a suite running inside the process cannot read about
   itself. So this module is compiled TWICE from one source - into
   [Mutsem_fixtures] through [ppx_windtrap.mutate], and into
   [Mutsem_baseline] untouched - and [main] is run by the two one-line
   executables mutsem_exit.ml and baseline/mutsem_exit.ml, whose output
   dune diffs against ONE committed golden under
   [with-accepted-exit-codes 2].

   It lives in the two libraries rather than in the executables because
   that is what makes the exit-code half of the suite non-vacuous. An
   instrumented-versus-plain comparison cannot certify from its own
   observable which side is which - that is the whole point of the law -
   so the certification has to come from the registry, and the registry
   only sees modules the test suite links. test_semantics.ml asserts
   that mutsem_boom.ml is one of the three files that registered
   mutants, and coerces this copy against its twin's signature. Were the
   rewriter dropped from the library stanza, or applied to the baseline
   by accident, those two checks fail - whereas a golden shared by two
   executables would stay green while proving nothing.

   Three things the golden pins that nothing else here does:

   - OCaml exits 2 on an uncaught exception. An instrumented build must
     still do that, and must still name the same exception.
   - Nothing is printed that the program did not print. The coverage
     runtime deliberately dumps a profile at exit; the mutation runtime
     installs no [at_exit] handler and writes no file, and a regression
     that added one would land in this output - on the instrumented side
     only, since it is the only side that links the runtime.
   - The exception travels out through guarded code: the raise is under
     a [cmp] guard in a boolean context, itself under a [con] guard, and
     the whole thing behind a tail-recursive call whose argument is an
     [ari] guard.

   The dune action sets OCAMLRUNPARAM to empty so the fatal-error line
   never grows a backtrace: a backtrace would name the source path, the
   two copies live in different directories, and one shared golden would
   then be impossible for a reason that has nothing to do with the law. *)

let rec countdown n = if n = 0 then 0 else countdown (n - 1)

let main () =
  Printf.printf "computed %d\n%!" (countdown 1_000 + 41);
  if countdown 3 = 0 && countdown 2 + 1 > 0 then
    failwith "the instrumented program raises"
