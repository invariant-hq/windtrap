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
   plain, printing the tree-wide summary dialect by hand (stdlib only, so
   the duration is [Sys.time]'s CPU clock).

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
   is the order the uninstrumented twin uses in test_semantics.ml. *)

(* The one sanctioned re-implementation of the dialect's styling: the link
   contract above forbids reaching windtrap's Pp/Env/Report, and the stdlib
   cannot see a terminal, so this mirrors Env's contract as far as it can —
   WINDTRAP_COLOR always/never wins, otherwise INSIDE_DUNE decides (empty
   and falsy spellings count as unset). Keep byte-compatible with
   Pp.styled_string and the harness dialect (test/unit/harness.ml). *)
let ansi =
  let set v =
    match String.lowercase_ascii v with
    | "" | "0" | "false" | "no" | "n" | "off" -> false
    | _ -> true
  in
  match Sys.getenv_opt "WINDTRAP_COLOR" with
  | Some v when String.lowercase_ascii v = "always" -> true
  | Some v when String.lowercase_ascii v = "never" -> false
  | _ -> (
      match Sys.getenv_opt "INSIDE_DUNE" with Some v -> set v | None -> false)

let styled code s = if ansi then "\027[" ^ code ^ "m" ^ s ^ "\027[0m" else s
let green = styled "32"
let red = styled "31"

(* The renderer's duration shape (Report.pp_run_duration): three
   significant digits, never scientific notation. *)
let pp_run_duration secs =
  if secs >= 999.5 then Printf.sprintf "%.0f" secs
  else if secs < 0.0001 then "0"
  else Printf.sprintf "%.3g" secs

let started = Sys.time ()
let failures = ref 0
let count = ref 0

let check name cond =
  incr count;
  if not cond then begin
    incr failures;
    Printf.printf "linkonly: %s: %s\n%!" (red "FAIL") name
  end

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
  let duration = pp_run_duration (Sys.time () -. started) in
  if !failures > 0 then begin
    Printf.printf "linkonly: %d checks passed, %s in %ss.\n%!"
      (!count - !failures)
      (red (Printf.sprintf "%d failed" !failures))
      duration;
    exit 1
  end;
  Printf.printf "linkonly: %s in %ss.\n%!"
    (green (Printf.sprintf "%d checks passed" !count))
    duration
