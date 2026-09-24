(* The coverage rewriter over generated code. Each rule is named by its id in
   ../../RULES.md and the line of instrument.mli that states it. *)

(* C59, cov:134-135: a mark whose attribution location is a ghost one is not
   inserted. The body of [entry] is generated and has no entry point; the
   callee of [edge]'s call is generated, so the call has no out-edge, and
   the call of [written] has one. *)
let entry x = print_int x [@generated]
let edge x = ignore ((succ [@generated]) x)
let written x = ignore (succ x)

(* C7, cov:102-103: an arm's extent runs from its pattern's start to its
   body's end, and is the body alone when the pattern is ghost or starts
   after the body: [x] and [0] below, [n + 1] from [Some n]. *)
let arms = function ((Some x) [@generated]) -> x | (None [@after_body]) -> 0
let written_arms = function Some n -> n + 1 | None -> 0
