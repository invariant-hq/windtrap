(* The mutation rewriter over generated code. Each rule is named by its id
   in ../../RULES.md and the line of instrument.mli that states it. *)

(* M29, mut:109: a site at a ghost location, which is generated code, is
   never mutated and never recorded; the written [a - b] beside it is. *)
let generated a b = (a + b) [@generated]
let written a b = a - b

(* M30, mut:110-112: the stand-in deriver follows [copied] with a copy of
   it, so the copy's site has the line, the column and the rewrite of the
   first. The first site keeps the identifier; the copy's is neither mutated
   nor recorded. *)
let copied a b = a * (a + b) [@@duplicate]
