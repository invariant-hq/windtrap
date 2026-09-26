(* [ari]: the one operator whose armed arm names an operator the source
   did not write, and the only one that reaches pure computation with no
   branch and no comparison. *)

let sum a b = a + b
let diff a b = a - b
let fsum a b = a +. b
let fdiff a b = a -. b

(* Module initialization is NOT excluded here: the runtime classifies a
   site from the reach epoch at run time, and the instrumenter must not
   try to guess. *)
let origin = 1 + 2

(* Unary minus is one application of [~-], not two of [-]: no site. *)
let negate x = -x
let scaled a b = (a + b) * 2

(* Rules pinned here, by id in RULES.md and interface line: M11, mut:65-66;
   M12, mut:52-53; M31, mut:114-115; M47, mut:155-158; M55, mut:186-187. *)
