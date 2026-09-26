(* A [[@@@mutate off]] region opened and never closed suppresses the rest
   of its enclosing structure, and is not an error: an unbalanced region
   is a file-scoped decision, and the pass cannot tell it from a mistake.

   The region is enclosing-structure-scoped, not file-scoped: a module
   that turns mutation off inside itself restores the outer setting when
   its structure ends. Both halves are pinned here, and the second is why
   the whole file is not simply excluded - [visible] below is instrumented
   although [Quiet] above it is not. *)

module Quiet = struct
  [@@@mutate off]

  let hidden a b = a + b
end

let visible a b = a + b

[@@@mutate off]

let suppressed a b = a + b
let also_suppressed a b = if a && b then 1 else 0

(* Rules pinned here, by id in RULES.md and interface line: M43, mut:136-140. *)
