(* Half B of the shared library: arithmetic. Exercised in full by
   test_b; test_a never references it, so the linker drops it from
   test_a's executable entirely — its points are absent from test_a's
   inline line, not reported as 0%. *)

let clamp lo hi x = if x < lo then lo else if x > hi then hi else x
let sum = List.fold_left ( + ) 0
let sign x = if x > 0 then 1 else if x < 0 then -1 else 0
