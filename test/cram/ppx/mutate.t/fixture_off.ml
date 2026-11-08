(* [[@mutate off]] in all four spellings. An expression-level dismissal
   leaves the expression exactly as written and records no site for it,
   so the catalogue has no mutant there and the indices of the sites
   after it close up; the three coarser spellings record nothing either,
   for a whole binding, region or file, and their reasons are dropped
   alike. *)

let cap want =
  if (want > 16) [@mutate off "both arms yield 16 at the boundary"] then want
  else 16

let plain a b = if (a && b) [@mutate off] then 1 else 0
let sum a b = a + b [@@mutate off]

module Hidden = struct
  let diff a b = a - b
end
[@@mutate off]

[@@@mutate off]

let suppressed a b = a + b

[@@@mutate on]

let visible a b = a + b
