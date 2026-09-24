(* [[@mutate off]] in all four spellings. An expression-level dismissal
   leaves the expression exactly as written and records the site it
   suppressed, with its reason, in the catalogue; the three coarser
   spellings suppress without recording, because there is nothing to
   dismiss individually where a whole binding, region or file is out of
   scope. *)

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

(* Rules pinned here, by id in RULES.md and interface line: M38, mut:126-129;
   M39, mut:128-129; M41, mut:132-133; M43, mut:136-140; M47, mut:151-154;
   M50, mut:162-163. *)
