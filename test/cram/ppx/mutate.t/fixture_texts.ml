(* The texts and the arms of a site, where the other fixtures leave them
   out. *)

(* Each run of blanks becomes one space inside a string literal too: the
   [before] text reads ["a b"] where the source has three blanks. *)
let spaced s = if s = "a   b" then 1 else 0

(* The disarmed arm of an ordering or [ari] guard keeps the attributes of its
   site, which the [before] text leaves out. *)
let kept a b = (a + b) [@kept]
let kept_cmp a b = if (a < b) [@kept] then 1 else 0
