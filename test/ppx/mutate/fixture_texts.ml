(* The texts and the arms of a site, where the other fixtures leave them
   out. Each rule is named by its id in ../RULES.md and the line of
   instrument.mli that states it. *)

(* M52, mut:164: each run of blanks becomes one space inside a string
   literal too: the [before] text reads ["a b"] where the source has three
   blanks. *)
let spaced s = if s = "a   b" then 1 else 0

(* M56, mut:189-191: the disarmed arm of an ordering or [ari] guard keeps
   the attributes of its site, which the [before] text leaves out. *)
let kept a b = (a + b) [@kept]
let kept_cmp a b = if (a < b) [@kept] then 1 else 0
