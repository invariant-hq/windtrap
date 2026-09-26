(* The dismissal attributes on structures, where fixture_off and
   fixture_off_edges leave them out. *)

(* [[@@mutate off]] on a recursive module binding leaves it as written. *)
module rec Dark : sig
  val f : int -> int
end = struct
  let f n = n + 1
end
[@@mutate off]

(* On a [let ... in] binding and on any other item, the attribute is ignored and
   its payload is not checked: [h]'s site stays, and no payload below is
   refused. *)
let local n =
  let h x = x - 1 [@@mutate bogus] in
  h n

type t = int [@@mutate bogus]

(* The reason of a [[@@mutate off]] and of a [[@@@mutate off]] is accepted and
   dropped: neither records a site. *)
let reasoned a b = a + b [@@mutate off "binding reason"]

[@@@mutate off "region reason"]

let in_region a b = a - b

[@@@mutate on]

(* A nested structure inherits the region it opens in, and may close it for
   itself; its end restores the outer setting, so [still_dark] is left as
   written and [lit] below the region is not. *)
[@@@mutate off]

module Inherits = struct
  let dark a b = a + b

  [@@@mutate on]

  let lit_inside a b = a + b
end

let still_dark a b = a + b

[@@@mutate on]

let lit a b = a + b
