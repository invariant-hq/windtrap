(* The exclusion attributes on structures, where fixture_off leaves them out.
   Each rule is named by its id in ../RULES.md and the line of
   instrument.mli that states it. *)

(* C64, cov:160: [[@@coverage off]] on a recursive module binding leaves it
   as written. *)
module rec Dark : sig
  val f : int -> int
end = struct
  let f n = if n > 0 then 1 else 0
end
[@@coverage off]

(* C65, cov:160-162: on a [let ... in] binding and on any other item, the
   attribute is ignored and its payload not checked: [local]'s [g] keeps
   its points, and no payload below is refused. *)
let local n =
  let g x = if x then 1 else 0 [@@coverage off] in
  let h x = if x then 1 else 0 [@@coverage bogus] in
  g n + h n

type t = int [@@coverage bogus]

(* C67, cov:164-166: a nested structure inherits the region it opens in, and
   may close it for itself; its end restores the outer setting, so
   [still_dark] is left as written and [lit] below the region is not. *)
[@@@coverage off]

module Inherits = struct
  let dark n = if n > 0 then 1 else 0

  [@@@coverage on]

  let lit_inside n = if n > 0 then 1 else 0
end

let still_dark n = if n > 0 then 1 else 0

[@@@coverage on]

let lit n = if n > 0 then 1 else 0

(* C68, cov:166-167: a region never closed runs to the end of its structure,
   and no further: [after] is instrumented. *)
module Unclosed = struct
  let lit_before n = if n > 0 then 1 else 0

  [@@@coverage off]

  let dark n = if n > 0 then 1 else 0
end

let after n = if n > 0 then 1 else 0

(* C81, cov:207-208: an attribute inside excluded code is never examined, so
   the misplaced [on] and the bad payloads here are not refused. *)
let excluded = (fun x -> (x [@coverage on])) [@coverage off]
let excluded_binding x = x [@coverage bogus] [@@coverage off]

[@@@coverage off]

let in_region x = x [@coverage exclude_file]

[@@@coverage on]
