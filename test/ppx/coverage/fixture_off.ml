(* The Bisect_ppx off spelling at every level: expression, value binding,
   module binding, and structure toggles. [visible] and [light] are
   instrumented; nothing between the markers and nothing inside
   [Dark_module] is. *)

let visible n = if n > 0 then "p" else "n"
let hidden n = (if n > 0 then "p" else "n") [@coverage off]
let skipped n = if n > 0 then "p" else "n" [@@coverage off]

module Dark_module = struct
  let inside n = if n > 0 then "p" else "n"
end
[@@coverage off]

[@@@coverage off]

let dark n = match n with 0 -> "z" | _ -> "x"

(* Expressions reached through module expressions - not through a value
   binding - honor the off region too. *)
module type T = sig
  val v : int
end

module Packed =
  (val if Sys.word_size = 64 then
         (module struct
           let v = 1
         end : T)
       else
         (module struct
           let v = 2
         end : T))

[@@@coverage on]

let light n = match n with 0 -> "z" | _ -> "x"
