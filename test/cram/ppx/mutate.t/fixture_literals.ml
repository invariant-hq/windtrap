(* [int] nudges an integer literal by one in its type, 1 to 0 and n to
   n + 1, and [bool] flips a boolean outside a boolean context. *)

let pick b = if b then 7 else 1
let suffixed () = (1l, 2L, 3n, 0x10, -5, 1_000)

(* A nudge that leaves its type, and an unknown suffix, are no site. *)
let edges () = (max_int, 4611686018427387903, 9223372036854775807L, 4z)
let flags x = (true, x && false, if true then x else false)

(* Arid: AOR's operands, a capacity, ROR's own boundary move, a literal
   that runs at module initialization, and an OS flag. *)
let succ x = x + 1
let pred x = 1 - x
let table () = Hashtbl.create 16
let below x = if x < 0 then 2 else 3
let at_most x = if x <= 1 then 4 else 5
let above x = if x > 1 then 6 else 8
let at_least x = if x >= 1 then 9 else 10
let mirrored x = if 0 < x then 11 else 12
let limit = 64
let verbose = false
let later = lazy (65, ())

module Make (X : sig end) = struct
  let size = 66
end

class counter =
  object
    val mutable n = 67
    method get = n
  end

let pipe () = Unix.pipe ~cloexec:true ()
