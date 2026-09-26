(* The coverage rewriter over a file that holds the mutation rewriter's guards.
   A guard is generated code and takes no mark: every node of a guard is
   ghost but its disarmed arm, which keeps its site's location, so that arm
   alone is marked. *)

(* The disarmed arm of an ordering or [ari] guard is marked as the branch of
   an [if], whether or not the site is a block: [a < b] is a condition, and
   [a + b] is no block at all. *)
let lt a b = if a < b then 1 else 0
let add a b = a + b

(* A block that is a [neg], an equality or a [con] site has no entry point:
   the three guards of [match], and the [then] branch of [both]. *)
let arms x p a b =
  match x with
  | 0 when p x -> "neg"
  | 1 when a = b -> "equality"
  | 2 when a && b -> "con"
  | _ -> "other"

let both c a b = if c then a && b else false

(* An application that is a [neg] site, or the left operand of a [con]
   site, has no out-edge. *)
let rec loop f x =
  while f x do
    loop f x
  done

let left f g x = ignore (f x && g x)

(* A [||] that is a site is not rewritten, and its right operand is marked
   as a branch of the guard. *)
let either a b = a || b
