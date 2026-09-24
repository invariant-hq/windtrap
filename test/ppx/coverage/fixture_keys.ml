(* The offset a point is keyed at, where the other fixtures leave it out,
   and the payloads no mark enters. A key is not printed: it shows when two
   marks share it and so share one index, which is how each case below is
   built. Each rule is named by its id in ../RULES.md and the line of
   instrument.mli that states it. *)

(* C52, cov:112-113: a [let] with one binding gives its bound call the body
   as successor. In [one] the body [h a] starts with its one-byte callee, so
   both out-edges share a point. In [two] the [let] has two bindings, gives
   no successor, and its three calls are three points. *)
let one f h =
  ignore
    (let a = f 1 in
     h a)

let two f g h =
  ignore
    (let a = f 1 and b = g 2 in
     h a b)

(* C54, cov:116: [l @@ x] without a successor is keyed at [l]'s last byte,
   here the start of the loop body, so it shares the body's point. *)
let loop c f x =
  while c () do
    f @@ x
  done

(* C55, cov:116-117: a pipeline without a successor is keyed at the last
   byte of the head function of its last stage. The left operand of [||] is
   keyed at its own last byte: in [bare] that is [f]'s, and the pipeline
   shares the operand's point; in [applied] it is [z]'s, and the two are
   two points. *)
let bare x f y = x |> f || y
let applied x f z y = x |> f z || y

(* C56, cov:118-119: a method call without a successor is keyed at the
   expression's last byte, where an operand of [||] is keyed too, so the
   call and the operand share a point. *)
let sent a (o : < get : bool >) = ignore (a || o#get)

(* C60, cov:135-136: the payloads of extension nodes and of attributes, on an
   expression or on an item, are never traversed: the [if] and the call
   inside them take no mark. *)
let extended = [%ext if true then succ 1 else 0]
let attributed = (0 [@attr if true then succ 1 else 0])

[%%ext let x = if true then succ 1 else 0]

type t = int [@@attr if true then succ 1 else 0]
