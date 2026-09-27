(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf

(* Clauses *)

type law = { name : string; pos : Loc.pos option; msg : string option }

(* One equation of a law, with the terms computed for it so far, newest
   first. A term is printed only when its clause fails, so a passing law
   calls no printer. *)
type clause = {
  law : law;
  clause : string option;
  equation : string;
  mutable terms : (string * (unit -> string)) list;
}

let law ?__POS__ ?msg name = { name; pos = __POS__; msg }
let clause ?clause law equation = { law; clause; equation; terms = [] }

(* Raised within the law's call, as [Loc.capture] requires. *)
let fail cl terms =
  raise
    (Failure.Check_failure
       (Failure.law
          ?loc:(Loc.resolve ?__POS__:cl.law.pos ())
          ?msg:cl.law.msg ?clause:cl.clause ~law:cl.law.name
          ~equation:cl.equation terms))

let printed ?(except = []) cl =
  List.filter_map
    (fun (name, print) ->
      if List.mem name except then None
      else Some (Failure.Term { name; value = Failure.text (print ()) }))
    (List.rev cl.terms)

let term cl name w v =
  cl.terms <- (name, fun () -> Testable.to_string w v) :: cl.terms

(* A fact of a clause: a boolean it computed, as a term. *)
let fact cl name b =
  term cl name Testable.bool b;
  b

(* [f ()] as the term [name]: what [f] fails or raises ends the law there,
   and a control goes on to its owner. *)
let apply cl name w f =
  match Failure.catch f with
  | Ok v ->
      term cl name w v;
      v
  | Error (#Failure.fault as fault) ->
      fail cl
        (printed cl
        @ [ Failure.Failed { name; failure = Failure.of_fault fault } ])
  | Error (#Failure.control as control) -> Failure.reraise control

let holds cl ok = if not ok then fail cl (printed cl)

let sides cl w (left, l) (right, r) =
  if not (Testable.equal w l r) then
    let side name v =
      Failure.Side { name; value = Failure.text (Testable.to_string w v) }
    in
    fail cl (printed ~except:[ left; right ] cl @ [ side left l; side right r ])

(* Demands *)

(* A demand is a [cover] where one may be placed, and nothing elsewhere. The
   law's [msg] tells apart the demands of two calls of one law. *)
let demand law label cond =
  match Run.prop_context () with
  | None -> ()
  | Some context ->
      let label =
        match law.msg with
        | None -> strf "%s: %s" law.name label
        | Some msg -> strf "%s: %s (%s)" law.name label msg
      in
      Property.cover context label cond

(* [compare] answers [0] for a physically equal pair before it looks inside,
   so a pair it refuses is two blocks. *)
let differs x y =
  match Stdlib.compare x y with
  | order -> order <> 0
  | exception Invalid_argument _ -> true

(* Equivalences and orders *)

let order_of ~law ~witness w =
  match Testable.compare w with
  | Some compare -> compare
  | None ->
      invalid_arg
        (strf
           "Windtrap.Law.%s: %s has no order; give it one with \
            Testable.with_compare"
           law witness)

(* A term's name as the operand of a comparison. *)
let operand name = if String.contains name ' ' then "(" ^ name ^ ")" else name
let sign n = Int.compare n 0

let equivalence ?__POS__ ?msg ?respell w (a, b) =
  let law = law ?__POS__ ?msg "equivalence" in
  let equal = Testable.equal w in
  let reflexive name x =
    let cl = clause ~clause:"reflexive" law (strf "%s = %s" name name) in
    term cl name w x;
    holds cl (fact cl (strf "%s = %s" name name) (equal x x))
  in
  reflexive "a" a;
  reflexive "b" b;
  let cl = clause ~clause:"symmetric" law "a = b iff b = a" in
  term cl "a" w a;
  term cl "b" w b;
  let ab = fact cl "a = b" (equal a b) in
  let ba = fact cl "b = a" (equal b a) in
  holds cl (Bool.equal ab ba);
  demand law "an unequal pair" (not ab);
  match respell with
  | None -> ()
  | Some r ->
      let cl = clause ~clause:"respelled" law "a = r a" in
      term cl "a" w a;
      let ra = apply cl "r a" w (fun () -> r a) in
      demand law "r a differs from a" (differs ra a);
      holds cl (fact cl "a = r a" (equal a ra));
      let cl = clause ~clause:"respelled" law "r a = a" in
      term cl "a" w a;
      term cl "r a" w ra;
      holds cl (fact cl "r a = a" (equal ra a));
      let cl = clause ~clause:"transitive" law "a = r (r a)" in
      term cl "a" w a;
      term cl "r a" w ra;
      let rra = apply cl "r (r a)" w (fun () -> r ra) in
      holds cl (fact cl "a = r (r a)" (equal a rra));
      let cl = clause ~clause:"transitive" law "r a = b iff a = b" in
      term cl "a" w a;
      term cl "b" w b;
      term cl "r a" w ra;
      let rab = fact cl "r a = b" (equal ra b) in
      holds cl (Bool.equal rab (fact cl "a = b" ab))

let order ?__POS__ ?msg ?respell w (a, b, c) =
  let cmp = order_of ~law:"order" ~witness:"the witness" w in
  let law = law ?__POS__ ?msg "order" in
  let equal = Testable.equal w in
  let compared cl (x, vx) (y, vy) =
    let n = cmp vx vy in
    term cl (strf "cmp %s %s" (operand x) (operand y)) Testable.int n;
    n
  in
  let inputs cl = List.iter (fun (name, v) -> term cl name w v) in
  let a = ("a", a) and b = ("b", b) and c = ("c", c) in
  List.iter
    (fun ((x, _) as vx) ->
      let cl = clause ~clause:"reflexive" law (strf "cmp %s %s = 0" x x) in
      inputs cl [ vx ];
      holds cl (compared cl vx vx = 0))
    [ a; b; c ];
  let pairs = [ (a, b); (a, c); (b, c) ] in
  List.iter
    (fun (((x, _) as vx), ((y, _) as vy)) ->
      let cl =
        clause ~clause:"antisymmetric" law
          (strf "sign (cmp %s %s) = -sign (cmp %s %s)" x y y x)
      in
      inputs cl [ vx; vy ];
      let xy = compared cl vx vy in
      holds cl (sign xy = -sign (compared cl vy vx)))
    pairs;
  List.iter
    (fun (((x, _) as vx), ((y, _) as vy), ((z, _) as vz)) ->
      let cl =
        clause ~clause:"transitive" law
          (strf "cmp %s %s <= 0 and cmp %s %s <= 0 imply cmp %s %s <= 0" x y y z
             x z)
      in
      inputs cl [ a; b; c ];
      if compared cl vx vy <= 0 && compared cl vy vz <= 0 then
        holds cl (compared cl vx vz <= 0))
    [ (a, b, c); (a, c, b); (b, a, c); (b, c, a); (c, a, b); (c, b, a) ];
  let agrees ((x, v) as vx) ((y, u) as vy) =
    let cl =
      clause ~clause:"agrees with equal" law
        (strf "cmp %s %s = 0 iff %s = %s" (operand x) (operand y) x y)
    in
    inputs cl [ vx; vy ];
    let xy = compared cl vx vy in
    holds cl (Bool.equal (xy = 0) (fact cl (strf "%s = %s" x y) (equal v u)))
  in
  List.iter (fun (vx, vy) -> agrees vx vy) pairs;
  demand law "an unequal pair" (not (equal (snd a) (snd b)));
  match respell with
  | None -> ()
  | Some r ->
      let cl = clause ~clause:"respelled" law "cmp a (r a) = 0" in
      inputs cl [ a ];
      let ra = ("r a", apply cl "r a" w (fun () -> r (snd a))) in
      demand law "r a differs from a" (differs (snd ra) (snd a));
      holds cl (compared cl a ra = 0);
      agrees a ra;
      let cl =
        clause ~clause:"respelled" law "sign (cmp (r a) b) = sign (cmp a b)"
      in
      inputs cl [ a; b; ra ];
      let ab = compared cl a b in
      holds cl (sign (compared cl ra b) = sign ab)

let partial_order ?__POS__ ?msg w leq (a, b, c) =
  let law = law ?__POS__ ?msg "partial order" in
  let equal = Testable.equal w in
  let related cl (x, vx) (y, vy) =
    apply cl (strf "leq %s %s" x y) Testable.bool (fun () -> leq vx vy)
  in
  let inputs cl = List.iter (fun (name, v) -> term cl name w v) in
  let a = ("a", a) and b = ("b", b) and c = ("c", c) in
  List.iter
    (fun ((x, _) as vx) ->
      let cl = clause ~clause:"reflexive" law (strf "leq %s %s" x x) in
      inputs cl [ vx ];
      holds cl (related cl vx vx))
    [ a; b; c ];
  let pairs = [ (a, b); (a, c); (b, c) ] in
  List.iter
    (fun (((x, v) as vx), ((y, u) as vy)) ->
      let cl =
        clause ~clause:"antisymmetric" law
          (strf "leq %s %s and leq %s %s imply %s = %s" x y y x x y)
      in
      inputs cl [ vx; vy ];
      if related cl vx vy && related cl vy vx then
        holds cl (fact cl (strf "%s = %s" x y) (equal v u)))
    pairs;
  let chained = ref false in
  List.iter
    (fun (((x, _) as vx), ((y, _) as vy), ((z, _) as vz)) ->
      let cl =
        clause ~clause:"transitive" law
          (strf "leq %s %s and leq %s %s imply leq %s %s" x y y z x z)
      in
      inputs cl [ a; b; c ];
      if related cl vx vy && related cl vy vz then begin
        holds cl (related cl vx vz);
        chained := true
      end)
    [ (a, b, c); (a, c, b); (b, a, c); (b, c, a); (c, a, b); (c, b, a) ];
  let apart ((_, v), (_, u)) = not (equal v u) in
  demand law "a strict chain" (!chained && List.for_all apart pairs)

(* Laws of operations *)

let associative ?__POS__ ?msg w op (a, b, c) =
  let cl =
    clause (law ?__POS__ ?msg "associative") "op (op a b) c = op a (op b c)"
  in
  term cl "a" w a;
  term cl "b" w b;
  term cl "c" w c;
  let ab = apply cl "op a b" w (fun () -> op a b) in
  let left = apply cl "op (op a b) c" w (fun () -> op ab c) in
  let bc = apply cl "op b c" w (fun () -> op b c) in
  let right = apply cl "op a (op b c)" w (fun () -> op a bc) in
  sides cl w ("op (op a b) c", left) ("op a (op b c)", right)

let commutative ?__POS__ ?msg w op (a, b) =
  let cl = clause (law ?__POS__ ?msg "commutative") "op a b = op b a" in
  term cl "a" w a;
  term cl "b" w b;
  let ab = apply cl "op a b" w (fun () -> op a b) in
  let ba = apply cl "op b a" w (fun () -> op b a) in
  sides cl w ("op a b", ab) ("op b a", ba)

let neutral ?__POS__ ?msg w op e x =
  let law = law ?__POS__ ?msg "neutral" in
  let check left y z =
    let cl = clause law (left ^ " = x") in
    term cl "e" w e;
    term cl "x" w x;
    let side = apply cl left w (fun () -> op y z) in
    sides cl w (left, side) ("x", x)
  in
  check "op e x" e x;
  check "op x e" x e

let absorbing ?__POS__ ?msg w op z x =
  let law = law ?__POS__ ?msg "absorbing" in
  let check left y u =
    let cl = clause law (left ^ " = z") in
    term cl "z" w z;
    term cl "x" w x;
    let side = apply cl left w (fun () -> op y u) in
    sides cl w (left, side) ("z", z)
  in
  check "op z x" z x;
  check "op x z" x z

let invertible ?__POS__ ?msg w op e inv x =
  let law = law ?__POS__ ?msg "invertible" in
  let inputs cl =
    term cl "e" w e;
    term cl "x" w x
  in
  let cl = clause law "op x (inv x) = e" in
  inputs cl;
  let ix = apply cl "inv x" w (fun () -> inv x) in
  let x_ix = apply cl "op x (inv x)" w (fun () -> op x ix) in
  sides cl w ("op x (inv x)", x_ix) ("e", e);
  let cl = clause law "op (inv x) x = e" in
  inputs cl;
  term cl "inv x" w ix;
  let ix_x = apply cl "op (inv x) x" w (fun () -> op ix x) in
  sides cl w ("op (inv x) x", ix_x) ("e", e)

let distributive ?__POS__ ?msg w op ~over (a, b, c) =
  let law = law ?__POS__ ?msg "distributive" in
  let inputs cl =
    term cl "a" w a;
    term cl "b" w b;
    term cl "c" w c
  in
  let cl = clause law "op a (over b c) = over (op a b) (op a c)" in
  inputs cl;
  let bc = apply cl "over b c" w (fun () -> over b c) in
  let left = apply cl "op a (over b c)" w (fun () -> op a bc) in
  let ab = apply cl "op a b" w (fun () -> op a b) in
  let ac = apply cl "op a c" w (fun () -> op a c) in
  let right = apply cl "over (op a b) (op a c)" w (fun () -> over ab ac) in
  sides cl w ("op a (over b c)", left) ("over (op a b) (op a c)", right);
  let cl = clause law "op (over a b) c = over (op a c) (op b c)" in
  inputs cl;
  let ab = apply cl "over a b" w (fun () -> over a b) in
  let left = apply cl "op (over a b) c" w (fun () -> op ab c) in
  term cl "op a c" w ac;
  let bc = apply cl "op b c" w (fun () -> op b c) in
  let right = apply cl "over (op a c) (op b c)" w (fun () -> over ac bc) in
  sides cl w ("op (over a b) c", left) ("over (op a c) (op b c)", right)

let idempotent ?__POS__ ?msg w f x =
  let law = law ?__POS__ ?msg "idempotent" in
  let cl = clause law "f (f x) = f x" in
  term cl "x" w x;
  let fx = apply cl "f x" w (fun () -> f x) in
  demand law "f x differs from x" (differs fx x);
  let ffx = apply cl "f (f x)" w (fun () -> f fx) in
  sides cl w ("f (f x)", ffx) ("f x", fx)

let involutive ?__POS__ ?msg w f x =
  let law = law ?__POS__ ?msg "involutive" in
  let cl = clause law "f (f x) = x" in
  term cl "x" w x;
  let fx = apply cl "f x" w (fun () -> f x) in
  demand law "f x differs from x" (differs fx x);
  let ffx = apply cl "f (f x)" w (fun () -> f fx) in
  sides cl w ("f (f x)", ffx) ("x", x)

let commutes ?__POS__ ?msg w f g x =
  let cl = clause (law ?__POS__ ?msg "commutes") "f (g x) = g (f x)" in
  term cl "x" w x;
  let gx = apply cl "g x" w (fun () -> g x) in
  let fgx = apply cl "f (g x)" w (fun () -> f gx) in
  let fx = apply cl "f x" w (fun () -> f x) in
  let gfx = apply cl "g (f x)" w (fun () -> g fx) in
  sides cl w ("f (g x)", fgx) ("g (f x)", gfx)

let homomorphic ?__POS__ ?msg wa wb f op op' (a, b) =
  let cl =
    clause (law ?__POS__ ?msg "homomorphic") "f (op a b) = op' (f a) (f b)"
  in
  term cl "a" wa a;
  term cl "b" wa b;
  let ab = apply cl "op a b" wa (fun () -> op a b) in
  let left = apply cl "f (op a b)" wb (fun () -> f ab) in
  let fa = apply cl "f a" wb (fun () -> f a) in
  let fb = apply cl "f b" wb (fun () -> f b) in
  let right = apply cl "op' (f a) (f b)" wb (fun () -> op' fa fb) in
  sides cl wb ("f (op a b)", left) ("op' (f a) (f b)", right)

(* Conversions and relations *)

let round_trip ?__POS__ ?msg wa wb f g x =
  let cl = clause (law ?__POS__ ?msg "round trip") "g (f x) = x" in
  term cl "x" wa x;
  let fx = apply cl "f x" wb (fun () -> f x) in
  let gfx = apply cl "g (f x)" wa (fun () -> g fx) in
  sides cl wa ("g (f x)", gfx) ("x", x)

let monotone ?__POS__ ?msg wa wb f (a, b) =
  let cmp_a = order_of ~law:"monotone" ~witness:"the first witness" wa in
  let cmp_b = order_of ~law:"monotone" ~witness:"the second witness" wb in
  let law = law ?__POS__ ?msg "monotone" in
  let a, b = if cmp_a a b > 0 then (b, a) else (a, b) in
  let cl = clause law "a <= b implies f a <= f b" in
  term cl "a" wa a;
  term cl "b" wa b;
  let ab = cmp_a a b in
  term cl "cmp a b" Testable.int ab;
  demand law "a strict pair" (ab <> 0);
  let fa = apply cl "f a" wb (fun () -> f a) in
  let fb = apply cl "f b" wb (fun () -> f b) in
  let images = cmp_b fa fb in
  term cl "cmp (f a) (f b)" Testable.int images;
  (* Equal under the order, each of [a] and [b] is below the other. *)
  holds cl (images <= 0 && (ab <> 0 || images >= 0))

let ignores ?__POS__ ?msg wa wb f g x =
  let law = law ?__POS__ ?msg "ignores" in
  let cl = clause law "f (g x) = f x" in
  term cl "x" wa x;
  let gx = apply cl "g x" wa (fun () -> g x) in
  demand law "g x differs from x" (differs gx x);
  let fgx = apply cl "f (g x)" wb (fun () -> f gx) in
  let fx = apply cl "f x" wb (fun () -> f x) in
  sides cl wb ("f (g x)", fgx) ("f x", fx)

let preserves ?__POS__ ?msg w f inv x =
  let law = law ?__POS__ ?msg "preserves" in
  let cl = clause ~clause:"premise" law "inv x" in
  term cl "x" w x;
  holds cl (apply cl "inv x" Testable.bool (fun () -> inv x));
  let cl = clause law "inv x implies inv (f x)" in
  term cl "x" w x;
  term cl "inv x" Testable.bool true;
  let fx = apply cl "f x" w (fun () -> f x) in
  holds cl (apply cl "inv (f x)" Testable.bool (fun () -> inv fx))
