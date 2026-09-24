(* A library that enables both instrumentation backends at once, which
   the manual's stanza shows side by side. Nothing calls it: its job is to
   be compiled under [--instrument-with ppx_windtrap.coverage
   --instrument-with ppx_windtrap.mutate]
   and to prove the two instrumenters compose - one wraps application
   out-edges, the other replaces expressions with guards, and each must
   survive the other's output. *)

let apply op a b = match op with `Add -> a + b | `Sub -> a - b
let clamp lo hi x = if x < lo then lo else if x > hi then hi else x
let both p q x = p x && q x
