(* The library under test in the opt-in shape: one [(instrumentation
   (backend ppx_windtrap.mutate))] stanza, inert without the flag. *)

type op = Add | Sub

let apply op a b = match op with Add -> a + b | Sub -> a - b
let clamp lo hi x = if x < lo then lo else if x > hi then hi else x
