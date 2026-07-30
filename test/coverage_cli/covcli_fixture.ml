(* Instrumented by ppx_windtrap.coverage (see dune). [trip] calls a
   raiser in non-tail position: the out-edge point on [boom ()] can never
   fire, so this file always reports exactly one uncovered point on the
   call line. The CLI test pins line numbers: [trip]'s call must stay on
   line 7. *)
let boom () = raise Exit
let trip () = Fun.id (boom ())
