(* The inline suite: one passing test through the instrumented library —
   the [true] branch only, leaving [choose]'s other arm uncovered. *)

let%test "covers the yes branch" =
  Windtrap.equal Windtrap.string "yes" (Covlib.choose true)
