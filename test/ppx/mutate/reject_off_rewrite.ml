let f n = (n + 1) [@mutate off sub "equal at zero"]

(* M66, mut:146-148: [off] that names a rewrite is refused, so a bare [off]
   keeps dismissing every mutant of its expression. *)
