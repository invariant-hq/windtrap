let f n = (n + 1) [@mutate off 42]

(* M60, mut:233-234: [off] with a payload other than one string literal is
   refused. *)
