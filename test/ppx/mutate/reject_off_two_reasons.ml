let f n = (n + 1) [@mutate off "a" "b"]

(* M60, mut:233-234: [off] with two string literals is refused. *)
