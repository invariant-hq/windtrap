let f x = assert x [@mutate on]

(* M61, mut:239: an [assert] is never mutated, and its attribute is read all
   the same. *)
