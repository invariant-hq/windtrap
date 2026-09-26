let f x = assert x [@mutate on]

(* An [assert] is never mutated, and its attribute is read all the same. *)
