let f x = assert x [@mutate on]

(* M61, mut:235: an [assert] is never mutated, and its attribute is read all
   the same. *)
