let f n = n + 1 [@@mutate on]

(* M62, mut:239: [on] on a binding is refused. *)
