let f n = n + 1 [@@mutate exclude_file]

(* M62, mut:239: [exclude_file] on a binding is refused. *)
