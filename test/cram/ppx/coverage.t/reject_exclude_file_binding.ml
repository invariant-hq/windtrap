let f n = n + 1 [@@coverage exclude_file]

(* C77, cov:213: [exclude_file] on a binding is refused. *)
