module M = struct
  [@@@coverage exclude_file]

  let f n = n + 1
end

(* C78, cov:214: [exclude_file] floating in a nested structure is
   refused. *)
