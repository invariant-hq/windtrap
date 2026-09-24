module M = struct
  [@@@mutate exclude_file]

  let f n = n + 1
end

(* Rules pinned here, by id in RULES.md and interface line: M63, mut:236. *)
