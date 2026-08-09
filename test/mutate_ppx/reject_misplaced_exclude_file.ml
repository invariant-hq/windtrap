module M = struct
  [@@@mutate exclude_file]

  let f n = n + 1
end
