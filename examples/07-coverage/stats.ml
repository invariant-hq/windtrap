let mean = function
  | [] -> None
  | l -> Some (List.fold_left ( + ) 0 l / List.length l)
