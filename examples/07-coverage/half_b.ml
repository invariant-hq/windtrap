let clamp lo hi x = if x < lo then lo else if x > hi then hi else x
let sum = List.fold_left ( + ) 0
let sign x = if x > 0 then 1 else if x < 0 then -1 else 0
