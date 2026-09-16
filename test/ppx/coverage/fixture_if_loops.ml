(* If branches (else-if chains are blocks of their own), while and for
   bodies. An if without else has only a then point. *)

let sign n = if n > 0 then 1 else if n < 0 then -1 else 0
let warn flag = if flag then print_endline "watch out"

let count_up n =
  let i = ref 0 in
  while !i < n do
    incr i
  done;
  !i

let sum n =
  let total = ref 0 in
  for i = 1 to n do
    total := !total + i
  done;
  !total
