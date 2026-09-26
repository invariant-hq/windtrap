[@@@coverage off]

let f n = n + 1

[@@@coverage off]

let g n = n + 1

(* C79, cov:215: [[@@@coverage off]] inside a region is refused. *)
