(* Pipelines: each [|>] stage's out-edge is attributed to the next
   stage; the final stage's point keys on the head callee at the end of
   the chain. A pipeline in tail position keeps its tail application
   unwrapped (inner stages still carry their points - they are arguments,
   not tail calls). *)

let double x = x * 2
let staged x = x |> double |> double

let bound x =
  let y = x |> double in
  y + 1
