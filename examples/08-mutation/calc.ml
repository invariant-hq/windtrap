type op = Add | Sub | Mul | Div

let apply op a b =
  match op with
  | Add -> a + b
  | Sub -> a - b
  | Mul -> a * b
  | Div -> if b = 0 then invalid_arg "Calc.apply: division by zero" else a / b

let sign n = if n > 0 then 1 else if n < 0 then -1 else 0

let abs n =
  if (n >= 0) [@mutate off "both arms yield 0 at n = 0"] then n else -n
