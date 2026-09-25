type op = Add | Sub | Mul | Div

let apply op a b =
  match op with
  | Add -> a + b
  | Sub -> a - b
  | Mul -> a * b
  | Div -> if b = 0 then invalid_arg "Calc.apply: division by zero" else a / b

let eval start steps =
  List.fold_left (fun acc (op, operand) -> apply op acc operand) start steps

let symbol = function Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
[@@coverage off]
