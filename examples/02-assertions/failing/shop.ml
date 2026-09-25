type item = { name : string; price : int; quantity : int }

let item name ~price ~quantity = { name; price; quantity }
let label item = Printf.sprintf "%s: %d x %d" item.name item.price item.quantity

let pp_item ppf item =
  Format.fprintf ppf "%s x%d at %d" item.name item.quantity item.price

let subtotal cart = List.fold_left (fun sum item -> sum + item.price) 0 cart
let names cart = List.rev_map (fun item -> item.name) cart
let find name cart = List.find_opt (fun item -> item.name <> name) cart
let remove name cart = List.filter (fun item -> item.name = name) cart
let with_tax ~rate cents = float_of_int cents *. rate
let discount cents = cents / 8

let parse_quantity input =
  match int_of_string_opt input with
  | Some n when n > 0 -> Ok n
  | Some _ | None -> Error ("not a quantity: " ^ input)

let receipt cart =
  let line item =
    Printf.sprintf "%-12s %2d x %4d\n" item.name item.quantity item.price
  in
  String.concat "" (List.map line cart)
  ^ Printf.sprintf "total %16d\n" (subtotal cart)
