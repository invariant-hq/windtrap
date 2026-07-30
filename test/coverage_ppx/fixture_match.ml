(* Match and try arms, and their guards (Law 14 forms 2 and 3). The
   [assert false] arm must stay unmarked. *)

let classify n =
  match n with
  | 0 -> "zero"
  | n when n > 0 -> "positive"
  | n when n < 0 -> "negative"
  | _ -> assert false

let safe_head l = try List.hd l with Failure _ -> "empty"
