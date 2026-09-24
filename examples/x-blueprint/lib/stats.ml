(* [clamp]'s comparison carries a dismissed mutant: [n > 0] and
   [n >= 0] agree at zero (both arms yield zero marks), so the mutation
   loop cannot distinguish them. The attribute records that reasoning
   where [git blame] can see it. *)
let clamp n =
  if (n > 0) [@mutate off "both arms yield zero marks at 0"] then n else 0

let render rows =
  let width =
    List.fold_left (fun acc (label, _) -> max acc (String.length label)) 0 rows
  in
  let line (label, n) =
    label
    ^ String.make (width - String.length label) ' '
    ^ "  "
    ^ String.make (clamp n) '#'
  in
  let total = List.fold_left (fun acc (_, n) -> acc + clamp n) 0 rows in
  String.concat "\n" (List.map line rows @ [ "total " ^ string_of_int total ])
