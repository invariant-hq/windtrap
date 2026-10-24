open Windtrap

module Model = struct
  type t = int list ref

  let create () = ref []
  let length m = List.length !m
  let push m x = m := !m @ [ x ]

  let get m i =
    if i < 0 || i >= length m then invalid_arg "Vec.get" else List.nth !m i

  let set m i x =
    if i < 0 || i >= length m then invalid_arg "Vec.set"
    else m := List.mapi (fun j y -> if j = i then x else y) !m
end

let vec = abstract "v" ~pp:(fun ppf m -> Testable.pp (list int) ppf !m)
let index = among int vec (fun m -> List.init (Model.length m) Fun.id)
let value = Gen.int_range 0 9

let commands =
  [
    command "create" (Gen.unit @-> makes vec) Model.create Vec.create;
    command "push" (vec ^-> value @-> returns unit) Model.push Vec.push;
    command "get" (vec ^-> index ^-> returns int) Model.get Vec.get;
    command "set" (vec ^-> index ^-> value @-> returns unit) Model.set Vec.set;
    command "get anywhere"
      (vec ^-> Gen.int_range (-2) 9 @-> returns int)
      Model.get Vec.get;
  ]

let vecs = group "Vec" [ stateful "behaves like a list" commands ]
let () = exit (run "vec" [ vecs ])
