open Windtrap

let queue = abstract "q"

let commands =
  [
    command "create" (Gen.unit @-> makes queue) Queue.create Mpmc.create;
    command "push"
      (queue ^-> Gen.int_range 0 9 @-> returns unit)
      (fun q x -> Queue.push x q)
      Mpmc.push;
    command "pop" (queue ^-> returns (option int)) Queue.take_opt Mpmc.pop_opt;
    command "length" (queue ^-> returns int) Queue.length Mpmc.length;
  ]

let mpmc =
  group "Mpmc"
    [
      stateful "behaves like Queue" commands;
      stateful ~domains:2 "behaves like Queue from two domains" commands;
    ]

let () = exit (run "mpmc" [ mpmc ])
