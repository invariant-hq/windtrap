open Windtrap

module Model = struct
  type t = { capacity : int; mutable items : int list }

  let create capacity = { capacity; items = [] }
  let size m = List.length m.items

  let peek m =
    match m.items with [] -> raise Bounded_queue.Empty | x :: _ -> x

  let pop m =
    let x = peek m in
    m.items <- List.tl m.items;
    x

  let push m x =
    if size m = m.capacity then raise Bounded_queue.Full;
    m.items <- m.items @ [ x ];
    cover "reached capacity" (size m = m.capacity)
end

let queue =
  abstract "q" ~pp:(fun ppf m -> Testable.pp (list int) ppf m.Model.items)

let commands =
  [
    command "create"
      (Gen.int_range 1 4 @-> makes queue)
      Model.create Bounded_queue.create;
    command "push"
      (queue ^-> Gen.int_range 0 9 @-> returns unit)
      Model.push Bounded_queue.push;
    command "pop" (queue ^-> returns int) Model.pop Bounded_queue.pop;
    command "peek" (queue ^-> returns int) Model.peek Bounded_queue.peek;
    command "size" (queue ^-> returns int) Model.size Bounded_queue.size;
  ]

let queues = group "queue" [ stateful "behaves like a list" commands ]

let regressions =
  group "regressions"
    [
      test "a full queue refuses a push" (fun () ->
          let q = Bounded_queue.create 1 in
          Bounded_queue.push q 0;
          raises ~__POS__ Bounded_queue.Full (fun () -> Bounded_queue.push q 0));
    ]

let () = exit (run "bounded_queue" [ queues; regressions ])
