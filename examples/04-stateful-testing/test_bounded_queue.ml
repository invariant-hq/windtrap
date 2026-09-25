open Windtrap

let capacity = 4

let commands =
  [
    command "push" (Gen.int_range 0 9)
      ~pre:(fun m _ -> List.length m < capacity)
      ~next:(fun m x -> m @ [ x ])
      (fun _ x q -> Bounded_queue.push q x);
    call "pop"
      ~pre:(fun m -> m <> [])
      ~next:List.tl
      (fun m q -> equal ~__POS__ int (List.hd m) (Bounded_queue.pop q));
    call "peek"
      ~pre:(fun m -> m <> [])
      ~next:Fun.id
      (fun m q -> equal ~__POS__ int (List.hd m) (Bounded_queue.peek q));
    call "push when full"
      ~pre:(fun m -> List.length m = capacity)
      ~next:Fun.id
      (fun _ q ->
        raises ~__POS__ Bounded_queue.Full (fun () -> Bounded_queue.push q 0));
  ]

let queue =
  group "queue"
    [
      stateful "behaves like a list" ~model:[]
        ~scope:(fun run -> run (Bounded_queue.create capacity))
        ~pp_model:(Testable.pp (list int))
        ~invariant:(fun m q ->
          cover "reached capacity" (List.length m = capacity);
          equal ~__POS__ int (List.length m) (Bounded_queue.size q))
        commands;
    ]

let regressions =
  group "regressions"
    [
      test "a full queue refuses a push" (fun () ->
          let q = Bounded_queue.create capacity in
          List.iter (Bounded_queue.push q) [ 0; 0; 0; 0 ];
          raises ~__POS__ Bounded_queue.Full (fun () -> Bounded_queue.push q 0));
    ]

let () = exit (run "bounded_queue" [ queue; regressions ])
