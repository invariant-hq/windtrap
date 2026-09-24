(* The guide's stateful example: four commands over a bounded queue, a list
   as the model, and an invariant that ties the two sizes together. A
   precondition both excludes an illegal call and selects a rare state
   ("push when full" is generated only at capacity). *)

open Windtrap

let capacity = 4

(* The model: the elements the queue should hold, oldest first. *)
type model = int list

let commands =
  [
    command "push" (Gen.int_range 0 9)
      ~pre:(fun m _ -> List.length m < capacity)
      ~next:(fun m x -> m @ [ x ])
      (fun _ x q -> Bounded_queue.push q x);
    call "pop"
      ~pre:(fun m -> m <> [])
      ~next:List.tl
      (fun m q -> equal int (List.hd m) (Bounded_queue.pop q));
    call "peek"
      ~pre:(fun m -> m <> [])
      ~next:Fun.id
      (fun m q -> equal int (List.hd m) (Bounded_queue.peek q));
    call "push when full"
      ~pre:(fun m -> List.length m = capacity)
      ~next:Fun.id
      (fun _ q -> raises Bounded_queue.Full (fun () -> Bounded_queue.push q 0));
  ]

let () =
  exit
  @@ run "bounded_queue"
       [
         stateful "behaves like a list" ~model:[]
           ~scope:(fun run -> run (Bounded_queue.create capacity))
           ~pp_model:(Testable.pp (list int))
           ~invariant:(fun m q ->
             equal int (List.length m) (Bounded_queue.size q))
           commands;
       ]
