(* Compiled mirrors of the manual's *failing* walkthroughs — the transcripts
   shown in doc/manual/. This executable is built (so every snippet
   compiles) but never wired to runtest: run it by hand to regenerate a
   transcript, selecting one walkthrough with -f. *)

open Windtrap
open Windtrap_stateful

(* getting-started.md / assertions.md: the failing equal with a diff. *)

module Sessions = struct
  let all () = [ ("alice", [ 1; 2; 3 ]); ("bob", [ 4; 5 ]); ("carol", []) ]
end

let failing_equal =
  group "users"
    [
      test "sessions after login" (fun () ->
          let sessions = Sessions.all () in
          equal
            (list (pair string (list int)))
            [ ("alice", [ 1; 2; 3 ]); ("bob", [ 4 ]) ]
            sessions;
          equal int 3 (List.length sessions));
    ]

(* property-testing.md: the failing property and its replay line. *)

let encode fields = String.concat "," fields
let decode = function "" -> [] | s -> String.split_on_char ',' s

let failing_prop =
  prop "decode inverts encode"
    Gen.(list string)
    (fun fields ->
      equal (list string) fields (decode (encode fields));
      classify "empty" (fields = []))

(* snapshots-and-expect.md: the missing-baseline failure. *)

let help () =
  String.concat "\n"
    [
      "Usage: mytool [OPTIONS] COMMAND";
      "";
      "Commands:";
      "  build    Build the project";
      "  test     Run the tests";
    ]

let missing_snapshot =
  group "cli" [ test "cli help" (fun () -> snapshot "help" (help ())) ]

(* stateful-testing.md: the bounded queue whose ring buffer wraps on the
   queue's length instead of its capacity, so a slot goes stale. *)

module Bounded_queue = struct
  exception Full
  exception Empty

  type t = {
    data : int array;
    capacity : int;
    mutable head : int;
    mutable size : int;
  }

  let create capacity =
    { data = Array.make capacity 0; capacity; head = 0; size = 0 }

  let size q = q.size

  let push q x =
    if q.size = q.capacity then raise Full;
    q.data.((q.head + q.size) mod q.capacity) <- x;
    q.size <- q.size + 1

  let pop q =
    if q.size = 0 then raise Empty;
    let x = q.data.(q.head) in
    (* The bug: [mod q.size] where the ring is [q.capacity] long. *)
    q.head <- (q.head + 1) mod q.size;
    q.size <- q.size - 1;
    x

  let peek q = if q.size = 0 then raise Empty else q.data.(q.head)
end

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

let failing_stateful =
  stateful "behaves like a list" ~model:[]
    ~setup:(fun () -> Bounded_queue.create capacity)
    ~pp_model:(Testable.pp (list int))
    ~invariant:(fun m q -> equal int (List.length m) (Bounded_queue.size q))
    commands

(* stateful-testing.md: a ~pre that raises. The program is poisoned rather
   than the generator, so the search minimises the specification bug. *)

module Pool = struct
  type t = (int, Buffer.t) Hashtbl.t

  let create () : t = Hashtbl.create 8
  let open_ pool id = Hashtbl.replace pool id (Buffer.create 16)
  let close pool id = Hashtbl.remove pool id
end

type pool_model = { live : int list; next_id : int }

let slot = Gen.int_range 0 3

let poisoned_pre =
  stateful "handles stay live" ~model:{ live = []; next_id = 0 }
    ~setup:Pool.create
    ~pp_model:(fun ppf m -> Testable.pp (list int) ppf m.live)
    [
      call "open"
        ~pre:(fun m -> List.length m.live < 4)
        ~next:(fun m ->
          { live = m.live @ [ m.next_id ]; next_id = m.next_id + 1 })
        (fun m pool -> Pool.open_ pool m.next_id);
      (* The bug: [List.nth] raises when the slot is past the live set. *)
      command "close" slot
        ~pre:(fun m i -> List.nth m.live i >= 0)
        ~next:(fun m i ->
          { m with live = List.filteri (fun j _ -> j <> i) m.live })
        (fun m i pool -> Pool.close pool (List.nth m.live i));
    ]

let suite =
  [
    failing_equal;
    failing_prop;
    missing_snapshot;
    failing_stateful;
    poisoned_pre;
  ]

let () = run "mytool" suite
