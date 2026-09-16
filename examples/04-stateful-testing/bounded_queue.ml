(* The system under test: a fixed-capacity FIFO queue over a ring buffer.
   [push] raises [Full] at capacity, [pop] and [peek] raise [Empty]. *)

exception Full
exception Empty

type t = {
  data : int array;
  capacity : int;
  mutable head : int;
  mutable size : int;
}

let create capacity =
  if capacity <= 0 then invalid_arg "Bounded_queue.create: capacity <= 0";
  { data = Array.make capacity 0; capacity; head = 0; size = 0 }

let size q = q.size

let push q x =
  if q.size = q.capacity then raise Full;
  q.data.((q.head + q.size) mod q.capacity) <- x;
  q.size <- q.size + 1

let pop q =
  if q.size = 0 then raise Empty;
  let x = q.data.(q.head) in
  q.head <- (q.head + 1) mod q.capacity;
  q.size <- q.size - 1;
  x

let peek q = if q.size = 0 then raise Empty else q.data.(q.head)
